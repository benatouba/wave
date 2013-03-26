; docformat = 'rst'
;+
;
;  w_DEM is a basis class to read DEM in ENVI's grd format.
;  It reads the projection info frm the .hdr file and the 
;  elevation from the binary .grd files, so both must be put in the 
;  same directory for reading.
;
; :History:
;     Written by FaM, 2011.
;
;-  

;+
; :Description:
;    Initialize the object instance
; 
; :Params:
;    file: in, optional
;          the path to the file to open
;    
; :Keywords:
;    _EXTRA: in, optional
;            any keyword accepted by `w_GISdata::defineSubset`
;            
;               
; :Returns: 
;    1 if the object is created successfully, 0 if not
;
;-
function w_DEM::init, file, NO_DELTA=no_delta, _EXTRA=extra
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ; Check arguments
  if N_ELEMENTS(file) ne 1 then file = DIALOG_PICKFILE(TITLE='Please select DEM grd file to read', /MUST_EXIST)  
  spli = STRSPLIT(file, '.', /EXTRACT)
  if str_equiv(spli[1]) ne 'GRD' then message, WAVE_Std_Message(/FILE)
  hdr = spli[0] + '.hdr'
  
  self.file = file
  self.hdr = hdr
   
  ; Original grid geoloc
  _quiet = !QUIET
  !QUIET = 1
  GIS_open_grid, ret, info, id, FILE=self.file, /RONLY, /NO_STC
  !QUIET = _quiet
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message(/FILE)
  
  self.id = id

  ; Check and correct TNT coordinate structure
  coord = info.coord
  coord.system_z = 'DEM'
  if ~ GIS_check_coord(coord, /CORR, ERROR=error) then begin
    GIS_close_grid, ret, id
    ret = TNT_return(DTM, TNT_S_ERROR, error)
    message, WAVE_Std_Message(/FILE)
  endif

  info.coord = coord
  map_info = utils_replace_string(utils_replace_string(coord.map_info, '{', ''), '}', '')
  proj_name = (STRSPLIT(map_info, ',', /EXTRACT))[0]
  if proj_name eq 'Geographic Lat/Lon' then begin
    GIS_make_proj, ret, proj, NAME='Geographic (WGS-84)'
  endif else begin
    proj = coord.proj
  endelse
  
  if KEYWORD_SET(NO_DELTA) then begin
      deltaX =  0
      deltaY =  0      
  endif else begin    
      deltaX =  coord.dx/2
      deltaY =  - coord.dy/2
  endelse
  

  grid = OBJ_NEW('w_Grid2D',    nx = coord.nx               , $
                                ny = coord.ny               , $
                                dx = coord.dx               , $
                                dy = coord.dy               , $
                                x0 = coord.x0 + deltaX      , $
                                y0 = coord.y0 + deltaY      , $
                                proj = proj               )

  ok = self->w_GISdata::init(grid, _EXTRA=extra)
  undefine, grid
  if ~ ok then return, 0
  
  return, 1
  
end

;+
; :Description:
;    Destroy the object instance
;
;-
pro w_DEM::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_gisdata::Cleanup 
  
  if self.id ne 0 then GIS_close_grid, ret, self.id
  
  
end

;+
; :Description:
;    To obtain the list af available variables in the dataset.
;
; :Keywords:
;    COUNT: out, optional
;           the number of variables
;    PRINT: in, optional
;           set this keyword to print the variables (and info)
;           in the console
;           
; :Returns:
;   An array of variable ids
;
;-
function w_DEM::getVarNames, COUNT=count, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  varid = 'z' 
  varnames = 'z' 
  varndims = 2
  varunits = 'm' 
  vardescriptions = 'Altitude'
  vartypes = ''
  count = N_ELEMENTS(varid)
    
  if KEYWORD_SET(PRINT) then begin
    print, '   ID   NAME            DESCRIPTION                                 UNIT                   TYPE'
    
    for i = 0L, count-1 do begin
      ns = '                                                                                                                                  '
      STRPUT, ns, str_equiv(varid[i]), 3
      STRPUT, ns, STRLOWCASE(str_equiv(varnames[i])), 4 + 4
      STRPUT, ns, STRLOWCASE(str_equiv(vardescriptions[i])), 20 + 4
      STRPUT, ns, STRLOWCASE(str_equiv(varunits[i])), 70 - 2
      STRPUT, ns, STRLOWCASE(str_equiv(vartypes[i])), 81 + 10
      print, ns
    endfor
  endif
    
  return, varnames
  
end

;+
; :Description:
;    Checks if a variable is available
;
; :Params:
;    id: in, required
;        the variable ID
;
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data
;          
; :Returns:
;   1 if the variable is available, 0 if not
;   
;-
function w_DEM::hasVar, id, INFO=info
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  undefine, info
  
  n = self->GetVarNames()  
  p = where(str_equiv(n) eq str_equiv(id), cnt)   
  if cnt eq 0 then return, 0
  
  name = 'z'
  unit = 'm' 
  description = 'Altitude'

  info = {id:id, name:name, description:description, unit:unit}
  
  return, 1

end

;+
; :Description:
;    Get the data for a specific variable, at the dimensions of the subset.
; 
; :Params:
;    id: in, optional
;        the variable ID. If not set, the TRMM precipitation will be returned instead
;    time: out, type = qms
;          the variable time
;    nt: out, type = long
;        the variable number of times
;        
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data. Contains the tags:: 
;            - name
;            - id
;            - description
;            - unit
;            
; :Returns:
;   the data array
;   
;-
function w_DEM::getVarData, id, time, nt, INFO=info, T0=t0, T1=t1

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, info, time, nt
  
  if N_ELEMENTS(id) eq 0 then begin
    id = 'z'
  endif
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  if TOTAL(self.subset) ne 0 then begin
    col = self.subset[0]
    n_cols = self.subset[1]
    self.ogrid->getProperty, NY=ny
    row = ny - self.subset[2] - self.subset[3]
    n_rows = self.subset[3]
  endif
  
  ; Read DEM data *
  GIS_read_grid, ret, self.id, z, $
    COL=col,N_COLS=n_cols, $
    ROW=row,N_ROWS=n_rows
  if TNT_err_code(ret) ne TNT_E_NONE then message, WAVE_Std_Message(/FILE)
  z = ROTATE(z,7)
  
  return, z
  
end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_DEM__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_DEM                      ,  $
            INHERITS w_GISdata         ,  $
            file:                ''    ,  $ ; .grd file
            hdr:                 ''    ,  $ ; .hdr file
            id:                  0L       $ ; grid ID
          }
    
end