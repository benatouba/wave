; docformat = 'rst'
;+
;
;  w_TRMM is the basis class for TRMM V7 files.
;  
; :History:
;     Written by FaM, 2012.
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
function w_TRMM::init, file, _EXTRA=extra
  
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
  if N_ELEMENTS(file) ne 1 then file = DIALOG_PICKFILE(TITLE='Please select TRMM ncdf file to read', /MUST_EXIST)  
  geo = OBJ_NEW('w_GEO_nc', FILE=file)
  IF ~ OBJ_VALID(geo) THEN RETURN, 0
  self.obj = geo
  
  ; Check filename for type
  geo->GetProperty, FNAME=fname, CDFID=cdfid
  fname = str_equiv(fname)
  
  ; type of data granule: '3B42d', '3B42', '3B43', +'_agg'  
  type = ''
  isHere = STRPOS(fname, '.7.')
  if isHere lt 0 then message, 'file does not seem to be a V7 product.'   
  isHere = STRPOS(fname, '3B43')
  if isHere ge 0 then type = '3B43'
  isHere = STRPOS(fname, '3B42')
  if isHere ge 0 then type = '3B42'
  isHere = STRPOS(fname, '3B42_DAILY')
  if isHere ge 0 then type = '3B42d'  
  if type eq '' then message, WAVE_Std_Message('Input file not recognized as a known TRMM product.')
  self.type = type
    
  ; Original grid geoloc
  ok = geo->define_subset()
  ok = utils_nc_LonLat(cdfid, lon_id, lat_id)  
  lon = geo->get_Var(lon_id)  
  lat = geo->get_Var(lat_id)
  nx = N_ELEMENTS(lon)
  ny = N_ELEMENTS(lat)  
  ;Projection
  GIS_make_proj, ret, proj, PARAM='1, WGS-84'
  
  case (self.type) of
    '3B42' : meta = 'TRMM 3B42 3hrly file.'
    '3B42d': meta = 'TRMM 3B42 daily file.'
    '3B43' : meta = 'TRMM 3B43 monthly file.'
  endcase
  
  grid = OBJ_NEW('w_Grid2D', nx=nx, $
    ny=ny, $
    dx=0.25D, $
    dy=0.25D, $
    x0=lon[0], $
    y0=lat[ny-1], $
    proj=proj, $
    meta=meta)

  ok = self->w_GISdata::init(grid, _EXTRA=extra)
  undefine, grid
  if ~ ok then return, 0
  
  ; Time
  tok = utils_nc_COARDS_time(cdfid, time, time0, time1, nt)
  if ~ tok then Message, 'Time could not be read.' 
  self.time = PTR_NEW(time)
  
  return, 1
  
end

;+
; :Description:
;    Destroy the object instance
;
;-
pro w_TRMM::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_gisdata::Cleanup 
  OBJ_DESTROY, self.obj
  PTR_FREE, self.time

end

;+
; :Description:
;    Get access to some params. 
;
; :Keywords:
;    TYPE: out, optional
;          product type
;    _Ref_Extra: out
;                all parent classe property
;                
;-      
pro w_TRMM::GetProperty,  $
    TYPE=type,  $
    _Ref_Extra=extra
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  IF Arg_Present(TYPE) THEN type = self.type
  
  self->w_GISdata::GetProperty, _Extra=extra
  
end

;+
; :Description:
;   Retrieve time info.
;
; :Params:
;    time: out, type = QMS
;          the time in qms
;    nt: out, type = integer
;        number of elements in time
;    t0: out, type = LL64
;        first time in qms       
;    t1: out, type = LL64
;        end time in qms 
;        
;-
pro w_TRMM::getTime, time, nt, t0, t1

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
      
  time = *self.time
  nt = N_ELEMENTS(time)
  t0 = MIN(time)
  t1 = MAX(time)
  
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
function w_TRMM::getVarNames, COUNT=count, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  self.obj->get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes
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
function w_TRMM::hasVar, id, INFO=info
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  undefine, info
  
  n = self->GetVarNames()  
  p = where(str_equiv(n) eq str_equiv(id), cnt)   
  if cnt eq 0 then return, 0
  
  ok = self.obj->get_Var_Info(id, $
    out_id = out_id, $
    units = units, $
    description = description, $
    varname = varname , $ ;
    dims = dims, $ ;
    dimnames = dimnames)

  info = {id:id, name:id, description:description, unit:units}
  
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
function w_TRMM::getVarData, id, time, nt, INFO=info, T0=t0, T1=t1

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, info, time, nt
  
  if N_ELEMENTS(id) eq 0 then begin
    case (self.type) of
      '3B42' : id = 'pcp'
      '3B42d': id = 'r'
      '3B43' : id = 'pcp'
    endcase
  endif
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  if TOTAL(self.subset) ne 0 then ok = self.obj->define_subset(SUBSET=self.subset) else ok = self.obj->define_subset()
  out = self.obj->get_Var(id, time, nt, T0=t0, T1=t1)
  
  p = where(out lt 0., cnt)
  if cnt ne 0 then out[p] = 0.
  
  return, out
 
  
end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_TRMM__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_TRMM                              ,  $
            INHERITS w_GISdata                  ,  $
            type:               ''              ,  $ ; type of active trmm file: '3B42D', '3B42', '3B43', + '_agg'
            time:               PTR_NEW()       ,  $ ; Time array in QMS
            obj:                OBJ_NEW()          $ ; geo_Nc object
          }
    
end