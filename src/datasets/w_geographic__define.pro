; docformat = 'rst'
;+
;
;  w_geographic is the basis class for all NCDF files with a
;  geographic coordinate system
;  
; :History:
;     Written by FaM, 2013.
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
;    DATUM: in, optional, default='WGS-84'
;           a string to define the datum
;    _EXTRA: in, optional
;            any keyword accepted by `w_GISdata::defineSubset`
;            
;               
; :Returns: 
;    1 if the object is created successfully, 0 if not
;
;-
function w_geographic::init, file, DATUM=datum, _EXTRA=extra
  
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
  if N_ELEMENTS(file) ne 1 then file = DIALOG_PICKFILE(TITLE='Please select ncdf file to read', /MUST_EXIST)  
  geo = OBJ_NEW('w_GEO_nc', FILE=file)
  IF ~ OBJ_VALID(geo) THEN RETURN, 0
  self.obj = geo
      
  ; Original grid geoloc
  ok = geo->define_subset()
  geo->get_ncdf_coordinates, lon, lat, nx, ny, /NO_REFORM
  
  y0 = max(lat, pm)
  if pm eq 0 then begin
    self.order = 1 ; Check this
    Message, 'I decided that the file is upside down.', /INFORMATIONAL
  endif
    
  ;Projection
  SetDefaultValue, datum, 'WGS-84'
  GIS_make_proj, ret, proj, PARAM='1, ' + datum
  
  grid = OBJ_NEW('w_Grid2D', nx=nx, $
    ny=ny, $
    dx=ABS(lon[1]-lon[0]), $
    dy=ABS(lat[1]-lat[0]), $
    x0=lon[0], $
    y0=y0, $
    proj=proj, $
    meta=meta)

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
pro w_geographic::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_gisdata::Cleanup 
  OBJ_DESTROY, self.obj

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
pro w_geographic::getTime, time, nt, t0, t1

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
      
  self.obj->get_time, time, nt, t0, t1 
  
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
function w_geographic::getVarNames, COUNT=count, PRINT=print

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
function w_geographic::hasVar, id, INFO=info
  
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
;   HOUROFDAY: in, optional, type = long
;              to get strides of the time serie at specific hours of day
;   ZLEVELS: in, optional, type = long
;            set this keyword to an array of one or two elements, containing the range
;            of the indexes to keep from the original NCDF file in the Z dimension.
;            It is better to use the `ZDIM_ID` keyword to specify
;            in which dimension these indexes must be kept, otherwise `get_Var` will try
;            to do its best from what it knows 
;            (in most cases -4D arrays-, it should work fine).
;   INVERTZ: in, optional, type=boolean
;            if set, the Z levels are upside down
;            
; :Returns:
;   the data array
;   
;-
function w_geographic::getVarData, id, time, nt, INFO=info, T0=t0, T1=t1, $
    HOUROFDAY=hourofday, $
    INVERTZ=invertz, $
    ZLEVELS=zlevels

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, info, time, nt
    
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  if TOTAL(self.subset) ne 0 then ok = self.obj->define_subset(SUBSET=self.subset) else ok = self.obj->define_subset()
  out = self.obj->get_Var(id, time, nt, T0=t0, T1=t1, HOUROFDAY=hourofday, ZLEVELS=zlevels)
  
  if self.order eq 1 then begin
     s = SIZE(out, /N_DIMENSIONS)
     if s eq 2 then out = ROTATE(out, 7)
     if s eq 3 then for i=0, nt-1 do out[*,*,i] = ROTATE(out[*,*,i], 7)
     if s eq 4 then for i=0, nt-1 do for j=0, N_ELEMENTS(out[0,0,*,0])-1 do out[*,*,j,i] = ROTATE(out[*,*,j,i], 7)
  endif
  
  if KEYWORD_SET(INVERTZ) then begin
    s = SIZE(out, /N_DIMENSIONS)
    if nt eq 1 and s eq 2 then Message, 'INVERTZ not possible'
    if nt ne 1 and s eq 3 then Message, 'INVERTZ not possible'
    nz = N_ELEMENTS(out[0,0,*,0])
    _out = out
    for i=0, nt-1 do for j=0, nz-1 do out[*,*,j,i] = _out[*,*,nz-j-1,i]
    undefine, _out
  endif  
  
  return, out
 
  
end

;+
; :Description:
;    Gives access to the w_geo_nc object (for dumps, etc)
;-
function w_geographic::obj
  
  return, self.obj

end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_geographic__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_geographic                        ,  $
            INHERITS w_GISdata                  ,  $
            obj:                OBJ_NEW()          $ ; geo_Nc object
          }
    
end