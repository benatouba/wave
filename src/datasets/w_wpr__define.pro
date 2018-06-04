; docformat = 'rst'
;+
;
;  w_WPR is the basis class for WRF products directories.
;  
; :History:
;     Written by FaM, 2012.
;
;-      

;+
; :Description:
;    Initialize the object instance
;    
; :Keywords:
;    DIRECTORY: in, required
;               the path to the WRF files directory. either m, d, h or y
;    IGNORE_ALTERNATE: in, optional
;                      set this keyword to ignore the 2d_alternate folder in the product dir.
;    _EXTRA: in, optional
;            any keyword accepted by `w_GISdata::defineSubset`
;            
;               
; :Returns: 
;    1 if the object is created successfully, 0 if not
;
;-
function w_WPR::init, DIRECTORY=directory, IGNORE_ALTERNATE=ignore_alternate, _EXTRA=extra
  
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
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select WRF product directory to read', /MUST_EXIST, /DIRECTORY)
  if directory eq '' then MESSAGE, WAVE_Std_Message(/FILE)
  if ~ FILE_TEST(directory, /DIRECTORY) then MESSAGE, WAVE_Std_Message(FILE=directory)
  
  dir = utils_clean_path(directory)
  prdir = FILE_DIRNAME(dir)
  tres = FILE_BASENAME(dir)
  hres = FILE_BASENAME(prdir)
  statdir = prdir + '/static'
  
  if ~ FILE_TEST(statdir, /DIRECTORY) then Message, 'Cannot find a static directory.' + $
           ' Be sure you are at the right place in the product directory structure'
  
  self.tres = tres
  self.hres = hres
  self.directory = dir
  self.objs = OBJ_NEW('w_WRF_Container')
    
  file_list=FILE_SEARCH([self.directory, statdir], '*_' + self.hres +'_*.nc', count=filecnt)  
  if filecnt eq 0 then MESSAGE, 'No files in the directory?'  
  
  if KEYWORD_SET(IGNORE_ALTERNATE) then begin
    matches = Where(StrMatch(file_list, '*2d_alternate*'), cm)
    if cm ne 0 then begin
      utils_array_remove, matches, file_list
      filecnt = N_ELEMENTS(file_list)
    endif
  endif
    
  fnames = FILE_BASENAME(file_list, '.nc')
  years = LONARR(filecnt)
  for i=0, filecnt-1 do begin
    spl = STRSPLIT(fnames[i], '_', /EXTRACT, count=nspl)
    type = FILE_BASENAME(FILE_DIRNAME(file_list[i])) 
    if type eq 'static' then begin
      years[i] = -1
    endif else begin
      years[i] = spl[nspl-1]
      fnames[i] = STRMID(fnames[i], 0, N_ELEMENTS(BYTE(fnames[i]))-5)
    endelse
  endfor
  years = years[UNIQ(years, sort(years))]
  years = years[where(years gt 0)]
  nyears = N_ELEMENTS(years)
  self.oyears = PTR_NEW(years)
  self->setYears
  
  so = sort(fnames)
  fnames = fnames[so]
  file_list = file_list[so]
  self.files = PTR_NEW(file_list)
  
  uv = UNIQ(fnames)
  nv = N_ELEMENTS(uv)
  vars = REPLICATE(self->_varStruct(), nv)
  for i=0, nv-1 do begin
    v = vars[i]
    v.type = FILE_BASENAME(FILE_DIRNAME(file_list[uv[i]]))
    if v.type eq '2d_alternate' then v.type = '2d'
      
    id = NCDF_OPEN(file_list[uv[i]])
    NCDF_ATTGET, id , 'VARNAME', Value, /GLOBAL
    v.name = STRING(Value)
    NCDF_ATTGET, id, v.name, 'long_name', value
    v.description = STRING(Value)
    NCDF_ATTGET, id, v.name, 'units', value
    v.unit = STRING(Value)
    if v.type eq '3d_press' and ~ PTR_VALID(self.pressure_levels) then begin
      NCDF_VARGET, id, 'pressure', value
      PTR_FREE, self.pressure_levels
      self.pressure_levels = PTR_NEW(value)
    endif
    if v.type eq '3d_soil' and ~ PTR_VALID(self.soil_levels) then begin
      NCDF_VARGET, id, 'soil', value
      PTR_FREE, self.soil_levels
      self.soil_levels = PTR_NEW(value)
    endif
    NCDF_CLOSE, id    
    
    if v.type eq 'static' or v.type eq '2d' then begin
      v.id = v.name
    endif else begin
      v.id = v.name + '_' + STRMID(v.type, 3, N_ELEMENTS(BYTE(v.type))-3)
    endelse
    p = where(fnames eq fnames[uv[i]], cnt)
    v.pos[0:cnt-1] = p
    vars[i] = v
  endfor
  self.vars = PTR_NEW(vars)
  
  self->_addDerivedVars
  
  w = OBJ_NEW('w_WRF', file=file_list[0])
  ok = self->w_GISdata::init(w, _EXTRA=extra)
  undefine, w
  if ~ ok then return, 0
  
  return, 1
  
end

;+
; :Description:
;    Destroy the object instance
;
;-
pro w_WPR::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_gisdata::Cleanup 
  PTR_FREE, self.vars
  PTR_FREE, self.files
  PTR_FREE, self.time
  PTR_FREE, self.years
  PTR_FREE, self.oyears
  PTR_FREE, self.pressure_levels
  PTR_FREE, self.soil_levels
  OBJ_DESTROY, self.objs 

end

;+
; :Description:
;    Var Info structure
;
; :Private:
; 
; :Keywords:
;   DERIVED: in, optional
;            set if the variable is derived
; 
;-
function w_WPR::_varStruct, DERIVED=derived

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if KEYWORD_SET(DERIVED) then d = 1 else d = 0 
  
  return,   {  id:'' , $
               name:'' , $
               unit:'' , $
               description:'' , $
               type: '' , $ ; 2d, 3d_eta, 3d_soil, 3d_press, static, diag
               open: 0 , $ 
               derived: d , $ 
               pos: LONARR(N_ELEMENTS(*self.years))-1 $             
               }
               
end

;+
; :Description:
;    Time from product info
;
; :Private:
; 
;-
function w_WPR::_makeTime, years
  
  nyears = N_ELEMENTS(years)
  
  case self.tres of
    'h': begin
      if self.hres eq 'd30km' then h=3 else h=1
      for i=0, nyears-1 do begin
        t0 = QMS_TIME(year=years[i],month=1,day=1,hour=h)
        t1 = QMS_TIME(year=years[i]+1,month=1,day=1,hour=0)
        _t = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(HOUR=h), NSTEPS=nt)
        if N_ELEMENTS(time) eq 0 then time = _t else time = [time, _t]
      endfor
    end
    'd': begin
      for i=0, nyears-1 do begin
        t0 = QMS_TIME(year=years[i],month=1,day=1)
        t1 = QMS_TIME(year=years[i],month=12,day=31)
        _t = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(day=1), NSTEPS=nt)
        if N_ELEMENTS(time) eq 0 then time = _t else time = [time, _t]
      endfor
    end
    'm': begin
      for i=0, nyears-1 do begin
        t0 = QMS_TIME(year=years[i],month=1,day=1,hour=0)
        t1 = QMS_TIME(year=years[i],month=12,day=1,hour=0)
        _t = MAKE_ENDED_TIME_SERIE(t0, t1, MONTH=1, NSTEPS=nt)
        if N_ELEMENTS(time) eq 0 then time = _t else time = [time, _t]
      endfor
    end
    'y': begin
      for i=0, nyears-1 do begin
        _t = QMS_TIME(year=years[i],month=1,day=1,hour=0)
        if N_ELEMENTS(time) eq 0 then time = _t else time = [time, _t]
      endfor
    end
  endcase
  
  return, time
  
end

;+
; :Description:
;    Adds the diagnostic variables info to the "real" ones
;
; :Private:
; 
;-
pro w_WPR::_addDerivedVars

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  ;First remove old diagvars if someone made a second call to the routine
  vars = *self.vars
  dummy = where(vars.derived eq 1, COMPLEMENT=c, NCOMPLEMENT=nc)
  if nc eq 0 then Message, 'Big problem'
  vars = vars[c]
  
  
  if PTR_VALID(self.pressure_levels) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'pressure_press'
    v.name = 'pressure'
    v.unit = 'hPa'
    v.description = 'Full model pressure'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('geopotential_eta')
  if d1 then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'z_eta'
    v.name = 'z'
    v.unit = 'm'
    v.description = 'Full model height'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    d1 = self->hasVar('hgt')
    if d1 then begin
      v = self->_varStruct(/DERIVED)
      v.id = 'zag_eta'
      v.name = 'z'
      v.unit = 'm'
      v.description = 'Full model height above ground'
      v.type = '3d_eta'
      if ~ self->hasVar(v.id) then vars = [vars,v]
    endif
  endif
  d1 = self->hasVar('geopotential_press')
  if d1 then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'z_press'
    v.name = 'z'
    v.unit = 'm'
    v.description = 'Full model height'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    d1 = self->hasVar('hgt')
    if d1 then begin
      v = self->_varStruct(/DERIVED)
      v.id = 'zag_press'
      v.name = 'z'
      v.unit = 'm'
      v.description = 'Full model height above ground'
      v.type = '3d_press'
      if ~ self->hasVar(v.id) then vars = [vars,v]
    endif
  endif
  
  d1 = self->hasVar('theta_eta')
  d2 = self->hasVar('pressure_eta')
  if (d1 and d2) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'tk_eta'
    v.name = 'tk'
    v.unit = 'K'
    v.description = 'Temperature'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    v = self->_varStruct(/DERIVED)
    v.id = 'tc_eta'
    v.name = 'tc'
    v.unit = 'C'
    v.description = 'Temperature'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    if self->hasVar('hgt') then begin
      v = self->_varStruct(/DERIVED)
      v.id = 't2pbl'
      v.name = 't2pbl'
      v.unit = 'K'
      v.description = '2 m temperature (extrapolated from eta-levels)'
      v.type = '2d'
      if ~ self->hasVar(v.id) then vars = [vars,v]
      v = self->_varStruct(/DERIVED)
      v.id = 't2pblc'
      v.name = 't2pblc'
      v.unit = 'C'
      v.description = '2 m temperature (extrapolated from eta-levels)'
      v.type = '2d'
      if ~ self->hasVar(v.id) then vars = [vars,v]
    endif
  endif  
      
  d1 = self->hasVar('theta_press')
  if (d1) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'tk_press'
    v.name = 'tk'
    v.unit = 'K'
    v.description = 'Temperature'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    v = self->_varStruct(/DERIVED)
    v.id = 'tc_press'
    v.name = 'tc'
    v.unit = 'C'
    v.description = 'Temperature'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif      
  
  d1 = self->hasVar('qvapor_eta')
  d2 = self->hasVar('pressure_eta')
  if (d1 and d2) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'td_eta'
    v.name = 'td'
    v.unit = 'C'
    v.description = 'Dewpoint Temperature'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif      
  
  d1 = self->hasVar('qvapor_press')
  if (d1) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'td_press'
    v.name = 'td'
    v.unit = 'C'
    v.description = 'Dewpoint Temperature'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif    
  
  d1 = self->hasVar('psfc')
  d2 = self->hasVar('q2')
  if (d1 and d2) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'td2'
    v.name = 'td2'
    v.unit = 'C'
    v.description = '2m Dewpoint Temperature'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif      

  d1 = self->hasVar('t2')
  if (d1) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 't2c'
    v.name = 't2c'
    v.unit = 'C'
    v.description = '2m Temperature'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif  
      
  d1 = self->hasVar('theta_eta')
  d2 = self->hasVar('pressure_eta')
  d3 = self->hasVar('qvapor_eta')
  if (d1 and d2 and d3) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'rh_eta'
    v.name = 'rh'
    v.unit = '%'
    v.description = 'Relative Humidity'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif    
  
  d1 = self->hasVar('u_eta')
  d2 = self->hasVar('v_eta')
  d3 = ~ self->hasVar('ws_eta')
  if (d1 and d2 and d3) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'ws_eta'
    v.name = 'ws'
    v.unit = 'm.s-1'
    v.description = 'Horizontal wind speed'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif    
  
  d1 = self->hasVar('u_press')
  d2 = self->hasVar('v_press')
  d3 = ~ self->hasVar('ws_press')
  if (d1 and d2 and d3) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'ws_press'
    v.name = 'ws'
    v.unit = 'm.s-1'
    v.description = 'Horizontal wind speed'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif    
  
  d1 = self->hasVar('theta_press')
  d2 = self->hasVar('qvapor_press')
  if (d1 and d2) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'rh_press'
    v.name = 'rh'
    v.unit = '%'
    v.description = 'Relative Humidity'
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif    

  d1 = self->hasVar('col_qliquid')
  d2 = self->hasVar('col_qsolid')
  if (d1 and d2) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'col_qcloud'
    v.name = 'col_qcloud'
    v.unit = 'kg kg-1'
    v.description = 'Total column cloud water mixing ratio (qliquid + qsolid)'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif    

  d1 = self->hasVar('t2')
  d2 = self->hasVar('q2')
  d3 = self->hasVar('psfc')
  if (d1 and d2 and d3) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'rh2'
    v.name = 'rh2'
    v.unit = '%'
    v.description = '2 m Relative Humidity'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('prcp')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp_sum'
    v.name = 'prcp_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total precipitation'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('prcp_nc')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp_nc_sum'
    v.name = 'prcp_nc_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total grid-scale precipitation'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('prcp_c')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp_c_sum'
    v.name = 'prcp_c_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total convective precipitation'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('prcp_fr')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp_fr_sum'
    v.name = 'prcp_fr_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total frozen precipitation'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('snowfall')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'snowfall_sum'
    v.name = 'snowfall_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total snowfall precipitation'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('graupel')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'graupel_sum'
    v.name = 'graupel_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total graupel'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('hail')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'hail_sum'
    v.name = 'hail_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total hail'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('et')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'et_sum'
    v.name = 'et_sum'
    v.unit = 'mm ' + self.tres + '-1'
    v.description = 'Total evapotranspiration'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('u10')
  d2 = self->hasVar('v10')
  if (d1 and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'ws10'
    v.name = 'ws10'
    v.unit = 'm s-1'
    v.description = '10 m wind speed'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    v = self->_varStruct(/DERIVED)
    v.id = 'wd10'
    v.name = 'wd10'
    v.unit = 'degrees'
    v.description = '10 m wind direction'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    d1 = self->hasVar('cosalpha')
    d2 = self->hasVar('sinalpha')
    if d1 and d2 then begin
      v = self->_varStruct(/DERIVED)
      v.id = 'umet10'
      v.name = 'umet10'
      v.unit = 'm s-1'
      v.description = 'U component of 10m wind rotated to earth coordinates'
      v.type = '2d'
      if ~ self->hasVar(v.id) then vars = [vars,v]
      v = self->_varStruct(/DERIVED)
      v.id = 'vmet10'
      v.name = 'vmet10'
      v.unit = 'm s-1'
      v.description = 'V component of 10m wind rotated to earth coordinates'
      v.type = '2d'
      if ~ self->hasVar(v.id) then vars = [vars,v]
      v = self->_varStruct(/DERIVED)
      v = self->_varStruct(/DERIVED)
      v.id = 'wdmet10'
      v.name = 'wdmet10'
      v.unit = 'degrees'
      v.description = '10m wind direction rotated to earth coordinates'
      v.type = '2d'
      if ~ self->hasVar(v.id) then vars = [vars,v]
    endif    
  endif
  
  PTR_FREE, self.vars 
  self.vars = PTR_NEW(vars)
  
end

;+
; :Description:
;    Get access to some params. 
;
; :Keywords:
;    HRES: out, optional
;          product vertical resolution
;    TRES: out, optional
;          product time resolution
;    DIRECTORY: out, optional
;              product directory
;    _Ref_Extra: out
;                all parent classed property
;                
;-      
pro w_WPR::GetProperty,  $
    HRES=hres,  $
    TRES=tres,  $
    DIRECTORY=directory,  $
    _Ref_Extra=extra
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  IF Arg_Present(HRES) THEN hres = self.hres
  IF Arg_Present(TRES) THEN tres = self.tres
  IF Arg_Present(DIRECTORY) THEN directory = self.directory
  
  self->w_GISdata::GetProperty, _Extra=extra
  
end

;+
; :Description:
;    This set the period of interest to a set of years
;    and prevents to use getVarData(YEARS=years) all
;    the time. Since the object remmbers this choice, 
;    use setYears without argument to reset to the original
;    period.
;
; :Params:
;    years: in, array
;           the years of the period of interest
;           if no argument, set to original period
;           
;-
pro w_WPR::setYears, years

  PTR_FREE, self.years
  if N_ELEMENTS(years) eq 0 then begin
    self.years = PTR_NEW(*self.oyears)
  endif else begin
    self.years = PTR_NEW(years)
  endelse
  
  ; Time
  PTR_FREE, self.time
  self.time = PTR_NEW(self->_makeTime(*self.years))
  
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
pro w_WPR::getTime, time, nt, t0, t1

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
;    If data on pressure levels is available, then this function gives you access to the 
;    product's available levels
;
; :Keywords:
;    COUNT: out, optional
;           the number of levels
;           
; :Returns:
;   An array of pressure levels
;
;-
function w_WPR::getPressureLevels, COUNT=count

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if PTR_VALID(self.pressure_levels) then out = *self.pressure_levels
  count = N_ELEMENTS(out)
  
  return, out
  
end

;+
; :Description:
;    If data on soil levels is available, then this function gives you access to the 
;    product's available levels
;
; :Keywords:
;    COUNT: out, optional
;           the number of levels
;           
; :Returns:
;   An array of soil levels
;
;-
function w_WPR::getSoilLevels, COUNT=count

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if PTR_VALID(self.soil_levels) then out = *self.soil_levels
  count = N_ELEMENTS(out)
  
  return, out
  
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
;    TOFILE: in, optional
;            set this keyword with a path to a scv file were the 
;            variables will be printed out
;    NODERIVED: in, optional
;               set this keyword to get only the original variables
;    VARINFO: out, optional
;             a structure containg the variables information
;           
; :Returns:
;   An array of variable ids
;
;-
function w_WPR::getVarNames, COUNT=count, PRINT=print, TOFILE=tofile, NODERIVED=noderived, VARINFO=varinfo

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  vars = *self.vars
  count = N_ELEMENTS(vars)
   
  if KEYWORD_SET(NODERIVED) then begin
   pok = where(~ vars.derived, count)
   if count eq 0 then return, ''
   vars = vars[pok]
  endif
  
  out = vars.id
  
  if KEYWORD_SET(PRINT) then begin
    print, '   ID                 NAME            DESCRIPTION                                       UNIT       TYPE'
    
    for i = 0L, count-1 do begin
      v = vars[i]
      ns = '                                                                                                                                  '
      STRPUT, ns, str_equiv(i), 0
      STRPUT, ns, v.id, 4
      STRPUT, ns, v.name, 5+18
      STRPUT, ns, v.description, 21+18
      STRPUT, ns, v.unit, 71+18
      t = v.type
      if v.derived then t += ' (derived)'
      STRPUT, ns, t, 82+18
      print, ns
    endfor
  endif
  
  if N_ELEMENTS(TOFILE) ne 0 then begin
    header = ['ID','NAME','DESCRIPTION','UNIT','TYPE','DIAGNOSTIC']
    str = STRARR(6,count)
    for i = 0L, count-1 do begin
      v = vars[i]
      str[0,i] = v.id
      str[1,i] = v.name
      str[2,i] = v.description
      str[3,i] = v.unit
      str[4,i] = v.type
      if v.derived then str[5,i] = 'YES' else str[5,i] = 'NO'
    endfor
    WRITE_CSV, tofile, str, HEADER=header
  endif
  
  
  varinfo = vars
  return, out
  
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
function w_WPR::hasVar, id, INFO=info
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  undefine, info
  
  n = self->GetVarNames()  
  p = where(str_equiv(n) eq str_equiv(id), cnt) 
  
  if cnt eq 0 then return, 0
  
  v = (*self.vars)[p]
  info = {id:v.id, name:v.name, description:v.description, unit:v.unit, type:v.type, derived:v.derived}
  
  return, 1

end

;+
; :Description:
;    Get access to a single product file object (w_geo_nc)
;
; :Params:
;    id: in, required
;        the variable ID
;    year: in, required
;          the requested year
;          
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data
; 
; :Returns:
;     the object
;
;-
function w_WPR::getVarObj, id, year, INFO=info

  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  if info.derived then Message, 'Variable is derived. No object found: ' + str_equiv(id)
  
  v = *self.vars
  pv = where(str_equiv(v.id) eq str_equiv(id))
  v = v[pv[0]]
  
  ; It is a product variable
  if ~ v.open then begin
    files = (*self.files)[v.pos[where(v.pos ne -1, nf)]]
    for i = 0, nf-1 do self.objs->Add, OBJ_NEW('w_GEO_nc', FILE=files[i])
    v.open = 1
    (*self.vars)[pv[0]] = v
  endif    
  
  obj = self.objs->FindByVar(id, year, COUNT=count)
  if count eq 0 then Message, 'Found no file with id: ' + str_equiv(id) + ' and year: ' + str_equiv(year)
  if count gt 1 then Message, 'Found more than one file with id: ' + str_equiv(id) + ' and year: ' + str_equiv(year)

  return, obj

end

;+
; :Description:
;    Get the data for a specific variable, at the dimensions of the subset.
;    
;    Additionaly to the product variables, some derived variables are computed
;    (depending on the available product variables)::
;    
;         ID                NAME            DESCRIPTION                                       UNIT       TYPE
;       - pressure_press     pressure        Full model pressure                               hPa        3d_press (derived)             
;       - z_eta              z               Full model height                                 m          3d_eta (derived)               
;       - zag_eta            z               Full model height above ground                    m          3d_eta (derived)               
;       - z_press            z               Full model height                                 m          3d_press (derived)             
;       - zag_press          z               Full model height above ground                    m          3d_press (derived)             
;       - tk_eta             tk              Temperature                                       K          3d_eta (derived)               
;       - tc_eta             tc              Temperature                                       C          3d_eta (derived)               
;       - t2pbl              t2pbl           2 m temperature (extrapolated from eta-levels)    K          2d (derived)                   
;       - t2pblc             t2pblc          2 m temperature (extrapolated from eta-levels)    C          2d (derived)                   
;       - tk_press           tk              Temperature                                       K          3d_press (derived)             
;       - tc_press           tc              Temperature                                       C          3d_press (derived)             
;       - td_eta             td              Dewpoint Temperature                              C          3d_eta (derived)               
;       - td_press           td              Dewpoint Temperature                              C          3d_press (derived)             
;       - td2                td2             2m Dewpoint Temperature                           C          2d (derived)                   
;       - t2c                t2c             2m Temperature                                    C          2d (derived)                   
;       - rh_eta             rh              Relative Humidity                                 %          3d_eta (derived)               
;       - ws_eta             ws              Horizontal wind speed                             m.s-1      3d_eta (derived)               
;       - ws_press           ws              Horizontal wind speed                             m.s-1      3d_press (derived)             
;       - rh_press           rh              Relative Humidity                                 %          3d_press (derived)             
;       - rh2                rh2             2 m Relative Humidity                             %          2d (derived)                   
;       - ws10               ws10            10 m wind speed                                   m s-1      2d (derived)                   
;       - wd10               wd10            10 m wind direction                               degrees    2d (derived)                   
;       - umet10             umet10          U component of 10m wind rotated to earth coordinatm s-1      2d (derived)                   
;       - vmet10             vmet10          V component of 10m wind rotated to earth coordinatm s-1      2d (derived)                   
;       - wdmet10            wdmet10         10m wind direction rotated to earth coordinates   degrees    2d (derived) ;
; 
; :Params:
;    id: in, required
;        the variable ID
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
;    YEARS: in, optional
;           array of one or more elements containing selected years
;           for example YEARS=[2002,2008,2009] or YEARS=2011
;    ZLEVELS: in, optional
;             if you want to get the data for specific Z levels only 
;             (e.g: ZLEVELS=0 for first eta/pressure level, ZLEVELS=[0,12] for the first twelve)
;    T0: in, optional, type = qms/{ABS_DATE}
;        set this keyword to a date to obtain a subset of the WPR timeserie 
;        starting at T0 (or after, if T0 is not found)
;    T1: in, optional, type = qms/{ABS_DATE}
;        set this keyword to a date to obtain a subset of the WPR timeserie 
;        ending at T1 (or before, if T1 is not found)
;    MONTH: in, optional, type = long
;           set this keyword to a date to obtain a subset of the WPR timeserie 
;           for a specific month only
;    HOUROFDAY: in, optional, type = long
;               set this keyword to a date to obtain a subset of the WPR timeserie 
;               for a specific hour of day only
;            
; :Returns:
;   the data array
;   
;-
function w_WPR::getVarData, id, time, nt, INFO=info, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month, HOUROFDAY=hourofday

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_Error, 2
  
  undefine, info, time, nt
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
    
  if info.derived then begin
    if STRMID(str_equiv(id), N_ELEMENTS(byte(id))-4, 4) eq '_SUM' then begin      
      _id = utils_replace_string(str_equiv(id), '_SUM', '')
      value = self->GetVarData(_id, time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
      case self.tres of
        'h': 
        'd': value = value * 24.
        'm': begin
          ad = MAKE_ABS_DATE(QMS=time)
          nh = GEN_month_days(ad.month, ad.year) * 24.
          for i=0, nt-1 do value[*,*,i] = value[*,*,i] * nh[i]
        end
        'y': begin
          ad = MAKE_ABS_DATE(QMS=time)
          nh = (365. + GEN_switch_year(ad.year)) * 24.
          for i=0, nt-1 do value[*,*,i] = value[*,*,i] * nh[i]
        end
      endcase
      return, temporary(value)    
    endif
    
    ; Its a derived variable that we have to compute
    case str_equiv(id) of
      'Z_ETA': begin
        return, self->GetVarData('geopotential_eta', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) / 9.81
      end
      'ZAG_ETA': begin
        z = self->GetVarData('Z_ETA', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        dims = SIZE(z, /DIMENSIONS)
        _dims = dims & _dims[2:*] = 1
        ter =  rebin(reform(self->GetVarData('HGT'),_dims), dims) ; make it same dim as z
        return, TEMPORARY(z) - TEMPORARY(ter)
      end
      'Z_PRESS': begin
        return, self->GetVarData('geopotential_press', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) / 9.81
      end
      'ZAG_PRESS': begin
        z = self->GetVarData('Z_PRESS', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        dims = SIZE(z, /DIMENSIONS)
        _dims = dims & _dims[2:*] = 1
        ter =  rebin(reform(self->GetVarData('HGT'),_dims), dims) ; make it same dim as z
        return, TEMPORARY(z) - TEMPORARY(ter)
      end
      'TK_ETA': begin
        p = self->GetVarData('pressure_eta', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) * 100.
        t = self->GetVarData('theta_eta', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_tk(TEMPORARY(p),TEMPORARY(t))
      end
      'TC_ETA': begin
        return, self->GetVarData('tk_eta', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) - 273.15
      end
      'PRESSURE_PRESS': begin
        levs = self->GetPressureLevels()
        if N_ELEMENTS(YEARS) ne 0 then _y = years else _y = *self.years
        time = self->_makeTime(_y)
        nt = N_ELEMENTS(time)
        if N_ELEMENTS(ZLEVELS) eq 1 then levs = levs[zlevels]
        if N_ELEMENTS(ZLEVELS) eq 2 then levs = levs[zlevels[0]:zlevels[1]]
        nl = N_ELEMENTS(levs)
        if n_elements(t0) gt 0 or n_elements(t1) gt 0 then begin
          p0 = 0 > min(where(time ge t0)) < (n_elements(time)-1)
          p1 = 0 > max(where(time le t1)) < (n_elements(time)-1)
          nt = (p1 - p0) + 1
        endif
        out = FLTARR(self.tnt_c.nx, self.tnt_c.ny, nl, nt)
        for i=0, nl-1 do out[*,*,i,*] = levs[i]
        return, out
      end
      'TK_PRESS': begin
        p = self->GetVarData('pressure_press', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) * 100.
        t = self->GetVarData('theta_press', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_tk(TEMPORARY(p),TEMPORARY(t))
      end
      'TC_PRESS': begin
        return, self->GetVarData('tk_press', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) - 273.15
      end
      'T2PBL': begin
        tk = self->getVarData('tk_eta', time, nt, YEARS=years, ZLEVELS=[0,1], T0=t0, T1=t1, MONTH=month)
        dims = SIZE(tk, /DIMENSIONS)
        _dims = dims & _dims[2:*] = 1
        ter =  rebin(reform(self->GetVarData('hgt'),_dims), dims) ; make it same dim as z
        h = self->GetVarData('z_ETA', ZLEVELS=[0,1], YEARS=years, T0=t0, T1=t1) - TEMPORARY(ter)
        return, reform(utils_wrf_intrp3d(TEMPORARY(tk), TEMPORARY(h), 2., /EXTRAPOLATE))
      end
      'T2PBLC': begin
        return, self->GetVarData('t2pbl', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) - 273.15
      end
      'TD_ETA': begin
        p = self->GetVarData('pressure_eta', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) *100.
        qvapor = self->GetVarData('qvapor_eta', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'TD_PRESS': begin
        p = self->GetVarData('pressure_press', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) *100.
        qvapor = self->GetVarData('qvapor_press', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'TD2': begin
        p = self->GetVarData('psfc', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        qvapor = self->GetVarData('q2', YEARS=years, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'T2C': begin
        return, self->GetVarData('t2', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month) - 273.15
      end
      'RH_ETA': begin
        TK = self->GetVarData('tk_eta', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        P = self->GetVarData('pressure_eta', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) *100.
        QVAPOR = self->GetVarData('qvapor_eta', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'RH_PRESS': begin
        TK = self->GetVarData('tk_press', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        P = self->GetVarData('pressure_press', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month) *100.
        QVAPOR = self->GetVarData('qvapor_press', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'WS_ETA': begin
        u = self->GetVarData('u_eta', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        v = self->GetVarData('v_eta', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, sqrt(TEMPORARY(u)^2+TEMPORARY(v)^2)
      end
      'WS_PRESS': begin
        u = self->GetVarData('u_press', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        v = self->GetVarData('v_press', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, sqrt(TEMPORARY(u)^2+TEMPORARY(v)^2)
      end
      'COL_QCLOUD': begin
        q1 = self->GetVarData('col_qliquid', time, nt, YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        q2 = self->GetVarData('col_qsolid', YEARS=years, ZLEVELS=zlevels, T0=t0, T1=t1, MONTH=month)
        return, q1+q2
      end
      'RH2': begin
        TK = self->GetVarData('t2', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        P = self->GetVarData('psfc', YEARS=years, T0=t0, T1=t1, MONTH=month)
        QVAPOR = self->GetVarData('q2', YEARS=years, T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'UMET10': begin
        u10 = self->GetVarData('U10', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('V10', YEARS=years, T0=t0, T1=t1, MONTH=month)
        cosalpha = self->GetVarData('COSALPHA')
        sinalpha = self->GetVarData('SINALPHA')
        if SIZE(cosalpha, /N_DIMENSIONS) ne size(u10, /N_DIMENSIONS) then begin ; has been cropped
          dims = SIZE(u10, /DIMENSIONS)
          _dims = dims & _dims[2:*] = 1
          cosalpha =  rebin(reform(TEMPORARY(cosalpha),_dims), dims) ; make it same dim
          sinalpha =  rebin(reform(TEMPORARY(sinalpha),_dims), dims) ; make it same dim
        endif
        return, TEMPORARY(u10)*TEMPORARY(cosalpha) + TEMPORARY(v10)*TEMPORARY(sinalpha)
      end
      'VMET10': begin
        u10 = self->GetVarData('U10', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('V10', YEARS=years, T0=t0, T1=t1, MONTH=month)
        cosalpha = self->GetVarData('COSALPHA')
        sinalpha = self->GetVarData('SINALPHA')
        if SIZE(cosalpha, /N_DIMENSIONS) ne size(u10, /N_DIMENSIONS) then begin ; has been cropped
          dims = SIZE(u10, /DIMENSIONS)
          _dims = dims & _dims[2:*] = 1
          cosalpha =  rebin(reform(TEMPORARY(cosalpha),_dims), dims) ; make it same dim
          sinalpha =  rebin(reform(TEMPORARY(sinalpha),_dims), dims) ; make it same dim
        endif
        return, TEMPORARY(v10)*TEMPORARY(cosalpha) - TEMPORARY(u10)*TEMPORARY(sinalpha)
      end
      'WS10': begin
        u10 = self->GetVarData('U10', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('V10', YEARS=years, T0=t0, T1=t1, MONTH=month)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WS=value
        return, value
      end
      'WD10': begin
        u10 = self->GetVarData('U10', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('V10', YEARS=years, T0=t0, T1=t1, MONTH=month)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WD=value
        return, value
      end
      'WDMET10': begin
        u10 = self->GetVarData('Umet10', time, nt, YEARS=years, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('Vmet10', YEARS=years, T0=t0, T1=t1, MONTH=month)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WD=value
        return, value
      end
      else: Message, 'No'
    endcase
    
  endif else begin
  
    
    if info.type eq 'static' then begin
      obj = self->getVarObj(id)
      if TOTAL(self.subset) ne 0 then ok = obj->define_subset(SUBSET=self.subset) else ok = obj->define_subset()
      out = obj->get_Var(info.name, time, nt)
    endif else begin
      ; Some time subsetting
      do_ts = N_ELEMENTS(T0) eq 1 or N_ELEMENTS(T1) eq 1
      do_y = N_ELEMENTS(YEARS) ne 0
      do_m = N_ELEMENTS(MONTH) ne 0
      do_h = N_ELEMENTS(HOUROFDAY) ne 0
      if (do_y and do_ts) then message, 'Incompatible keywords (T0 or T1 and YEARS)'
      if (do_m and do_ts) then message, 'Incompatible keywords (T0 or T1 and MONTH)'
      if do_h and self.tres ne 'h' then message, 'Incompatible keywords (HOUROFDAY) with product time resolution: ' + self.tres
      
      _y = *self.years
      if do_y then _y = years
      if do_ts then begin
        self->getTime, ot, cnot
        p0 = 0
        p1 = cnot-1
        if check_WTIME(t0, OUT_QMS=it0) and cnot gt 1 then p0 = 0 > min(where(ot ge it0)) < (cnot-1)
        if check_WTIME(t1, OUT_QMS=it1) and cnot gt 1 then p1 = 0 > max(where(ot le it1)) < (cnot-1)
        if self.tres eq 'h' then ot = MAKE_ABS_DATE(QMS=ot[p0:p1]-H_QMS) $
           else ot = MAKE_ABS_DATE(QMS=ot[p0:p1]) ;TODO: check this! Tom removed the -H_QMS wrong dates the -HQMS is because of the last element in year
        _y = (ot.year)[uniq(ot.year, sort(ot.year))]
      endif
      if do_m then begin
        if N_ELEMENTS(month) ne 1 then Message, WAVE_Std_Message('MONTH', NELEMENTS=1)
        dummy = where(month gt 12 or month lt 1, nok)
        if nok ne 0 then Message, WAVE_Std_Message('MONTH', /RANGE)
        if self.tres eq 'y' then Message, '$MONTH not compatible with yearly products'
      endif
      
      ; let's loop over the yearly files
      for y=0, N_ELEMENTS(_y)-1 do begin
        obj = self->getVarObj(id, (_y)[y])
        ; Subset spatially if needed
        if TOTAL(self.subset) ne 0 then ok = obj->define_subset(SUBSET=self.subset) else ok = obj->define_subset()
        ; Subset in time if needed
        undefine, _t0, _t1
        if do_ts then begin
          obj->get_Time, ot, cnot
          p0 = 0
          p1 = cnot-1
          if check_WTIME(t0, OUT_QMS=it0) and cnot gt 1 then p0 = 0 > min(where(ot ge it0)) < (cnot-1)
          if check_WTIME(t1, OUT_QMS=it1) and cnot gt 1 then p1 = 0 > max(where(ot le it1)) < (cnot-1)
          _t0 = ot[p0]
          _t1 = ot[p1]          
        endif
        if do_m then begin
          obj->get_Time, ot, cnot
          ot = MAKE_ABS_DATE(QMS=ot)
          pmo = where(ot.month eq month, ntmo)
          if ntmo eq 0 then Message, 'Huge error'
          _t0 = ot[min(pmo)]
          _t1 = ot[max(pmo)]          
        endif
        tmp = reform(obj->get_Var(info.name, t, ZLEVELS=zlevels, T0=_t0, T1=_t1, HOUROFDAY=hourofday))
        s = SIZE(tmp, /DIMENSIONS)
        nd = N_ELEMENTS(s)                
        if (STRMID(info.type, 0, 2) eq '2d') or ((STRMID(info.type, 0, 2) eq '3d') and (n_elements(zlevels) eq 1)) then begin
          case nd of
            1: if N_ELEMENTS(out) eq 0 then out = TEMPORARY(tmp) else out = [out,TEMPORARY(tmp)]
            else: if N_ELEMENTS(out) eq 0 then out = TEMPORARY(tmp) else out = [[[out]],[[TEMPORARY(tmp)]]]
          endcase
        endif else if self.tnt_C.nx eq 1 and self.tnt_C.ny eq 1 then begin
          if N_ELEMENTS(out) eq 0 then out = TEMPORARY(tmp) else out = [[out],[TEMPORARY(tmp)]]
        endif else begin
          case nd of
            2: if N_ELEMENTS(out) eq 0 then out = TEMPORARY(tmp) else out = [[out],[TEMPORARY(tmp)]]
            3: begin
              tmp = reform(tmp, [s,1])
              if N_ELEMENTS(out) eq 0 then begin
                out = TEMPORARY(tmp)
              endif else begin
                sout = size(out, /DIMENSIONS)
                stmp = size(tmp, /DIMENSIONS)
                out_ = TEMPORARY(out)
                out = FLTARR(sout+[0,0,0,stmp[3]])
                out[*,*,*,0:sout[3]-1] = TEMPORARY(out_)
                out[*,*,*,sout[3]:*] = TEMPORARY(tmp)
              endelse
            end
            4: begin
              if N_ELEMENTS(out) eq 0 then begin
                out = TEMPORARY(tmp)
              endif else begin
                sout = size(out, /DIMENSIONS)
                stmp = size(tmp, /DIMENSIONS)
                out_ = TEMPORARY(out)
                out = FLTARR(sout+[0,0,0,stmp[3]])
                out[*,*,*,0:sout[3]-1] = TEMPORARY(out_)
                out[*,*,*,sout[3]:*] = TEMPORARY(tmp)
              endelse
            end            
          endcase
        endelse        
        if N_ELEMENTS(time) eq 0 then time = t else time = [time,t]
      endfor
      nt = N_ELEMENTS(time)
    endelse
    
    if N_ELEMENTS(out) eq 1 then out = out[0]
    return, reform(TEMPORARY(out))
    
  endelse
  
end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_WPR__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_WPR                               ,  $
            INHERITS w_GISdata                  ,  $
            tres:               ''              ,  $ ; type of active directory: h, d, m, y
            hres:               ''              ,  $ ; type of active directory: d30km, d10km, d02km
            directory:          ''              ,  $ ; path to the file directory
            pressure_levels:    PTR_NEW()       ,  $ ; if 3d_press in the directory, the pressure levels
            soil_levels:        PTR_NEW()       ,  $ ; if 3d_soil in the directory, the soil levels
            vars:               PTR_NEW()       ,  $ ; array of var structures (see w_WPR::_varStruct)
            files:              PTR_NEW()       ,  $ ; Path to the files
            time:               PTR_NEW()       ,  $ ; Time array in QMS
            oyears:             PTR_NEW()       ,  $ ; The available product years
            years:              PTR_NEW()       ,  $ ; The product years set by user
            objs:               OBJ_NEW()          $ ; the ncdf objects 
            }
    
end