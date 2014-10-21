;+
; :Description:
;    Initialize the object instance
;
; :Keywords:
;    DIRECTORY: in, required
;               the path to the WRF files directory. either m, d, h or y
;    _EXTRA: in, optional
;            any keyword accepted by `w_GISdata::defineSubset`
;
;
; :Returns:
;    1 if the object is created successfully, 0 if not
;
;-
function wa_WPR::init, DIRECTORY=directory, YEAR=year, _EXTRA=extra

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
  if N_ELEMENTS(directory) eq 0 then message, 'File directory does not exist' 
  if directory eq '' then MESSAGE, WAVE_Std_Message(/FILE)
  if ~ FILE_TEST(directory, /DIRECTORY) then MESSAGE, 'No files in directory'
  
  dir = utils_clean_path(directory)
  prdir = FILE_DIRNAME(dir)

  if KEYWORD_SET(TRES) then tres=tres else tres = FILE_BASENAME(dir)
  hres = FILE_BASENAME(prdir)
  str = STRSPLIT(dir, '/', /EXTRACT, COUNT=nstr)
  statpos = where(str eq 'POST_OUT', cntp)
  if cntp eq 0 then MEssage, 'POST_OUT not found'
  statdir = '/'+strjoin(str[0:statpos],'/')+'/static'
  expe = str[statpos+1]
  if ~ FILE_TEST(statdir, /DIRECTORY) then Message, 'Cannot find a static directory.' + $
    ' Be sure you are at the right place in the product directory structure'
  if (tres ne 'h') and (tres ne 'd') and (tres ne 'm') and  (tres ne 'y') then Message, 'Cannot understand timestep.' + $
    ' Be sure you are at the right place in the product directory structure'
  
  self.hres=hres
  self.domain='d01' 
  self.tres = tres
  self.hres = hres
  self.directory = dir
  self.expe = expe
  self.years = year
  
  file_list=FILE_SEARCH(self.directory, '*_' + self.domain +'*'+self.years+'.nc', count=filecnt)
  file_list=[file_list, FILE_SEARCH(statdir, '*_' + self.domain +'*.nc', count=filecnt)]
  if filecnt eq 0 then MESSAGE, 'No files in the directory?'
 
  ; This is for the XLAT XLON variable
  matches = Where(StrMatch(file_list, '*xlatlong*'), cm)
  if cm ne 0 then begin
    utils_array_remove, matches, file_list
    filecnt = N_ELEMENTS(file_list)
  endif
  
  fnames = FILE_BASENAME(file_list, '.nc')
  
;    years = LONARR(filecnt)
;    for i=0, filecnt-1 do begin
;      spl = STRSPLIT(fnames[i], '_', /EXTRACT, count=nspl)
;      type = FILE_BASENAME(FILE_DIRNAME(file_list[i]))
;      if type eq 'static' then begin
;        years[i] = -1
;      endif else begin
;        years[i] = spl[nspl-1]
;        fnames[i] = STRMID(fnames[i], 0, N_ELEMENTS(BYTE(fnames[i]))-5)
;      endelse
;    endfor
;    years = years[UNIQ(years, sort(years))]
;    years = years[where(years gt 0)]
;    nyears = N_ELEMENTS(years)
;    self.oyears = PTR_NEW(years)
;    self->setYears
  
  so = sort(fnames)
  fnames = fnames[so]
  file_list = file_list[so]
  self.files = PTR_NEW(file_list)
  
  self.objs = hash()
  
  uv = UNIQ(fnames)
  nv = N_ELEMENTS(uv)
  if nv ne N_ELEMENTS(file_list) then Message, 'Mini WPR has no years, so only one file per variable !!!'
  vars = REPLICATE(self->_varStruct(), nv)

  for i=0, nv-1 do begin
    v = vars[i]
    v.type = FILE_BASENAME(FILE_DIRNAME(file_list[uv[i]]))
    if v.type eq '2d_alternate' then v.type = '2d'
    ncObj = NCDF_FILE(file_list[i])    
    varNames = ncObj->GetVarNames()
    pos=where(str_equiv(varNames) eq 'TIMES', cnt)
    if cnt eq 0 then message, 'No times?? Need times variable!!'
    pos=where((str_equiv(varNames) ne 'TIMES') and (str_equiv(varNames) ne 'XLAT') and (str_equiv(varNames) ne 'XLONG'), cnt)
    if cnt ne 1 then message, 'Not ONE variable?? Need ONE variable!!'
    if v.type eq '2d' and ~ PTR_VALID(self.time) then begin
      ; use the opportunity to read time
      cdfid = ncObj->GetProperty('FILEID')
      if utils_nc_COARDS_time(cdfid, time, time0, time1, nt) then begin
        self.time = PTR_NEW(time)
      endif else if utils_wrf_time(cdfid, time, time0, time1, nt) then begin
        self.time = PTR_NEW(time)
      endif else  Message, 'Time could not be read'
    endif
    
    v.name = varNames[pos[0]]
    v.description = ncObj->GetVarAttrValue(v.name, 'description')
    v.unit = ncObj->GetVarAttrValue(v.name, 'units')
    undefine, ncObj        
    ; here possible mismasch wiht d , press blabla
    if v.type eq 'static' or v.type eq '2d' then begin
      v.id = v.name
    endif else if v.type eq '3d_eta' then begin
      v.id = v.name + '_' + STRMID(v.type, 3, N_ELEMENTS(BYTE(v.type))-3)
    endif else begin
      v.id = v.name
    endelse
    v.pos = i
    vars[i] = v
  endfor
  self.vars = PTR_NEW(vars)


  self->_addPressureLevels
  self->_addDerivedVars
  
  w = OBJ_NEW('w_WRF', FILE=statdir+'/xlatlong_'+self.domain+'.nc')
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
pro wa_WPR::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  self->w_gisdata::Cleanup
  PTR_FREE, self.time
  PTR_FREE, self.vars
  PTR_FREE, self.pressurelevels
  PTR_FREE, self.files
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
;   PL: in, optional
;            set if the pressure level variable is interpolated from WRF       
;
;-
function wa_WPR::_varStruct, DERIVED=derived, PL=pl

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
    
  return,   {  id:'' , $
    name:'' , $
    unit:'' , $
    description:'' , $
    type: '' , $ ; 2d, 3d_eta, 3d_soil, 3d_press, static
    open: 0 , $
    derived: KEYWORD_SET(DERIVED), $
    pl: KEYWORD_SET(PL), $
    pos: -1L $  ; The position in the file list !!!
  }
  
end

;+
; :Description:
;    Initialise the standard pressure levels.
;
; :Private:
;
;-
pro wa_WPR::_addPressureLevels

 self.pressurelevels = PTR_NEW([1000., 975, 925, 900, 850, 800, 700, 650, $
   600, 550, 500, 450, 400, 350, 300, 250, 200, 150, 100, 75])

end
;+
; :Description:
;    Adds the diagnostic variables info to the "real" ones
;
; :Private:
;
;-
pro wa_WPR::_addDerivedVars

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ;First remove old diagvars if someone made a second call to the routine
  vars = *self.vars
  c = where(vars.derived eq 0, nc)
  if nc eq 0 then Message, 'Big problem'
  vars = vars[c]
 
 
  ;LW up in W/m²
  d1 = self->hasVar('TSK')
  d2 = self->hasVar('EMISS')
  if (d1 and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'lwup'
    v.name = 'lwup'
    v.unit = 'w m-2'
    v.description = 'upward long wave flux at ground surface'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
    ;SW up in W/m²
  d1 = self->hasVar('SWDOWN')
  d2 = self->hasVar('ALBEDO')
  if (d1 and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'swup'
    v.name = 'swup'
    v.unit = 'w m-2'
    v.description = 'upward short wave flux at ground surface'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
 
      ;SW up in W/m²
  d1 = self->hasVar('LWDOWN')
  d2 = self->hasVar('GLW')
  if (d1 or d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'lwdown'
    v.name = 'lwdown'
    v.unit = 'w m-2'
    v.description = 'upward short wave flux at ground surface'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif 

       ;NETRAD up in W/m²
  d1 = self->hasVar('SWDOWN')
  d2 = self->hasVar('LWDOWN')
  d3 = self->hasVar('SWUP')
  d4 = self->hasVar('LWUP')
  if (d1 and d2 and d3 and d4) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'netrad'
    v.name = 'netrad'
    v.unit = 'w m-2'
    v.description = 'net radiation flux at ground surface'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
    ;TD in K
  d1 = self->hasVar('QVAPOR_eta')
  d2 = self->hasVar('P_eta')
  d3 = self->hasVar('PB_eta')
  if (d1 and d2 and d3) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'td_eta'
    v.name = 'td_eta'
    v.unit = 'k'
    v.description = 'dew point'
    v.type = '3d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif



  
  ;Precipitation rate in mm h-1
  d1 = self->hasVar('rainc')
  d2 = self->hasVar('rainnc')
  if (d1 and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp'
    v.name = 'prcp'
    v.unit = 'mm h-1'
    v.description = 'Precipitation rate'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('psfc')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'p2hpa'
    v.name = 'p2hpa'
    v.unit = 'hPa'
    v.description = 'Surface Pressure'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  d1 = self->hasVar('p2hpa')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'psfc'
    v.name = 'psfc'
    v.unit = 'Pa'
    v.description = 'Surface Pressure'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  ; Prcp rate of resolved/explicit precipitation
  d1 = self->hasVar('rainnc')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp_nc'
    v.name = 'prcp_nc'
    v.unit = 'mm h-1'
    v.description = 'Explicit Precipitation rate '
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  ;Prcp sum of convective precipitation
  d1 = self->hasVar('rainc')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'prcp_c'
    v.name = 'prcp_c'
    v.unit = 'mm h-1'
    v.description = 'Convective precipitation rate'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
    
  ;2m Dewpoint Temperature
  d1 = self->hasVar('psfc')
  d2 = self->hasVar('q2')
  d3 = self->hasVar('p2hpa')
  if ((d1 or d3) and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'td2'
    v.name = 'td2'
    v.unit = 'C'
    v.description = '2m Dewpoint Temperature'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  ;2m Temperature in degree celsius
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
  
  ;2m Temperature in kelvin
  d1 = self->hasVar('t2c')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 't2'
    v.name = 't2'
    v.unit = 'K'
    v.description = '2m Temperature'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
    
  ; 2m Relative humidity
  d1 = self->hasVar('t2')
  d2 = self->hasVar('q2')
  d3 = self->hasVar('psfc')
  d4 = self->hasVar('p2hpa')
  d5 = self->hasVar('t2c')
  if ((d1 or d5) and d2 and (d3 or d4)) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'rh2'
    v.name = 'rh2'
    v.unit = '%'
    v.description = '2 m Relative Humidity'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  ; Prcp sum (total) for temporal resolution (eg. total/d)
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
  
  ; Prcp sum of resolved/ explicit precipitation
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
  
  ;Prcp sum of convective precipitation
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
  
  ;Evapotranspiration sum for temporal res (eg. evap/d)
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
  
  ; 10m Wind speed
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
  endif
  
;pressure
  d1 = self->hasVar('P_eta')
  d2 = self->hasVar('PB_eta')
  if (d1 and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'pressure_eta'
    v.name = 'pressure'
    v.unit = 'hPa'
    v.description = 'Full model pressure'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  ; geopotential, z
  d1 = self->hasVar('PH_eta')
  d1 = self->hasVar('PHB_eta')
  if (d1 and d2) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'geopotential_eta'
    v.name = 'geopotential'
    v.unit = 'm2 s-2'
    v.description = 'Full model geopotential on mass points'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    
    v = self->_varStruct(/DERIVED)
    v.id = 'z_eta'
    v.name = 'z'
    v.unit = 'm'
    v.description = 'Full model height on mass points'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    
    d1 = self->hasVar('HGT')
    if (d1) then begin
      v = self->_varStruct(/DERIVED)
      v.id = 'zag_eta'
      v.name = 'zag'
      v.unit = 'm'
      v.description = 'Full model height  above ground on mass points'
      v.type = '3d_eta'
      if ~ self->hasVar(v.id) then vars = [vars,v]
    endif
  endif
    
  ;TK and TC
  d1 = self->hasVar('T_eta')
  d2 = self->hasVar('P_eta')
  d3 = self->hasVar('PB_eta')
  if (d1 and d2 and d3) then begin
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
    v.unit = 'degC'
    v.description = 'Temperature'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif

   ;TD
  d1 = self->hasVar('QVAPOR_eta')
  d2 = self->hasVar('P_eta')
  d3 = self->hasVar('PB_eta')
  if (d1 and d2 and d3) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'td_eta'
    v.name = 'td'
    v.unit = 'K'
    v.description = 'Dewpoint Temperature'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif

  
  ;THETA
  d1 = self->hasVar('T_eta')
  if (d1) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'theta_eta'
    v.name = 'theta'
    v.unit = 'K'
    v.description = 'Potential Temperature (theta)'
    v.type = '3d_eta'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
   ;Sea level pressure
  d1 = self->hasVar('PSFC')
  d2 = self->hasVar('T2')
  d3 = self->hasVar('HGT')
  d4 = self->hasVar('p2hpa')
  d5 = self->hasVar('t2c')
  if ((d1 or d4) and (d2 or d5) and d3) then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'slp'
    v.name = 'slp'
    v.unit = 'hPa'
    v.description = 'Sea Level Pressure Barometric'
    v.type = '2d'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endif
  
  ; OK. Now add all the pressure stuff for the 3d variables
  c = where(vars.type eq '3d_eta', nc)
  if nc ne 0 then begin
  for i=0, nc-1 do begin
    _v = vars[c[i]]
    v = self->_varStruct(/DERIVED)
    v.id = _v.name + '_press'
    v.name = _v.name + '_press'
    v.unit = _v.unit
    v.description = _v.description
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
  endfor
  endif
  
;    ; OK. Now add all the pressure stuff for the 3d pl variables
  for j=0, N_ELEMENTS(vars.name)-1 do begin
    str=STRMID(str_equiv((vars.name)[j]), N_ELEMENTS(byte((vars.name)[j]))-3, 3)
    if str eq '_PL' then begin

    _v = vars[j]
    v = self->_varStruct(/PL)
    v.id =   utils_replace_string(str_equiv(_v.name), '_PL', '_press')
    v.name = _v.name
    v.unit = _v.unit
    v.description = _v.description
    v.type = '3d_press'
    if ~ self->hasVar(v.id) then vars = [vars,v]
    endif
  endfor

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
pro wa_WPR::GetProperty,  $
  TRES=tres,  $
  HRES=hres,  $
  DOMAIN=domain, $
  EXPE=expe,  $
  DIRECTORY=directory,  $
  PRESSURELEVELS=pressurelevels, $
  _Ref_Extra=extra
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  IF Arg_Present(DIRECTORY) THEN directory = self.directory  
  IF Arg_Present(TRES) THEN tres = self.tres  
  IF Arg_Present(HRES) THEN hres = self.hres  
  IF Arg_Present(EXPE) THEN expe = self.expe  
  IF Arg_Present(DOMAIN) THEN domain = self.domain
  IF Arg_Present(PRESSURELEVELS) then pressurelevels = *self.pressurelevels
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
pro wa_WPR::getTime, time, nt, t0, t1

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
pro wa_WPR::setYears, years

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
;    The standard pressure levels
;
; :Keywords:
;    COUNT: out, optional
;           the number of levels
;           
; :Returns:
;   An array of pressure levels
;
;-
function wa_WPR::getPressureLevels, COUNT=count

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if PTR_VALID(self.pressurelevels) then out = *self.pressurelevels
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
;
; :Returns:
;   An array of variable ids
;
;-
function wa_WPR::getVarNames, COUNT=count, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  vars = *self.vars
  count = N_ELEMENTS(vars)
  out = vars.id
  
  if KEYWORD_SET(PRINT) then begin
    print, '   ID                NAME            DESCRIPTION                                       UNIT       TYPE'
    
    for i = 0L, count-1 do begin
      v = vars[i]
      ns = '                                                                                                                                  '
      STRPUT, ns, str_equiv(i), 0
      STRPUT, ns, v.id, 3
      STRPUT, ns, v.name, 4+18
      STRPUT, ns, v.description, 20+18
      STRPUT, ns, v.unit, 70+18
      t = v.type
      if v.derived then t += ' (derived)'
      STRPUT, ns, t, 81+18
      print, ns
    endfor
  endif
  
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
function wa_WPR::hasVar, id, INFO=info

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, info
  
  n = self->GetVarNames()
  p = where(str_equiv(n) eq str_equiv(id), cnt)
  
  if cnt eq 0 then return, 0
  
  v = (*self.vars)[p]
  info = {id:v.id, name:v.name, description:v.description, unit:v.unit, type:v.type, derived:v.derived, pl:v.pl}
  
  return, 1
  
end

;+
; :Description:
;    Get access to a single product file object (w_geo_nc)
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
;     the object
;
;-  TO GET A WHOLE VARIABLE OBJECT (SINGLE VARIABLE) - POSSIBLE FOR ALL PRODUCTS
function wa_WPR::getVarObj, id, INFO=info

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_Error, 2
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  if info.derived then Message, 'Variable is derived. You should not ask me for this: ' + str_equiv(id)
  
  v = *self.vars
  pv = where(str_equiv(v.id) eq str_equiv(id))
  v = v[pv[0]]
  
  ; It is a product variable which is not open
  if ~ self.objs->hasKey(v.id) then begin
    if v.pos eq -1 then Message, 'big problem, pos was not initialised'
    obj = OBJ_NEW('w_GEO_nc', FILE=(*self.files)[v.pos]) ; add the object to the hash
    if OBJ_VALID(obj) then (self.objs)[v.id] = obj 
  endif
  
  obj = (self.objs)[v.id]
  
  return, obj
  
end

;+
; :Description:
;    Get the data for a specific variable, at the dimensions of the subset.
;
;    Additionaly to the product variables, some derived variables are computed
;    (depending on the available product variables):
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
;   ZLEVELS: in, optional, type = float
;            set this keyword to an array of one or two elements, containing the range
;            of the indexes to keep from the original NCDF file in the Z dimension (eta-levels).
;   PRESSURE_LEVELS: in, optional, type = float
;                    set this keyword to an array of pressure levels (hPa) to interpolate to.
;                    the output array will then have the dimensions [nx,ny,nl,nt], where nl is the 
;                    number of elements in pressure_levels
;   HEIGHT_LEVELS: in, optional, type = float
;                  set this keyword to an array of height levels (m) to interpolate to.
;                  the output array will then have the dimensions [nx,ny,nl,nt], where nl is the 
;                  number of elements in height_levels
;   ABOVE_GROUND_LEVELS: in, optional, type = float
;                        set this keyword to an array of height levels (m) ABOVE model height to interpolate to.
;                        the output array will then have the dimensions [nx,ny,nl,nt], where nl is the 
;                        number of elements in height_levels
; :Returns:
;   the data array
;
;-
function wa_WPR::getVarData, id, $
  time, $
  nt, $
  INFO=info, $
  T0=t0, $
  T1=t1, $
  MONTH=month, $
  HOUROFDAY=hourofday, $
  ZLEVELS=zlevels, $
  PRESSURE_LEVELS=pressure_levels, $
  HEIGHT_LEVELS=height_levels, $
  ABOVE_GROUND_LEVELS=above_ground_levels, $
  ACC_TO_STEP=acc_to_step
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
;  on_Error, 2
  
  undefine, info, time, nt
 
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  ;Some check
  if N_ELEMENTS(HOUROFDAY) ne 0 then Message, 'Hourofday maybe buggy. easy to check though'
  _acc_to_step = KEYWORD_SET(ACC_TO_STEP)
  _do_eta = N_ELEMENTS(ZLEVELS) ne 0
  _do_pres = N_ELEMENTS(PRESSURE_LEVELS) ne 0
  _do_h = N_ELEMENTS(HEIGHT_LEVELS) ne 0
  _do_ag = N_ELEMENTS(ABOVE_GROUND_LEVELS) ne 0
  if total([_do_eta,_do_pres,_do_h,_do_ag]) gt 1 then Message, 'Some keywords are incompatible (Z-dimension).'
  
  ; If static this is really easy
  if info.type eq 'static' then begin
    obj = self->getVarObj(id)
    if TOTAL(self.subset) ne 0 then ok = obj->define_subset(SUBSET=self.subset) else ok = obj->define_subset()
    return, obj->get_Var(info.name, time, nt)
  endif
  
  if info.pl then begin
    id = utils_replace_string(str_equiv(id), '_PRESS', '_PL')
    
  endif
    
  if info.derived then begin
    ; This is for the summed variables per interval
    if STRMID(str_equiv(id), N_ELEMENTS(byte(id))-4, 4) eq '_SUM' then begin
      _id = utils_replace_string(str_equiv(id), '_SUM', '')
      value = self->GetVarData(_id, time, nt, T0=t0, T1=t1, MONTH=month)
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
    
    if STRMID(str_equiv(id), N_ELEMENTS(byte(id))-6, 6) eq '_PRESS' then begin
      _id = utils_replace_string(str_equiv(id), '_PRESS', '_ETA')
      pl = self.getPressureLevels()
      return, self->GetVarData(_id, time, nt, T0=t0, T1=t1, MONTH=month, PRESSURE_LEVELS=pl)
    endif
    
    ; Its a derived variable that we have to compute
    case str_equiv(id) of
       'LWUP': begin
      value = self->GetVarData('TSK', time, nt, t0 = t0, t1 = t1, MONTH=month) 
      e = self->GetVarData('EMISS', time, nt, t0 = t0, t1 = t1, MONTH=month)
      value = (5.6704e-8) * e * value^4
    end
     'SWUP': begin
      value = self->GetVarData('SWDOWN', time, nt, t0 = t0, t1 = t1, MONTH=month)
      e = self->GetVarData('ALBEDO', time, nt, t0 = t0, t1 = t1, MONTH=month)
      return,value*e
    end
    'LWDOWN': begin
      value = self->GetVarData('GLW', time, nt, t0 = t0, t1 = t1, MONTH=month)
      return,value
    end


      'TD2': begin
        p = self->GetVarData('psfc', time, nt, T0=t0, T1=t1, MONTH=month)
        help, p
        qvapor = self->GetVarData('q2', T0=t0, T1=t1, MONTH=month)
        help, qvapor
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'T2' : begin
        return, self->GetVarData('t2c', time, nt, T0=t0, T1=t1, MONTH=month) + 273.15
      end
      'T2C': begin
        return, self->GetVarData('t2', time, nt, T0=t0, T1=t1, MONTH=month) - 273.15
      end
      'RH2': begin
        TK = self->GetVarData('t2', time, nt, T0=t0, T1=t1, MONTH=month)
        P = self->GetVarData('psfc', T0=t0, T1=t1, MONTH=month)
        QVAPOR = self->GetVarData('q2', T0=t0, T1=t1, MONTH=month)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'WS10': begin
        u10 = self->GetVarData('U10', time, nt, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('V10', T0=t0, T1=t1, MONTH=month)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WS=value
        return, value
      end
      'WD10': begin
        u10 = self->GetVarData('U10', time, nt, T0=t0, T1=t1, MONTH=month)
        v10 = self->GetVarData('V10', T0=t0, T1=t1, MONTH=month)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WD=value
        return, value
      end
      'PRCP': begin
        value = self->GetVarData('RAINNC', time, nt, t0=t0, t1=t1) + self->GetVarData('RAINC', t0=t0, t1=t1)
        ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
        if ts ne 1 then value = value / ts
        _acc_to_step = TRUE
      end
      'PRCP_NC': begin
        value = self->GetVarData('RAINNC', time, nt, t0 = t0, t1 = t1)
        ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
        if ts ne 1 then value = value / ts
        _acc_to_step = TRUE
      end
      'PRCP_C': begin
        value = self->GetVarData('RAINC', time, nt, t0 = t0, t1 = t1)
        ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
        if ts ne 1 then value = value / ts
        _acc_to_step = TRUE
      end
      'P2HPA': begin
        return, self->GetVarData('psfc', time, nt, T0=t0, T1=t1, MONTH=month)*0.01
      end
      'PSFC': begin
        return, self->GetVarData('p2hpa', time, nt, T0=t0, T1=t1, MONTH=month)/0.01
      end
      'SLP': begin
      ps = self->getVarData('PSFC', time, nt, T0=t0, T1=t1) * 0.01 ; in hPa
      T2 = self->getVarData('T2', T0=t0, T1=t1) - 273.15 ; in degC
      zs = self->getVarData('HGT', T0=t0, T1=t1) ; in m
      mdims = SIZE(t2, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for k=0,Nt-1 do value[*,*,k] = MET_barometric(ps[*,*,k], zs, T2[*,*,k], 0.)
    end
      'TK_ETA': begin
        T = self->GetVarData('T_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels) + 300.
        P = self->GetVarData('PB_ETA', T0=t0, T1=t1, ZLEVELS=zlevels) + self->GetVarData('P_ETA', T0=t0, T1=t1, ZLEVELS=zlevels)
        value = utils_wrf_tk(temporary(P),temporary(T))    ; calculate TK
      end
      'TC_ETA': begin
        value = self->GetVarData('TK_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels) - 273.15
      end
      'TD_ETA': begin
        QVAPOR = self->getVarData('QVAPOR_ETA', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels)
        P = self->GetVarData('PB_ETA', T0=t0, T1=t1, ZLEVELS=zlevels) + self->GetVarData('P_ETA', T0=t0, T1=t1, ZLEVELS=zlevels)
        value = utils_wrf_td(temporary(P),temporary(QVAPOR))    ; calculate TD
    end
      'THETA_ETA': begin
        value = self->GetVarData('T_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels) + 300.
      end
      'PRESSURE_ETA': begin
        value = (self->GetVarData('PB_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels) + self->GetVarData('P_ETA', T0=t0, T1=t1, ZLEVELS=zlevels)) * 0.01
      end
      'GEOPOTENTIAL_ETA': begin
        value = self->GetVarData('PH_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels) + self->GetVarData('PHB_ETA', T0=t0, T1=t1, ZLEVELS=zlevels)
      end
      
      'Z_ETA': begin
        value = self->GetVarData('GEOPOTENTIAL_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels) / 9.81
      end
      'ZAG_ETA': begin
        value = self->GetVarData('Z_ETA', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels)
        _dims = SIZE(value, /DIMENSIONS) & _dims[2:*] = 1
        ter =  rebin(reform(self->getVarData('ter'),_dims), dims) ; make it same dim as z
        value = TEMPORARY(value) - TEMPORARY(ter)
      end
      else: Message, 'No'
    endcase
    
  endif else begin
  
    ; ORIGINAL VARIABLE FROM FILE
   
    obj = self->getVarObj(id)
    
    ; Some time subsetting
    do_ts = N_ELEMENTS(T0) eq 1 or N_ELEMENTS(T1) eq 1
    do_m = N_ELEMENTS(MONTH) ne 0
    do_h = N_ELEMENTS(HOUROFDAY) ne 0
    if (do_m and do_ts) then message, 'Incompatible keywords (T0 or T1 and MONTH)'
    if do_h and self.tres ne 'h' then message, 'Incompatible keywords (HOUROFDAY) with product time resolution: ' + self.tres
    
    if do_m then begin
      if N_ELEMENTS(month) ne 1 then Message, WAVE_Std_Message('MONTH', NELEMENTS=1)
      dummy = where(month gt 12 or month lt 1, nok)
      if nok ne 0 then Message, WAVE_Std_Message('MONTH', /RANGE)
      if self.tres eq 'y' then Message, '$MONTH not compatible with yearly products'
    endif
    
    
    obj->get_dimList, dimIds, dimNames, dimSizes
    ndims = N_ELEMENTS(dimIds)
    
    ; Check if we need to unstagger
    found_stag = -1
    add_subs = [0,0,0,0]
    for i=0, ndims-1 do begin
      isHere = STRPOS(str_equiv(dimNames[i]), str_equiv('_stag'))
      p = WHERE(isHere ne -1, cnt)
      if cnt ne 0 then begin
        found_stag = i
        dim_stag = utils_replace_string(str_equiv(dimNames[i]), str_equiv('_stag'), '')
      endif
    endfor
    ; If we unstagger than the subset has to be larger of 1 in x or y 
    if found_stag ne -1 then begin
      if TOTAL(self.subset) ne 0 then Message, 'UNSTAGG not possible with subsets. Maybe WAVE 2.0. Sorry.'
      if dim_stag eq 'WEST_EAST' then found_stag = 0
      if dim_stag eq 'SOUTH_NORTH' then found_stag = 1
      if dim_stag eq 'BOTTOM_TOP' then found_stag = 2
      if dim_stag eq 'NUM_PRESS_LEVELS' then found_stag = -1
    endif
    
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
    value = obj->get_Var(info.name, time, nt, ZLEVELS=zlevels, T0=_t0, T1=_t1, HOUROFDAY=hourofday)
    ; unstagger if needed
    if found_stag ne -1 then value = utils_wrf_unstagger(TEMPORARY(value), found_stag)
  endelse
  
  ; deaccumulate?
  if _acc_to_step then begin
    if nt eq 1 then Message, 'You asked to de-accumulate only one time-step. Not possible.'
    value = utils_ACC_TO_STEP(value)
  endif
  
  ; interpol on z?
  if _do_pres then begin
    p = self->getVarData('pressure_eta', T0=t0, T1=t1, HOUROFDAY=hourofday)
    value = reform(utils_wrf_intrp3d(value, p, pressure_levels))
  endif
  if _do_h then begin
    h = self->getVarData('z_eta', T0=t0, T1=t1, HOUROFDAY=hourofday)
    value = reform(utils_wrf_intrp3d(value, h, height_levels))
  endif
  if _do_ag then begin
    h = self->getVarData('zag_eta', T0=t0, T1=t1, HOUROFDAY=hourofday)
    value = reform(utils_wrf_intrp3d(value, h, above_ground_levels, /EXTRAPOLATE))
  endif
  
  return, value
  
end

;+
; :Description:
;    Comuptes the daily means in a new directory
;    (must be starting from a hourly directory!)
;
; :Params:
;    id: the variable to compute
;
; :Keywords:
;    CLOBBER: set this keyword to force overwriting of existing files
;    COMPRESS: set this keyword to use compression option and netcdf4
;
;-
pro wa_WPR::makeDailyMeans, id, CLOBBER=clobber, COMPRESS=compress, ST0=st0
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
;  on_Error, 2
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  if self.tres ne 'h' then Message, 'I need to be an H to make means'
  vname = (STRLOWCASE(info.name))
  if info.type eq '3d_soil' then message, 'soil not yet'
  if info.type eq 'static' then message, 'static dont need averages'
  _compress = KEYWORD_SET(COMPRESS)
  self->getTime, otime, ont, ot0, ot1
  oabs=MAKE_ABS_DATE(QMS=otime)
  if ~KEYWORD_SET(ST0) then ot0 = MAKE_ABS_DATE(QMS=ot0) else ot0= MAKE_ABS_DATE(QMS=st0)
  ot1 = MAKE_ABS_DATE(QMS=ot1)
  if ot0.hour ne 0 then message, 'Please do not make it too complicated buhuhu'
  if ot1.hour ne 0 then begin; message, 'Please do not make it too complicated buhuhu'
    hval=where(oabs.hour eq 0)
    ot1=oabs[hval[N_ELEMENTS(hval)-1]]
    print, 'Please do not make it too complicated buhuhu - ot1 is', time_to_str(ot1)
  endif
  
  print, 'T0: ', ot0
  print, 'T1: ', ot1
 
  if (ot1.qms-ot0.qms) lt D_QMS then message, 'Please do not make it too complicated buhuhu'  
  ndays = (ot1.qms-ot0.qms) / D_QMS
  
  ; Directory stuff
  prdir = FILE_DIRNAME(self.directory)
  out_dir = prdir + '/d/' + info.type + '/'
  if ~ FILE_TEST(out_dir) then FILE_MKDIR, out_dir  
  out_file = out_dir + vname + '_' + self.domain + '_d_' + self.expe +'_'+self.years+'.nc'
  
  ; Right now I need a dummy obj. Not very elegant but well
  vars = *self.vars
  if info.type eq '2d' then c = where(vars.type eq '2d', nc)
  if info.type eq '3d_eta' then c = where(vars.type eq '3d_eta', nc)
  if info.type eq '3d_press' then c = where(vars.type eq '3d_press', nc)
  if nc eq 0 then message, 'Noooo'
  sObj = NCDF_FILE((*self.files)[(vars[c[0]]).pos])
  print, 'Clobber prob?' 
  dObj = NCDF_FILE(out_file, CLOBBER=clobber, /CREATE, NETCDF4_FORMAT=_compress)
  print, 'No clobber prob'
  ; Find all the global attributes in the source file and copy them.
  attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
  for j=0,attrCount-1 do begin
    if attrNames[j] eq 'history' then continue
    sObj->CopyGlobalAttrTo, attrNames[j], dObj
  endfor
  
  ; Dimensions
  sObj->CopyDimTo, 'south_north', dObj
  sObj->CopyDimTo, 'west_east', dObj
  if info.type eq '3d_eta' then begin
    nn = sObj->GetDimNames()
    pok = where(str_equiv(nn) eq 'BOTTOM_TOP', cntok)
    if cntok ne 0 then begin
      dObj->WriteDim, 'bottom_top', sObj->GetDimValue(nn[pok])
    endif else begin
      pok = where(str_equiv(nn) eq 'BOTTOM_TOP_STAG', cntok)
      if cntok eq 0 then Message, 'big problem' 
      dObj->WriteDim, 'bottom_top', sObj->GetDimValue(nn[pok])-1
    endelse
  endif
  if info.type eq '3d_press' then begin
    pl = self->getPressureLevels(COUNT=npl)
    dObj->WriteDim, 'bottom_top', npl
  endif
  dObj->WriteDim, 'time', /UNLIMITED
  
  ; Time var
  dObj->WriteVarDef, 'Times', ['time'], DATATYPE='LONG'
  dObj->WriteVarAttr, 'Times', 'units', 'days since 1990-01-01 00:00:00'
  dObj->WriteVarAttr, 'Times', 'description', 'time'
  std_time = QMS_TIME(YEAR=1990, MONTH=1, DAY=1)
   
  ; Real var  
  if info.type eq '2d' then dimnames = ['west_east', 'south_north', 'time']
  if info.type eq '3d_press' or info.type eq '3d_eta' then BEGIN
    dimnames = ['west_east', 'south_north', 'bottom_top','time']
  endif
  if _compress then  dObj->WriteVarDef, vname, dimnames, DATATYPE='FLOAT', GZIP=5, SHUFFLE=1 $
   else dObj->WriteVarDef, vname, dimnames, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vname, 'units',  info.unit
  dObj->WriteVarAttr, vname, 'description', info.description
  
  ; Ok. The final times
  final_time_in_days = LONG((ot0.qms - std_time)/D_QMS) + LINDGEN(ndays)  
  interval_time = ot0.qms + L64INDGEN(ndays+1) * D_QMS
     
  ; Ok. now go through the days
  for i=0, ndays-1 do begin

    t0 = interval_time[i]
    t1 = interval_time[i+1]

    ; Because of PRCP we take n+1 timesteps but keep the last ns
    var = self->getVarData(id, T0=t0, T1=t1) 
    nok =where(var eq -999.)
    var[nok]=!VALUES.F_NAN
    
    if STRMID(str_equiv(id), 0, 4) eq 'PRCP' then begin
      pnok=where(var lt (machar()).eps, cnt)
      if cnt ne 0 then var[pnok] = 0
    endif
    if info.type eq '3d_press' or info.type eq '3d_eta' then begin
      var = MEAN(var[*,*,*,1:*], DIMENSION=4, /NAN)
      offset = [0,0,0,i]
    endif else begin
      var = MEAN(var[*,*,1:*], DIMENSION=3, /NAN)
      offset = [0,0,i]
    endelse
    ;write
    dObj->WriteVarData, 'Times', final_time_in_days[i], OFFSET=i 
    dObj->WriteVarData, vname, TEMPORARY(var), OFFSET=offset
  endfor
  
  undefine, sObj, dObj
 
end

;+
; :Description:
;    Comuptes the monhly means in a new directory
;    (must be starting from a daily directory!)
;
; :Params:
;    id: the variable to compute
;
; :Keywords:
;    CLOBBER: set this keyword to force overwriting of existing files
;    COMPRESS: set this keyword to use compression option and netcdf4
;
;-
pro wa_WPR::makeMonthlyMeans, id, CLOBBER=clobber, COMPRESS=compress

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  ;  on_Error, 2
  print, 'Starting monthly means' 
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  if self.tres ne 'd' then Message, 'I need to be an d to make means'
  if info.type eq '3d_soil' then message, 'soil not yet'
  if info.type eq 'static' then message, 'static dont need averages'
  vname = STRLOWCASE(info.name)
  _compress = KEYWORD_SET(COMPRESS)
  print, 'Get time'
  self->getTime, otime, ont, ot0, ot1
  oabs=MAKE_ABS_DATE(QMS=otime)
  ot0 = MAKE_ABS_DATE(QMS=ot0)
  ot1 = MAKE_ABS_DATE(QMS=ot1)
  dplus1 = MAKE_ABS_DATE(QMS=(ot1.qms + D_QMS))
  if ot0.day ne 1 then message, 'Please do not make it too complicated buhuhu'
  if dplus1.day ne 1 then begin; message, 'Please do not make it too complicated buhuhu'
    hval=where(oabs.day eq 1)
    dplus1=oabs[hval[N_ELEMENTS(hval)-1]]
    print, 'Please do not make it too complicated buhuhu - ot1 is', time_to_str(ot1)
  endif
  
  m0 = w_time_to_month(ot0)
  m1 = w_time_to_month(dplus1)
  nmonths = m1-m0
  
  ; Directory stuff
  prdir = FILE_DIRNAME(self.directory)
  print, 'indir', prdir
  out_dir = prdir + '/m/' + info.type + '/'
  if ~ FILE_TEST(out_dir) then FILE_MKDIR, out_dir
  out_file = out_dir + vname + '_' + self.domain + '_m_' + self.expe +'_'+self.years+'.nc'
  
  if FILE_TEST(out_file) and ~ KEYWORD_SET(CLOBBER) then Message, 'File exists and CLOBBER not set. Stop here'
  
  ; Right now I need a dummy obj. Not very elegant but well
  vars = *self.vars
  c = where(vars.type eq info.type, nc)
  if nc eq 0 then message, 'Noooo'
  sObj = NCDF_FILE((*self.files)[(vars[c[0]]).pos])
  dObj = NCDF_FILE(out_file, CLOBBER=clobber, /CREATE, NETCDF4_FORMAT=_compress)
  
  ; Find all the global attributes in the source file and copy them.
  attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
  for j=0,attrCount-1 do begin
    if attrNames[j] eq 'history' then continue
    sObj->CopyGlobalAttrTo, attrNames[j], dObj
  endfor
  
  ; Dimensions
  sObj->CopyDimTo, 'south_north', dObj
  sObj->CopyDimTo, 'west_east', dObj
  if info.type eq '3d_eta' or info.type eq '3d_press' then sObj->CopyDimTo, 'bottom_top', dObj
  dObj->WriteDim, 'time', /UNLIMITED
  
  ; Time var
  dObj->WriteVarDef, 'Times', ['time'], DATATYPE='LONG'
  dObj->WriteVarAttr, 'Times', 'units', 'months since 1990-01-01 00:00:00'
  dObj->WriteVarAttr, 'Times', 'description', 'time'
  std_time = QMS_TIME(YEAR=1990, MONTH=1, DAY=1)
  
  ; Real var
  if info.type eq '2d' then dimnames = ['west_east', 'south_north', 'time']
  if info.type eq '3d_press' or info.type eq '3d_eta' then BEGIN
    dimnames = ['west_east', 'south_north', 'bottom_top','time']
  endif
  if _compress then  dObj->WriteVarDef, vname, dimnames, DATATYPE='FLOAT', GZIP=5, SHUFFLE=1 $
   else dObj->WriteVarDef, vname, dimnames, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vname, 'units',  info.unit
  dObj->WriteVarAttr, vname, 'description', info.description
  
  ; Ok. The final times
  final_time_in_months = (m0-w_time_to_month(std_time)) + LINDGEN(nmonths)
  interval_time = w_month_to_time(m0 + LINDGEN(nmonths+1))
  
  ; Ok. now go through the months
  for i=0, nmonths-1 do begin
    t0 = interval_time[i]
    t1 = interval_time[i+1] - D_QMS

    var = self->getVarData(id, T0=t0, T1=t1)
    nok =where(var eq -999.)
    var[nok]=!VALUES.F_NAN
    if info.type eq '3d_press' or info.type eq '3d_eta' then begin
      var = MEAN(TEMPORARY(var), DIMENSION=4, /NAN)
      offset = [0,0,0,i]
    endif else begin
      var = MEAN(TEMPORARY(var), DIMENSION=3, /NAN)
      offset = [0,0,i]
    endelse
    ;write
    dObj->WriteVarData, 'Times', final_time_in_months[i], OFFSET=i
    dObj->WriteVarData, vname, TEMPORARY(var), OFFSET=offset
  endfor
  
  undefine, sObj, dObj
  
end

;+
; :Description:
;    Class structure definition
;
;-
pro wa_WPR__Define, class

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  class = { wa_WPR                            ,  $
    INHERITS w_GISdata                  ,  $
    directory:          ''              ,  $ ; path to the file directory
    time:               PTR_NEW()       ,  $ ; the time PTR
    oyears:             PTR_NEW()       ,  $ ; The available product years
    years:              ''       ,  $ ; The product years set by user
    vars:               PTR_NEW()       ,  $ ; array of var structures (see w_WPR::_varStruct)
    pressurelevels:     PTR_NEW()       ,  $ ; array of pressure levels (saved by init, not modifiable)
    tres:               ''              ,  $ ; type of active directory: h, d, m, y
    hres:               ''              ,  $ ; type of active directory: d30km, d10km, d02km
    expe:               ''              ,  $ ; type of active directory EXPERIMENT (e.g. BMJ_LIN_ACM2)
    domain:             ''              ,  $ ; domain number (in file name)
    files:              PTR_NEW()       ,  $ ; Path to the files
    objs:               hash()             $ ; the ncdf objects
  }
  
end
