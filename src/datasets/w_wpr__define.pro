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
;    _EXTRA: in, optional
;            any keyword accepted by `w_GISdata::defineSubset`
;            
;               
; :Returns: 
;    1 if the object is created successfully, 0 if not
;
;-
function w_WPR::init, DIRECTORY=directory, _EXTRA=extra
  
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
  if ~ FILE_TEST(directory, /DIRECTORY) then MESSAGE, WAVE_Std_Message(/FILE)
  
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
  self.years = PTR_NEW(years)
  
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
  
  ; Time
  self.time = PTR_NEW(self->_makeTime(years))
  
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
  PTR_FREE, self.pressure_levels
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
    vars = [vars,v]
  endif
  
  d1 = self->hasVar('geopotential_eta')
  if d1 then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'z_eta'
    v.name = 'z'
    v.unit = 'm'
    v.description = 'Full model height'
    v.type = '3d_eta'
    vars = [vars,v]
  endif
  d1 = self->hasVar('geopotential_press')
  if d1 then begin
    v = self->_varStruct(/DERIVED)
    v.id = 'z_press'
    v.name = 'z'
    v.unit = 'm'
    v.description = 'Full model height'
    v.type = '3d_press'
    vars = [vars,v]
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
    vars = [vars,v]
    v = self->_varStruct(/DERIVED)
    v.id = 'tc_eta'
    v.name = 'tc'
    v.unit = 'C'
    v.description = 'Temperature'
    v.type = '3d_eta'
    vars = [vars,v]
    if self->hasVar('hgt') then begin
      v = self->_varStruct(/DERIVED)
      v.id = 't2pbl'
      v.name = 't2pbl'
      v.unit = 'K'
      v.description = '2 m temperature (extrapolated from eta-levels)'
      v.type = '2d'
      vars = [vars,v]
      v = self->_varStruct(/DERIVED)
      v.id = 't2pblc'
      v.name = 't2pblc'
      v.unit = 'C'
      v.description = '2 m temperature (extrapolated from eta-levels)'
      v.type = '2d'
      vars = [vars,v]
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
    vars = [vars,v]
    v = self->_varStruct(/DERIVED)
    v.id = 'tc_press'
    v.name = 'tc'
    v.unit = 'C'
    v.description = 'Temperature'
    v.type = '3d_press'
    vars = [vars,v]
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
    vars = [vars,v]
  endif      
  
  d1 = self->hasVar('qvapor_press')
  if (d1) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 'td_press'
    v.name = 'td'
    v.unit = 'C'
    v.description = 'Dewpoint Temperature'
    v.type = '3d_press'
    vars = [vars,v]
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
    vars = [vars,v]
  endif      

  d1 = self->hasVar('t2')
  if (d1) then begin 
    v = self->_varStruct(/DERIVED)
    v.id = 't2c'
    v.name = 't2c'
    v.unit = 'C'
    v.description = '2m Temperature'
    v.type = '2d'
    vars = [vars,v]
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
    vars = [vars,v]
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
    vars = [vars,v]
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
    vars = [vars,v]
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
    vars = [vars,v]
    v = self->_varStruct(/DERIVED)
    v.id = 'wd10'
    v.name = 'wd10'
    v.unit = 'degrees'
    v.description = '10 m wind direction'
    v.type = '2d'
    vars = [vars,v]    
  endif
  
  PTR_FREE, self.vars 
  self.vars = PTR_NEW(vars)
  
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
function w_WPR::getVarNames, COUNT=count, PRINT=print

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
function w_WPR::hasVar, id, INFO=info
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  undefine, info
  
  n = self->GetVarNames()  
  p = where(str_equiv(n) eq str_equiv(id), cnt) 
  
  if cnt eq 0 then return, 0
  
  v = (*self.vars)[p]
  info = {id:v.id, name:v.name, description:v.description, unit:v.unit, type:v.type}
  
  return, 1

end

;+
; :Description:
;    Get the data for a specific variable, at the dimensions of the subset.
;    
;    Additionaly to the product variables, some derived variables are computed
;    (depending on the available product variables)::
;    
;         ID                NAME            DESCRIPTION                                       UNIT       TYPE
;       - pressure_press     pressure        Full model pressure (on pressure levels ;-)       hPa        3d_press (derived)             
;       - z_eta              z               Full model height                                 m          3d_eta (derived)               
;       - z_press            z               Full model height                                 m          3d_press (derived)             
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
;       - rh_press           rh              Relative Humidity                                 %          3d_press (derived)             
;       - rh2                rh2             2 m Relative Humidity                             %          2d (derived)                   
;       - ws10               ws10            10 m wind speed                                   m s-1      2d (derived)                   
;       - wd10               wd10            10 m wind direction                               degrees    2d (derived)      
;
; :Params:
;    id: in, required
;        the variable ID
;
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data. Contains the tags:: 
;            - name
;            - id
;            - description
;            - unit
;    YEARS: in, optional
;           if you want to get the data for specific years only
;    ZLEVELS: in, optional
;             if you want to get the data for specific Z levels only 
;             (e.g: ZLEVELS=0 for first eta/pressure level, ZLEVELS=[0:12] for the first twelve)
;            
; :Returns:
;   the data array
;   
;-
function w_WPR::getVarData, id, time, nt, INFO=info, YEARS=years, ZLEVELS=zlevels

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, info, time, nt
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  v = *self.vars
  pv = where(str_equiv(v.id) eq str_equiv(id))
  v = v[pv[0]]
  
  if v.derived then begin
    ; Its a derived variable that we have to compute
    case str_equiv(id) of
      'Z_ETA': begin
        return, self->GetVarData('geopotential_eta', time, nt, YEARS=years, ZLEVELS=zlevels) / 9.81
      end
      'Z_PRESS': begin
        return, self->GetVarData('geopotential_press', time, nt, YEARS=years, ZLEVELS=zlevels) / 9.81
      end
      'TK_ETA': begin
        p = self->GetVarData('pressure_eta', time, nt, YEARS=years, ZLEVELS=zlevels) * 100.
        t = self->GetVarData('theta_eta', YEARS=years, ZLEVELS=zlevels)
        return, utils_wrf_tk(TEMPORARY(p),TEMPORARY(t))
      end
      'TC_ETA': begin
        return, self->GetVarData('tk_eta', time, nt, YEARS=years, ZLEVELS=zlevels) - 273.15
      end
      'PRESSURE_PRESS': begin
        levs = self->GetPressureLevels()
        if N_ELEMENTS(YEARS) ne 0 then _y = years else _y = *self.years
        time = self->_makeTime(years)
        nt = N_ELEMENTS(time)
        if N_ELEMENTS(ZLEVELS) eq 1 then levs = levs[zlevels]
        if N_ELEMENTS(ZLEVELS) eq 2 then levs = levs[zlevels[0]:zlevels[1]]
        nl = N_ELEMENTS(levs)
        out = FLTARR(self.tnt_c.nx, self.tnt_c.ny, nt, nl)
        for i=0, nl-1 do out[*,*,*,i] = levs[i]
        return, out
      end
      'TK_PRESS': begin
        p = self->GetVarData('pressure_press', time, nt, YEARS=years, ZLEVELS=zlevels) * 100.
        t = self->GetVarData('theta_eta', YEARS=years, ZLEVELS=zlevels)
        return, utils_wrf_tk(TEMPORARY(p),TEMPORARY(t))
      end
      'TC_PRESS': begin
        return, self->GetVarData('tk_eta', time, nt, YEARS=years, ZLEVELS=zlevels) - 273.15
      end
      'T2PBL': begin
        tk = self->get_Var('tk_eta', time, nt, YEARS=years, ZLEVELS=zlevels)
        dims = SIZE(tk, /DIMENSIONS)
        _dims = dims & _dims[2:*] = 1
        ter =  rebin(reform(self->GetVarData('ter'),_dims), dims) ; make it same dim as z
        h = self->GetVarData('z', YEARS=years, ZLEVELS=zlevels) - TEMPORARY(ter)
        return, reform(utils_wrf_intrp3d(TEMPORARY(tk), TEMPORARY(h), 2., /EXTRAPOLATE))
      end
      'T2PBLC': begin
        return, self->GetVarData('t2pbl', YEARS=years, ZLEVELS=zlevels) - 273.15
      end
      'TD_ETA': begin
        p = self->GetVarData('pressure_eta', time, nt, YEARS=years, ZLEVELS=zlevels) *100.
        qvapor = self->GetVarData('qvapor_eta', YEARS=years, ZLEVELS=zlevels)
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'TD_PRESS': begin
        p = self->GetVarData('pressure_press', time, nt, YEARS=years, ZLEVELS=zlevels) *100.
        qvapor = self->GetVarData('qvapor_press', YEARS=years, ZLEVELS=zlevels)
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'TD2': begin
        p = self->GetVarData('psfc', time, nt, YEARS=years)
        qvapor = self->GetVarData('q2', YEARS=years)
        return, utils_wrf_td(temporary(p),temporary(qvapor))
      end
      'T2C': begin
        return, self->GetVarData('t2', time, nt, YEARS=years) - 273.15
      end
      'RH_ETA': begin
        TK = self->GetVarData('tk_eta', time, nt, YEARS=years, ZLEVELS=zlevels)
        P = self->GetVarData('pressure_eta', YEARS=years, ZLEVELS=zlevels) *100.
        QVAPOR = self->GetVarData('qvapor_eta', YEARS=years, ZLEVELS=zlevels)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'RH_PRESS': begin
        TK = self->GetVarData('tk_press', time, nt, YEARS=years, ZLEVELS=zlevels)
        P = self->GetVarData('pressure_press', YEARS=years, ZLEVELS=zlevels) *100.
        QVAPOR = self->GetVarData('qvapor_press', YEARS=years, ZLEVELS=zlevels)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'RH2': begin
        TK = self->GetVarData('t2', time, nt, YEARS=years)
        P = self->GetVarData('psfc', YEARS=years)
        QVAPOR = self->GetVarData('q2', YEARS=years)
        return, utils_wrf_rh(TEMPORARY(QVAPOR), TEMPORARY(P), TEMPORARY(tk))
      end
      'WS10': begin
        u10 = self->GetVarData('U10', time, nt, YEARS=years)
        v10 = self->GetVarData('V10', YEARS=years)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WS=value
        return, value
      end
      'WD10': begin
        u10 = self->GetVarData('U10', time, nt, YEARS=years)
        v10 = self->GetVarData('V10', YEARS=years)
        MET_u_v_to_ws_wd, ret, TEMPORARY(u10), TEMPORARY(v10), WD=value
        return, value
      end
      else: Message, 'No'
    endcase
    
  endif else begin
  
    ; It is a product variable
    if ~ v.open then begin
      files = (*self.files)[v.pos[where(v.pos ne -1, nf)]]
      for i = 0, nf-1 do self.objs->Add, OBJ_NEW('w_GEO_nc', FILE=files[i])
      v.open = 1
      (*self.vars)[pv[0]] = v
    endif
    
    if v.type eq 'static' then begin
      obj = self.objs->FindByVar(id, COUNT=count)
      if TOTAL(self.subset) ne 0 then ok = obj->define_subset(SUBSET=self.subset) else ok = obj->define_subset()
      out = obj->get_Var(v.name, time, nt)
    endif else begin
      if N_ELEMENTS(YEARS) ne 0 then _y = years else _y = *self.years
      for y=0, N_ELEMENTS(_y)-1 do begin
        obj = self.objs->FindByVar(id, (_y)[y], COUNT=count)
        if TOTAL(self.subset) ne 0 then ok = obj->define_subset(SUBSET=self.subset) else ok = obj->define_subset()
        tmp = reform(obj->get_Var(v.name, t, ZLEVELS=zlevels))
        s = SIZE(tmp, /N_DIMENSIONS)
        if s le 3 then begin
          if N_ELEMENTS(out) eq 0 then out = TEMPORARY(tmp) else out = [[[out]],[[TEMPORARY(tmp)]]]
        endif else begin
          if N_ELEMENTS(out) eq 0 then out = TEMPORARY(tmp) else begin
            sout = size(out, /DIMENSIONS)
            stmp = size(tmp, /DIMENSIONS)
            out_ = TEMPORARY(out)
            out = FLTARR(sout+[0,0,0,stmp[3]])
            out[*,*,*,0:sout[3]-1] = TEMPORARY(out_)
            out[*,*,*,sout[3]:*] = TEMPORARY(tmp)
          endelse
        endelse
        if N_ELEMENTS(time) eq 0 then time = t else time = [time,t]
      endfor
      nt = N_ELEMENTS(time)
    endelse
    
    return, out
    
  endelse
  
end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_WPR__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_WPR                               ,  $
            INHERITS w_GISdata                  ,  $
            tres:               ''              ,  $ ; type of active directory: h, d, m, y
            hres:               ''              ,  $ ; type of active directory: d30km, d10km, d02km
            directory:          ''              ,  $ ; path to the file directory
            pressure_levels:    PTR_NEW()       ,  $ ; if 3d_press in the directory, the pressure levels
            vars:               PTR_NEW()       ,  $ ; array of var structures (see w_WPR::_varStruct)
            files:              PTR_NEW()       ,  $ ; Path to the files
            time:               PTR_NEW()       ,  $ ; Time array in QMS
            years:              PTR_NEW()       ,  $ ; The available product years
            objs:               OBJ_NEW()          $ ; the ncdf objects 
            }
    
end