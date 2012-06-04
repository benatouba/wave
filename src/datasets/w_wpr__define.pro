; docformat = 'rst'
;+
;
;  w_WPR is the basis class for WRF products directories.
;
; :History:
;     Written by FaM, 2011.
;
;
;-      


;+
; :Description:
;    Object structure definition. Attributes::
;       w_WPR                   
;            INHERITS w_WRF     
;            
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :History:
;     Written by FaM, 2011.
;-      
PRO w_WPR__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { w_WPR                    ,  $
            INHERITS w_WRF            ,  $
            tres:               ''    ,  $ ; type of active directory: h, d, m, y
            res:                ''    ,  $ ; type of active directory: d30km, d10km, d02km
            directory:          ''    ,  $ ; path to the file directory
            pr_time:        PTR_NEW() ,  $
            years:          PTR_NEW() ,  $
            objs:           OBJ_NEW()    $
            }
    
END

   
;+
; :Description:
;    Build function.
;
; :Keywords:
;       DIRECTORY   : in, optional, type = string
;                   the path to the WRF files directory. either m, d, h or y
;       _REF_EXTRA: in, optional
;                   all keywords accepted by w_WRF::define_subset
;
; :Returns:
; 
;    1 if the object is created successfully. 
;    
; :History:
;     Written by FaM, 2010.
;-
Function w_WPR::Init, DIRECTORY=directory, _REF_EXTRA=extra  
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    if self.cdfid gt 0 then ncdf_close, self.cdfid
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select WRF product directory to read', /MUST_EXIST, /DIRECTORY)
  if directory eq '' then MESSAGE, WAVE_Std_Message(/FILE)
  if ~ FILE_TEST(directory, /DIRECTORY) then MESSAGE, WAVE_Std_Message(/FILE)
  
  dir = utils_clean_path(directory)
  prdir = FILE_DIRNAME(dir)
  tres = FILE_BASENAME(dir)
  res = FILE_BASENAME(prdir)
  statdir = prdir + '/static'
  
  if ~ FILE_TEST(statdir, /DIRECTORY) then MESSAGE, 'Cannot find a static directory'
  
  self.tres = tres
  self.res = res
  self.directory = dir
  
  file_list=FILE_SEARCH(self.directory, '*_' + self.res +'_*.nc', count=filecnt)  
  if filecnt eq 0 then MESSAGE, 'No files in the directory?'  
  self.objs = OBJ_NEW('w_WRF_Container')
  for i=0, filecnt-1 do self.objs->Add, OBJ_NEW('w_GEO_nc', FILE=FILE_LIST[i])
  
  file_lists=FILE_SEARCH(statdir, '*_' + self.res +'_*.nc', count=filecnt)  
  if filecnt eq 0 then MESSAGE, 'No files in the static directory?'  
  for i=0, filecnt-1 do self.objs->Add, OBJ_NEW('w_GEO_nc', FILE=FILE_LISTs[i])
  
  IF NOT self->w_WRF::Init(file=file_list[0], _EXTRA=extra) THEN RETURN, 0 
    
  for y=2000, 2012 do begin 
    object = self.objs->FindByVar('t2', y, COUNT=count)
    if COUNT ne 0 then begin      
      object->get_time, t
      if N_ELEMENTS(time) eq 0 then time=t else time=[time,t]
      if N_ELEMENTS(years) eq 0 then years=y else years=[years,y]
    endif    
  endfor 
  self.years = PTR_NEW(years)
  self.pr_time = PTR_NEW(time)
    
  RETURN, 1
  
END

;+
; :Description:
;    Destroy function. 
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :History:
;     Written by FaM, 2010.
;-      
pro w_WPR::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_WRF::Cleanup  
  OBJ_DESTROY, self.objs
  PTR_FREE, self.pr_time
  PTR_FREE, self.years
  
END


;+
; :Description:
;   Retrieve time info.
;    
; :Categories:
;         WAVE/OBJ_GIS   
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
pro w_WPR::get_time, time, nt, t0, t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  time = *self.pr_time
  nt = N_ELEMENTS(time)
  t0 = MIN(time)
  t1 = MAX(time)
  
end

;+
; :Description:
;    This function checks if a variable ID is valid and returns 1 if it is. Additionally,
;    it tries to obtain a maximum of information about the desired variable.
;    
;    This function have been enhanced for `w_WPR` to include additional diagnostic variables.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid: in, required, type = string/integer
;           the variable ID (string or integer) to check
;
; :Keywords:
;   out_id: out, type = long
;           the netcdf variable ID (long)
;   description: out, type = string
;               If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions
;   dimnames: out, type = string
;             the dimensions names
; 
; :Returns:
;         1 if the variable id is valid, 0 if not
;
;       :History:
;     Written by FaM, 2010.
;-
function w_WPR::get_Var_Info, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                              out_id = out_id, $
                              units = units, $
                              description = description, $
                              varname = varname , $ ; 
                              dims = dims, $ ;
                              dimnames = dimnames, $ ;
                              is_static = is_static, $ ;
                              is_original = is_original ;
                              
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, FALSE
  ENDIF
  
  
  if ~arg_okay(Varid, TYPE=IDL_STRING, /SCALAR) then Message, WAVE_Std_Message('varName', /ARG)
  
  is_original = FALSE
  is_static = FALSE
  ; Is it a template variable?
  tpl = self.objs->Get(POSITION=0)
  ok = tpl->get_Var_Info(Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable.
    out_id = out_id, $
    units = units, $
    description = description, $
    varname = varname , $ ;
    dims = dims, $ ;
    dimnames = dimnames)
  tpl->getProperty, NVARS=nv
  nodiag = FALSE
  if ok then begin
    nodiag = TRUE
    ok = ok and (out_id ne (nv-1))
  endif
  if ~ ok then begin
    
    ;is it a diagnostic variable?
    ok = self->w_WRF::get_Var_Info(Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable.
      out_id = out_id, $
      units = units, $
      description = description, $
      varname = varname , $ ;
      dims = dims, $ ;
      dimnames = dimnames)
    if ~ ok or NODIAG then begin
      
      ; Can you find a variable object with this name?
      object = self.objs->FindByVar(Varid, COUNT=count)
      if count gt 0 then begin
       is_original = TRUE
       if count eq 1 then is_static = TRUE 
       vid = utils_replace_string(Varid, '_press', '')
       vid = utils_replace_string(vid, '_eta', '')
       return, (object[0])->get_Var_Info(vid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable.
        out_id = out_id, $
        units = units, $
        description = description, $
        varname = varname , $ ;
        dims = dims, $ ;
        dimnames = dimnames) 
       endif else return, 0
    endif
    
  endif
  
  return, TRUE
  
end

pro w_WPR::get_Varlist, varnames, PRINTVARS=printvars

  varnames = self.objs->getVarnames(COUNT=cnt)
 
  if ~KEYWORD_SET(PRINTVARS) then return

  d3 = BYTARR(N_ELEMENTS(varnames))
  soil = BYTARR(N_ELEMENTS(varnames))
  
  pok = where(StrMatch(varnames, '*_eta*') or StrMatch(varnames, '*_press*'), cnt)  
  d3[pok] = 1
  varnames = utils_replace_string(varnames, '_eta', '')
  varnames = utils_replace_string(varnames, '_press', '')
  pok = where(StrMatch(varnames, '*sh2o*') or StrMatch(varnames, '*smcrel*') or StrMatch(varnames, '*smois*') or StrMatch(varnames, '*tslb*'), cnt)
  soil[pok] = 1
  
  d3 = d3[UNIQ(varnames, SORT(varnames))]
  soil = soil[UNIQ(varnames, SORT(varnames))]
  varnames = varnames[UNIQ(varnames, SORT(varnames))]
  
  type = STRARR(N_ELEMENTS(varnames))
  type[*] = '2d'
  type[where(d3)] = '3d'
  type[where(soil)] = 'soil'
  
  print, 'NAME                 TYPE    DESCRIPTION                                    UNIT '
      
  for i = 0, N_ELEMENTS(varnames)-1 DO begin
    vn = varnames[i]
    if d3[i] then vn = vn + '_eta'
    ok= self->get_Var_Info(vn, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable.
      out_id = out_id, $
      units = units, $
      description = description, $
      varname = varname , $ ;
      dims = dims, $ ;
      dimnames = dimnames, $ ;
      is_static = is_static, $ ;
      is_original = is_original)
      
    ns = '                                                                                  '
    STRPUT, ns, varnames[i], 1
    STRPUT, ns, type[i], 22
    STRPUT, ns, description, 30
    STRPUT, ns, units, 77

    print, ns
  endfor
  
end

;+
; :Description:
;    This function reads a variable from the netcdf file and makes a
;    subset of it if it has been previously set with 'define_subset'.
;    
;    There is the possibility to restrict the retrieved variable to 
;    a given time period (keywords `T0` and `T1`)
;    
;    Additionaly to the "standard" variables available in the WRF file,
;    a few diagnostic variables are computed automatically.
;    Here is a list of the diagnostic variables (only if needed WRF 
;    variables are present. Check the available variables with
;    `w_WRF::get_Varlist, /DIAGNOSTIC, /PRINT`) ::
;              
;             prcp: total precipitation (step-wize) [mm]
;             snowfall: Grid scale snow and ice (step-wize) [mm]
;             prcp_c: Cumulus precipitation (step-wize) [mm]
;             prcp_nc: Grid scale precipitation (step-wize) [mm]
;             prcp_fr: Frozen precipitation (step-wize) [mm]
;             graupel: Grid scale graupel (step-wize) [mm]
;             hail: Grid scale hail (step-wize) (step-wize) [mm]             
;             potevap:  Potential evaporation (step-wize) [w m-2]             
;             rh: Relative Humidity [%]
;             rh2: 2m Relative Humidity [%]
;             td: Dewpoint temperature [C]
;             td2: 2m Dewpoint temperature [C]
;             slp: Sea level pressure [hPa] (computed with full vertical levels - slow. See `utils_wrf_slp` 
;                  (If the vertical dimension is not present in the file, slp_b is computed automatically instead)
;             slp_b: Sea level pressure [hPa] (computed with surface values - fast. see `MET_barometric` for more info)
;             ter: Model terrain height [m] (static: no time dimension)
;             lucat: Model landuse category [] (static: no time dimension)
;             soiltop: Model soil category top [] (static: no time dimension)
;             soilbot: Model soil category bot [] (static: no time dimension)
;             tc: Temperature [C]
;             t2c: 2m Temperature [C]
;             t2pbl: 2 m temperature (extrapolated from eta-levels) [K]
;             t2pblc: 2 m temperature (extrapolated from eta-levels) [C]
;             theta: Potential temperature [K]
;             tk: Temperature [K]
;             ws10: wind speed at 10m [m.s-1] TODO: rotated to earth coordinates
;             wd10: wind direction [degrees] TODO: rotated to earth coordinates
;             geopotential: Full model geopotential [m2 s-2] (unstaggered)
;             pressure: Full model pressure [hPa]
;             z: Full model height (geopotential / 9.81) [m]
;             
;             TODO: umet10, vmet10, umet, vmet, components of wind rotated to earth coordinates
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Params:
;    Varid: in, required, type = string/integer
;           the variable ID (string or integer) to retrieve
;    time:  out, type = qms
;           the variable times
;    nt: out, type = long
;        the variable number of times
;
; :Keywords:
;   T0: in, optional, type = qms/{ABS_DATE}
;       if set, it defines the first time of the variable timeserie
;   T1: in, optional, type = qms/{ABS_DATE}
;       if set, it defines the last time of the variable timeserie
;   YEARS: in, optional, type = long
;          if set, it defines the years to take in account
;   description: out, type = string
;                If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions (if the variable is cropped, the dimensions are updated too)
;   dimnames: out, type = string
;             the dimensions names (if the variable is cropped, the dimension names are updated too)
; 
; :Returns:
;         The variable
;
; :History:
;      Written by FaM, 2011.
;-
function w_WPR::get_Var, Varid, $ 
                            time,  $
                            nt,  $
                            T0=t0, $
                            T1=t1, $
                            YEARS=years, $
                            UNSTAGGER=unstagger, $
                            ETA_LEVELS=eta_levels, $
                            ZLEVELS=zlevels, $
                            PRESSURE_LEVELS=pressure_levels, $
                            HEIGHT_LEVELS=height_levels, $
                            ABOVE_GROUND_LEVELS=above_ground_levels, $
                            ACC_TO_STEP=acc_to_step , $
                            UNITS=units, $
                            DESCRIPTION=description, $
                            VARNAME=varname , $ 
                            DIMS=dims, $ 
                            DIMNAMES=dimnames 

  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  undefine, count, offset, time

  ;Check if the variable is available
  if ~self->get_Var_Info(Varid, out_id=vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames, $
    is_static = is_static, $ 
    is_original = is_original) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
    
  if is_original then begin
    if is_static then begin
      obj = self.objs->FindByVar(Varid, COUNT=count)
      ok = obj->define_subset(SUBSET=self.subset)
      obj->getProperty, Nvars=Nvars
      out = obj->get_Var(Nvars-1, time, nt)
    endif else begin
      if N_ELEMENTS(years) ne 0 then _y = years else _y = *self.years
      for y=0, N_ELEMENTS(_y)-1 do begin
        obj = self.objs->FindByVar(Varid, (_y)[y], COUNT=count)
        ok = obj->define_subset(SUBSET=self.subset)
        obj->getProperty, Nvars=Nvars
        tmp = reform(obj->get_Var(Nvars-1, t, ZLEVELS=zlevels))
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
  endif else return, self->w_WRF::get_Var(Varid, $ 
                            time,  $
                            nt,  $
                            T0=t0, $
                            T1=t1, $
                            UNITS=units, $
                            DESCRIPTION=description, $
                            VARNAME=varname , $ 
                            DIMS=dims, $ 
                            DIMNAMES=dimnames)
  
  
end

pro w_WPR::altitudinal_gradient, Varid, grad_VarName, $
    FORCE=force, $
    KERNEL_SIZE=kernel_size, $
    DEFAULT_VAL=default_val, $
    CLIP_MIN=clip_min, $
    CLIP_MAX=clip_max, $
    MIN_SIG=min_sig
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ;ON_ERROR, 2
  
  undefine, count, offset, time
  
  ;Check if the variable is available
  if ~self->get_Var_Info(Varid, out_id=vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames, $
    is_static = is_static, $
    is_original = is_original) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
    
  height = self->get_Var('hgt')
 
  _gradvn = STRLOWCASE(grad_VarName)
  
  if N_ELEMENTS(KERNEL_SIZE) eq 0 then kernel_size = 9
  if N_ELEMENTS(DEFAULT_VAL) eq 0 then default_val = 0.
  if N_ELEMENTS(MIN_SIG) eq 0 then min_sig = 0.5
  
  _y = *self.years
  for y=0, N_ELEMENTS(_y)-1 do begin
    obj = self.objs->FindByVar(Varid, (_y)[y], COUNT=count)
    ok = obj->define_subset(SUBSET=self.subset)
    obj->getProperty, Nvars=Nvars, PATH=fPath
    oPath= utils_replace_string(fPath, VarName, _gradvn)
    
    ; Check
    if FILE_TEST(oPath) and ~KEYWORD_SET(FORCE) then Message, 'Output file already here. Set /FORCE if you want to overwrite it.'
    
    ; Make File
    ; Open the source file in read-only mode.
    sObj = Obj_New('NCDF_FILE', fPath, $
      ErrorLoggerName='sourcefilelogger', /TIMESTAMP)
    IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
    
    ; Open the destination file for writing.
    dObj = Obj_New('NCDF_FILE', oPath, /CREATE, CLOBBER=force, /NETCDF4_FORMAT, $
      ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
    
    ; Find all the global attributes in the source file and copy them.
    attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
    FOR j=0,attrCount-1 DO BEGIN
      if str_equiv(attrNames[j]) eq 'CREATION_DATE' then begin
        dObj->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
        continue
      endif
      if str_equiv(attrNames[j]) eq 'VARNAME' then begin
        dObj->WriteGlobalAttr, 'VARNAME', _gradvn, DATATYPE='CHAR'
        continue
      endif
      sObj->CopyGlobalAttrTo, attrNames[j], dObj
    ENDFOR
    
    ; Find all the dimensions in the source file and copy them.
    dimNames = sObj -> GetDimNames(COUNT=dimCount)
    FOR j=0,dimCount-1 DO BEGIN
      if str_equiv(dimNames[j]) eq 'TIME' then begin
        dObj->WriteDim, 'time', /UNLIMITED
        continue
      endif
      sObj->CopyDimTo, dimNames[j], dObj
    ENDFOR
    
    ; Find all the variable definitions, attributes and data in the
    ; source file and copy them.
    varNames = sObj->GetVarNames(COUNT=varCount)
    FOR j=0,varCount-1 DO BEGIN
      if str_equiv(varNames[j]) eq str_equiv(varId) then continue
      sObj->CopyVarDefTo, varNames[j], dObj
      varAttrNames = sObj -> GetVarAttrNames(varNames[j], COUNT=varAttrCount)
      FOR k=0,varAttrCount-1 DO BEGIN
        sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObj
      ENDFOR
      sObj->CopyVarDataTo, varNames[j], dObj
    ENDFOR
    
    des_str = 'Altitudinal gradient from var ' + varId
    des_str += ' KERNEL_SIZE='+str_equiv(KERNEL_SIZE)
    des_str += ' DEFAULT_VAL='+str_equiv(DEFAULT_VAL)
    des_str += ' MIN_SIG='+str_equiv(MIN_SIG)
    if N_ELEMENTS(CLIP_MIN) ne 0 then des_str += ' CLIP_MIN='+str_equiv(CLIP_MIN) else des_str += ' NO_CLIP_MIN'
    if N_ELEMENTS(CLIP_MAX) ne 0 then des_str += ' CLIP_MAX='+str_equiv(CLIP_MAX) else des_str += ' NO_CLIP_MAX'
    
    dObj->WriteVarDef, _gradvn, ['west_east','south_north','time'], DATATYPE='FLOAT'
    dObj->WriteVarAttr, _gradvn, 'long_name', des_str
    dObj->WriteVarAttr, _gradvn, 'units', units + '.m-1'
    dObj->WriteVarAttr, _gradvn, 'agg_method', 'MEAN'
    
    ; Compute
    data = reform(obj->get_Var(Nvars-1, t))
    grad = w_altitudinal_gradient(data, height, $
      KERNEL_SIZE=kernel_size, $
      DEFAULT_VAL=default_val, $
      CLIP_MIN=clip_min, $
      CLIP_MAX=clip_max, $
      SIG=sig)
            
    pnok = where(sig lt MIN_SIG, cntnok)
    if cntnok ne 0 then grad[pnok] = default_val
       
    ; Fill with data
    dObj->SetMode, /DATA
    dObj->WriteVarData, _gradvn, grad
    
    undefine, dObj, sObj
    
    self.objs->Add, OBJ_NEW('w_GEO_nc', FILE=oPath)
    
  endfor
  
end

;+
; :Description:
;    This function reads a variable from the file but only
;    at a specific location. The output is a vector of 
;    nt elements, where nt is the number of times in the 
;    time serie.
;
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :Params:
;    varid: in, required, type = string/integer
;           the variable ID (string or integer) to retrieve
;    x: in, required, type = long
;       the X coordinate of the point where to get the variable (if SRC is not specified, it is an index within the Grid)
;    y: in, required, type = long
;       the Y coordinate of the point where to get the variable (if SRC is not specified, it is an index within the Grid)
;    time:  out, type = qms
;           the variable times
;    nt: out, type = long
;        the variable number of times
;
; :Keywords:
;    t0: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the first time of the variable timeserie
;    t1: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the last time of the variable timeserie
;    src: in, optional
;         the coordinate system (w_Grid2D or {TNT_PROJ} or {TNT_DATUM}) in which x and y are defined
;    point_i: out, optional
;             the i index in the grid where the nearest point was found
;    point_j: out, optional
;             the j index in the grid where the nearest point was found
;    point_lon: out, optional
;              the longitude of the nearest grid point
;    point_lat: out, optional
;              the latitude of the nearest grid point
;    dist_x: out, optional
;            the easting difference between the (x,y) point and the nearest grid point 
;    dist_y: out, optional
;            the northing difference between the (x,y) point and the nearest grid point 
;    units: out, type = string
;           If available, the units of the variable
;    description: out, type = string
;                 If available, the description of the variable
;    varname: out, type = string
;             the name of the variable
;    dims: out, type = long
;          the variable dimensions
;    dimnames: out, type = string
;              the dimensions names
;              
; :History:
;     Written by FaM, 2010.
;-      
function w_WPR::get_TimeSerie,varid, x, y, $
                              time, nt, $
                              T0=t0, T1=t1, $
                              SRC=src, $
                              K=K, $
                              POINT_I=point_i, $
                              POINT_J=point_j, $
                              POINT_LON=point_lon, $
                              POINT_LAT=point_lat, $
                              DIST_X = dist_x, $
                              DIST_Y = dist_y, $
                              UNITS=units, $
                              DESCRIPTION=description, $
                              VARNAME=varname , $ ; 
                              DIMS=dims, $ ;
                              DIMNAMES=dimnames 
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  undefine, time
  
  if N_PARAMS() lt 3 then Message, WAVE_Std_Message(/NARG)
  if ~arg_okay(VarId, /SCALAR) then MEssage, WAVE_Std_Message('VarId', /SCALAR)
  
  ;Check if the variable is available
  if ~self->get_Var_Info(Varid, out_id = vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames, $
    is_original = is_original) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
    
  ; GIS
  if N_ELEMENTS(src) EQ 0 then mysrc = self else mysrc = src
  ; This is to obtain the indexes in the grid
  self->transform,  x, y, point_i, point_j, SRC = mysrc, /NEAREST, E_DST=_x, N_DST=_y
  ; This is to obtain lat and lons of the selected grid point
  self->transform, point_i, point_j, dummy, dummy, src=self, $
    LON_DST=point_lon, LAT_DST=point_lat, E_DST=point_x, N_DST=point_y
  dist_x = _x - point_x
  dist_y = _y - point_y
  
  if is_original then begin
  
    for yrs=0, N_ELEMENTS(*self.years)-1 do begin
    
      obj = self.objs->FindByVar(Varid, (*self.years)[yrs], COUNT=count)
      ok = obj->define_subset(SUBSET=self.subset)
      obj->getProperty, Nvars=Nvars
      tmp = obj->w_GEO_nc::get_TimeSerie(Nvars-1, point_i, point_j, t, $
        dims = dims, $ ;
        dimnames = dimnames)
      if N_ELEMENTS(out) eq 0 then out = tmp else out = [out,tmp]
      if N_ELEMENTS(time) eq 0 then time = t else time = [time,t]
      
    endfor
    
    nt = N_ELEMENTS(time)
    
  endif else begin
  
    if str_equiv(varid) eq 'T2C' then begin
    
      out=self->get_TimeSerie( 'T2', x, y, $
        time, nt, $
        T0=t0, T1=t1, $
        SRC=src, $
        K=K, $
        POINT_I=point_i, $
        POINT_J=point_j, $
        POINT_LON=point_lon, $
        POINT_LAT=point_lat, $
        DIST_X = dist_x, $
        DIST_Y = dist_y, $
        UNITS=units, $
        DESCRIPTION=description, $
        VARNAME=varname , $ ;
        DIMS=dims, $ ;
        DIMNAMES=dimnames) - 273.15
    endif
    
    if str_equiv(varid) eq 'RH2' then begin
    
      T2 = self->get_Var('T2', x, y, $
        time, nt, $
        T0=t0, T1=t1, $
        SRC=src, $
        K=K, $
        POINT_I=point_i, $
        POINT_J=point_j, $
        POINT_LON=point_lon, $
        POINT_LAT=point_lat, $
        DIST_X = dist_x, $
        DIST_Y = dist_y, $
        UNITS=units, $
        DESCRIPTION=description, $
        VARNAME=varname , $ ;
        DIMS=dims, $ ;
        DIMNAMES=dimnames)
      PSFC = self->get_Var('PSFC', x, y, $
        time, nt, $
        T0=t0, T1=t1, $
        SRC=src)
      Q2 = self->get_Var('Q2', x, y, $
        time, nt, $
        T0=t0, T1=t1, $
        SRC=src) > 0.
      out = utils_wrf_rh(Q2, PSFC, T2)
    endif
    
    if N_ELEMENTS(out) eq 0 then Message, str_equiv(varid) + ' not available yet'
    
  endelse
  
  return, out
  
end


