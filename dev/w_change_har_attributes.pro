function change_test_time, file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' ' + file)
    RETURN, 0
  ENDIF 
  
  do_time = 0
  wObj = w_geo_nc(FILE=file)
  wObj->get_time, time, nt
  
  bn = FILE_BASENAME(file)
  if FILE_BASENAME(FILE_DIRNAME(file)) ne 'static' then begin
    year = LONG(STRMID(bn, STRLEN(bn)-7, 4))
    tts = MAKE_ABS_DATE(QMS=time)
    p = where(tts.year ne year, cnt)
    if cnt ne 0 then begin
      if cnt eq 1 and p[0] eq (nt-1) then begin
      ;ok
      endif else begin
        test = 1
        print, 'Houston!'
        print, file
      endelse
    endif
  endif else begin
    print, 'Static!'
    print, file
    print, TIME_to_STR(time)
  end    

  undefine, wObj
  return, 1

end


function change_har_attributes_file, file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' ' + file)
    RETURN, 0
  ENDIF 
  
  do_time = 0
  wObj = w_geo_nc(FILE=file)
  wObj->get_time, time, nt
  
  bn = FILE_BASENAME(file)
  if FILE_BASENAME(FILE_DIRNAME(file)) ne 'static' then begin
    year = LONG(STRMID(bn, STRLEN(bn)-7, 4))
    tts = MAKE_ABS_DATE(QMS=time)
    p = where(tts.year ne year, cnt)
    if cnt ne 0 then begin
      if cnt eq 1 and p[0] eq (nt-1) then begin
      ;ok
      endif else begin
        test = 1
        print, 'Houston!'
        print, file
      endelse
    endif
  endif else begin
    print, 'Static!'
    print, file
    print, TIME_to_STR(time)
  end    
  if nt eq 12 or nt eq 1 then begin
    do_time = 1
    tindays = (time - time[0])/D_QMS
  endif
  undefine, wObj

  sObj = Obj_New('NCDF_FILE', file, /MODIFY)
  IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
  
  DOMAIN = sObj->GetGlobalAttrValue('DOMAIN')
  case (DOMAIN) of
    1: dStr = 'd30km'
    2: dStr = 'd10km'
    else: Message, 'no'
  endcase
  
  nx = sObj->GetDimValue('west_east')
  ny = sObj->GetDimValue('south_north')
  
  xx = sObj->GetVarData('west_east')
  yy = sObj->GetVarData('south_north')
  x00 = min(xx)
  y00 = min(yy)
  x01 = min(xx)
  y01 = max(yy)
 
  TITLE = sObj->GetGlobalAttrValue('TITLE')
  DATA_NOTES = sObj->GetGlobalAttrValue('DATA_NOTES')
  WRF_VERSION = sObj->GetGlobalAttrValue('WRF_VERSION')
  CREATED_BY = sObj->GetGlobalAttrValue('CREATED_BY')
  INSTITUTION = sObj->GetGlobalAttrValue('INSTITUTION')
  CREATION_DATE = sObj->GetGlobalAttrValue('CREATION_DATE')
  SOFTWARE_NOTES = sObj->GetGlobalAttrValue('SOFTWARE_NOTES')
  VARNAME = sObj->GetGlobalAttrValue('VARNAME')
   
  PROJ_ENVI_STRING = sObj->GetGlobalAttrValue('PROJ_ENVI_STRING')
  PROJECTION = sObj->GetGlobalAttrValue('PROJECTION')
  DATUM = sObj->GetGlobalAttrValue('DATUM')
  DOMAIN = sObj->GetGlobalAttrValue('DOMAIN')
  NESTED = sObj->GetGlobalAttrValue('NESTED')
  TIME_ZONE = sObj->GetGlobalAttrValue('TIME_ZONE')
  GRID_INFO = sObj->GetGlobalAttrValue('GRID_INFO')  
  DX = sObj->GetGlobalAttrValue('DX')
  DY = sObj->GetGlobalAttrValue('DY')
  X0 = sObj->GetGlobalAttrValue('X0')
  Y0 = sObj->GetGlobalAttrValue('Y0')
  PRODUCT_LEVEL = sObj->GetGlobalAttrValue('PRODUCT_LEVEL')
  LEVEL_INFO = sObj->GetGlobalAttrValue('LEVEL_INFO')
  
  projection = 'Lambert Conformal Conic' 
  title = 'High Asia Reanalysis - HAR V1 - ' + dStr
  PROJ_ENVI_STRING = utils_replace_string(PROJ_ENVI_STRING, '.00000000', '.0')
  PROJ_ENVI_STRING = utils_replace_string(PROJ_ENVI_STRING, ',      ', ', ')
  
  toDel = sObj->GetGlobalAttrNames()
  for i=0, N_ELEMENTS(toDel)-1 do begin
    sObj->DelGlobalAttr, toDel[i]
  endfor
  
  sObj->WriteGlobalAttr, 'TITLE', TITLE
  sObj->WriteGlobalAttr, 'DATA_NOTES', DATA_NOTES
  sObj->WriteGlobalAttr, 'WRF_VERSION', WRF_VERSION
  sObj->WriteGlobalAttr, 'CREATED_BY', CREATED_BY
  sObj->WriteGlobalAttr, 'INSTITUTION', INSTITUTION
  sObj->WriteGlobalAttr, 'CREATION_DATE', CREATION_DATE
  sObj->WriteGlobalAttr, 'SOFTWARE_NOTES', SOFTWARE_NOTES
  sObj->WriteGlobalAttr, 'VARNAME', VARNAME
  
  sObj->WriteGlobalAttr, 'DOMAIN', str_equiv(DOMAIN)
  sObj->WriteGlobalAttr, 'NESTED', NESTED
  sObj->WriteGlobalAttr, 'TIME_ZONE', TIME_ZONE
  sObj->WriteGlobalAttr, 'PRODUCT_LEVEL', str_equiv(PRODUCT_LEVEL)
  sObj->WriteGlobalAttr, 'LEVEL_INFO', LEVEL_INFO + 'S: static'
  
  projlis = STRSPLIT(PROJ_ENVI_STRING, ',', /EXTRACT)
  sObj->WriteGlobalAttr, 'PROJ_NAME', PROJECTION
  sObj->WriteGlobalAttr, 'PROJ_CENTRAL_LON', projlis[4]
  sObj->WriteGlobalAttr, 'PROJ_CENTRAL_LAT', projlis[3]
  sObj->WriteGlobalAttr, 'PROJ_STANDARD_PAR1', projlis[7]
  sObj->WriteGlobalAttr, 'PROJ_STANDARD_PAR2', projlis[8]
  sObj->WriteGlobalAttr, 'PROJ_SEMIMAJOR_AXIS', projlis[1]
  sObj->WriteGlobalAttr, 'PROJ_SEMIMINOR_AXIS', projlis[2]
  sObj->WriteGlobalAttr, 'PROJ_FALSE_EASTING', projlis[5]
  sObj->WriteGlobalAttr, 'PROJ_FALSE_NORTHING', projlis[6]
  sObj->WriteGlobalAttr, 'PROJ_DATUM', 'WGS-84'
  sObj->WriteGlobalAttr, 'PROJ_ENVI_STRING', proj_envi_string
  
  
  sObj->WriteGlobalAttr, 'GRID_INFO', 'Grid spacing: GRID_DX and GRID_DY (unit: m), Down left corner: GRID_X00 and GRID_Y00 (unit: m), Upper Left Corner: GRID_X01 and GRID_Y01 (unit: m)'
  sObj->WriteGlobalAttr, 'GRID_DX', dx
  sObj->WriteGlobalAttr, 'GRID_DY', dy
  sObj->WriteGlobalAttr, 'GRID_X00', x00
  sObj->WriteGlobalAttr, 'GRID_Y00', y00
  sObj->WriteGlobalAttr, 'GRID_X01', x01
  sObj->WriteGlobalAttr, 'GRID_Y01', y01
  sObj->WriteGlobalAttr, 'GRID_NX', nx
  sObj->WriteGlobalAttr, 'GRID_NY', ny
  
  varName = sObj->GetGlobalAttrValue('VARNAME')  
  sObj->writeVarAttr, varName, 'coordinates', 'lon lat'
  
  if str_equiv(varname) eq 'SCLDFRA' then begin
    sObj->WriteVarAttr, 'scldfra', 'long_name', 'Surface cloud fraction computed in a 50km radius FOV (Moelg and Kaser 2011).' 
  endif
  
  if do_time then begin
    units = sObj->GetVarAttrValue('time', 'units')
    if nt eq 12 then begin
      if STRMID(units, 0,6) ne 'months' then message, 'wrf'
      units = utils_replace_string(units, 'months', 'days')
    endif
    if nt eq 1 then begin
      if STRMID(units, 0,5) eq 'years' then begin
        units = utils_replace_string(units, 'years', 'days')
      endif else begin
        do_time = 0
      endelse
    endif
    if do_time then begin
      sObj->WriteVarAttr, 'time', 'units', units
      sObj->WriteVarData, 'time', tindays
    endif
  endif
  
  sObj->Sync
  undefine, sObj 
  
  return, 1

end

pro w_change_har_names
 ; SET UP ENVIRONNEMENT
 
  @WAVE.inc
  COMPILE_OPT IDL2
 
 dir = '/cfs/tip-pr1/HAR/d30km/'
 
 files = FILE_SEARCH(dir, '*tibet*.nc', COUNT=cnt)
 ok = LONARR(cnt)
 
 for i=0, cnt-1 do begin
  inF = files[i]  
  outF = utils_replace_string(inF, '/tibet_', '/har_')  
  FILE_MOVE, inF, outF, /VERBOSE   
 endfor
 
end

pro w_change_check_har_attributes

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  dir = '/cfs/tip-pr1/HAR/d30km/'
  
  ; files = FILE_SEARCH(dir, '*tibet*.nc', COUNT=cnt)
  files = FILE_SEARCH(dir, '*.nc', COUNT=cnt)
  ok = LONARR(cnt)
  
 cgProgressBar = Obj_New("CGPROGRESSBAR", /Cancel)
 cgProgressBar->Start    
  for i=0, cnt-1 do begin
    inF = files[i]
    ;  outF = utils_replace_string(inF, '/tibet_', '/har_')
    ;  FILE_MOVE, inF, outF, /VERBOSE
;    print, inF
    ok[i] = change_test_time(inF)
    cgProgressBar->update, (i / DOUBLE(cnt-1)) * 100
  endfor
  cgProgressBar->Destroy
  
;  save, files, ok, FILENAME='w_change_har_attributes_definitiv_har10.sav'
  
end

pro w_change_har_attributes

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  dir = '/cfs/tip-pr1/HAR/d10km/'
  
  ; files = FILE_SEARCH(dir, '*tibet*.nc', COUNT=cnt)
  files = FILE_SEARCH(dir, '*har*.nc', COUNT=cnt)
  ok = LONARR(cnt)
  
  cgProgressBar = Obj_New("CGPROGRESSBAR", /Cancel)
  cgProgressBar->Start    
  for i=0, cnt-1 do begin
    inF = files[i]
    ;  outF = utils_replace_string(inF, '/tibet_', '/har_')
    ;  FILE_MOVE, inF, outF, /VERBOSE
    ok[i] = change_har_attributes_file(inF)
    cgProgressBar->update, (i / DOUBLE(cnt-1)) * 100
  endfor
  cgProgressBar->Destroy
  
  save, files, ok, FILENAME='w_change_har_attributes_definitiv_har10.sav'
  
end
;
;pro test_change_har_attributes
;  
;  outD = '/home/mowglie/tmp/test_aattr/'
;    
;  inF =  '/home/mowglie/disk/Data/Products/HAR/d30km/m/2d/tibet_d30km_m_2d_albedo_2003.nc'
;  inF =   '/home/mowglie/disk/Data/Products/HAR/d30km/static/tibet_d30km_static_hgt.nc'
;  
;  tf = utils_replace_string(utils_replace_string(inF, '/HAR/', '/HAR_DS/'), 'tibet_', 'har_')
;  
;  fname = FILE_BASENAME(inF)
;  outF = outD + utils_replace_string(fname, 'tibet_', 'har_')
;  
;  FILE_COPY, inF, outF, /OVERWRITE
;  change_har_attributes_file, outF
;  
;  wi = w_NCDF(FILE=inF)
;  wi->dump, FILE= outD +'orig.txt'
;  wi = w_NCDF(FILE=outF)
;  wi->dump, FILE= outD +'new.txt'  
;  wi = w_NCDF(FILE=tF)
;  wi->dump, FILE= outD + 'ds.txt'  
;  
;
;end