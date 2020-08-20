
function change_cer_attributes_file, file

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
    3: dStr = 'd02km'
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
  title = 'Central Europe Refined analysis - CER V1 - ' + dStr
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
  sObj->WriteGlobalAttr, 'HISTORY', TIME_to_STR() + ' Modified attributes to make file COORDS conform.'
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

;+
; Change attributes to make wave products COORDS conform
;
;
; :Params:
;   directory (in): The directory to search for files
;   pattern   (in): The Pattern to use for searching files, default is '*.nc'
;   savefile  (in): Optional, information which files are ok is saved in the savefile
;   info     (out): A struct with the tags ok and file for each processed file.

;-

pro w_change_cer_attributes, directory, PATTERN=pattern, SAVEFILE=savefile, INFO=info

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2


  dir = directory
  i_pattern = arg_default('*.nc', pattern)

  files = FILE_SEARCH(dir, i_pattern, COUNT=cnt)
  if cnt eq 0 then begin
    message, "No files found for pattern " +pattern+ " in " + dir
  endif
  ok = LONARR(cnt)

  if TNT_is_gui then begin
    cgProgressBar = Obj_New("CGPROGRESSBAR", /Cancel)
    cgProgressBar->Start
  endif else begin
    frq = cnt-1 / 100
  endelse

  for i=0, cnt-1 do begin
    inF = files[i]
    ok[i] = change_cer_attributes_file(inF)
    if TNT_is_gui then begin
      cgProgressBar->update, (i / DOUBLE(cnt-1)) * 100
    endif else begin
      if i mod frq eq 0 then begin
        print, "Done" + str_equiv(i) + " from " +str_equiv(cnt-1) + " ( " +str_equiv((i / DOUBLE(cnt-1)) * 100) +" %)"
      endif
    endelse
  endfor

  if TNT_is_gui then begin
    cgProgressBar->Destroy
  endif

  if N_ELEMENTS(savefile) ne 0 then begin
    save, files, ok, FILENAME=savefile
  endif

  info = {ok:0b, file:''}
  info = replicate(info, cnt)
  info.ok = ok
  info.file = files

end
