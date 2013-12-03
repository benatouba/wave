pro w_change_har_attributes, file

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
  sObj->WriteGlobalAttr, 'PROJ_CENTRAL_LON', projlis[3]
  sObj->WriteGlobalAttr, 'PROJ_CENTRAL_LAT', projlis[4]
  sObj->WriteGlobalAttr, 'PROJ_STANDARD_PAR1', projlis[7]
  sObj->WriteGlobalAttr, 'PROJ_STANDARD_PAR2', projlis[8]
  sObj->WriteGlobalAttr, 'PROJ_SEMIMAJOR_AXIS', projlis[1]
  sObj->WriteGlobalAttr, 'PROJ_SEMIMINOR_AXIS', projlis[2]
  sObj->WriteGlobalAttr, 'PROJ_FALSE_EASTING', projlis[5]
  sObj->WriteGlobalAttr, 'PROJ_FALSE_NORTHING', projlis[6]
  sObj->WriteGlobalAttr, 'PROJ_ENVI_STRING', PROJ_ENVI_STRING
  
  
  sObj->WriteGlobalAttr, 'GRID_INFO', 'Grid spacing: GRID_DX and GRID_DY (unit: m), Down left corner: GRID_X00 and GRID_Y00 (unit: m), Upper Left Corner: GRID_X01 and GRID_Y01 (unit: m)'
  sObj->WriteGlobalAttr, 'GRID_DX', dx
  sObj->WriteGlobalAttr, 'GRID_DY', dy
  sObj->WriteGlobalAttr, 'GRID_X00', x00
  sObj->WriteGlobalAttr, 'GRID_Y00', y00
  sObj->WriteGlobalAttr, 'GRID_X01', x01
  sObj->WriteGlobalAttr, 'GRID_Y01', y01
  sObj->WriteGlobalAttr, 'GRID_NX', nx
  sObj->WriteGlobalAttr, 'GRID_NY', ny
  
  sObj->Sync
  undefine, sObj 

end