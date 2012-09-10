pro regional_hydro_product, wpr, shapeFile, destFile

  ; Make a subset around the shape
  ok = wpr->definesubset()
  ok = wpr->definesubset(SHAPE=shapeFile, MARGIN=5)
  wpr->Get_LonLat, lon, lat, nx, ny
  wpr->Get_XY, xx, yy, nx, ny
  wpr->getProperty, TNT_C=c
  wrf_proj = c.proj
  
  ; Show it
  map = OBJ_NEW('w_Map', wpr, YSIZE=700, /HR_BLUE_MARBLE)
  ok = map->set_shape_file(SHPFILE = shapeFile, COLOR='blue', THICK=3)
  ok = map->set_point(lon, lat, PSYM=16, SYMSIZE=1, COLOR='red')
  w_standard_2d_plot, map, /NO_BAR, TITLE=FILE_BASENAME(destFile), PNG=utils_replace_string(destFile, '.nc', '.png')
  undefine, map
  
  wpr->getTime, time, nt, t0, t1
  
  ;find a template file
  wpr->GetProperty, DIRECTORY=dir, HRES=res
  file_list=FILE_SEARCH(dir, '*_' + res +'_*.nc', count=filecnt)
  
  ; Open the source file in read-only mode.
  sObj = Obj_New('NCDF_FILE', file_list[0], /TIMESTAMP)
  IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
  
  ; Open the destination file for writing.
  dObj = Obj_New('NCDF_FILE', destFile, /CREATE, /CLOBBER, /TIMESTAMP, /NETCDF4_FORMAT)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
  
  
  ; Find all the global attributes in the source file and copy them.
  attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
  FOR j=0,4 DO BEGIN
    sObj->CopyGlobalAttrTo, attrNames[j], dObj
  ENDFOR
  undefine, sObj
  
  
  ; dimensions  Time, south_north, west_east
  t_dim_name = 'time'
  x_dim_name = 'south_north'
  y_dim_name = 'west_east'
  dObj->WriteDim, t_dim_name, nt
  dObj->WriteDim, x_dim_name, nx
  dObj->WriteDim, y_dim_name, ny
  
  ; Variables
  vn = 'time'
  dObj->WriteVarDef, vn, t_dim_name, DATATYPE='LONG'
  dObj->WriteVarAttr, vn, 'long_name', 'Time'
  dObj->WriteVarAttr, vn, 'units', 'days since 2001-01-01 00:00:00'
  ncdftime = LONG((time-QMS_TIME(year=2001,month=1,day=1, HOUR=0)) / (MAKE_TIME_STEP(DAY=1)).dms)
  dObj->WriteVarData, vn, ncdftime
  
  vn = 'lon'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Longitude'
  dObj->WriteVarAttr, vn, 'units', 'degrees_east'
  dObj->WriteVarData, vn, lon
  
  vn = 'lat'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Latitude'
  dObj->WriteVarAttr, vn, 'units', 'degrees_north'
  dObj->WriteVarData, vn, lat
  
  vars = ['HGT']
  for i=0, N_ELEMENTS(vars)-1 do begin
    data = wpr->getVarData(vars[i], INFO=inf)
    vn = inf.id
    dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
    dObj->WriteVarAttr, vn, 'long_name', inf.description
    dObj->WriteVarAttr, vn, 'units', inf.unit
    dObj->WriteVarData, vn, TEMPORARY(data)
  endfor
  
  vars = ['T2', 'U10', 'V10', 'SWDOWN', 'PRCP', 'PRCP_FR', 'TSK', 'PSFC', 'Q2', 'RH2', 'SWUP', 'LWDOWN', 'CLDFRA_ETA', 'LWUP']
  for i=0, N_ELEMENTS(vars)-1 do begin
    ok = wpr->hasVar(vars[i])
    if ~ ok then message, 'Var not available: ' + str_equiv(vars[i]) 
  endfor
  
  vars = ['T2', 'SWDOWN', 'PRCP', 'PRCP_FR', 'TSK', 'PSFC', 'Q2', 'RH2', 'SWUP', 'LWDOWN', 'LWUP', 'U10', 'V10']
  for i=0, N_ELEMENTS(vars)-1 do begin
    data = wpr->getVarData(vars[i], INFO=inf)
    vn = inf.id
    dObj->WriteVarDef, vn, [x_dim_name,y_dim_name,t_dim_name], DATATYPE='FLOAT'
    dObj->WriteVarAttr, vn, 'long_name', inf.description
    dObj->WriteVarAttr, vn, 'units', inf.unit
    dObj->WriteVarData, vn, data
    dObj->Sync
    undefine, data
  endfor
  
  for y=2001, 2011 do begin
    c = TOTAL(wpr->getVarData('CLDFRA_ETA'), 3) < 1.
    if N_ELEMENTS(cld) eq 0 then cld = c else cld = [[[cld]],[[c]]]
  endfor
  
  cld = reform(cld, [nx * ny, nt])
  data = cld * 0.  
  radius = 55000
  
  npix = LONARR(nx, ny)
  
  phi = Findgen(360) * (!PI * 2 / 360.)
  phi = [phi, phi(0)]  
  
  for i = 0L, N_ELEMENTS(lon)-1 do begin 
     ; at my pooint
     x = xx[i]
     y = yy[i]
     _cld = cld
     pol = [transpose(cos(phi)*radius + x), transpose(sin(phi)*radius + y)]
     ok = tpl->set_ROI(POLYGON=pol, SRC=wrf_proj)
     tpl->get_ROI, MASK=mask
     _cld[where(mask eq 0.),*] = 0.
     data[i, *] = TOTAL(_cld,1) / TOTAL(mask)     
     npix[i] = TOTAL(mask)
  endfor
  
  data = reform(data, [nx, ny, nt])

  vn = 'sfc_cldfra'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name,t_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Skyview cloudfraction'
  dObj->WriteVarAttr, vn, 'units', '-'
  dObj->WriteVarData, vn, data
  
  undefine, dObj
  
end

pro make_sophieprods

  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/mowglie/disk/Data/WRF/products/WET/d10km/d')
  
  shapeFile = '/home/mowglie/disk/Data/GIS/shapes/sophie_ezg/catchments/Dudh_Kosi/Dudh_Kosi_basin.shp'
  destFile = '/home/mowglie/disk/Data/WRF/HYDRO_SOPHIE/wet10km_DudhKosi_Hydro_daily.nc'
  regional_hydro_product, wpr, shapeFile, destFile
  shapeFile = '/home/mowglie/disk/Data/GIS/shapes/sophie_ezg/catchments/lake_Nam_Co/bsn_namc.shp'
  destFile = '/home/mowglie/disk/Data/WRF/HYDRO_SOPHIE/wet10km_NamCo_Hydro_daily.nc'
  regional_hydro_product, wpr, shapeFile, destFile
  shapeFile = '/home/mowglie/disk/Data/GIS/shapes/sophie_ezg/catchments/lake_Manasarovar/bsn_both.shp'
  destFile = '/home/mowglie/disk/Data/WRF/HYDRO_SOPHIE/wet10km_NamCo_Hydro_daily.nc'
  regional_hydro_product, wpr, shapeFile, destFile
  
  undefine, wpr
  
end