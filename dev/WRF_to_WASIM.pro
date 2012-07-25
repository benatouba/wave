pro WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo, DO_PLOT=do_plot
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Modify your paths here
  wrf_data_dir = '/home/mowglie/DATA/UAC/d02km/d'
  output_dir = '/home/mowglie/DATA/WASIM_UAC/ascii_data'
  dem_path = '/home/mowglie/DATA/WASIM_UAC/GIS/BouregregDEM.grd'
  shape_path = '/home/mowglie/DATA/WASIM_UAC/GIS/Bouregreg_shp/rastert_watersh3.shp'
  
  ; Nothing to modify below this line
  undefine, wrf_obj
  wrf_obj = OBJ_NEW('w_WPR', DIRECTORY=wrf_data_dir)
  
  output_dir = utils_clean_path(OUTPUT_DIR, /MARK_DIRECTORY)
  
  ; Map
  undefine, map
  GIS_make_proj, ret, utm, PARAM='2, 29, WGS-84'
  
  dem_obj = OBJ_NEW('w_DEM', FILE=dem_path)
      
  map = OBJ_NEW('w_Map', dem_obj, YSIZE=700, /NO_COUNTRIES)
  

  ok = map->set_map_params(INTERVAL=0.5)
  ok = map->set_shape_file(SHPFILE=shape_path, SHP_SRC=utm)
  
  dem_topo = dem_obj->get_Z()
  
  wrf_topo = wrf_obj->getVarData('hgt')
  wrf_topo = dem_obj->map_gridded_data(wrf_topo, wrf_obj)
  pmiss = where(dem_topo eq MIN(dem_topo))
  wrf_topo[pmiss] = !VALUES.F_NAN
  
  plot_dir = output_dir + 'plots/'
  if ~ FILE_TEST(plot_dir, /DIRECTORY) then FILE_MKDIR, plot_dir

  if KEYWORD_SET(DO_PLOT) then begin
    w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
    ok = map->set_plot_params(N_LEVELS=ts+1, CMAX=ts-1, NEUTRAL_COLOR='light grey')
    ok = map->set_data(dem_topo, dem_obj, MISSING=MIN(dem_topo))
    w_standard_2d_plot, map, TITLE='Bouregreg Topo - SRTM 250m', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', /PIXMAP, PNG=plot_dir+'plot_bouregregtopo_vasim.png'
    ok = map->set_data(wrf_topo, dem_obj)
    w_standard_2d_plot, map, TITLE='Bouregreg Topo - WRF  02km', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', /PIXMAP, PNG=plot_dir+'plot_bouregregtopo_WRF.png'
    
  endif
  
 
  
end

pro WRF_to_WASIM_t2

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo
  
  var_id = 't2c'
  str_format = '(F7.3)'
  
  sav_dir = output_dir + var_id + '_sav'
  output_dir = output_dir + var_id
  if ~ FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
  if ~ FILE_TEST(sav_dir, /DIRECTORY) then FILE_MKDIR, sav_dir
    
  dem_obj->getProperty, TNT_C=c
  
  wrf_obj->getTime, time, nt
    
  grad = wrf_obj->getVarData('grad_t2_ks3')
  gradsig = wrf_obj->getVarData('gradsig_t2_ks3')  
  
  p = where(gradsig lt 0.8)
  undefine, gradsig
  grad[p] = -0.0098 ; where gradient is not significant, put the dry adiab
  grad = grad > (-0.0098) ; higher than dry adiab is not allowed
  
  data = wrf_obj->getVarData(var_id, INFO=info)
  Print, "Unit of " + var_id + ': ' + info.unit

  ; Proj transformation WRF->DEM
  grad = dem_obj->map_gridded_data(grad, wrf_obj)
  data = dem_obj->map_gridded_data(data, wrf_obj)
  
  ; Put NaN values outside the basin
  p = where(dem_topo eq MIN(dem_topo))  
  data = reform(TEMPORARY(data), c.nx*c.ny, nt)
  data[p, *] = !VALUES.F_NAN
  data = reform(TEMPORARY(data), c.nx, c.ny, nt)
    
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  ; For loop on very single day
  for i = pt0[0], nt-1 do begin
  
    ; Gradient correction
    _data = data[*,*,i] + (dem_topo - wrf_topo) * grad[*,*,i]
   
    ; Output file path
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = output_dir + '/wrf_'+var_id+'_' + ts + '.dat' 
    sav_file = sav_dir + '/wrf_'+var_id+'_' + ts + '.sav' ; For psfc
    save, _data, FILENAME=sav_file
    
     ; Rotate data for VASIM 
    _data = rotate(_data, 7)    
    
    ; Convert data to string
    ; Missing string are writen as '-9999'
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT=str_format)
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
    ; Open and write file
    OPENW, lun, file, /GET_LUN
    text = 'ncols        525'
    printf, lun, text
    text = 'nrows        539'
    printf, lun, text
    text = 'xllcorner    698017'
    printf, lun, text
    text = 'yllcorner    3634795'
    printf, lun, text
    text = 'cellsize     250'
    printf, lun, text
    text = 'nodata_value -9999'
    printf, lun, text    
    for l=0, c.ny-1 do begin
      line = STRJOIN(_data[*, l], STRING(9B))
      printf, lun, line
    endfor        
    FREE_LUN, lun        
        
  endfor
  
  ; Clean objects
  undefine, dem_obj, wrf_obj, map
  
end

pro WRF_to_WASIM_psfc

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo
  
  var_id = 'psfc'
  str_format = '(F8.1)'
  
  savtemp_dir = output_dir + 't2c_sav'
  sav_dir = output_dir + var_id +'_sav'
  output_dir = output_dir + var_id
  if ~ FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
  if ~ FILE_TEST(sav_dir, /DIRECTORY) then FILE_MKDIR, sav_dir
    
  dem_obj->getProperty, TNT_C=c  
  wrf_obj->getTime, time, nt

  data = wrf_obj->getVarData(var_id, INFO=info)
  Print, "Unit of " + var_id + ': ' + info.unit

  ; Proj transformation WRF->DEM
  data = dem_obj->map_gridded_data(TEMPORARY(data), wrf_obj)
  
  ; Put NaN values outside the basin
  p = where(dem_topo eq MIN(dem_topo))  
  data = reform(TEMPORARY(data), c.nx*c.ny, nt)
  data[p, *] = !VALUES.F_NAN
  data = reform(TEMPORARY(data), c.nx, c.ny, nt)
    
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  ; For loop on very single day
  for i = pt0[0], nt-1 do begin
  
    ; Output file path
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = output_dir + '/wrf_'+var_id+'_' + ts + '.dat' 
    savtemp_file = savtemp_dir + '/wrf_t2c_' + ts + '.sav' ; For psfc
    sav_file = sav_dir + '/wrf_'+var_id+'_' + ts + '.sav'  ; For later
  
    ; temp
    restore, FILENAME=savtemp_file
    _t2 = _data + 273.15
    
    ; Gradient correction for psfc    
    _data = data[*,*,i] * exp(- 0.0289644 * 9.80665 * (dem_topo - wrf_topo) / (8.31447 * _t2)) 
    save, _data, FILENAME=sav_file
        
    ; Rotate data for VASIM 
    _data = rotate(_data, 7)
            
    ; Convert data to string
    ; Missing string are writen as '-9999'
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT=str_format)
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
    ; Open and write file
    OPENW, lun, file, /GET_LUN
    text = 'ncols        525'
    printf, lun, text
    text = 'nrows        539'
    printf, lun, text
    text = 'xllcorner    698017'
    printf, lun, text
    text = 'yllcorner    3634795'
    printf, lun, text
    text = 'cellsize     250'
    printf, lun, text
    text = 'nodata_value -9999'
    printf, lun, text    
    for l=0, c.ny-1 do begin
      line = STRJOIN(_data[*, l], STRING(9B))
      printf, lun, line
    endfor        
    FREE_LUN, lun        
        
  endfor
  
  ; Clean objects
  undefine, dem_obj, wrf_obj, map
  
end

pro WRF_to_WASIM_rh

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo
  
  str_format = '(F6.2)'
  
  var_id = 'rh2'
  savpsfc_dir = output_dir + 'psfc_sav'
  savtemp_dir = output_dir + 't2c_sav'
  output_dir = output_dir + var_id
  if ~ FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
    
  dem_obj->getProperty, TNT_C=c  
  wrf_obj->getTime, time, nt

  data = wrf_obj->getVarData('q2', INFO=info)


  ; Proj transformation WRF->DEM
  data = dem_obj->map_gridded_data(TEMPORARY(data), wrf_obj, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(dem_topo eq MIN(dem_topo))  
  data = reform(TEMPORARY(data), c.nx*c.ny, nt)
  data[p, *] = !VALUES.F_NAN
  data = reform(TEMPORARY(data), c.nx, c.ny, nt)
    
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
    
  ; For loop on very single day
  for i = pt0[0], nt-1 do begin
  
    ; Output file path
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = output_dir + '/wrf_'+var_id+'_' + ts + '.dat' 
    savtemp_file = savtemp_dir + '/wrf_t2c_' + ts + '.sav' 
    savpsfc_file = savpsfc_dir + '/wrf_psfc_' + ts + '.sav' 
      
    ; temp
    restore, FILENAME=savtemp_file
    _t2 = _data + 273.15
    ; psfc
    restore, FILENAME=savpsfc_file
    _psfc = _data    
    
    ; Compute RH    
    _data = utils_wrf_rh(data[*,*,i], _psfc, _t2)
        
    ; Rotate data for VASIM 
    _data = rotate(_data, 7)
            
    ; Convert data to string
    ; Missing string are writen as '-9999'
    _data = STRING(_data, FORMAT=str_format)
    _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
    ; Open and write file
    OPENW, lun, file, /GET_LUN
    text = 'ncols        525'
    printf, lun, text
    text = 'nrows        539'
    printf, lun, text
    text = 'xllcorner    698017'
    printf, lun, text
    text = 'yllcorner    3634795'
    printf, lun, text
    text = 'cellsize     250'
    printf, lun, text
    text = 'nodata_value -9999'
    printf, lun, text    
    for l=0, c.ny-1 do begin
      line = STRJOIN(_data[*, l], STRING(9B))
      printf, lun, line
    endfor        
    FREE_LUN, lun        
        
  endfor
  
  ; Clean objects
  undefine, dem_obj, wrf_obj, map
  
end

pro WRF_to_WASIM_prcp

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo
  
  str_format = '(F7.2)'
  
  var_id = 'prcp'
  output_dir = output_dir + var_id
  if ~ FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
    
  dem_obj->getProperty, TNT_C=c  
  wrf_obj->getTime, time, nt

  data = wrf_obj->getVarData(var_id, INFO=info)

  ; Proj transformation WRF->DEM
  data = dem_obj->map_gridded_data(TEMPORARY(data), wrf_obj, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(dem_topo eq MIN(dem_topo))  
  data = reform(TEMPORARY(data), c.nx*c.ny, nt)
  data[p, *] = !VALUES.F_NAN
  data = reform(TEMPORARY(data), c.nx, c.ny, nt)
    
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  ; For loop on very single day
  for i = pt0[0], nt-1 do begin
  
    ; Output file path
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = output_dir + '/wrf_'+var_id+'_' + ts + '.dat' 
    
    _data = data[*,*,i]
    
    ; Rotate data for VASIM 
    _data = rotate(_data, 7)
            
    ; Convert data to string
    ; Missing string are writen as '-9999'
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT=str_format)
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
    ; Open and write file
    OPENW, lun, file, /GET_LUN
    text = 'ncols        525'
    printf, lun, text
    text = 'nrows        539'
    printf, lun, text
    text = 'xllcorner    698017'
    printf, lun, text
    text = 'yllcorner    3634795'
    printf, lun, text
    text = 'cellsize     250'
    printf, lun, text
    text = 'nodata_value -9999'
    printf, lun, text    
    for l=0, c.ny-1 do begin
      line = STRJOIN(_data[*, l], STRING(9B))
      printf, lun, line
    endfor        
    FREE_LUN, lun        
        
  endfor
  
  ; Clean objects
  undefine, dem_obj, wrf_obj, map
  
end

pro WRF_to_WASIM_ws10

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo
  
  str_format = '(F8.2)'
  
  var_id = 'ws10'
  output_dir = output_dir + var_id
  if ~ FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
    
  dem_obj->getProperty, TNT_C=c  
  wrf_obj->getTime, time, nt

  data = wrf_obj->getVarData(var_id, INFO=info)

  ; Proj transformation WRF->DEM
  data = dem_obj->map_gridded_data(TEMPORARY(data), wrf_obj, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(dem_topo eq MIN(dem_topo))  
  data = reform(TEMPORARY(data), c.nx*c.ny, nt)
  data[p, *] = !VALUES.F_NAN
  data = reform(TEMPORARY(data), c.nx, c.ny, nt)
    
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  ; For loop on very single day
  for i = pt0[0], nt-1 do begin
  
    ; Output file path
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = output_dir + '/wrf_'+var_id+'_' + ts + '.dat' 
    
    _data = data[*,*,i]
    
    ; Rotate data for VASIM 
    _data = rotate(_data, 7)
            
    ; Convert data to string
    ; Missing string are writen as '-9999'
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT=str_format)
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
    ; Open and write file
    OPENW, lun, file, /GET_LUN
    text = 'ncols        525'
    printf, lun, text
    text = 'nrows        539'
    printf, lun, text
    text = 'xllcorner    698017'
    printf, lun, text
    text = 'yllcorner    3634795'
    printf, lun, text
    text = 'cellsize     250'
    printf, lun, text
    text = 'nodata_value -9999'
    printf, lun, text    
    for l=0, c.ny-1 do begin
      line = STRJOIN(_data[*, l], STRING(9B))
      printf, lun, line
    endfor        
    FREE_LUN, lun        
        
  endfor
  
  ; Clean objects
  undefine, dem_obj, wrf_obj, map
  
end

pro WRF_to_WASIM_rad

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  WRF_to_WASIM_init, wrf_obj, map, output_dir, dem_obj, dem_topo, wrf_topo
  
  str_format = '(F8.2)'
  
  var_id = 'rad'
  output_dir = output_dir + var_id
  if ~ FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
    
  dem_obj->getProperty, TNT_C=c  
  wrf_obj->getTime, time, nt

  data = wrf_obj->getVarData('swdown', INFO=info)

  ; Proj transformation WRF->DEM
  data = dem_obj->map_gridded_data(TEMPORARY(data), wrf_obj, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(dem_topo eq MIN(dem_topo))  
  data = reform(TEMPORARY(data), c.nx*c.ny, nt)
  data[p, *] = !VALUES.F_NAN
  data = reform(TEMPORARY(data), c.nx, c.ny, nt)
    
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  ; For loop on very single day
  for i = pt0[0], nt-1 do begin
  
    ; Output file path
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = output_dir + '/wrf_'+var_id+'_' + ts + '.dat' 
    
    _data = data[*,*,i] * 24 ; W m-2 to Wh m-2 
        
    ; Rotate data for VASIM 
    _data = rotate(_data, 7)
            
    ; Convert data to string
    ; Missing string are writen as '-9999'
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT=str_format)
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
    ; Open and write file
    OPENW, lun, file, /GET_LUN
    text = 'ncols        525'
    printf, lun, text
    text = 'nrows        539'
    printf, lun, text
    text = 'xllcorner    698017'
    printf, lun, text
    text = 'yllcorner    3634795'
    printf, lun, text
    text = 'cellsize     250'
    printf, lun, text
    text = 'nodata_value -9999'
    printf, lun, text    
    for l=0, c.ny-1 do begin
      line = STRJOIN(_data[*, l], STRING(9B))
      printf, lun, line
    endfor        
    FREE_LUN, lun        
        
  endfor
  
  ; Clean objects
  undefine, dem_obj, wrf_obj, map
  
end