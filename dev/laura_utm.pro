pro laura_reproj_temp
  
  dir = '/home/curio/LAURA/'
  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wpr->getTime, time, nt
  h = wpr->getVarData('hgt')
  
  grad = wpr->getVarData('grad_t2_ks3')
  gradsig = wpr->getVarData('gradsig_t2_ks3')
  
  p = where(gradsig lt 0.8)
  undefine, gradsig
  
  grad[p] = -0.0098 ; where gradient is not significant, put the dry adiab
  grad = grad > (-0.0098) ; higher than dry adiab is not allowed
  
  t2 = wpr->getVarData('t2c')

  ; Proj transformation
  h = grid->map_gridded_data(h, wpr)
  grad = grid->map_gridded_data(grad, wpr)
  t2 = grid->map_gridded_data(t2, wpr)
  
  ; Put NaN values outside the basin
  p = where(topo eq MIN(topo))  
  t2 = reform(t2, c.nx*c.ny, nt)
  t2[p, *] = !VALUES.F_NAN
  t2 = reform(t2, c.nx, c.ny, nt)
  
  w_LoadCT, 34, TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts, NEUTRAL_COLOR='light grey')   
  
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  for i = pt0[0], nt-1 do begin
  
    _data = t2[*,*,i] + (topo - h) * grad[*,*,i]
    
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = dir + 'data_temp/wrf_temp_' + ts + '.dat' 
    pfile = dir + 'plots/plot_wrf_temp_' + ts + '.png' 
;    ok = map->set_data(_data)
;    w_standard_2d_plot, map, TITLE='Corr Temp ' + ts, BAR_TITLE='Temp', BAR_FORMAT='(F7.2)', PNG=pfile
    
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT='(F7.3)')
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
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
  
  undefine, wpr
  
end


pro laura_reproj_prcp
  
  dir = '/home/curio/LAURA/'
  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wpr->getTime, time, nt
  
  prcp = wpr->getVarData('prcp')

  ; Proj transformation
;  h = grid->map_gridded_data(h, wpr)
;  grad = grid->map_gridded_data(grad, wpr)
  prcp = grid->map_gridded_data(prcp, wpr, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(topo eq MIN(topo))  
  prcp = reform(prcp, c.nx*c.ny, nt)
  prcp[p, *] = !VALUES.F_NAN
  prcp = reform(prcp, c.nx, c.ny, nt)
  
  w_LoadCT, 34, TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts, NEUTRAL_COLOR='light grey')   
  
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  for i = pt0[0], nt-1 do begin
  
;    _data = t2[*,*,i] + (topo - h) * grad[*,*,i]
    _data = prcp[*,*,i]
    
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = dir + 'data_prcp/wrf_prcp_' + ts + '.dat' 
    pfile = dir + 'plots/plot_wrf_prcp_' + ts + '.png' 
;    ok = map->set_data(_data)
;    w_standard_2d_plot, map, TITLE='Corr Temp ' + ts, BAR_TITLE='Temp', BAR_FORMAT='(F7.2)', PNG=pfile
    
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT='(F7.3)')
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
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
  
  undefine, wpr
  
end


pro laura_reproj_ws10
  
  dir = '/home/curio/LAURA/'
  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wpr->getTime, time, nt
  
  ws10 = wpr->getVarData('ws10')

  ; Proj transformation
;  h = grid->map_gridded_data(h, wpr)
;  grad = grid->map_gridded_data(grad, wpr)
  ws10 = grid->map_gridded_data(ws10, wpr, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(topo eq MIN(topo))  
  ws10 = reform(ws10, c.nx*c.ny, nt)
  ws10[p, *] = !VALUES.F_NAN
  ws10 = reform(ws10, c.nx, c.ny, nt)
  
  w_LoadCT, 34, TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts, NEUTRAL_COLOR='light grey')   
  
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  for i = pt0[0], nt-1 do begin
  
;    _data = t2[*,*,i] + (topo - h) * grad[*,*,i]
    _data = ws10[*,*,i]
    
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = dir + 'data_ws10/wrf_ws10_' + ts + '.dat' 
    pfile = dir + 'plots/plot_wrf_ws10_' + ts + '.png' 
;    ok = map->set_data(_data)
;    w_standard_2d_plot, map, TITLE='Corr Temp ' + ts, BAR_TITLE='Temp', BAR_FORMAT='(F7.2)', PNG=pfile
    
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT='(F7.3)')
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
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
  
  undefine, wpr
  
end


pro laura_reproj_netrad
  
  dir = '/home/curio/LAURA/'
  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wpr->getTime, time, nt
  
  netrad = wpr->getVarData('netrad')

  ; Proj transformation
;  h = grid->map_gridded_data(h, wpr)
;  grad = grid->map_gridded_data(grad, wpr)
  netrad = grid->map_gridded_data(netrad, wpr, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(topo eq MIN(topo))  
  netrad = reform(netrad, c.nx*c.ny, nt)
  netrad[p, *] = !VALUES.F_NAN
  netrad = reform(netrad, c.nx, c.ny, nt)
  
  w_LoadCT, 34, TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts, NEUTRAL_COLOR='light grey')   
  
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  for i = pt0[0], nt-1 do begin
  
;    _data = t2[*,*,i] + (topo - h) * grad[*,*,i]
    _data = netrad[*,*,i]
    
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = dir + 'data_netrad/wrf_netrad_' + ts + '.dat' 
    pfile = dir + 'plots/plot_wrf_netrad_' + ts + '.png' 
;    ok = map->set_data(_data)
;    w_standard_2d_plot, map, TITLE='Corr Temp ' + ts, BAR_TITLE='Temp', BAR_FORMAT='(F7.2)', PNG=pfile
    
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT='(F7.3)')
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
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
  
  undefine, wpr
  
end


pro laura_reproj_q2
  
  dir = '/home/curio/LAURA/'
  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wpr->getTime, time, nt
  
  q2 = wpr->getVarData('q2')

  ; Proj transformation
;  h = grid->map_gridded_data(h, wpr)
;  grad = grid->map_gridded_data(grad, wpr)
  q2 = grid->map_gridded_data(q2, wpr, /BILINEAR)
  
  ; Put NaN values outside the basin
  p = where(topo eq MIN(topo))  
  q2 = reform(q2, c.nx*c.ny, nt)
  q2[p, *] = !VALUES.F_NAN
  q2 = reform(q2, c.nx, c.ny, nt)
  
  w_LoadCT, 34, TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts, NEUTRAL_COLOR='light grey')   
  
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  for i = pt0[0], nt-1 do begin
  
;    _data = t2[*,*,i] + (topo - h) * grad[*,*,i]
    _data = q2[*,*,i]
    
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = dir + 'data_q2/wrf_q2_' + ts + '.dat' 
    pfile = dir + 'plots/plot_wrf_q2_' + ts + '.png' 
;    ok = map->set_data(_data)
;    w_standard_2d_plot, map, TITLE='Corr Temp ' + ts, BAR_TITLE='Temp', BAR_FORMAT='(F7.2)', PNG=pfile
    
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT='(F8.5)')
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
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
  
  undefine, wpr
  
end


pro laura_reproj_psfc
  
  dir = '/home/curio/LAURA/'
  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wpr->getTime, time, nt
  h = wpr->getVarData('hgt')
  
  grad = wpr->getVarData('grad_psfc_ks3')
  gradsig = wpr->getVarData('gradsig_psfc_ks3')
  
  p = where(gradsig lt 0.8)
  undefine, gradsig
  
  grad[p] = -0.0098 ; where gradient is not significant, put the dry adiab
  grad = grad > (-0.0098) ; higher than dry adiab is not allowed
  
  psfc = wpr->getVarData('psfc')

  ; Proj transformation
  h = grid->map_gridded_data(h, wpr)
  grad = grid->map_gridded_data(grad, wpr)
  psfc = grid->map_gridded_data(psfc, wpr)
  
  ; Put NaN values outside the basin
  p = where(topo eq MIN(topo))  
  psfc = reform(psfc, c.nx*c.ny, nt)
  psfc[p, *] = !VALUES.F_NAN
  psfc = reform(psfc, c.nx, c.ny, nt)
  
  w_LoadCT, 34, TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts, NEUTRAL_COLOR='light grey')   
  
  pt0 = where(time eq QMS_TIME(YEAR=2000, month=09, day=1))
  
  for i = pt0[0], nt-1 do begin
  
    _data = psfc[*,*,i] + (topo - h) * grad[*,*,i]
    
    ts = TIME_to_STR(time[i], MASK='YYYY_MM_DD')
    file = dir + 'data_psfc/wrf_psfc_' + ts + '.dat' 
    pfile = dir + 'plots/plot_wrf_psfc_' + ts + '.png' 
;    ok = map->set_data(_data)
;    w_standard_2d_plot, map, TITLE='Corr Temp ' + ts, BAR_TITLE='Temp', BAR_FORMAT='(F7.2)', PNG=pfile
    
    p = where(~ FINITE(_data), cnt)
    _data = STRING(_data, FORMAT='(F7.3)')
    if cnt ne 0 then _data[p] = '-9999'
    _data = reform(_data, c.nx, c.ny)
    
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
  
  undefine, wpr
  
end


pro laura_reproj

  dir = '/home/curio/LAURA/'

  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/curio/DATA/CASA/products/d02km/d')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wrf_data = wpr->getVarData('hgt')
  
  
  
  w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts+1, CMAX=ts-1, NEUTRAL_COLOR='light grey')
 
  reproj_data = grid->map_gridded_data(wrf_data, wpr)
  p = where(topo eq MIN(topo))  
  reproj_data[p] = !VALUES.F_NAN
  ok = map->set_data(reproj_data)
  w_standard_2d_plot, map, TITLE='WRF Topo', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG=dir+'plot_wrftopo.png'
  
  reproj_data = grid->map_gridded_data(wrf_data, wpr, /BILINEAR)
  p = where(topo eq MIN(topo))  
  reproj_data[p] = !VALUES.F_NAN
  ok = map->set_data(reproj_data)
  w_standard_2d_plot, map, TITLE='WRF Topo BILINEAR', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG=dir+'plot_wrfbili.png'
  
  reproj_data = grid->map_gridded_data(wrf_data, wpr, /CUBIC)
  p = where(topo eq MIN(topo))  
  reproj_data[p] = !VALUES.F_NAN
  ok = map->set_data(reproj_data)
  w_standard_2d_plot, map, TITLE='WRF Topo CUBIC', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG=dir+'plot_wrfcubi.png'
    
  undefine, wpr
  
end


pro laura_utm, grid, map, topo, DO_PLOT=do_plot

dir = '/home/curio/LAURA/'

  GIS_make_proj, ret, utm, PARAM='2, 29, WGS-84'
  
  x0 = 698017.931
  y0 = 3769295.219
  dx = 250
  dy = 250
  nx = 525
  ny = 539
  
  undefine, grid, map
  grid = OBJ_NEW('w_Grid2D', PROJ=utm, X0=x0, Y0=y0, NX=nx, NY=ny, DX=dx, DY=dy)
  
  map = OBJ_NEW('w_Map', grid, YSIZE=700, /NO_COUNTRIES)
  ok = map->set_map_params(INTERVAL=0.5)
  ok = map->set_shape_file(SHPFILE='/home/curio/LAURA/Bouregreg_shp/rastert_watersh3.shp', SHP_SRC=utm)
  
  topo = intarr(nx, ny)
  openr, lun, '/home/curio/LAURA/BouregregDEM.grd', /GET
  readu, lun, topo
  free_lun, lun
  topo = ROTATE(LONG(topo), 7)

  if KEYWORD_SET(DO_PLOT) then begin
    w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
    ok = map->set_plot_params(N_LEVELS=ts+1, CMAX=ts-1, NEUTRAL_COLOR='light grey')
    ok = map->set_data(topo, grid, MISSING=MIN(TOPO))
    w_standard_2d_plot, map, TITLE='Bouregreg Topo', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG=dir+'plot_bouregregtopo.png'
  endif
  
end