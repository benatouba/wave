pro laura_reproj

  wpr = OBJ_NEW('w_WPR', DIRECTORY='/home/mowglie/disk/Data/WRF/products/UAC/d02km/y')
  
  laura_utm, grid, map, topo
  
  grid->getProperty, TNT_C=c
  
  wrf_data = wpr->getVarData('hgt')
  
  
  
  w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts+1, CMAX=ts-1, NEUTRAL_COLOR='light grey')
 
  reproj_data = grid->map_gridded_data(wrf_data, wpr)
  ok = map->set_data(reproj_data)
  w_standard_2d_plot, map, TITLE='WRF Topo', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG='wrftopo.png'
  
  reproj_data = grid->map_gridded_data(wrf_data, wpr, /BILINEAR)
  ok = map->set_data(reproj_data)
  w_standard_2d_plot, map, TITLE='WRF Topo BILINEAR', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG='wrfbili.png'
  reproj_data = grid->map_gridded_data(wrf_data, wpr, /CUBIC)
  ok = map->set_data(reproj_data)
  w_standard_2d_plot, map, TITLE='WRF Topo CUBIC', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG='wrfcubi.png'
  
  
  w_QuickPlot, reproj_data, COLORTABLE=34
  
  undefine, wpr
  
end


pro laura_utm, grid, map, topo, DO_PLOT=do_plot

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
  ok = map->set_shape_file(SHPFILE='/home/mowglie/tmp/laura/rastert_watersh3.shp', SHP_SRC=utm)
  
  topo = intarr(nx, ny)
  openr, lun, '/home/mowglie/tmp/laura/laura_dem.grd', /GET
  readu, lun, topo
  free_lun, lun
  topo = ROTATE(LONG(topo), 7)

  if KEYWORD_SET(DO_PLOT) then begin
  w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=ts+1, CMAX=ts-1, NEUTRAL_COLOR='light grey')
  ok = map->set_data(topo, grid, MISSING=MIN(TOPO))
  w_standard_2d_plot, map, TITLE='Bouregreg Topo', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)', PNG='bouregregtopo.png'
  endif
  
end