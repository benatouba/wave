pro lnl_map

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common ADMIN_LNL  
  
  
  ; shape of basin and lake
  shpFbasin = 'C:\Users\Hinners\Documents\GIS\shapes\sophie_ezg\catchments\lake_Nam_Co\bsn_namc.shp'
  shpFlake = 'C:\Users\Hinners\Documents\GIS\shapes\sophie_ezg\catchments\lake_Nam_Co\flat_lake.shp'
  
  undefine, map
  
  ;  check map data and plot map 
  map = OBJ_NEW('w_Map', wrf_std, YSIZE=700)
  w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
  ok = map->set_plot_params(N_LEVELS=127, CMAX=ts-1)  
  ok = map->set_map_params(INTERVAL=0.5)
  ok = map->set_topography(GRDFILE='C:\Users\Hinners\Documents\GIS\Topo\NAMCO\Namco.grd', /USE_GRID, Z=z)
  ok = map->set_shape_file(SHPFILE=shpFbasin, THICK=4)  
  ok = map->set_shape_file(SHPFILE=shpFlake, COLOR='PBG4', /FILL)
  ok = map->set_shape_file(SHPFILE=shpFlake, THICK=2)
  z[where(z lt 3000)] = z[where(z lt 3000)+1]
  ok = map->set_data(z)    
  w_standard_2d_plot, map, TITLE='Nam Co Basin - SRTM topography', BAR_TITLE='Alt. (m)', BAR_FORMAT='(I4)'
  
  ok = map->set_shape_file()
  ok = map->set_shape_file(SHPFILE=shpFbasin, THICK=4)  
  ok = map->set_shape_file(SHPFILE=shpFlake, THICK=2)  
  undefine, w
  
end

;+
; :Description:
;    Get an atmospheric data
;
; :Params:
;    wrf: in
;         wrf object
;    theta: out
;    p: out
;    t: out
;    z: out
;    rh: out
;    qv: out
;    w: out
;    
; :Author: Hinners
;-
pro lnl_pt_diagramms_get_Vars, wrf, theta, p, t, z, rh, qv, w, td, time 

  years = [2002,2008,2009]
  hgt = wrf->getvarData("hgt")

  p = wrf->getvarData("pressure_eta", time, years=years)
  z = wrf->getvarData("geopotential_eta", years=years)/9.81
  theta = wrf->getvarData("theta_eta", years=years)
  t = utils_wrf_tk(P*100.,theta) - 273.15  
  theta = theta - 273.15  
  qvapor = wrf->getvarData('qvapor_eta', years=years) > 0.
  w = wrf->getvarData('w_eta', years=years) > 0.
  rh = utils_wrf_rh(QVAPOR, P*100., t + 273.15)
  td = utils_wrf_td(P*100.,QVAPOR)
    
end

pro lake_nolahe, RESET=reset

  common ADMIN_LNL, wrf_std, wrf_nl, map, basin_mask, lake_mask
  
  if KEYWORD_SET(RESET) then undefine, wrf_std, wrf_nl, map, basin_mask, lake_mask
  
  if N_ELEMENTS(wrf_std) eq 0 then wrf_std = OBJ_NEW('w_WPR', DIRECTORY='C:\Users\Hinners\Documents\WRF_DATA\LAKE\d10km\m')
  if N_ELEMENTS(wrf_nl) eq 0 then wrf_nl = OBJ_NEW('w_WPR', DIRECTORY='C:\Users\Hinners\Documents\WRF_DATA\NOLAKE\d10km\m')
  
  ok = wrf_std->defineSubset()
  ok = wrf_nl->defineSubset()
  
  GIS_make_datum, ret, src, NAME='WGS-84'  
  shpFlake = 'C:\Users\Hinners\Documents\GIS\shapes\sophie_ezg\catchments\lake_Nam_Co\flat_lake.shp'
  ok = wrf_std->defineSubset(SHAPE=shpFlake, MARGIN=10, SRC=src)
  ok = wrf_nl->defineSubset(SHAPE=shpFlake, MARGIN=10, SRC=src)
  
  ok = wrf_nl->set_ROI(SHAPE=shpFlake, SRC=src)
  wrf_nl->get_ROI, MASK=lake_mask
  ok = wrf_nl->set_ROI()
  
  shpFbasin = 'C:\Users\Hinners\Documents\GIS\shapes\sophie_ezg\catchments\lake_Nam_Co\bsn_namc.shp'
  ok = wrf_nl->set_ROI(SHAPE=shpFbasin, SRC=src)
  wrf_nl->get_ROI, MASK=basin_mask
  ok = wrf_nl->set_ROI()
  if N_ELEMENTS(map) eq 0 then lnl_map
  
end



pro lnl_skew_diags, ipix_l, jpix_l ,ipix_nl, jpix_nl, month
  
  common ADMIN_LNL
 
 ;investigated pixels over lake and over land
  il = ipix_l
  jl = jpix_l
  in = ipix_nl
  jn = jpix_nl  
   m = month ; investigated month
   
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  

  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  t_lake = REFORM(t[il, jl, *, m])
  p_lake = REFORM(p[il, jl, *, m])
  td_lake = REFORM(td[il, jl, *, m])
  t_nlake = REFORM(t[in, jn, *, m])
  p_nlake = REFORM(p[in, jn, *, m])
  td_nlake = REFORM(td[in, jn, *, m])
  
  skewt_logp_diagram, t_lake, p_lake, DEWPOINT=td_lake, ANGLE=45, TITLE='Profile WRF_STD over lake ' + time_str
  skewt_logp_diagram, t_nlake, p_nlake, DEWPOINT=td_nlake, ANGLE=45, TITLE='Profile WRF_STD over land ' + time_str
  
  
end

;+
; :Description:
;    This procedure creates a stability diagram using the Brunt-Väisälä frequency, for a given pixel and a specific month
;
; :Params:
;    ipix = pixel number in x direction
;    jpix = pixel number in y direction
;    month
;
; :Keywords:
;    STD_PNG = in, optional, default =0,
;              Set this keyword to save the igure as a standard png in the output directory
;    OUTPUT_DIR = in optional, default = 0,
;                 Set this keyword to determine an output directory.
;                 If this keyword is not set although the std_png keyword is set, a window opens to choose an output directory
;
; :Author: JaH 2012
;-
pro stability_diag, ipix, jpix, month, STD_PNG=std_png, OUTPUT_DIR=output_dir
  
  ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  common ADMIN_LNL
  
  ;investigated pixels in area
  i = ipix
  j = jpix
  m = month ; investigated month
  
  
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  theta_l = theta[i,j,*,m]
  z_l = z[i,j,*,m]
  theta0 = interpol(theta_l, z_l, 0)
  dtheta_l = theta_l[0,0,*] - theta0
  p_l = p[i,j,*,m]

  
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  theta_nl=theta[i,j,*,m]
  z_nl=z[i,j,*,m]
  theta0 = interpol(theta_nl, z_nl, 0)
  dtheta_nl = theta_nl[0,0,*] - theta0
  p_nl = p[i,j,*,m]
  prange = [ max( [[[p_l]],[[p_nl]]] ), min( [[[p_l]],[[p_nl]]] ) ]
      
  g = 9.81  
  BV_l  = sqrt( (g * dtheta_l[0,0,*])  / (theta_l[0,0,*]  * z_l[0,0,*])  )
  BV_nl = sqrt( (g * dtheta_nl[0,0,*]) / (theta_nl[0,0,*] * z_nl[0,0,*]) )
  BV=[[[BV_l]],[[BV_nl]]]
  xrange=[ min(BV), max(BV)]
  
 
  cgplot, BV_l,  p_l,  color='blue', yrange=prange, xrange=xrange, $
  Title='stability diagram !C !C i='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | m='+STRING(month,FORMAT='(I2)')+'', $
  position=[0.12, 0.12, 0.9, 0.85], xtitle=ansi_value('Brunt-Väisälä-frequency [1/s]'), ytitle='pressure [hPa]', /WINDOW
  cgplot, BV_nl, p_nl, color='black', /Overplot, /WINDOW
  
  al_legend, ['Lake', 'No Lake'], color=['blue', 'black'], LineStyle=[0,0], POSITION=[(max(BV)-(max(BV)/20)),(min(prange)+(max(prange)/20))], /Window
 
  
  if N_ELEMENTS(std_png) ne 0 then begin
    FILE_MKDIR,output_dir+'StabilityDiagrams'
    pngname='StabilityDiagrams/'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif
  
  
end




