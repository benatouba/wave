;+
; :Description:
;    This procedure creates a map showing the lake location and the basin shape
;
;-
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
;    wrf: in, required, wrf_std or wrf_nl
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
  qv=qvapor
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



;+
; :Description:
;    This procedure creates skewT-log(p)-diagrams for one of the two models (LAKE / NO LAKE) for two different locations
;
; :Params:
;    ipix_l = in, pixel number over lake in x direction
;    jpix_l = in, pixel number over lake in y direction
;    ipix_nl = in, pixel number over land in x direction
;    jpix_nl = in pixel number over land in y direction
;    month= in
;
; :Keywords:
;    NOLAKEMODEL= in,optional, set this keyword to generate the pt-diagram for the nolake model instead of the lake model
;
; :Author: Hinners
;-
pro lnl_skew_diags, ipix_l, jpix_l ,ipix_nl, jpix_nl, month, NOLAKEMODEL=nolakemodel
  
  common ADMIN_LNL
 
 ;investigated pixels over lake and over land
  il = ipix_l
  jl = jpix_l
  in = ipix_nl
  jn = jpix_nl  
   m = month ; investigated month
  
  if N_Elements(nolakemodel) eq 0 then begin
      lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time
      modelname='WRF_STD'
  endif else begin
      lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time
      modelname ='WRF_NL'
  endelse
  

  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  t_lake = REFORM(t[il, jl, *, m])
  p_lake = REFORM(p[il, jl, *, m])
  td_lake = REFORM(td[il, jl, *, m])
  z_lake = REFORM(z[il,jl,*,m])
  t_nlake = REFORM(t[in, jn, *, m])
  p_nlake = REFORM(p[in, jn, *, m])
  td_nlake = REFORM(td[in, jn, *, m])
  z_nlake = REFORM(z[in,jn,*,m])
  
  skewt_logp_diagram, t_lake, p_lake, DEWPOINT=td_lake, HEIGHT=z_lake, ANGLE=45, TITLE='PT-Profile for '+modelname+' over lake ' + time_str
  skewt_logp_diagram, t_nlake, p_nlake, DEWPOINT=td_nlake, HEIGHT=z_nlake, ANGLE=45, TITLE='PT-Profile for '+modelname+' over land ' + time_str
  
  
end




;+
; :Description:
;    This procedure creates skewT_log(p)-diagrams (like the procedure lnl_skewT_diags),
;    but the two diagrams which are generated are for both models, the Lake and the Nolake model at the same location.
;
; :Params:
;    ipix = in, pixel number in x direction
;    jpix = in, pixel number in y direction
;    month = in
;
; :Keywords:
;    STD_PNG = in, optional, default =0,
;              Set this keyword to save the figure as a standard png in the output directory
;    OUTPUT_DIR = in optional, default = 0,
;                 Set this keyword to determine an output directory.
;                 If this keyword is not set although the std_png keyword is set, a window opens to choose an output directory
;
; :Author: JaH 2012
;-
pro lakenolake_skewT_diags, ipix, jpix , month, STD_PNG=std_png, OUTPUT_DIR=output_dir

  ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  common ADMIN_LNL
  
  ;investigated pixels in area
  i = ipix
  j = jpix
  m = month ; investigated month
  
    ; get variables of lake model
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  t_l = REFORM(t[i, j, *, m])
  p_l = REFORM(p[i,j,*,m])
  td_l= REFORM(td[i,j,*,m])   
  z_l = REFORM(z[i,j,*,m])

  ; get variables of no lake model
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  t_nl = REFORM(t[i, j, *, m])
  p_nl = REFORM(p[i,j,*,m])
  td_nl= REFORM(td[i,j,*,m])   
  z_nl = REFORM(z[i,j,*,m])
    
  skewt_logp_diagram, t_l, p_l, DEWPOINT=td_l, HEIGHT=z_l,  ANGLE=45,$
  TITLE=' PT-Profile for WRF_STD !C !Ci='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str
 
;  ; save figure
;  if N_ELEMENTS(std_png) ne 0 then begin
;    FILE_MKDIR,output_dir+'SkewT-logP-Diagrams'
;    pngname1='SkewT-logP-Diagrams/Lake_'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
;    STD_PNG=output_dir+pngname1+'.png'  
;    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  
    skewt_logp_diagram, t_nl, p_nl, DEWPOINT=td_nl,HEIGHT=z_nl, ANGLE=45, $
    TITLE=' PT-Profile for WRF_NL !C !Ci='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str 
    
;  ; save figure
;  if N_ELEMENTS(std_png) ne 0 then begin
;    FILE_MKDIR,output_dir+'SkewT-logP-Diagrams'
;    pngname2='SkewT-logP-Diagrams/NoLake_'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
;    STD_PNG=output_dir+pngname2+'.png'  
;    cgControl, CREATE_PNG=std_png, IM_RASTER=0
    
  
end


;+
; :Description:
;    This procedure creates a stability diagram using the Brunt-Väisälä frequency, for a given pixel and a specific month
;
; :Params:
;    ipix = in, pixel number in x direction
;    jpix = in, pixel number in y direction
;    month = in
;
; :Keywords:
;    STD_PNG = in, optional, default =0,
;              Set this keyword to save the figure as a standard png in the output directory
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
  
  ; get variables of lake model
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  theta_l = reform(theta[i,j,*,m])
  z_l = reform(z[i,j,*,m])
  p_l = reform(p[i,j,*,m])
  dtheta_l = fltarr(N_elements(theta_l)-1)
  dz_l = fltarr(N_elements(z_l)-1)
  for n = 0,(N_Elements(theta_l)-2) do begin
    dtheta_l[n] = theta_l[n+1] - theta_l[n]
    dz_l[n] = z_l[n+1] - z_l[n]
  endfor 

  ; get variables of no lake model
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  theta_nl=reform(theta[i,j,*,m])
  z_nl=reform(z[i,j,*,m])
  p_nl = reform(p[i,j,*,m])
  dtheta_nl = fltarr(N_elements(theta_nl)-1)
  dz_nl = fltarr(N_elements(z_nl)-1)
  for n = 0,(N_Elements(theta_nl)-2) do begin
    dtheta_nl[n] = theta_nl[n+1] - theta_nl[n]
    dz_nl[n] = z_nl[n+1] - z_nl[n]
  endfor
  
  prange = [ max( [p_l,p_nl] ), min( [p_l,p_nl] ) ]
  
  ; compute Brunt-Väisälä frequency    
  g = 9.81  
  theta_l=theta_l[1:*]
  theta_nl=theta_nl[1:*]
  p_l = p_l[1:*]
  p_nl = p_nl[1:*]
  BV_l  = sqrt( (g * dtheta_l)  / (theta_l  * dz_l)  )
  BV_nl = sqrt( (g * dtheta_nl) / (theta_nl * dz_nl) )
  BV=[BV_l,BV_nl]
  xrange=[ min(BV), max(BV)]
  
 ; plot
  cgplot, BV_l,  p_l,  color='blue', yrange=prange, xrange=xrange, $
  Title='stability diagram !C !C i='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str, $
  position=[0.12, 0.12, 0.9, 0.85], xtitle=ansi_value('Brunt-Väisälä-frequency [1/s]'), ytitle='pressure [hPa]', /WINDOW
  cgplot, BV_nl, p_nl, color='black', /Overplot, /WINDOW
  al_legend, ['Lake', 'No Lake'], color=['blue', 'black'], LineStyle=[0,0], POSITION=[0.66,0.8],/Normal, /Window
 
  ; save figure
  if N_ELEMENTS(std_png) ne 0 then begin
    FILE_MKDIR,output_dir+'StabilityDiagrams'
    pngname='StabilityDiagrams/'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif
  
  
end


;+
; :Description:
;    This procedure generates a relative humidity diagram for both models (LAKE & NOLAKE) at a chosen location
;
; :Params:
;    ipix = in, pixel number in x direction
;    jpix = in, pixel number in y direction
;    month = in
;
; :Keywords:
;    STD_PNG = in, optional, default =0,
;              Set this keyword to save the figure as a standard png in the output directory
;    OUTPUT_DIR = in optional, default = 0,
;                 Set this keyword to determine an output directory.
;                 If this keyword is not set although the std_png keyword is set, a window opens to choose an output directory
;
; :Author: JaH 2012
;-
pro relHumidity_diagram,  ipix, jpix, month, STD_PNG=std_png, OUTPUT_DIR=output_dir

 ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  common ADMIN_LNL
  
  ;investigated pixels in area
  i = ipix
  j = jpix
  m = month ; investigated month
  
  ; get variables of lake model
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  rh_l = rh[i,j,*,m]
  p_l = p[i,j,*,m]

  ; get variables of no lake model  
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  rh_nl=rh[i,j,*,m]
  p_nl = p[i,j,*,m]
  
  ; plot
  xrange = [ min( [[[rh_l]],[[rh_nl]]] ), max( [[[rh_l]],[[rh_nl]]] ) ]
  prange = [ max( [[[p_l]],[[p_nl]]] ), min( [[[p_l]],[[p_nl]]] ) ]
  cgplot, rh_l,  p_l,  color='blue', yrange=prange, xrange=xrange, $
  Title='relative humidity !C !C i='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str, $
  position=[0.12, 0.12, 0.9, 0.85], xtitle='relative humidity [%]', ytitle='pressure [hPa]', /WINDOW
  cgplot, rh_nl, p_nl, color='black', /Overplot, /WINDOW
  al_legend, ['Lake', 'No Lake'], color=['blue', 'black'], LineStyle=[0,0], POSITION=[0.66,0.8],/Normal, /Window
 
  ; save figure
  if N_ELEMENTS(std_png) ne 0 then begin
    FILE_MKDIR,output_dir+'HumidityDiagrams'
    pngname='HumidityDiagrams/RelH_'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif

end


;+
; :Description:
;    This procedure generates a specific humidity diagram for both models (LAKE & NOLAKE) at a chosen location
;
; :Params:
;    ipix = in, pixel number in x direction
;    jpix = in, pixel number in y direction
;    month = in
;
; :Keywords:
;    STD_PNG = in, optional, default =0,
;              Set this keyword to save the figure as a standard png in the output directory
;    OUTPUT_DIR = in optional, default = 0,
;                 Set this keyword to determine an output directory.
;                 If this keyword is not set although the std_png keyword is set, a window opens to choose an output directory
;
; :Author: JaH 2012
;-
pro specHumidity_diagram,  ipix, jpix, month, STD_PNG=std_png, OUTPUT_DIR=output_dir

 ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  common ADMIN_LNL
  
  ;investigated pixels in area
  i = ipix
  j = jpix
  m = month ; investigated month
  
  ; get variables of lake model  
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  qv_l = qv[i,j,*,m]
  p_l = p[i,j,*,m]

  ; get variables of no lake model  
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  qv_nl=qv[i,j,*,m]
  p_nl = p[i,j,*,m]
  
  ; plot
  xrange = [ min( [[[qv_l]],[[qv_nl]]] ), max( [[[qv_l]],[[qv_nl]]] ) ]
  prange = [ max( [[[p_l]],[[p_nl]]] ), min( [[[p_l]],[[p_nl]]] ) ] 
  cgplot, qv_l,  p_l,  color='blue', yrange=prange, xrange=xrange, $
  Title='specific humidity !C !C i='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str, $
  position=[0.12, 0.12, 0.9, 0.85], xtitle='specific humidity [g/m^3]', ytitle='pressure [hPa]', /WINDOW
  cgplot, qv_nl, p_nl, color='black', /Overplot, /WINDOW
  al_legend, ['Lake', 'No Lake'], color=['blue', 'black'], LineStyle=[0,0], POSITION=[0.66,0.8],/Normal, /Window
 
  ; save figure
  if N_ELEMENTS(std_png) ne 0 then begin
    FILE_MKDIR,output_dir+'HumidityDiagrams'
    pngname='HumidityDiagrams/SpecH_'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif

end


pro temp_diagram,  ipix, jpix, month, STD_PNG=std_png, OUTPUT_DIR=output_dir

 ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  common ADMIN_LNL
  
  ;investigated pixels in area
  i = ipix
  j = jpix
  m = month ; investigated month
  
  ; get variables of lake model  
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  t_l = t[i,j,*,m]
  p_l = p[i,j,*,m]

  ; get variables of no lake model  
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  t_nl = t[i,j,*,m]
  p_nl = p[i,j,*,m]
  
  ; plot
  xrange = [ min( [[[t_l]],[[t_nl]]] ), max( [[[t_l]],[[t_nl]]] ) ]
  prange = [ max( [[[p_l]],[[p_nl]]] ), min( [[[p_l]],[[p_nl]]] ) ] 
  cgplot, t_l,  p_l,  color='blue', yrange=prange, xrange=xrange, $
  Title='temperature !C !C i='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str, $
  position=[0.12, 0.12, 0.9, 0.85], xtitle='temperature ['+cgsymbol('deg')+'C]', ytitle='pressure [hPa]', /WINDOW
  cgplot, t_nl, p_nl, color='black', /Overplot, /WINDOW
  al_legend, ['Lake', 'No Lake'], color=['blue', 'black'], LineStyle=[0,0], POSITION=[0.66,0.8],/Normal, /Window
 
  ; save figure
  if N_ELEMENTS(std_png) ne 0 then begin
    FILE_MKDIR,output_dir+'TemperatureDiagrams'
    pngname='TemperatureDiagrams/Temp_'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif

end



pro potTemp_diagram,  ipix, jpix, month, STD_PNG=std_png, OUTPUT_DIR=output_dir

 ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  common ADMIN_LNL
  
  ;investigated pixels in area
  i = ipix
  j = jpix
  m = month ; investigated month
  
  ; get variables of lake model  
  lnl_pt_diagramms_get_Vars, wrf_std, theta, p, t, z, rh, qv, w, td, time 
  time_str = TIME_to_STR(time[m], MASK='YYYY.MM')
  theta_l = theta[i,j,*,m]
  p_l = p[i,j,*,m]

  ; get variables of no lake model  
  lnl_pt_diagramms_get_Vars, wrf_nl, theta, p, t, z, rh, qv, w, td, time 
  theta_nl = theta[i,j,*,m]
  p_nl = p[i,j,*,m]
  
  ; plot
  xrange = [ min( [[[theta_l]],[[theta_nl]]] ), max( [[[theta_l]],[[theta_nl]]] ) ]
  prange = [ max( [[[p_l]],[[p_nl]]] ), min( [[[p_l]],[[p_nl]]] ) ] 
  cgplot, theta_l,  p_l,  color='blue', yrange=prange, xrange=xrange, $
  Title='potential temperature !C !C i='+STRING(ipix,FORMAT='(I2)')+' | j='+STRING(jpix,FORMAT='(I2)')+' | ' + time_str, $
  position=[0.12, 0.12, 0.9, 0.85], xtitle='theta ['+cgsymbol('deg')+'C]', ytitle='pressure [hPa]', /WINDOW
  cgplot, theta_nl, p_nl, color='black', /Overplot, /WINDOW
  al_legend, ['Lake', 'No Lake'], color=['blue', 'black'], LineStyle=[0,0], POSITION=[0.66,0.23],/Normal, /Window
 
  ; save figure
  if N_ELEMENTS(std_png) ne 0 then begin
    FILE_MKDIR,output_dir+'TemperatureDiagrams'
    pngname='TemperatureDiagrams/potTemp_'+STRING(ipix,FORMAT='(I2)')+'_'+STRING(jpix,FORMAT='(I2)')+'_'+STRING(month,FORMAT='(I2)')+''
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif

end


