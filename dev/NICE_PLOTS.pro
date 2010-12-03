;-----------------------------------------------------------------------
;+
; NAME:
;       PLO_plot_domain
;
; PURPOSE:
;       This procedure plots the domains definition
;
; CATEGORY:
;       PLO
;
; CALLING SEQUENCE:
;       PLO_plot_domain
;       
; INPUT:
; 
; KEYWORDS:
;       wrf_obj: the obj to plot
;       N_nested_dom: the number of internal domains to draw
;       PNG: string containing the path of the pic to save. if setted, the pic wont be showed but saved in the given path. 
;       PIXMAP: show, not show ?
;       WINDOW: window id (default: 1)
;       TITLE: Graph title
;       NO_BAR: you dont want a color bar ? 
;       HORIZONTAL_BAR: or you want to have it horizontal ?
; OUTPUT:
;       a beautiful plot
;-
; MODIFICATION HISTORY:
;       Written by: FM, 2009
;-  
;-----------------------------------------------------------------------
pro nice_plot_domain, wrf_obj = wrf_obj, N_nested_dom = N_nested_dom, $
                      NO_BAR = no_bar, HORIZONTAL_BAR = horizontal_bar, $
                      WINDOW = window, PIXMAP = pixmap, PNG = png, $
                      TITLE = title, TOPO_FINE = TOPO_FINE, DATA_LEVELS = data_levels, $
                      LatLon_interval = LatLon_interval, SHAPE_loc = SHAPE_loc
  
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  WAVE_root, root
  
  ;--------------------------
  ; WRF Input file (default)
  ;--------------------------
  wrf_file = ''
  if not keyword_set(wrf_obj) then begin
    wrf_file = DIALOG_PICKFILE(TITLE='Please select a WRF file')
    if wrf_file eq '' then return
    wrf_obj = OBJ_NEW('WRF_nc', file = wrf_file, CROPB=cropb, CROPD2=cropd2, SUBSET = subset)
  endif
  
  ;--------------------------
  ; Set Topography
  ;--------------------------
  type = wrf_obj->type()
  if type eq 'GEO' or type eq 'MET' then  attid = 'HGT_M' else   attid = 'HGT'
  height = wrf_obj->GET_VAR(attid, /CONTI)
  
  ; Definition of colors and levels
  ;data_levels = [-999,-100,250,650,1000,1400,1800,2200,2600,3000,3400,3800,4200,4600,5000,5400,5800]
  if NOT KEYWORD_SET(DATA_LEVELS) then utils_data_levels, height, DATA_LEVELS, NLEVS=16
  data_levels = [-900, DATA_LEVELS]
 
  water = wrf_obj->GET_VAR('LANDMASK', /CONTI)
  p = where(water eq 0, cnt)
  if cnt ne 0 then height[p] = -900.
  

  
  bar_tags = STRING(data_levels,FORMAT='(I4)')
  bar_tags[0] = 'Water'
   
  r = [0, 45, 33, 24, 0, 52 , 65, 75, 87, 154, 239, 254, 249, 244, 235, 220, 202]
  g = [138, 85, 100, 128, 150, 167 , 189, 215, 230, 245, 253, 237, 200, 171, 133, 102, 83]   ; Version 1
  b = [255, 105, 115, 118, 127, 124 , 91, 45, 0, 0, 0, 8, 26, 31, 44, 53, 60]
  
  
  color_levels = [[r],[g],[b]]
  
  titlebar = 'Height (m)'
  if KEYWORD_SET(title) then graphtitle = title else  graphtitle = 'WRF domain.'
  subtitle = ''
  
  plo_array, height, wrf_obj, data_levels, color_levels, BAR_TAGS=bar_tags, TITLEBAR=titlebar, GRAPHTITLE = graphtitle, $
            /PIXMAP, WINDOW = window, SUBTITLE = subtitle, NO_BAR = no_bar, HORIZONTAL_BAR = horizontal_bar, $
            TOPO_FINE = TOPO_FINE, LatLon_interval = LatLon_interval, SHAPE_loc = SHAPE_loc
    
  ;--------------------------
  ; Plot under domains
  ;--------------------------
  wrf_obj->GetProperty, dom = dom, path =  active_file
  if KEYWORD_SET(N_nested_dom) then begin
    XYOUTS, StartX  + 4, StartY + 4, 'd0'+STRING(dom, FORMAT='(I1)'), ALIGNMENT=0 , CHARSIZE= N_nested_dom * W_size_fac, /DEVICE, COLOR = 0
    for j = 0, N_nested_dom-1 do begin
      GEN_str_subst, ret, active_file, 'd0' + STRING(dom, FORMAT='(I1)'), 'd0' + STRING(dom+1+j, FORMAT='(I1)'), wrf_filej
      inner = OBJ_NEW('WRF_nc', file = wrf_filej)
      PLO_draw_dom, inner, (N_nested_dom - j), ADD_num = (N_nested_dom - j)
      OBJ_DESTROY, inner
    endfor
  endif
  
  ;--------------------------
  ; Resolve all
  ;--------------------------
  PLO_show_img, PIXMAP=pixmap, WINDOW = window, PNG = png
  
  if wrf_file ne '' then OBJ_DESTROY, wrf_obj
  
end


pro nice_plot_temps, WRF_OBJ = WRF_OBJ, CROPB=cropb, CROPD2=cropd2, SUBSET = subset,  $
                     WINDOW = window, HORIZONTAL_BAR = horizontal_bar, NO_BAR = No_bar, $
                     PIXMAP=pixmap, PNG = png, range = range, levels = levels,  $
                     SKINTEMP = SKINTEMP, GROUNDTEMP1=GROUNDTEMP1, GROUNDTEMP2=GROUNDTEMP2, $
                     GROUNDTEMP3=GROUNDTEMP3, GROUNDTEMP4=GROUNDTEMP4, SST=SST, AIRTEMP = AIRTEMP, $
                     VER_level = VER_level, SECRET = secret, SOILTEMP = SOILTEMP
         

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  WAVE_root, root
  
  ;--------------------------
  ; WRF Input file (default)
  ;--------------------------
  wrf_file = ''
  if not keyword_set(wrf_obj) then begin
    wrf_file = DIALOG_PICKFILE(TITLE='Please select a WRF file')
    if wrf_file eq '' then return
    wrf_obj = OBJ_NEW('WRF_nc', file = wrf_file, CROPB=cropb, CROPD2=cropd2, SUBSET = subset)
  endif
  
  if wrf_obj->type() eq 'WRF' or wrf_obj->type() eq 'AGG' then message, 'This is a WRF file, the plot routine is not the good one'
  
  ;--------------------------
  ; Init data structures
  ;--------------------------  
  if KEYWORD_SET(SKINTEMP) then begin
    toplot = wrf_obj->get_Var('SKINTEMP') - 273.15
    title = 'Skin temperature ' 
  endif else if KEYWORD_SET(SOILTEMP) then begin
    toplot = wrf_obj->get_Var('SOILTEMP') - 273.15
    title = 'Annual mean deep soil temperature ' 
  endif else if KEYWORD_SET(GROUNDTEMP1) then begin
    toplot = wrf_obj->get_Var('ST000010') - 273.15
    title = 'T 0-10 cm below ground ' 
  endif else if KEYWORD_SET(GROUNDTEMP2) then begin
    toplot = wrf_obj->get_Var('ST010040') - 273.15
    title = 'T 10-40 cm below ground ' 
  endif else if KEYWORD_SET(GROUNDTEMP3) then begin
    toplot = wrf_obj->get_Var('ST040100') - 273.15
    title = 'T 40-100 cm below ground ' 
  endif else if KEYWORD_SET(GROUNDTEMP4) then begin
    toplot = wrf_obj->get_Var('ST100200') - 273.15
    title = 'T 100-200 cm below ground ' 
  endif else if KEYWORD_SET(SECRET) then begin
    toplot = wrf_obj->get_Var('ST') - 273.15
    if ~KEYWORD_SET(VER_level) then vlev = 0 else vlev = VER_level
    toplot = toplot[*,*,vlev]
    title = 'Secret temp level ' + str_equiv(vlev) + '. '
  endif else if KEYWORD_SET(AIRTEMP) then begin
    toplot = wrf_obj->get_Var('TT') - 273.15
    if ~KEYWORD_SET(VER_level) then vlev = 0 else vlev = VER_level
    toplot = toplot[*,*,vlev]
    title = 'T on vertical level ' + str_equiv(vlev) + '. '
  endif else if KEYWORD_SET(SST) then begin
    toplot = wrf_obj->get_Var('SST') - 273.15
    title = 'Sea Surface Temperature ' 
  endif
  
  if ~ KEYWORD_SET(RANGE) then range = [FLOOR(min(toplot)), FLOOR(max(toplot))]  
  if KEYWORD_SET(levels) then begin
    n_levels = N_ELEMENTS(levels) 
  endif else begin
    n_levels = 16     
  endelse

  utils_data_levels, toplot, levs, clevs, NLEVS=n_levels, range = range;, MODIF_DATA = toplot  
  if KEYWORD_SET(levels) then levs = levels
  
  tags = STRING(levs, FORMAT = '(F8.1)')
  titlebar = 'Temperature (C)'
  wrf_obj->get_time, time
  strdate = PLO_time_str_for_plot(time[0], /HOUR)      
  
  PLO_array, toPlot, WRF_OBJ, levs, clevs, COLOR_TABLE = 13, BAr_tags = tags, titlebar = titlebar,GRAPHTITLE = title + strdate, $
                            PNG = png, PIXMAP=PIXMAP, WINDOW = window, SUBTITLE = '', $
                            HORIZONTAL_BAR = horizontal_bar, NO_BAR = No_bar
    
end


pro nice_plot_pcrp_wind,  WRF_OBJ = WRF_OBJ, CROPB=cropb, CROPD2=cropd2, SUBSET = subset,  $
                          WINDOW = window, HORIZONTAL_BAR = horizontal_bar, NO_BAR = No_bar, $
                          PIXMAP=pixmap, PNG = png, range = range, levels = levels, $
                          LATLON_INTERVAL=LATLON_INTERVAL, DENSITY = density
                          
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  WAVE_root, root
                          
  ;--------------------------
  ; WRF Input file (default)
  ;--------------------------
  wrf_file = ''
  if not keyword_set(wrf_obj) then begin
    wrf_file = DIALOG_PICKFILE(TITLE='Please select a WRF file')
    if wrf_file eq '' then return
    wrf_obj = OBJ_NEW('WRF_nc', file = wrf_file, CROPB=cropb, CROPD2=cropd2, SUBSET = subset)
  endif
  
  if ~KEYWORD_SET(density) then density = 1
  
  if ~(wrf_obj->type() eq 'WRF' or wrf_obj->type() eq 'AGG') then message, 'This is not a WRF file'
  
  toplot = wrf_obj->get_prcp(times, nt) ; This is acc pcp (NC + C)
  toplot = toplot[*,*,nt-1] ; For the total acc pcp  
  
  if ~ KEYWORD_SET(RANGE) then range = [FLOOR(min(toplot)), FLOOR(max(toplot))]  
  if KEYWORD_SET(levels) then begin
    n_levels = N_ELEMENTS(levels) 
  endif else begin
    n_levels = 16     
  endelse

  utils_data_levels, toplot, levs, clevs, NLEVS=n_levels, range = range, COLOR_RANGE=[110,240]
  if KEYWORD_SET(levels) then levs = levels
  tags = STRING(levs, FORMAT = '(F8.1)')
  titlebar = 'Prcp (mm)' ; Title for color bar

  u = wrf_obj->get_VAR('U10', times = t, nt = nt) ; wind u direction
  meanu = total(u,3) / nt
  v = wrf_obj->get_VAR('V10', times = t, nt = nt) ; wind v direction
  meanv = total(v,3) / nt

  title = 'WRF pcp and wind'

   PLO_array, toplot, wrf_obj, levs, ROTATE(clevs,2), COLOR_TABLE = 1, /PIXMAP, GRAPHTITLE= title, TITLEBAR=titlebar, LATLON_INTERVAL=LATLON_INTERVAL, BAR_TAGS=tags, WINDOW=1, /TOPO_FINE

   PLO_draw_wind, wrf_obj, meanu, meanv, density, /LEGEND

   PLO_show_img, window = 1, PNG = png, PIXMAP = pixmap

end 
