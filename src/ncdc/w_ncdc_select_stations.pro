pro w_ncdc_select_stations, usaf, wban, n_stat, GRID=grid, CORNERS=corners, DO_PLOT=do_plot
 
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ;ON_ERROR, 2
  
  do_p = KEYWORD_SET(DO_PLOT)
  
  undefine, usaf, wban
  n_stat = 0L
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  GIS_make_datum, ret, wgs, NAME='WGS-84'
  
  lat = ncdc_history.lat > (-89.99) ; for the AMUNDSEN-SCOTT station!
  lon = ncdc_history.lon
  _usaf= ncdc_history.usaf
  _wban= ncdc_history.wban  
   
  IF N_ELEMENTS(GRID) ne 0 then begin
   
   if not OBJ_ISA(grid, 'w_Grid2D') then Message, WAVE_Std_Message('grid', OBJ='w_Grid2D')
   grid->GetProperty,  nx=nx, ny=ny

   grid->transform_LonLat, lon, lat, wgs, i_dst, j_dst   
   dummy = where((i_dst lt (-0.5)) or (j_dst lt (-0.5)) or (i_dst gt (nx+0.5)) or (j_dst gt (ny+0.5)), COMPLEMENT=p_in, NCOMPLEMENT=cnt_in)
   if cnt_in eq 0 then begin 
    MESSAGE, 'No NCDC station found!', /INFORMATIONAL
    return
   endif
   
   lat = lat[p_in]
   lon = lon[p_in]
   usaf= _usaf[p_in]
   wban= _wban[p_in]  
   
   if do_p then map = OBJ_NEW('w_Map', grid, YSIZE=600, /BLUE_MARBLE)
   
  endif else if N_ELEMENTS(CORNERS) eq 4 then begin
    
    dummy = where((lon lt corners[0]) or (lat lt corners[1]) or (lon gt corners[2]) or (lat gt corners[3]), COMPLEMENT=p_in, NCOMPLEMENT=cnt_in)
    
    if cnt_in eq 0 then begin 
    MESSAGE, 'No NCDC station found!', /INFORMATIONAL
    return
   endif
   
   lat = lat[p_in]
   lon = lon[p_in]
   usaf= _usaf[p_in]
   wban= _wban[p_in]  
   
   if do_p then begin 
    GIS_make_proj, ret, proj, PARAM='1, WGS-84'
    grid = OBJ_NEW('w_Grid2D', x0 = corners[0], x1 = corners[2], y0 = corners[3], y1 = corners[1], dx=0.01, dy=0.01, PROJ=proj)
    map = OBJ_NEW('w_Map', grid, YSIZE=600, /BLUE_MARBLE)
    undefine, grid
   endif
  endif else MESSAGE, WAVE_Std_Message(/ARG)
  
  n_stat = N_ELEMENTS(usaf)
  
  if do_p and n_stat ne 0 then begin
    d = map->set_point(lon, lat, SRC=wgs, PSYM=SymCat(16), COLOR='orange')
    w_standard_2d_plot, map, title='ALL available NCDC stations', /RESIZABLE, /NO_BAR
    undefine, map
  endif
  
end
