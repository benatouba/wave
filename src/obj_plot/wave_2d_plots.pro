pro standard_2d_plot, plot_map, WINDOW = window, TITLE = title, BAR_TITLE = BAR_TITLE, BAR_TAGS = bar_tags, STR_FORMAT = str_format, PNG = png, PIXMAP = pixmap
   
  @WAVE.inc
   
  pp = !ORDER ;To restore later
  !ORDER = 0
  
  ;******************
  ; Check arguments *
  ;******************
  if not OBJ_ISA(plot_map, 'PLOT_MAP')  then Message, WAVE_Std_Message('PLOT_MAP', OBJ='Grid2D')
  plot_map->GetProperty, XSIZE = xsize, YSIZE = ysize, LEVELS = levels, COLORS = colors
  
  if N_ELEMENTS(Title) eq 0 then title = ''
  if N_ELEMENTS(str_format) eq 0 then str_format = '(F5.1)'
  if N_ELEMENTS(bar_tags) eq 0 then bar_tags = STRING(levels, FORMAT = str_format)

  if TNT_OS eq 'WINDOWS' then set_plot, 'WIN' else set_plot, 'X'
  device, DECOMP=0

  xs = xsize + 200
  ys = ysize + 100     
  window, XSIZE= xs, YSIZE=ys, Title='WAVE Plot', /FREE, PIXMAP = pixmap
  pwin = !D.WINDOW
  tv, BYTARR(3, xs, ys)+255, /TRUE ;All white
    
  ; Get the real plot
  plot_map->show_img, iwin, /PIXMAP
  WSET, iwin
  tempI = Transpose(tvrd(/TRUE), [1,2,0])
  WDELETE, iwin
  
  ; Put a black fram over it
  window, XSIZE= xsize+4, YSIZE=ysize+4, Title='temp', /FREE, /PIXMAP
  twin = !D.WINDOW
  tv, tempI, 2, 2, TRUE=3, /DEVICE  
  tempI = Transpose(tvrd(/TRUE), [1,2,0])
  WDELETE, twin  
  
  ; Plot it on the global plot
  WSET, pwin  
  tv, tempI, 40, 30, TRUE=3, /DEVICE
  
  ;Title
  XYOUTS, 40 + xsize/2, ys - 50,  title, ALIGNMENT=0.5, CHARSIZE=2, COLOR=FSC_Color('BLACK'), /DEVICE;, FONT=1
  
  ; Bar
  plot_map->show_color_bar, iwin, /PIXMAP, TITLE = BAR_TITLE, BAR_TAGS = bar_tags
  WSET, iwin
  tempI = Transpose(tvrd(/TRUE), [1,2,0])
  WDELETE, iwin   
  WSET, pwin
  tv, tempI, 40+XSIZE+30, 90, TRUE=3, /DEVICE
  
  ; write it down  
  if KEYWORD_SET(PNG) then WRITE_PNG, png, tvrd(/TRUE)  
  if not ARG_PRESENT(WINDOW) and KEYWORD_SET(PIXMAP) then WDELETE, pwin
  
  !ORDER = pp
  
end

pro MAKE_WPLOT_WIND, plot_map, grid, ud, vd, density, LENGTH=length, LEGEND = legend, WINDOW = window, $
           TITLE = title, BAR_TITLE = BAR_TITLE, BAR_TAGS = bar_tags, STR_FORMAT = str_format, PNG = png, PIXMAP = pixmap
   
  @WAVE.inc
   
  pp = !ORDER ;To restore later
  !ORDER = 0
  
  ;******************
  ; Check arguments *
  ;******************
  if not OBJ_ISA(plot_map, 'PLOT_MAP')  then Message, WAVE_Std_Message('PLOT_MAP', OBJ='Grid2D')
  plot_map->GetProperty, XSIZE = xsize, YSIZE = ysize, LEVELS = levels, COLORS = colors
  
  if N_ELEMENTS(Title) eq 0 then title = ''
  if N_ELEMENTS(str_format) eq 0 then str_format = '(F5.1)'
  if N_ELEMENTS(bar_tags) eq 0 then bar_tags = STRING(levels, FORMAT = str_format)

  if TNT_OS eq 'WINDOWS' then set_plot, 'WIN' else set_plot, 'X'
  device, DECOMP=0

  xs = xsize + 200
  ys = ysize + 100     
  window, XSIZE= xs, YSIZE=ys, Title='WAVE Plot', /FREE, PIXMAP = pixmap
  pwin = !D.WINDOW
  tv, BYTARR(3, xs, ys)+255, /TRUE ;All white
    
  ; Get the real plot
  plot_map->show_img, iwin, /PIXMAP
  plot_map->draw_wind, grid, ud, vd, density, LENGTH=length, LEGEND = legend
  WSET, iwin
  tempI = Transpose(tvrd(/TRUE), [1,2,0])
  WDELETE, iwin
  
  ; Put a black fram over it
  window, XSIZE= xsize+4, YSIZE=ysize+4, Title='temp', /FREE, /PIXMAP
  twin = !D.WINDOW
  tv, tempI, 2, 2, TRUE=3, /DEVICE  
  tempI = Transpose(tvrd(/TRUE), [1,2,0])
  WDELETE, twin  
  
  ; Plot it on the global plot
  WSET, pwin  
  tv, tempI, 40, 30, TRUE=3, /DEVICE
  
  ;Title
  XYOUTS, 40 + xsize/2, ys - 50,  title, ALIGNMENT=0.5, CHARSIZE=2, COLOR=FSC_Color('BLACK'), /DEVICE;, FONT=1
  
  ; Bar
  plot_map->show_color_bar, iwin, /PIXMAP, TITLE = BAR_TITLE, BAR_TAGS = bar_tags
  WSET, iwin
  tempI = Transpose(tvrd(/TRUE), [1,2,0])
  WDELETE, iwin   
  WSET, pwin
  tv, tempI, 40+XSIZE+30, 90, TRUE=3, /DEVICE
  
  ; write it down  
  if KEYWORD_SET(PNG) then WRITE_PNG, png, tvrd(/TRUE)  
  if not ARG_PRESENT(WINDOW) and KEYWORD_SET(PIXMAP) then WDELETE, pwin
  
  !ORDER = pp
  
end