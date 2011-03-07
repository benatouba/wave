pro standard_2d_plot, plot_map, TITLE = title, BAR_TITLE = BAR_TITLE, BAR_TAGS = bar_tags, STR_FORMAT = str_format, $
                      PIXMAP = pixmap, RESIZABLE = resizable, PNG = png, JPEG = jpeg, EPS = eps, STD_PNG = std_png
   
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
    
  pp = !ORDER ;To restore later
  !ORDER = 0
    
  ;******************
  ; Check arguments *
  ;******************
  if not OBJ_ISA(plot_map, 'w_Map')  then Message, WAVE_Std_Message('PLOT_MAP', OBJ='w_Map')
  plot_map->GetProperty, XSIZE = xsize, YSIZE = ysize, LEVELS = levels, COLORS = colors
  
  if N_ELEMENTS(Title) eq 0 then title = ''
  if N_ELEMENTS(str_format) eq 0 then str_format = '(F5.1)'
  if N_ELEMENTS(bar_tags) eq 0 then bar_tags = STRING(levels, FORMAT = str_format)
  
  xs = xsize + 180
  ys = ysize + 70  
  pos = [45d/xs,30d/ys,(1d - 125d/xs),(1d - 40d/ys)]
  
  ;Trick because no output keyword
  cgDisplay, /FREE, XSIZE=xs, YSIZE=ys, /PIXMAP, Title='WAVE Plot'
  plot_map->add_img, POSITION = pos
  xwin = !D.WINDOW
  
  ; Check what we want to do
  if KEYWORD_SET(PIXMAP) then visible = FALSE else visible = TRUE
  cgWIN = FALSE  
  if visible and KEYWORD_SET(RESIZABLE) then begin
    WDELETE, xwin
    cgWindow, WXSIZE=xs, WYSIZE=ys, Title='WAVE Plot'
    cgControl, EXECUTE=0
    cgWIN = true
  endif else begin
    if KEYWORD_SET(EPS) then PS_START, FILENAME= eps, Decomposed=1, /Encapsulated, /Metric  $
    else if KEYWORD_SET(PNG) then PS_START, FILENAME= png, Decomposed=1 $
    else if KEYWORD_SET(JPEG) then PS_START, FILENAME= jpeg, Decomposed=1
  endelse

  ;Begin the plot
  plot_map->add_img, POSITION = pos, WINDOW = cgWIN   
  ;Title  
  cgText, (pos[0]+pos[2])/2., pos[3] + 15d/xs, title, ALIGNMENT=0.5, COLOR=cgColor('BLACK'), WINDOW = cgWIN, /NORMAL, CHARSIZE=1.5  
  ; Bar
  pbar = [pos[2] + 40d/xs, pos[1]+0.1, pos[2] + 60d/xs, pos[3]-0.1]
  plot_map->add_color_bar, TITLE = '', LABELS = bar_tags, WINDOW = cgWIN, POSITION = pbar, /RIGHT, /VERTICAL, CHARSIZE = 1.  
  ; Title bar
  cgText, (pbar[0]+pbar[2])/2., pbar[3]+0.025, BAR_TITLE, ALIGNMENT=0.5, COLOR=cgColor('BLACK'), WINDOW = cgWIN, /NORMAL, CHARSIZE=1.
  
  ; output
  
  if visible then begin
    if CGWIN then cgControl, EXECUTE=1 else begin
      img = Transpose(tvrd(/TRUE), [1,2,0])
      WDELETE, xwin
      cgDisplay, xs, ys, /FREE, Title='Map Plot'
      cgImage, img
    endelse
    if KEYWORD_SET(PNG) then cgControl, CREATE_PNG=png, IM_RESIZE= 50, /RASTER_IM
    if KEYWORD_SET(STD_PNG) then cgControl, CREATE_PNG=std_png, RASTER_IM=0
    if KEYWORD_SET(JPEG) then cgControl, CREATE_JPEG=jpeg, IM_RESIZE= 50, /RASTER_IM
    if KEYWORD_SET(EPS) then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  endif else begin
    if KEYWORD_SET(PNG) then PS_END, /PNG, resize = 50
    if KEYWORD_SET(JPEG) then PS_END, /JPEG, resize = 50
    if KEYWORD_SET(EPS) then PS_END, /PS_ENCAPSULATED, /PS_METRIC
    if KEYWORD_SET(STD_PNG) then  WRITE_PNG, STD_PNG, tvrd(/TRUE)
    WDELETE, xwin
  endelse
    
  !ORDER = pp
  
end
