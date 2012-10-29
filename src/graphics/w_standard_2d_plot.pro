; docformat = 'rst'
;+
; :Description:
;    This routine is a template for generating figures from a 
;    map object and write the output in different formats (png, postscript...).
;    
;    A standard procedure to follow would look like: 
;    
;    1. Generate a `W_Map` object
;    
;    2. Set the map data and plot properties
;    
;    3. Call `w_standard_2d_plot`, map
;    
;    4. Re-set the map data and plot properties
;
;    5. Call `w_standard_2d_plot`, map
;    
;    .. etc.
;    
;    X. Destroy the map object
;
; :Params:
;    map: in, required, type = `W_Map`
;         the map object from which the plot has to be taken from
;
; :Keywords:
;    TITLE: in, optional, type = string, default = ''
;           The title of the plot
;    BAR_TITLE: in, optional, type = string, default = 'Data levels'
;               The title of the color bar
;    BAR_TAGS: in, optional, type = string array
;              the levels tags for the color bar (ignored if the color bar is continuous)
;    BAR_FORMAT: in, optional, type = string, default = '(F5.1)'
;                if `BAR_TAGS` is not set, the tags will be generated automatically from the data levels.
;                set this keyword to a format accepted by the IDL `STRING()` function to format the tags. 
;    BAR_SPACING: in, optional, type = float
;                 This keyword gives the user some control over the location of the tags
;                 with respect to the bar. 
;                 The default spacing is 1.0. The location "spacer" is
;                 multiplied by this amount. So, for example, to move the labels a little
;                 further away from the color bar, make this number greater than 1 (e.g, 1.25).
;                 To move the labels a little closer, use a number less than 1 (e.g, 0.75).
;    PIXMAP: in, optional, type = boolean
;            set this keyword to plot everything in the pixmap buffer (usefull for automatic output generation)
;    SOURCE_INFO: in, optional, type = string, default = ''
;                 A subtitile to the plot. To generate multiple lines, use the '!C' symbol 
;                 (e.g: LEGEND_INFO = 'line1 blabla' + '!C' + 'line2 blabla')
;    RESIZABLE: in, optional, type = boolean
;               set this keyword to generate the plot in a cgWindow
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    JPEG: in, optional, type = string
;         set to a filename to generate a jpeg output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    PDF: in, optional, type = string
;         set to a filename to generate a PDF output
;    STD_PNG: in, optional, type = string
;             set to a filename to generate a standard png output (without image magick)
;    STD_JPEG: in, optional, type = string
;              set to a filename to generate a standard jpeg output (without image magick)
;    DISP_IMG: out, type = byte array
;              the plot image in RGB colours (dimensions: [3, nx, ny])
;    ANTI_ALIASING: in, optional, type = boolean
;                   if set, an anti-aliasing is made "on the fly". 
;                   (ignored if the `RESIZABLE`, `JPEG`, `EPS` or `PNG` keywords are set)
;    XFACTOR: in, optional, type=float, default=1.
;             This is a way to control the X size of the plot. This factor will be multipled 
;             to the default computed lenght of the plot
;    NO_BAR: in, optional
;            set this keyword if you don't want to plot the color bar
;    NO_LEGEND: in, optional
;               set this keyword if you don't want to add a legend to the plot
;    IM_RESIZE: in, optional, type=integer, default=50
;                Set this keyword to percentage that the raster image file created my ImageMagick
;                from PostScript output should be resized.
;
;
; :History:
;     Written by FaM, DiS, 2011.
;
;
;-
pro w_standard_2d_plot, map, $
    TITLE=title,$
    BACKGROUND=background,$
    BAR_TITLE=bar_title,  $
    BAR_TAGS=bar_tags, $
    BAR_FORMAT=bar_format, $
    BAR_OPEN=bar_open,  $
    TOP_BAR_OPEN=top_bar_open, $
    BOT_BAR_OPEN=bot_bar_open, $
    BAR_SPACING=BAR_spacing,  $
    PIXMAP=pixmap,  $
    SOURCE_INFO=source_info, $
    RESIZABLE=resizable, $
    PNG=png, JPEG=jpeg, EPS=eps, PDF=pdf, STD_PNG=std_png, STD_JPEG=std_jpeg, $
    DISP_IMG=disp_img, $
    ANTI_ALIASING=anti_aliasing, $
    XFACTOR=xfactor, $
    NO_BAR=no_bar, $
    NO_LEGEND=no_legend, $
    IM_RESIZE=im_resize
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  pp = !ORDER ; To restore later
  !ORDER = 0
  
  ;******************
  ; Check arguments *
  ;******************
  if not OBJ_ISA(map, 'w_Map')  then begin
    message, WAVE_Std_Message('PLOT_MAP', OBJ='w_Map')
    return
  endif
  if KEYWORD_SET(BAR_OPEN) then message, 'BAR_OPEN keyword is now obsolete. See w_map->set_plot_params() for controling the color bar', /INFORMATIONAL
  if KEYWORD_SET(TOP_BAR_OPEN) then message, 'TOP_BAR_OPEN keyword is now obsolete. See w_map->set_plot_params() for controling the color bar', /INFORMATIONAL
  if KEYWORD_SET(BOT_BAR_OPEN) then message, 'BOT_BAR_OPEN keyword is now obsolete. See w_map->set_plot_params() for controling the color bar', /INFORMATIONAL
  
  map->GetProperty, XSIZE=xsize, YSIZE=ysize, TNT_c = tnt_c
  
  if n_elements(title) eq 0 then title = ''
  if n_elements(WTITLE) eq 0 then wtitle = 'WAVE standard plot'
  if n_elements(BAR_TITLE) eq 0 then bar_title = ''
  if n_elements(IM_RESIZE) eq 0 then im_resize = 50
  
  ar =  float(xsize) / ysize
  if ar ge 1. then begin
    xs = FLOOR(xsize * 1.26d)
    ys = FLOOR(ysize * 1.16d)
  endif else begin
    xs = FLOOR(xsize * 1.34d)
    ys = FLOOR(ysize * 1.14d)
  endelse
  
  if N_ELEMENTS(XFACTOR) eq 0 then XFACTOR = 1.
  xs *= XFACTOR
  
  imgX = xsize/double(xs)
  imgY = ysize/double(ys)
  imx0 = 0.07
  imy0 = 0.08
  pos = [imx0,imy0,imx0+imgX,imy0+imgY]
  
  sfac = 1. ; Font size factor
  
  ; Anti aliasing
  do_as = FALSE
  if KEYWORD_SET(anti_aliasing) $
    and ~KEYWORD_SET(PNG) $
    and ~KEYWORD_SET(RESIZABLE) $
    and ~KEYWORD_SET(EPS) $
    and ~KEYWORD_SET(PDF) $
    and ~KEYWORD_SET(JPEG) then begin
    oxs = xs
    oys = ys
    xs = xs * 4
    ys = ys * 4
    sfac *= 4.
    do_as = TRUE
  endif
  
  ; Check what we want to do
  if keyword_set(eps) or keyword_set(pdf) or keyword_set(png) or keyword_set(jpeg) and ~KEYWORD_SET(RESIZABLE) then begin
;    if ~KEYWORD_SET(PIXMAP) then Message, 'With the PNG, PDF, JPEG and EPS keyword and without resizable windows, the window cannot be visible. Im setting PIXMAP.', /INFORMATIONAL
    PIXMAP = true
  endif
  if keyword_set(pixmap) then visible = FALSE else visible = TRUE
  cgWIN = FALSE
  
  cgDisplay, /FREE, XSIZE=xs, YSIZE=ys, /PIXMAP, TITLE=WTITLE, COLOR=background
  xwin = !D.WINDOW
  
  if visible and keyword_set(resizable) then begin
    WDELETE, xwin
    cgWindow, WXSIZE=xs, WYSIZE=ys, WTITLE=WTITLE, WOBJ=wobj, WBACKGROUND=background
    cgControl, EXECUTE=0, PS_DECOMPOSED=1 
    cgWIN = TRUE
    wobj->GetProperty, WID=cgWID
  endif else begin
    if total([keyword_set(eps),keyword_set(pdf),keyword_set(png),keyword_set(jpeg)]) gt 1 then message, 'In /PIXMAP mode, only one image can be written at the same time.'
      if keyword_set(eps) then PS_Start, FILENAME=eps, /DECOMPOSED, /LAND $
        else if keyword_set(pdf) then PS_Start, FILENAME=pdf, /DECOMPOSED   $
          else if keyword_set(png) then PS_Start, FILENAME=png, /DECOMPOSED   $
            else if keyword_set(jpeg) then PS_Start, FILENAME=jpeg, /DECOMPOSED
  endelse

   ; Begin the plot
  map->add_img, POSITION=pos, WINDOW=cgWIN
  
  ; Title
  cgText, (pos[0]+pos[2])/2., pos[3] + 0.015, title, ALIGNMENT=0.5, $
    WINDOW=cgWIN, /NORMAL, CHARSIZE=1.5*sfac, CHARTHICK = 1.*sfac
    
  ; Bar
  pbar = [pos[2] + 0.04, pos[1]+0.05, pos[2] + 0.06, pos[3]-0.05]
  if ~KEYWORD_SET(NO_BAR) then begin
    map->add_color_bar, TITLE='', LABELS=bar_tags, WINDOW=cgWIN, POSITION=pbar, /RIGHT, /VERTICAL, FORMAT=bar_format, $
      CHARSIZE=1.*sfac, SPACING=BAR_spacing, CHARTHICK = 1.* sfac
    ; Title bar
    cgText, (pbar[0]+pbar[2])/2., pbar[3]+0.025, bar_title, ALIGNMENT=0.5, $
      WINDOW=cgWIN, /NORMAL, CHARSIZE=1. * sfac, CHARTHICK = 1. *sfac
  endif
  
  if ~KEYWORD_SET(NO_LEGEND) then begin
  
    ; Scale
    xSize_map = tnt_C.dx * tnt_C.nx
    xSize_map_bar =  nicenumber(xSize_map/5.)
    if tnt_c.proj.name eq 'Geographic (WGS-84)' then unit = ' deg' else unit = ' m'
    xSize_bar_device =  xSize_map_bar / xSize_map * (pos[2] - pos[0])
    if xSize_map_bar ge 1000. then begin
      xSize_map_bar =  xSize_map_bar / 1000.
      unit = ' km'
    endif
    
    if xSize_map_bar ge 5. then str_xSize_map_bar = str_equiv(LONG(xSize_map_bar)) $
     else if xSize_map_bar ge 1. then str_xSize_map_bar = STRING(xSize_map_bar, FORMAT='(F3.1)') $
      else str_xSize_map_bar = STRING(xSize_map_bar, FORMAT='(F4.2)')
  
    xLegend = pos[0] + [0., xSize_bar_device]
    yLegend = pos[1] / 5. + [0.,0.]
    cgPlotS, xLegend, yLegend, /NORMAL, WINDOW=cgWIN, thick = 1. * sfac
    cgPlotS, [xLegend[0],xLegend[0]], yLegend + [0.005,-0.005], /NORMAL, WINDOW=cgWIN, thick = 1. * sfac
    cgPlotS, [xLegend[1],xLegend[1]], yLegend + [0.005,-0.005], /NORMAL, WINDOW=cgWIN, thick = 1. * sfac
    cgText, (xLegend[1]-xLegend[0]) / 2. + xLegend[0],  yLegend + 0.01, str_xSize_map_bar + unit, ALIGNMENT=0.5, $
      CHARSIZE=1.* sfac, CHARTHICK = 1.*sfac, /NORMAL, WINDOW=cgWIN
    
    ; Projection
    proj_name = 'Projection: ' + tnt_c.proj.name
    cgText, xLegend[1] + 0.05,  yLegend + 0.02, proj_name, ALIGNMENT=0., CHARSIZE=1.* sfac, CHARTHICK = 1.*sfac, /NORMAL, WINDOW=cgWIN
    proj_name = 'Datum: ' + tnt_c.proj.datum.name
    cgText, xLegend[1] + 0.05,  yLegend - 0.005, proj_name, ALIGNMENT=0., CHARSIZE=1.* sfac, CHARTHICK = 1.*sfac, /NORMAL, WINDOW=cgWIN
  
    ; Legend info
    
    if N_ELEMENTS(SOURCE_INFO) eq 0 then source_info = cgSymbol('copyright') + ' '+ $
      TIME_to_STR(QMS_TIME(), MASK='YYYY') +' TU Berlin!C!CChair of Climatology'
      
    if arg_okay(source_info, TYPE=IDL_STRING) then begin
      cgText, 0.80,  yLegend + 0.02, source_info, ALIGNMENT=0., CHARSIZE=0.8* sfac, CHARTHICK = 0.*sfac, /NORMAL, WINDOW=cgWIN
    endif
  
  endif
  
  ; Output
  if visible then begin
  
    if cgWIN then begin
      cgControl, EXECUTE=1
      tmp = !D.window
      wset, cgWID
      disp_img = tvrd(TRUE=1)
      wset, tmp
    endif else begin
      disp_img = tvrd(TRUE=1)
      if do_as then begin
        disp_img = rebin(disp_img, 3, oxs, oys)
        xs = oxs
        ys = oys
      endif
      wdelete, xwin
      cgDisplay, xs, ys, /FREE, Title=WTITLE
      cgImage, transpose(disp_img, [1,2,0])
    endelse
    
    if keyword_set(eps) then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
    if keyword_set(pdf) then cgControl, CREATE_PDF=pdf
    if keyword_set(png) then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
    if keyword_set(jpeg) then cgControl, CREATE_JPEG=jpeg, IM_RESIZE=im_resize, /IM_RASTER
    if keyword_set(std_png) then if cgWin then cgControl, CREATE_PNG=std_png, IM_RASTER=0 else write_png, std_png, disp_img
    if keyword_set(std_jpeg) then if cgWin then cgControl, CREATE_JPEG=std_jpeg, IM_RASTER=0 else write_jpeg, std_jpeg, disp_img
    
  endif else begin
  
    if keyword_set(eps) then PS_End
    if keyword_set(pdf) then PS_End, /PDF
    if keyword_set(png) then PS_End, /PNG, RESIZE=im_resize
    if keyword_set(jpeg) then PS_End, /JPEG, RESIZE=im_resize
    
    disp_img = tvrd(TRUE=1)
    if do_as then disp_img = rebin(disp_img, 3, oxs, oys)
    
    if keyword_set(std_png) then write_png, std_png, disp_img
    if keyword_set(std_jpeg) then write_jpeg, std_jpeg, disp_img
    
    if xwin ne -1 then wdelete, xwin
    
  endelse
  
  !ORDER = pp
  
end
