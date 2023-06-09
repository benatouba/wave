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
;              the levels tags for the color bar (only for the DCBAR case)
;    BAR_FORMAT: in, optional, type = string
;                if `BAR_TAGS` is not set, the tags will be generated automatically from the data levels.
;                set this keyword to a format accepted by the IDL `STRING()` function to format the tags.                
;    PIXMAP: in, optional, type = boolean
;            set this keyword to make a plot without displaying it
;            (usefull for automatic output generation - STD_PNG)
;    SOURCE_INFO: in, optional, type = string, default = 'TU Berlin Chair of Climatology'
;                 A subtitle to the plot. To generate multiple lines, use the '!C' symbol 
;                 (e.g: LEGEND_INFO = 'line1 blabla' + '!C' + 'line2 blabla')
;    WINDOW: in, optional, type=boolean
;            set this keyword to generate the plot in a cgWindow
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    JPEG: in, optional, type = string
;         set to a filename to generate a jpeg output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    PDF: in, optional, type = string
;         set to a filename to generate a PDF output (uses an external converting tool)
;    IM_RESIZE: in, optional, type=integer, default=50
;               Set this keyword to percentage that the raster image file created my ImageMagick
;               from PostScript output should be resized.
;    STD_PNG: in, optional, type = string
;             set to a filename to generate a standard png output (without image magick)
;    STD_JPEG: in, optional, type = string
;              set to a filename to generate a standard jpeg output (without image magick)
;    DISP_IMG: out, type = byte array
;              the plot image in RGB colours (dimensions: [3, nx, ny])
;    ANTI_ALIASING: in, optional, type = boolean
;                   if set, an anti-aliasing is made "on the fly". This improves the quality 
;                   of the displayed output.  
;                   (ignored if the `WINDOW`, `PDF`, `JPEG`, `EPS` or `PNG` keywords are set)
;    XFACTOR: in, optional, type=float, default=1.
;             This is a way to control the X size of the plot. This factor will be multipled 
;             to the default length of the plot (usefull if your colorbar is too wide for example)
;    SPACING: in, optional
;             control the spacing between tags and DCBar
;    HORIZONTALCB: in, optional
;                  set this keyword if you want an horizontal Color Bar (experimental)
;    NO_BAR: in, optional
;            set this keyword if you don't want to plot the color bar
;    NO_LEGEND: in, optional
;               set this keyword if you don't want to add a legend to the plot
;    NO_SOURCE_INFO: in, optional
;                    set this keyword if you don't want to add a source information text to the plot
;    NO_SCALE_INFO: in, optional
;                   set this keyword if you don't want to add a scale bar to the plot
;    NO_PROJ_INFO: in, optional
;                  set this keyword if you don't want to add a projection information text to the plot
;    OOB_FACTOR: in, optional, type=float, default=1.0
;       The default is to make the length of the out-of-bounds triangle the
;       same distance as the height (or width, in the case of a vertical
;       color bar) of the color bar. If you would prefer a shorted triangle length, 
;       set this keyword to a value less than zero (e.g., 0.5). If you prefer a 
;       longer length, set this keyword to a value greater than zero. The "standard"
;       length will be multiplied by this value.
;    FONTSIZEFACTOR: in, optional, type=float, default=1.0
;       Some factor controlling the font size of scale bar etc.
;
; :History:
;     Written by FaM, DiS, 2011, 2012.
;
;-
pro w_standard_2d_plot, map, $
    TITLE=title,$
    BACKGROUND=background, $
    BAR_TITLE=bar_title, $
    BAR_TAGS=bar_tags, $
    BAR_FORMAT=bar_format, $
    PIXMAP=pixmap,  $
    SOURCE_INFO=source_info, $
    WINDOW=window, $
    PNG=png, JPEG=jpeg, EPS=eps, PDF=pdf, STD_PNG=std_png, STD_JPEG=std_jpeg, $
    IM_RESIZE=im_resize, $
    DISP_IMG=disp_img, $
    ANTI_ALIASING=anti_aliasing, $
    XFACTOR=xfactor, $
    HORIZONTALCB=horizontalcb, $
    CHARSIZE=charsize, $
    OOB_FACTOR=oob_factor, $
    SPACING=spacing, $
    NO_BAR=no_bar, $
    NO_LEGEND=no_legend, $
    NO_SOURCE_INFO=no_source_info, $
    NO_SCALE_INFO=no_scale_info, $
    NO_PROJ_INFO=no_proj_info, $
    FONTSIZEFACTOR=fontsizefactor
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  ;******************
  ; Check arguments *
  ;******************
  if not OBJ_ISA(map, 'w_Map')  then begin
    message, WAVE_Std_Message('PLOT_MAP', OBJ='w_Map')
    return
  endif
   
  pp = !ORDER ; To restore later
  !ORDER = 0
  
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
  
  _hcb = KEYWORD_SET(HORIZONTALCB)
  
  if _hcb then begin
    _ys = xs
    xs = ys
    ys = _ys
  endif
  
  if N_ELEMENTS(XFACTOR) eq 0 then XFACTOR = 1.
  xs *= XFACTOR
  
  imgX = xsize/double(xs)
  imgY = ysize/double(ys)
  imx0 = 0.07
  imy0 = 0.08
  pos = [imx0,imy0,imx0+imgX,imy0+imgY]
  
  if ~isa(fontsizefactor) then begin
    sfac = 1. ; Font size factor
  endif else begin
    sfac = fontsizefactor
  endelse
  
  xyFactor = FLOAT(imgX)/imgY
  SetDefaultValue, OOB_FACTOR, xyFactor
  SetDefaultValue, WINDOW, 1
  SetDefaultValue, CHARSIZE, 1.
  
  ; Anti aliasing
  do_as = FALSE
  if KEYWORD_SET(anti_aliasing) $
    and ~KEYWORD_SET(PNG) $
    and ~KEYWORD_SET(WINDOW) $
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
  if keyword_set(eps) or keyword_set(pdf) or keyword_set(png) or keyword_set(jpeg) and ~KEYWORD_SET(WINDOW) then begin
;    if ~KEYWORD_SET(PIXMAP) then Message, 'With the PNG, PDF, JPEG and EPS keyword and without resizable windows, the window cannot be visible. Im setting PIXMAP.', /INFORMATIONAL
    PIXMAP = true
  endif
  if keyword_set(pixmap) then visible = FALSE else visible = TRUE
  cgWIN = FALSE
  
  cgDisplay, XSIZE=xs, YSIZE=ys, TITLE=WTITLE, COLOR=background, /PIXMAP, /FREE, /WINDOW
  xwin = !D.WINDOW
  
  if visible and keyword_set(WINDOW) then begin
    WDELETE, xwin
    cgWindow, WXSIZE=xs, WYSIZE=ys, WTITLE=WTITLE, WOBJ=wobj, WBACKGROUND=background
    cgControl, EXECUTE=0, PS_DECOMPOSED=1 
    cgWIN = TRUE
    wobj->GetProperty, WID=cgWID
  endif else begin
    if total([keyword_set(eps),keyword_set(pdf),keyword_set(png),keyword_set(jpeg)]) gt 1 then message, 'In /PIXMAP mode, only one image can be written at the same time.'
      if keyword_set(eps) then cgPS_Open, FILENAME=eps, /DECOMPOSED, /ENCAPSULATED $
        else if keyword_set(pdf) then cgPS_Open, FILENAME=pdf, /DECOMPOSED   $
          else if keyword_set(png) then cgPS_Open, FILENAME=png, /DECOMPOSED   $
            else if keyword_set(jpeg) then cgPS_Open, FILENAME=jpeg, /DECOMPOSED
  endelse

   ; Begin the plot
  map->add_img, POSITION=pos, WINDOW=cgWIN
  
  ; Title
  cgText, (pos[0]+pos[2])/2., pos[3] + 0.015, title, ALIGNMENT=0.5, $
    WINDOW=cgWIN, /NORMAL, CHARSIZE=1.5*sfac, CHARTHICK = 1.*sfac
    
  ; Bar

  if ~KEYWORD_SET(NO_BAR) then begin
    if _hcb then begin
      pbar = [pos[0] + 0.06, pos[3]+0.06, pos[2] - 0.06, pos[3]+0.08]
      map->add_color_bar, TITLE='', LABELS=bar_tags, WINDOW=cgWIN, POSITION=pbar, /RIGHT, VERTICAL=0, FORMAT=bar_format, $
        CHARSIZE=charsize *sfac, CHARTHICK = 1.* sfac, OOB_FACTOR=oob_factor, SPACING=spacing
      ; Title bar
      cgText, (pbar[0]+pbar[2])/2., pbar[3]+0.01, bar_title, ALIGNMENT=0.5, $
        WINDOW=cgWIN, /NORMAL, CHARSIZE=charsize * sfac, CHARTHICK = 1. *sfac
    endif else begin
      pbar = [pos[2] + 0.04, pos[1]+0.05, pos[2] + 0.06, pos[3]-0.05]
      map->add_color_bar, TITLE='', LABELS=bar_tags, WINDOW=cgWIN, POSITION=pbar, /RIGHT, /VERTICAL, FORMAT=bar_format, $
        CHARSIZE=charsize *sfac, CHARTHICK = 1.* sfac, OOB_FACTOR=oob_factor, SPACING=spacing
      ; Title bar
      cgText, (pbar[0]+pbar[2])/2., pbar[3]+0.025, bar_title, ALIGNMENT=0.5, $
        WINDOW=cgWIN, /NORMAL, CHARSIZE=charsize * sfac, CHARTHICK = 1. *sfac
    endelse
  endif
  
  if ~ KEYWORD_SET(NO_LEGEND) then begin
  
    if ~ KEYWORD_SET(NO_SCALE_INFO) then begin
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
        
    endif
    
    if ~KEYWORD_SET(NO_PROJ_INFO) then begin
    
      ; Projection
      proj_name = 'Projection: ' + tnt_c.proj.name
      cgText, xLegend[1] + 0.05,  yLegend + 0.02, proj_name, ALIGNMENT=0., CHARSIZE=0.7* sfac, CHARTHICK = 1.*sfac, /NORMAL, WINDOW=cgWIN
      proj_name = 'Datum: ' + tnt_c.proj.datum.name
      cgText, xLegend[1] + 0.05,  yLegend - 0.005, proj_name, ALIGNMENT=0., CHARSIZE=0.7* sfac, CHARTHICK = 1.*sfac, /NORMAL, WINDOW=cgWIN
      
    endif
    
    if ~KEYWORD_SET(NO_SOURCE_INFO) then begin
    
      ; Legend info
      if N_ELEMENTS(SOURCE_INFO) eq 0 then source_info = cgSymbol('copyright') + ' '+ $
        TIME_to_STR(QMS_TIME(), MASK='YYYY') +' TU Berlin!C!CChair of Climatology'
      if arg_okay(source_info, TYPE=IDL_STRING) then begin
        cgText, 0.80,  yLegend + 0.026, source_info, ALIGNMENT=0., CHARSIZE=0.7* sfac, CHARTHICK = 0.*sfac, /NORMAL, WINDOW=cgWIN
      endif
      
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
  
    if keyword_set(eps) then cgPS_Close
    if keyword_set(pdf) then cgPS_Close, /PDF
    if keyword_set(png) then cgPS_Close, /PNG, RESIZE=im_resize
    if keyword_set(jpeg) then cgPS_Close, /JPEG, RESIZE=im_resize
    
    disp_img = tvrd(TRUE=1)
    if do_as then disp_img = rebin(disp_img, 3, oxs, oys)
    
    if keyword_set(std_png) then write_png, std_png, disp_img
    if keyword_set(std_jpeg) then write_jpeg, std_jpeg, disp_img
    
    if xwin ne -1 then wdelete, xwin
    
  endelse
  
  !ORDER = pp
  
end
