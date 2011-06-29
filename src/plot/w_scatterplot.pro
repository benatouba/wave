;+
; :Description:
;    This is a simple routine for quick visualisation of scatter plots.
;
; :Params:
;    x: in, required, type = numeric
;       the X data
;    y: in, required, type = numeric
;       the Y data (same size as X)
;
; :Keywords:
;    XTITLE: in, optional, type = string
;            the title for X axis
;    YTITLE: in, optional, type = string
;            the title for Y axis
;    TITLE: in, optional, type = string
;            the plot title 
;    RANGE: in, optional, type = numeric
;            a two elements vector (default = [min([x,y]),max([x,y])])
;    NOFIT: in, optional 
;           if you do not want to see the linear fit         
;    NOCORRELATION: in, optional 
;                   if you do not want to see the linear correlation results (r2)
;    COLOR: in, optional, type = string
;           the cgCOLOR of the points
;    PSYM: in, optional, type = numeric
;          the psym of the points
;    LEGEND_UL: in, optional 
;               if you do not want to put the legend in the upper left corner
;               
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    IM_RESIZE: in, optional, type=integer, default=25
;                Set this keyword to percentage that the raster image file created my ImageMagick
;                 from PostScript output should be resized.
;    
;    CONST: out, optional
;           Set this keyword to a named variable that will contain the constant term of the fit. 
;    CORRELATION: out, optional
;                 Set this keyword to a named variable that will contain the linear correlation coefficient. 
;    SIGMA: out, optional
;           Set this keyword to a named variable that will contain the 1-sigma uncertainty estimates 
;           for the returned parameters
;           
; :Author: FaM
;
; :History:
;     Written by FaM, 2011.
;-
pro w_ScatterPlot, x, y, XTITLE=xtitle, YTITLE=ytitle, TITLE= title,  $
                   RANGE = range, NOFIT = nofit, NOCORRELATION = nocorrelation, COLOR = color, PSYM=psym, $
                   LEGEND_UL = LEGEND_UL, EPS = eps, PNG = png, PIXMAP = pixmap, IM_RESIZE = im_resize, $
                   CONST=const, CORRELATION=correlation, SIGMA=sigma, YFIT=yfit, GAIN=gain

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                 

  ;check args
  if ~ array_processing(x, y) then message, WAVE_Std_Message(/ARG)
  
  if ~KEYWORD_SET(range) then range = [min([x,y]),max([x,y])]
  if ~KEYWORD_SET(color) then color = 'black'
  if ~KEYWORD_SET(psym) then psym = 1
  
  ; prepare the plot 
  device, DECOMPOSED=1, RETAIN=2
  pp = !ORDER ;To restore later
  !ORDER = 0  
  xsiz = 600
  ysiz = 600  
  
  ; Check what we want to do  
  visible = ~KEYWORD_SET(PIXMAP)   
  cgDisplay, /FREE, XSIZE=xsiz, YSIZE=ysiz, /PIXMAP, Title='w_ScatterPlot'
  xwin = !D.WINDOW  
  cgWIN = FALSE    
  if visible then begin
    WDELETE, xwin
    cgWindow, WXSIZE=xsiz, WYSIZE=ysiz, Title='w_ScatterPlot resizable window'
    cgControl, EXECUTE=0
    cgWIN = true
  endif else begin
    if KEYWORD_SET(EPS) and KEYWORD_SET(PNG) then Message, 'In pixmap mode you have to choose between EPS and PNG'
    if KEYWORD_SET(EPS) then PS_START, FILENAME= eps, Decomposed=1, /Encapsulated, /Metric
    if KEYWORD_SET(PNG) then PS_START, FILENAME= png, Decomposed=1
  endelse
  
  cgPlot, x, y, title = title, CHARSIZE=1.5, /NORMAL, $
    CHARTHICK = 1, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [0.14,0.12,0.94,0.92], /NODATA, YSTYLE = 1, xstyle = 1, $
       XRANGE=range, YRANGE=range, PSYM=psym, WINDOW=cgWin      
    
  cgplots, [range[0],range[1]], [range[0],range[1]], color = cgColor('grey'), LINESTYLE=5, WINDOW=cgWin
  
  cgplot, x, y,  COLOR=cgColor(color), PSYM=psym, /OVERPLOT, WINDOW=cgWin
  
  gain = regress(x, y, CONST=const, CORRELATION=correlation, SIGMA=sigma, YFIT=yfit)

  if ~KEYWORD_SET(NOFIT) then begin  

    cgPlots, range, const[0] + gain[0] * range, color = cgColor('dark grey'), NOCLIP = 0, WINDOW=cgWin    
    
    if const lt 0 then sign = '- ' else sign = '+ ' 
    sq_sign = String(108B)
       
    text = 'y = ' + STRING(gain, FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(const), FORMAT='(F6.2)')        
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.85] $
    else  pos = [0.5, 0.22]
    
    cgText, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, WINDOW=cgWin 
        
  endif
  
  if ~KEYWORD_SET(NOCORRELATION) then begin
  
    r2 = correlation*correlation
    text = 'r'+ STRING(178B) +' = ' + STRING(r2, FORMAT='(F5.2)')    
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.80] $
    else  pos = [0.5, 0.17]
    cgtext, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, WINDOW=cgWin
    
  endif
  
  if ~KEYWORD_SET(NOBIAS) then begin
  
    mb = mean(y - x)
    text = 'Bias = ' + STRING(mb, FORMAT='(F5.2)')    
    if KEYWORD_SET(LEGEND_UL) then pos = [0.38, 0.80] $
    else  pos = [0.68, 0.17]
    cgtext, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, WINDOW=cgWin
    
  endif
  
  ; output  
  if visible then begin
    cgControl, EXECUTE=1
    if KEYWORD_SET(PNG) then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
    if KEYWORD_SET(EPS) then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  endif else begin
    if KEYWORD_SET(PNG) then PS_END, /PNG, resize = im_resize
    if KEYWORD_SET(EPS) then PS_END
    WDELETE, xwin
  endelse

  !ORDER = pp
    
end