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
; :Author: FaM
;
; :History:
;     Written by FaM, 2011.
;-
pro w_ScatterPlot, x, y, XTITLE=xtitle, YTITLE=ytitle, TITLE= title,  $
                   RANGE = range, NOFIT = nofit, NOCORRELATION = nocorrelation, COLOR = color, PSYM=psym, $
                   LEGEND_UL = LEGEND_UL, EPS = eps, PNG = png, PIXMAP = pixmap

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

   ; prepare the plot 
   device, DECOMPOSED=1, RETAIN=2
   pp = !ORDER ;To restore later
   !ORDER = 0
  
  ;check args
  if ~ array_processing(x, y) then message, WAVE_Std_Message(/ARG)
  
  if ~KEYWORD_SET(range) then range = [min([x,y]),max([x,y])]
  if ~KEYWORD_SET(color) then color = 'black'
  if ~KEYWORD_SET(psym) then psym = 1
  
  ; Check what we want to do
  if (KEYWORD_SET(EPS) or KEYWORD_SET(PNG)) and KEYWORD_SET(NO_RESIZE) then PIXMAP = TRUE  
  if KEYWORD_SET(PIXMAP) then visible = FALSE else visible = TRUE
  
  xsiz = 600
  ysiz = 600
  cgDisplay, /FREE, XSIZE=xsiz, YSIZE=ysiz, /PIXMAP, Title='WAVE Plot'
  xwin = !D.WINDOW
  
  cgWIN = FALSE    
  if visible and ~KEYWORD_SET(NO_RESIZE) then begin
    WDELETE, xwin
    cgWindow, WXSIZE=xsiz, WYSIZE=ysiz, Title='w_ScatterPlot resizable window'
    cgControl, EXECUTE=0
    cgWIN = true
  endif else begin
    if KEYWORD_SET(EPS) then PS_START, FILENAME= eps, Decomposed=1 $
    else if KEYWORD_SET(PNG) then PS_START, FILENAME= png, Decomposed=1
  endelse
  
  cgPlot, x, y, title = title, CHARSIZE=1.5, /NORMAL, $
    CHARTHICK = 1, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [0.14,0.12,0.94,0.92], /NODATA, YSTYLE = 1, xstyle = 1, $
       XRANGE=range, YRANGE= range, PSYM=psym, WINDOW=cgWin      
    
  cgplots, [range[0],range[1]], [range[0],range[1]], color = cgColor('grey'), LINESTYLE=5, WINDOW=cgWin
  
  cgplot, x, y,  COLOR=cgColor(color), PSYM=psym, /OVERPLOT, WINDOW=cgWin
    
  if ~KEYWORD_SET(NOFIT) then begin  
    l = LINFIT(x, y, /DOUBLE)    
    cgPlots, range, l[0] + l[1] * range, color = cgColor('dark grey'), NOCLIP = 0, WINDOW=cgWin    
    
    if l[0] lt 0 then sign = '- ' else sign = '+ ' 
    sq_sign = String(108B)
       
    text = 'y = ' + STRING(l[1], FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(l[0]), FORMAT='(F6.2)')        
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.85] $
    else  pos = [0.5, 0.22]
    
    cgText, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, WINDOW=cgWin 
        
  endif
  
  if ~KEYWORD_SET(NOCORRELATION) then begin
    r2 = CORRELATE(x,y)
    r2 = r2*r2
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
    if CGWIN then cgControl, EXECUTE=1 else begin
      img = Transpose(tvrd(/TRUE), [1,2,0])
      WDELETE, xwin
      cgDisplay, xsiz, ysiz, /FREE, Title='w_ScatterPlot window'
      cgImage, img
    endelse
    if KEYWORD_SET(PNG) then cgControl, CREATE_PNG=png, IM_RESIZE= 50, /IM_RASTER
    if KEYWORD_SET(EPS) then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  endif else begin
    if KEYWORD_SET(PNG) then PS_END, /PNG, resize = RESIZE_PNG
    if KEYWORD_SET(EPS) then PS_END
    WDELETE, xwin
  endelse

  !ORDER = pp
    
end