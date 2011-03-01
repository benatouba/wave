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
                   LEGEND_UL = LEGEND_UL

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
  
  cgWindow, WXSize = 600, WYSize = 600, WTITLE = 'WScatter_plot resizable window'
  cgControl, Execute=0
  
  cgPlot, x, y, title = title, CHARSIZE=1.5, /NORMAL, $
    CHARTHICK = 1, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [0.14,0.12,0.94,0.92], /NODATA, YSTYLE = 1, xstyle = 1, $
       XRANGE=range, YRANGE= range, PSYM=psym, /WINDOW      
    
  cgplots, [range[0],range[1]], [range[0],range[1]], color = cgColor('grey'), LINESTYLE=5, /WINDOW
  
  cgplot, x, y,  COLOR=cgColor(color), PSYM=psym, /OVERPLOT, /WINDOW
    
  if ~KEYWORD_SET(NOFIT) then begin  
    l = LINFIT(x, y, /DOUBLE)    
    cgPlots, range, l[0] + l[1] * range, color = cgColor('dark grey'), NOCLIP = 0, /WINDOW    
    
    if l[0] lt 0 then sign = '- ' else sign = '+ ' 
    sq_sign = String(108B)
       
    text = 'y = ' + STRING(l[1], FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(l[0]), FORMAT='(F6.2)')        
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.85] $
    else  pos = [0.5, 0.22]
    
    cgText, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, /WINDOW 
        
  endif
  
  if ~KEYWORD_SET(NOCORRELATION) then begin
    r2 = CORRELATE(x,y)
    r2 = r2*r2
    text = 'r'+ STRING(178B) +' = ' + STRING(r2, FORMAT='(F5.2)')
    
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.80] $
    else  pos = [0.5, 0.17]
    cgtext, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, /WINDOW
  endif
  
  if ~KEYWORD_SET(NOBIAS) then begin
    mb = mean(y - x)

    text = 'Bias = ' + STRING(mb, FORMAT='(F5.2)')
    
    if KEYWORD_SET(LEGEND_UL) then pos = [0.35, 0.80] $
    else  pos = [0.68, 0.17]
    cgtext, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, /WINDOW
  endif
  cgControl, Execute=1  
  !ORDER = pp
    
end