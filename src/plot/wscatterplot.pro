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
;           the FSC_COLOR of the points
;    PSYM: in, optional, type = numeric
;          the psym of the points
;    LEGEND_UL: in, optional 
;               if you do not want to put the legend in the upper left corner
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2011.
;
;       Modified::
;
;-
pro WScatterPlot, x, y, XTITLE=xtitle, YTITLE=ytitle, TITLE= title,  $
                   RANGE = range, NOFIT = nofit, NOCORRELATION = nocorrelation, COLOR = color, PSYM=psym, $
                   LEGEND_UL = LEGEND_UL

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

   ; prepare the plot 
   device, DECOMPOSED=0, RETAIN=2 ;, SET_FONT='Courier Italic', /TT_FONT  
   pp = !ORDER ;To restore later
   !ORDER = 0
  
  ;check args
  if ~ array_processing(x, y) then message, WAVE_Std_Message(/ARG)
  
  if ~KEYWORD_SET(range) then range = [min([x,y]),max([x,y])]
  if ~KEYWORD_SET(color) then color = 'black'
  if ~KEYWORD_SET(psym) then psym = 1
  
  FSC_Window, 'FSC_Plot', x, y, title = title, CHARSIZE=1.8, /NORMAL, $
    CHARTHICK = 1.3, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [0.14,0.12,0.94,0.92], /NODATA, YSTYLE = 1, xstyle = 1, $
       WXSize = 600, WYSize = 600, WTITLE = 'WScatter_plot resizable window', XRANGE=range, YRANGE= range, PSYM=psym
 
  FSC_plots, [range[0],range[1]], [range[0],range[1]], color = FSC_Color('grey'), LINESTYLE=5, /WINDOW
  
  FSC_plot, x, y,  COLOR=FSC_Color(color), PSYM=psym, /OVERPLOT, /WINDOW
  
  if ~KEYWORD_SET(NOFIT) then begin  
    l = LINFIT(x, y, /DOUBLE)    
    FSC_Plots, range, l[0] + l[1] * range, color = FSC_Color('dark grey'), NOCLIP = 0, /WINDOW    
    
    if l[0] lt 0 then sign = '- ' else sign = '+ ' 
    sq_sign = String(108B)
       
    text = 'y = ' + STRING(l[1], FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(l[0]), FORMAT='(F6.2)')        
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.85] $
    else  pos = [0.5, 0.22]
    
    FSC_Text, pos[0], pos[1], text, CHARSIZE= 2., COLOR=FSC_Color('black'), CHARTHICK=1., /NORMAL, /WINDOW 
        
  endif
  
  if ~KEYWORD_SET(NOCORRELATION) then begin
    r2 = CORRELATE(x,y)
    r2 = r2*r2
    text = 'r'+ STRING(178B) +' = ' + STRING(r2, FORMAT='(F5.2)')
    
    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.80] $
    else  pos = [0.5, 0.17]
    FSC_text, pos[0], pos[1], text, CHARSIZE= 2., COLOR=FSC_Color('black'), CHARTHICK=1., /NORMAL, /ADDCMD
  endif

  !ORDER = pp
    
end