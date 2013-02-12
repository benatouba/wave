;+
; :Description:
;    Generates scatterplots for validation purposes. For quick 
;    visualisation purposes, use cgScatterPlot.
;
; :Params:
;    x: in, required
;       the X data array
;    y: in, required
;       the Y data array (same size as X)
;
; :Keywords:
;    XTITLE: in, optional, type=string
;            the title for X axis
;    YTITLE: in, optional, type=string
;            the title for Y axis
;    TITLE: in, optional, type=string
;            the plot title 
;    RANGE: in, optional
;            a two elements vector (default = [min([x,y]),max([x,y])])
;    FIT: in, optional, DEFAULT=0
;         if you want to add a linear fit         
;    CORRELATION: in, optional, DEFAULT=1
;                 if you do want to see the linear correlation results (r2)
;    MD: in, optional, DEFAULT=1
;        if you do want to see the MD results
;    MAD: in, optional, DEFAULT=1
;                 if you do want to see the MAD results
;    COLOR: in, optional, default='black'
;           the cgCOLOR of the points
;    PSYM: in, optional, default=1
;          the cgSymCat of the points
;    S_CHARSIZE: in, optional
;                score charsize
;    SCORE_POS: in, optional
;               [X0, Y0] array for positioning the scores 
;           
; :History:
;     Written by FaM, 2013.
;-
pro w_ScatterPlot, x, y, $
  XTITLE=xtitle, $
  YTITLE=ytitle, $
  TITLE=title, $
  RANGE=range, $
  FIT=fit, $
  CORRELATION=correlation, $
  MD=md, $
  MAD=mad, $
  COLOR=color, $
  PSYM=psym, $
  SCORE_POS=score_pos, $
  ADDCMD=addcmd, $
  S_CHARSIZE=s_charsize, $
  _REF_EXTRA=extra

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                 

  ;check args
  if ~ array_processing(x, y, REP_A0=_x, REP_A1=_y) then message, WAVE_Std_Message(/ARG)
  
  SetDefaultValue, fit, 0, /BOOLEAN
  SetDefaultValue, correlation, 1, /BOOLEAN
  SetDefaultValue, md, 1, /BOOLEAN
  SetDefaultValue, mad, 1, /BOOLEAN
  SetDefaultValue, color, 'black'
  SetDefaultValue, psym, 1
  
  ;Data
  pok = where(FINITE(_x + _y), cntok)
  if cntok eq 0 then Message, 'no valid elements in x or y'
  _x = _x[pok]
  _y = _y[pok]
  
  if N_ELEMENTS(range) eq 0 then range = [min([_x,_y]),max([_x,_y])]
  
  ; Empty plot
  cgPlot, _x, _y, TITLE=title, XTITLE=xtitle, YTITLE=ytitle, $
            XRANGE=range, YRANGE=range, $
            /NODATA, ADDCMD=addcmd, _EXTRA=extra
    
  ; Grey one to one line
  cgplots, [-1e5,1e5], [-1e5,1e5], color='grey', LINESTYLE=5, NOCLIP=0, ADDCMD=addcmd
  
  ; The points
  cgplot, _x, _y,  COLOR=color, PSYM=psym, /OVERPLOT, ADDCMD=addcmd
    

  if KEYWORD_SET(FIT) then begin  
    Message, 'Fit temporarily non-available'
;    gain = regress(_x, _y, CONST=const, CORRELATION=correlation, SIGMA=sigma, YFIT=yfit)
;    cgPlots, [-1e7,1e7], const[0] + gain[0] * [-1e7,1e7], color=cgColor('dark grey'), NOCLIP=0, WINDOW=cgWin    
;    
;    if const lt 0 then sign = '- ' else sign = '+ ' 
;    sq_sign = String(108B)
;       
;    text = 'y = ' + STRING(gain, FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(const), FORMAT='(F6.2)')        
;    if KEYWORD_SET(LEGEND_UL) then pos = [0.2, 0.85] $
;    else  pos = [0.5, 0.22]
;    
;    cgText, pos[0], pos[1], text, CHARSIZE= 1.8, COLOR=cgColor('black'), CHARTHICK=1., /NORMAL, WINDOW=cgWin 
        
  endif
  
  if KEYWORD_SET(CORRELATION) then begin
    tt = CORRELATE(_x, _y)^2
    text = 'r!U2!N= ' + cgNumber_Formatter(tt, DECIMALS=2)
    if N_ELEMENTS(legtext) eq 0 then legtext = text else legtext = [legtext, text]
  endif
  
  if KEYWORD_SET(MD) then begin
    tt = w_MD(_x, _y)
    text = 'MD = ' + cgNumber_Formatter(tt, DECIMALS=2)
    if N_ELEMENTS(legtext) eq 0 then legtext = text else legtext = [legtext, text]
  endif
  
  if KEYWORD_SET(MAD) then begin
    tt = w_MAD(_x, _y)
    text = 'MAD = ' + cgNumber_Formatter(tt, DECIMALS=2)
    if N_ELEMENTS(legtext) eq 0 then legtext = text else legtext = [legtext, text]
  endif
  
  if N_ELEMENTS(legtext) ne 0 then begin
    al_legend, legtext, BOX=0, POSITION=score_pos, CHARSIZE=s_charsize, WINDOW=addcmd, _EXTRA=extra
  endif
    
end