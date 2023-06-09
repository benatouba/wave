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
;    BINSIZE=binsize, MIN=min, MAX=max: in, optional
;             if you want to do binned scatterplots
;           
; :History:
;     Written by FaM, 2013.
;-
pro w_ScatterPlot, x, y, $
  XTITLE=xtitle, $
  YTITLE=ytitle, $
  TITLE=title, $
  RANGE=range, $
  XRANGE=xrange, $
  YRANGE=yrange, $
  FIT=fit, $
  CORRELATION=correlation, $
  MD=md, $
  MAD=mad, $
  POSITION=position, $
  COLOR=color, $
  PSYM=psym, $
  SCORE_POS=score_pos, $
  ADDCMD=addcmd, $
  SYMSIZE=symsize, $
  S_CHARSIZE=s_charsize, $
  BINSIZE=binsize, MIN=min, MAX=max, $
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
  
  do_bins = 0
  if N_ELEMENTS(BINSIZE) ne 0 or N_ELEMENTS(MIN) ne 0 or N_ELEMENTS(MAX) ne 0 then begin
    do_bins = 1
    h = HISTOGRAM(_x, BINSIZE=binsize, MIN=min, MAX=max, LOCATIONS=locs, REVERSE_INDICES=ri)
    _bx = locs + (locs[1]-locs[0])/2
    _bmean = float(h*0)
    _bstddev = float(h*0)
    for i=0, N_ELEMENTS(h)-1 do begin
      if ri[i] eq ri[i+1] then continue
      top = _y[ri[ri[i]:ri[i+1]-1]]
      _bmean[i] = MEAN(top)
      _bstddev[i] = STDDEV(top)
    endfor
    _by = _bmean
    color = 'light grey'
  endif

  if N_ELEMENTS(range) eq 0 then begin
    _xrange = [min([_x,_y]),max([_x,_y])]
    _yrange = [min([_x,_y]),max([_x,_y])]    
  endif else begin
    _xrange = range
    _yrange = range
  endelse
  
  if N_ELEMENTS(Xrange) ne 0 then _xrange = Xrange
  if N_ELEMENTS(Yrange) ne 0 then _yrange = Yrange
  
  ; Empty plot
  cgPlot, _x, _y, TITLE=title, XTITLE=xtitle, YTITLE=ytitle, $
            XRANGE=_xrange, YRANGE=_yrange, POSITION=position, $
            /NODATA, ADDCMD=addcmd, _EXTRA=extra, Aspect=1., /NOERASE
    
  ; The points
  cgplot, _x, _y,  COLOR=color, PSYM=psym, /OVERPLOT, ADDCMD=addcmd, NOCLIP=0, symsize=symsize

  ; Grey one to one line
  cgplots, [-1e5,1e5], [-1e5,1e5], color='grey', LINESTYLE=5, NOCLIP=0, ADDCMD=addcmd
  
  ;if histo
  if do_bins then begin
   cgPlot, _bx, _by, Color='black', /OVERPLOT, ADDCMD=addcmd
   cgErrPlot, _bx, _by-_bstddev, _by+_bstddev, Color='black', ADDCMD=addcmd
  endif
  
  ; Restore plot
;  Axis, XAXIS=0, XRANGE=_xrange;, ADDCMD=addcmd, /SAVE
;  Axis, YAXIS=0, YRANGE=_yrange;, ADDCMD=addcmd, /SAVE
  
  if KEYWORD_SET(FIT) then begin  
    Message, 'Fit temporarily non-available'        
  endif
  
  if KEYWORD_SET(CORRELATION) then begin
    tt = CORRELATE(_x, _y)
    text = 'r= ' + cgNumber_Formatter(tt, DECIMALS=2)
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
    al_legend, legtext, BOX=0, POSITION=score_pos, CHARSIZE=s_charsize, WINDOW=addcmd, /DATA
  endif
  

    
end