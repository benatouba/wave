;+
; :Description:
;    WAVE wrapper for the cgColorbar routine. 
;    Since a large part of the job is done internaly, many color- and level-
;    related keywords are not available anymore for simplification.
;    Some new default values are set by the WAVE, like VERTICAL
;    for example.
;
; :Params:
;    info: in, required
;          output from `w_gr_DataLevels`
;    
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    charpercent: in, optional, type=float, default=0.85                 
;       A value from 0.0 go 1.0 that is multiplied by the CHARSIZE to produce
;       the character size for the color bar. This value is only used if CHARSIZE is 
;       undefined. This keyword is primarily useful for using color bars in resizeable 
;       graphics windows (cgWindow).
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    color: in, optional, type=string
;        The name of the color to use for color bar annotations. Ignored unless passed 
;        the name of a cgColor color.
;    discrete: in, optional, type=boolean, default=0
;         Force colorbar to be discrete. Default is to be discrete when less then 25
;         levels are defined.
;    divisions: in, optional, type=integer
;         The number of divisions to divide the bar into. There will
;         be (divisions + 1) annotations. The default is 0 if using the
;         default color bar formatting, which allows the plot command to 
;         determine how many divisions to make. Otherwise, if you are specifying
;         some other format for the tick labels, the default number of divisions
;         is six.
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar "fits" itself to the normalized
;       coordinates of the last plot command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot.
;    font: in, optional, type=integer, default=!P.Font
;       Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;    format: in, optional, type=string, default=""
;       The format of the color bar annotations. Default is "", which allows
;       the Plot command to determine the appropriate format.
;    minor: in, optional, type=integer, default=2
;       The number of minor tick divisions. 
;    oob_factor: in, optional, type=float, default=1.0
;       The default is to make the length of the out-of-bounds triangle the
;       same distance as the height (or width, in the case of a vertical
;       color bar) of the color bar. If you would prefer a shorted triangle length, 
;       set this keyword to a value less than zero (e.g., 0.5). If you prefer a 
;       longer length, set this keyword to a value greater than zero. The "standard"
;       length will be multiplied by this value.
;    position: in, optional, type=float          
;       A four-element array of normalized coordinates in the same
;       form as the POSITION keyword on a plot. Default is[0.88, 0.10, 0.95, 0.90] 
;       for a vertical bar and [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;       See the FIT keyword, also.
;    right: in, optional, type=boolean, default=1   
;       This puts the labels on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    ticklen: in, optional, type=float, default=0.25
;       Set this keyword to the major tick length desired. Default is 0.25. Setting this 
;       keyword to a value greater than or equal to 0.5 will result in major tick marks 
;       extending the width of the color bar. Note that setting this keyword to 0.3 or
;       greater will result in minor tick mark lengths being set to 0.01, which is almost 
;       too small to be seen. All direct graphics tick marks act in this (strange!) way.
;    ticknames: in, optional, type=string                 
;       A string array of names or values for the color bar tick marks.
;    tickinterval: in, optional                 
;       colorbar tick interval
;    title: in, optional, type=string, default=""
;       This is title for the color bar. The default is to have no title.
;    tlocation: in, optional, type=string
;       The title location, which allows the user to set the title location independently 
;       of the colorbar labels. May be "TOP" or "BOTTOM" for horizontal color bars, and
;       "LEFT" or "RIGHT" for vertical color bars.
;    top: in, optional, type=boolean, default=0
;       This puts the labels on top of the bar rather than under it. The keyword only 
;       applies if a horizontal color bar is rendered.
;    vertical: in, optional, type=boolean, default=1
;       Set this keyword to 0 to give a horizontal color bar.
;    window: in, optional, type=boolean, default=0               
;       Set this keyword to display the plot in a resizeable graphics window (cgWindow).
;    _ref_extra: in, optional
;         Any keyword appropriate for the PLOT and AXIS commands is also accepted by keyword
;         inheritance.
; 
;-
pro w_gr_Colorbar, info, $
    ADDCMD=addcmd, $
    CHARPERCENT=charpercent, $
    CHARSIZE=charsize, $
    COLOR=color, $
    DISCRETE=discrete, $
    DIVISIONS=divisions, $
    FIT=fit, $
    FONT=font, $
    FORMAT=format, $
    MINOR=minor, $
    OOB_FACTOR=oob_factor, $
    POSITION=position, $
    RIGHT=right, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TICKINTERVAL=tickinterval, $
    TITLE=title, $
    TOP=top, $
    TLOCATION=tlocation, $
    VERTICAL=vertical, $
    WINDOW=window, $
    _EXTRA=extra
    
    
  @WAVE.inc
  compile_opt idl2
  
  ; Get the current color table vectors.
  TVLCT, rr, gg, bb, /GET
  
  DEVICE, DECOMPOSED=1
  
  if N_ELEMENTS(VERTICAL) eq 0 then VERTICAL=1
  if N_ELEMENTS(RIGHT) eq 0 then RIGHT=1
  if N_ELEMENTS(DIVISIONS) eq 0 then divicolorsions=0
  
  if ~arg_okay(info, /STRUCT) then Message, '$INFO should be a structure.'
  if ~ tag_exist(info, 'dcbar') then  Message, '$INFO should contain a "DCBAR" tag.'
  if info.dcbar then Message, 'You sure? $INFO does not seem to agree with you choice of w_gr_Colorbar. Use w_gr_DCBar instead.'
  
  if ~ tag_exist(info, 'colors') then  Message, '$INFO should contain a "COLORS" tag.'
  colors = info.colors
  
  if info.is_Missing then begin
    neutral=colors[0]
    colors = colors[1:*]
  endif
  if info.is_ooBotColor then begin
    oob_low=colors[0]
    colors = colors[1:*]
  endif
  if info.is_ooTopColor then begin
    oob_high = colors[N_ELEMENTS(colors)-1]
    colors = colors[0:N_ELEMENTS(colors)-2]
  endif
  
  ncolors = N_ELEMENTS(colors)
  if ncolors eq 3 then row=1
  palette = w_gr_ColorToRGB(colors, ROW=row)  
  if ncolors lt 1 then Message, 'Less than one color?'
  
  
  if ~ tag_exist(info, 'levels') then  Message, '$INFO should contain a "LEVELS" tag.'
  levels = info.levels
  
  _discrete = 0
  if N_ELEMENTS(DISCRETE) eq 0 then $
   if N_ELEMENTS(levels) lt 25 then _discrete = 1 $
    else _discrete = KEYWORD_SET(DISCRETE)
  
  if _discrete then begin
    if N_ELEMENTS(FORMAT) ne 0 then ticknames = STRING(levels, FORMAT=format) else ticknames = cgNumber_Formatter(levels)
    divisions = ncolors
  endif else begin
    range = utils_minmax(levels)
    if info.is_hist then begin
      ; So color is allways n_levels-1 
      my_range = utils_minmax(levels[1:ncolors])
      reg_levels = cgScaleVector(FINDGEN(ncolors), my_range[0], my_range[1]) 
      palette[*,0] = FIX(0 > INTERPOL(float(palette[*,0]), levels[1:ncolors], reg_levels) < 255)
      palette[*,1] = FIX(0 > INTERPOL(float(palette[*,1]), levels[1:ncolors], reg_levels) < 255)
      palette[*,2] = FIX(0 > INTERPOL(float(palette[*,2]), levels[1:ncolors], reg_levels) < 255)
    endif
  endelse
  

  cgColorbar, $
    ADDCMD=addcmd, $
    ANNOTATECOLOR=annotatecolor, $
    CHARPERCENT=charpercent, $
    CHARSIZE=charsize, $
    CLAMP=clamp, $
    COLOR=color, $
    DISCRETE=_discrete, $
    DIVISIONS=divisions, $
    FIT=fit, $
    FONT=font, $
    FORMAT=format, $
    MINOR=minor, $
    NCOLORS=ncolors, $
    OOB_FACTOR=oob_factor, $
    OOB_HIGH=oob_high, $
    OOB_LOW=oob_low, $
    PALETTE=palette, $
    POSITION=position, $
    RANGE=range, $
    RIGHT=right, $
    TICKLEN=ticklen, $
    TICKNAMES=ticknames, $
    TICKINTERVAL=tickinterval, $
    TITLE=title, $
    TOP=top, $
    TLOCATION=tlocation, $
    VERTICAL=vertical, $
    XLOG=xlog, $
    YLOG=ylog, $
    WINDOW=window, $
    _REF_EXTRA=extra
  
  ; Set the current colors back.
  TVLCT, rr, gg, bb
end