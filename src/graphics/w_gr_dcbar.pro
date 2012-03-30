;+
; :Description:
;    WAVE wrapper for the cgDCBar routine. Since a large
;    part of the job is done internaly, many color and level
;    related keywords are not available anymore for simplification.
;    Some new default values are set by the WAVE, like vertical
;    for example.
;
; :Params:
;    info: in, required
;          output from `w_gr_DataLevels`
;    
; :Keywords:
;    addcmd: in, optional, type=boolean, default=0
;       Set this keyword to add the command to the resizeable graphics window cgWindow.
;    barcolor: in, optional, type=string
;       This is the name of a color known to cgCOLOR that can be used to draw the color 
;       bar outlines. By default, the same as specified with the COLOR keyword.
;    charsize: in, optional, type=float
;       The character size of the color bar annotations. Default is cgDefCharsize()*charPercent.
;    color: in, optional, type=string, default="opposite"
;        The name of the color to use for color bar annotations. 
;    fit: in, optional, type=boolean, default=0
;       If this keyword is set, the colorbar "fits" itself to the normalized
;       coordinates of the last plot command executed. In other words, for
;       a horizontal color bar, postition[[0,2]] = !X.Window, and for a vertical
;       color bar, position[[1,3]] = !Y.Window. Other positions are adjusted
;       to put the colorbar "reasonably" close to the plot.
;    font: in, optional, type=integer, default=!P.Font
;       Sets the font of the annotation. Hershey: -1, Hardware:0, True-Type: 1.
;    format: in, optional, type=string
;            the string format to apply to the label fromatting
;    labels: in, optional, type=string
;       An array of string labels that should annotate each color. Must be the same length
;       as the colors vector. Colors are labelled consecutively by default.
;    position: in, optional, type=float          
;       A four-element array of normalized coordinates in the same
;       form as the POSITION keyword on a plot. Default is[0.88, 0.10, 0.95, 0.90] 
;       for a vertical bar and [0.10, 0.88, 0.90, 0.95] for a horizontal bar.
;       See the FIT keyword, also.
;    right: in, optional, type=boolean, default=0   
;       This puts the labels on the right-hand side of a vertical color bar. It applies 
;       only to vertical color bars.
;    rotate: in, optional, type=float, default=0.0
;        Set this keyword to a value that will rotate the label text.
;        Positive values between 0 and 180 degrees rotate in a counter-clockwise
;        sense. Negative values between 0 and 180 degress rotate in a 
;        clockwise sense.
;    spacing: in, optional, type=float, default=1.0
;        When labels are rotated, it is a little difficult to determine where,
;        exactly, they should be located. This keyword gives the user some control
;        over this location. The location "spacer" is multiplied by this amount. 
;        So, for example, to move the labels a little further away from the color bar, 
;        make this number greater than 1 (e.g, 1.25). To move the labels a little closer, 
;        use a number less than 1 (e.g, 0.75).
;    tcharsize: in, optional, type=float
;        The character size of the title. By default, same as CHARSIZE.
;    title: in, optional, type=string, default=""
;       This is title for the color bar. The default is to have no title.
;    vertical: in, optional, type=boolean, default=1
;       Set this keyword to 0 to give a horizontal color bar.
;    window: in, optional, type=boolean, default=0               
;       Set this keyword to display the plot in a resizeable graphics window (cgWindow).
;            
;-
pro w_gr_DCBar, info, $
    ADDCMD=addcmd, $
    BARCOLOR=barcolor, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FONT=font, $
    FIT=fit, $
    FORMAT=format, $
    LABELS=labels, $
    POSITION=position, $
    RIGHT=right, $
    ROTATE=rotate, $
    SPACING=spacing, $
    TCHARSIZE=tcharsize, $
    TITLE=title, $
    VERTICAL=vertical, $
    WINDOW=window
    
  @WAVE.inc
  compile_opt idl2
  
  DEVICE, DECOMPOSED=1
  
  if N_ELEMENTS(VERTICAL) eq 0 then VERTICAL=1
  
  if ~arg_okay(info, /STRUCT) then Message, '$INFO should be a structure.'
  if ~ tag_exist(info, 'dcbar') then  Message, '$INFO should contain a "DCBAR" tag.'
  if ~ info.dcbar then Message, 'You sure? $INFO does not seem to agree with you choice of w_gr_DCBar. Use w_gr_Colorbar instead.'
  
 
  if ~ tag_exist(info, 'colors') then  Message, '$INFO should contain a "COLORS" tag.'
  
  colors = info.colors
  
  if info.is_Missing then begin
    neutral=colors[0]
    colors = colors[1:*]
  endif
 
  n_colors = N_ELEMENTS(colors)
  
  if n_colors lt 1 then Message, 'Less than one color?'
  
  if N_ELEMENTS(labels) ne 0 then begin
    if N_ELEMENTS(labels) ne n_colors then Message, 'LABELS has not the right number of elements. Expected ' + $
      str_equiv(n_colors) + ', got ' + str_equiv(N_ELEMENTS(labels))      
  endif else begin
    if ~ tag_exist(info, 'levels') then  Message, '$INFO should contain a "LEVELS" tag.'
    levels = info.levels
    if N_ELEMENTS(FORMAT) ne 0 then labels = STRING(levels, FORMAT=format) else labels = w_str(levels)  
  endelse
  
  cgDCBar, colors, $
    ADDCMD=addcmd, $
    BARCOLOR=barcolor, $
    CHARSIZE=charsize, $
    COLOR=color, $
    FONT=font, $
    FIT=fit, $
    LABELS=labels, $
    POSITION=position, $
    RIGHT=right, $
    ROTATE=rotate, $
    SPACING=spacing, $
    TCHARSIZE=tcharsize, $
    TITLE=title, $
    VERTICAL=vertical, $
    WINDOW=window
    
end