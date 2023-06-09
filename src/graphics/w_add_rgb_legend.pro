;+
; 
; Adds a RGB colorcircle legend to the current device.
;  
; :Author: 
;   JaH
;-

; :Description:
;    This function calculates the rbg color values for a certain angle and radius (or an array of angle and radius)
;    of a rainbow-like color circle. The top of the circle is green, the lower right side is blue and the lower left side is red.
;    The output is a 3-dim. (to 4dim., for array of agnle ansd radius) array containing the color values for [red, green, blue] for the angle and radius.
;
; :Parameters:
;    angle: in, required, value (or array of values) between 0 and 360 degrees, the 0 (resp. 360 value) lies horizontally on the right side of the circle
;    radius: in, required, value (or array of values) between 0 and 100, 0 marking the center of the circle

function w_add_RGB_legend_make_color, angle, radius 

  ;--------------------
  ; Set up environment
  ;--------------------
  compile_opt idl2
  
  ; create angle
  tick_coords = [0  , 60 , 120, 180, 240, 300, 360]
  ticks_r     = [0  , 0  , 0  , 255, 255, 255, 0  ]
  ticks_g     = [255, 255, 0  , 0  , 0  , 255, 255]
  ticks_b     = [0  , 255, 255, 255, 0  , 0  , 0  ]
  
  ; calculate r,g,b for certain angle
  r = interpol(ticks_r, tick_coords, angle)
  g = interpol(ticks_g, tick_coords, angle)
  b = interpol(ticks_b, tick_coords, angle)
  
 ; create radius
  rad = [0., 100.]
  centre = 255
  rad_r = [centre, r]
  rad_g = [centre, g]
  rad_b = [centre, b]
  
  ; calculate r,g,b for certain angle at any point within the cirlce
  r = interpol(rad_r, rad, radius)
  g = interpol(rad_g, rad, radius)
  b = interpol(rad_b, rad, radius)
  
 return, [r, g, b]
 
end


; :Description:
;    This function uses the w_add_RGB_legend_make_color function to create a full color circle.
;    By setting the keyword RAD to a certain value, the size of the circle can be varied.
;
; :Params:
;    rad: in, required, radius of the circle 
;         set this keyword to vary the resolution of the color circle

function w_add_RGB_legend_make_color_circle, rad
  ;--------------------
  ; Set up environment
  ;--------------------
  compile_opt idl2
  common w_add_RGB_legend_ADMIN, circle_rad, colorcircle
  
  if N_ELEMENTS(rad) eq 0 then Message, 'rad must be set'
  
  redo = N_ELEMENTS(circle_rad) eq 0
  if ~ redo then redo = circle_rad ne rad
  
  if redo then begin
    circle_rad = rad
    s = 2.*circle_rad+1.
    center = s/2.
    
    ; create basic color = white
    r = BYTARR(s,s) + 255
    g = BYTARR(s,s) + 255
    b = BYTARR(s,s) + 255
    
    ; create angle and radius
    radius = FLTARR(s,s)
    angle = FLTARR(s,s)
    is = INDGEN(s) # (INTARR(s)+1) - center
    js = (INTARR(s)+1) # INDGEN(s) - center
    rectang_coord = TRANSPOSE([[is[*]],[js[*]]])
    test = CV_COORD(FROM_RECT=rectang_coord, /TO_POLAR)
    radius[*] = test[1,*] * 100./circle_rad
    angle[*] = test[0,*]
    angle = ROTATE(ABS(angle - !PI),3) * 180. / !PI
    
    ; generate color circle
    pok = where(radius le 100, cntok)
    for i=0, cntok-1 do begin
      ind = pok[i]
      r_g_b = w_add_RGB_legend_make_color(angle[ind], radius[ind])
      r[ind] = r_g_b[0]
      g[ind] = r_g_b[1]
      b[ind] = r_g_b[2]
    endfor
    colorcircle= bytarr(s,s,3)
    colorcircle[*,*,0]=r
    colorcircle[*,*,1]=g
    colorcircle[*,*,2]=b
    
  endif
  
  return, colorcircle
  
end


;+
; :Description:
;    Adds a RGB colorcircle legend to the current device.
;
; :Keywords:
;    ADDCMD : in , optional
;             set this keyword to add the color circle to the current figure
;    DATA : in, optional
;           Set this keyword to indicate that the clipping and/or positioning 
;           coordinates supplied are specified in the data coordinate system
;    NORMAL : in, optional, default = 1
;             Set this keyword to indicate that the clipping and/or positioning 
;             coordinates supplied are specified in the normalized coordinate system
;    POSITION : in, optional, default = [0.5,0.5],
;               set this keyword to define the positiov of the circle in the figure
;    RAD : in, optional, radius of the circle, default = 0.4, 
;          set this keyword to vary the resolution of the color circle
;    PICRAD : in, optional, default = 300.
;             set this keyword to define the size of the circle in your cgwindow
;    LEGEND : in, optional, default is ['winter', 'summer', 'autumn']
;             set this keyword to vary the legend of the color circle
;    CHARSIZE: in, optional
;              the charsize
;    THICK: in, optional
;           the charthick         
;-
pro w_add_RGB_legend, ADDCMD=addcmd, DATA=data, NORMAL=normal, POSITION=position, RAD=rad, PIXRAD=pixrad, LEGEND=legend, CHARSIZE=charsize, THICK=thick

  ;--------------------
  ; Set up environment
  ;--------------------
  compile_opt idl2
    
  if N_ELEMENTS(POSITION) eq 0 then position = [0.5,0.5]
  if N_ELEMENTS(RAD) eq 0 then rad = [0.4]  
  if N_ELEMENTS(NORMAL) eq 0 then normal = 1
  
  ; dimension of circle in pixels
  if N_ELEMENTS(PIXRAD) EQ 0 then pixrad = 300.
  
  mypos = [POSITION[0]-rad,POSITION[1]-rad,POSITION[0]+rad,POSITION[1]+rad] 
  
  colorcircle = w_add_RGB_legend_make_color_circle(pixrad)
   
   ; prepare image
  cgImage, colorcircle, /KEEP_ASPECT_RATIO, /SAVE, POSITION=mypos, ADDCMD=addcmd, DATA=data, NORMAL=normal, /NOERASE
  
  center = pixrad+1
  npoints = 1000
  alpha = indgen(npoints) * 2. *!PI / (npoints-1)
  xcircle = center + cos(alpha) * pixrad
  ycircle = center + sin(alpha) * pixrad
  cgPlots, xcircle,ycircle, ADDCMD=addcmd , /DATA, THICK=thick
  
  ; legend marks
  xmonsoonmark = [(center + cos(!PI*1/2) * pixrad), (center + cos(!PI*1/2) * (pixrad+pixrad/10))]
  ymonsoonmark = [(center + sin(!PI*1/2) * pixrad), (center + sin(!PI*1/2) * (pixrad+pixrad/10))]
  xwintermark  = [(center + cos(!PI*7/6) * pixrad), (center + cos(!PI*7/6) * (pixrad+pixrad/10))]
  ywintermark  = [(center + sin(!PI*7/6) * pixrad), (center + sin(!PI*7/6) * (pixrad+pixrad/10))]
  xautumnmark  = [(center + cos(!PI*11/6) * pixrad), (center + cos(!PI*11/6) * (pixrad+pixrad/10))]
  yautumnmark  = [(center + sin(!PI*11/6) * pixrad), (center + sin(!PI*11/6) * (pixrad+pixrad/10))]
  cgplots, xwintermark, ywintermark, ADDCMD=addcmd , /Data, THICK=thick
  cgplots, xmonsoonmark, ymonsoonmark, ADDCMD=addcmd , /Data, THICK=thick
  cgplots, xautumnmark, yautumnmark, ADDCMD=addcmd , /Data, THICK=thick
  
  ; mark names
  xmonsoon = [center -(pixrad/5) + cos(!PI*1/2) *pixrad]
  ymonsoon = [center +(pixrad/9) + sin(!PI*1/2) *pixrad]
  xwinter =  [center -(2*pixrad/5) + cos(!PI*7/6) *pixrad]
  ywinter =  [center -(pixrad/10) + sin(!PI*7/6) *pixrad]
  xautumn = [center +(pixrad/7) + cos(!PI*11/6) *pixrad]
  yautumn = [center -(pixrad/10) + sin(!PI*11/6) *pixrad]
  chars = rad * 4.
  
  if N_ELEMENTS(LEGEND) eq 0 then legend=['winter', 'summer', 'autumn']
    cgtext, xmonsoon, ymonsoon, legend[1], /Data, ADDCMD=addcmd, CHARSIZE=charsize
    cgtext, xwinter, ywinter, legend[0], /Data, ADDCMD=addcmd, ALIGN=0.7, CHARSIZE=charsize
    cgtext, xautumn, yautumn, legend[2], /Data, ADDCMD=addcmd, CHARSIZE=charsize

end