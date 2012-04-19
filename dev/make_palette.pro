
function make_color, angle, radius 

  ; create angle
  tick_coords = [0  , 60 , 120, 180, 240, 300, 360]
  ticks_r     = [0  , 0  , 0  , 255, 255, 255, 0  ]
  ticks_g     = [255, 255, 0  , 0  , 0  , 255, 255]
  ticks_b     = [0  , 255, 255, 255, 0  , 0  , 0  ]
  
  ; calculate r,g,b for certain angle on periphery of circle
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


function test_circle

  ; dimension of circle
  ; if you vary the dimension, you probably have to vary the legend as well (see below in 'legend marks' and 'mark names')
  s = 601
  center = s/2
  maxR = s-center-1
  
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
  radius[*] = test[1,*] * 100./maxR
  angle[*] = test[0,*]
  angle = ROTATE(ABS(angle - !PI),3) * 180. / !PI
  
  ; generate color circle
  pok = where(radius le 100, cntok)
  for i=0, cntok-1 do begin
    ind = pok[i]
    r_g_b = make_color(angle[ind], radius[ind])
    r[ind] = r_g_b[0]
    g[ind] = r_g_b[1]
    b[ind] = r_g_b[2]
  endfor
  colorcircle= bytarr(s,s,3)
  colorcircle[*,*,0]=r 
  colorcircle[*,*,1]=g
  colorcircle[*,*,2]=b   
  
  ; prepare image
  wxsize = 600
  wysize = 600
  cgImage, colorcircle, WXSIZE=wxsize, WYSIZE=wysize, /Window, /KEEP_ASPECT_RATIO, /SAVE, MARGIN=0.1
  
  
  npoints = 1000
  alpha = indgen(npoints) * 2. *!PI / (npoints-1)
  xcircle = center + cos(alpha) * maxR
  ycircle = center + sin(alpha) * maxR
  cgPlots, xcircle,ycircle, /ADDCMD, /DATA, THICK=2
  
  ; legend marks
  xmonsoonmark = [(center + cos(!PI*1/2) * maxR), (center + cos(!PI*1/2) * (maxR+20))] ; change the (maxR+***) value if you vary 
  ymonsoonmark = [(center + sin(!PI*1/2) * maxR), (center + sin(!PI*1/2) * (maxR+20))] ; the dimension of the circle
  xwintermark  = [(center + cos(!PI*7/6) * maxR), (center + cos(!PI*7/6) * (maxR+20))]
  ywintermark  = [(center + sin(!PI*7/6) * maxR), (center + sin(!PI*7/6) * (maxR+20))]
  xautumnmark  = [(center + cos(!PI*11/6) * maxR), (center + cos(!PI*11/6) * (maxR+20))]
  yautumnmark  = [(center + sin(!PI*11/6) * maxR), (center + sin(!PI*11/6) * (maxR+20))]
  cgplots, xwintermark, ywintermark, /ADDCMD, /Data, Thick=2
  cgplots, xmonsoonmark, ymonsoonmark, /ADDCMD, /Data, Thick=2
  cgplots, xautumnmark, yautumnmark, /ADDCMD, /Data, Thick=2
  
  ; mark names
  xmonsoon = [center -40 + cos(!PI*1/2) *maxR] ;change the (center - ***) value if you vary the dimension of the circle
  ymonsoon = [center +25 + sin(!PI*1/2) *maxR]
  xwinter =  [center -80 + cos(!PI*7/6) *maxR]
  ywinter =  [center -20 + sin(!PI*7/6) *maxR]
  xautumn = [center +20 + cos(!PI*11/6) *maxR]
  yautumn = [center -20 + sin(!PI*11/6) *maxR]
  ;cgtext, 0.05, 0.95, 'Legend', /Normal, /Window
  cgtext, xmonsoon, ymonsoon, 'Monsoon', /Data, /Window
  cgtext, xwinter, ywinter, 'Winter', /Data, /Window
  cgtext, xautumn, yautumn, 'Autumn', /Data, /Window
  
  return, colorcircle
  
end


