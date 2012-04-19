
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
  
;  ; create radius
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

;function color_circle
;
;r = fltarr(360, 101)
;g = fltarr(360, 101)
;b = fltarr(360, 101)
;for radius = 0,100 do begin
;  for angle = 0,359 do begin
;      r_g_b = make_color(angle, radius)
;      r[angle, radius] = r_g_b[0]
;      g[angle, radius] = r_g_b[1]
;      b[angle, radius] = r_g_b[2]
;   endfor
;endfor
;colorcircle= fltarr(3,360,101)
;colorcircle= TRANSPOSE([[[r]],[[g]],[[b]]])
;
;return, colorcircle
;
;end

function test_circle

  ; dimension of circle
  s = 601
  center = s/2
  maxR = s-center-1
  
  ; generate basic color = white
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
  
  pok = where(radius le 100, cntok) ; wieso kommt aus 2-dim radius 1-dim pok raus?
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
  
  wxsize = 600
  wysize = 600
  ;cgImage, colorcircle, WXSIZE=wxsize, WYSIZE=wysize, /Window, /KEEP_ASPECT_RATIO
  cgImage, colorcircle, WXSIZE=wxsize, WYSIZE=wysize, /Window, /KEEP_ASPECT_RATIO, /SAVE, MARGIN=0.1
  
  
  npoints = 1000
  alpha = indgen(npoints) * 2. *!PI / (npoints-1)
  xcircle = center + cos(alpha) * maxR
  ycircle = center + sin(alpha) * maxR
  cgPlots, xcircle,ycircle, /ADDCMD, /DATA, THICK=2
  
  xmonsoonmark = [(center + cos(!PI*1/2) * maxR), (center + cos(!PI*1/2) * (maxR+20))]
  ymonsoonmark = [(center + sin(!PI*1/2) * maxR), (center + sin(!PI*1/2) * (maxR+20))]
  xwintermark  = [(center + cos(!PI*7/6) * maxR), (center + cos(!PI*7/6) * (maxR+20))]
  ywintermark  = [(center + sin(!PI*7/6) * maxR), (center + sin(!PI*7/6) * (maxR+20))]
  xautumnmark  = [(center + cos(!PI*11/6) * maxR), (center + cos(!PI*11/6) * (maxR+20))]
  yautumnmark  = [(center + sin(!PI*11/6) * maxR), (center + sin(!PI*11/6) * (maxR+20))]
  cgplots, xwintermark, ywintermark, /ADDCMD, /Data, Thick=2
  cgplots, xmonsoonmark, ymonsoonmark, /ADDCMD, /Data, Thick=2
  cgplots, xautumnmark, yautumnmark, /ADDCMD, /Data, Thick=2
  ;cgtext, 0.05, 0.95, 'Legend', /Normal, /Window
  cgtext, 0.45, 0.94, 'Monsoon', /Normal, /Window
  cgtext, 0.05, 0.27, 'Winter', /Normal, /Window
  cgtext, 0.87, 0.27, 'Autumn', /Normal, /Window
  
  return, colorcircle
  
end


