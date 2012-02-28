; Problems:
; change the y-axis (=p) to log2 scale - maybe with logscl?
; rotate the x-axis (=T)

  ; Data in:
  ; restore, filename='\\klima-fs1\hinners\skew-t-log-p\test_data_pt.sav'
  
function skewt_logp_diagram_skewY_old, x, y, xrange, yrange, ANGLE=angle

   if N_ELEMENTS(ANGLE) eq 0 then angle = 0.
   
   ; Convert the data coordinates into NORMAL coordinates ([0.,1.])   
   dx = float(max(xrange)-min(xrange))
   dy = float(max(yrange)-min(yrange))        
   _x = (float(x) - min(xrange))/dx
   _y = - (float(y) - max(yrange))/dy ; minus is because we are inverted
   
   
  ; Make the trigonometry in normal coordinates
  _delta =  _y * tan(angle*!PI/180.)
  
  ; go back to data coordinates  
  return, (_x + _delta) * dx + min(xrange)


end

  
function skewt_logp_diagram_skewY, x, y, xrange, yrange, ANGLE=angle

  if N_ELEMENTS(ANGLE) eq 0 then angle = 0.
   
   ; Convert the data coordinates into NORMAL coordinates ([0.,1.])   
  r =  CONVERT_COORD(X, Y, /DATA, /TO_NORMAL)
   
  ; Make the trigonometry in normal coordinates
  _delta =  r[1,*] * tan(angle*!PI/180.)
  
  ; go back to data coordinates  
  _x  = r[0,*] + _delta
  r = CONVERT_COORD(_x, r[1,*], /TO_DATA, /NORMAL)
  
  return, r[0,*]


end


pro skewt_logp_diagram, temperature, pressure, ANGLE=angle

  ; gas constant
  R  =  8.314 ; J/mol*K
  
  ; Heat capacity of dry air
  Cp = 28.964 ; J/mol*K ; is this a generally accepted value ?
  
  ; sample parameters for dry adiabates
  p_0 = 1000 ; hPa
  T_0 = -60
  p = findgen(1000)
 
  ; dry adiabate formula
  T_adiab = fltarr(21,N_ELEMENTS(p))  
  for nda = 0,20 do begin
    T_adiab[nda, *] = (T_0+273.15) * (p/p_0)^(R/Cp) - 273.15
    T_0 = T_0 + 20
  endfor
  
  
  
  xrange= [-20, 80]
  yrange= [1000,100]
  
  T_iso = INDGEN(10)*20 - 100
  
  cgplot, skewt_logp_diagram_skewY(temperature, pressure, xrange, yrange, ANGLE=angle), pressure, position=[0.13, 0.15, 0.85, 0.85], color = 'blue', $
            yrange=yrange, xrange=xrange, ytitle='pressure [hPa]',$
           xtitle='temperature ['+ cgsymbol('deg')+'C]', title='Skew-T-log(p)-diagram !C', thick=2., yticklen=1, YSTYLE=1, XSTYLE=1, /WINDOW
  
  yps = [0,1000]
  for i=0, N_ELEMENTS(T_iso)-1 do cgplots, skewt_logp_diagram_skewY([T_iso[i],T_iso[i]], yps, xrange, yrange, ANGLE=angle), yps, /DATA, NOCLIP=0, /WINDOW
   
  for nda = 0,20 do cgplots, skewt_logp_diagram_skewY(T_adiab[nda,*], p, xrange, yrange, ANGLE=angle), p, /DATA, NOCLIP=0, LINESTYLE=5, color='brown', /WINDOW


end