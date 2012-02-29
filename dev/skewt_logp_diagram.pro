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

  
function skewt_logp_diagram_skewY, x, y, ANGLE=angle

  if N_ELEMENTS(ANGLE) eq 0 then angle = 0.
  if angle eq 0. then return, x
   
   ; Convert the data coordinates into NORMAL coordinates ([0.,1.])   
  r =  CONVERT_COORD(X, Y, /DATA, /TO_DEVICE)
   
  ; Make the trigonometry in normal coordinates
  _delta =  REFORM(r[1,*] * tan(angle*!PI/180.))
  
  ; go back to data coordinates  
  _x  = REFORM(r[0,*] + _delta)
  r = CONVERT_COORD(_x, r[1,*], /TO_DATA, /DEVICE)
  
  return, REFORM(r[0,*])


end


pro skewt_logp_diagram, temperature, pressure, ANGLE=angle

  ; gas constant
  R  =  8.314 ; J/mol*K
  
  ; Heat capacity of dry air
  Cp = 28.964 ; J/mol*K ; is this a generally accepted value ?
  
  ; sample parameters for dry adiabates
  p_0 = 1000. ; hPa
  T_0 = 213.15
  p = findgen(1000)
 
  ; dry adiabate formula
  T_adiab = fltarr(21,N_ELEMENTS(p))  
  for nda = 0,20 do begin
    T_adiab[nda, *] = (T_0) * (p/p_0)^(R/Cp) - 273.15
    T_0 = T_0 + 20
  endfor
  
  wxsize = 400
  wysize = 600  
;  YLOG=1        
  
  xrange= [-20, 80]
  yrange= [1000,100]
  

  ; moist adiabate formula
  a = 0.28571 ;  a, b unc c = vereinfachte Werte aus Buch Meteorology for Sc. & Eng., S. 109
  b = (1.35*10^7)
  c = 2488.4
  T0 = 253.15
  r_s = [0.78, 3.77, 14.91, 51.43] ; Werte aus Tabelle 5.1,S.97 im Buch Meteorology for Sc. & Eng.
  T_moistadiab = fltarr(4, N_Elements(p))
  for nma = 0,3 do begin
    T_moistadiab[nma,*] = (T0 + ( p - p_0) * ((a*T0 + c*r_s[nma]) / ( p_0 * (1 + b*r_s[nma]/T0) )) ) - 273.15
    T0 = T0 + 20
  endfor 
  
  print, T_moistadiab

  WINDOW=1 
  cgWindow, WXSIZE=wxsize, WYSIZE=wysize
;  cgDisplay,  wxsize, wysize
  cgControl, EXECUTE=0
  
   
  cgplot, temperature, pressure, position=[0.13, 0.15, 0.85, 0.85], $
            yrange=yrange, xrange=xrange, ytitle='pressure [hPa]', $ 
           xtitle='temperature ['+ cgsymbol('deg')+'C]', title='Skew-T-log(p)-diagram !C', $
           yticklen=1, YSTYLE=1, XSTYLE=1, WINDOW=window, /NODATA, YLOG=YLOG

  cgplot, skewt_logp_diagram_skewY(temperature, pressure, ANGLE=angle), pressure, thick=2., /OVERPLOT, color = 'blue', WINDOW=window
  
  yps = [0,1000]
  T_iso = INDGEN(30)*20 - 100 
  for i=0, N_ELEMENTS(T_iso)-1 do cgplots, skewt_logp_diagram_skewY([T_iso[i],T_iso[i]], yps, ANGLE=angle), yps, /DATA, NOCLIP=0, WINDOW=window
   
  for nda = 0,20 do cgplots, skewt_logp_diagram_skewY(T_adiab[nda,*], p, ANGLE=angle), p, /DATA, NOCLIP=0, LINESTYLE=5, color='brown', WINDOW=window
  cgControl, EXECUTE=1
  for nma = 0,3 do cgplots, skewt_logp_diagram_skewY(T_moistadiab[nma,*], p, ANGLE=angle), p, /DATA, NOCLIP=0, LINESTYLE=5, $
  color='darkgreen', WINDOW=window
  cgControl, EXECUTE=1
 ; wdelete, xwin 

end