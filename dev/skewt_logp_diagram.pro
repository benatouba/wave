;+
; :Description:
;  This procedure returns a skew-T-log-p-diagram. The entered temperature and pressure values are plotted as well as
;  isobars, isotherms, unsaturated (dry) adiabates and saturated (moist) adiabates.
;  As an option, the angle of the temperature axis can be varied (e.g. to 45 degrees) to generate a "skew" temperature axis.
; 
; :Params:
;    temperature: in, required, array of temperature values (in °C), same array size as pressure array needed
;    pressure: in, required, array of atmospheric pressure values (in hPa)
;
; :Keywords:
;    ANGLE: in, optional, default = 0.,
;               set this keyword to turn the temperature axis to a certain angle, e.g. 45 degree
;    TEMPRANGE: in, optional, default = [-60,60],
;               set this keyword as deg. Celsius [min.value, max.value] to vary the x range of the diagram
;
; :Examples:
;   skewt_logp_diagram, temperature, pressure, ANGLE=45., TEMPRANGE=[-60,40]
;   
;   
; :History:
;       Written by JaH, 2012.
;       
;    
; ---------------------------------------------------------------------------$
  ; Data in:                                                                 $
  ; restore, filename='\\klima-fs1\hinners\skew-t-log-p\test_data_pt.sav'    $
  ;--------------------------------------------------------------------------$


; this function generates the skew temperature axis if an angle is set
function skewt_logp_diagram_skewY, x, y, ANGLE=angle, MINP=minp

  if N_ELEMENTS(ANGLE) eq 0 then angle = 0.
  if angle eq 0. then return, x
   
   ; Convert the data coordinates into NORMAL coordinates ([0.,1.])   
  r =  CONVERT_COORD(X, Y, /DATA, /TO_DEVICE)  
  r0 = CONVERT_COORD(X, REPLICATE(minp,N_ELEMENTS(X)), /DATA, /TO_DEVICE)
   
  ; Make the trigonometry in normal coordinates
  _delta =  REFORM((r[1,*]-r0[1,*]) * tan(angle*!PI/180.))
  
  ; go back to data coordinates  
  _x  = REFORM(r[0,*] + _delta)
  r = CONVERT_COORD(_x, r[1,*], /TO_DATA, /DEVICE)
  
  return, REFORM(r[0,*])
  
end


pro skewt_logp_diagram, temperature, pressure, ANGLE=angle, TEMPRANGE=temprange

  ; unsaturated (dry) adiabate formula
  R  =  8.314 ; gas konstant [J/mol*K]
  Cp = 28.964 ; heat capacity of dry air [J/mol*K]
  p_0 = 1000. ; [hPa]
  T_0 = 213.15 ; [K]
  p = findgen(101)*10
  T_adiab = fltarr(31,N_ELEMENTS(p))  
  for nda = 0,30 do begin
    T_adiab[nda, *] = (T_0) * (p/p_0)^(R/Cp) - 273.15
    T_0 = T_0 + 20.
  endfor
  
  ; saturated (moist) adiabate formula
  ; (calculation and parameter values from R. Stull, 2000: "Meteorology for Scientists and Engineers", p.111)
  T_0 = 213.15
  pp=reverse(findgen(101)) ; [kPa]
  T_moistadiab = fltarr(10,N_Elements(pp)) 
  for nma = 0,9 do begin
    for i = 0, (N_Elements(pp)-1) do begin
      if i eq 0 then (_T = T_0) else (_T = T_moistadiab[nma,i-1]+273.15) 
      if i eq 0 then (_p = p_0/10.) else (_p = pp[i-1])
      if pp[i] eq 0 then continue
      es = 0.611* exp(5423.*( 1/273.15 - 1/_T ))
      rs = 0.622*es/(pp[i]-es)
      deltaT = (0.28571*_T + 2488.4*rs) / (pp[i]*(1.+(13500000.* rs/(_T^2))))
      T_moistadiab[nma,i] = _T + deltaT *(pp[i]-_p) - 273.15
    endfor
    T_0 = T_0 + 20.
    endfor

  ; skew-T-log-p-diagram
  if N_ELEMENTS(TEMPRANGE) eq 0 then xrange=[-60,60] else xrange=TEMPRANGE
  
  minp=1050
  yrange= [minp,100]
 
  WINDOW=1 
  wxsize = 800
  wysize = 600
  cgWindow, WXSIZE=wxsize, WYSIZE=wysize
    
  ; plot isobars and entered temperature and pressure
  cgplot, temperature, pressure, position=[0.13, 0.15, 0.85, 0.85], $
           yrange=yrange, xrange=xrange, ytitle='pressure [hPa]', $ 
           xtitle='temperature ['+ cgsymbol('deg')+'C]', title='Skew-T-p-diagram !C', $
           YSTYLE=1, XSTYLE=1, WINDOW=window, /NODATA, YLOG=1
  cgControl, EXECUTE=0
  wset, cgQuery(/Current)   
  
  cgplot, skewt_logp_diagram_skewY(temperature, pressure, ANGLE=angle, MINP=minp), pressure, thick=2., /OVERPLOT, color = 'black', WINDOW=window
  
  
  ; plot isotherms
  
  T_iso = INDGEN(30)*20 - 100 
  for i=0, N_ELEMENTS(T_iso)-1 do cgplots, skewt_logp_diagram_skewY([T_iso[i],T_iso[i]], yrange, ANGLE=angle, MINP=minp), yrange, /DATA, NOCLIP=0, color='dark grey', WINDOW=window
  
  ; plot dry adiabates
  for nda = 0,30 do cgplots, skewt_logp_diagram_skewY(T_adiab[nda,*], p, ANGLE=angle, MINP=minp), p, /DATA, NOCLIP=0, LINESTYLE=5, color='brown', WINDOW=window
    
  ; plot moist adiabates
  for nma = 0,9 do cgplots, skewt_logp_diagram_skewY(T_moistadiab[nma,*], (pp*10), ANGLE=angle, MINP=minp), (pp*10), /DATA, NOCLIP=0, LINESTYLE=2, $
  color='blue', WINDOW=window
  cgControl, EXECUTE=1


end