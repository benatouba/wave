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
;    DEWPOINT: in optional, array of dewpoint temperature values (in °C), same array size as pressure array needed
;    HEIGHT: in, otional, array of height values (in m), same array size as pressure array needed
;    ANGLE: in, optional, default = 0.,
;              set this keyword to turn the temperature axis to a certain angle, e.g. 45 degree
;    TEMPRANGE: in, optional, default = [-30,40],
;              set this keyword as deg. Celsius [min.value, max.value] to vary the x range of the diagram
;    TITLE: in, optional, default='Skew-T-log-p-diagram'
;    PNG: in, optional, type = string
;              set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;              set to a filename to generate an encapsulated postscript output
;    STD_PNG: in, optional, type = string
;              set to a filename to generate a standard png output (without image magick)
;
; :Examples:
;   skewt_logp_diagram, temperature, pressure, ANGLE=45., TEMPRANGE=[-60,40]
;   
;   
; :History:
;       Written by JaH, 2012.


; this function generates the skew temperature axis if an angle is set
function w_skewPTdiagram_skewY, x, y, ANGLE=angle, MINP=minp

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


pro w_skewPTdiagram, temperature, pressure, DEWPOINT=dewpoint, HEIGHT=height, ANGLE=angle, TEMPRANGE=temprange, TITLE=title, EPS=eps, PNG=png, STD_PNG=std_png

  ; unsaturated (dry) adiabate formula
  R  =  8.314 ; gas constant [J/mol*K]
  Cp = 28.964 ; heat capacity of dry air [J/mol*K]
  p_0 = 1000. ; [hPa]
  T_0 = 213.15 ; [K]
  p = findgen(106)*10
  T_adiab = fltarr(121,N_ELEMENTS(p))
  for nda = 0,120 do begin
    T_adiab[nda, *] = (T_0) * (p/p_0)^(R/Cp) - 273.15
    T_0 = T_0 + 5.
  endfor
  
  ; saturated (moist) adiabate formula
  ; (calculation and parameter values from R. Stull, 2000: "Meteorology for Scientists and Engineers", p.111)
  T_0 = 213.15
  pp=reverse(findgen(106)) ; [kPa]
  T_moistadiab = fltarr(40,N_Elements(pp))
  for nma = 0,39 do begin
    for i = 0, (N_Elements(pp)-1) do begin
      if i eq 0 then (_T = T_0) else (_T = T_moistadiab[nma,i-1]+273.15)
      if i eq 0 then (_p = p_0/10.) else (_p = pp[i-1])
      if pp[i] eq 0 then continue
      es = 0.611* exp(5423.*( 1/273.15 - 1/_T ))
      rs = 0.622*es/(pp[i]-es)
      deltaT = (0.28571*_T + 2488.4*rs) / (pp[i]*(1.+(13500000.* rs/(_T^2))))
      T_moistadiab[nma,i] = _T + deltaT *(pp[i]-_p) - 273.15
    endfor
    T_0 = T_0 + 5.
  endfor
  
  ; skew-T-log-p-diagram
  if N_ELEMENTS(TEMPRANGE) eq 0 then xrange=[-30,40] else xrange=TEMPRANGE
  
  minp=1050
  maxp=100
  yrange= [minp,maxp]
  pticks=[100,150,200,300,400,600,800,1000]
  
  WINDOW=1
  wxsize = 700
  wysize = 830
  cgWindow, WXSIZE=wxsize, WYSIZE=wysize
  
  if N_ELEMENTS(title) eq 0 then title='Skew-T-log-p-diagram !C'
  
  ; prepare plot
  cgplot, temperature, pressure, position=[0.13, 0.1, 0.87, 0.9], $
    yrange=yrange, xrange=xrange, ytitle='pressure [hPa]', $
    xtitle='!C temperature ['+ cgsymbol('deg')+'C]', title=title, $
    YSTYLE=9,XSTYLE=9, WINDOW=window, /NODATA, YTICKS=7, YTICKV=pticks, YLOG=1
  cgControl, EXECUTE=0
  wset, cgQuery(/Current)
  
  
  ; plot isotherms
  T_iso = INDGEN(60)*10 - 100
  for i=0, N_ELEMENTS(T_iso)-1 do cgplots, w_skewPTdiagram_skewY([T_iso[i],T_iso[i]], yrange, ANGLE=angle, MINP=minp),$
    yrange, /DATA, NOCLIP=0, color='dark grey', WINDOW=window
    
  ; plot isobars
  for i = 0, N_ELEMENTS(pticks)-1 do cgplots, [T_iso[0],T_iso[N_ELEMENTS(T_iso)-1]], [pticks[i],pticks[i]], /DATA,  NOCLIP=0,$
    color='dark grey', WINDOW=window
    
  ; plot dry adiabates
  for nda = 0,60 do cgplots, w_skewPTdiagram_skewY(T_adiab[nda,*], p, ANGLE=angle, MINP=minp), p, /DATA, NOCLIP=0, LINESTYLE=5,$
    color='red7', WINDOW=window
    
  ; plot moist adiabates
  for nma = 0,19 do cgplots, w_skewPTdiagram_skewY(T_moistadiab[nma,*], (pp*10), ANGLE=angle, MINP=minp), (pp*10), /DATA, NOCLIP=0,$
    LINESTYLE=2, color='blu7', WINDOW=window
    
  ; plot dewpoint temperature and pressure
  if N_ELEMENTS(dewpoint) ne 0 then cgplot, w_skewPTdiagram_skewY(dewpoint, pressure, ANGLE=angle, MINP=minp), pressure, thick=3.,$
    /OVERPLOT, color = 'blue', WINDOW=window
    
  ; plot entered pressure and temperature
  cgplot, w_skewPTdiagram_skewY(temperature, pressure, ANGLE=angle, MINP=minp), pressure, thick=3., /OVERPLOT, color = 'black',$
    WINDOW=window
  cgplots,[xrange(0),xrange(1)],[yrange(1),yrange(1)], color=black, WINDOW=window, /DATA
  
  ; plot isohypses
  if N_ELEMENTS(height) ne 0 then begin
    hticks=interpol(height, pressure, pticks)/1000.
    cgAxis, /YAxis, YTICKS=7, YTICKV=pticks, YLOG=1, YTICKNAME=cgNumber_Formatter(hticks), WINDOW=window, YTICKLEN=0., YTITLE='height [km]'
  endif
  
  cgControl, EXECUTE=1
  
  if N_ELEMENTS(eps) ne 0 then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  if N_ELEMENTS(png) ne 0 then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
  if N_ELEMENTS(std_png) ne 0 then cgControl, CREATE_PNG=std_png, IM_RASTER=0
  
end