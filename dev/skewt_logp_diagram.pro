; Problems:
; change the y-axis (=p) to log2 scale - maybe with logscl?
; rotate the x-axis (=T)

  ; Data in:
  ; restore, filename='\\klima-fs1\hinners\skew-t-log-p\test_data_pt.sav'
  
function skewt_logp_diagram_skewY, x, y, ANGLE=angle

   if N_ELEMENTS(ANGLE) eq 0 then angle = 0.

   _y = (1000.-y)/1000.
   _x = (x + 60.) / 100.

  _x =  _x + _y * tan(angle*!PI/180.)
  
  return, _x * 100. - 60.


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
  
  T_iso = INDGEN(6)*20-60
  
  
  ; include the slope of the t-axis in the temperature values
  temprange = 40+60
  pressrange= 1000

  
  cgplot, skewt_logp_diagram_skewY(temperature, pressure, ANGLE=angle), pressure, position=[0.13, 0.15, 0.85, 0.85], color = 'blue', yrange= [1000,100], xrange=[-60, 40], ytitle='pressure [hPa]',$
           xtitle='temperature ['+ cgsymbol('deg')+'C]', title='Skew-T-log(p)-diagram !C', thick=2., /window, yticklen=1, YSTYLE=1
  
  yps =   [0,1000]
  for i=0, N_ELEMENTS(T_iso)-1 do cgplots, skewt_logp_diagram_skewY([T_iso[i],T_iso[i]], yps, ANGLE=angle), yps, /window, /DATA, NOCLIP=0
   
  for nda = 0,20 do cgplots, skewt_logp_diagram_skewY(T_adiab[nda,*], p, ANGLE=angle), p, /window, /DATA, NOCLIP=0, LINESTYLE=5, color='brown'


end