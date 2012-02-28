; Problems:
; change the y-axis (=p) to log2 scale - maybe with logscl?
; rotate the x-axis (=T)

  ; Data in:
  ; restore, filename='\\klima-fs1\hinners\skew-t-log-p\test_data_pt.sav'

pro skewt_logp_diagram, temperature, pressure

  ; gas constant
  R  =  8.314 ; J/mol*K
  
  ; Heat capacity of dry air
  Cp = 28.964 ; J/mol*K ; is this a generally accepted value ?
  
  ; sample parameters for dry adiabates
  p_0 = 1000 ; hPa
  T_0 = -60
  p = findgen(1000)
  T = fltarr(6,N_ELEMENTS(p))
  ; dry adiabate formula
  for nda = 0,5 do begin
    T[nda, *] =  T_0 * (p/p_0)^(R/Cp)
    T_0 = T_0 + 20
  endfor
  
  ; include the slope of the t-axis in the temperature values
  temprange = 40+60
  pressrange= 1000
  temp = temperature + (1/tan(45))* pressure * (temprange/pressrange)

  
  cgplot, temperature, pressure, position=[0.13, 0.15, 0.85, 0.85], color = 'blue', yrange= [1000,0], xrange=[-60, 40], ytitle='pressure [hPa]',$
  xtitle='temperature ['+ cgsymbol('deg')+'C]', title='Skew-T-log(p)-diagram !C', thick=1.5, /window, yticklen=1
  for nda = 0,5 do cgplot, T[nda,*], p, /overplot, /window
  cgplot, temp, pressure, color='red', /Overplot, /Window
  end