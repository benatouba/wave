pro skewt_logp_diagram, temperature, pressure

  ; Data in:
  ; restore, filename='\\klima-fs1\hinners\skew-t-log-p\test_data_pt.sav'

  R  =  8.314 ; J/mol*K
  Cp = 28.964 ; J/mol*K

  p_0 = 1013.15 ;hPa
  T_0 = 15 ;°C
  
  ;  for i = 0,4 do begin
  T = findgen(61)
  p = p_0 * (T/T_0)^(Cp/R) ; Gleichung für eine Trockenadiabate


  CGPLOT, TEMPERATURE, PRESSURE, YRANGE= [1000,0], XRANGE=[-60, 40], YTITLE='PRESSURE [HPA]', XTITLE='TEMPERATURE ['+ CGSYMBOL('DEG')+'C]', TITLE='SKEW-T-LOG(P)-DIAGRAM', /WINDOW
  CGPLOT, T,P,/OVERPLOT, /WINDOW
  CGPLOTS, [0.0, 0], [0,1000], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [20.0, 20], [0,1000], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [-20.0, -20], [0,1000], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [-40.0, -40], [0,1000], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [-60.0, 40], [200,200], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [-60.0, 40], [400,400], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [-60.0, 40], [600,600], COLOR='GREY', /ADDCMD, /DATA
  CGPLOTS, [-60.0, 40], [800,800], COLOR='GREY', /ADDCMD, /DATA
end