pro skewt_logp_diagram, temperature, pressure

;Data in:
;restore, filename='\\klima-fs1\hinners\skew-t-log-p\test_data_pt.sav'
;
;  R = 8.314
;  m = ?
;  p = -R*ln(pressure)
;  t = temperature + m * ln(pressure)

cgplot, temperature, pressure, yrange= [1000,0], xrange=[-60, 40], ytitle='pressure [hPa]', xtitle='temperature ['+ cgsymbol('deg')+'C]', /window
cgplots, [0.0, 0], [0,1000], color='grey', /addcmd, /data
cgplots, [20.0, 20], [0,1000], color='grey', /addcmd, /data
cgplots, [-20.0, -20], [0,1000], color='grey', /addcmd, /data
cgplots, [-40.0, -40], [0,1000], color='grey', /addcmd, /data
cgplots, [-60.0, 40], [200,200], color='grey', /addcmd, /data
cgplots, [-60.0, 40], [400,400], color='grey', /addcmd, /data
cgplots, [-60.0, 40], [600,600], color='grey', /addcmd, /data
cgplots, [-60.0, 40], [800,800], color='grey', /addcmd, /data
end