pro w_climate_diagram, temperature, prcp
  
  
  temperature = [0.,1,3,6,8,12,14,18,14,8,4,0]
  prcp = [100,20,100,80,42,150,100,20,100,80,42,150]
  
  meanT = mean(temperature)
  totPrcp = total(prcp)
  
  labels = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  
  cgbarplot, prcp, color='blue', title='climate diagram Berlin-Dahlem!C(52'+cgsymbol('deg')+'27´N/13'+cgsymbol('deg')+'18´E), 58m', $
             xtitle='month', ytitle='precipitation [mm]', yrange=[0,400], xstyle= 1, $
             position=[0.1, 0.25, 0.9, 0.85], barnames=labels, XTICK_GET = V
              ;, barspace=0.05
  
  axis, yaxis=1, yrange=[-10,30], ystyle=1, color = cgcolor('black'), /save ,ytitle='temperature ['+cgsymbol('deg')+'C]'

  cgplot, INDGEN(12)+0.5, temperature, color=cgcolor('red'), /overplot, PSYM=5

end