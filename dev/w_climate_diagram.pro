pro w_climate_diagram,name, precipitation, temperature, lat, lon, vnames, h, timeperiod, cntnok
  
  labels = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  
  lat = STRING(lat,FORMAT='(F7.2)')
  lon = STRING(lat,FORMAT='(F7.2)')
  h   = STRING(h,FORMAT='(I4)')
  
  meanTemp = STRING(mean(temperature),FORMAT='(F4.1)')
  sumPrcp = STRING(total(precipitation), FORMAT='(I4)')
  
  cgbarplot, precipitation, color='blue',$
             xtitle='month', ytitle='precipitation [mm]', yrange=[0,max(precipitation)+60], xstyle= 1, $
             position=[0.15, 0.15, 0.85, 0.78], barnames=labels, XTICK_GET = V , /window
  cgaxis, yaxis=1, yrange=[min(temperature)-10,max(temperature)+10], ystyle=1, color = cgcolor('black'), /save ,ytitle='temperature ['+cgsymbol('deg')+'C]', /window
  cgplot, INDGEN(12)+0.5, temperature, color=cgcolor('red'), /overplot, /window
  cgtext, 0.25, 0.95,'climate diagram '+name+' ('+timeperiod+')', /Normal, /Window
  cgtext, 0.25,0.9,'lat ='+lat+''+cgsymbol('deg')+' / lon ='+lon+''+cgsymbol('deg')+' / '+h+' m', /Normal, /Window
  cgtext, 0.38, 0.85, 'missing days = '+STR_equiv(cntnok)+'',/Normal, /Window
  cgtext, 0.4, 0.8, ''+meanTemp+''+cgsymbol('deg')+'C, '+sumPrcp+'mm',/Normal, /Window

end