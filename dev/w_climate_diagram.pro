;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    name
;    precipitation
;    temperature
;    lat
;    lon
;    vnames
;    height
;    timeperiod
;    missingdays_temp
;    missingdays_prcp
; 
; :Keywords: 
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    STD_PNG: in, optional, type = string
;             set to a filename to generate a standard png output (without image magick)
;
; :History:
;     Written by JaH, 2012.
;
;-
pro w_climate_diagram, name, precipitation, temperature, lat, lon, height, timeperiod, missingdays_temp, missingdays_prcp, $
                        EPS=eps, PNG=png, STD_PNG=std_png
    
  labels = [' ','Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec', ' ']
  
  _lat = STRING(lat,FORMAT='(F7.2)')
  _lon = STRING(lon,FORMAT='(F7.2)')
  h = STRING(height,FORMAT='(I4)')
  
  meanTemp = STRING(mean(temperature),FORMAT='(F4.1)')
  sumPrcp = STRING(total(precipitation), FORMAT='(I4)')
  
  x = INDGEN(12)+1
  
  cgplot, x, precipitation, color='blue', YSTYLE=8, XTICKS=13,XTICKNAME=labels,$
    ytitle='precipitation [mm]', yrange=[0,max(precipitation)+60], xstyle=1, $
    position=[0.15, 0.15, 0.85, 0.78], /window, PSYM=10, xrange=[0,13]  ;, barnames=labels
    
  cgaxis, yaxis=1, yrange=[min(temperature)-10,max(temperature)+10], ystyle=1, color = 'black', /save ,ytitle='temperature ['+cgsymbol('deg')+'C]', /window
  cgplot, x, temperature, color='red', /overplot, /window
  cgtext, 0.25, 0.95,'climate diagram '+name+' (' + timeperiod + ')', /Normal, /Window
  cgtext, 0.25,0.9,'lat ='+_lat+''+ cgsymbol('deg')+' / lon ='+_lon+''+ cgsymbol('deg')+' / '+ h+' m', /Normal, /Window
  cgtext, 0.38, 0.85, 'missing days = '+ STR_equiv(missingdays_temp)+'', /Normal, /Window
  cgtext, 0.4, 0.8, ''+meanTemp+'' +cgsymbol('deg')+'C, '+sumPrcp+'mm',/Normal, /Window
  
  
  if N_ELEMENTS(eps) ne 0 then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  if N_ELEMENTS(png) ne 0 then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
  if N_ELEMENTS(std_png) ne 0 then cgControl, CREATE_PNG=std_png, IM_RASTER=0
  
end