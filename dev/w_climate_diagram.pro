;+
; :Description:
;    This procedure generates a climate diagram which shows monthly mean values of temperature and precipitation.
;    The following parameters are necessary inputs for the procedure.
;
; :Params:
;    name : in, required, 'station name'
;    precipitation:in, required, the monthly precipitation sum as an array of 12 elements
;    temperature: in, required, the monthly mean temperature as an array of 12 elements
;    lat: in, required, latitude
;    lon:in, required, longitude
;    height:in, required, height above sea level
;    timeperiod: in, required, 'time period', the time period of years for which the climate diagram is averaged
;    missingdays_temp: in, required, number of days without temperature information
;    missingdays_prcp: in, required, number of days without precipitation information
; 
; :Keywords: 
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    STD_PNG: in, optional, type = string
;             set to a filename to generate a standard png output (without image magick)
;
; :Example:
;   IDL> w_climate_diagram, 'Berlin', [45,35,40,40,55,70,55,65,45,35,50,55], [-1,0,4,8,15,17,18,17,14,9,4,1],$
;        52.27, 13.18, 58, '1961-1990', 0, 0, STD_PNG=std_png
;
; :History:
;     Written by JaH, 2012.
;
;-
pro w_climate_diagram, name, precipitation, temperature, lat, lon, height, timeperiod, missingdays_temp, missingdays_prcp, $
                        EPS=eps, PNG=png, STD_PNG=std_png
    
  labels = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  
  _lat = STRING(lat,FORMAT='(F7.2)')
  _lon = STRING(lon,FORMAT='(F7.2)')
  h = STRING(height,FORMAT='(I4)')
  
  meanTemp = STRING(mean(temperature),FORMAT='(F4.1)')
  sumPrcp = STRING(total(precipitation), FORMAT='(I4)')
  
  ;trick
  cgDisplay,/PIXMAP  
  cgbarplot, precipitation, BARCOORDS=x, yrange=[0,(max(precipitation)+max(precipitation)/3)], xstyle=1
  WDELETE, !D.WINDOW  
    
  cgbarplot, precipitation, color='blue', barnames=labels, YSTYLE=8, $
    ytitle='precipitation [mm]', yrange=[0,(max(precipitation)+max(precipitation)/3)], xstyle=1, $
    position=[0.15, 0.15, 0.85, 0.78], /window
    
  cgaxis, yaxis=1, yrange=[min(temperature)-10,max(temperature)+10], ystyle=1, color = 'black', /save ,ytitle='temperature ['+cgsymbol('deg')+'C]', /window
  cgplot, x, temperature, color='red', /overplot, /window
  cgtext, 0.15, 0.95,'climate diagram '+name+' (' + timeperiod + ')', /Normal, /Window
  cgtext, 0.15,0.9,'lat ='+_lat+''+ cgsymbol('deg')+' / lon ='+_lon+''+ cgsymbol('deg')+' / '+ h+' m', /Normal, /Window
  cgtext, 0.15, 0.85, ''+meanTemp+' ' +cgsymbol('deg')+'C, '+sumPrcp+' mm',/Normal, /Window
  cgtext, 0.15, 0.05, 'missing days temperature = '+ STR_equiv(missingdays_temp)+', precipitation = '+ STR_equiv(missingdays_prcp)+'', /Normal, /Window
  
  if N_ELEMENTS(eps) ne 0 then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  if N_ELEMENTS(png) ne 0 then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
  if N_ELEMENTS(std_png) ne 0 then cgControl, CREATE_PNG=std_png, IM_RASTER=0
  
end