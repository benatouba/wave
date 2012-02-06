;+
; :Description:
;    This procedure generates a climate diagram which shows monthly mean values of temperature and precipitation.
;    The following parameters are necessary inputs for the procedure.
;
; :Params:
;    precipitation: in, required, the monthly precipitation sum as an array of 12 elements
;    temperature: in, required, the monthly mean temperature as an array of 12 elements
      
; 
; :Keywords:
;    name : in, optional, 'station name'
;    lat: in, optional, latitude
;    lon: in, optional, longitude
;    height: in, optional, height above sea level
;    timeperiod: in, optional, 'time period', the time period of years for which the climate diagram is averaged
;    max_temp: in, optional, array of 12 values with maximal temperature values measured for the respective months
;    min_temp: in, optional, array of 12 values with minimal temperature values measured for the respective months
;    max_prcp: in, optional, array of 12 values with maximal precipitation values measured for the respective months    
;    min_prcp: in, optional, array of 12 values with minimal precipitation values measured for the respective months  
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    STD_PNG: in, optional, type = string
;             set to a filename to generate a standard png output (without image magick)
;
; :Example:
;   IDL> w_climate_diagram, 'Berlin', [45,35,40,40,55,70,55,65,45,35,50,55], [-1,0,4,8,15,17,18,17,14,9,4,1],$
;        52.27, 13.18, 58, '1961-1990', STD_PNG=std_png
;
; :History:
;     Written by JaH, 2012.
;
;-
pro w_climate_diagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HEIGHT=height, TIMEPERIOD=timeperiod, $
                       MAX_TEMP=max_temp, MIN_TEMP=min_temp, MAX_PRCP=max_prcp, MIN_PRCP=min_prcp, EPS=eps, PNG=png, STD_PNG=std_png
    
  labels = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec']
  if N_ELEMENTS(lat) ne 0 then begin
    _lat = STRING(lat,FORMAT='(F7.2)')
  endif
  if N_ELEMENTS(lon) ne 0 then begin
    _lon = STRING(lon,FORMAT='(F7.2)')
  endif
  if N_ELEMENTS(height) ne 0 then begin
    h = STRING(height,FORMAT='(I4)')
  endif
  
  if N_ELEMENTS(max_temp) ne 0 then begin
    maxitemp=max(max_temp)
    else begin maxitemp = max(temperature)
  endif
  if N_ELEMENTS(min_temp) ne 0 then begin
    minitemp=min(min_temp)
    else begin minitemp = min(temperature)
  endif
  
  meanTemp = STRING(mean(temperature),FORMAT='(F4.1)')
  sumPrcp = STRING(total(precipitation), FORMAT='(I4)')
  
  ;trick
  cgDisplay,/PIXMAP  
  cgbarplot, precipitation, BARCOORDS=x, yrange=[0,(max(precipitation)+max(precipitation)/3)], xstyle=9, YSTYLE=8
  WDELETE, !D.WINDOW  
    
  cgbarplot, precipitation, color='blue', barnames=labels, YSTYLE=8, $
    ytitle='precipitation [mm]', xstyle=9, yrange=[0,(max(precipitation)+max(precipitation)/3)], $
    position=[0.15, 0.15, 0.85, 0.78], /window

  if (N_ELEMENTS(max_prcp) ne 0)  and (N_ELEMENTS(min_prcp) ne 0) then $
    cgerrplot, x, min_prcp, max_prcp, color='blu6', /addcmd

    
  cgaxis, yaxis=1, yrange=[min(minitemp),max(maxitemp)], ystyle=0, color = 'black', /save ,ytitle='temperature ['+cgsymbol('deg')+'C]', /window
  cgplot, x, temperature, color='red', /overplot, /window
  
  if N_ELEMENTS(max_temp) ne 0  and if N_ELEMENTS(min_temp) ne 0 do begin
    cgerrplot, x, min_temp, max_temp, color='red',  /addcmd
  endif
    
  if N_ELEMENTS(name) ne 0 and if N_ELEMENTS(timeperiod) ne 0 do begin
    cgtext, 0.15, 0.95,'climate diagram '+name+' (' + timeperiod + ')', /Normal, /Window
  endif
  if N_ELEMENTS(lat) ne 0 and if N_ELEMENTS(lon) ne 0 do begin
    cgtext, 0.15,0.9,'lat ='+_lat+''+ cgsymbol('deg')+' / lon ='+_lon+''+ cgsymbol('deg')+' / '+ h+' m', /Normal, /Window
  endif
  cgtext, 0.15, 0.85, ''+meanTemp+' ' +cgsymbol('deg')+'C, '+sumPrcp+' mm',/Normal, /Window
    
  if N_ELEMENTS(eps) ne 0 then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  if N_ELEMENTS(png) ne 0 then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
  if N_ELEMENTS(std_png) ne 0 then cgControl, CREATE_PNG=std_png, IM_RASTER=0
  
end