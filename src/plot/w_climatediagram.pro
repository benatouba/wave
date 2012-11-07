;+
; :Description:
;    This procedure generates a climate diagram which shows monthly mean values of temperature and precipitation.
;    The following parameters are necessary inputs for the procedure.
;
; :Params:
;    precipitation: in, required
;                   the monthly precipitation as an array of 12 elements
;    temperature: in, required
;                 the monthly mean temperature as an array of 12 elements    
; 
; :Keywords:
;    name : in, optional
;           station name
;    lat: in, optional
;         latitude
;    lon: in, optional
;         longitude
;    height: in, optional 
;            height above sea level
;    timeperiod: in, optional
;                the time period of years for which the climate diagram is averaged
;    max_temp: in, optional
;               array of 12 values with maximal temperature values measured for the respective months
;    min_temp: in, optional
;              array of 12 values with minimal temperature values measured for the respective months
;    max_prcp: in, optional
;              array of 12 values with maximal precipitation values per day measured for the respective months    
;    min_prcp: in, optional
;              array of 12 values with minimal precipitation values per day measured for the respective months
;    valyears_temp: in, otional
;                   array of 12 values with number of number years of validated temperature for the respective months
;    valyears_prcp: in, otional 
;                    array of 12 values with number of number years of validated precipitation for the respective months
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    STD_PNG: in, optional, type = string
;             set to a filename to generate a standard png output (without image magick)
;
; :Example:
;   IDL> w_climate_diagram, [45,35,40,40,55,70,55,65,45,35,50,55], [-1,0,4,8,15,17,18,17,14,9,4,1], NAME='Berlin',$
;        LAT=52.27, LON=13.18, HEIGHT=58, TIMEPERIOD='1961-1990', VALYEARS_TEMP=[30,30,30,30,30,30,30,30,30,30,30,30],$
;        VALYEARS_PRCP=[30,30,30,30,30,30,30,30,30,30,30,30]
;
; :History:
;     Written by JaH, 2012.
;
;-
pro w_climateDiagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HEIGHT=height, TIMEPERIOD=timeperiod, $
                       MAX_TEMP=max_temp, MIN_TEMP=min_temp, MAX_PRCP=max_prcp, MIN_PRCP=min_prcp, $
                       VALYEARS_TEMP=valyears_temp, VALYEARS_PRCP=valyears_prcp, EPS=eps, PNG=png, STD_PNG=std_png
  
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
  
  if N_ELEMENTS(max_temp) ne 0 then $
    maxitemp=max(max_temp)$
    else maxitemp = max(temperature)

  if N_ELEMENTS(min_temp) ne 0 then $
    minitemp=min(min_temp) $
    else minitemp = min(temperature)
    
  if N_ELEMENTS(max_prcp) ne 0 then $
    maxiprcp=max(max_prcp)$
    else maxiprcp = max(precipitation)
  
  meanTemp = STRING(mean(temperature),FORMAT='(F4.1)')
  sumPrcp = STRING(total(precipitation), FORMAT='(I4)')

  ; trick
  cgDisplay,/PIXMAP  
  cgbarplot, precipitation, BARCOORDS=x, xstyle=9, YSTYLE=8,  yrange=[0, maxiprcp]
  cgaxis, yaxis=1, yrange=[min(minitemp),max(maxitemp)], ystyle=0, YTICK_GET=yt
  WDELETE, !D.WINDOW  
  
  cgWindow
  cgControl, EXECUTE=0
  cgbarplot, precipitation, color='blue', barnames=labels, YSTYLE=4, $
     yrange=[0, maxiprcp], XSTYLE=9, $
    position=[0.18, 0.15, 0.85, 0.78], /window
    
  cgaxis, yaxis=0, yrange=[0, maxiprcp], color = 'Blue', /save, ytitle='precipitation [mm/m]', /window

  if (N_ELEMENTS(max_prcp) ne 0)  and (N_ELEMENTS(min_prcp) ne 0) then $
     cgerrplot, x, min_prcp, max_prcp, color='dodger blue', /addcmd
    
  cgaxis, yaxis=1, yrange=[min(minitemp),max(maxitemp)], ystyle=0, color = 'Red', /save ,ytitle='temperature [$\deg$C]', /window
  cgplot, x, temperature, color='red', /overplot, /window, THICK=12
  
  if (N_ELEMENTS(max_temp) ne 0) and (N_ELEMENTS(min_temp) ne 0) then $
     cgerrplot, x, min_temp, max_temp, color='red',  /addcmd
    
  if (N_ELEMENTS(name) ne 0) and (N_ELEMENTS(timeperiod) ne 0) then $
     cgtext, 0.18, 0.95,''+name+' (' + timeperiod + ')', /Normal, /window

  if (N_ELEMENTS(lat) ne 0) and (N_ELEMENTS(lon) ne 0) and (N_Elements(height) ne 0) then $
    cgtext, 0.18,0.9,'lat:'+_lat+'$\deg$, lon:'+_lon+'$\deg$,  elevation:'+ h+' m', /Normal, /Window

  cgtext, 0.18, 0.85, ''+meanTemp+' $\deg$C, '+sumPrcp+' mm',/Normal, /Window
  
  if (N_ELEMENTS(valyears_temp) ne 0) and (N_ELEMENTS(valyears_prcp) ne 0) then begin
  
     vt=strarr(12)
     vp=strarr(12)
     for v= 0,11 do begin
        vt[v]=String(valyears_temp[v], Format='(I2)')
        vp[v]=String(valyears_prcp[v],Format ='(I2)')
     endfor
     
     ycoord = min(yt) - 0.12 * (MAx(yt)-min(yt))
     cgtext,-4.5, ycoord, 'val. years T.!Cval. years P.', /DATA, /Window
     strtags = vt+'!C'+vp
     for t=0, N_ELEMENTS(strtags)-1 do cgtext, x[t], ycoord, strtags[t], /DATA, /Window, ALIGNMENT=0.5
     
  endif
  cgControl, EXECUTE=1
  if N_ELEMENTS(eps) ne 0 then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  if N_ELEMENTS(png) ne 0 then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
  if N_ELEMENTS(std_png) ne 0 then cgControl, CREATE_PNG=std_png, IM_RASTER=0
  
end