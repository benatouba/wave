;-----------------------------------------------------------------------
;+
; NAME:
;       w_Climate_Diagram
;
; PURPOSE:
;       This procedure plots a climate diagram from monthly sums of precipitation and
;       monthly mean air temperature for the twelve calender months (starting from January).
;       Optionally lowest and highest monthly values of precipitation and air temperature
;       may be plotted, too.
;       
;       Meta-data for the station and the time period may be plotted, if specified in the 
;       corresponding keywords.
;       
;       Axis scaling may either be auto-computed or specified by the user. The plot is either
;       shown in a window or s
;
; CATEGORY:
;       Climatology
;
; CALLING SEQUENCE:
;       w_Climate_Diagram, Pr_clim, Ta_clim, NAME=name, PERIOD=period, LON=lon, LAT=lat, HGT=hgt,      $
;                          PR_LOWER=Pr_lower, PR_UPPER=Pr_upper, TA_LOWER=Ta_lower, TA_UPPER=Ta_upper, $
;                          PR_RANGE=Pr_range, TA_RANGE=Ta_range,                                       $
;                          FILENAME=filename, /CLOSE, PLOT_OBJ=plot_obj
;
; INPUT:
;       Pr_clim: monthly sums of precipitation (mm/month) for the twelve calender months 
;       Ta_clim: monthly mean air temperature (deg C) for the twelve calender months
;
; KEYWORDS:
;       name     (I): string with name of the station to be shown in title of the plot
;       period   (I): string with time period for which the climate data are valid 
;       lon      (I): station longitude (decimal degrees) 
;       lat      (I): station latitude (decimal degrees)
;       hgt      (I): station height (m asl)
;       Pr_lower (I): lowest monthly precipition (mm/month) in period
;       Pr_upper (I): highest monthly precipition (mm/month) in period
;       Ta_lower (I): lowest monthly air temperature (deg C) in period
;       Ta_upper (I): highest monthly precipition (deg C) in period
;       Pr_range (I): axis range for monthly precipition (mm/month)
;       Ta_range (I): axis range for monthly air temperature (deg C)
;       filename (I): string with file name (including extension) for saving the plot to a file
;       /CLOSE   (I): set keyword to close the plot file (the plot is not shown in a window)
;       /SHOW    (I): set keyword to show the plot even when a file name is specified
;       plot_obj (O): named variable in which the plot object is returned if shown
;
; MODIFICATION HISTORY:
;       Written by: D. Scherer 2019
;       Modified:   05-Apr-2019 DiS
;                   Added to TNT 2019.0
;-
;-----------------------------------------------------------------------

pro w_Climate_Diagram, Pr_clim, Ta_clim, NAME=name, PERIOD=period, LON=lon, LAT=lat, HGT=hgt,      $
                       PR_LOWER=Pr_lower, PR_UPPER=Pr_upper, TA_LOWER=Ta_lower, TA_UPPER=Ta_upper, $
                       PR_RANGE=Pr_range, TA_RANGE=Ta_range,                                       $
                       FILENAME=filename, CLOSE=close, SHOW=show, PLOT_OBJ=plot_obj       

  ;******************
  ; Check arguments *
  ;******************

  if n_elements(Pr_clim ne 12) or n_elements(Ta_clim) ne 12 then return

  Pr_mean = round(Pr_clim) > 0
  Ta_mean = float(Ta_clim)

  ;*****************
  ; Check keywords *
  ;*****************

  if n_elements(name)   ne 1 then title = '' else title = strtrim(name, 2)
  if n_elements(period) eq 1 then title += ' (' + strtrim(period, 2) + ')'

  if n_elements(lon) ne 1 then slon = '' else slon = str_equiv(lon)
  if n_elements(lat) ne 1 then slat = '' else slat = str_equiv(lat)
  if n_elements(hgt) ne 1 then shgt = '' else shgt = str_equiv(round(hgt))

  if n_elements(lon) eq 1 or n_elements(lat) eq 1 or n_elements(hgt) eq 1 then begin
    subtitle = '('
    if n_elements(lon) eq 1 then subtitle += 'lon: ' + slon + ' deg, '
    if n_elements(lat) eq 1 then subtitle += 'lat: ' + slat + ' deg, '
    if n_elements(hgt) eq 1 then subtitle += 'alt: ' + shgt + ' m a.s.l.'
    subtitle += ')'
  endif else begin
    subtitle = ''
  endelse

  ; Bounds specified?

  do_Pr_bounds = n_elements(Pr_upper) eq 12 and n_elements(Pr_lower) eq 12
  do_Ta_bounds = n_elements(Ta_upper) eq 12 and n_elements(Ta_lower) eq 12

  if do_Pr_bounds then begin
    Pr_max = round(Pr_upper) > 0
    Pr_min = round(Pr_lower) > 0
    p = where(Pr_max ge Pr_mean and Pr_min le Pr_mean, cnt)
    if cnt lt 12 then return
  endif else begin
    Pr_max = Pr_mean
    Pr_min = Pr_mean
  endelse

  if do_Ta_bounds then begin
    Ta_max = float(Ta_upper)
    Ta_min = float(Ta_lower)
    p = where(Ta_max ge Ta_mean and Ta_min le Ta_mean, cnt)
    if cnt lt 12 then return
  endif else begin
    Ta_max = Ta_mean
    Ta_min = Ta_mean
  endelse

  ; Axis scaling

  if arg_okay(Pr_range, /NUMERIC, N_ELEM=2) then begin
    Pr_l = Pr_range[0]
    Pr_h = Pr_range[1]
  endif else begin
    Pr_l = 0.
    Pr_h = 20.*ceil(max(Pr_max)/20.)
  endelse

  if arg_okay(Ta_range, /NUMERIC, N_ELEM=2) then begin
    Ta_l = Ta_range[0]
    Ta_h = Ta_range[1]
  endif else begin
    Ta_l = 5.*floor(min(Ta_min)/5.)
    Ta_h = 5.*ceil(max(Ta_max)/5.)
  endelse

  ; Output

  do_save = arg_okay(filename, N_ELEM=1, TNAME='STRING')
  if do_save then fname = strtrim(filename, 2)

  buffer = do_save && ~ keyword_set(show)

  ;***********************
  ; Plot climate diagram *
  ;***********************

  ; General settings

  resol  = 120                   ; dpi
  width  = resol*18.3/2.54       ; 183 mm two-column print width (Nature)
  height = 0.75*width            ; 4:3 aspect ratio
  dim    = [width,height]        ; size of plot window
  pos    = [0.06,0.03,0.94,0.85] ; position of graph in plot

  x      = 1 + indgen(12)
  months = ['J','F','M','A','M','J','J','A','S','O','N','D']
  xrange = [0.5,12.5]

  ;*********************
  ; Plot precipitation *
  ;*********************

  Pr_title = 'precipitation (mm/month)'

  plt = barplot(x, Pr_mean, FILL_COLOR='blue', AXIS_STYLE=1,                         $
    XTICKVAL=x, XTICKNAME=months, XRANGE=xrange, XMAJOR=12, XMINOR=0, XTICKLEN=0.02, $
    YTITLE=Pr_title, YRANGE=[Pr_l,Pr_h],                                             $
    DIM=dim, POS=pos, BUFFER=buffer)

  if do_Pr_bounds then begin
    plt = errorplot(1+indgen(12), 0.5*(Pr_max+Pr_min), 0.5*(Pr_max-Pr_min), /OVERPLOT, $
      ERRORBAR_COLOR='black', ERRORBAR_THICK=3)
  endif

  ;***********************
  ; Plot air temperature *
  ;***********************

  Ta_title = 'air temperature (deg C)'

  ax = axis(1, LOCATION='right', TITLE=Ta_title, AXIS_RANGE=[round(Ta_l),round(Ta_h)], $
    COORD=[Ta_l,(Ta_h-Ta_l)/(Pr_h-Pr_l)], MAJOR=1+round((Ta_h-Ta_l)/5), MINOR=4)

  if do_Ta_bounds then begin
    ymax = (Ta_max-Ta_l)*(Pr_h-Pr_l)/(Ta_h-Ta_l)
    ymin = (Ta_min-Ta_l)*(Pr_h-Pr_l)/(Ta_h-Ta_l)

    plt = polygon([1+indgen(12),reverse(1+indgen(12))], [ymax,reverse(ymin)], /DATA, /OVERPLOT, $
      FILL_COLOR='red', FILL_TRANSPARENCY=70)
  endif

  y = (Ta_mean-Ta_l)*(Pr_h-Pr_l)/(Ta_h-Ta_l)

  plt = plot(1+indgen(12), y, LINESTYLE='-', COLOR='red', THICK=5, /OVERPLOT)

  ;*************************
  ; Add title and subtitle *
  ;*************************

  txt = text(0.5, 0.96, title, ALIGN=0.5, FONT_SIZE=15, FONT_STYLE=1)
  txt = text(0.5, 0.92, subtitle, ALIGN=0.5, FONT_SIZE=12)

  ;***************************
  ; Add info on annal values *
  ;***************************

  Pr_year = round(total(Pr_mean))
  Ta_year = mean(Ta_mean)

  sPr_year = str_equiv(Pr_year) + ' mm/year'
  sTa_year = str_equiv(string(Ta_year, FORMAT='(F5.1)')) + ' deg C'

  txt = text(0.48, 0.87, sPr_year, ALIGN=1.0, FONT_STYLE=1, COLOR='blue')
  txt = text(0.52, 0.87, sTa_year, ALIGN=0.0, FONT_STYLE=1, COLOR='red')

  ;**************************************************
  ; Optionally save figure to file and close window *
  ;**************************************************

  if do_save eq 1 then plt.save, fname, WIDTH=width, RESOL=resol

  plot_obj = plt

  if keyword_set(close) then plt.close

end

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
;    CLOSE:   in, optional, type = boolean
;             if set then close the window after execution
;
; :Example:
;   IDL> w_climateDiagram, [45,35,40,40,55,70,55,65,45,35,50,55], [-1,0,4,8,15,17,18,17,14,9,4,1], NAME='Berlin',$
;        LAT=52.27, LON=13.18, HEIGHT=58, TIMEPERIOD='1961-1990', VALYEARS_TEMP=[30,30,30,30,30,30,30,30,30,30,30,30],$
;        VALYEARS_PRCP=[30,30,30,30,30,30,30,30,30,30,30,30]
;
; :History:
;    Written by: JaH, 2012.
;    Modified  : 22-Mar-2019 DiS
;                Completely revised; this routines only serves as wrapper for w_climate_diagram (see below)
;
;-
pro w_climateDiagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HEIGHT=height, TIMEPERIOD=timeperiod, $
                      MAX_TEMP=max_temp, MIN_TEMP=min_temp, MAX_PRCP=max_prcp, MIN_PRCP=min_prcp, $
                      VALYEARS_TEMP=valyears_temp, VALYEARS_PRCP=valyears_prcp, EPS=eps, PNG=png, STD_PNG=std_png, $
                      CLOSE=close

  if n_elements(eps) eq 1 then $
    w_climate_diagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HGT=height, PERIOD=timeperiod, $
                       PR_UPPER=max_prcp, PR_LOWER=min_prcp, TA_UPPER=max_temp, TA_LOWER=min_temp, $
                       FILENAME=eps, CLOSE=close

  if n_elements(png) eq 1 then $
    w_climate_diagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HGT=height, PERIOD=timeperiod, $
                       PR_UPPER=max_prcp, PR_LOWER=min_prcp, TA_UPPER=max_temp, TA_LOWER=min_temp, $
                       FILENAME=png, CLOSE=close

  if n_elements(std_png) eq 1 then $
    w_climate_diagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HGT=height, PERIOD=timeperiod, $
                       PR_UPPER=max_prcp, PR_LOWER=min_prcp, TA_UPPER=max_temp, TA_LOWER=min_temp, $
                       FILENAME=std_png, CLOSE=close

  if n_elements(eps) eq 1 or n_elements(png) eq 1 or n_elements(std_png) eq 1 then return

  w_climate_diagram, precipitation, temperature, NAME=name, LAT=lat, LON=lon, HGT=height, PERIOD=timeperiod, $
                     PR_UPPER=max_prcp, PR_LOWER=min_prcp, TA_UPPER=max_temp, TA_LOWER=min_temp
  
  return
  
  ;***********************
  ; Old code (erroneous) *
  ;***********************
  
  labels = ['J','F','M','A','M','J','J','A','S','O','N','D']
     
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
  cgDisplay, 1200, 900, /PIXMAP, /FREE, /WINDOW 
  cgbarplot, precipitation, color='blue', barnames=labels, YSTYLE=4, $
    XSTYLE=9, $
    position=[0.18, 0.15, 0.85, 0.78], BARCOORDS=x
  cgaxis, yaxis=1, yrange=[min(minitemp),max(maxitemp)], ystyle=0, YTICK_GET=yt
  WDELETE, !D.WINDOW  
  
  cgWindow, WXSIZE=1200, WYSIZE=900
  ;cgControl, EXECUTE=0
  cgbarplot, precipitation, color='blue', barnames=labels, YSTYLE=4, $
    XSTYLE=9, $
    position=[0.18, 0.15, 0.85, 0.78], /window
    
  cgaxis, yaxis=0, color = 'Blue', /save, ytitle='precipitation (mm/month)', /window
;  cgaxis, yaxis=0, yrange=[0, maxiprcp], color = 'Blue', /save, ytitle='precipitation (mm/month)', /window

  if (N_ELEMENTS(max_prcp) ne 0)  and (N_ELEMENTS(min_prcp) ne 0) then $
     cgerrplot, x, min_prcp, max_prcp, color='dodger blue', /addcmd
    
  cgaxis, yaxis=1, yrange=[min(minitemp),max(maxitemp)], ystyle=0, color = 'Red', /save ,ytitle='air temperature ($\deg$C)', /window
  cgplot, x, temperature, color='red', /overplot, /window, THICK=9
  
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

  if keyword_set(close) then cgControl, /DESTROY
  
end

