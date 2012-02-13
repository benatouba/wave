pro valid_temperature_stations, FILE=file, OUTPUT_DIR=output_dir

  compile_opt idl2
  @WAVE.inc
    
  ; choose file if no keyword is set
  if N_ELEMENTS(FILE) eq 0 then file = DIALOG_PICKFILE(TITLE='Select a NCDC file to read', /MUST_EXIST)
  
  ; choose output directory if no keyword is set
  if N_Elements(output_dir) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)
  
  
  ;  get data  
  data = w_ncdc_read_gsod_file(FILE = file)
  vNames = data->getVarNames()
  varObj = data->getVar('TEMP')
  org_time=varObj->getTime(nt)
  temp = varObj->getData()
  
  ; requested time series of temperature information
  t0=QMS_TIME(year=2001,month=01,day=01)
  t1=QMS_TIME(year=2011,month=12,day=31)
  p = where(org_time ge t0 and org_time le t1, ndays)
  if ndays eq 0 then Message, 'No valid data in time series'
  
  ; requirement: valid temperature information of at least 3 years in time series
  temp = temp[p]
  tempvalid = varObj->valid()
  tempvalid = tempvalid[p]
  nvalidtemp = TOTAL(tempvalid)
  perc_valid = (nvalidtemp/N_Elements(temp))
  min_daynr =(3*365) ; or more precise requirement that time series is continuous?
  if nvalidtemp lt min_daynr then begin Message, 'Not enough data in time series'
     endif else begin ;end of else-loop at the end of the whole procedure
  
  ; ---------------------------------------------------------------------------------------------------
  ; climate diagram
  ; --------------------------------------------------------------------------------------------------- 
  ; accepted percentage of missing days for temperature and prcp per month
   perc_temp=1
   perc_prcp=1
  
  ; basic information of the station  
  lat  = data->getProperty('loc_y')
  lon  = data->getProperty('loc_x')
  height = round(data->getProperty('elevation'))
  name = data->getProperty('name')
  id = data->getProperty('id')
   
  ; precipitation
  varObj = data->getVar('PRCP')
  prcp = varObj->getData()
  prcp = prcp[p]
  time = varObj->getTime(nt)
  time = time[p]
  prcpvalid = varObj->valid()
  nvalidprcp = TOTAL(prcpvalid)  

  ; time period of available weather data
  startTime= TIME_TO_STR(time[0], MASK='YYYY')
  stopTime=TIME_TO_STR(time[ndays-1], MASK='YYYY')
  timeperiod=''+startTime+' - '+stopTime+''   
  
  undefine, data
  
  ; template for new time unit (month)
  month0 = MAKE_ABS_DATE(QMS=time[0])
  month0 = MAKE_ABS_DATE(YEAR=month0.year, MONTH=month0.month, day=1)
  month1 = MAKE_ABS_DATE(QMS=time[ndays-1]+D_QMS)
  month1 = MAKE_ABS_DATE(YEAR=month1.year, MONTH=month1.month, day=1)
  monthly_time = MAKE_ENDED_TIME_SERIE(month0, month1, MONTH=1)
  StartYear = month0.year
  StopYear  = month1.year
  
  ; mean temperature(+validity information) and precipitation sum(+validity information) for new time unit
  TS_AGG, temp, time, monthly_temp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='MEAN'
  TS_AGG, temp, time, monthly_valid_temp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='N_SIG'
  TS_AGG, prcp, time, monthly_prcp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='SUM'
  TS_AGG, prcp, time, monthly_valid_prcp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='N_SIG'
  
  ; remove the last element
  monthly_time= monthly_time[0:N_ELEMENTS(monthly_time)-2]  
  ndays = GEN_month_days(monthly_time.month, monthly_time.year)
  monthly_valid_temp = FLOAT(monthly_valid_temp) / ndays
  monthly_valid_prcp = FLOAT(monthly_valid_prcp) / ndays

  ; cut temp and prcp with corresponding time to valid monthly values
  i_temp = where(monthly_valid_temp ge perc_temp)
  valid_monthly_temp= monthly_temp[i_temp]
  monthly_time_temp = monthly_time[i_temp]
  i_prcp = where(monthly_valid_prcp ge perc_prcp)
  valid_monthly_prcp = monthly_prcp[i_prcp]
  monthly_time_prcp = monthly_time[i_prcp]
  
  ; calculate  monthly temperature and features over all years
   temperature=fltarr(12)
   max_temp=fltarr(12)
   min_temp=fltarr(12)
   valyears_temp=fltarr(12)
   for m = 0,11 do begin
     i_months = where(monthly_time_temp.month eq m+1, n_months)
     if n_months eq 0 then begin print,' no temperature information for all months'
        endif else begin
        temperature[m] = mean(valid_monthly_temp[i_months])
        max_temp[m] = max(valid_monthly_temp[i_months])
        min_temp[m] = min(valid_monthly_temp[i_months])
        valyears_temp[m] = N_ELEMENTS(i_months)
     endelse
   endfor

   ; calculate monthly precipitation and features over all years
   precipitation=fltarr(12)
   max_prcp =fltarr(12)
   min_prcp=fltarr(12)
   valyears_prcp=fltarr(12)  
   for m = 0,11 do begin
     i_months = where(monthly_time_prcp.month eq m+1, n_months)
     if n_months eq 0 then begin print,' no precipitation information for all months'
       endif else begin
       precipitation [m] = mean(valid_monthly_prcp[i_months])
       max_prcp[m] = max(valid_monthly_prcp[i_months])
       min_prcp[m] = min(valid_monthly_prcp[i_months])
       valyears_prcp[m] = N_ELEMENTS(i_months)
     endelse
   endfor
   
   pngfile = 'NCDC_VALID_TEMP_'+id+'.png'
  
   w_climate_diagram,  precipitation, temperature, NAME=name, LAT=lat, LON=lon, HEIGHT=height, TIMEPERIOD=timeperiod, $
                       MAX_TEMP=max_temp, MIN_TEMP=min_temp, MAX_PRCP=max_prcp, MIN_PRCP=min_prcp, $
                       VALYEARS_TEMP=valyears_temp, VALYEARS_PRCP=valyears_prcp;, STD_PNG=output_dir+'/'+pngfile

   ;---------------------------------------------------------------------------------------------------------------------
   ; log file of all stations with at least 3 years of valid temperature information in the time interval 2001-2011
   ;---------------------------------------------------------------------------------------------------------------------    
   
    logfile='NCDC_VALID_TEMP_'+id+'.log'
    OPENW, lun, output_dir+'/'+logfile, /GET_LUN
    header='#######Stations with 3 years valid temperature data in 2001-2011#######'
    descr= 'NAME, ID, START_YEAR, STOP_YEAR, LAT, LON, HEIGHT, PERC_VALID'
    printf, lun, header
    printf, lun, descr
    free_lun, lun
    
    startYear = STRING(startYear,FORMAT='(I4)')
    stopYear = STRING(stopYear,FORMAT='(I4)')
    lat = STRING(lat,FORMAT='(F7.2)')
    lon = STRING(lon,FORMAT='(F7.2)')
    height = STRING(height,FORMAT='(I4)')
    perc_valid = STRING(perc_valid,FORMAT='(F5.3)')
      
    stat_info=''+name+', '+id+', '+StartYear+', '+StopYear+','+lat+', '+lon+', '+height+','+perc_valid+''
    OPENU, lun, output_dir+'/'+ logfile, /GET_LUN, /APPEND
    printf, lun, stat_info
    free_lun, lun

  endelse
end