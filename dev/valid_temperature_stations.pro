;+
; :Description:
;    This procedure checks a ncdc file or all ncdc files of a path if their weather information lies in a chosen time interval
;    and contains a minimal number of valid temperature measurements. All files meeting these requirements are filtered and a new
;    folder "NCDC_filtered" is created in a chosen output directory. The new folder contains a "data" subfolder in which the
;    filtered NCDC files are stored and a "png" subfolder in which the corresponding climate diagrams for all filtered NCDC files
;    are stored. As well a csv file is generated which lists all filtered ncdc files and their station information.
;  
; :Parameters:
;    startpoint : in, required, startpoint of timeinterval, set as 'DD.MM.YYYY'
;    endpoint : in, required, endpoint of timeinterval, set as 'DD.MM.YYYY'
;    min_daynumber: in, required, minimal number of valid temperature measurements which are supposed to lie in the time interval
; 
; :Keywords:
;   Output_Dir : in, optional, output directory in which the new folder "NCDC_filtered" with the filtered files is stored.
;   Path : in, optional, input directory contaning all files which are tested. If no path is set as a keyword,
;   a window opens to select a file
;   
; :Example:
;     valid_temperature_stations,'01.01.2001', '31.12.2011', (3*365), OUTPUT_DIR = '//KLIMA-FS1/hinners/Valid_Stations', $
;     PATH = '\\KLIMA-FS1\hinners\NCDC\*' 
;
; :History:
;     Written by JaH, 2012.
;
;-
pro valid_temperature_stations, startpoint, endpoint, min_daynumber, OUTPUT_DIR=output_dir, PATH = path

  compile_opt idl2
  @WAVE.inc
  
  ; choose file if no path keyword is set
  if (N_ELEMENTS(PATH) eq 0) then PATH = String(DIALOG_PICKFILE(TITLE='Select a NCDC file to read', /MUST_EXIST))
  
  ; choose output directory if no keyword is set
  if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)
  
  ; the chosen file / all files of the path is/are tested
  file = file_search(PATH)
  filtered_files = 0
  
  ac_date = time_to_str(QMS_TIME(), MASK='YYYY-MM-DD_HHTT')
  
  for nfile = 0,(N_ELEMENTS(file)-1) do begin ; end of for-loop at the end of the whole procedure
    testfile = file[nfile]
    
    ; get time information
    if ~STRMATCH(FILE_BASENAME(FILE[nfile]), 'gsod-*.dat', /FOLD_CASE)then continue 
    data = w_ncdc_read_gsod_file(FILE = testfile)
    if ~ OBJ_VALID(data) then continue 
    vNames = data->getVarNames()
    varObj = data->getVar('TEMP')
    org_time=varObj->getTime(nt)
    
    
    ; requested time series of temperature information
    t0=QMS_TIME(Date_Str=startpoint)
    t1=QMS_TIME(Date_Str=endpoint)
    p = where(org_time ge t0 and org_time le t1, ndays)
    if ndays eq 0 then continue
    
    ; requirement: valid temperature information of at least min_daynumber in time series
    varObj = data->getVar('TEMP')
    temp = varObj->getData()
    temp = temp[p]
    tempvalid = varObj->valid()
    tempvalid = tempvalid[p]
    nvalidtemp = TOTAL(tempvalid)
    min_daynr =(min_daynumber)
    if nvalidtemp lt min_daynr then continue
    
    ; basic information of the station
    lat  = data->getProperty('loc_y')
    lon  = data->getProperty('loc_x')
    height = round(data->getProperty('elevation'))
    name = data->getProperty('name')
    id = data->getProperty('id')
        
    ; precipitation and time
    varObj = data->getVar('PRCP')
    prcp = varObj->getData()
    prcp = prcp[p]
    time = varObj->getTime(nt)
    time = time[p]
    
    ; time period of available weather data in chosen time interval
    startTime= TIME_TO_STR(time[0], MASK='YYYY')
    stopTime=TIME_TO_STR(time[ndays-1], MASK='YYYY')
    timeperiod=''+startTime+' - '+stopTime+''
    
    ; percentage of valid data and number of valid measurements for all variables
    nval=strarr(N_ELEMENTS(vNames))
    percval=strarr(N_ELEMENTS(vNames))
    name_nval=strarr(N_ELEMENTS(vNames))
    name_percval=strarr(N_ELEMENTS(vNames))
    for nvar= 0,(N_ELEMENTS(vNames)-1)do begin
      varObj = data->getVar(String(vNames[nvar]))
      var=varobj->getData()
      var=var[p]
      varvalid=varObj->valid()
      varvalid=varvalid[p]
      n_val= total(varvalid)
      nval[nvar]=String(n_val, FORMAT='(I6)')
      name_nval[nvar]='VALID_DAYS_'+String(vNames[nvar])
      percval[nvar]=String((n_val/N_Elements(var)), FORMAT='(F5.3)')
      name_percVal[nvar]='PERC_VALID_'+String(vNames[nvar])
    endfor

    undefine, data
    
    ; ----------------------------------------------------------------------------------------------------------------------------
    ; climate diagram
    ; ----------------------------------------------------------------------------------------------------------------------------
    
    ; accepted percentage of missing days for temperature and prcp per month
    perc_temp=1
    perc_prcp=1
    
    ; template for new time unit (month)
    month0 = MAKE_ABS_DATE(QMS=time[0])
    month0 = MAKE_ABS_DATE(YEAR=month0.year, MONTH=month0.month, day=1)
    month1 = MAKE_ABS_DATE(QMS=time[ndays-1]+D_QMS)
    month1 = MAKE_ABS_DATE(YEAR=month1.year, MONTH=month1.month, day=1)
    monthly_time = MAKE_ENDED_TIME_SERIE(month0, month1, MONTH=1)
    
    ; mean temperature(+validity information) and precipitation sum(+validity information) for new time unit
    TS_AGG, temp, time, monthly_temp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='MEAN'
    TS_AGG, temp, time, monthly_valid_temp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='N_SIG'
    TS_AGG, prcp, time, monthly_prcp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='SUM'
    TS_AGG, prcp, time, monthly_valid_prcp, monthly_end_time, NEW_TIME=monthly_time, AGG_METHOD='N_SIG'
    
    ; remove the last element
    monthly_time= monthly_time[0:N_ELEMENTS(monthly_time)-2]
    n_days = GEN_month_days(monthly_time.month, monthly_time.year)
    monthly_valid_temp = FLOAT(monthly_valid_temp) / n_days
    monthly_valid_prcp = FLOAT(monthly_valid_prcp) / n_days
    
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
      if n_months eq 0 then continue
      temperature[m] = mean(valid_monthly_temp[i_months])
      max_temp[m] = max(valid_monthly_temp[i_months])
      min_temp[m] = min(valid_monthly_temp[i_months])
      valyears_temp[m] = N_ELEMENTS(i_months)
    endfor
    
    ; calculate monthly precipitation and features over all years
    precipitation=fltarr(12)
    max_prcp =fltarr(12)
    min_prcp=fltarr(12)
    valyears_prcp=fltarr(12)
    for m = 0,11 do begin
      i_months = where(monthly_time_prcp.month eq m+1, n_months)
      if n_months eq 0 then continue
      precipitation [m] = mean(valid_monthly_prcp[i_months])
      max_prcp[m] = max(valid_monthly_prcp[i_months])
      min_prcp[m] = min(valid_monthly_prcp[i_months])
      valyears_prcp[m] = N_ELEMENTS(i_months)
    endfor
    
    
    FILE_MKDIR,output_dir+'/NCDC_filtered/pngs'    
    
    w_climate_diagram,  precipitation, temperature, NAME=name, LAT=lat, LON=lon, HEIGHT=height, TIMEPERIOD=timeperiod, $
      MAX_TEMP=max_temp, MIN_TEMP=min_temp, MAX_PRCP=max_prcp, MIN_PRCP=min_prcp, $
      VALYEARS_TEMP=valyears_temp, VALYEARS_PRCP=valyears_prcp, STD_PNG=output_dir+'/NCDC_filtered/pngs/gsod-'+id+'.png'
      
    ;----------------------------------------------------------------------------------------------------------------------------
    ; csv file of all stations with at least 3 years of valid temperature information in the time interval 2001-2011
    ;----------------------------------------------------------------------------------------------------------------------------
    filtered_files = filtered_files+1
    startYear = STRING(startTime,FORMAT='(I4)')
    stopYear = STRING(stopTime,FORMAT='(I4)')
    lat = STRING(lat,FORMAT='(F7.2)')
    lon = STRING(lon,FORMAT='(F7.2)')
    height = STRING(height,FORMAT='(I4)')  
    
    FILE_MKDIR,output_dir+'/NCDC_filtered/data'
    FILE_COPY, testfile, output_dir+'/NCDC_filtered/data/gsod-'+id+'.dat', /OVERWRITE
    csvfile=output_dir+'/NCDC_filtered/filtered_stations_'+ac_date+'.csv'
    started_header = 0
    if filtered_files eq 1 then begin
       OPENW, lun, csvfile, /GET_LUN
      header= 'NAME, ID (USAF-WBAN), START_YEAR, STOP_YEAR, LAT, LON, HEIGHT , '+name_nval[0]+','+name_percval[0]+' , '+name_nval[1]+','+name_percval[1]+' , '+name_nval[2]+','+name_percval[2]+' , '+name_nval[3]+','+name_percval[3]+' , '+name_nval[4]+','+name_percval[4]+' , '+name_nval[5]+','+name_percval[5]+' , '+name_nval[6]+','+name_percval[6]+' , '+name_nval[7]+','+name_percval[7]+' , '+name_nval[8]+','+name_percval[8]+' , '+name_nval[9]+','+name_percval[9]+''
      printf, lun, header
      free_lun, lun
    endif
    
    stat_info=''+name+', '+id+', '+StartYear+', '+StopYear+', '+lat+', '+lon+', '+height+' , '+nval[0]+','+percval[0]+' , '+nval[1]+','+percval[1]+' , '+nval[2]+','+percval[2]+' , '+nval[3]+','+percval[3]+' , '+nval[4]+','+percval[4]+' , '+nval[5]+','+percval[5]+' , '+nval[6]+','+percval[6]+' , '+nval[7]+','+percval[7]+' , '+nval[8]+','+percval[8]+' , '+nval[9]+','+percval[9]+''
    OPENU, lun, csvfile, /GET_LUN, /APPEND
    printf, lun, stat_info
    free_lun, lun
    
  endfor
 
end