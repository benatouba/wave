pro plot_diagram_chain, FILE=file

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
    
  if N_ELEMENTS(FILE) eq 0 then file = DIALOG_PICKFILE(TITLE='Select a NCDC file to read', /MUST_EXIST)
    
  data = w_ncdc_read_gsod_file(FILE = file)
  
  ; accepted percentage of missing days for temperature and prcp per month
  perc_temp=0.5
  perc_prcp=0.5
  
  ; basic information of the station  
  lat  = data->getProperty('loc_y')
  lon  = data->getProperty('loc_x')
  height = round(data->getProperty('elevation'))
  name = data->getProperty('name')
  vNames = data->getVarNames()
   
  ; get temperature, precipitation and time
  varObj = data->getVar('TEMP')
  temp = varObj->getData()
  time = varObj->getTime(nt)
  varObj = data->getVar('PRCP')  
  prcp = varObj->getData()

  ; time period of available weather data
  startTime= TIME_TO_STR(time[0], MASK='YYYY')
  stopTime=TIME_TO_STR(time[nt-1], MASK='YYYY')
  timeperiod=''+startTime+' - '+stopTime+''   
  
  undefine, data
  
  monthly_temp = w_ts_monthly_annual_cycle(temp, time, MIN=minTemp, MAX=maxTemp, SIGMIN=0.75, N_VALIDYEARS=n_validyears_temp)
  monthly_prcp = w_ts_monthly_annual_cycle(prcp, time, MIN=minprcp, MAX=maxprcp, SIGMIN=0.75, N_VALIDYEARS=n_validyears_prcp)
    
   cgWindow
   w_climate_diagram,  monthly_prcp, monthly_temp, NAME=name, LAT=lat, LON=lon, HEIGHT=height, TIMEPERIOD=timeperiod, $
                       MAX_TEMP=maxTemp, MIN_TEMP=minTemp, MAX_PRCP=maxprcp, MIN_PRCP=minprcp, $
                       VALYEARS_TEMP=n_validyears_temp, VALYEARS_PRCP=n_validyears_prcp 
  
end
