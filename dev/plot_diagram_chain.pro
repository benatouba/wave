pro plot_diagram_chain, file_path

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2

  file_path = 'C:\Dokumente und Einstellungen\Hinners\Data\NCDC\gsod-561060-99999.dat'
  
  data = w_ncdc_read_gsod_file(FILE = file_path)
  
  lat  = data.loc_y
  lon  = data.loc_x
  h = round(data.elevation)
  name = data.name
  vNames = *data.varnames
  
  p = where(vNames eq 'TEMP', cnt)
  if cnt ne 1 then message, 'Variable not found'
  varTemp = (*data.vars)[p]
  temp = *varTemp.data
  time = *varTemp.time
  
  p = where(vNames eq 'PRCP', cnt)
  if cnt ne 1 then message, 'Variable not found'
  varTemp = (*data.vars)[p]
  prcp = *varTemp.data
  
  p = where(~finite(temp), cnt)
  if cnt ne 0 then print, 'Info,there are NaN values in the TS'
  
  t0 = QMS_TIME(year=1980, Month=01, day=01)
  t1 = QMS_TIME(year=2009, Month=12, day=31)
  p = where(time ge t0 and time le t1, ndays)
  if ndays eq 0 then Message, 'No valid data in the timeserie'
  
  ;Crop to the selected period
  temp = temp[p]
  prcp = prcp[p]
  time = time[p]
  abs_date = MAKE_ABS_DATE(QMS=time)
  
  ;Check for valid values
  pok = where(finite(temp), cntok, COMPLEMENT=pnok, NCOMPLEMENT=cntnok)
  if cntnok ne 0 then print, STR_equiv(cntnok) + ' missing values in temp'
  pok = where(finite(prcp), cntok, COMPLEMENT=pnok, NCOMPLEMENT=cntnok)
  if cntnok ne 0 then print, STR_equiv(cntnok) + ' missing values in prcp'
  
  ; diagram of daily values for temperature and precipitation
  w_TimeLinePlot, temp, time, 'temperature', prcp, time, 'blue',psym2=10,'precipitation', color1='red', $
    title='daily values of temperature and precipitation, '+name+'', xtitle='year',ytitle ='temperature ['+cgsymbol('deg')+'C]',$
    newaxis=2, newrange=[min(prcp),max(prcp)], newtitle='precipitation[mm]'
  ;
  ;  help, data, /STR
  ;  print, TIME_TO_STR(data.op_time)
  ;  undefine, data
    
  startTime=STRING((abs_date.year)[0],FORMAT='(I4)')
  stopTime=STRING((abs_date.year)[n_elements(abs_date.year)-1],FORMAT='(I4)')
  timeperiod=''+startTime+' - '+stopTime+''
  
  allmonths = abs_date.month
  nyears = 2009-1980+1  
  prcp_per_month = FLTARR(12, nyears) 
  
  for y=0, nyears-1 do begin  
    oneyear = where(abs_date.year EQ 1980+y)          ; oneyear = indices of one year in abs_date
    oneyearPrcp = prcp[oneyear]
    for m = 0,11 do begin
      onemonth = where(allmonths[oneyear] EQ m+1)       ; onemonth = indices for one month of one year array
      prcp_per_month[m,y] = total(oneyearPrcp[onemonth],/NAN)
    endfor
  endfor
  
  precipitation = TOTAL(prcp_per_month, 2) / nyears
    
  temperature = FLTARR(12)
  for m=0, 11 do begin
    imonths=where(abs_date.month EQ m+1)
    temperature[m]=mean(temp[imonths])
  endfor
  
; Make the plot
  
  
end
