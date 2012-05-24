;+
; :Description:
;    Compute monthly mean values from a daily climatological time serie.
;
; :Params:
;    data: in, required
;          the data serie, 1. column = temperature, 2. column = precipitation
;    time: in, required
;          the time
;
; :Keywords:
;    MAXMIN: out, optional
;         set this to a named variable to obtain the maximal an minimal monthly values
;    N_VALIDYEARS: out, optional
;                  set this to a named variable to obtain the number of 
;                  available monthly values to compute the mean 
;    SIGMIN: in, optional, default=75
;            the significance threshold for a monthly value
;
; :Returns:
;    an array of 12 elements containing the monthly mean values
;-
function w_ts_monthly_annual_cycle, data, time, MAXMIN=maxmin, SIGMIN=sigmin, N_VALIDYEARS=n_validyears
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  
  
  ; template for new time unit (month)
  month0 = MAKE_ABS_DATE(QMS=time[0])
  month0 = MAKE_ABS_DATE(YEAR=month0.year, MONTH=month0.month, day=1)
  month1 = MAKE_ABS_DATE(QMS=time[(N_Elements(time))-1]+D_QMS)
  month1 = MAKE_ABS_DATE(YEAR=month1.year, MONTH=month1.month, day=1)
  monthly_time = MAKE_ENDED_TIME_SERIE(month0, month1, MONTH=1)
  
  ; mean data values (+valididy info) for new time unit
  TS_AGG, data, time, permonth_data, new_time_series, NEW_TIME=monthly_time, AGG_METHOD='MEAN'
  TS_AGG, data, time, validity_mon_data, new_time_series, NEW_TIME=monthly_time, AGG_METHOD='N_SIG'
  
  ; check validity of data
  monthly_time= monthly_time[0:N_ELEMENTS(monthly_time)-2]  
  ndays = GEN_month_days(monthly_time.month, monthly_time.year)
  validity_mon_data = FLOAT(validity_mon_data) / ndays
  if N_Elements(SIGMIN) eq 0 then sigmin=0.75
  if validity_mon_data lt sigmin then print, "Not enough valid data in time series" 

  ; cut data with corresponding time to valid monthly values
  i_data = where(validity_mon_data ge sigmin)
  valid_monthly_data= permonth_data[i_data]
  valid_monthly_time = monthly_time[i_data]
  
  ; calculate  monthly data and features over all years
   monthly_data=fltarr(12)
   if n_Elements(MAXMIN) ne 0 then begin
      max_data =fltarr(12)
      min_data=fltarr(12)
   endif
   if N_elements(N_VALIDYEARS) ne 0 then valyears_data=fltarr(12)
   for m = 0,11 do begin
     i_months = where(valid_monthly_time.month eq m+1, n_months)
     if n_months eq 0 then begin print,'no data for all months'
        endif else begin
        monthly_data[m] = mean(valid_monthly_data[i_months])
        if N_elements(MAXMIN) ne 0 then begin
          max_data[m] = max(valid_monthly_data[i_months])
          min_data[m] = min(valid_monthly_data[i_months])
        endif
        if N_elements(N_VALIDYEARS) ne 0 then valyears_data[m] = N_ELEMENTS(i_months)
     endelse
   endfor


  return, monthly_data
  if N_Elements(N_VALIDYEARS) ne 0 then return,valyears_data
  if N_Elements(MAXMIN) ne 0 then begin 
      return,max_data 
      return,min_data
  endif

end