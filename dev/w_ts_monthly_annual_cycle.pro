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
;    an array of n*12 elements containing the monthly mean values
;    if both keywords N_VALIDYEARS and MAXMIN are set:
;         monthly_data is an array of 4 columns, columns stand for: 1=monthly_mean_values, 2=max_values, 3=min_values, 4=validyears
;    if only keyword MAXMIN is set:
;         monthly_data is an array of 3 columns, columns stand for: 1=monthly_mean_values, 2=max_values, 3=min_values
;    if only keyword N_VALIDYEARS is set:
;        monthly_data is an array of 2 columns, columns stand for: 1=monthly_mean_values, 2=validyears
;    if none of these keywords is set:
;         monthly_data is an array of 1 column,  column  stand for: 1=monthly_mean_values
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

  ; cut data with corresponding time to valid monthly values
  i_data = where(validity_mon_data ge sigmin)
  valid_monthly_data= permonth_data[i_data]
  valid_monthly_time = monthly_time[i_data]
  
  ; calculate  monthly data and features over all years
   monthly_data=fltarr(12)
   if n_Elements(MAXMIN) ne 0 then begin
   max_data = fltarr(12)
   min_data = fltarr(12)
   endif
   if N_elements(N_VALIDYEARS) ne 0 then n_valyears=fltarr(12)
   for m = 0,11 do begin
     i_months = where(valid_monthly_time.month eq m+1, n_months)
     if n_months eq 0 then begin print,'missing data for one or more of the 12 months'
        endif else begin
        monthly_data[m] = mean(valid_monthly_data[i_months])
        if n_Elements(MAXMIN) ne 0 then begin 
        max_data[m] = max(valid_monthly_data[i_months])
        min_data[m] = min(valid_monthly_data[i_months])
        endif
        if N_elements(N_VALIDYEARS) ne 0 then n_valyears[m] = N_ELEMENTS(i_months)
     endelse
   endfor
   
if N_elements(N_VALIDYEARS) ne 0 then begin
    if N_elements(MAXMIN) ne 0 then monthly_data=transpose([[monthly_data], [max_data], [min_data], [n_valyears]])$
    else monthly_data=transpose([[monthly_data], [n_valyears]])
endif else begin
    if N_elements(MAXMIN) ne 0 then monthly_data=transpose([[monthly_data], [max_data], [min_data]])$
    else monthly_data=transpose([monthly_data])
endelse

  return, monthly_data


end