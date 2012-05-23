;+
; :Description:
;    Compute monthly mean values from a daily climatological time serie.
;
; :Params:
;    data: in, required
;          the data serie
;    time: in, required
;          the time
;
; :Keywords:
;    MAX: out, optional
;         set this to a named variable to obtain the maximal monthly values
;    MIN: out, optional
;         set this to a named variable to obtain the maximal monthly value
;    N_VALIDYEARS: out, optional
;                  set this to a named variable to obtain the number of 
;                  available monthly values to compute the mean 
;    MINSIG: in, optional, default=75
;            the significance threshold for a monthly value
;
; :Returns:
;    an array of 12 elements containing the monthlz mean values
;-
function w_ts_monthly_annual_cycle, data, time, MAX=max, MIN=min, SIGMIN=sigmin, N_VALIDYEARS=n_validyears
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  out = FLTARR(12)
  
  
  
  return, out

end