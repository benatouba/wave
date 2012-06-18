;+
; :Description:
;    Simple routine to convert times into a delta in months from a reference date
;
; :Params:
;    time: in, required, type=qms/{ABS_DATE}
;          the time to convert
;
; :Keywords:
;    SINCE: in, optional, type=qms/{ABS_DATE}, DEFAULT= dec 1899
;           the reference date
; 
; :Returns:
;    the number of months since the reference date 
;
; :History:
;     Written by FaM, 2012.
;
;-
function w_time_to_month, time, SINCE=since

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~ check_WTIME(time, OUT_ABSDATE=t) then Message, WAVE_Std_Message('time', /ARG)  
  if N_ELEMENTS(since) eq 0 then since = MAKE_ABS_DATE(YEAR=1899, MONTH=12, day=1)
  if ~ check_WTIME(since, OUT_ABSDATE=s) then Message, WAVE_Std_Message('since', /ARG)
  
  dy = t.year - s.year
  dm = t.month - s.month
  
  return, dy * 12 + dm    

end