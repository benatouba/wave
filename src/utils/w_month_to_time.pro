;+
; :Description:
;    Simple routine to convert a delta in months from a reference date into time
;
; :Params:
;    month: in, required, type=long
;           the number of months since the reference date 
;
; :Keywords:
;    SINCE: in, optional, type=qms/{ABS_DATE}, DEFAULT= dec 1899
;           the reference date
; 
; :Returns:
;    A QMS_TIME
;  
; :History:
;     Written by FaM, 2012.
;
;-
function w_month_to_time, month, SINCE=since

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if N_ELEMENTS(since) eq 0 then since = MAKE_ABS_DATE(YEAR=1899, MONTH=12, day=1)
  if ~ check_WTIME(since, OUT_ABSDATE=s) then Message, WAVE_Std_Message('since', /ARG)
  
  dy = LONG(month) / 12 
  dm = LONG(month) - dy * 12
  
  y = dy + s.year
  m = dm + s.month
  
  p = where(m gt 12, cnt)
  if cnt ne 0 then begin
    y[p] = y[p] + 1
    m[p] = m[p] - 12
  endif  
  
  return, QMS_TIME(YEAR=y, MONTH=m, DAY=1)

end