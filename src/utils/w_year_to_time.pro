;+
; :Description:
;    Simple routine to convert a delta in years from a reference date into time
;
; :Params:
;    year: in, required, type=long
;          the number of months since the reference date 
;
; :Keywords:
;    SINCE: in, optional, type=qms/{ABS_DATE}, DEFAULT= 1899
;           the reference date
; 
; :Returns:
;    A QMS_TIME
;  
; :History:
;     Written by FaM, 2012.
;
;-
function w_year_to_time, year, SINCE=since

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if N_ELEMENTS(since) eq 0 then since = MAKE_ABS_DATE(YEAR=1899, MONTH=12, day=1)
  if ~ check_WTIME(since, OUT_ABSDATE=s) then Message, WAVE_Std_Message('since', /ARG)
  
  return, QMS_TIME(YEAR=LONG(year) + s.year, MONTH=1, DAY=1)

end