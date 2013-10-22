;+
; :Description:
;    Converts Arcangles, Arcminutes and Arcseconds to degree coordinates.
;
; :Params:
;    angle: in, required
;           angle
;    minute: in, optional
;            arcminute
;    second: in, optional
;            arcsecond
; :Keywords:
;    DOUBLE: in, optional
;            if set, double precision
;            
; :Author: FM, 2013
;-
function w_ArcConvert, angle, minute, second, DOUBLE=double

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(angle) eq 0 then angle = 0
  if N_ELEMENTS(minute) eq 0 then minute = 0
  if N_ELEMENTS(second) eq 0 then second = 0
  
  if KEYWORD_SET(DOUBLE) then div = 60d else div = 60.
  
  return, angle + (minute + second / div ) / div
  
end