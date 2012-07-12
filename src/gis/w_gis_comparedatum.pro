;+
;    Compares two {TNT_DATUM} structures
;
; :Params:
;    dat1: in, required
;           the first dat
;    dat2: in, required
;           the second dat
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the dat are the same, 0 if not
;  
;
;-
function w_gis_compareDatum, dat1, dat2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(dat1, STRUCT={TNT_DATUM}) then return, 0
  if not arg_okay(dat2, STRUCT={TNT_DATUM}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(dat1.name) ne  str_equiv(dat2.name)  then return, 0 
  if ~ w_gis_compareEllipsoid(dat1.ellipsoid, dat1.ellipsoid, epsilon = epsilon) then return, 0 
  if ABS(dat1.dx - dat2.dx) gt epsilon then return, 0 
  if ABS(dat1.dy - dat2.dy) gt epsilon then return, 0 
  if ABS(dat1.dz - dat2.dz) gt epsilon then return, 0 
            
  return, 1
    
end