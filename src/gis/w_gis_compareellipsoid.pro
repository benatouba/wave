;+
;    Compares two {TNT_ELLIPSOID} structures
;
; :Params:
;    ell1: in, required
;           the first ell
;    ell2: in, required
;           the second ell
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the ell are the same, 0 if not
;
;
;-
function w_gis_compareEllipsoid, ell1, ell2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(ell1, STRUCT={TNT_ELLIPSOID}) then return, 0
  if not arg_okay(ell2, STRUCT={TNT_ELLIPSOID}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(ell1.name) ne  str_equiv(ell2.name)  then return, 0 
  if ABS(ell1.a - ell2.a) gt epsilon then return, 0 
  if ABS(ell1.b - ell2.b) gt epsilon then return, 0 
            
  return, 1
    
end