;+
; :Description:
;    Compares two {TNT_PROJ} structures
;
; :Params:
;    proj1: in, required
;           the first proj
;    proj2: in, required
;           the second proj
;
; :Keywords:
;    EPSILON: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the proj are the same, 0 if not
;  
;
;
;-
function w_gis_compareProj, proj1, proj2, EPSILON=epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(proj1, STRUCT={TNT_PROJ}) then return, 0
  if not arg_okay(proj2, STRUCT={TNT_PROJ}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(proj1.name) ne  str_equiv(proj2.name)  then return, 0 
  if ~ w_gis_compareDatum(proj1.datum, proj2.datum, epsilon = epsilon) then return, 0 
  if proj1.idx ne proj2.idx then return, 0 
  if ABS(proj1.a - proj2.a) gt epsilon then return, 0 
  if ABS(proj1.b - proj2.b) gt epsilon then return, 0 
  if ABS(proj1.azi - proj2.azi) gt epsilon then return, 0 
  if total(ABS(proj1.lat - proj2.lat)) gt 3*epsilon then return, 0 
  if total(ABS(proj1.lon - proj2.lon)) gt 3*epsilon then return, 0 
  if total(ABS(proj1.xy - proj2.xy)) gt 2*epsilon then return, 0 
  if ABS(proj1.h - proj2.h) gt epsilon then return, 0 
  if total(ABS(proj1.sp - proj2.sp)) gt 2*epsilon then return, 0 
  if proj1.zone ne proj2.zone then return, 0 
  if proj1.flag ne proj2.flag then return, 0 
  if ABS(proj1.k0 - proj2.k0) gt epsilon then return, 0 
  if ABS(proj1.rot - proj2.rot) gt epsilon then return, 0 
  if total(ABS(proj1.shp - proj2.shp)) gt 2*epsilon then return, 0 
  if ABS(proj1.ang - proj2.ang) gt epsilon then return, 0   
  if total(ABS(proj1.som - proj2.som)) gt 2*epsilon then return, 0 
  if total(ABS(proj1.sat - proj2.sat)) gt 2*epsilon then return, 0 
  if str_equiv(proj1.envi) ne  str_equiv(proj2.envi)  then return, 0 
          
  return, 1
    
end