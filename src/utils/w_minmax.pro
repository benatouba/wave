;+
; :Description:
;   Returns min and max in an array
;
; :Params:
;    val: in, required
;         array to compute min and max from
;    subscript: out
;               a 2-el arrays containing the subscripts of min and max
;
; :Keywords:
;    NAN: in, optional
;         ignore NaNs
; 
; :Returns:
;    a 2-el arrays containing the min and max values
;
; :Author: Fabien Maussion 2014
;          Last modification: FaM, Jun 17, 2014
;
;-
function w_MinMax, val, subscript, NAN=nan

  ; Set Up environnement
  COMPILE_OPT idl2
  on_error, 2
  
  mi = min(val, submi, MAX=ma, SUBSCRIPT_MAX=subma, NAN=nan)  
  subscript = [submi, subma]
    
  return, [mi, ma]
  
end