; docformat = 'rst'

;+
; Determine whether any elements of an array of logical values is true. 
; 
; This allows a more elegant, "single line", conditional array testing.
;
; :Examples:
;   Check for NaNs in an array::
;
;     IDL> x = randomu(seed, 10)
;     IDL> print, w_any(x lt 0.01)
;        0
;     IDL> x[[1,4,5]] = !VALUES.F_NaN
;     IDL> if w_any(~finite(a)) then message, 'Non finite values in array'
;
; :Returns:
;   0B or 1L
;
; :Params:
;   condition : in, required, type=numeric array
;     array of conditions to check
;
; :Keywords:
;   indices : out, optional, type=lonarr
;     array of indices of true values in `condition`
;   count : out, optional, type=long
;     number of true values in `condition`
;     
; :History:
;   Original code by M. Galloy (https://github.com/mgalloy/mglib)
;   Adapted to WAVE by FaM, 2014
;     
;-
function w_any, condition, INDICES=indices, COUNT=count
  compile_opt idl2

  indices = where(condition, count)
  return, count gt 0L
end