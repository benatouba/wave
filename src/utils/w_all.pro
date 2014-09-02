; docformat = 'rst'

;+
; Determine whether all elements of an array of logical values are true.
;
; :ESee if all elements are finite and if not, coninue working::
;
;     IDL> a = replicate(!VALUES.F_NaN, 10)
;     IDL> if w_all(~finite(a)) then Message, 'No finite element in array'
;
; :Returns:
;   0B or 1L
;
; :Params:
;   condition : in, required, type=numeric array
;     array of conditions to check
;     
; :History:
;   Original code by M. Galloy (https://github.com/mgalloy/mglib)
;   Adapted to WAVE by FaM, 2014
;
;-
function w_all, condition
  compile_opt idl2

  ind = where(condition, count)
  return, count eq n_elements(condition)
end