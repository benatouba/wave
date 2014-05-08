;+
; :Description:
;   Computes the saturation vapor pressure over water.
;   
;   Original code by Johnny Lin:
;   http://www.johnny-lin.com/py_pkgs/atmqty/lib/esat.py
;
; :Params:
;    t: in, required
;       temperature [K]
;-
function w_vp_water, t

  compile_opt idl2
  on_error, 2
  
  return , exp( 53.67957 - (6743.769 / T) - (4.8451 * alog(T)) )
  
end