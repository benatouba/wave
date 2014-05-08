;+
; :Description:
;   Computes the saturation vapor pressure over ice.
;   
;   Original code by Johnny Lin:
;   http://www.johnny-lin.com/py_pkgs/atmqty/lib/eice.py
;
; :Params:
;    t: in, required
;       temperature [K]
;-
function w_vp_ice, t

  compile_opt idl2
  on_error, 2
  
  return , exp( 23.33086 - (6111.72784 / T) + (0.15215 * alog(T)) ) 
  
end