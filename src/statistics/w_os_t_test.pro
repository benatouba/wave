;+
; :Description:
;    One-sample t-test that the mean of the variable x differs from a given value.
;
; :Params:
;    x: in, required
;       An Npoints array of independent variable data, where Npoints is the number of samples.
;
; :Keywords:
;    VALUE: in, optional, default=0.0
;           the value you want to test against.
; 
; :Returns:
;     a two element array [t-stat, p-value] containing the t-statistic and probability p that the
;     average of x is different of value only by chance.
;
; :Author: FM, 2015
;-
function w_os_t_test, x, VALUE=value
    
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  
  Np = N_ELEMENTS(x)
  Df = Np - 1  ; Degrees of freedom
  
  SetDefaultValue, value, 0.0
  
  t = (mean(x) - value) / (stddev(x) / sqrt(Np))
  
  p_e = 2d * (1d - t_pdf(t, Df)) ; error probability
  
  return, [t, p_e]
  
end

pro w_os_t_test_test
  
  v = [2., 2., 5., -2.]
  out = w_os_t_test(v, VALUE=0)
  
  if abs(out[0] - 1.2185) gt 0.0001 then print, 'TEST NOT PASSED'
  if abs(out[1] - 0.3101) gt 0.0001 then print, 'TEST NOT PASSED'
  
end