;+
; :Description:
;    Computes the density of lognormal function
;
; :Returns:
;    the density::
;
;      y(x) = exp( -(log(x-thresh) - mu)^2 / 2*sigma^2 )
;                -------------------------------------
;                      (x-thresh)*sigma*sqrt(2*pi)
;
; :Params:
;    x: in, required
;        where to compute the density
;
; :Keywords:
;    MU: in, optional, default=0.
;        location parameter (mean)
;    SIGMA: in, optional, default=1.
;           shape parameter (stddev)
;    THRESH: in, optional, default=0.
;            threshold parameter (shift)
;
;-
function w_LogNormal, x, MU=mu, SIGMA=sigma, THRESH=thresh

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  ; Check the arguments
  SetDefaultValue, mu, 0
  SetDefaultValue, sigma, 1
  SetDefaultValue, thresh, 0
  
  y = x * !VALUES.F_NAN
  xx = x-thresh
  p = where(xx gt 0, cnt)
  if cnt ne 0 then begin
    y[p] = exp( -(alog(xx[p]) - mu)^2 / (2*sigma^2) ) / $
      ((xx[p])*sigma*sqrt(2*!pi))
      
  endif
  
  return, y
  
end
