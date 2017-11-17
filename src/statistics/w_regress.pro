;+
; :Description:
;    Wrapper for the IDL regress function, with additional significance analysis.
;
; :Params:
;    x: in, required
;       An Nterms by Npoints array of independent variable data, where Nterms is the number
;       of coefficients (independent variables) and Npoints is the number of samples.
;    y: in, required
;       An Npoints-element vector of dependent variable points.
;
; :Keywords:
;    CHISQ: out
;           see IDL documentation       
;    CONST: out
;           see IDL documentation;      
;    CORRELATION: out
;                 see IDL documentation       
;    DOUBLE: in
;            see IDL documentation       
;    FTEST: out
;           see IDL documentation      
;    MCORRELATION: out
;           see IDL documentation
;    MEASURE_ERRORS: out
;           see IDL documentation
;    NAN: in, optional, type=boolean, default=0
;           set this keyword to internally remove xy pairs where either value is NaN
;    SIGMA: out
;           see IDL documentation
;    STATUS: out
;            see IDL documentation
;    YFIT: out
;          see IDL documentation
;    P_VALUE: in, optional, default=0.05
;             the P value for the significance test 
;             (i.e. the value not to overcome)
;    ERROR_ANALYSIS: out
;                    a structure with following tags:: 
;                      error_probability: the probability of error
;                      sig: significant at the P_VALUE level? yes or no
;                      sig_01: significant at the 0.01 level? yes or no
;                      sig_05: significant at the 0.05 level? yes or no
;                      
;
; :Author: FM, 2013
;-
function w_regress, x, y, $
  CHISQ=chisq, $
  CONST=const, $
  CORRELATION=correlation, $
  DOUBLE=double, $
  FTEST=ftest, $
  MCORRELATION=mcorrelation, $
  MEASURE_ERRORS=measure_errors, $
  NAN=nan, $
  SIGMA=sigma, $
  STATUS=status, $
  YFIT=yfit, $
  P_VALUE=p_value, $
  ERROR_ANALYSIS=error_analysis
    
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  _x = x
  _y = y
  if KEYWORD_SET(NAN) then begin
    p = where(finite(_x) and finite(_y), /NULL)
    _x = _x[p]
    _y = _y[p]
  endif
  
  Np = N_ELEMENTS(_y)
  if N_ELEMENTS(_x) eq Np then Nv = 1 else Nv = N_ELEMENTS(_x[*,0])
  Df = Np - Nv - 1  ; Degrees of freedom
  
  SetDefaultValue, p_value, 0.05
  
  r = regress(_x, _y, $
  CHISQ=chisq, $
  CONST=const, $
  CORRELATION=correlation, $
  DOUBLE=double, $
  FTEST=ftest, $
  MCORRELATION=mcorrelation, $
  MEASURE_ERRORS=measure_errors, $
  SIGMA=sigma, $
  STATUS=status, $
  YFIT=yfit)
  
  p_e = 2 * (1 - t_pdf(sqrt(ftest), Df)) ; error probability
  sig_01 = p_e lt 0.01
  sig_05 = p_e lt 0.05
  sig = p_e lt p_value
  error_analysis = {error_probability:p_e, sig:sig, sig_01:sig_01, sig_05:sig_05}
  
  return, r
  
end