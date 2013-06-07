function w_regress, x, y, $
  CHISQ=chisq, $
  CONST=const, $
  CORRELATION=correlation, $
  DOUBLE=double, $
  FTEST=ftest, $
  MCORRELATION=mcorrelation, $
  MEASURE_ERRORS=measure_errors, $
  SIGMA=sigma, $
  STATUS=status, $
  YFIT=yfit, $
  P_VALUE=p_value, $
  ERROR_ANALYSIS=error_analysis
    
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  
  Np = N_ELEMENTS(y)
  if N_ELEMENTS(x) eq Np then Nv = 1 else Nv = N_ELEMENTS(x[*,0])
  Df = Np - Nv - 1  ; Degrees of freedom
  
  SetDefaultValue, p_value, 0.05
  
  r = regress(x, y, $
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