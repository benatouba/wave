;+
; :Description:
;    Computes the Probability of detection between two datasets.
;    
;    If the two datasets are vectors, the result is a scalar,
;    if the datasets are [N*M] arrays, then the result will
;    be an N elements vector 
;
; :Params:
;    ref: in, required
;         the reference data
;    data: in, required
;          the data to compare to the reference
;    threshold: in, required
;               the threshold
;
; :Returns:
;    The HSS
;
; :History:
;     Written by FaM, 2012.
;-
function w_POD, ref, data, threshold, VALID=valid

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  s = w_ContingencyMatrix(ref, data, threshold, VALID=valid)
       
  denom = FLOAT(s.a+s.c)
  pnok = where(denom lt (MACHAR()).eps, cnt)
  if cnt ne 0 then denom[pnok] = 1.
  out = float(s.a) / denom
  if cnt ne 0 then out[pnok] = !VALUES.F_NAN

  return, FLOAT(out)

end