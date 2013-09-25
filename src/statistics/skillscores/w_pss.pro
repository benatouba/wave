;+
; :Description:
;    Computes the Peirce Skill Score between two datasets.
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
;    The PSS
;
; :History:
;     Written by FaM, 2013.
;-
function w_PSS, ref, data, threshold, VALID=valid, CONT=cont

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  s = w_ContingencyMatrix(ref, data, threshold, VALID=valid)
      
  denom = DOUBLE((s.a+s.c)*(s.b+s.d))
  pnok = where(denom lt (MACHAR(/DOUBLE)).eps, cnt)
  if cnt ne 0 then denom[pnok] = 1.
  out = FLOAT(((s.a*s.d)-(s.b*s.c)) / denom)
  if cnt ne 0 then out[pnok] = !VALUES.F_NAN
  
  cont = s
  return, FLOAT(out)

end