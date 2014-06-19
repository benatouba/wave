;+
; :Description:
;    Computes the Brier Skill Score 1 - MSD_model / MSD_refmodel
;    
;    If the two datasets are vectors, the result is a scalar,
;    if the datasets are [N*M] arrays, then the result will
;    be an N elements vector 
;
; :Params:
;    ref: in, required
;         the reference data
;    refModel: in, required
;              the reference Model data
;    data: in, required
;          the data to compare to the reference Model
;
; :Returns:
;    The Brier Skill Score
;
; :History:
;     Written by FaM, 2014
;-
function w_Brier_SS, ref, refmodel, data, VALID=valid

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  return, 1 - w_MSD(ref, data) /  w_MSD(ref, refmodel)

end