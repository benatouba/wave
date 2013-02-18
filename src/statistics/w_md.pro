;+
; :Description:
;    Computes the mean deviation between two datasets.
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
;
; :Returns:
;    The mean deviation
;
; :History:
;     Written by FaM, 2012.
;-
function w_MD, ref, data, VALID=valid

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ array_processing(ref, data, REP_A0=_ref, REP_A1=_data) then message, '$ref and $data arrays not compatible'
  
  if N_ELEMENTS(valid) ne 0 then begin
    if ~ array_processing(_ref, valid) then message, '$ref, $data and $valid arrays not compatible'
    p = where(~ valid, cnt)
    if cnt ne 0 then begin
      dataTypeName = Size(_ref, /TNAME)
      CASE dataTypeName OF
        'FLOAT': begin
          _data = FLOAT(_data)
          _ref[p] = !VALUES.F_NAN
          _data[p] = !VALUES.F_NAN
        end
        'DOUBLE': begin
          _data = DOUBLE(_data)
          _ref[p] = !VALUES.D_NAN
          _data[p] = !VALUES.D_NAN
        end
        else: Message, 'We do not do integer arithmetic. Integer arithmetic sucks.'
      endcase
    endif
  endif
  
  case size(ref, /N_DIMENSIONS) of
    1: return, mean(_data - _ref, /NAN)
    2: return, mean(_data - _ref, /NAN, DIMENSION=2)
    else: message, 'Dimension of ref not valid'
  endcase

end