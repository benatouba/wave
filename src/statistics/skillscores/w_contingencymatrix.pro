;+
; :Description:
;    Computes the contingency matrix of two datasets
;    
;    If the two datasets are vectors, the result is a scalar,
;    if the datasets are [N*M] arrays, then the result will
;    be an N elements vector.
;    
;    Countingency matrix basics:: 
;                    Obs:
;                 Yes   No
;       Mod: Yes   a    b
;            No    c    d
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
;    The contingency matrix as a structure of the form::
;       {a:a, b:b, c:c, d:d, n:n}
;
; :History:
;     Written by FaM, 2012.
;-
function w_ContingencyMatrix, ref, data, threshold, VALID=valid

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if N_PARAMS() ne 3 then Message, 'Need 3 parameters'
  if ~ array_processing(ref, data) then message, '$ref and $data arrays not compatible'
  
  if N_ELEMENTS(valid) eq 0 then valid = FINITE(data+ref)
  
  s = size(ref, /N_DIMENSIONS)
  if s gt 2 then message, 'Dimension of ref not valid'
  
  if s eq 1 then begin
    res = BYTARR(N_ELEMENTS(data))
    p = where(ref ge threshold, cnt)
    if cnt ne 0 then res[p] = 2
    p = where(data ge threshold, cnt)
    if cnt ne 0 then res[p] = res[p] + 1
    p = where(~ valid, cnt)
    if cnt ne 0 then res[p] = 255
    p = where(res eq 3, a, /L64)
    p = where(res eq 2, c, /L64)
    p = where(res eq 1, b, /L64)
    p = where(res eq 0, d, /L64)
    n = a + b + c + d
  endif else begin
    nm = N_ELEMENTS(ref[*,0])
    a = LON64ARR(nm)
    b = a & c = a & d = a & n = a
    for i = 0, nm-1 do begin
      tmp = w_ContingencyMatrix(REFORM(ref[i,*]), REFORM(data[i,*]), threshold, VALID=REFORM(valid[i,*]))
      a[i] = tmp.a
      b[i] = tmp.b
      c[i] = tmp.c
      d[i] = tmp.d
      n[i] = tmp.n
    endfor
  endelse
  
  return, {a:a, b:b, c:c, d:d, n:n}
  
end