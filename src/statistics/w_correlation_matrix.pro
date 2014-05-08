;+
; :Description:
;    Computes the correlation matrix of N
;    independant variables
;
; :Params:
;    x: in, required
;       a N*T variable where N is the number of independant 
;       variables (ge 3) and T the number of points.
;       
; :Author: FaM, 2014
;-
function w_correlation_matrix, x

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  d = SIZE(x)
  if d[0] ne 2 then Message, 'X should be of dimension 2.'
  nn = d[1]
    
  m = FLTARR(nn,nn)
  for i=0,nn-1 do begin
    for j=0,nn-1 do begin
      ; TODO: this is not optimized
      m[i,j] = CORRELATE(x[i,*],x[j,*])
    endfor
  endfor  
  
  return, m
  
end