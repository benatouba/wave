function w_naive_1dnn, out_x, x, val
  
  nx = N_ELEMENTS(out_x)
  out = FLTARR(nx)
  
  for i=0, nx-1 do begin
    dummy = min(ABS(out_x[i] - x), p)
    out[i] = x[p]        
  endfor
  
  return, out
    
end