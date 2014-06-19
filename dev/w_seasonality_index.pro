function w_seasonality_index, data
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  ;check if we have a entire number of years
  nt = n_elements(data)
  ny =  nt / 12
  if nt / 12. ne ny then message, 'nope'
  
  
  _data = mean(reform(data, 12, ny), DIMENSION=2)
  _m = total(_data)
  
  return, TOTAL(abs(_data - (_m/12))) / abs(_m)
  

end