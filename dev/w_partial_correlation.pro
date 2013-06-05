function w_partial_correlation, a, b, c
  
  rab = CORRELATE(a, b)
  rca = CORRELATE(c, a)
  rbc = CORRELATE(b, c)
  
  r = (rab - (rca*rbc)) / (SQRT(1.-rca^2) * SQRT(1.-rbc^2))
  
  return, r
  
end