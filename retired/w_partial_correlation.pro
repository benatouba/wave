pro w_partial_correlation_test
   
   ; Let's make two independant random time series
   n = 1000
   r = RANDOMNUMBERGENERATOR()
   d1 = r->GetRandomNumbers(n)
   d2 = r->GetRandomNumbers(n)
   
   ; D0 is a linear combination of d1 and d2, with a 
   ; bit of random inside
   d0 = 0.8 * d1 + 0.2 * d2 + r->GetRandomNumbers(n, /NORMAL) * 0.1
   
   ; I can restitute the linear model with w_regress()
   xx = FLTARR(2,n)
   xx[0,*] = d1
   xx[1,*] = d2   
   res = w_regress(xx, d0, MCORRELATION=cor, ERROR_ANALYSIS=err)
   print, 'Regress model 1:'
   print, ' R2: ' + w_STR(cor^2)
   print, ' a: ' + w_str(res[0]) + ' , b: ' + w_str(res[1])
   print, ''
   
   ; D3 is simply a linear combination of d2 with a bit of random
   d3 = 0.5 * d2 + r->GetRandomNumbers(n, /NORMAL) * 0.5
   
   ; So if I add d3 to the model, this sould not improve it 
   xx = FLTARR(3,n)
   xx[0,*] = d1
   xx[1,*] = d2   
   xx[2,*] = d3   
   res = w_regress(xx, d0, MCORRELATION=cor, ERROR_ANALYSIS=err)
   print, 'Regress model 2:'
   print, ' R2: ' + w_STR(cor^2)
   print, ' a: ' + w_str(res[0]) + ' , b: ' + w_str(res[1]) + ' , c: ' + w_str(res[2])
   print, ''
   
   ; indeed. But could he replace d2?
   xx = FLTARR(2,n)
   xx[0,*] = d1
   xx[1,*] = d3   
   res = w_regress(xx, d0, MCORRELATION=cor, ERROR_ANALYSIS=err)
   print, 'Regress model 3:'
   print, ' R2: ' + w_STR(cor^2)
   print, ' a: ' + w_str(res[0]) + ' , b: ' + w_str(res[1])
   print, ''
    
   ; it could. With slightly worse results, but still
   ; So... How to know?
   
   ; First check the correlation between 
   ; variables to understand them
   xx = FLTARR(4,n)
   xx[0,*] = d0
   xx[1,*] = d1
   xx[2,*] = d2
   xx[3,*] = d3
   mm = w_correlation_matrix(xx)
   print, 'Correlation matrix'
   print, 'd0   d1   d2   d3'
   print, w_str(ABS(mm),FORMAT='(F4.2)')
   print, ''
   
   ; Then check the partial correlations
   print, 'Partial correlations of a with b when the effect of the others is considered:'
   print, 'Partial correlation of d0 with d1: ' + w_STR(w_partial_correlation(xx, 0, 1))
   print, 'Partial correlation of d0 with d2: ' + w_STR(w_partial_correlation(xx, 0, 2))
   print, 'Partial correlation of d0 with d3: ' + w_STR(w_partial_correlation(xx, 0, 3))
   
   ; Now is is clear that d2 explains more variance id d0 then d3 does.
   
   
   ; I would like to check that the results are in accordance with 
   ; the standard formula with N=3   
   rr = w_r(d0,d1) - w_r(d0,d2)*w_r(d1,d2)
   rr = rr / (sqrt(1.-w_r(d0,d2)^2) * sqrt(1.-w_r(d1,d2)^2))
   
   xx = [transpose(d0),transpose(d1),transpose(d2)]
   mrr = w_partial_correlation(xx, 0, 1)
   
   if abs(rr-mrr) gt 0.0001 then Message, 'nooo'
   
end

;+
; :Description:
;    Computes the partial correlation of two variables
;    whith the effect of the other variables removed.
;    see w_partial_correlation_test for an example.
;
; :Params:
;    x: in, required
;       a N*T variable where N is the number of independant 
;       variables (ge 3) and T the number of points.
;    i1: in, required
;       the index of the first of the two variables to 
;       compute the partial correlation from
;    i2: in, required
;       the index of the second of the two variables to 
;       compute the partial correlation from
;
; :Author: FaM, 2014, after Wikipedia
;-
function w_partial_correlation, x, i1, i2
  
  d = SIZE(x)
  if d[0] ne 2 then Message, 'X should be of dimension 2.'
  nn = d[1]
  
  m = INVERT(CORRELATE(x)) 
  
  return, - m[i1,i2] / SQRT(m[i1,i1]*m[i2,i2])
  
end