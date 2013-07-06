;+
; :Description:
;    Wrapper for cgNumber_Formatter (shorter, no keyword)
;
; :Params:
;     number: in, required
;       The number to be turned into a string. May be any data type except complex, 
;       double complex, pointer or object. May be an array of numbers.
;       
;    decimals: in, optional, type=integer, default=2
;        Set this keyword to the number of decimal places to be included to the right of 
;        the decimal point in floats and doubles. Set to 2 by default. In numbers with
;        absolute values between 0 and 1, the number of decimals is equivalent to the
;        number of significant digits. 
;        
;-
function w_str, number, decimals
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  on_Error, 2
  
  if N_ELEMENTS(decimals) eq 1 then if decimals eq 0 then return, str_equiv(fix(number))
    
  return, cgNumber_Formatter(number, DECIMALS=decimals)

end