;+
; :Description:
;    This functions intends to be an "intelligent" string operator,
;    making a string format adapted to the type of data you want to 
;    print and the order of magnitude of the numbers you are 
;    manipulating.
;
; :Params:
;    val: in,  required, type=any
;         the expressions to be converted to string type. 
;    ndecimals: in,  optional, type=long, default=2
;               for floating point values, the number of decimals to print
;               
; :Examples:
;   Default behavior is to mak the shortest possible string with 2 decimals
;   if it is a float::
;     IDL> print, w_str(8)
;     8
;     IDL> print, w_str(8.2)
;     8.20
;     IDL> print, w_str(-8.2)
;     -8.20
;   All the strings of an array have the same length::
;     IDL> for i=0,1 do print, (w_str([-8.2,8.2]))[i]
;     -8.20
;      8.20
;   The user can control the number of decimals::
;     IDL> print, w_str(21.28793210348, 5)
;     21.28793
;   But, as usual, be carefull with large numbers or the sky
;   may fall again::
;     IDL> print, w_str(1845621.28793210348, 5)
;     1845621.25000
;     IDL> print, w_str(1845621.28793210348D, 5)
;     1845621.28793
;      
; :History:
;     Written by FaM, 2012.
;
;-
function w_str, val, ndecimals

  @WAVE.inc
  compile_opt idl2
  
  if N_ELEMENTS(val) eq 0 then MESSAGE, WAVE_Std_Message('VAL', /ARG)
  
  ; Get some info about the data
  dataTypeName = Size(val, /TNAME)
  
  case dataTypeName of
    'FLOAT' : format_code = 'F'
    'DOUBLE': format_code = 'F'
    'STRING': return, val
    else: return, str_equiv(val)
  endcase
  
  maxV = max(ABS(val), /NAN)
  
  num = N_ELEMENTS(BYTE(str_equiv(FLOOR(maxV))))
  if min(val) lt 0 then num += 1 ; for minus sign
  
  _ndecimals = (N_ELEMENTS(ndecimals) eq 0) ? 2 : long(ndecimals) 
  
  return, STRING(val, FORMAT='('+ format_code + str_equiv(num+_ndecimals+1) +'.'+str_equiv(_ndecimals)+')')
  
end