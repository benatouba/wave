;+
; :Description:
; 
;    This function fills gaps in a time serie with user defined values or NaN's.
;    It is based on the IDL value_locate function, which is pretty fast.
;
; :Params:
;    data: in, required, type=numeric
;          the data serie to complete
;    time: in, required, type = {ABS_DATE}/qms
;          the time serie associated to data
;    new_time: in, required, type = {ABS_DATE}/qms
;              the output time serie (complete)
;              
; :Keywords:
;    FILL_VALUE: in, optional
;                the value to insert into the gaps. Default is to check 
;                for the type of input data and find a suitable value
;    SUBSCRIPT: out, type=long
;               the indexes in 'new_time' where the gaps ere filled (-1 if the time series was complete)         
;    COUNT: out, type=long
;           the number of gaps filled    
; 
; :Returns:
;    An array of same size as 'new_time' similar to 'data' but with gaps filled.
;  
; :History:
;     Written by FaM, 2011.
;-
function w_ts_fill_missing, data, time, new_time, FILL_VALUE=fill_value, SUBSCRIPT=subscript, COUNT=count

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC) then message, WAVE_Std_Message('data', /NUMERIC)
  if ~ check_WTIME(time, OUT_QMS=qms1) then message, WAVE_Std_Message('time', /ARG)
  if ~ check_WTIME(new_time, OUT_QMS=qms2) then message, WAVE_Std_Message('new_time', /ARG)
  
  ;Check for fill value
  dataTypeName = Size(data, /TNAME)  
  if N_ELEMENTS(FILL_VALUE) eq 0 then begin
    case dataTypeName of
      'FLOAT' : _fill = !VALUES.F_NAN
      'DOUBLE': _fill = !VALUES.D_NAN
      'BYTE': _fill = 0B
      'LONG': _fill = -9999L
      'INT': _fill = -9999
      else: Message, 'Data type too exotic for me'
    endcase
  endif else begin
    if ~ arg_okay(FILL_VALUE, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('FILL_VALUE', /ARG)
    _fill = fill_value
  endelse
  
  subscript = -1
   
  n = N_elements(data) 
  if n_elements(qms1) ne n then message, 'data and time arrays must have same number of elements'
  if n_elements(qms2) eq n and total(abs(qms2-qms1)) eq 0 then return, data ;nothing to do
  
  s = VALUE_LOCATE(qms1, qms2) ;Subscript intervals
    
  out = data[s > 0]  
  sd = s[1:*] - s[0:N_ELEMENTS(s)-2]
    
  p = where(sd eq 0, cnt)
  if cnt ne 0 then begin
   out[p+1] = _fill 
   subscript = [subscript, p+1]
  endif
  
  p = where(s eq -1 or qms2 gt max(qms1), cnt)
  if cnt ne 0 then begin
   out[p] = _fill 
   subscript = [subscript, p]
  endif  
  
  if N_ELEMENTS(subscript) gt 1 then begin
   subscript = subscript[1:*]
   subscript = subscript[UNIQ(subscript, SORT(subscript))]
   count = N_ELEMENTS(subscript)
  endif else begin
   count = 0L
  endelse
  
  return, out

end