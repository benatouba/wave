function w_smooth_ts, data, Width, EDGE_TRUNCATE=edge_truncate, MISSING=missing, NAN=nan
  
  @WAVE.inc
  compile_opt idl2
  
  _data = data
  
  dataTypeName = Size(_data, /TNAME)
  
   ;Check for missing
  if N_ELEMENTS(MISSING) eq 0 then begin
    case dataTypeName of
      'FLOAT' : _missing = !VALUES.F_NAN
      'DOUBLE': _missing = !VALUES.D_NAN
      'BYTE': _missing = 0B
      'LONG': _missing = -9999L
      'INT': _missing = -9999
      else: Message, 'Data type too exotic for me'
    endcase
  endif else begin
    if ~ arg_okay(missing, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('MISSING', /ARG)
    _missing = missing
  endelse
 
  _data[0] = _missing
  _data[N_ELEMENTS(_data)-1] = _missing
  
  ;Check for epsilon
  if N_ELEMENTS(EPSILON) eq 0 then begin
    case dataTypeName of
      'FLOAT' : _epsilon = (MACHAR()).eps
      'DOUBLE': _epsilon = (MACHAR(/DOUBLE)).eps
      else: _epsilon = 0
    endcase
  endif else begin
    if ~ arg_okay(epsilon, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('EPSILON', /ARG)
    _epsilon = epsilon
  endelse

  ; First check for finite elements
  pFin = where(finite(_data), cntFin, COMPLEMENT=pNoFin, NCOMPLEMENT=cntNoFin)
    
  ; Then add the test for missing values if needed
  if finite(_missing) then begin
    if cntNoFin ne 0 then _data[pNoFin] = _missing ; just in case
    pValid = where(Abs(_data - _missing) gt _epsilon, cntValid, $
                       COMPLEMENT=pNoValid, NCOMPLEMENT=cntNoValid)
  endif else begin
    pValid = TEMPORARY(pFin)
    cntValid = TEMPORARY(cntFin)
    pNoValid = TEMPORARY(pNoFin)
    cntNoValid = TEMPORARY(cntNoFin)   
  endelse
  is_Missing = cntNoValid ne 0
  is_Valid = cntValid ne 0
  
  if ~ is_Missing then return, SMOOTH(_data, Width, EDGE_TRUNCATE=edge_truncate, MISSING=missing, NAN=nan) 
  
  out = SMOOTH(_data, Width, EDGE_TRUNCATE=edge_truncate, MISSING=missing, NAN=nan) 
  mask = _data*0
  mask[pNoValid] = 100
  mask = SMOOTH(mask, Width, EDGE_TRUNCATE=edge_truncate, MISSING=missing, NAN=nan)
  
  pnok = where(mask gt 0., cntnok)
  if cntnok ne 0 then out[pnok] = _missing
  
  return, out
  
end