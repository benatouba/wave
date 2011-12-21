function w_ts_MakeData, data, time, NAME=name, $
                                    DESCRIPTION=description, $
                                    UNIT=unit, $
                                    TYPE=type, $
                                    VALID=valid, $
                                    AGG_METHOD=agg_method, $
                                    MISSING=missing

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
;  ON_ERROR, 2
  
  if N_PARAMS() ne 2 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~array_processing(data, time, REP_A0=_data) then MESSAGE, '$DATA and $TIME arrays not compatible'
  if ~CHECK_WTIME(time, OUT_QMS=qms) then MESSAGE, WAVE_Std_Message('$TIME', /ARG)
  
  if N_ELEMENTS(NAME) eq 0 then _name = 'Data' else _name = name
  if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
  if N_ELEMENTS(UNIT) eq 0 then _unit = '' else _unit = unit
  if N_ELEMENTS(TYPE) eq 0 then _type = SIZE(_data, /TYPE) else _type = type
  if N_ELEMENTS(AGG_METHOD) eq 0 then _agg_method = 'MEAN' else _agg_method = agg_method
  if N_ELEMENTS(VALID) eq 0 then _valid = 'POINT' else _valid = valid
  
  ;TODO: check input
  
  
  case _type of
    IDL_STRING: _data = STRING(_data)
    IDL_BYTE: _data = LONG(_data)
    IDL_INT: _data = LONG(_data)
    IDL_LONG: _data = LONG(_data)
    IDL_UINT: _data = LONG(_data)
    IDL_ULONG: _data = LONG(_data)
    IDL_LONG64: _data = LONG(_data)
    IDL_ULONG64: _data = LONG(_data)
    IDL_FLOAT: _data = FLOAT(_data)
    IDL_DOUBLE: _data = DOUBLE(_data)
    else: MESSAGE,WAVE_Std_Message('$TYPE', /ARG)    
  endcase
  
  _type = SIZE(_data, /TYPE)

  if N_ELEMENTS(MISSING) eq 0 then begin
    case _type of
      IDL_STRING: _missing = ''
      IDL_LONG: _missing = -9999
      IDL_FLOAT: _missing = !VALUES.F_NAN
      IDL_DOUBLE: _missing = !VALUES.D_NAN
      else: MESSAGE, 'Should not be there'
    endcase
  endif else _missing = missing
  
  _regular = CHECK_TIMESERIE(qms, ts)

  
  out = {w_ts_Data}  
  
  out.agg_method = _agg_method
  out.data = PTR_NEW(_data, /NO_COPY)
  out.description = _description
  out.missing = PTR_NEW(_missing, /NO_COPY)
  out.name = _name
  out.nt = N_ELEMENTS(qms)
  out.regular = _regular
  out.step = ts
  out.time = PTR_NEW(qms, /NO_COPY)
  out.type = _type
  out.unit = _unit
  out.valid = _valid
  
  return, out
  
end

