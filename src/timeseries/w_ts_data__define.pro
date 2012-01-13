pro w_ts_Data::cleanTS

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  fmissing = FINITE(*self.missing)
  
  if fmissing then begin
    dataTypeName = Size(*self.data, /TNAME)
    CASE dataTypeName OF
      'STRING': indices = Where(*self.data ne *self.missing, count)
      'FLOAT': indices = Where(Abs(*self.data - missing) gt (MACHAR()).eps, count)
      'DOUBLE': indices = Where( Abs(*self.data - missing) gt (MACHAR(DOUBLE=1)).eps, count)
      ELSE: indices = Where(*self.data ne missing, count)
    ENDCASE
  endif else begin
    indices = Where(finite(*self.data), count)
  endelse
  
  if count eq 0 then begin
    PTR_FREE, self.data
    PTR_FREE, self.time
    PTR_FREE, self.mask
    self.nt = 0
    Message, 'Mega problem, no element in the time serie'
    self.regular = FALSE
  endif else begin
    _data = (*self.data)[indices]
    _time = (*self.time)[indices]
    self.nt  = count
    _mask = BYTARR(count) + 1B
    PTR_FREE, self.data
    PTR_FREE, self.time
    PTR_FREE, self.mask
    self.data = PTR_NEW(_data, /NO_COPY)
    self.time = PTR_NEW(_time, /NO_COPY)
    self.mask = PTR_NEW(_mask, /NO_COPY)
  endelse
    
end

pro w_ts_Data::printMissingPeriods, T0=t0, T1=t1
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->regularTS, T0=t0, T1=t1
  t = self->getTime()
  ents = [0,~self->getMask(),0]
  ents = LABEL_REGION(ents)
  ents = ents[1:N_elements(ents)-2]
  n_ents = max(ents)
  print, ' ' + str_equiv(n_ents) + ' missing periods'
  if n_ents gt 15 then ok = DIALOG_MESSAGE(str_equiv(n_ents) + ' missing periods, sure you want to print them?', /QUESTION) else ok = 'YES'
  
  if ok eq 'No' then return
  
  for e=1, n_ents do begin
    pent = where(ents eq e)
    tent0 = t[min(pent)]
    tent1 = t[max(pent)]
    if tent0 eq tent1 then print, '  ' + TIME_to_STR(tent0) $
    else  print, '  ' + TIME_to_STR(tent0) + ' -> ' + TIME_to_STR(tent1)
  endfor
    
end

pro w_ts_Data::plotTS

  if self.description ne '' then TITLE = self.description else TITLE = self.name

  w_TimeLinePlot, self->getData(), self->getTime(), self.name, COLOR1='blue', TITLE=title, YTITLE=self.unit 

end

pro w_ts_Data::regularTS, T0=t0, T1=t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  self->cleanTS
  
  if self.nt eq 0 and (N_ELEMENTS(T0) eq 0 or N_ELEMENTS(T1) eq 0) then Message, 'Mega problem, no element in the time serie'
  
  if N_ELEMENTS(T0) ne 0 then begin  
    if ~ CHECK_WTIME(T0, OUT_QMS=_t0) then Message, WAVE_Std_Message('T0', /ARG)    
  endif else _t0 = (self->getTime())[0]
  
  if N_ELEMENTS(T1) ne 0 then begin  
    if ~ CHECK_WTIME(T1, OUT_QMS=_t1) then Message, WAVE_Std_Message('T1', /ARG)    
  endif else _t1 = (self->getTime())[self.nt-1]
  
  _time = MAKE_ENDED_TIME_SERIE(_t0, _t1, TIMESTEP=self.step, NSTEPS=nt)
  
  _data = TS_FILL_MISSING(self->getData(), self->getTime(), _time, FILL_VALUE=self->getMissing(), INDEXES=indexes)
  
  _mask = BYTARR(nt) + 1B
  if indexes[0] ne -1 then _mask[indexes] = 0B
  
  PTR_FREE, self.data
  PTR_FREE, self.time
  PTR_FREE, self.mask
  self.data = PTR_NEW(_data, /NO_COPY)
  self.time = PTR_NEW(_time, /NO_COPY)
  self.mask = PTR_NEW(_mask, /NO_COPY)
 
end

pro w_ts_Data::interpolTS, T0=t0, T1=t1
  
  self->regularTS, T0=t0, T1=t1

  pv = where(self->getMask(), cnt, NCOMPLEMENT=nc)
  if nc eq 0 then return
  if cnt eq 0 then message, 'arf'

  t = self->getTime(nt)
  d = self->getData()
  ents = [0,~self->getMask(),0]
  ents = LABEL_REGION(ents)
  ents = ents[1:N_elements(ents)-2]
  
  self->cleanTS    
  new = INTERPOL(self->getData(), self->getTime(), t) 
  
  ;Carefull with extrapolating
  if ents[0] ne 0 then begin
    val0 = d[min(where(ents eq 0))]
    message, 'carefull, extrapolating prohibited we put the nearest valid value instead.', /INFORMATIONAL
    new[where(ents eq ents[0])] = val0
  endif
  if ents[nt-1] ne 0 then begin
    val0 = d[max(where(ents eq 0))]
    message, 'carefull, extrapolating prohibited we put the nearest valid value instead.', /INFORMATIONAL
    new[where(ents eq ents[nt-1])] = val0
  endif
  
  self->addData, new, t, /REPLACE

end

function w_ts_Data::isRegular

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2   

  return, *self.regular
  
end

function w_ts_Data::getMissing

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  return, *self.missing
  
end

function w_ts_Data::getData

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  return, *self.data
  
end

function w_ts_Data::getTime, nt

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  nt = N_ELEMENTS(*self.time)
  return, *self.time
  
end

function w_ts_Data::getMask

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  return, *self.mask
  
end

function w_ts_Data::getName

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  return, self.name
  
end

pro w_ts_Data::getProperty, NAME=name, $
                            DESCRIPTION=description, $
                            UNIT=unit, $
                            TYPE=type, $                            
                            TYPE_NAME=type_name, $
                            VALID=valid, $
                            TIMESTEP=timestep, $
                            AGG_METHOD=agg_method

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  NAME=self.name
  DESCRIPTION=self.description
  UNIT=self.unit
  TYPE=self.type
  VALID=self.valid
  TIMESTEP=self.step
  AGG_METHOD=self.agg_method
  TYPE_NAME=TYPE_NAME(self.type)
  
end

pro w_ts_Data::setProperty, DESCRIPTION=description, $
                            UNIT=unit, $
                            VALID=valid, $
                            TYPE=type, $
                            AGG_METHOD=agg_method

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(DESCRIPTION) ne 0 then self.description = description
  if N_ELEMENTS(UNIT) ne 0  then self.UNIT = unit
  if N_ELEMENTS(AGG_METHOD) ne 0  then self.AGG_METHOD = agg_method
  if N_ELEMENTS(VALID) ne 0 then self.VALID = valid
  if N_ELEMENTS(TYPE) ne 0 then self.TYPE = type
    
end

pro w_ts_Data::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  PTR_FREE, self.time
  PTR_FREE, self.data
  PTR_FREE, self.mask
  PTR_FREE, self.missing
  
end

pro w_ts_Data::addData, data, time, REPLACE=replace

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_PARAMS() ne 2 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~array_processing(data, time, REP_A0=_data) then MESSAGE, '$DATA and $TIME arrays not compatible'
  if ~CHECK_WTIME(time, OUT_QMS=qms) then MESSAGE, WAVE_Std_Message('$TIME', /ARG)
  
  if ~KEYWORD_SET(REPLACE) and self.nt ne 0 then begin
    qms = [qms,self->getTime()]
    _data = [_data,self->getData()]
  endif
  
  s = sort(qms)
  if N_ELEMENTS(uniq(qms, s)) ne N_ELEMENTS(qms) then message, 'Times are not unique?'
  qms = qms[s]
  _data = _data[s]
  
  _regular = CHECK_TIMESERIE(qms, ts)
  
  PTR_FREE, self.data
  PTR_FREE, self.time
  self.data = PTR_NEW(_data, /NO_COPY)
  self.nt = N_ELEMENTS(qms)
  self.time = PTR_NEW(qms, /NO_COPY)  
  self.regular = _regular
  self.step = ts
  
  self->cleanTS
  
end

function w_ts_Data::init, data, time, NAME=name, $
                                      DESCRIPTION=description, $
                                      UNIT=unit, $
                                      TYPE=type, $
                                      VALID=valid, $
                                      AGG_METHOD=agg_method, $
                                      MISSING=missing
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
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
      IDL_LONG: _missing = -9999L
      IDL_FLOAT: _missing = !VALUES.F_NAN
      IDL_DOUBLE: _missing = !VALUES.D_NAN
      else: MESSAGE, 'Should not be there'
    endcase
  endif else _missing = missing
    
  self.agg_method = _agg_method
  self.description = _description
  self.missing = PTR_NEW(_missing, /NO_COPY)
  self.name = _name
  self.type = _type
  self.unit = _unit
  self.valid = _valid
  
  self->addData, _data, qms
  
  self->cleanTS
  
  return, 1
    
end

pro w_ts_Data__Define

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  class = {w_ts_Data        , $
           name:           '', $ ; The name of the variable
           description:    '', $ ; A short description of the variable
           nt:             0L, $ ; Number of times
           time:    PTR_NEW(), $ ; Array of QMS/ABS_DATE of nt elements
           data:    PTR_NEW(), $ ; Data array of nt elements
           mask:    PTR_NEW(), $ ; mask array of nt elements
           unit:           '', $ ; The variable unit
           type:          0L , $ ; The variable type (IDL)
           regular:     FALSE, $ ; If the TS is regular
           step : {TIME_STEP}, $ ; Probale Timestep
           agg_method: 'NONE', $ ; Aggregation method. See `TS_AGG`
           valid:     'POINT', $ ; If it is aggregated
           missing: PTR_NEW()  $ ; Missing data values
           }

end