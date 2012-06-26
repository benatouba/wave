; docformat = 'rst'
;+
;  
; Object to store, retrieve and analyse data timeseries.
; 
; The basic principle of this object is that input data points and
; associated time stamps are internaly stored and retrieved on demand
; for any time period selected by the user with the `setPeriod` procedure.
; 
; A timeserie is therefore defined by three explicit parameters: t0, t1 and
; the timestep. The later can be either a regular timestep defined by its "dms" 
; (delta milli-second) or a monthly or yearly timestep.
; 
; Definition::
;       
;         class = {w_ts_Data              , $
;                  name:           ''     , $ ; The name of the variable
;                  description:    ''     , $ ; A short description of the variable
;                  unit:           ''     , $ ; The variable unit
;                  type:           0L     , $ ; The variable type (IDL)
;                  nt:             0L     , $ ; Number of times
;                  t0:             0LL    , $ ; Start Time
;                  t1:             0LL    , $ ; End Time
;                  step:           ''     , $ ; Either IRREGULAR, TIMESTEP, MONTH or YEAR
;                  timestep :      0LL    , $ ; Timestep. Unit is millisecond, or MONTH, or YEAR
;                  agg_method: 'NONE'     , $ ; Aggregation method. See `TS_AGG`
;                  validity:  'POINT'     , $ ; Temporal validity of the data values ('INTEGRAL' or 'POINT')
;                  missing: PTR_NEW()     , $ ; Missing data value
;                  time:    PTR_NEW()     , $ ; Array of QMS/ABS_DATE
;                  data:    PTR_NEW()       $ ; Array of data values
;                  }
;
; :Categories:
;    Timeseries, Objects
;
; :History:
;     Written by FaM, 2012
;           
;-

;+
; :Description:
;    Creates the object instance.
;    
;    `data` and `time` arrays are not required to initialize the object, but 
;    the instance will be operational only after the first call to add_data()
;    It is also a good idea to give values to the `NAME`,`UNIT`,`VALIDITY`,
;    `AGG_METHOD`,and `MISSING` keywords. If the timeserie is exotic, 
;    the `STEP` and `TIMESTEP` keywords are highly recommended.
;
; :Params:
;    data: in, optional
;          array of data values
;          (you can also add it it later with `w_ts_Data::addData`)
;    time: in, optional
;          array of time values
;          (you can also add it it later with `w_ts_Data::addData`)
;          
; :Keywords:
;    NAME: in, optional, type=string
;          The name of the variable
;    DESCRIPTION: in, optional, type=string
;                 A short description of the variable
;    UNIT: in, optional, type=string
;          The variable unit
;    VALIDITY: in, optional, type=string
;              Temporal validity of the data values ('INTEGRAL' or 'POINT')
;    AGG_METHOD: in, optional, type=string
;                Aggregation method. See `TS_AGG`
;    T0: in, optional, type=qms/{ABS_DATE}
;        The first time of the period (see `w_ts_Data::setPeriod`)
;        Ignored if `data` and `time` are not set
;    T1: in, optional, type=qms/{ABS_DATE}
;        The last time of the period (see `w_ts_Data::setPeriod`)
;        Ignored if `data` and `time` are not set
;    STEP: in, optional, type=string
;          Either IRREGULAR, TIMESTEP, MONTH or YEAR (see `w_ts_Data::addData`)
;          Ignored if `data` and `time` are not set
;    TIMESTEP: in, optional, type=DMS/{TIME_STEP}
;              Timeserie timestep (see `w_ts_Data::addData`)
;              Ignored if `data` and `time` are not set
;    MISSING: in, optional, type=string
;             value for missing data in the timeserie (see `w_ts_Data::addData`)
;             Ignored if `data` and `time` are not set
;
;-
function w_ts_Data::init, data, time, NAME=name, $
                                      DESCRIPTION=description, $
                                      UNIT=unit, $
                                      VALIDITY=validity, $
                                      AGG_METHOD=agg_method, $
                                      T0=t0, $
                                      T1=t1, $
                                      STEP=step, $
                                      TIMESTEP=timestep, $ 
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
  
  if N_ELEMENTS(NAME) eq 0 then _name = 'Data' else _name = name
  if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
  if N_ELEMENTS(UNIT) eq 0 then _unit = '' else _unit = unit
  if N_ELEMENTS(AGG_METHOD) eq 0 then _agg_method = 'MEAN' else _agg_method = agg_method
  if N_ELEMENTS(VALIDITY) eq 0 then _validity = 'POINT' else _validity = validity
    
  self.agg_method = _agg_method
  self.description = _description
  self.name = _name
  self.unit = _unit
  self.validity = _validity
  
  if N_PARAMS() eq 2 then self->addData, data, time, STEP=step, TIMESTEP=timestep, MISSING=missing
  self->setPeriod, T0=t0, T1=t1
  
  return, 1
    
end

;+
; :Description:
;    Destroys the object instance and frees the memory.
;
;-
pro w_ts_Data::cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  PTR_FREE, self.time
  PTR_FREE, self.data
  PTR_FREE, self.missing
  
end

;+
; :Description:
;    Get access to some object properties.
;          
; :Keywords:
;    NAME: out, type=string
;          The name of the variable
;    DESCRIPTION: in, type=string
;                 A short description of the variable
;    UNIT: out, type=string
;          The variable unit
;    VALIDITY: out, type=string
;              Temporal validity of the data values ('INTEGRAL' or 'POINT')
;    TYPE: out, type=long
;          data type
;    AGG_METHOD: out, type=string
;                Aggregation method. See `TS_AGG`
;    STEP: out, type=string
;          Either IRREGULAR, TIMESTEP, MONTH or YEAR 
;    T0: out, type=qms/{ABS_DATE}
;        The first time of the period
;    T1: out, type=qms/{ABS_DATE}
;        The last time of the period
;    NT: out, type=qms/{ABS_DATE}
;        The number of times in the period
;    TIMESTEP: out, type={TIME_STEP}
;              Timeserie timestep
;    MISSING: out, type=string
;             value for missing data in the timeserie
;
;-
pro w_ts_Data::getProperty, NAME=name, $
                            DESCRIPTION=description, $
                            UNIT=unit, $
                            TYPE=type, $                            
                            VALIDITY=validity, $
                            STEP=step, $
                            T0=t0, $
                            T1=t1, $
                            NT=nt, $
                            TIMESTEP=timestep, $
                            MISSING=missing, $
                            AGG_METHOD=agg_method

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  NAME=self.name
  DESCRIPTION=self.description
  UNIT=self.unit
  TYPE=self.type
  VALIDITY=self.validity
  STEP=self.step
  TIMESTEP=self.timestep
  if PTR_VALID(self.missing) then MISSING=*self.missing
  AGG_METHOD=self.agg_method
  T0=self.t0
  T1=self.t1
  NT=self.nt
 
end

;+
; :Description:
;    Returns the properties of the object one at a time. Only
;    the properties accessible with the homonym routine are 
;    accessible here.
;
;
; :Params:
;    thisProperty: in, required
;                  A string variable that is equivalent to a field in the object's    
;                  class structure. See the getProperty routine for which properties  
;                  can be returned. The property is case insensitive.
;                  
; :Returns:
;    The value of a particular object property. Note that pointer       
;    properties will return the variable the pointer points to.
;                    
; :History:
;    Modifications::
;     (c) David Fanning
;     FaM, 2012: Adapted to the WAVE
;
;-
function w_ts_Data::getProperty, thisProperty

  ; SET UP ENVIRONNEMENT
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  avail = ['NAME', $
           'DESCRIPTION', $
           'UNIT', $
           'TYPE', $
           'VALIDITY', $
           'STEP', $
           'TIMESTEP', $
           'MISSING', $
           'AGG_METHOD', $
           'T0', $
           'T1', $
           'NT']
  
  ; Check if ok
  index = Where(StrPos(avail, str_equiv(thisProperty)) EQ 0, count)
  index = index[0]    
  CASE count OF
    0: Message, 'Property ' + StrUpCase(thisProperty) + ' could not be found.'
    1:
    ELSE: Message, 'Ambiguous property. Use more characters to specify it.'
  ENDCASE
  
  ; Now david fanning's stuff
  
  ; Get the self structure as a structure, rather than as an object.
  Call_Procedure, StrLowCase(Obj_Class(self)) + '__define', classStruct
  
  ; Find the property in this class structure.
  index = Where(StrPos(Tag_Names(classStruct), StrUpCase(thisProperty)) EQ 0, count)
  index = index[0]
  
  ; What happened?
  CASE count OF
    0: Message, 'Property ' + StrUpCase(thisProperty) + ' could not be found.'
    1: propertyValue = self.(index)
    ELSE: Message, 'Ambiguous property. Use more characters to specify it.'
  ENDCASE
  
  ; If this is a pointer, you want the thing pointed to.
  IF Size(propertyValue, /TNAME) EQ 'POINTER' THEN propertyValue = *propertyValue
  return, propertyValue
  
end

;+
; :Description:
;    Resets some object properties.   
;          
; :Keywords:
;    NAME: in, optional, type=string
;          The name of the variable
;    DESCRIPTION: in, optional, type=string
;                 A short description of the variable
;    UNIT: in, optional, type=string
;          The variable unit
;    VALIDITY: in, optional, type=string
;              Temporal validity of the data values ('INTEGRAL' or 'POINT')
;    AGG_METHOD: in, optional, type=string
;                Aggregation method. See `TS_AGG`
;
;-
pro w_ts_Data::setProperty, NAME=name, $
                            DESCRIPTION=description, $
                            UNIT=unit, $
                            VALIDITY=validity, $
                            AGG_METHOD=agg_method

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  if N_ELEMENTS(NAME) ne 0 then self.name = name
  if N_ELEMENTS(DESCRIPTION) ne 0 then self.description = description
  if N_ELEMENTS(UNIT) ne 0  then self.unit = unit
  if N_ELEMENTS(AGG_METHOD) ne 0  then self.agg_method = agg_method
  if N_ELEMENTS(VALIDITY) ne 0 then self.validity = validity
    
end

;+
; :Description:
;    Adds data to the timeserie or replaces old one.
;
; :Params:
;    data: in, required
;          array of data values
;    time: in, required
;          array of time values
;          
; :Keywords:
;    STEP: in, optional, type=string
;          Either IRREGULAR, TIMESTEP, MONTH or YEAR. 
;          First call only, ignored afterwards
;    TIMESTEP: in, optional, type=DMS/{TIME_STEP}
;              Timeserie timestep, unit is dms or month or years
;              First call only, ignored afterwards  
;    MISSING: in, optional, type=string
;             value for missing data in the timeserie. 
;             First call only, ignored afterwards            
;    REPLACE: in, optional, type=boolean
;             replace the old data?
;
;-
pro w_ts_Data::addData, data, time, STEP=step, TIMESTEP=timestep, MISSING=missing, REPLACE=replace

  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_PARAMS() ne 2 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~array_processing(data, time, REP_A0=_data) then MESSAGE, '$DATA and $TIME arrays not compatible'
  if ~CHECK_WTIME(time, OUT_QMS=qms) then MESSAGE, WAVE_Std_Message('$TIME', /ARG)
  if N_ELEMENTS(data) lt 2 then Message, '$DATA must contain at least two elements.'
  if ptr_valid(self.time) then _first = FALSE else _first = TRUE
  if _first then _replace = TRUE else _replace = KEYWORD_SET(REPLACE)
  
  ;Type
  if _first then begin
    self.type = SIZE(_data, /TYPE) ; first call
  endif else begin
    if self.type ne SIZE(_data, /TYPE) then Message, 'Data type do not match previous entry (was ' + $
      type_name(self.type) + ', now ' + SIZE(_data, /TNAME) +').'
  endelse
  
  ; Define a missing flag
  if _first then begin ;first call
    if N_ELEMENTS(missing) eq 0 then begin
      case self.type of
        IDL_INT: _missing = FIX(-9999)
        IDL_LONG: _missing = LONG(-9999)
        IDL_LONG64: _missing = LONG64(-9999)
        IDL_FLOAT: _missing = !VALUES.F_NAN
        IDL_DOUBLE: _missing = !VALUES.D_NAN
        else: MESSAGE, 'With data array of type: ' + type_name(self.type) + ', the MISSING keyword must be explicitly specified'
      endcase
    endif else _missing = missing
    PTR_FREE, self.missing
    self.missing = PTR_NEW(_missing, /NO_COPY)
  endif
  
  
  ; Steps
  if _first then begin ; first call
  
    ; Did the user specified some stuffs allready
    if N_ELEMENTS(STEP) ne 0 then self.step = str_equiv(step)
    if N_ELEMENTS(TIMESTEP) ne 0 then begin
      if ~ CHECK_WTIMESTEP(timestep, OUT_DMS=_ts) then Message, WAVE_Std_Message('TIMESTEP', /ARG)
      self.timestep = _ts
    endif
    
    if self.timestep eq 0 then begin ; find out by myself
      case self.step of
        'MONTH': _timestep = 1
        'YEAR': _timestep = 1
        else: begin
          ok = check_timeserie(qms, _timestep, CONFIDENCE=confidence)
          if confidence lt 0.75 then begin
            Message, 'Probable timestep chosen with a low confidence of: ' + $
              STRING(confidence, FORMAT='(F5.2)') + '. You should check it.', /INFORMATIONAL
          endif
        end
      endcase
    endif else _timestep = timestep
    
    if ~ CHECK_WTIMESTEP(_timestep, OUT_DMS=dms) then message, WAVE_Std_Message('TIMESTEP', /ARG)
    if dms le 0 then message, 'Timestep le 0?'
    
    if self.step eq '' then begin
      self.step = 'TIMESTEP'
      if dms ge 2419200000LL and dms le 2678400000LL then begin ; We are probably in a monthly timeserie
        message, 'I think this is a monthly timestep', /INFORMATIONAL
        self.step = 'MONTH'
        dms = 1
      endif
      if dms ge 31449600000LL and dms le 31622400000LL then begin ; We are probably in a yearly timeserie
        message, 'I think this is a yearly timestep', /INFORMATIONAL
        self.step = 'YEAR'
        dms = 1
      endif
    endif
    
    self.timestep = dms
    
    if self.step eq 'IRREGULAR' then message, 'IRREGULAR timestep not implemented yet.'
    
  endif
 
  ; Check if we have to add the data or replace it
  if ~ _replace then begin
    qms = [*self.time, qms]
    _data = [*self.data, _data]
  endif

  ; Check for non-finite values in the timeserie anyway
  pf = where(~finite(_data), cntf)
  if cntf ne 0 then _data[pf]=*self.missing
  
  ; TODO: Remove non valid points: yes or no?   
;  fmissing = finite(*self.missing)  
;  if fmissing then begin
;    CASE self.type OF
;      'FLOAT': indices = Where(Abs(_data - *self.missing) gt (MACHAR()).eps, count)
;      'DOUBLE': indices = Where(Abs(_data - *self.missing) gt (MACHAR(/DOUBLE)).eps, count)
;      ELSE: indices = Where(_data ne *self.missing, count)
;    ENDCASE
;  endif else begin
;    indices = where(finite(_data), count)
;  endelse  
;  _data = _data[indices > 0] ; so if there are no valid data, the first element is kept
;  qms = qms[indices > 0] ; so if there are no valid data, the first element is kept
    
  ; Check for the time serie validity
  s = sort(qms)
  if N_ELEMENTS(uniq(qms, s)) ne N_ELEMENTS(qms) then message, 'Times are not unique.'
  qms = qms[s]
  _data = _data[s]
  
  ; Done
  PTR_FREE, self.data
  PTR_FREE, self.time
  self.data = PTR_NEW(_data, /NO_COPY)
  self.time = PTR_NEW(qms, /NO_COPY)
  
  self->setPeriod, DEFAULT=_first
  
end

;+
; :Description:
;    Sets the focus period to the shortest period 
;    enclosing all valid data points or to a user 
;    defined period.
;    Once the user set t0 and/or t1, this parameter are 
;    remembered for later calls. Set the `DEFAULT` keyword
;    to reset to the default period.
;
; :Keywords:
;    T0: in, optional, type=qms/{ABS_DATE}
;        the first time of the desired period
;    T1: in, optional, type=qms/{ABS_DATE}
;        the last time of the desired period
;    DEFAULT: in, optional, type=boolean
;             Reset to the default period         
;
;-
pro w_ts_Data::setPeriod, T0=t0, T1=t1, DEFAULT=default

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~ptr_valid(self.time) then return ; no data yet
  
  if KEYWORD_SET(DEFAULT) then begin
    _t0 = (*self.time)[0]
    _t1 = (*self.time)[N_ELEMENTS(*self.time)-1]
  endif else begin
    _t0 = self.t0
    _t1 = self.t1
  endelse
  
  ; User wants something new ? 
  if N_ELEMENTS(T0) ne 0 then begin
    if ~ CHECK_WTIME(T0, OUT_QMS=_t0) then Message, WAVE_Std_Message('T0', /ARG)
    self.t0 = _t0
  endif  
  if N_ELEMENTS(T1) ne 0 then begin
    if ~ CHECK_WTIME(T1, OUT_QMS=_t1) then Message, WAVE_Std_Message('T1', /ARG)
    self.t1 = _t1
  endif
  
  ; Check the time
  case self.step of
    'TIMESTEP': begin
      _nt = ABS(_t1 - _t0) / self.timestep
      if _nt ne DOUBLE(_t1 - _t0) / self.timestep then begin
        Message, 'T0, T1 and TIMESTEP are incompatible. Reset to default.', /INFORMATIONAL
        self->setPeriod, /DEFAULT
      endif
      _nt+=1
    end
    'MONTH': begin
      _t0 = w_time_to_month(_t0)
      _t1 = w_time_to_month(_t1)
      _nt = ABS(_t1 - _t0) / self.timestep
      if _nt ne DOUBLE(_t1 - _t0) / self.timestep then begin
        Message, 'T0, T1 and TIMESTEP are incompatible. Reset to default.', /INFORMATIONAL
        self->setPeriod, /DEFAULT
      endif
      _t0 = w_month_to_time(_t0)
      _t1 = w_month_to_time(_t1)
      _nt+=1
    end
    'YEAR': begin
      _t0 = w_time_to_year(_t0)
      _t1 = w_time_to_year(_t1)
      _nt = ABS(_t1 - _t0) / self.timestep
      if _nt ne DOUBLE(_t1 - _t0) / self.timestep then begin
        Message, 'T0, T1 and TIMESTEP are incompatible. Reset to default.', /INFORMATIONAL
        self->setPeriod, /DEFAULT
      endif
      _t0 = w_year_to_time(_t0)
      _t1 = w_year_to_time(_t1)
      _nt+=1
    end
  endcase
  
  ;Fill
  self.t0 = _t0
  self.t1 = _t1
  self.nt = _nt
  
end

;+
; :Description:
;    Get the time
;
; :Keywords:
;    nt: out
;        the number of times
;
; :Returns:
;   A time array of nt elements
;
;-
function w_ts_Data::getTime, NT=nt

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if ~ ptr_valid(self.time) then return, dummy
    
  nt = self.nt      
  ; Check the time
  case self.step of
    'TIMESTEP': time = L64INDGEN(nt) * self.timestep + self.t0
    'MONTH': time = w_month_to_time(LINDGEN(nt) * self.timestep + w_time_to_month(self.t0))
    'YEAR': time = w_year_to_time(LINDGEN(nt) * self.timestep + w_time_to_year(self.t0))
  endcase   
  
  return, time

end

;+
; :Description:
;    Get the data
;  
; :Keywords:
;    nt: out
;        the number of times
;
; :Returns:
;   A data array of nt elements
;
;-
function w_ts_Data::getData, NT=nt

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~ ptr_valid(self.data) then return, dummy
  t = self->getTime(NT=nt)
  if nt eq 1 then begin
    p = where(*self.time eq t[0], cnt)
    if cnt ne 0 then return, (*self.data)[p] else  return, *self.missing
  endif
  return, w_ts_fill_missing(*self.data, *self.time, self->getTime(NT=nt), FILL_VALUE=*self.missing)
  
end


;+
; :Description:
;   The validity of the data timeserie
;   
; :Keywords:
;    nt: out
;        the number of times
;
; :Returns:
;   An array of nt elements (valid = 1)
;
;-
function w_ts_Data::valid, NT=nt

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  data = self->getData(NT=nt)
  _mask = BYTARR(nt)
  
  missing = *self.missing
  
  fmissing = finite(missing)  
  if fmissing then begin
    CASE type_name(self.type) OF
      'FLOAT': indices = Where(Abs(data - missing) gt (MACHAR()).eps, count)
      'DOUBLE': indices = Where(Abs(data - missing) gt (MACHAR(/DOUBLE)).eps, count)
      ELSE: indices = Where(data ne missing, count)
    ENDCASE
  endif else begin
    indices = where(finite(data), count)
  endelse
  
  if count ne 0 then _mask[indices] = 1
  
  return, _mask
  
end


;+
; :Description:
;    Short way to get acccess to a clean timeserie 
;    (without missing values but possibly irregular or empty)
;
; :Params: 
;    data: out
;          the data
;    time: out
;          the data
;    nt: out
;        number of times
;
;-
pro w_ts_Data::cleanTS, data, time, nt

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  data = self->getData()
  time = self->getTime() 

  p = where(self->Valid(), cnt)
  if cnt ne 0 then begin
    data = data[p]
    time = time[p]    
  endif else begin
    undefine, data, time  
  endelse

  nt = N_ELEMENTS(time)
  
end

;+
; :Description:
;    to know if there are missing values in the TS
;
; :Returns:
;    1 if the TS is clean, 0 otherwise
;
;-
function w_ts_Data::isClean
  
  return, TOTAL(self->valid()) eq self.nt
  
end
  
  
;+
; :Description:
;    Just prints the missing periods of a TS in the console
;
;-
pro w_ts_Data::printMissingPeriods
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  t = self->getTime()
  ents = LABEL_REGION([0,~self->valid(),0])
  ents = ents[1:N_elements(ents)-2]
  n_ents = max(ents)
  print, '  ' + str_equiv(n_ents) + ' missing periods'
  if n_ents gt 15 then ok = DIALOG_MESSAGE(str_equiv(n_ents) + ' missing periods, sure you want to print them?', /QUESTION) else ok = 'YES'
  
  if ok eq 'No' then return
  
  for e=1, n_ents do begin
    pent = where(ents eq e, cp)
    tent0 = t[min(pent)]
    tent1 = t[max(pent)]
    if tent0 eq tent1 then print, '  ' + TIME_to_STR(tent0) $
    else  print, '  ' + TIME_to_STR(tent0) + ' -> ' + TIME_to_STR(tent1) + ' ('+ str_equiv(cp)+' steps)' 
  endfor
    
end

;+
; :Description:
;    Just plots the time serie
;    
; :Keywords:
;    TITLE_INFO: in, optional
;                something to add to the title
;
;-
pro w_ts_Data::plotTS, TITLE_INFO=title_info

  title = self.name
  if self.description ne '' then title = title + ': ' + self.description 
  if N_ELEMENTS(TITLE_INFO) ne 0 then title = title_info + title
  
  if FINITE(*self.missing) then begin
     self->cleanTS, dclean
     min_value=min(dclean)
  endif  
  
  if total(self->valid()) ne 0 then w_gr_tzplot, self->getTime(), self->getData(), TITLE=title, YTITLE=self.unit, $
          THICK=2, COLOR='blue', position=[0.1,0.15,0.94,0.82], CHARSIZE=1., MIN_value=min_value

end


;+
; :Description:
; 
;   Replaces all invalid data values by linear interpolation.
;   In case extrapolation is needed, a warning is sent and the closest
;   data value is chosen instead.
;   
;   Carefull: this change is irreversible, there is no way back to
;   the original data afterwards.
;   
; :Keywords:
;    T0: in, optional
;        if the interpolation have to be made on a section of the data only (remaining data is unchanged)
;    T1: in, optional
;        if the interpolation have to be made on a section of the data only (remaining data is unchanged)
;        
;-
pro w_ts_Data::interpol, T0=t0, T1=t1

   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
    
  pv = where(self->valid(), cnt, NCOMPLEMENT=nc)
  if nc eq 0 then return ;nothing to do
  if cnt eq 0 then message, 'No valid data values'

  t = self->getTime(nt)
  old = self->getdata()
  self->cleanTS, data, time, nt
  new = INTERPOL(data, time, t) 
  
  if N_ELEMENTS(T0) ne 0 then begin
    if ~ check_WTIME(t0, OUT_QMS= it0) then message, WAVE_Std_Message('T0')
    v = 0 > VALUE_LOCATE(t, it0) < (nt-1)
    p = v[0] 
    if p gt 0 then new[0:p-1] = old[0:p-1]
  endif
  if N_ELEMENTS(T1) ne 0 then begin
    if ~ check_WTIME(t1, OUT_QMS= it1) then message, WAVE_Std_Message('T0')
    v = 0 > VALUE_LOCATE(t, it1) < (nt-1)
    p = v[0] 
    if p lt nt-1 then new[p+1:*] = old[p+1:*]
  endif
         
  ;Carefull with extrapolating
  ents = LABEL_REGION([0,~self->valid(),0])
  ents = ents[1:N_elements(ents)-2]
  if ents[0] ne 0 then begin
    val0 = (self->getData())[min(where(ents eq 0))]
    message, self.name + ': carefull, extrapolating prohibited we put the nearest valid value instead.', /INFORMATIONAL
    new[where(ents eq ents[0])] = val0
  endif
  if ents[N_ELEMENTS(ents)-1] ne 0 then begin
    val0 = (self->getData())[max(where(ents eq 0))]
    message, self.name + ': carefull, extrapolating prohibited we put the nearest valid value instead.', /INFORMATIONAL
    new[where(ents eq ents[nt-1])] = val0
  endif
  
  self->addData, new, t, /REPLACE

end

;+
; :Description:
;   Replaces data within a period with a value (if omitted, MISSING)
; 
; :Params:
;    value: in, optional
;           
; :Keywords:
;    T0: in, optional
;        first step where to put the value
;    T1: in, optional
;        last step where to put the value
;        
;-
pro w_ts_Data::insertValue, value, T0=t0, T1=t1

   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  t = self->getTime(nt)
  d = self->getdata()
  
  if N_ELEMENTS(value) eq 0 then _val = *self.missing else _val = value
  
  if N_ELEMENTS(_val) ne 0 then Message, 'VALUE must be as scalar'
  
  if N_ELEMENTS(T0) ne 0 and N_ELEMENTS(T1) ne 0 then begin
    if ~ check_WTIME(t0, OUT_QMS= it0) then message, WAVE_Std_Message('T0')
    v = 0 > VALUE_LOCATE(t, it0) < (nt-1)
    p0 = v[0] 
    if ~ check_WTIME(t1, OUT_QMS= it1) then message, WAVE_Std_Message('T1')
    v = 0 > VALUE_LOCATE(t, it1) < (nt-1)
    p1 = v[0] 
    d[p0:p1] = _val
  endif
  
  self->addData, d, t, /REPLACE

end


;+
; :Description:
;    Aggregates the timeserie data and creates a new TS object with the
;    new timeserie.
;    
;    See `TS_AGG` for more info on the aggregation.
;
; :Keywords:
;    DAY: in, optional, default=none
;         set to day interval (e.g: 1, or 7) to compute 
;         daily or seven-daily statistics
;    HOUR: in, optional, default=none
;         set to hourly interval (e.g: 1, or 6) to compute 
;         hourly or six-hourly statistics
;    MONH: in, optional, default=none
;          set to compute monthly statistics
;    YEAR: in, optional, default=none
;          set to compute yearly statistics    
;    NEW_TIME: in, optional, type = {ABS_DATE}/qms, default=none
;              ignored if `DAY` or `HOUR` are set. set this value to 
;              any time serie of n+1 elements. The ouptut will contain
;              n elements of the statistics for each interval [t, t+1]
;              (t excluded)
;              Ignored if HOUR, DAY, MONTH or YEAR are set
;    MIN_SIG: in, optional, default=none
;             if set, all intervals having less than MIN_SIG 
;             (0<min_sig<1) will be set to missing
;    MIN_NSIG: in, optional, default = none
;              if set, all intervals having less than MIN_NSIG 
;              valid values will be set to missing
;              MIN_NSIG can be eather a scalar or an array of the size
;              of the number of intervals (N_ELEMENTS(NEW_TIME)- 1)
;              Ignored if MIN_SIG is set
;             
; :Returns:
;    A new object with the aggregated data
;
;-
function w_ts_Data::aggregate, DAY=day, HOUR=hour, MONTH=month, YEAR=year, $
                                NEW_TIME=new_time, MIN_SIG=min_sig, MIN_NSIG=min_nsig

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(HOUR) ne 0 then begin
    d = MAKE_ABS_DATE(QMS=self.t0-1LL)
    start_d = QMS_TIME(YEAR=D.year,MONTH=D.month,DAY=D.day,HOUR=D.hour)
    d = MAKE_ABS_DATE(QMS=self.t1)
    end_D = QMS_TIME(YEAR=D.year,MONTH=D.month,DAY=D.day,HOUR=D.hour)
    if end_D lt self.t1 then end_D += H_QMS
    new_time = MAKE_ENDED_TIME_SERIE(start_d, end_D, TIMESTEP=H_QMS * LONG64(HOUR))
    step = 'TIMESTEP'
    timestep = H_QMS * LONG64(HOUR)
  endif
  
  if N_ELEMENTS(DAY) ne 0 then begin
    d = MAKE_ABS_DATE(QMS=self.t0-1LL)
    start_d = QMS_TIME(YEAR=D.year,MONTH=D.month,DAY=D.day)
    d = MAKE_ABS_DATE(QMS=self.t1)
    end_D = QMS_TIME(YEAR=D.year,MONTH=D.month,DAY=D.day)
    if end_D lt self.t1 then end_D += D_QMS
    new_time = MAKE_ENDED_TIME_SERIE(start_d, end_D, TIMESTEP=D_QMS * LONG64(DAY))
    step = 'TIMESTEP'
    timestep = D_QMS * LONG64(DAY)
  endif
  
  if N_ELEMENTS(MONTH) ne 0 then begin
    t0 = w_time_to_month(self.t0-1LL)
    t1 = w_time_to_month(self.t1)
    if t1 eq w_time_to_month(self.t1-1LL) then t1 += 1
    new_time = w_month_to_time(INDGEN(t1-t0 + 1)  + t0)
    step = 'MONTH'
    timestep = 1
  endif
  
  if N_ELEMENTS(YEAR) ne 0 then begin
    t0 = w_time_to_year(self.t0-1LL)
    t1 = w_time_to_year(self.t1)
    if t1 eq w_time_to_year(self.t1-1LL) then t1 += 1
    new_time = w_year_to_time(INDGEN(t1-t0 + 1) + t0)
    step = 'YEAR'
    timestep = 1
  endif
  
  if N_ELEMENTS(MIN_SIG) ne 0 then begin
    if N_ELEMENTS(step) eq 0 then MESSAGE, 'MIN_SIG is applicable with H, D, M, Y positional keywords only.'
    case step of
      'YEAR': begin
        if self.step eq 'YEAR' then Message, 'No'
        if self.step eq 'MONTH' then n = 12
        if self.step eq 'TIMESTEP' then begin
          n = 365 + GEN_switch_year(w_time_to_year(new_time[0:N_ELEMENTS(new_time)-2]))
          n = n *  D_QMS / self.timestep
        endif
      end
      'MONTH': begin
        if self.step eq 'YEAR' then Message, 'No'
        if self.step eq 'MONTH' then Message, 'No'
        if self.step eq 'TIMESTEP' then begin
          monthly_time = MAKE_ABS_DATE(QMS=new_time[0:N_ELEMENTS(new_time)-2])
          n = GEN_month_days(monthly_time.month, monthly_time.year)
          n = n *  D_QMS / self.timestep
        endif
      end
      'TIMESTEP': begin
        if self.step eq 'YEAR' then Message, 'No'
        if self.step eq 'MONTH' then Message, 'No'
        if self.step eq 'TIMESTEP' then n = REPLICATE(timestep / self.timestep, N_ELEMENTS(new_time)-1)
      end
    endcase
    min_nsig = FLOAT(0 > MIN_SIG < 1) * n
  endif
  
  if total(self->valid()) eq 0 then begin
    agg_time = new_time[1:*]
    agg = REPLICATE(*self.missing, N_ELEMENTS(agg_time))  
  endif else begin
    TS_AGG, self->getData(), self->getTime(), agg, agg_time, $
    NEW_TIME=new_time, MIN_NSIG=min_nsig, $
    AGG_METHOD=self.agg_method, MISSING=*self.missing
  endelse
      
  out= OBJ_NEW('w_ts_Data', agg, agg_time, $
    NAME=self.name, $
    DESCRIPTION=self.description, $
    UNIT=self.unit, $
    VALIDITY='INTERVAL', $
    AGG_METHOD=self.agg_method, $
    STEP=step, $
    TIMESTEP=timestep, $
    MISSING=*self.missing)
    
  return, out
  
end

;+
; :Description:
;    Makes a copy of the time serie
; 
; :Returns:
;    a copy of the time serie
;-
function w_ts_Data::copy

  out= OBJ_NEW('w_ts_Data', *self.data, *self.time, $
    NAME=self.name, $
    DESCRIPTION=self.description, $
    T0=self.t0, $
    T1=self.t1, $
    UNIT=self.unit, $
    VALIDITY=self.validity, $
    AGG_METHOD=self.agg_method, $
    STEP=self.step, $
    MISSING=*self.missing)
      
  return, out

end

;+
;   Class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;           class definition as a structure variable
;           
;-
pro w_ts_Data__Define, class

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  class = {w_ts_Data              , $
           name:           ''     , $ ; The name of the variable
           description:    ''     , $ ; A short description of the variable
           unit:           ''     , $ ; The variable unit
           type:           0L     , $ ; The variable type (IDL)
           nt:             0L     , $ ; Number of times
           t0:             0LL    , $ ; Start Time
           t1:             0LL    , $ ; End Time
           step:           ''     , $ ; Either IRREGULAR, TIMESTEP, MONTH or YEAR
           timestep:       0LL    , $ ; Timestep
           agg_method: 'NONE'     , $ ; Aggregation method. See `TS_AGG`
           validity:  'POINT'     , $ ; Temporal validity of the data values ('INTEGRAL' or 'POINT')
           missing: PTR_NEW()     , $ ; Missing data value
           time:    PTR_NEW()     , $ ; Array of QMS/ABS_DATE
           data:    PTR_NEW()       $ ; Array of data values
           }

end