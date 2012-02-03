; docformat = 'rst'
;+
;  
; Object to store, retrieve  and analyse data timeseries.   
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
;                  timestep :      0LL    , $ ; Timestep
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
;    To create the object instance.
;    
;    `data` and `time` arrays are required to initialize the object, but
;    it is also a good idea to set the `NAME`,`UNIT`,`VALIDITY`,`AGG_METHOD`,
;    and `MISSING` keywords. If the timeserie is exotic, the `STEP` and 
;    `TIMESTEP` keywords are highly recommended.
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
pro w_ts_Data::Cleanup

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
  MISSING=*self.missing
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
;          FIRST CALL ONLY   
;    TIMESTEP: in, optional, type=DMS/{TIME_STEP}
;              Timeserie timestep. 
;              FIRST CALL ONLY      
;    MISSING: in, optional, type=string
;             value for missing data in the timeserie. 
;             FIRST CALL ONLY             
;    REPLACE: in, optional, type=boolean
;             replace the old data? 
;             Automatically set for the first call
;
;-
pro w_ts_Data::addData, data, time, STEP=step, TIMESTEP=timestep, MISSING=missing, REPLACE=replace

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_PARAMS() ne 2 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~array_processing(data, time, REP_A0=_data) then MESSAGE, '$DATA and $TIME arrays not compatible'
  if ~CHECK_WTIME(time, OUT_QMS=qms) then MESSAGE, WAVE_Std_Message('$TIME', /ARG)
  if N_ELEMENTS(data) lt 2 then Message, '$DATA must contain at least two elements.'
  if ptr_valid(self.time) then _first = FALSE else _first = TRUE
  if _first then _replace = TRUE else _replace = KEYWORD_SET(REPLACE)
  
  ;Type
  if self.type eq 0 then self.type = SIZE(_data, /TYPE);first call
  
  ; Define a missing flag
  if ~ptr_valid(self.missing) then begin ;first call
    if N_ELEMENTS(MISSING) eq 0 then begin
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
  if self.step eq '' then begin ;first call
    if N_ELEMENTS(STEP) ne 0 then _step = step else _step = 'TIMESTEP'
    if _step eq 'IRREGULAR' then Message, 'IRREGULAR steps not implemented yet'
    if _step eq 'MONTH' then Message, 'MONTH steps not implemented yet'
    if _step eq 'YEAR' then Message, 'YEAR steps not implemented yet'
    self.step=_step
  endif
  
  ; Timestep
  if (self.timestep eq 0) and self.step eq 'TIMESTEP' then begin ;first call
    if N_ELEMENTS(TIMESTEP) eq 0 then begin ; find out by myself
      ok = check_timeserie(qms, _timestep, CONFIDENCE=confidence)
      if confidence lt 0.75 then begin
        Message, 'Probable timestep chosen with a low confidence of: ' + $
          STRING(confidence, FORMAT='(F5.2)') + '. You should check it.'
      endif
    endif else _timestep = timestep
    if ~ CHECK_WTIMESTEP(_timestep, OUT_DMS=dms) then message, WAVE_Std_Message('TIMESTEP', /ARG)
    if dms le 0 then message, 'Timestep le 0?'
    if dms ge 2419200000LL and dms le 2678400000LL then begin ; We are probably in a monthly timeserie
      message, 'Monthly timestep not implemented yet'
    endif    
    self.timestep = dms    
  endif
 
  ; Check if we have to add the data or replace it
  if ~ _replace then begin
    qms = [*self.time, qms]
    _data = [*self.data, _data]
  endif

  ;Check for non-finite values in the timeserie anyway
  pf = where(~finite(_data), cntf)
  if cntf ne 0 then _data[pf]=*self.missing
  
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
  
  self->setPeriod
  
end

;+
; :Description:
;    Sets the focus period to its original period
;    or to a user-defined period. All later
;    calls to `w_ts_Data::getData`, `w_ts_Data::getTime`
;    or similar object functions will be affected by this change. 
;
; :Keywords:
;    T0: in, optional, type=qms/{ABS_DATE}, default=first original time in the ts
;        the first time of the desired period
;    T1: in, optional, type=qms/{ABS_DATE}, default=last original time in the ts
;        the last time of the desired period
;
;-
pro w_ts_Data::setPeriod, T0=t0, T1=t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~ptr_valid(self.time) then return ; no data yet
   
  if N_ELEMENTS(T0) ne 0 then begin  
    if ~ CHECK_WTIME(T0, OUT_QMS=_t0) then Message, WAVE_Std_Message('T0', /ARG)    
  endif else _t0 = (*self.time)[0]
  
  if N_ELEMENTS(T1) ne 0 then begin  
    if ~ CHECK_WTIME(T1, OUT_QMS=_t1) then Message, WAVE_Std_Message('T1', /ARG)    
  endif else _t1 = (*self.time)[N_ELEMENTS(*self.time)-1]
  
  ; Check the time
  _nt = (_t1 - _t0) / self.timestep
  if _nt ne DOUBLE(_t1 - _t0) / self.timestep then begin
   Message, 'T0, T1 and TIMESTEP are incompatible. Reset to default.', /INFORMATIONAL
   self->setPeriod
  endif
  _nt+=1
  
  ;Fill
  self.t0 = _t0
  self.t1 = _t1
  self.nt = _nt
 
end

;+
; :Description:
;    Get the time
;
; :Params:
;    nt: out
;        the number of times
;
; :Returns:
;   A time array of nt elements
;
;-
function w_ts_Data::getTime, nt

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  nt = self.nt
  return, MAKE_ENDED_TIME_SERIE(self.t0, self.t1, TIMESTEP=self.timestep, NSTEPS=nt) 
  
end

;+
; :Description:
;    Get the data
;   
; :Returns:
;   A data array of nt elements
;
;-
function w_ts_Data::getData

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  return, TS_FILL_MISSING(*self.data, *self.time, self->getTime(), FILL_VALUE=*self.missing)
  
end


;+
; :Description:
;   The validity of the data timeserie
;   
; :Returns:
;   An array of nt elements (valid = 1)
;
;-
function w_ts_Data::Valid

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  _mask = BYTARR(self.nt)
  data = self->getData()
  
  missing = *self.missing
  
  fmissing = finite(missing)  
  if fmissing then begin
    CASE self.type OF
      'FLOAT': indices = Where(Abs(data - missing) gt (MACHAR()).eps, count)
      'DOUBLE': indices = Where(Abs(data - missing) gt (MACHAR(/DOUBLE)).eps, count)
      ELSE: indices = Where(data ne missing, count)
    ENDCASE
  endif else begin
    indices = Where(finite(data), count)
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
  print, ' ' + str_equiv(n_ents) + ' missing periods'
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
;-
pro w_ts_Data::plotTS

  if self.description ne '' then TITLE = self.description else TITLE = self.name

  w_TimeLinePlot, self->getData(), self->getTime(), self.name, COLOR1='blue', TITLE=title, YTITLE=self.unit 

end


;+
; :Description:
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
;   Replaces periods with a value (if omitted, MISSING)
; 
; :Params:
;    value: in, optional
;           
; :Keywords:
;    T0: in, optional
;        if the interpolation have to be made on a section of the data only (remaining data is unchanged)
;    T1: in, optional
;        if the interpolation have to be made on a section of the data only (remaining data is unchanged)
;        
;-
pro w_ts_Data::insertValue, value, T0=t0, T1=t1

   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  t = self->getTime(nt)
  d = self->getdata()
  
  if N_ELEMENTS(value) eq 0 then _val = *self.missing else _val = value

  
  if N_ELEMENTS(T0) ne 0 and N_ELEMENTS(T1) ne 0 then begin
    if ~ check_WTIME(t0, OUT_QMS= it0) then message, WAVE_Std_Message('T0')
    v = 0 > VALUE_LOCATE(t, it0) < (nt-1)
    p0 = v[0] 
    if ~ check_WTIME(t1, OUT_QMS= it1) then message, WAVE_Std_Message('T0')
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
;    DAY: in, optional, default = none
;         set to an day interval (e.g: 1, or 7) to compute 
;         daily or seven-daily statistics
;    HOUR: in, optional, default = none
;         set to an hourly interval (e.g: 1, or 6) to compute 
;         hourly or six-hourly statistics
;    NEW_TIME: in, optional, type = {ABS_DATE}/qms ,default = none
;              ignored if `DAY` or `HOUR` are set. set this value to 
;              any time serie of n+1 elements. The ouptut will contain
;              n elements of the statistics for each interval [t, t+1]
;              (t excluded)
;
; :Returns:
;    A new object with the aggregated data
;
;-
function w_ts_Data::Aggregate, DAY=day, HOUR=hour, NEW_TIME=new_time


  TS_AGG, self->getData(), self->getTime(), agg, agg_time, $
  DAY=day, HOUR=hour, NEW_TIME=new_time, $
    AGG_METHOD=self.agg_method, MISSING=*self.missing
    
  out= OBJ_NEW('w_ts_Data', agg, agg_time, $
    NAME=self.name, $
    DESCRIPTION=self.description, $
    UNIT=self.unit, $
    VALIDITY='INTERVAL', $
    AGG_METHOD=self.agg_method, $
    STEP=self.step, $
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
function w_ts_Data::Copy

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