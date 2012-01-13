function w_ts_Station::GetVarNames, COUNT=varCount

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    ok = WAVE_Error_Message(!Error_State.Msg)
    return, ''
  ENDIF
  
  ; Count the number of global attribute objects.
  varCount = self.vars->Count()
  
  IF varCount EQ 0 THEN RETURN, ""
  
  varNames = StrArr(varCount)
  FOR j=0,varCount-1 DO BEGIN
    thisObj = self.vars->Get(POSITION=j)
    varNames[j] = thisObj->GetName()
  ENDFOR
  
  ; Return a scalar if necessary.
  IF varCount EQ 1 THEN varNames = varNames[0]
  
  RETURN, varNames
  
END

function w_ts_Station::HasVar, varName, OBJECT=object

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    ok = WAVE_Error_Message(!Error_State.Msg)
    return, ''
  ENDIF
  
  ; Can you find a variable object with this name?
  object = self.vars->FindByName(varName, COUNT=count)
  
  IF count GT 0 THEN RETURN, 1 ELSE RETURN, 0
  
END

function w_ts_Station::getVar, varName

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  if ~ self->HasVar(varName, OBJECT=object) then Message, 'No variable found'
  
  return, object
  
end

pro w_ts_Station::addVar, var, REPLACE=replace

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if ~OBJ_ISA(var, 'w_ts_Data') then message, WAVE_Std_Message('var', /ARG)
   
  if self->HasVar(var->getName(), OBJECT=object) then begin     
    if KEYWORD_SET(REPLACE) then begin
      self.vars->Remove, object
      self.vars->Add, var
    endif else begin
      object->addData, var->getData(), var->getTime()
    endelse  
  endif else begin
    self.vars->Add, var    
  endelse
    
  self->adjustVars
  
end

pro w_ts_Station::removeVar, varName, REPLACE=replace

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  n = N_ELEMENTS(varName)
  
  for i=0, n-1 do if self->HasVar(varName[i], OBJECT=object) then self.vars->Remove, object
    
  self->adjustVars
  
end



pro w_ts_Station::adjustVars, T0=t0, T1=t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  vNames = self->GetVarNames(COUNT=varCount)
   
  for i=0, varCount-1 do begin    
    _var = self->getVar(vNames[i])    
    _var->getProperty, TIMESTEP=_ts
    if N_ELEMENTS(_op_time) eq 0 then begin
      _op_time = utils_minmax(_var->getTime())      
      self.step = _ts
    endif else begin
      _op_time[0] = min(_var->getTime()) < _op_time[0] 
      _op_time[1] = max(_var->getTime()) > _op_time[1]       
      if self.step.dms ne _ts.dms then message, 'Time steps do not match'
    endelse    
  endfor
  
  if N_ELEMENTS(T0) ne 0 then begin  
    if ~ CHECK_WTIME(T0, OUT_QMS=_t0) then Message, WAVE_Std_Message('T0', /ARG)    
    _op_time[0] = _t0
  endif
  if N_ELEMENTS(T1) ne 0 then begin  
    if ~ CHECK_WTIME(T1, OUT_QMS=_t1) then Message, WAVE_Std_Message('T1', /ARG)    
    _op_time[1] = _t1
  endif
   
  self.op_time = _op_time
  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->regularTS, T0=_op_time[0], T1=_op_time[1]
  endfor        
    
end

pro w_ts_Station::printMissingPeriods, T0=t0, T1=t1
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->adjustVars, T0=t0, T1=t1
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    print, _var->getName()
    _var->printMissingPeriods
  endfor        
      
end

pro w_ts_Station::interpolVars, T0=t0, T1=t1
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->adjustVars, T0=t0, T1=t1
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->interpolTS
  endfor        
      
end

pro w_ts_Station::write_file, FILE = file, TITLE = title

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  if N_ELEMENTS(file) eq 0 then message, 'Wrong'
  
  openw, id, file, /GET_LUN
  
  if ~KEYWORD_SET(title) then title = '% File generated with IDL. See metadata for more info.'
  printf, id, title
  
  meta = '% Station: ' + self.name
  printf, id, meta
  meta = '% Description: ' + self.description
  printf, id, meta
  meta = '% Location info: Longitude ' + str_equiv(STRING(self.loc_x)) + ' ; Latitude ' + str_equiv(STRING(self.loc_y)) + ' ; Altitude ' + STRING(self.elevation, FORMAT='(I4)') 
  printf, id, meta
  
  
  vNames = self->GetVarNames(COUNT=nvar) 
  
  units = STRARR(nvar)
  types = STRARR(nvar)
  descriptions = STRARR(nvar)
  valids = STRARR(nvar)
  
  for i = 0, nvar - 1 do begin
   _var = (self->GetVar(vNames[i]))
   _var->getProperty, DESCRIPTION=description, $
                      UNIT=unit, $
                      VALID=valid, $
                      TYPE_NAME=type, $
                      AGG_METHOD=agg_method
   
   units[i] = unit
   types[i] = type
   descriptions[i] = description
   valids[i] = valid
      
  endfor 
  meta = '% Validity (POINT or INTERVAL. If INTERVAL the data is aggregated and the time indicates the time at the *END* of the interval): ' + valids[0]
  printf, id, meta
  
  time = _var->getTime(nt)    
  
  sep = '","'  
  text = '"TIMESTAMP","'
  for i = 0, nvar - 2 do text +=vNames[i] + sep
  text += vNames[nvar- 1]  + '"'
  printf, id, text
  
  text = '"-","'
  for i = 0, nvar - 2 do text +=descriptions[i] + sep
  text += descriptions[nvar- 1]  + '"'
  printf, id, text
  
  text = '"-","'
  for i = 0, nvar - 2 do text +=units[i] + sep
  text += units[nvar- 1]  + '"'
  printf, id, text
  
  text = '"-","'
  for i = 0, nvar - 2 do text +=types[i] + sep
  text += types[nvar- 1]  + '"'
  printf, id, text
    
  sep = ','
  for l = 0, nt-1 do begin
  
    t = TIME_to_STR(time[l], /YMD)    
    GEN_str_subst, ret,t,'.','-',t
    GEN_str_subst, ret,t,'.','-',t
    
    text = '"' + t + '",'
    for i = 0, nvar - 1 do begin
      _var = (self->GetVar(vNames[i]))
      data = (_var->getData())[l]
      if str_equiv(types[i]) eq str_equiv('float') then v = strcompress(STRING(data,FORMAT = '(F8.3)'),/REMOVE_ALL)
      if str_equiv(types[i]) eq str_equiv('long') then v = strcompress(STRING(data,FORMAT = '(I8)'),/REMOVE_ALL)
      if i lt nvar - 1 then text += v + sep else text += v
      undefine, v
    endfor
    
    printf, id, text
  endfor
  
  close, id
  
end

function w_ts_Station::Aggregate, DAY = day, HOUR = hour, NEW_TIME = new_time
  
  ; Make a new identical station
  out = OBJ_NEW('w_ts_Station', NAME=self.name, ID=self.id, DESCRIPTION=self.description, $ ; A short description of the station 
            ELEVATION=self.elevation, LOC_X=self.loc_x, LOC_Y=self.loc_y, SRC=self.src)
                             
                             
  varCount = self.vars->Count()  
  IF varCount EQ 0 THEN return, out
  
  FOR j=0,varCount-1 DO BEGIN
    _var = self.vars->Get(POSITION=j)
    _var->getProperty, NAME=name, $
      DESCRIPTION=description, $
      UNIT=unit, $
      AGG_METHOD=agg_method
      MISSING=_var->getMissing()
                         
    TS_AGG, _var->getData(), _var->getTime(), agg, agg_time, DAY=day, HOUR=hour, NEW_TIME=new_time, AGG_METHOD=agg_method
        
    out->addVar, OBJ_NEW('w_ts_Data', agg, agg_time, NAME=name, $
      DESCRIPTION=description, $
      UNIT=unit, $
      VALID='INTERVAL', $
      AGG_METHOD=agg_method, $
      MISSING=missing)

  endfor
  
  return, out


end


pro w_ts_Station::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  PTR_FREE, self.src
  Obj_Destroy, self.vars
  
end

function w_ts_Station::init, NAME=name, $ ; The name of the station
                             ID=id, $ ; Station ID
                             DESCRIPTION=description, $ ; A short description of the station 
                             ELEVATION=elevation, $ ; altitude in m
                             LOC_X=loc_x, $ ; X location in SRC
                             LOC_Y=loc_y, $ ; Y location in SRC
                             SRC=src ; Location information ({TNT_DATUM}, {TNT_PROJ})
   
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ; Check input and set defaults
  if N_ELEMENTS(NAME) eq 0 then _name = 'Station' else _name = name
  if N_ELEMENTS(ID) eq 0 then _id = '' else _id = id
  if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
  if N_ELEMENTS(ELEVATION) eq 0 then _elevation = 0 else _elevation = elevation
  if N_ELEMENTS(LOC_X) eq 0 then _loc_x = 0 else _loc_x = loc_x
  if N_ELEMENTS(LOC_Y) eq 0 then _loc_y = 0 else _loc_y = loc_y
  if N_ELEMENTS(SRC) eq 0 then GIS_make_datum, ret, _src, NAME = 'WGS-84' else _src = src  
  
  self.name = _name
  self.id = _id
  self.description = _description
  self.elevation = _elevation
  self.loc_x = _loc_x
  self.loc_y = _loc_y
  self.src = PTR_NEW(_src)
  
  ; Initialize object containers for contents.
  self.vars = Obj_New('w_ts_Container')
  
  return, 1
    
end

pro w_ts_Station__Define

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  struct = {w_ts_Station      , $
           name:            '', $ ; The name of the station
           id:              '', $ ; Station ID
           description:     '', $ ; A short description of the station 
           elevation :      0L, $ ; altitude in m
           loc_x :          0D, $ ; X location in SRC
           loc_y :          0D, $ ; Y location in SRC
           src:      PTR_NEW(), $ ; Location information ({TNT_DATUM}, {TNT_PROJ})
           op_time:  [0LL,0LL], $ ; Operating period [QMS,QMS]
           step :  {TIME_STEP}, $ ; Probale Timestep
           vars:     OBJ_NEW()  $ ; Variables
           }    

end