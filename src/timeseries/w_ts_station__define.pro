; docformat = 'rst'
;+
;   Object to store and manage timeseries objects all together.
;   
;   The princip of the station is to allow to perform the same actions
;   on a set of timeseries objects together, and add some usefull information
;   like location, etc.
;   
;   The station accepts variables of the same timestep and validity, but can
;   accept variables that cover different periods. All the station variables are then
;   automatically set to the longest period containing all variables.
;   
;   The Station object can be seen as a container that performs some actions 
;   (uniformisation of timesteps, interpolation, aggregation) on all its variables. 
;   Additionally, some tools are provided like writing the variable data into an
;   ASCII file.
; 
; Definition::
;       
;       class = {w_ts_Station       , $
;               name:            '' , $ ; The name of the station
;               id:              '' , $ ; Station ID
;               description:     '' , $ ; A short description of the station 
;               elevation:       0L , $ ; altitude in m
;               loc_x:           0D , $ ; X location in SRC
;               loc_y:           0D , $ ; Y location in SRC
;               src:      PTR_NEW() , $ ; Location information ({TNT_DATUM}, {TNT_PROJ})
;               t0:             0LL , $ ; User def start Time (0 if default period)
;               t1:             0LL , $ ; User def end Time (0 if default period)
;               vars:     OBJ_NEW()   $ ; Variables container (w_ts_Container)
;               }   
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
; :Keywords:
;    NAME: in, optional, type=string
;          The name of the station
;    ID: in, optional, type=string
;        The station ID
;    DESCRIPTION: in, optional, type=string
;                 A short description of the station
;    ELEVATION: in, optional, type=long
;               The height of the station
;    LOC_X: in, optional, type=double
;           The X location of the station
;    LOC_Y: in, optional, type=double
;           The X location of the station
;    SRC: in, optional, type=double
;         the source of X and Y ({TNT_DATUM}, {TNT_PROJ})
;
; :History:
;     Written by FaM, 2012.
;
;-
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
  if N_ELEMENTS(ID) eq 0 then _id = _name else _id = id
  if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
  if N_ELEMENTS(ELEVATION) eq 0 then _elevation = -9999L else _elevation = elevation
  if N_ELEMENTS(LOC_X) eq 0 then _loc_x = -9999d else _loc_x = loc_x
  if N_ELEMENTS(LOC_Y) eq 0 then _loc_y = -9999d else _loc_y = loc_y
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

;+
; :Description:
;    Destroys the object instance and frees the memory.
;
;-
pro w_ts_Station::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  PTR_FREE, self.src
  Obj_Destroy, self.vars
  
end

;+
; :Description:
;    Get access to some object properties.
;          
; :Keywords:
;    NAME: out, type=string
;          The name of the station
;    ID: out, type=string
;        The station ID
;    DESCRIPTION: out, type=string
;                 A short description of the station
;    ELEVATION: out, type=long
;               The height of the station
;    LOC_X: out, type=double
;           The X location of the station
;    LOC_Y: out, type=double
;           The X location of the station
;    SRC: out, type=double
;         the source of X and Y ({TNT_DATUM}, {TNT_PROJ})
;    T0: out, type=qms/{ABS_DATE}
;        User def start time
;    T1: out, type=qms/{ABS_DATE}
;        User def end time
;    TIMESTEP: out, type={TIME_STEP}
;              Timeserie timestep
;-
pro w_ts_Station::getProperty, NAME=name, $
                               ID=id, $ 
                               DESCRIPTION=description, $  
                               ELEVATION=elevation, $ 
                               T0=t0, $
                               T1=t1, $
                               LOC_X=loc_x, $ 
                               LOC_Y=loc_y, $ 
                               SRC=src
                               

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  NAME=self.name
  ID=self.id
  DESCRIPTION=self.description
  ELEVATION=self.elevation
  LOC_X=self.loc_x
  LOC_Y=self.loc_y
  SRC=*self.src
  T0=self.t0
  T1=self.t1
  
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
function w_ts_Station::getProperty, thisProperty

  ; SET UP ENVIRONNEMENT
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  avail = ['NAME' , $
           'ID' , $
           'DESCRIPTION' , $
           'ELEVATION' , $
           'LOC_X' , $
           'LOC_Y' , $
           'SRC' , $
           'T0' , $
           'T1' ]
  
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
;          The name of the station
;    ID: in, optional, type=string
;        The station ID
;    DESCRIPTION: in, optional, type=string
;                 A short description of the station
;    ELEVATION: in, optional, type=long
;               The height of the station
;    LOC_X: in, optional, type=double
;           The X location of the station
;    LOC_Y: in, optional, type=double
;           The X location of the station
;    SRC: in, optional, type=double
;         the source of X and Y ({TNT_DATUM}, {TNT_PROJ})
;
;-
pro w_ts_Station::setProperty, NAME=name, $
                               DESCRIPTION=description, $
                               ID=id, $
                               ELEVATION=elevation, $ 
                               LOC_X=loc_x, $ 
                               LOC_Y=loc_y, $ 
                               SRC=src

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  if N_ELEMENTS(NAME) ne 0 then self.name = name
  if N_ELEMENTS(DESCRIPTION) ne 0 then self.description = description
  if N_ELEMENTS(ID) ne 0  then self.id = id
  if N_ELEMENTS(ELEVATION) ne 0  then self.elevation = elevation
  if N_ELEMENTS(LOC_X) ne 0 then self.loc_x = loc_x
  if N_ELEMENTS(LOC_Y) ne 0 then self.loc_y = loc_y
  if N_ELEMENTS(SRC) ne 0 then begin 
    PTR_FREE, self.src
    self.src = PTR_NEW(SRC)
  endif 
  
end

;+
; :Description:
;    Adds a variable to the station. Default behavior is to
;    check if the variable allready exists and if not, store it. 
;    If the variable allready exists, the the data is added to the 
;    existing data.
;
; :Params:
;    var: in, required, type={w_Ts_Data}
;         the variable to add
;
; :Keywords:
;    REPLACE: in, optional, type=boolean
;             set this keyword to replace the existing variable (if any)
;             instead of adding the data to the existing timeserie
;    NO_DESTROY: in, optional, type=boolean     
;                default behavior when a variable allready exists 
;                is to add the data to the stored object and destroy
;                the argument. This makes sense in most of the cases
;                but sometimes you don't want this. Set this keyword
;                to avoid it.
;
;-
pro w_ts_Station::addVar, var, REPLACE=replace, NO_DESTROY=no_destroy

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if ~OBJ_VALID(var) then message, WAVE_Std_Message('var', /ARG)
  if ~OBJ_ISA(var, 'w_ts_Data') then message, WAVE_Std_Message('var', /ARG)
   
  if self->HasVar(var->getProperty('NAME'), OBJECT=object) then begin     
    if KEYWORD_SET(REPLACE) then begin
      self.vars->Remove, object
      self.vars->Add, var
    endif else begin
      object->addData, var->getData(), var->getTime()
      if ~KEYWORD_SET(NO_DESTROY) then OBJ_DESTROY, var
    endelse  
  endif else begin
    self.vars->Add, var    
  endelse
    
  self->setPeriod
  
end

;+
; :Description:
;    To obtain the variables names.
;
; :Keywords:
;    COUNT: out
;           the number of variables
;    PRINT: in, optional, type=boolean
;           print the variables in the console
;
; :Returns:
;    An string array of COUNT variable names
;-
function w_ts_Station::GetVarNames, COUNT=varCount, PRINT=print

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
    varNames[j] = thisObj->GetProperty('NAME')
  ENDFOR
  
  ; Return a scalar if necessary.
  IF varCount EQ 1 THEN varNames = varNames[0]
  
  if KEYWORD_SET(PRINT) then begin
    for i=0, varCount-1 do print, str_equiv(i) + ': ' + varNames[i]
  endif
  
  RETURN, varNames
  
END

;+
; :Description:
;    Test if a variable is available or not
;
; :Params:
;    varName: in, type=string
;             the variable name to look for
;
; :Keywords:
;    OBJECT: out
;            the variable object (if found)
;
; :Returns:
;    1 if the variable is found, 0 otherwise
;    
;-
function w_ts_Station::HasVar, varName, OBJECT=object

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~arg_okay(varName, TYPE=IDL_STRING, /SCALAR) then Message, WAVE_Std_Message('varName', /ARG)
  
  ; Can you find a variable object with this name?
  object = self.vars->FindByName(varName, COUNT=count)
  
  if count gt 0 then return, 1 else return, 0
  
END

;+
; :Description:
;    Get a variable object.
;
; :Params:
;    varName: in, type=string
;             the variable name to look for
;             
; :Returns:
;    The variable object
;
;-
function w_ts_Station::getVar, varName

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~arg_okay(varName, TYPE=IDL_STRING, /SCALAR) then Message, WAVE_Std_Message('varName', /ARG)
  
  if ~ self->HasVar(varName, OBJECT=object) then Message, 'No variable found'
  
  return, object
  
end

;+
; :Description:
;    Delete variables from the station.
;
; :Params:
;    varName: in, optional, type=string array
;             the name(s) of the variables to remove
;
;-
pro w_ts_Station::removeVar, varName

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~arg_okay(varName, TYPE=IDL_STRING) then Message, WAVE_Std_Message('varName', /ARG)
  
  n = N_ELEMENTS(varName)
  
  for i=0, n-1 do if self->HasVar(varName[i], OBJECT=object) then self.vars->Remove, object
    
  self->setPeriod
  
end


;+
; :Description:
;    Sets the focus period to the shortest period 
;    enclosing all variables or to a user defined period.
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
pro w_ts_Station::setPeriod, T0=t0, T1=t1, DEFAULT=default

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if KEYWORD_SET(DEFAULT) then begin
    self.t0 = 0LL
    self.t1 = 0LL
  endif
  
  vNames = self->GetVarNames(COUNT=varCount)
  
  if N_ELEMENTS(T0) ne 0 then begin 
    if ~ CHECK_WTIME(T0, OUT_QMS=_t0) then Message, WAVE_Std_Message('T0', /ARG)
    self.t0 = _t0
  endif
  
  if N_ELEMENTS(T1) ne 0 then begin 
    if ~ CHECK_WTIME(T1, OUT_QMS=_t1) then Message, WAVE_Std_Message('T1', /ARG)
    self.t1 = _t1
  endif
  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    if i eq 0 then begin
      _t0 = _var->getProperty('t0')
      _t1 = _var->getProperty('t1')
      _step = _var->getProperty('step')
      _timestep = _var->getProperty('timestep')
    endif else begin
      _t0 = _var->getProperty('t0')  < _t0
      _t1 = _var->getProperty('t1')  > _t1
      if _step ne _var->getProperty('step') then message, 'Step type do not match'
      if _timestep ne _var->getProperty('timestep') then message, 'Timesteps do not match'
    endelse
  endfor
  
  if self.t0 ne 0 then _t0 = self.t0
  if self.t1 ne 0 then _t1 = self.t1
  
  for i=0, varCount-1 do (self->getVar(vNames[i]))->setPeriod, T0=_t0, T1=_t1
  
end

;+
; :Description:
;    Just prints the missing periods of all variables in the console
;
;-
pro w_ts_Station::printMissingPeriods
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->setPeriod
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    print, ' ' + _var->getProperty('NAME')
    _var->printMissingPeriods
  endfor        
      
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
pro w_ts_Station::interpol, T0=t0, T1=t1
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->setPeriod
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->interpol, T0=t0, T1=t1
  endfor        
      
end

;+
; :Description:
;   Replaces periods with a value (if omitted, MISSING)
; 
; :Params:
;    value: in, optional
;           
;    
; :Keywords:
;    T0: in, optional
;        if the interpolation have to be made on a section of the data only (remaining data is unchanged)
;    T1: in, optional
;        if the interpolation have to be made on a section of the data only (remaining data is unchanged)
;        
;-
pro w_ts_Station::insertValue, value, T0=t0, T1=t1

   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->setPeriod
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->insertValue, value, T0=t0, T1=t1
  endfor   

end

;+
; :Description:
;    Aggregates all variables and creates a new station object with the
;    new variables.
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
function w_ts_Station::Aggregate, DAY = day, HOUR = hour, NEW_TIME = new_time
  
  ; Make a new identical station
  out = OBJ_NEW('w_ts_Station', NAME=self.name, ID=self.id, DESCRIPTION=self.description, $ ; A short description of the station 
            ELEVATION=self.elevation, LOC_X=self.loc_x, LOC_Y=self.loc_y, SRC=self.src)                             
                             
  varCount = self.vars->Count()  
  IF varCount EQ 0 THEN return, out
  
  FOR j=0,varCount-1 DO BEGIN
    _var = self.vars->Get(POSITION=j)
    out->addVar, _var->Aggregate(DAY=day, HOUR=hour, NEW_TIME=new_time)   
  endfor
  
  return, out

end

;+
; :Description:
;   Plot all the vars in separate windows
;
;-
pro w_ts_Station::plotVars
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->setPeriod
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->plotTS, TITLE_INFO = self.name + ': '
  endfor        
      
end


;+
; :Description:
;    Writes the variables data in an ASCII file.
;
; :Keywords:
;    FILE: in, required, type=string
;          the path to the file to write
;    TITLE: in, optional, type=string
;           The title of the ASCII file
;    FORMAT: in, optional, type=string
;            the string format code for floats
;
;-
pro w_ts_Station::write_ASCII_file, FILE=file, TITLE=title, FORMAT=format

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  if N_ELEMENTS(file) eq 0 then message, WAVE_Std_Message('FILE', /ARG)
  
  if N_ELEMENTS(format) eq 0 then format = '(F9.3)'
  
  openw, id, file, /GET_LUN
  
  if N_ELEMENTS(title) eq 0 then title = 'File generated with IDL.'
  printf, id, '% TITLE: ' + title
  
  meta = '% STATION_NAME: ' + self.name
  printf, id, meta
  meta = '% STATION_DESCRIPTION: ' + self.description
  printf, id, meta
  meta = '% STATION_LOCATION: Longitude ' + str_equiv(STRING(self.loc_x)) + ' ; Latitude ' + str_equiv(STRING(self.loc_y)) + ' ; Altitude ' + STRING(self.elevation, FORMAT='(I4)') 
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
                      VALIDITY=Validity, $
                      TYPE=type, $
                      AGG_METHOD=agg_method
   
   units[i] = unit
   types[i] = type_name(type)
   descriptions[i] = description
   valids[i] = Validity
      
  endfor 
  meta = '% DATA_VALIDITY: ' + valids[0]
  printf, id, meta
  meta = '% FILE_FORMAT: NAME, DESCRIPTION, UNIT, TYPE'
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
  
  text = '"STRING","'
  for i = 0, nvar - 2 do text +=types[i] + sep
  text += types[nvar- 1]  + '"'
  printf, id, text
    
  sep = ','
  
  ; Bad optim
  datas = PTRARR(nvar)
  for i = 0, nvar - 1 do begin
    _var = (self->GetVar(vNames[i]))
    datas[i] = PTR_NEW(_var->getData())
  endfor
  
  for l = 0, nt-1 do begin
  
    t = TIME_to_STR(time[l], /YMD)    
    GEN_str_subst, ret,t,'.','-',t
    GEN_str_subst, ret,t,'.','-',t
    
    text = '"' + t + '",'
    for i = 0, nvar - 1 do begin
      data = (*(datas[i]))[l]
      if str_equiv(types[i]) eq str_equiv('float') then v = strcompress(STRING(data, FORMAT=format),/REMOVE_ALL)
      if str_equiv(types[i]) eq str_equiv('long') then v = strcompress(STRING(data,FORMAT = '(I8)'),/REMOVE_ALL)
      if i lt nvar - 1 then text += v + sep else text += v
      undefine, v
    endfor
    
    printf, id, text
  endfor
  undefine, datas
  FREE_LUN, id
  
end

;+
; Class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;           class definition as a structure variable
;           
;-
pro w_ts_Station__Define, class

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  class = {w_ts_Station       , $
           name:            '' , $ ; The name of the station
           id:              '' , $ ; Station ID
           description:     '' , $ ; A short description of the station 
           elevation:       0L , $ ; altitude in m
           loc_x:           0D , $ ; X location in SRC
           loc_y:           0D , $ ; Y location in SRC
           src:      PTR_NEW() , $ ; Location information ({TNT_DATUM}, {TNT_PROJ})
           t0:             0LL , $ ; Used def start Time
           t1:             0LL , $ ; Used def End Time
           vars:     OBJ_NEW()   $ ; Variables container (w_ts_Container)
           }    

end