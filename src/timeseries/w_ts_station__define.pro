; docformat = 'rst'
;+
;   Object to store and manage timeseries objects all together.
;   The station object also stores locations and additional informations. 
;   
;   The Station object can be seen as a container that performs some actions 
;   (uniformisation of timesteps, interpolation, aggregation) on all of its variables. 
;   Additionally, some tools are provided like writing the variable data into an
;   ASCII or NCDF file.
; 
;   The station accepts only variables having the same timestep, 
;   but can accept variables that cover different time periods. 
;   All the station variables are then automatically set to the 
;   longest period containing all variables.
;   
; 
; Definition::
;       
;       class = {w_ts_Station        , $
;                  name:            '' , $ ; The name of the station
;                  id:              '' , $ ; Station ID
;                  description:     '' , $ ; A short description of the station 
;                  elevation:       0L , $ ; altitude in m
;                  loc_x:           0D , $ ; X location in SRC
;                  loc_y:           0D , $ ; Y location in SRC
;                  src:      PTR_NEW() , $ ; Location information ({TNT_DATUM} or {TNT_PROJ})
;                  t0:             0LL , $ ; Start Time
;                  t1:             0LL , $ ; End Time
;                  vars:     OBJ_NEW()   $ ; Variables container (w_ts_Container)
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
;    FILE: in, optional, type=string
;          path to a station ncdf file (see `NCDFwrite`)
;          if set, all other keywords are ignored.
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
                             SRC=src, $ ; Location information ({TNT_DATUM}, {TNT_PROJ})
                             FILE=file ; ncdf file
   
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ; Initialize object containers for contents.
  self.vars = Obj_New('w_ts_Container')
  
  if N_ELEMENTS(FILE) ne 0 then begin
  
    n = OBJ_NEW('NCDF_FILE', file)
    
    _name = n->GetGlobalAttrValue('NAME')
    _id =  n->GetGlobalAttrValue('ID')
    _description =  n->GetGlobalAttrValue('DESCRIPTION')
    _elevation = n->GetGlobalAttrValue('ELEVATION')
    _loc_x = n->GetGlobalAttrValue('LOC_X')
    _loc_y = n->GetGlobalAttrValue('LOC_Y')
    case  n->GetGlobalAttrValue('SRC') of
      'DATUM': GIS_make_datum, ret, _src, NAME=n->GetGlobalAttrValue('SRC_INFO')
      'PROJ': GIS_make_proj, ret, _src, PARAM=n->GetGlobalAttrValue('SRC_INFO')
      else: Message, 'Error by src'
    endcase
    
    n->GetProperty, FILEID=cdfid
    ok = utils_nc_coards_time(cdfid, time, time0, time1, nt)
    if ~ ok then Message, 'Error by time'
    
    vnames = n->GetVarNames(COUNT=nv)
    
    for i=0, nv-1 do begin
      vname = vnames[i]
      if str_equiv(vname) eq 'TIME' then continue
      v = OBJ_NEW('w_ts_Data', n->GetVarData(vname), time, NAME=vname, $
        AGG_METHOD=n->GetVarAttrValue(vname,'agg_method'), $
        DESCRIPTION=n->GetVarAttrValue(vname,'description'), $
        MISSING=n->GetVarAttrValue(vname,'missing'), $
        STEP=n->GetVarAttrValue(vname,'step'), $
        TIMESTEP=n->GetVarAttrValue(vname,'timestep'), $
        UNIT=n->GetVarAttrValue(vname,'units'), $
        VALIDITY=n->GetVarAttrValue(vname,'validity'))        
        self->addVar, v
        
    endfor
     
    undefine, n
  
  endif else begin  
  
    ; Check input and set defaults
    if N_ELEMENTS(NAME) eq 0 then _name = 'Station' else _name = name
    if N_ELEMENTS(ID) eq 0 then _id = _name else _id = id
    if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
    if N_ELEMENTS(ELEVATION) eq 0 then _elevation = -9999L else _elevation = elevation
    if N_ELEMENTS(LOC_X) eq 0 then _loc_x = -9999d else _loc_x = loc_x
    if N_ELEMENTS(LOC_Y) eq 0 then _loc_y = -9999d else _loc_y = loc_y
    if N_ELEMENTS(SRC) eq 0 then GIS_make_datum, ret, _src, NAME = 'WGS-84' else _src = src
    
  endelse
  
  self.name = _name
  self.id = _id
  self.description = _description
  self.elevation = _elevation
  self.loc_x = _loc_x
  self.loc_y = _loc_y
  self.src = PTR_NEW(_src)
 
  return, 1
    
end

;+
; :Description:
;    Destroys the object instance and frees the memory.
;
;-
pro w_ts_Station::cleanup

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
;    TIMESTEP: out, type={TIME_STEP}
;              Timeserie timestep
;-
pro w_ts_Station::getProperty, NAME=name, $
                               ID=id, $ 
                               DESCRIPTION=description, $  
                               ELEVATION=elevation, $ 
                               LOC_X=loc_x, $ 
                               LOC_Y=loc_y, $ 
                               T0=t0, $ 
                               T1=t1, $ 
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
           'T0' , $
           'T1' , $
           'SRC' ]
  
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
;    check if the variable already exists and if not, store it. 
;    If the variable allready exists, the data is added to the 
;    existing timeserie.
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
;                but sometimes you want to keep the original object.
;                Set this keyword to avoid destroying it.
;   NO_SET_PERIOD: in, optional, type=boolean
;                  default behavior is to check all variable periods
;                  before return for consistency. For a station 
;                  in creation this can lead to a large number of useless
;                  operations. Set this keyword to avoid this check. 
;                  However, it is recommended to do a ->set_period
;                  by yourself after the station has been defined
;
;-
pro w_ts_Station::addVar, var, REPLACE=replace, NO_DESTROY=no_destroy, NO_SET_PERIOD=no_set_period

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
      if ~ KEYWORD_SET(NO_DESTROY) then OBJ_DESTROY, var
    endelse  
  endif else begin
    self.vars->Add, var    
  endelse
    
  if ~ KEYWORD_SET(NO_SET_PERIOD) then self->setPeriod
  
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
function w_ts_Station::getVarNames, COUNT=varCount, PRINT=print

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
function w_ts_Station::hasVar, varName, OBJECT=object

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~arg_okay(varName, TYPE=IDL_STRING, /SCALAR) then Message, WAVE_Std_Message('varName', /ARG)
  
  ; Can you find a variable object with this name?
  object = self.vars->FindByName(varName, COUNT=count)
  
  return, count gt 0
  
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
  
  if ~ self->HasVar(varName, OBJECT=object) then Message, 'No variable found with name: ' + str_equiv(varName)
  
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
;    Select variable(s) to be kept by the station. All other variables
;    are desrtroyed.
;
; :Params:
;    varName: in, optional, type=string array
;             the name of the variable(s) to keep
;
;-
pro w_ts_Station::selVar, varName

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~arg_okay(varName, TYPE=IDL_STRING) then Message, WAVE_Std_Message('varName', /ARG)
  
  names = self->GetVarNames(count=n)
  
  for i=0, n-1 do begin
    pos=where(str_equiv(varName) eq str_equiv(names[i]), cnt)
    if cnt ne 0 then continue
    self->removeVar, names[i]
  endfor
  
  self->setPeriod
  
end

;+
; :Description:
; 
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
    if KEYWORD_SET(DEFAULT) then _var->setPeriod, /DEFAULT
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
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->interpol, T0=t0, T1=t1
  endfor        
      
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
pro w_ts_Station::insertValue, value, T0=t0, T1=t1

   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->insertValue, value, T0=t0, T1=t1
  endfor   

end

;+
; :Description:
;    Aggregates all station's variables and creates a new station object with the
;    new aggregated variables.
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
function w_ts_Station::aggregate, DAY=day, HOUR=hour, MONTH=month, YEAR=year, $
                                NEW_TIME=new_time, MIN_SIG=min_sig, MIN_NSIG=min_nsig
  
  ; Make a new identical station
  out = OBJ_NEW('w_ts_Station', NAME=self.name, ID=self.id, DESCRIPTION=self.description, $ ; A short description of the station 
            ELEVATION=self.elevation, LOC_X=self.loc_x, LOC_Y=self.loc_y, SRC=self.src)                             
                             
  varCount = self.vars->Count()  
  IF varCount EQ 0 THEN return, out
  
  FOR j=0,varCount-1 DO BEGIN
    _var = self.vars->Get(POSITION=j)
    out->addVar, _var->aggregate(DAY=day, HOUR=hour, MONTH=month, YEAR=year, $
                                 NEW_TIME=new_time, MIN_SIG=min_sig, MIN_NSIG=min_nsig)   
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
  
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    _var->plotTS, TITLE_INFO = self.name + ': '
  endfor        
      
end


;+
; :Description:
;    Writes the station data in one single ASCII file.
;    
;    The ASCII file can be parsed by `w_aws_read_data_file` but
;    if you want to save the station data for further use within
;    then wave, it is recommended to use `NCDFwrite`
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
pro w_ts_Station::ASCIIwrite, FILE=file, TITLE=title, FORMAT=format

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
; :Description:
;    Writes the station data in one single NCDF file.
;    
;    This can be done for save/restore purposes, since the 
;    NCDF file can be later parsed automatically (and efficiently)
;    for the object initialisation.
;
; :Keywords:
;    FILE: in, required, type=string
;          the path to the file to write. If a directory is given as argument,
;          the routine will define the name automatically using the
;          station id and name
;    OVERWRITE: in, optional
;               default behavior is to stop if the file already exists.
;               Set this keyword to avoid this
;
;-
pro w_ts_Station::NCDFwrite, FILE=file, OVERWRITE=overwrite

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  ; Count the number of global attribute objects.
  varCount = self.vars->Count()  
  IF varCount EQ 0 THEN Message, 'No variables stored in the station!'
  
  self->setPeriod
  
  if N_ELEMENTS(FILE) eq 0 then Message, WAVE_Std_Message('FILE', /ARG)
  if FILE_TEST(FILE, /DIRECTORY) then begin
    _file = utils_clean_path(file, /MARK_DIRECTORY)
    _file = _file + 'w_ts_' + self.id + '_' + self.name + '.nc' 
  endif else _file = file
      
  dObj = OBJ_NEW('NCDF_FILE', _file, /CREATE, CLOBBER=overwrite)
  if ~ OBJ_VALID(dObj) then return
  
  dObj->WriteGlobalAttr, 'FILE_INFO', 'WAVE w_ts_Station data', DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'NAME', self.name, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'ID', self.id, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DESCRIPTION', self.description, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'ELEVATION', self.elevation, DATATYPE='LONG'
  dObj->WriteGlobalAttr, 'LOC_X', self.loc_x, DATATYPE='DOUBLE'
  dObj->WriteGlobalAttr, 'LOC_Y', self.loc_y, DATATYPE='DOUBLE'
  
  src = *self.src
  if arg_okay(src, STRUCT={TNT_DATUM}) then begin
    src_t = 'DATUM'
    src_str = src.name   
  endif else if arg_okay(src, STRUCT={TNT_DATUM}) then begin
    src_t = 'PROJ'
    src_str = src.proj.envi 
  endif else message, 'SRC iz what?'  
  dObj->WriteGlobalAttr, 'SRC', src_t, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'SRC_INFO', src_str, DATATYPE='CHAR'
  
  v = self.vars->Get(POSITION=0)
  time = v->getTime(nt)
  time_str = 'seconds since ' + TIME_to_STR(time[0], MASK='YYYY-MM-DD HH:TT:SS')
  time = LONG((time - time[0]) / 1000)
  
  t_dim_name = 'time'
  dObj->WriteDim, t_dim_name, nt
  
  ; Variables
  vn = 'time'
  dObj->WriteVarDef, vn, t_dim_name, DATATYPE='LONG'
  dObj->WriteVarAttr, vn, 'long_name', 'Time'
  dObj->WriteVarAttr, vn, 'units', time_str
  dObj->WriteVarData, vn, time
  
  for j=0,varCount-1 do begin
    v = self.vars->Get(POSITION=j)
    vn = v->GetProperty('NAME')
    dataTypeName = type_name(v->GetProperty('TYPE'))
    case dataTypeName of
      'FLOAT' :
      'DOUBLE':
      'BYTE':
      'LONG':
      'INT': dataTypeName = 'LONG'
      else: Message, 'Data type too exotic for me'
    endcase
    dObj->WriteVarDef, vn, t_dim_name, DATATYPE=dataTypeName
    dObj->WriteVarAttr, vn, 'description', v->GetProperty('DESCRIPTION'), DATATYPE='CHAR'
    dObj->WriteVarAttr, vn, 'units', v->GetProperty('UNIT'), DATATYPE='CHAR'
    dObj->WriteVarAttr, vn, 'step', v->GetProperty('STEP'), DATATYPE='CHAR'
    dObj->WriteVarAttr, vn, 'timestep', v->GetProperty('TIMESTEP'), DATATYPE='LONG'
    dObj->WriteVarAttr, vn, 'agg_method', v->GetProperty('AGG_METHOD'), DATATYPE='CHAR'
    dObj->WriteVarAttr, vn, 'validity', v->GetProperty('VALIDITY'), DATATYPE='CHAR'
    
    dataTypeName = SIZE(v->GetProperty('MISSING'), /TNAME)
    case dataTypeName of
      'FLOAT' :
      'DOUBLE':
      'BYTE':
      'LONG':
      'INT': dataTypeName = 'LONG'      
      else: Message, 'missing type too exotic for me'
    endcase
    dObj->WriteVarAttr, vn, 'missing', v->GetProperty('MISSING'), DATATYPE=dataTypeName
    dObj->WriteVarData, vn, v->GetData()    
  endfor
  
  undefine, dObj
  
end

;+
; :Description:
;    Makes a copy of the Station
; 
; :Returns:
;    a copy of the station
;-
function w_ts_Station::copy

  out= OBJ_NEW('w_ts_Station', $
    NAME=self.name, $
    ID=self.id, $ 
    DESCRIPTION=self.description, $
    ELEVATION=self.elevation, $ 
    LOC_X=self.loc_x, $ 
    LOC_Y=self.loc_y, $ 
    SRC=*self.src)
      
  vNames = self->GetVarNames(COUNT=varCount)  
  for i=0, varCount-1 do begin
    _var = self->getVar(vNames[i])
    out ->addVar, _var->copy()
  endfor        
    
  out->setPeriod, T0=self.t0, T1=self.t1
  
  return, out

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

  class = {w_ts_Station        , $
           name:            '' , $ ; The name of the station
           id:              '' , $ ; Station ID
           description:     '' , $ ; A short description of the station 
           elevation:       0L , $ ; altitude in m
           loc_x:           0D , $ ; X location in SRC
           loc_y:           0D , $ ; Y location in SRC
           src:      PTR_NEW() , $ ; Location information ({TNT_DATUM} or {TNT_PROJ})
           t0:             0LL , $ ; Start Time
           t1:             0LL , $ ; End Time
           vars:     OBJ_NEW()   $ ; Variables container (w_ts_Container)
           }    

end