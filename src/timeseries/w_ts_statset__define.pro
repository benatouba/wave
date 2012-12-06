; docformat = 'rst'
;+
;   Object to store and manage station objects all together.
;   
;   The Statset object can be seen as a container that performs some actions 
;   (uniformisation of timesteps, interpolation, aggregation) on all of its stations. 
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
;          The name of the station set
;    DESCRIPTION: in, optional, type=string
;                 A short description of the station set
;    DIRECTORY: in, optional, type=string
;               path to a directory containing station NCDF files
;
;
;-
function w_ts_StatSet::init, NAME=name, $ ; The name of the station set
    DESCRIPTION=description, $ ; A short description of the station set
    DIRECTORY=directory

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ; Check input and set defaults
  if N_ELEMENTS(NAME) eq 0 then _name = 'Station Set' else _name = name
  if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
 
  self.name = _name
  self.description = _description
  
  ; Initialize object containers for contents.
  self.stats = Obj_New('w_ts_Container')
  
  if N_ELEMENTS(DIRECTORY) ne 0 then begin
  
    if ~ FILE_TEST(directory, /DIRECTORY) then Message, WAVE_Std_Message('DIRECTORY', /FILE)
    
    filelist = FILE_SEARCH(directory, '*.nc', /EXPAND_ENVIRONMENT, COUNT=cnt)
    if cnt eq 0 then Message, 'No files found in the directory'
    
    for i=0, cnt-1 do begin
      s = OBJ_NEW('w_ts_Station', FILE=filelist[i])
      if OBJ_VALID(s) then self->addStat, s
    endfor    
    
  endif  
  
  return, 1
    
end

;+
; :Description:
;    Destroys the object instance and frees the memory.
;
;-
pro w_ts_StatSet::Cleanup

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2

  Obj_Destroy, self.stats
  Ptr_free, self.varNames
  Ptr_free, self.varTypes
  
end

;+
; :Description:
;    Get access to some object properties.
;          
; :Keywords:
;    NAME: out, type=string
;          The name of the station set
;    DESCRIPTION: out, type=string
;                 A short description of the station set 
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
;-
pro w_ts_StatSet::getProperty, NAME=name, $
                               DESCRIPTION=description, $ 
                               STEP=step, $ 
                               NT=nt, $ 
                               T0=t0, $ 
                               T1=t1, $ 
                               TIMESTEP=timestep
                               

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  NAME=self.name
  DESCRIPTION=self.description
  T0=self.t0
  T1=self.t1
  NT=self.nt
  TIMESTEP=self.timestep
  STEP=self.step

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
function w_ts_StatSet::getProperty, thisProperty

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  avail = ['NAME' , $
           'DESCRIPTION', $
           'T0' , $
           'T1' , $
           'STEP' , $
           'TIMESTEP' , $
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
;          The name of the station set
;    DESCRIPTION: in, optional, type=string
;                 A short description of the station set 
;
;-
pro w_ts_StatSet::setProperty, NAME=name, $
                               DESCRIPTION=description

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if N_ELEMENTS(NAME) ne 0 then self.name = name
  if N_ELEMENTS(DESCRIPTION) ne 0 then self.description = description
  
end

;+
; :Description:
;    Adds a station to the station set. Default behavior is to
;    check if the station allready exists and if not, store it. 
;
; :Params:
;    var: in, required, type={w_Ts_Data}
;         the variable to add
;
; :Keywords:
;    REPLACE: in, optional, type=boolean
;             set this keyword to replace the existing station (if any)
;             instead of adding the data to the existing timeserie
;    NO_DESTROY: in, optional, type=boolean     
;                default behavior when a station allready exists 
;                is to add the data to the stored object and destroy
;                the argument. This makes sense in most of the cases
;                but sometimes you don't want this. Set this keyword
;                to avoid it.
;
;-
pro w_ts_StatSet::addStat, stat, REPLACE=replace, NO_DESTROY=no_destroy

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~OBJ_VALID(stat) then message, WAVE_Std_Message('stat', /ARG)
  if ~OBJ_ISA(stat, 'w_ts_Station') then message, WAVE_Std_Message('stat', /ARG)
   
  if self->HasStat(stat->getProperty('ID'), OBJECT=object) then begin     
    if KEYWORD_SET(REPLACE) then begin
      self.stats->Remove, object
      self.stats->Add, stat
    endif else begin
      Message, 'Station already there. Not implemented yet'
    endelse  
  endif else begin
    self.stats->Add, stat    
  endelse
  
  ; This is to keep track of the variables (not very efficient)
  vns = str_equiv(stat->getVarNames(COUNT=nv))
  if nv eq 0 then return
  types = LONARR(nv)
  for i=0, nv-1 do types[i] = (stat->getVar(vns[i]))->getProperty('TYPE')
  if ~ PTR_VALID(self.varNames) then begin
    self.varNames = PTR_NEW(vns)
    self.varTypes = PTR_NEW(types)    
  endif else begin
    pvns = [*self.varNames, vns]
    pts = [*self.varTypes, types]
    PTR_FREE, self.varNames
    PTR_FREE, self.varTypes
    u = UNIQ(pvns, SORT(pvns))
    self.varNames = PTR_NEW(pvns[u])
    self.varTypes = PTR_NEW(pts[u])    
  endelse
  
end

;+
; :Description:
;    To obtain the same property for all the stations
;
; :Keywords:
;    COUNT: out
;           the number of stations
;
; :Returns:
;    An string array of COUNT station Ids
;-
function w_ts_StatSet::GetStatProperty, thisProperty, COUNT=count

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  ; Count the number of station objects.
  StatCount = self.Stats->Count()  
  IF StatCount eq 0 then Message, 'No stations yet'
  
  out = LIST()
  
  for j=0,StatCount-1 do begin
    thisObj = self.Stats->Get(POSITION=j)
    out->add, thisObj->getProperty(thisProperty)
  endfor
  
  count = N_ELEMENTS(out)
  
  return, out->toArray()

end

;+
; :Description:
;    To obtain the number of stations in the set.
;
; :Returns:
;    the number of stations in the set
;-
function w_ts_StatSet::GetStatCount
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  return, self.stats->Count()

end

;+
; :Description:
;    To obtain the station Ids.
;
; :Keywords:
;    COUNT: out
;           the number of stations
;    PRINT: in, optional, type=boolean
;           print the stations in the console
;
; :Returns:
;    An string array of COUNT station Ids
;-
function w_ts_StatSet::GetStatIds, COUNT=StatCount, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  ; Count the number of station objects.
  StatCount = self.Stats->Count()  
  IF StatCount eq 0 then Message, 'No stations yet'
  
  StatIds = StrArr(StatCount)
  FOR j=0,StatCount-1 DO BEGIN
    thisObj = self.Stats->Get(POSITION=j)
    StatIds[j] = thisObj->GetProperty('ID')
  ENDFOR
  
  ; Return a scalar if necessary.
  IF StatCount EQ 1 THEN StatIds = StatIds[0]
  
  if KEYWORD_SET(PRINT) then begin
    for i=0, StatCount-1 do print, str_equiv(i) + ': ' + StatIds[i]
  endif
  
  RETURN, StatIds
  
END

;+
; :Description:
;    Test if a station is available or not
;
; :Params:
;    statid: in, type=string
;            the station id to look for
;
; :Keywords:
;    OBJECT: out
;            the station object (if found)
;
; :Returns:
;    1 if the station is found, 0 otherwise
;    
;-
function w_ts_StatSet::HasStat, statid, OBJECT=object

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  count = 0 

  ; Can you find a variable object with this id?
  object = self.stats->FindById(statid, COUNT=count)
  
  if count gt 0 then return, 1 else return, 0
  
END

;+
; :Description:
;    Get a station object.
;
; :Params:
;    statid: in, optional, type=string
;            the station id to look for
;            
; :Keywords:
;    POSITION: in, optional
;              the position in the list where to get the station
;             
; :Returns:
;    The station object
;
;-
function w_ts_StatSet::getStat, statid, POSITION=position

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if N_ELEMENTS(POSITION) ne 0 then begin
    object = self.stats->get(POSITION=position)
  endif else begin  
    if ~ self->HasStat(statid, OBJECT=object) then Message, 'No station found with id: ' + str_equiv(statid)
  endif
  
  return, object
  
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
function w_ts_StatSet::GetTime, NT=nt

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  ; Count the number of station objects.
  StatCount = self.Stats->Count()  
  IF StatCount eq 0 then Message, 'No stations yet'
  
  thisObj = self.stats->Get(POSITION=0)
  return, thisObj->GetTime(NT=nt)
  
end


;+
; :Description:
;    Delete station from the set.
;
; :Params:
;    statid: in, type=string
;            the station id to look for
;         
;-
pro w_ts_StatSet::removeStat, statid

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
    
  n = N_ELEMENTS(statid)  
  for i=0, n-1 do begin
   if self->Hasstat(statid, OBJECT=object) then begin
    self.stats->Remove, object
    undefine, object
   endif
  endfor  
  
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
function w_ts_StatSet::getVarNames, COUNT=varCount, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ PTR_VALID(self.varNames) then Message, 'No variables yet'
  
  varNames = *self.varNames
  varCount = N_ELEMENTS(varNames)    
  
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
;    TYPE: out, type=long
;          the type of the variable
;
; :Returns:
;    1 if the variable is found, 0 otherwise
;    
;-
function w_ts_StatSet::hasVar, varName, TYPE=type

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~arg_okay(varName, TYPE=IDL_STRING, /SCALAR) then Message, WAVE_Std_Message('varName', /ARG)
  
  ; Can you find a variable with this name? 
  varNames = self->getVarNames(COUNT=varCount)
  if varCount eq 0 then return, 0
  
  pok = where(varNames eq str_equiv(varName), cnt)
  if cnt eq 0 then return, 0
  
  type = (*self.varTypes)[pok]
  
  return, 1
  
END

;+
; :Description:
;    Get the variable data for ALL stations in an array 
;    of dims N*M where N = the number of stations and
;    M = the number of times
;
; :Params:
;    varName: in, required
;             the name of the variable
;
;-
function w_ts_StatSet::GetVarData, varName

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ self->HasVar(varName, TYPE=type) then Message, 'No variable found with name: ' + str_equiv(varName)
  time = self->GetTime(NT=nt)
  if nt eq 0 then Message, 'No time'
  statIds = self->GetStatIds(COUNT=ns)
  if ns eq 0 then Message, 'No Stations'
  
  out = MAKE_ARRAY(ns, nt, TYPE=type)
  
  for i=0, ns-1 do out[i, *] = (self.stats->Get(POSITION=i))->getVarData(varName)
  
  return, out
  
end

;+
; :Description:
;    Get the variable validity for ALL stations in an array 
;    of dims N*M where N = the number of stations and
;    M = the number of times
;
; :Params:
;    varName: in, required
;             the name of the variable
;
;-
function w_ts_StatSet::GetVarValid, varName

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ self->HasVar(varName) then Message, 'No variable found with name: ' + str_equiv(varName)
  time = self->GetTime(NT=nt)
  if nt eq 0 then Message, 'No time'
  statIds = self->GetStatIds(COUNT=ns)
  if ns eq 0 then Message, 'No Stations'
  
  out = BYTARR(ns, nt)
  
  for i=0, ns-1 do out[i, *] = (self.stats->Get(POSITION=i))->getVarValid(varName)
  
  return, out
  
end

;+
; :Description:
;    Delete variables from the all stations in the set.
;
; :Params:
;    varName: in, optional, type=string array
;             the name(s) of the variables to remove
;
;-
pro w_ts_StatSet::removeVar, varName

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ PTR_VALID(self.varNames) then Message, 'No variables yet'
  
  StatCount = self.Stats->Count()
  FOR j=0,StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    _stat->removeVar, varName
  endfor
  
  ; Remove also from my vars
  varNames = *self.varNames
  varTypes = *self.varTypes
  for i=0, N_ELEMENTS(varName) - 1 do begin
    pok = WHERE(str_equiv(varNames) eq str_equiv(varName[i]), cntok)
    if cntOk ne 0 then utils_array_remove, pok, varNames, varTypes
  endfor  
  PTR_FREE, self.varNames
  PTR_FREE, self.varTypes
  self.varNames = PTR_NEW(varNames)
  self.varTypes = PTR_NEW(varTypes)
  
end

;+
; :Description:
;    Select variable(s) to be kept by the station set. All other variables
;    are destroyed.
;
; :Params:
;    varName: in, optional, type=string array
;             the name of the variable(s) to keep
;
;-
pro w_ts_StatSet::selVar, varName

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  StatCount = self.Stats->Count()  
  if StatCount eq 0 then return
  
  for j=0, StatCount-1 do begin
    _stat = self.Stats->Get(POSITION=j)
    _Stat->selVar, varName
  endfor  
  
end

;+
; :Description:
; 
;    Sets the focus period to the shortest period 
;    enclosing all variables or to a user defined period.
;    Once the user set t0 and/or t1, this parameter are 
;    remembered for later calls to getVarData, etc.
;
; :Keywords:
;    T0: in, optional, type=qms/{ABS_DATE}
;        the first time of the desired period
;    T1: in, optional, type=qms/{ABS_DATE}
;        the last time of the desired period
;
;-
pro w_ts_StatSet::setPeriod, T0=t0, T1=t1

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
    
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return
          
  for i=0, StatCount-1 do begin
    _stat = self.Stats->Get(POSITION=i)
    _stat->setPeriod, T0=t0, T1=t1
    if i eq 0 then begin
      _t0 = _stat->getProperty('t0')
      _t1 = _stat->getProperty('t1')
    endif else begin
      _t0 = _stat->getProperty('t0')  < _t0
      _t1 = _stat->getProperty('t1')  > _t1
    endelse
  endfor
  
  for i=0, StatCount-1 do (self.Stats->Get(POSITION=i))->setPeriod, T0=_t0, T1=_t1
  
  if StatCount eq 0 then begin
    self.t0 = 0
    self.t1 = 0
    self.nt = 0
  endif else begin
    _stat = (self.Stats->Get(POSITION=0))
    self.t0 = _stat->getProperty('t0')
    self.t1 = _stat->getProperty('t1')
    self.nt = _stat->getProperty('nt')
  endelse
   
end

;+
; :Description:
;    Makes a console print of the missing periods of all variables
;
;-
pro w_ts_StatSet::printMissingPeriods
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return  
  FOR j=0,StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    print, _Stat->getProperty('NAME')
    _Stat->printMissingPeriods
  endfor        
     
end

;+
; :Description:
;    Aggregates all stations and creates a new station set object with the
;    new stations.
;    
;    See `TS_AGG` for more info on the aggregation.
;
; :Keywords:
;    MINUTE: in, optional, default=none
;         set to minute interval (e.g: 10, or 30) to compute 
;         10 minutes or half-hourly statistics
;    HOUR: in, optional, default=none
;         set to hourly interval (e.g: 1, or 6) to compute 
;         hourly or six-hourly statistics
;    DAY: in, optional, default=none
;         set to day interval (e.g: 1, or 7) to compute 
;         daily or seven-daily statistics
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
function w_ts_StatSet::aggregate, MINUTE=minute, HOUR=hour, DAY=day, MONTH=month, YEAR=year, $
                                NEW_TIME=new_time, MIN_SIG=min_sig, MIN_NSIG=min_nsig
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  ; Make a new identical set
  out = OBJ_NEW('w_ts_StatSet', NAME=self.name, DESCRIPTION=self.description)                             
                             
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return, out
  
  FOR j=0,StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    out->addStat, _stat->Aggregate(MINUTE=minute, HOUR=hour, DAY=day, MONTH=month, YEAR=year, $
                                NEW_TIME=new_time, MIN_SIG=min_sig, MIN_NSIG=min_nsig)   
  endfor
  
  return, out

end

;+
; :Description:
;    Writes the station data in one single directory as NCDF files.
;    
;    This can be done for save/restore purposes, since the 
;    directory can be later parsed automatically (and efficiently)
;    for the object initialisation.
;
; :Keywords:
;    DIRECTORY: in, required, type=string
;                the path to the directory where to write the 
;                ncdf files
;    OVERWRITE: in, optional
;               default behavior is to stop if the file already exists.
;               Set this keyword to avoid this
;
;-
pro w_ts_StatSet::NCDFwrite, DIRECTORY=directory, OVERWRITE=overwrite

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ FILE_TEST(directory, /DIRECTORY) then Message, WAVE_Std_Message('DIRECTORY', /FILE)
  
  StatCount = self.Stats->Count()  
  iF StatCount EQ 0 THEN return
  
  for i=0, statCount-1 do begin
    (self.stats->get(POSITION=i))->NCDFwrite, FILE=directory, OVERWRITE=overwrite
  endfor
  
end

;+
; :Description:
;   Plot all the stations in separate windows
;
;-
pro w_ts_StatSet::plot
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return
  
  FOR j=0,StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    _stat->plot
  endfor        
      
end

;+
; Class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;           class definition as a structure variable
;           
;-
pro w_ts_StatSet__Define, class

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  class = {w_ts_StatSet       , $
           name:            '' , $ ; The name of the station set
           description:     '' , $ ; A short description of the station set           
           nt:             0L  , $ ; Number of times
           t0:             0LL , $ ; Start Time
           t1:             0LL , $ ; End Time
           step:           ''  , $ ; Either IRREGULAR, TIMESTEP, MONTH or YEAR
           timestep:       0LL , $ ; Timestep
           stats:     OBJ_NEW(), $ ; station container (w_ts_Container)
           varNames:  PTR_NEW(), $ ; array of variable names
           varTypes:  PTR_NEW()  $ ; array of variable types
           }    

end