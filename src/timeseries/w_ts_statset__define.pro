; docformat = 'rst'
;+
;   Object to store and manage station instances all together.
;   
;   The Statset object can be seen as a container that performs some actions 
;   (uniformisation of timesteps, interpolation, aggregation) on all of its stations.
; 
; Definition::
;       
;         class = {w_ts_StatSet       , $
;                 name:            '' , $ ; The name of the station set
;                 description:     '' , $ ; A short description of the station set           
;                 t0:             0LL , $ ; User def start Time
;                 t1:             0LL , $ ; User def End Time
;                 stats:     OBJ_NEW()  $ ; station container (w_ts_Container)
;                 }    
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
;
;
;-
function w_ts_StatSet::init, NAME=name, $ ; The name of the station set
    DESCRIPTION=description, $ ; A short description of the station set
    DIRECTORY=directory

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

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Obj_Destroy, self.stats
  
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
;        
;-
pro w_ts_StatSet::getProperty, NAME=name, $
                               DESCRIPTION=description

                               

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  NAME=self.name
  DESCRIPTION=self.description
  
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

  ; SET UP ENVIRONNEMENT
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  avail = ['NAME' , $
           'DESCRIPTION']
  
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
;    Adds a station to the station set. Default behavior is to
;    check if the variable allready exists and if not, store it. 
;    If the station allready exists, the the data is added to the 
;    existing station (TODO).
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
;   NO_SET_PERIOD: in, optional, type=boolean
;                  default behavior is to check all stations periods
;                  before return for consistency. For a station set 
;                  in creation this can lead to a large number of useless
;                  operations. Set this keyword to avoid this check. 
;                  However, it is recommended to do a ->set_period
;                  by yourself after the station set has been defined
;
;-
pro w_ts_StatSet::addStat, stat, REPLACE=replace, NO_DESTROY=no_destroy, NO_SET_PERIOD=no_set_period

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if ~OBJ_VALID(stat) then message, WAVE_Std_Message('stat', /ARG)
  if ~OBJ_ISA(stat, 'w_ts_Station') then message, WAVE_Std_Message('stat', /ARG)
   
  if self->HasStat(STATID=stat->getProperty('ID'), OBJECT=object) then begin     
    if KEYWORD_SET(REPLACE) then begin
      self.stats->Remove, object
      self.stats->Add, stat
    endif else begin
      MESSAGE, 'Station already there. Not implemented yet'
    endelse  
  endif else begin
    self.stats->Add, stat    
  endelse
    
  if ~ KEYWORD_SET(NO_SET_PERIOD) then self->setPeriod
  
end

;+
; :Description:
;    To obtain the station names.
;
; :Keywords:
;    COUNT: out
;           the number of stations
;    PRINT: in, optional, type=boolean
;           print the stations in the console
;
; :Returns:
;    An string array of COUNT station names
;-
function w_ts_StatSet::GetStatNames, COUNT=StatCount, PRINT=print

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
  StatCount = self.Stats->Count()
  
  IF StatCount EQ 0 THEN RETURN, ""
  
  StatNames = StrArr(StatCount)
  FOR j=0,StatCount-1 DO BEGIN
    thisObj = self.Stats->Get(POSITION=j)
    StatNames[j] = thisObj->GetProperty('NAME')
  ENDFOR
  
  ; Return a scalar if necessary.
  IF StatCount EQ 1 THEN StatNames = StatNames[0]
  
  if KEYWORD_SET(PRINT) then begin
    for i=0, StatCount-1 do print, str_equiv(i) + ': ' + StatNames[i]
  endif
  
  RETURN, StatNames
  
END

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
  StatCount = self.Stats->Count()
  
  IF StatCount EQ 0 THEN RETURN, ""
  
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
; :Keywords:
;    STATNAME: in
;              the station name to look for
;    STATID: in
;              the station id to look for
;    OBJECT: out
;            the station object (if found)
;    
; :Returns:
;    1 if the station is found, 0 otherwise
;    
;-
function w_ts_StatSet::HasStat, STATNAME=statname, STATID=statid, OBJECT=object

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  count = 0 
  
  if N_ELEMENTS(STATNAME) ne 0 then begin
    ; Can you find a variable object with this name?
    object = self.stats->FindByName(statname, COUNT=count)
  endif else if N_ELEMENTS(STATID) ne 0 then begin
    ; Can you find a variable object with this id?
    object = self.stats->FindById(statid, COUNT=count)
  endif  
  
  if count gt 0 then return, 1 else return, 0
  
END

;+
; :Description:
;    Get a station object.
;
; :Keywords:
;    STATNAME: in
;              the station name to look for
;    STATID: in
;              the station id to look for
;             
; :Returns:
;    The variable object
;
;-
function w_ts_StatSet::getStat, STATNAME=statname, STATID=statid

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~ self->HasStat(STATNAME=statname, STATID=statid, OBJECT=object) then Message, 'No station found'
  
  return, object
  
end

;+
; :Description:
;    Delete station from the set.
;
; :Keywords:
;    STATNAME: in
;              the station name to look for
;    STATID: in
;              the station id to look for
;
;-
pro w_ts_StatSet::removeStat, STATNAME=statname, STATID=statid

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  
  n = N_ELEMENTS(statname)  
  for i=0, n-1 do begin
   if self->Hasstat(STATNAME=statname, OBJECT=object) then begin
    self.stats->Remove, object
   endif
  endfor  
  n = N_ELEMENTS(statid)  
  for i=0, n-1 do begin
   if self->Hasstat(STATID=statid, OBJECT=object) then begin
    self.stats->Remove, object
   endif
  endfor  
  
  self->setPeriod
  
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

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return
  
  FOR j=0,StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    _Stat->removeVar, varName
  endfor  
     
  self->setPeriod
  
end

;+
; :Description:
;    Select variable(s) to be kept by the station set. All other variables
;    are desrtroyed.
;
; :Params:
;    varName: in, optional, type=string array
;             the name of the variable(s) to keep
;
;-
pro w_ts_StatSet::selVar, varName

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return
  
  FOR j=0, StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    _Stat->selVar, varName
  endfor  
     
  self->setPeriod
  
end

;+
; :Description:
;    Sets the focus period to the shortest period 
;    enclosing all stations or to a user defined period.
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
pro w_ts_StatSet::setPeriod, T0=t0, T1=t1, DEFAULT=default

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if KEYWORD_SET(DEFAULT) then begin
    self.t0 = 0LL
    self.t1 = 0LL
  endif
  
  StatCount = self.Stats->Count()  
  iF StatCount EQ 0 THEN return
  
  if N_ELEMENTS(T0) ne 0 then begin 
    if ~ CHECK_WTIME(T0, OUT_QMS=_t0) then Message, WAVE_Std_Message('T0', /ARG)
    self.t0 = _t0
  endif
  
  if N_ELEMENTS(T1) ne 0 then begin 
    if ~ CHECK_WTIME(T1, OUT_QMS=_t1) then Message, WAVE_Std_Message('T1', /ARG)
    self.t1 = _t1
  endif
  
  for i=0, statCount-1 do begin
    _stat = self.stats->get(POSITION=i)
    if i eq 0 then begin
      _t0 = _stat->getProperty('t0')
      _t1 = _stat->getProperty('t1')
    endif else begin
      _t0 = _stat->getProperty('t0')  < _t0
      _t1 = _stat->getProperty('t1')  > _t1
    endelse
  endfor
  
  if self.t0 ne 0 then _t0 = self.t0
  if self.t1 ne 0 then _t1 = self.t1
  
  for i=0, statCount-1 do (self.stats->get(POSITION=i))->setPeriod, T0=_t0, T1=_t1
  
end

;+
; :Description:
;    Just prints the missing periods of all stations in the console
;
;-
pro w_ts_StatSet::printMissingPeriods
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->setPeriod
  
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
  
  ; Make a new identical station
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

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
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
pro w_ts_StatSet::plotStats
  
   ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->setPeriod
  
  StatCount = self.Stats->Count()  
  IF StatCount EQ 0 THEN return
  
  FOR j=0,StatCount-1 DO BEGIN
    _stat = self.Stats->Get(POSITION=j)
    _stat->plotVars
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
           t0:             0LL , $ ; User def start Time
           t1:             0LL , $ ; User def End Time
           stats:     OBJ_NEW()  $ ; station container (w_ts_Container)
           }    

end