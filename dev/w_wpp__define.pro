; docformat = 'rst'
;+
;  The WAVE library WRF Post-Processor
;
;
; :Properties:
;
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 201.
;
;       Modified::
;
;-

PRO w_WPP__Define

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  struct = { w_WPP                             ,  $
    input_file: ''                   ,  $
    input_directory: ''              ,  $ ; where to find the orginal WRF files
    output_directory:  ''            ,  $ ; where to find the aggregated files
    of_vardef_directory: ''          ,  $ ; where to find the outut variables *.wdef files
    if_pattern: ''                   ,  $ ; input files search pattern
    of_preffix: ''                   ,  $ ; aggregated files preffix
    domains: PTR_NEW()               ,  $ ; domains to process
    input_hours:0L                   ,  $ ; effective output per file
    static_file_vars: PTR_NEW()      ,  $ ; variables to put in the one and only domain static file
    file_vars: PTR_NEW()             ,  $ ; time-dependant variables to add to ALL the aggregated files
    file_static_vars: PTR_NEW()      ,  $ ; static variables to add to ALL the aggregated files
    pressure_levels: PTR_NEW()       ,  $ ; pressure levels of the pressure levels files
    npressure_levels: 0L       ,  $ ; pressure levels of the pressure levels files
    obsolete_attributes: PTR_NEW()   ,  $ ; attributes to remove when copying the file
    comment_attribute: ''            ,  $ ; attribute to add when copying the file
    vardefs: PTR_NEW()           ,  $
    n_vardefs: 0L                  $
    }
    
  struct = {w_WPP_vardef,       $
    file_name: '',       $
    do_pres: FALSE,       $
    dimensions: PTR_NEW(),       $
    ndimensions: 0L,       $
    variables: PTR_NEW(),       $
    nvariables: 0L,       $
    unstagger: FALSE,       $
    agg_steps_default: PTR_NEW(),       $
    nagg_steps_default: 0L,       $
    agg_method: ''       $
    }
END

Function w_WPP::Init, FILE = file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select the input.wpp file', /MUST_EXIST, FILTER='*.wpp')
  if file eq '' then MESSAGE, WAVE_Std_Message(/FILE)
  
  self.input_file = file
  if ~ self->_Parse_Inputfile() then MESSAGE, 'Unable to parse the input file correctly. Please check it.'
  if ~ self->_Parse_VarDefs() then MESSAGE, 'Unable to parse the variable definitions correctly. Please check it.'
  
  return, 1
  
end

;+
; :Description:
;    Destroy function.
;
;-
pro w_WPP::_destroyInputParams

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  self.input_file = ''
  self.input_directory = ''
  self. output_directory =  ''
  self.of_vardef_directory = ''
  self.if_pattern = ''
  self.of_preffix = ''
  PTR_FREE, self.domains
  self.input_hours =0L
  PTR_FREE, self.static_file_vars
  PTR_FREE, self.file_vars
  PTR_FREE, self.file_static_vars
  PTR_FREE, self.pressure_levels
  PTR_FREE, self.obsolete_attributes
  self.comment_attribute = ''
  
END

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-
pro w_WPP::_destroyVardefs

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if PTR_VALID(self.vardefs) then begin
    vardefs = *self.vardefs
    for i = 0, N_ELEMENTS(vardefs) - 1 do begin
      ptr_free, (vardefs[i]).dimensions
      ptr_free, (vardefs[i]).variables
      ptr_free, (vardefs[i]).agg_steps_default
    endfor
  endif
  ptr_free, self.vardefs

  self.n_vardefs = 0L
  
end

;+
; :Description:
;    Destroy function.
;
;-
pro w_WPP::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  self->_destroyInputParams
  self->_destroyVardefs
  
END

Function w_WPP::_Parse_Inputfile

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_destroyInputParams
    if N_ELEMENTS(lun) ne 0 then FREE_LUN, lun
    RETURN, 0
  ENDIF
  
  ; Set default values
  self.if_pattern = 'wrfpost_*_25h'
  self.of_preffix = 'wrf_agg'
  self.input_hours = 24L
  
  ; Open the file and loop over the lines
  OPENR, lun, self.input_file, /GET_LUN
  while ~ eof(lun) do begin
    line = ''
    readf, lun, line
    if line eq '' then continue
    line = (STRSPLIT(line, ';', /EXTRACT, /PRESERVE_NULL))[0] ; before the comments is interesting
    if line eq '' then continue
    line = STRSPLIT(line, '=', /EXTRACT)
    if N_ELEMENTS(line) ne 2 then continue ; we just have one = sign
    key = str_equiv(line[0])
    val = STRCOMPRESS(line[1], /REMOVE_ALL)
    
    case (key) of
      'INPUT_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.input_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'OUTPUT_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.output_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'OF_VARDEF_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.of_vardef_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'IF_PATTERN': begin
        self.if_pattern = val
      end
      'OF_PREFFIX': begin
        self.of_preffix = val
      end
      'DOMAINS': begin
        val = LONG(val)
        if N_ELEMENTS(val) ne 0 then self.domains = PTR_NEW(val)
      end
      'INPUT_HOURS': begin
        val = LONG(val)
        if N_ELEMENTS(val) eq  1 then self.input_hours = val
      end
      'STATIC_FILE_VARS': begin
        val = STRSPLIT(val, ',', /EXTRACT)
        if N_ELEMENTS(val) ne 0 then self.static_file_vars = PTR_NEW(val)
      end
      'FILE_VARS': begin
        val = STRSPLIT(val, ',', /EXTRACT)
        if N_ELEMENTS(val) ne 0 then self.file_vars = PTR_NEW(val)
      end
      'FILE_STATIC_VARS': begin
        val = STRSPLIT(val, ',', /EXTRACT)
        if N_ELEMENTS(val) ne 0 then self.file_static_vars = PTR_NEW(val)
      end
      'PRESSURE_LEVELS': begin
        val = LONG(val)
        if N_ELEMENTS(val) ne 0 then begin
          self.pressure_levels = PTR_NEW(val)
          self.npressure_levels = N_ELEMENTS(val)
        endif
      end
      'OBSOLETE_ATTRIBUTES': begin
        val = STRSPLIT(val, ',', /EXTRACT)
        if N_ELEMENTS(val) ne 0 then self.file_static_vars = PTR_NEW(val)
      end
      'OBSOLETE_ATTRIBUTES': begin
        if N_ELEMENTS(val) eq 1 then self.comment_attribute = val
      end
      else:
    endcase
  endwhile
  FREE_LUN, lun
  
  ; Check
  if self.input_directory eq '' then Message, ''
  if self.output_directory eq  '' then Message, ''
  if self.of_vardef_directory eq '' then Message, ''
  if self.if_pattern eq '' then Message, ''
  if self.of_preffix eq '' then Message, ''
  if self.input_hours eq 0 then Message, ''
  
  return, 1
  
end

Function w_WPP::_Parse_VarDefFile, file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    if N_ELEMENTS(lun) ne 0 then FREE_LUN, lun
    RETURN, AAA
  ENDIF
  
  ; Set default values
  vardef = {w_WPP_vardef}
  vardef.unstagger = FALSE
  vardef.agg_steps_default = PTR_NEW('STEP')
  is_pres = FALSE
  vardef.nagg_steps_default = 1
  
  
  ; Open the file and loop over the lines
  OPENR, lun, file, /GET_LUN
  while ~ eof(lun) do begin
    line = ''
    readf, lun, line
    if line eq '' then continue
    line = (STRSPLIT(line, ';', /EXTRACT, /PRESERVE_NULL))[0] ; before the comments is interesting
    if line eq '' then continue
    line = STRSPLIT(line, '=', /EXTRACT)
    if N_ELEMENTS(line) ne 2 then continue ; we just have one = sign
    key = str_equiv(line[0])
    val = STRCOMPRESS(line[1], /REMOVE_ALL)
    
    case (key) of
      'FILE_NAME': begin
        dummy = STRSPLIT(STRLOWCASE(val), '.', COUNT=cnt, /EXTRACT)
        if cnt eq 2 then begin
          if dummy[1] eq 'sfc' or dummy[1] eq 'press' then vardef.file_name = STRLOWCASE(val)
          if dummy[1] eq 'press' then vardef.do_pres = TRUE
        endif
      end
      'DIMENSIONS': begin
        val = STRSPLIT(val, ',', /EXTRACT, COUNT=cnt)
        if cnt ne 0 then begin
          vardef.dimensions = PTR_NEW(val)
          vardef.ndimensions = N_ELEMENTS(val)
        endif
      end
      'VARIABLES': begin
        val = STRSPLIT(val, ',', /EXTRACT, COUNT=cnt)
        if cnt ne 0 then begin
          vardef.variables = PTR_NEW(val)
          vardef.nvariables = N_ELEMENTS(val)
        endif
      end
      'UNSTAGGER': begin
        if str_equiv(val) eq 'TRUE' then vardef.unstagger = TRUE
      end
      'AGG_STEPS_DEFAULT': begin
        val = str_equiv(STRSPLIT(val, ',', /EXTRACT, COUNT=cnt))
        cc = 0
        for i=0, cnt-1 do begin
          p = where(['STEP','DAILY'] eq val[i], cntt)
          cc += cntt
        endfor
        if cc ne 0 then begin 
         vardef.agg_steps_default = PTR_NEW(val)
         vardef.nagg_steps_default = N_ELEMENTS(val)         
        endif
      end
      'AGG_METHOD': begin
        val = str_equiv(val)
        p = where(['MEAN','SUM','WIND'] eq val, cnt)
        if cnt ne 0 then vardef.agg_method = val
      end
      else:
    endcase
  endwhile
  FREE_LUN, lun
  
  ; Check
  mes = 'Parsing of ' + file + ' failed'
  if ~PTR_VALID(vardef.variables) then Message, mes
  if ~PTR_VALID(vardef.dimensions) then Message, mes
  if ~PTR_VALID(vardef.agg_steps_default) then Message, mes
  if vardef.file_name eq  '' then Message, mes
  if vardef.agg_method eq  '' then Message, mes
  
  return, vardef
  
end

Function w_WPP::_Parse_VarDefs

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    self->_destroyVardefs
    RETURN, 0
  ENDIF
  
  fileList = FILE_SEARCH(self.of_vardef_directory, '*.wdef', /EXPAND_ENVIRONMENT, COUNT=cnt)
  if cnt eq 0 then MESSAGE, 'No *.wdef files found'
  
  for i=0, cnt-1 do begin
    vdef = self->_Parse_VarDefFile(fileList[i])
    if N_ELEMENTS(vdef) eq 0 then continue    
    if self.n_vardefs eq 0 then begin
      self.n_vardefs = 1
      self.vardefs = PTR_NEW(vdef, /NO_COPY)
    endif else begin
      temp = *self.vardefs
      np = self.n_vardefs
      ptr_free, self.vardefs
      temp = [temp, vdef]
      self.vardefs = PTR_NEW(temp, /NO_COPY)
      self.n_vardefs = np + 1
    endelse
  endfor
  
  if self.n_vardefs eq 0 then MESSAGE, 'Not a single *.wdef file found'
  
  return, 1
  
end

pro w_WPP::_define_out_file, vdef,  year, template, wrftemplate, timeserie, daytimeserie, FORCE=force, PATH=path
  

  
  for j=0, vdef.nagg_steps_default-1 do begin
  
    aggstep = STRLOWCASE((*vdef.agg_steps_default)[j])
    do_day = str_equiv(aggstep) eq 'DAILY'
    
    path = self.output_directory + '/wrf_agg.'+vdef.file_name+'.'+str_equiv(year)+'.'+aggstep+'.nc'
    if FILE_TEST(path) and KEYWORD_SET(FORCE) then FILE_DELETE, path  
    if FILE_TEST(path) then return    
    
    elog_ = '/home/mowglie/'+'log_'+str_equiv(LONG64(SYSTIME(/SECONDS)-1.3214766E+09))+'.wrf_agg.'+vdef.file_name+'.'+str_equiv(year)+'.'+aggstep+'.log'
    
    dObj = Obj_New('NCDF_FILE', path, /CREATE, /CLOBBER, /TIMESTAMP, ErrorLoggerName=elog_)
    IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
    
    ; Find all the global attributes in the source file and copy them.
    attrNames = template->GetGlobalAttrNames(COUNT=attrCount)
    FOR k=0,attrCount-1 DO BEGIN
      template->CopyGlobalAttrTo, attrNames[k], dObj
    ENDFOR
    
    ; Find all the dimensions in the source file and copy them.
    if do_day then nt = N_ELEMENTS(daytimeserie) else nt = N_ELEMENTS(timeserie)
    dObj->WriteDim, 'time', nt
    dObj->WriteDim, 'DateStrLen', 19
    
    dimNames = template->GetDimNames(COUNT=dimCount)
    mydimNames = *(vdef.dimensions)
    
    FOR k=0,dimCount-1 DO BEGIN
      dn = dimNames[k]
      if str_equiv(dn) eq 'TIME' then continue
      p = where(str_equiv(mydimNames) eq str_equiv(dn), cnt)
      if cnt ne 0 then template->CopyDimTo, dn, dObj
    ENDFOR
    p = where(str_equiv(mydimNames) eq str_equiv('pressure_levels'), cnt)
    if cnt ne 0 then begin
      dObj->WriteDim, 'pressure_levels', self.npressure_levels
    endif
    
    ; Define time variable
    template->CopyVarDefTo, 'Times', dObj
    vn = 'XLAT'
    template->CopyVarDefTo, vn, dObj
    varAttrNames = template->GetVarAttrNames(vn, COUNT=varAttrCount)
    FOR k=0,varAttrCount-1 DO BEGIN
      template->CopyVarAttrTo, vn, varAttrNames[k], dObj
    ENDFOR
    template->CopyVarDataTo, vn, dObj
    vn = 'XLONG'
    template->CopyVarDefTo, vn, dObj
    varAttrNames = template->GetVarAttrNames(vn, COUNT=varAttrCount)
    FOR k=0,varAttrCount-1 DO BEGIN
      template->CopyVarAttrTo, vn, varAttrNames[k], dObj
    ENDFOR
    template->CopyVarDataTo, vn, dObj
    
    dObj->WriteVarDef, 'WCHECK', 'time', DATATYPE='LONG'
    
    myVars = *vdef.variables
    for k=0, vdef.nvariables-1 do begin
      ok = wrftemplate->get_Var_Info(myVars[k], $
        units = units, $
        description = description, $
        varname = varname , $ ;
        dims = dims, $ ;
        dimnames = dimnames)
      if ~ok then message, 'var not ok'
      ; Define a variable for the file.
      dObj->WriteVarDef, myVars[k], mydimNames, DATATYPE='FLOAT', OBJECT=dataObj
      IF Obj_Valid(dataObj) EQ 0 THEN Message, 'Invalid data object returned.'
      ; Define variable attributes.
      dObj->WriteVarAttr, dataObj, 'description', description
      dObj->WriteVarAttr, dataObj, 'units', units
    endfor ; k=0, vdef.nvariables-1
    ; Sync the file by writing memory to disk.
    dObj->Sync
    Obj_Destroy, dObj
  endfor   ;j=0, vdef.nagg_steps_default-1
  
end

pro w_WPP::aggregate, YEAR=year, MONTH=month
  
  @WAVE.inc
  domain = 1
  if N_ELEMENTS(year) ne 1 then Message, 'YEAR please'
  
  filelist = FILE_SEARCH(self.input_directory, 'wrfpost*.nc', /EXPAND_ENVIRONMENT, COUNT=cnt)
  isHere = STRPOS(filelist, 'd0' + str_equiv(domain))
  p = WHERE(isHere ne -1, cnt)
  if cnt ne 0 then filelist = filelist[p] else fileList = ''
  fileList = fileList[SORT(fileList)]
  if N_ELEMENTS(fileList) le 1 then message, 'No files?'
  
  ; Now get a std file
  template = OBJ_NEW('NCDF_File', fileList[0])
  wrf =  OBJ_NEW('w_WRF', FILE= fileList[0])
  wrf->get_time, time
  tstep = time[1]-time[0]
  t0 = QMS_TIME(year=year, month=1, day=1, hour=tstep/H_QMS)
  t1 = QMS_TIME(year=year+1, month=1, day=1, hour=0)
  timeserie = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(DMS=tstep), NSTEPS=nt)
  t0 = QMS_TIME(year=year, month=1, day=1, hour=0)
  t1 = QMS_TIME(year=year, month=12, day=31, hour=0)
  daytimeserie = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(day=1), NSTEPS=nd)
  
  for i=0, self.n_vardefs-1 do begin  
      vdef = (*self.vardefs)[i]   
      self->_define_out_file, vdef, year, template, wrf, timeserie, daytimeserie
  endfor
  
  for f=0, cnt-1 do begin
    file = caching(filelist[f])
    wrfo = OBJ_NEW('w_WRF', FILE=file)
    wrfo->get_time, wtime, wnt, wt0, wt1
    
    for i=0, self.n_vardefs-1 do begin
    
      vdef = (*self.vardefs)[i]
      
      for j=0, vdef.nagg_steps_default-1 do begin
      
        aggstep = STRLOWCASE((*vdef.agg_steps_default)[j])
        do_day = str_equiv(aggstep) eq 'DAILY'
        opath = self.output_directory + '/wrf_agg.'+vdef.file_name+'.'+str_equiv(year)+'.'+aggstep+'.nc'
        
        wto_fill = OBJ_NEW('w_WRF', file=opath)
        to_fill = OBJ_NEW('NCDF_FILE', opath, /MODIFY)
        
        if DO_DAY then time_to_fill = DAYTIMESERIE else time_to_fill = TIMESERIE
        pok = where(time_to_fill eq wt1, cntok)
        if cntok ne 1 then begin
          undefine, wto_fill, to_fill
          continue
        endif
        if do_day then pok-=1 else pok = where(time_to_fill eq wtime[1], cntok)
        
        myVars = [*vdef.variables, 'Times']
        for k=0, vdef.nvariables-1 do begin
          if vdef.do_pres then pl = *self.pressure_levels else undefine, pl
          var = wrfo->get_Var(myVars[k], PRESSURE_LEVELS=pl)
          dims = SIZE(var, /DIMENSIONS)
          nd = N_ELEMENTS(dims)
          if nd eq 1 then var = reform(var[1:*])
          if nd eq 2 then var = reform(var[*,1:*])
          if nd eq 3 then var = reform(var[*,*,1:*])
          if nd eq 4 then var = reform(var[*,*,*,1:*])
          if do_day and nd eq 3 then var = utils_AVERAGE(var, 3)
          if do_day and nd eq 4 then var = utils_AVERAGE(var, 4)
          
          dims = SIZE(var, /DIMENSIONS)
          ndims = N_ELEMENTS(dims)
          if ndims eq 1 then OFFSET=[pok]
          if ndims eq 2 then OFFSET=[0,0,pok]
          if ndims eq 3 then OFFSET=[0,0,pok]
          if ndims eq 4 then OFFSET=[0,0,0,pok]
          to_fill->WriteVarData, myVars[k], var, OFFSET=OFFSET
        endfor
        to_fill->Sync
        undefine, wto_fill, to_fill
      endfor
    endfor
    undefine, wrfo
    file = caching(filelist[f], /DELETE)
  endfor
    
  undefine, template, wrf
  
end
