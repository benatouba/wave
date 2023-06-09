; docformat = 'rst'
;+
; 
;       w_HDF is the basis class for all kinds of HDF files. It reads
;       HDF files and provides some tools for rapid visualisation and
;       to analyse the content of a HDF file.
;       
;       It should be the superclass from all HDF related objects.
;       
;        TODO: Update routines: add the same functionalities as Ncdf.
;       
; :Properties: 
;          path: in, type = string
;                complete path of the active HDF file
;          HDFid: in, type = long
;                 id of the HDF file as given by HDF_start 
;          fname: in, type = string
;                 name of the active HDF file
;          directory: in, type=string 
;                     directory of the active HDF file
;          Nvars: in, type = long
;                 The number of variables defined for this HDF file. 
;          Ngatts:  in, type = long 
;                   The number of global attributes defined for this HDF file. 
;          FILE: in, optional, type = string
;                The path to the HDF file. If not set, a dialog window will open
;              
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  09-Dec-2010 FaM
;-

;+
; :Description:
;    Defines the attributes of the class w_Grid2D. Attributes::
;    w_HDF
;            path:               ''   
;            HDFid:              0L    
;            fname:              ''   
;            directory:          ''    
;            Nvars:              0L    
;            Ngatts:             0L    
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :History:
;     Written by FaM, 2010.
;-
PRO w_HDF__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {w_HDF                       ,  $
            path:               ''    ,  $ ; complete path of the active HDF file
            hdfid:              0L    ,  $ ; id of the HDF file as given by  HDF_start 
            fname:              ''    ,  $ ; name of the active HDF file
            directory:          ''    ,  $ ; directory of the active HDF file
            Nvars:              0L    ,  $ ; The number of variables defined for this HDF file. 
            Ngatts:             0L    ,  $ ; The number of global attributes defined for this HDF file. 
            varNames:    PTR_NEW()       $ ; An array of (nVars) strings containing the variable names. 
            }
            
    
END

;+
; :Description:
;    Build function. Output: 1 if the HDF object is updated successfully, 0 if not.
;
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;     FILE: in, optional, type = string
;           the path to the HDF file. If not set, a dialog window will open
;       
; :History:
;     Written by FaM, 2010.
;-
Function w_HDF::Init, FILE = file
           
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    if self.hdfID gt 0 then HDF_SD_END, self.hdfID
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(file) then begin
    file = DIALOG_PICKFILE(TITLE='Please select HDF file to read', /MUST_EXIST)
    IF file EQ "" THEN MESSAGE, WAVE_Std_Message(/FILE)
  endif
  
  ;*****************
  ; Check validity *
  ;***************** 
  if not HDF_ISHDF(file) then message, WAVE_Std_Message(/FILE)
  
  ;*****************
  ; Check filename *
  ;*****************  
  fname = FILE_BASENAME(file)
  directory = FILE_DIRNAME(file)
  
  ;****************
  ; Read metadata *
  ;****************    
  hdfID = HDF_SD_START(file, /READ)
  HDF_SD_Fileinfo, hdfID, num_vars, num_attr
    
  self.path = file
  self.hdfID = hdfID
  self.fname = fname
  self.directory = directory
  self.Nvars = num_vars
  self.Ngatts = num_attr
  
  self->get_Varlist, varId, varnames
  PTR_FREE, self.varNames
  self.varNames = PTR_NEW(varnames, /NO_COPY)
 
  RETURN, 1
  
END

;+
; :Description:
;    Destroy function. 
;
; :Categories:
;         WAVE/OBJ_GIS   
;
; :History:
;    Written by FaM, 2010.
;-
pro w_HDF::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  HDF_SD_END, self.hdfID
  PTR_FREE, self.varNames
  
END

;+
; :Description:
;    Get access to some params.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Keywords:
;    path: out, type = string
;          complete path of the active HDF file
;    HDFid: out, type = long
;          id of the HDF file as given by HDF_start 
;    fname: out, type = string
;          name of the active HDF file
;    directory: out, type = string
;               directory of the active HDF file
;    Nvars: out, type = long
;           The number of variables defined for this HDF file. 
;    Ngatts: out, type = long
;            The number of global attributes defined for this HDF file.
;    
; :History:
;     Written by FaM, 2010.
;-
PRO w_HDF::GetProperty, $
            path = path, $  ; complete path of the active HDF file
            HDFid = HDFid,  $ ; id of the HDF file as given by  HDF_start 
            fname = fname,  $ ; name of the active HDF file
            directory =  directory,  $ ; directory of the active HDF file
            Nvars = Nvars ,  $ ; The number of variables defined for this HDF file. 
            Ngatts = Ngatts    ; The number of global attributes defined for this HDF file. 
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  IF Arg_Present(path) NE 0 THEN path = self.path
  IF Arg_Present(fname) NE 0 THEN fname = self.fname
  IF Arg_Present(directory) NE 0 THEN directory = self.directory
  IF Arg_Present(HDFid) NE 0 THEN HDFid = self.HDFid
  IF Arg_Present(Nvars) NE 0 THEN Nvars = self.Nvars
  IF Arg_Present(Ngatts) NE 0 THEN Ngatts = self.Ngatts
  
end

;+
; :Description:
;    Get some informations on available variables in the HDF file
;    
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    varid: out, type = long
;           HDF SD indexes
;    varnames: out, type = string
;              variables name
;    varndims: out, type = long
;              variables number of dimensions
;    varunits: out, type = string
;              variables units
;    vardescriptions: out, type = string
;                     variables description (or long name)
;    vartypes: out, type = string
;              variables type 
;
; :Keywords:
;    PRINTVARS: in, optional
;               to print the infos in the console
;       
; :History:
;     Written by FaM, 2010.
;-
pro w_HDF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS = printvars

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  varid = 0L
  varnames = ''
  varunits = ''
  vardescriptions = ''
  varndims = 0L
  vartypes = ''
  
  IF self.Nvars eq 0 THEN begin
    if KEYWORD_SET(PRINTVARS) then print, 'No variables found!'
    return
  endif
  
  FOR j=0, self.Nvars-1 DO BEGIN
  
    ; Get information about the variable.
    sdID = HDF_SD_Select(self.HDFid, j)
    
    ; This routine throws all kinds of scary messages if CALDATA, for example, is
    ; not in the file. Turn this off for this call.
    _quiet = !QUIET
    !QUIET = 1
    HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
      RANGE=range, TYPE=datatype, CALDATA=calData
    !QUIET = _quiet
    
    varndims = [varndims, ndims]
    varid = [varid, j]
    vartypes = [vartypes, datatype]
    varnames = [varnames, name]
    
    if natts ne 0 then begin     
      ; Copy the variable attributes
      FOR k=0, natts-1 DO BEGIN
        HDF_SD_ATTRINFO, sdID, k, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
        theAttribute = String(theAttribute, /PRINT)
        theAttribute = STRMID(theAttribute, 0, STRLEN(theAttribute))
        if str_equiv(attribute_name) eq str_equiv('description') $
          or str_equiv(attribute_name) eq str_equiv('long_name') $
          then vardescriptions = [vardescriptions, theAttribute]
        if str_equiv(attribute_name) eq str_equiv('units') $
          or str_equiv(attribute_name) eq str_equiv('unit') $
          then varunits = [varunits, theAttribute]
      ENDFOR      
    endif
    
    HDF_SD_ENDACCESS, sdID
        
    if N_ELEMENTS(varunits) lt N_ELEMENTS(varnames) then varunits = [varunits, '']
    if N_ELEMENTS(vardescriptions) lt N_ELEMENTS(varnames) then vardescriptions = [vardescriptions, '']
    
  endfor
  
  varid = varid[1:*]
  varnames = varnames[1:*]
  varunits = varunits[1:*]
  vardescriptions = vardescriptions[1:*]
  varndims = varndims[1:*]
  vartypes = vartypes[1:*]
  
  if KEYWORD_SET(PRINTVARS) then for i = 0, self.Nvars-1 DO print, 'Id: ' + str_equiv(varid[i]) + $
       '. Name: ' + varnames[i] + '. Unit: ' + varunits[i] + '. Ndims: ' + str_equiv(varndims[i])+ $
         '. Type: ' + VARTYPES[i] + '. Description: ' + vardescriptions[i]
  
end


;+
; :Description:
;    Extracts the desired variable from the HDF file
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid: in, required, type = integer/ string
;           HDF SD index (int) or name (string) of the desired variable
;         TODO: DOC: Describe keywords (check) 
; :Keywords:
;        COUNT: in, optional, type = integer vector
;               An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        NOREVERSE: in, optional, type = integer vector
;                   An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        START: in, optional, type = integer vector 
;               An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        STRIDE: in, optional, type = integer vector
;                An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        description: out, optional, type = string 
;                     If available, the description of the variable
;        units: out, optional, type = string 
;               If available, the units of the variable
;        varname: out, optional, type = string
;                 the name of the variable
;        dims : out, optional, type = long
;               the variable dimensions
;        NO_CALIB: in, optional, type = string
;                  the default behaviour id to check if calibration data is contained 
;                  in the HDF variable attributes and apply it to the variable. Set this
;                  keyword to avoid making an automatic calibration
;                  
; :History:
;     Written by FaM, 2010.
;-
function w_HDF::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to HDF_VARDEF or HDF_VARID, or the name of the variable. 
                        COUNT=count, $ ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                        NOREVERSE = noreverse, $ ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                        START=start, $  ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                        STRIDE=stride, $ ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                        description = description , $ ; If available, the description of the variable
                        units = units, $ ; If available, the units of the variable
                        varname = varname , $  ; the name of the variable
                        dims = dims, $ ; the variable dimensions
                        NO_CALIB = no_calib
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR,2
  
  if ~self->get_Var_Info (Varid, $
                            out_id = vid, $ 
                            units = units, $
                            description = description, $
                            varname = varname , $ 
                            dims = dimss) then Message, '$Varid is not a correct variable ID'
  
  sdID = HDF_SD_Select(self.HDFid, vid)
  
  ; This routine throws all kinds of scary messages
  _quiet = !QUIET
  !QUIET = 1
  HDF_SD_GetInfo, sdID, DIMS=dims, NAME=varname, NATTS=natts, NDIMS=ndims, $
        RANGE=range, TYPE=datatype, CALDATA=calData
  !QUIET = _quiet
  
  HDF_SD_GETDATA, sdID, Data, COUNT=COUNT, NOREVERSE=NOREVERSE, START=START, STRIDE=STRIDE
  IF calData.cal NE 0 and ~KEYWORD_SET(NO_CALIB) THEN data = calData.cal * (Temporary(data) - calData.offset)
  
  HDF_SD_ENDACCESS, sdID
  
  return, data
  
end

;+
; :Description:
;    This function checks if a variable ID is valid and returns 1 if it is. Additionally,
;    it tries to obtain a maximum of information about the desired variable.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid: in, required, type = string/integer
;           the variable ID (string or integer) to check
;
; :Keywords:
;   out_id: out, type = long
;           the hdf  variable ID (long)
;   description: out, type = string
;               If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions
; 
; :Returns:
;         1 if the variable id is valid, 0 if not
; 
; :History:
;     Written by FaM, 2010.
;-
function w_HDF::get_Var_Info, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                            out_id = out_id, $
                            units = units, $
                            description = description, $
                            varname = varname , $ ; 
                            dims = dims
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, FALSE
  ENDIF
  
  if arg_okay(VarId, TYPE=IDL_STRING, /SCALAR) then begin
    p = WHERE(str_equiv(*self.varNames) eq str_equiv(varid), cnt)
    if cnt ne 0 then out_id = p[0] else return, FALSE
  endif else if arg_okay(VarId, /INTEGER, /SCALAR) then begin
    if varid lt 0 or varid ge self.Nvars then return, FALSE
    out_id = varid    
  endif else MESSAGE, WAVE_Std_Message('VarId', /ARG)
  
  
  sdID = HDF_SD_Select(self.HDFid, out_id)
  ; This routine throws all kinds of scary messages
  _quiet = !QUIET 
  !QUIET = 1
  HDF_SD_GetInfo, sdID, DIMS=dims, NAME=varname, NATTS=natts, NDIMS=ndims, $
        RANGE=range, TYPE=datatype, CALDATA=calData
  !QUIET = _quiet
  
     
  if ARG_PRESENT(description) then begin 
    description = ''
    for i = 0, natts -1 do begin
      HDF_SD_ATTRINFO, sdID, i, DATA=theAttribute, NAME=AttName, TYPE=attribute_datatype
      theAttribute = String(theAttribute, /PRINT)
      theAttribute = STRMID(theAttribute, 0, STRLEN(theAttribute))
      if str_equiv(AttName) eq str_equiv('description') $
        or str_equiv(AttName) eq str_equiv('long_name') $
          then description = theAttribute
    endfor
  endif
  
  if ARG_PRESENT(units) then begin 
    units = ''
    for i = 0, natts -1 do begin
      HDF_SD_ATTRINFO, sdID, i, DATA=theAttribute, NAME=AttName, TYPE=attribute_datatype
      theAttribute = String(theAttribute, /PRINT)
      theAttribute = STRMID(theAttribute, 0, STRLEN(theAttribute))
      if str_equiv(AttName) eq str_equiv('units') $
        or str_equiv(AttName) eq str_equiv('unit') $
          then units = theAttribute
    endfor
  endif  
  HDF_SD_ENDACCESS, sdID
  
  return, TRUE
  
end


;+
; :Description:
;    Flat plot of a desired variable for quick visualisation purposes.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid: in, required, type = integer/ string
;           HDF SD index or name of the desired variable
;
; :Keywords:
;    NO_CALIB: in, optional, type = string
;              the default behaviour id to check if calibration data is contained 
;              in the HDF variable attributes and apply it to the variable. Set this
;              keyword to avoid making an automatic calibration
;    UPSIDEDOWN: in, optional
;                to rotate the variable before plotting it
;    WID: out
;         the widget id
;       
; :History:
;     Written by FaM, 2010.
;-
pro w_HDF::QuickPlotVar, Varid, NO_CALIB = NO_CALIB, UPSIDEDOWN = UPSIDEDOWN, WID = wid

  var = self->get_Var(Varid, varname = varname, units = units, DESCRIPTION=DESCRIPTION, NO_CALIB = NO_CALIB)
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  if KEYWORD_SET(UPSIDEDOWN) then var = ROTATE(var, 7)
  
  w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='HDF view: ' + self.fname, CBARTITLE=units, WID = wid

end

;+
; :Description:
;    To write all infos contained in the HDF file to an ASCII file.
;    Output: an ascii file
;
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;    FILE: in, optional, type = string
;          An optional string containing the path to the output ASCII file. If not set, a dialog window will open
;    NO_GATTS: in, optional, type = string
;              Global attributes wont be written in the ASCII file 
;    NO_VARIABLES: in, optional, type = string
;                  variable wont be written in the ASCII file 
;    LUN: in, optional, type = string
;         If given, the dum routine will write into the given lun (/GET_LUN)
;
; :History:
;     Written by FaM, 2010.
;-
PRO w_HDF::dump, FILE = file, NO_GATTS = no_gatts, NO_VARIABLES = no_variables, LUN = lun

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Create dump file
  if not KEYWORD_SET(lun) then begin
  
    if N_ELEMENTS(file) eq 0 then begin
      isPoint = STRPOS(self.fname, '.')
      if isPoint ne -1 then file = Dialog_Pickfile(/Write, File= (STRSPLIT(self.fname,'.', /EXTRACT))[0] + '_dump.txt') $
      else file = Dialog_Pickfile(/Write, File= self.fname + '_dump.txt')
    endif
    
    if file eq '' then return
    
    
    OPENW, lu, file, /GET_LUN
    printf, lu, '{'
    printf, lu, ''
    text = 'hdf : ' + self.fname
    printf, lu, text
    text = 'directory : ' + self.directory
    printf, lu, text
  endif else lu = lun
  
  IF self.Ngatts GT 0 and ~KEYWORD_SET(no_gatts) THEN BEGIN
    printf, lu, ' '
    printf, lu, '----------------------'
    printf, lu, '* Global attributes * '
    printf, lu, '----------------------'
    printf, lu, ' '
    ;Go threw the global Attributes.
    for i =0, self.Ngatts-1 do begin
      HDF_SD_ATTRINFO, self.HDFid, i, DATA=theAttribute, HDF_TYPE=hdf_type, NAME=attribute_name, TYPE=att_type
      theAttribute = String(theAttribute, /PRINT)
      theAttribute = STRMID(theAttribute, 0, STRLEN(theAttribute))
      
      text = '' + attribute_name + ' = ' + theAttribute
      printf, lu, text
      
    endfor ; Att OK
  ENDIF
  
  ; Next, get the variables.
  IF self.Nvars GT 0 and ~KEYWORD_SET(no_variables) THEN BEGIN
    printf, lu, ' '
    printf, lu, '--------------'
    printf, lu, '* Variables * '
    printf, lu, '--------------'
    printf, lu, ' '
    
    FOR j=0, self.Nvars-1 DO BEGIN
    
      ; Get information about the variable.
      sdID = HDF_SD_Select(self.HDFid, j)
      
      ; This routine throws all kinds of scary messages if CALDATA, for example, is
      ; not in the file. Turn this off for this call.
      _quiet = !QUIET
      !QUIET = 1
      HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
        RANGE=range, TYPE=datatype, CALDATA=calData
      !QUIET = _quiet
      
      text = '       HDF_SD Ind: ' + str_equiv(j) + '. Type: ' + STRLOWCASE(DATATYPE) + '. ' + STRING(name) + '. Dim: ('
      for i =0, ndims - 1 do begin
        text += STRLOWCASE(str_equiv(dims[i]))
        if i ne ndims - 1 then  text += ','
      endfor
      text += ').'
      if N_ELEMENTS(range) ne 0 then text += ' Range: [' + str_equiv(STRING(range[0],/PRint)) + ',' + str_equiv(STRING(range[1],/PRint))  + ']'
      printf, lu, text
      
      
      
      ; If this variable has attributes, get those, too.
      if natts eq 0 then continue ; no need to continue
      
      ; Copy the variable attributes
      FOR k=0,natts-1 DO BEGIN
        HDF_SD_ATTRINFO, sdID, k, DATA=theAttribute, NAME=attribute_name, TYPE=attribute_datatype
        theAttribute = String(theAttribute, /PRINT)
        theAttribute = STRMID(theAttribute, 0, STRLEN(theAttribute))
        text = '               ' + str_equiv(attribute_datatype) + ':' + str_equiv(attribute_name) + ' = ' + theAttribute
        printf, lu, text
      ENDFOR
      HDF_SD_ENDACCESS, sdID
      
      ; Add the calibration data ?
      text = '                 Is calibrated? '
      IF calData.cal NE 0 THEN begin
        text += 'Yes'
        printf, lu, text
        text =   '                  Cal        : ' + str_equiv( calData.cal)
        printf, lu, text
        text =   '                  Cal err    : ' + str_equiv( calData.cal)
        printf, lu, text
        text =   '                  Offset     : ' + str_equiv( calData.offset)
        printf, lu, text
        text =   '                  Offset err : ' + str_equiv( calData.offset)
        printf, lu, text
        text =   '                  Num type   : ' + str_equiv( calData.num_type)
        printf, lu, text
      ENDIF else begin
        text += 'No'
        printf, lu, text
      endelse
      printf, lu, ''
    endfor
  ENDIF
  
  if not KEYWORD_SET(lun) then begin
    printf, lu, ''
    printf, lu, '}'
    
    close, lu ; close file
  endif

  
end
