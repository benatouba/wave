;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  HDF__Define.pro                                       *
; Version     :  WAVE 0.1                                              *
; Language    :  IDL 7.0 and higher                                    *
; Date        :  2010                                                  *
; Last Update :  04-Nov-2010 FaM                                       *
;                                                                      *
; IDL class file for the WAVE library.                                 *
;                                                                      *
;***********************************************************************

;-----------------------------------------------------------------------
;+
; NAME:
;       GENERAL INFORMATION
;
;       HDF is the basis class for all kinds of HDF files. It reads
;       HDF files and provides some tools for rapid visualisation and
;       to analyse the content of a HDF file.
;       
;       It should be the superclass from all HDF related objects 
;              
;       =================================================================
;       Superclass:
;       ----------------------
;       none
;       
;       =================================================================
;       Attributes:
;       ----------------------
;          path: complete path of the active HDF file
;          HDFid: id of the HDF file as given by  HDF_start 
;          fname: name of the active HDF file
;          directory: directory of the active HDF file
;          Nvars: The number of variables defined for this HDF file. 
;          Ngatts: The number of global attributes defined for this HDF file. 
;    
;       =================================================================
;       Object initialisation:
;       ----------------------
;       KEYWORDS:
;         FILE: the path to the HDF file. If not set, a dialog window will open
;              
;       
;       =================================================================
;       Methods:
;       ----------------------
;       The following methods can be used directly. Non ducumented methods 
;       are not for external use.
;       
;       obj->GetProperty: get access to some attributes
;       obj->Get_Varlist: to obtain the list of available variables in the HDF file
;       obj->Get_Var()  : get a specific variable along with some information
;       obj->quickPlotVar    : plots a "flat" image of a given variable 
;                         (usefull for quick analysis purposes)
;       obj->dump       : to write all infos contained in the HDF file to an ASCII file.
;                         (usefull to get to know what is in the file)
;       
;       =================================================================
;       
;-
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF__Define
;
; PURPOSE:
;       Object structure definition
;
; CATEGORY:
;       WAVE grid objects
;       
; MODIFICATION HISTORY:
;       Written by: Fabien Maussion 2010
;       Modified:   04-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO HDF__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {HDF                       ,  $
            path:               ''    ,  $ ; complete path of the active HDF file
            HDFid:              0L    ,  $ ; id of the HDF file as given by  HDF_start 
            fname:              ''    ,  $ ; name of the active HDF file
            directory:          ''    ,  $ ; directory of the active HDF file
            Nvars:              0L    ,  $ ; The number of variables defined for this HDF file. 
            Ngatts:             0L       $ ; The number of global attributes defined for this HDF file. 
            }
    
END

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::Init
;
; PURPOSE:
;       Build function. 
;
; CATEGORY:
;       WAVE grid objects
;
; KEYWORDS:
;       FILE: the path to the HDF file. If not set, a dialog window will open
;
; OUTPUT:
;       1 if the HDF object is updated successfully, 0 if not
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
Function HDF::Init, FILE = file
           
           
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
 
  RETURN, 1
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::Cleanup
;
; PURPOSE:
;       Destroy. 
;
; CATEGORY:
;       WAVE grid objects
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;-
;-----------------------------------------------------------------------
pro HDF::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  HDF_SD_END, self.hdfID
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::GetProperty
;
; PURPOSE:
;       Get access to some params. 
;
; CATEGORY:
;       WAVE grid objects
; 
; KEYWORDS:
;       Output:
;       path : complete path of the active HDF file
;       HDFid : id of the HDF file as given by  HDF_start 
;       fname : name of the active HDF file
;       directory : directory of the active HDF file
;       Nvars : The number of variables defined for this HDF file. 
;       Ngatts : The number of global attributes defined for this HDF file. 
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO HDF::GetProperty, $
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

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::get_Varlist
;
; PURPOSE:
;       Get some informations on available variables in the HDF file
;
; CATEGORY:
;       WAVE grid objects
; 
; INPUT:
;       none 
;       
; OUTPUT:
;       varid : HDF SD indexes
;       varnames : variables name
;       varndims : variables number of dimensions
;       varunits : variables units
;       vardescriptions : variables description (or long name)
;       vartypes : variables type 
;       
; KEYWORDS:
;       /PRINTVARS : to print the infos in the console
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
pro HDF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS = printvars

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
    !QUIET = 1
    HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
      RANGE=range, TYPE=datatype, CALDATA=calData
    !QUIET = 0
    
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

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::get_Var
;
; PURPOSE:
;       extracts the desired variable from the HDF file
;
; CATEGORY:
;       WAVE grid objects
;       
; INPUT:
;       varid : HDF SD index (int) or name (string) of the desired variable
;       
; OUTPUT:
;       the variable
;       
; KEYWORDS:
;        COUNT: (I) An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        NOREVERSE:(I) An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        START:(I) An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        STRIDE:(I) An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
;        description: (O) If available, the description of the variable
;        units: (O) If available, the units of the variable
;        varname: (O)the name of the variable
;        dims : (O)the variable dimensions
;        /NO_CALIB: the default behaviour id to check if calibration data is contained 
;                   in the HDF variable attributes and apply it to the variable. Set this
;                   keyword to avoid making an automatic calibration
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
function HDF::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to HDF_VARDEF or HDF_VARID, or the name of the variable. 
                       COUNT=count, $ ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                       NOREVERSE = noreverse, $ ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                       START=start, $  ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                       STRIDE=stride, $ ; An optional vector containing the counts to be used in reading Value (see #HDF_SD_GetData#).
                       description = description , $ ; If available, the description of the variable
                       units = units, $ ; If available, the units of the variable
                       varname = varname , $  ; the name of the variable
                       dims = dims, $ ; the variable dimensions
                       NO_CALIB = no_calib ; the variable dimensions
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if var_info(varid,/TYPE) eq IDL_STRING then  iVarid = HDF_SD_NAMETOINDEX(SDinterface_id, SDS_Name) else ivarid = Varid
  
  sdID = HDF_SD_Select(self.HDFid, iVarid)
  ; This routine throws all kinds of scary messages
  !QUIET = 1
  HDF_SD_GetInfo, sdID, DIMS=dims, NAME=varname, NATTS=natts, NDIMS=ndims, $
        RANGE=range, TYPE=datatype, CALDATA=calData
  !QUIET = 0
  
  HDF_SD_GETDATA, sdID, Data, COUNT=COUNT, NOREVERSE=NOREVERSE, START=START, STRIDE=STRIDE
  IF calData.cal NE 0 and ~KEYWORD_SET(NO_CALIB) THEN data = calData.cal * (Temporary(data) - calData.offset)
    
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
  
  return, data
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::quickPlotVar
;
; PURPOSE:
;       flat plot of a desired variable for quick visualisation purposes
;
; CATEGORY:
;       WAVE grid objects
; 
; INPUT:
;       varid : HDF SD index or name of the desired variable
;       
; OUTPUT:
;       a plot
;       
; KEYWORDS:
;        /NO_CALIB: the default behaviour id to check if calibration data is contained 
;                   in the HDF variable attributes and apply it to the variable. Set this
;                   keyword to avoid making an automatic calibration
;        /UPSIDEDOWN: to rotate the variable before plotting it
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
pro HDF::quickPlotVar, Varid, NO_CALIB = NO_CALIB, UPSIDEDOWN = UPSIDEDOWN

  var = self->get_Var(Varid, varname = varname, units = units, DESCRIPTION=DESCRIPTION, NO_CALIB = NO_CALIB)
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  if KEYWORD_SET(UPSIDEDOWN) then var = ROTATE(var, 7)
  
  QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='HDF view: ' + self.fname, CBARTITLE=units

end

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF::dump
;
; PURPOSE:
;       to write all infos contained in the HDF file to an ASCII file.
;
; CATEGORY:
;       WAVE grid objects
; 
; INPUT:
;       none
;       
; OUTPUT:
;       an ASCII file
;       
; KEYWORDS:
;        FILE: (I) An optional string containing the path to the output ASCII file. If not set, a dialog window will open
;        LUN : (I) if given, the dum routine will write into the given lun (/GET_LUN)
;        /NO_GATTS: Global attributes wont be written in the ASCII file
;        /NO_VARIABLES: variable wont be written in the ASCII file
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO HDF::dump, FILE = file, NO_GATTS = no_gatts, NO_VARIABLES = no_variables, LUN = lun

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
      !QUIET = 1
      HDF_SD_GetInfo, sdID, DIMS=dims, NAME=name, NATTS=natts, NDIMS=ndims, $
        RANGE=range, TYPE=datatype, CALDATA=calData
      !QUIET = 0
      
      text = '       HDF_SD Ind: ' + str_equiv(j) + '. Type: ' + STRLOWCASE(DATATYPE) + '. ' + str_equiv(name) + '. Dim: ('
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
