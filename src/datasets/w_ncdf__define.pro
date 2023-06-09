; docformat = 'rst'
;+
;
;  w_NCDF is the basis class for all kinds of NCDF files. It reads
;  NCDF files and provides some tools for rapid visualisation and
;  variable retrieval.
;       
;  It should be the superclass from all NCDF objects 
;  
; :Categories:
;         datasets
;              
; :Properties: 
;    
;    FILE: in, optional, type = string
;          the path to the NCDF file. If not set, a dialog window will open
;    path: out, type = string
;          complete path of the active NCDF file
;    cdfid: out, type = long
;          id of the ncdf file as given by NCDF_open
;    fname: out, type = string
;          name of the active NCDF file
;    Ndims: out, type = long
;           the number of dimensions
;    Nvars: out, type = long
;           The number of variables defined for this NCDF file. 
;    Ngatts: out, type = long
;            The number of global attributes defined for this NCDF file. 
;    varNames: out, type = string array
;              An array of (nVars) strings containing the variable names. 
;    dimNames: out, type = string array
;              An array of (nDims) strings containing the dimension names. 
;    gattNames: out, type = string array
;               An array of (nGatts) strings containing the global attribute names. 
;    dimSizes: out, type = long array
;              An array of (nDims) longs containing the dimension sizes. 
;    RecDim :  out, type = long
;              The ID of the unlimited dimension, if there is one, for this NetCDF file
;                    
; :Author:
;    FaM
;       
; :History:
;    Written by FaM, 2010
;    04.05.2012, FaM, revisited
;    
;-

;+
; :Description:
;    Build function. 
;
; :Keywords:
;    FILE: in, optional, type=string
;          the path to the NCDF file. If not set, a dialog window will open
;          
; :Returns:
;    1 if the NCDF object is updated successfully, 0 if not
;    
;-
Function w_NCDF::Init, FILE=file
           
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    if self.cdfid gt 0 then ncdf_close, self.cdfid
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select NCDF file to read', /MUST_EXIST)
  IF file EQ '' THEN MESSAGE, WAVE_Std_Message(/FILE)
  
  ;*****************
  ; Check filename *
  ;*****************  
  fname = FILE_BASENAME(file)
  
  ;****************
  ; Read metadata *
  ;****************    
  cdfid = NCDF_OPEN(file, /NOWRITE)
  inq = NCDF_INQUIRE(cdfid)
    
  self.path = file
  self.cdfid = cdfid
  self.fname = fname
  self.Ndims = inq.Ndims
  self.Nvars = inq.Nvars
  self.Ngatts = inq.Ngatts
  self.RecDim = inq.RecDim      
  
  self->w_NCDF::get_Varlist, varid, varnames
  self.varNames = PTR_NEW(varnames, /NO_COPY)
  self->w_NCDF::get_gattsList, gattsIds, gattNames
  self.gattNames = PTR_NEW(gattNames, /NO_COPY)
  self->w_NCDF::get_dimList, dimIds, dimNames, dimSizes
  self.dimNames = PTR_NEW(dimNames, /NO_COPY)
  self.dimSizes = PTR_NEW(dimSizes, /NO_COPY)
    
  RETURN, 1
  
END

;+
; :Description:
;    Destroy function. 
;
;-
pro w_NCDF::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    if STRMID(!Error_State.Msg, 11, 12, /REVERSE_OFFSET) ne 'valid cdfid.' then $
      ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF

  PTR_FREE, self.varNames
  PTR_FREE, self.dimNames
  PTR_FREE, self.dimSizes
  PTR_FREE, self.gattNames
  NCDF_CLOSE, self.cdfid

END

;+
; :Description:
;    Get access to some params. 
;
;
; :Keywords:
;    PATH: out, type=string
;          complete path of the active NCDF file
;    CDFID: out, type=long
;          id of the ncdf file as given by NCDF_open
;    FNAME: out, type=string
;           name of the active NCDF file
;    DIRECTORY: out, type=string
;               directory of the active NCDF file
;    NDIMS: out, type=long
;           the number of dimensions
;    NVARS: out, type=long
;           The number of variables defined for this NCDF file. 
;    NGATTS: out, type=long
;            The number of global attributes defined for this NCDF file. 
;    VARNAMES: out, type=string array
;              An array of (nVars) strings containing the variable names. 
;    DIMNAMES: out, type=string array
;              An array of (nDims) strings containing the dimension names. 
;    GATTNAMES: out, type=string array
;                An array of (nGatts) strings containing the attributes names. 
;    DIMSIZES: out, type=long array
;              An array of (nDims) longs containing the dimension sizes. 
;    RECDIM :  out, type=long
;              The ID of the unlimited dimension, if there is one, for this NetCDF file
;
;-
PRO w_NCDF::GetProperty, $
    PATH=path, $
    CDFID=cdfid, $
    FNAME=fname, $
    NDIMS=Ndims, $
    NVARS=Nvars, $
    NGATTS=Ngatts, $
    VARNAMES=varNames, $
    DIMNAMES=dimNames, $
    GATTNAMES=gattNames, $
    DIMSIZES=dimSizes, $
    RECDIM=RecDim
    
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
  IF Arg_Present(cdfid) NE 0 THEN cdfid = self.cdfid
  IF Arg_Present(Ndims) NE 0 THEN Ndims = self.Ndims
  IF Arg_Present(Nvars) NE 0 THEN Nvars = self.Nvars
  IF Arg_Present(Ngatts) NE 0 THEN Ngatts = self.Ngatts
  IF Arg_Present(varNames) NE 0 THEN varNames = self.varNames
  IF Arg_Present(gattNames) NE 0 THEN gattNames = self.gattNames
  IF Arg_Present(dimNames) NE 0 THEN dimNames = self.dimNames
  IF Arg_Present(dimSizes) NE 0 THEN dimSizes = self.dimSizes
  IF Arg_Present(RecDim) NE 0 THEN RecDim = self.RecDim
  
end

;+
; :Description:
;    Get some informations on available variables in the NCDF file 
;         
; :Params:
;    varid: out, type=long
;           NCDF var indexes
;    varnames: out, type=string
;              variables name
;    varndims: out, type=long
;              variables number of dimensions
;    varunits: out, type=string
;              variables units
;    vardescriptions: out, type=string
;                     variables description (or long name)
;    vartypes: out, type=string
;              variables type 
;
; :Keywords:
;    PRINTVARS: in, optional
;               to print the infos in the console
;               
; :History:
;     Written by FaM, 2010.
;-
pro w_NCDF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS=printvars

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
  
  FOR svid=0, self.Nvars-1 DO BEGIN
  
    s_var_info = NCDF_VARINQ(self.cdfid,svid)
   
    varid    = [varid, svid]
    vartypes = [vartypes, s_var_info.datatype]      
    varnames = [varnames, s_var_info.name]
    varndims = [varndims, s_var_info.ndims]
    
    
    if s_var_info.natts ne 0 then begin ; no need to continue
    
      ; Copy the variable attributes
      for sattid = 0, s_var_info.NATTS - 1 do begin      
        sName = NCDF_ATTNAME(self.cdfid, svid, sattid)
        sAtt_info = NCDF_attINQ(self.cdfid, svid, sName)
        NCDF_ATTGET, self.cdfid, svid, sName, sValue       
        if str_equiv(sName) eq str_equiv('description') $
          or str_equiv(sName) eq str_equiv('long_name') $
            then vardescriptions = [vardescriptions, str_equiv(sValue)]
        if str_equiv(sName) eq str_equiv('units') $
          or str_equiv(sName) eq str_equiv('unit') $
            then varunits = [varunits, str_equiv(sValue)]        
        
      endfor
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

;+
; :Description:
;    Get some informations on the dimensions of the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    dimIds: out, type = long
;            NCDF dimensions indexes
;    dimNames: out, type = string array
;              dimensions name
;    dimSizes: out, type = long array 
;              size of the dimensions
;
; :Keywords:
;    PRINTDIMS: in, optional
;               to print the infos in the console
;
; :History:
;     Written by FaM, 2010.
;-
pro w_NCDF::get_dimList, dimIds, dimNames, dimSizes, PRINTDIMS = printdims

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
    
    ;Go threw the dimensions. 
  UNDEFINE, dimIds, dimNames, dimSizes
  for i =0, self.ndims-1 do begin
    NCDF_DIMINQ, self.cdfid, i, sName, sSize
    if N_ELEMENTS(dimNames) eq 0 then dimNames = sName else dimNames=[dimNames,sName]
    if N_ELEMENTS(dimSizes) eq 0 then dimSizes = sSize else dimSizes=[dimSizes,sSize] 
    if N_ELEMENTS(dimids) eq 0 then dimids = i else dimids=[dimids,i] 
  endfor ; Dimensions OK
  
  if KEYWORD_SET(PRINTDIMS) then for i = 0, self.Ndims-1 DO print, 'Id: ' + str_equiv(dimids[i]) + $
       '. Name: ' + dimNames[i] + '. Size: ' + str_equiv(dimSizes[i])
  
end

;+
; :Description:
;    Get some informations on the global attributes of the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    gattsIds: out, type = long
;              NCDF gatts indexes
;    gattNames: out, type = string array
;              attributes name
;
; :Keywords:
;    PRINTGATTS: in, optional
;               to print the infos in the console
;               
; :History:
;     Written by FaM, 2010.
;-
pro w_NCDF::get_gattsList, gattsIds, gattNames, PRINTGATTS = printgatts

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
    
  ;Go threw the dimensions. 
  UNDEFINE, gattsIds, gattNames
  for i =0, self.Ngatts-1 do begin
    sName = NCDF_ATTNAME(self.cdfid, i , /GLOBAL)        
    if N_ELEMENTS(gattNames) eq 0 then gattNames = sName else gattNames=[gattNames,sName]
    if N_ELEMENTS(gattsIds) eq 0 then gattsIds = i else gattsIds=[gattsIds,i] 
  endfor ; Dimensions OK
  
  if KEYWORD_SET(PRINTGATTS) then for i = 0, self.Ngatts-1 DO print, 'Id: ' + str_equiv(gattsIds[i]) + $
       '. Name: ' + gattNames[i]
  
end

;+
; :Description:
;    Get some informations on variable attributes from a NCDF variable.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    varid: in, type = long/string
;            the variable id 
;    vattsIds: out, type = long
;              NCDF vatts indexes
;    vattNames: out, type = string array
;               attributes name
;
; :Keywords:
;    PRINTVATTS: in, optional
;                to print the infos in the console
;
; :History:
;    Written by FaM, 2010.
;-
pro w_NCDF::get_VattsList, varid, vattsIds, vattNames, PRINTVATTS = printvatts

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if not self->w_NCDF::get_Var_Info(varid, out_id = out_id) then MESSAGE, WAVE_Std_Message('VarId', /ARG)  
  
  s_var_info = NCDF_VARINQ(self.cdfid,out_id)
       
  if s_var_info.natts eq 0 then return ; no need to continue
 
  ; Copy the variable attributes
  UNDEFINE, vattsIds, vattNames
  for sattid = 0, s_var_info.NATTS - 1 do begin
    
    sName = NCDF_ATTNAME(self.cdfid, out_id, sattid)
    if N_ELEMENTS(vattNames) eq 0 then vattNames = sName else vattNames=[vattNames,sName]
    if N_ELEMENTS(vattsIds) eq 0 then vattsIds = sattid else vattsIds=[vattsIds,sattid] 

  endfor

  if KEYWORD_SET(PRINTVATTS) then for i = 0, s_var_info.natts-1 DO print, 'Id: ' + str_equiv(vattsIds[i]) + $
       '. Name: ' + vattNames[i]
  
end

;+
; :Description:
;    Extracts the desired variable from the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    varid: in, required, type = long/str
;           the netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable
;           (CASE INDEPENDENT)
;       
; :Keywords:
;    COUNT: in, optional, type = integer vector
;           An optional vector containing the counts to be used in reading Value. COUNT is a 1-based vector with an element for each dimension of the data to be written.The default matches the size of the variable so that all data is written out. 
;    OFFSET: in, optional, type = integer vector
;            An optional vector containing the starting position for the read. The default start position is [0, 0, ...]. 
;    STRIDE: in, optional, type = integer vector
;            An optional vector containing the strides, or sampling intervals, between accessed values of the netCDF variable. The default stride vector is that for a contiguous read, [1, 1, ...]. 
;   description: out, type = string
;                If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions (if the variable is cropped, the dimensions are updated too)
;   dimnames: out, type = string
;             the dimensions names (if the variable is cropped, the dimension names are updated too)
; 
; :Returns:
;    The variable
;       
; :History:
;     Written by FaM, 2010.
;-
function w_NCDF::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                        COUNT=COUNT, $ ; An optional vector containing the counts to be used in reading Value. COUNT is a 1-based vector with an element for each dimension of the data to be written.The default matches the size of the variable so that all data is written out. 
                        OFFSET=OFFSET, $ ; An optional vector containing the starting position for the read. The default start position is [0, 0, ...]. 
                        STRIDE=STRIDE, $ ; An optional vector containing the strides, or sampling intervals, between accessed values of the netCDF variable. The default stride vector is that for a contiguous read, [1, 1, ...]. 
                        units = units, $
                        description = description, $
                        varname = varname , $ ; 
                        dims = dims, $ ;
                        dimnames = dimnames ;
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  ON_ERROR, 2  
  
  if ~self->w_NCDF::get_Var_Info (Varid, $
                            out_id = vid, $
                            units = units, $
                            description = description, $
                            varname = varname , $ 
                            dims = dims, $ 
                            dimnames = dimnames) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'

  if N_ELEMENTS(STRIDE) ne 0 then begin
   if max(stride) ne 1 then _stride = stride ; this is because stride is much slowier
  endif
  NCDF_VARGET, self.Cdfid, vid, Value, COUNT=count, OFFSET=offset, STRIDE=_stride
  
  ; Is the variable scaled ?
  is_scale = self->w_NCDF::get_VAtt_Info(varid, 'scale_factor')
  if is_scale then value *= self->w_NCDF::get_VAtt(varid, 'scale_factor') 
  is_offset = self->w_NCDF::get_VAtt_Info(varid, 'add_offset')
  if is_offset then value += self->w_NCDF::get_VAtt(varid, 'add_offset')
  
  ; Is there (floating) missing data?
  miss_att = ['missing_value', '_fillvalue']
  for i=0, N_ELEMENTS(miss_att)-1 do begin
    is_missing = self->w_NCDF::get_VAtt_Info(varid, miss_att[i])
    if ~ is_missing then continue
    tname = TYPENAME(value)
    if ~(tname eq 'DOUBLE' or tname eq 'FLOAT') then continue
    pmissing = where(value eq self->w_NCDF::get_VAtt(varid, miss_att[i]), nmiss)
    if nmiss gt 0 then value[pmissing] = !VALUES.F_NAN
  endfor
  
  if N_ELEMENTS(count) ne 0 then begin ;Check if we have to change the variable metadata
    pr = where(count le 1, cnt)
    if cnt eq N_ELEMENTS(dimnames) then dimnames = '' else if cnt ne 0 then utils_array_remove, pr, dimnames
    dims = size(Value, /DIMENSIONS)
    pr = where(dims le 1, cnt)
    if cnt eq N_ELEMENTS(dims) then dims = 1 else if cnt ne 0 then utils_array_remove, pr, dims
  endif
  
  return, reform(value)
  
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
;           the netcdf variable ID (long)
;   description: out, type = string
;               If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions
;   dimnames: out, type = string
;             the dimensions names
; 
; :Returns:
;         1 if the variable id is valid, 0 if not
;
;       :History:
;     Written by FaM, 2010.
;-
function w_NCDF::get_Var_Info, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                            out_id = out_id, $
                            units = units, $
                            description = description, $
                            varname = varname , $ ; 
                            dims = dims, $ ;
                            dimnames = dimnames ;
                        
  
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
    
  varinfo = NCDF_VARINQ(self.Cdfid, out_id)
  
  if ARG_PRESENT(varname) then varname = varinfo.Name
  ;TODO: Update routine: take account of the new self.gevatt routines
  if ARG_PRESENT(description) then begin 
    description = ''
    for i = 0, varinfo.natts -1 do begin
      AttName = NCDF_ATTNAME(self.Cdfid, out_id, i)
      if str_equiv(AttName) eq str_equiv('description') $
        or str_equiv(AttName) eq str_equiv('long_name') $
          or str_equiv(AttName) eq str_equiv('des') $
          then NCDF_ATTGET, self.Cdfid, out_id, AttName, description
    endfor
    description = STRING(description)
  endif
  
  if ARG_PRESENT(units) then begin 
    units = ''
    for i = 0, varinfo.natts -1 do begin
      AttName = NCDF_ATTNAME(self.Cdfid, out_id, i)
      if str_equiv(AttName) eq str_equiv('units') $
        or str_equiv(AttName) eq str_equiv('unit') $
          then NCDF_ATTGET, self.Cdfid, out_id, AttName, units
    endfor
    units = STRING(units)
  endif  
  
  if ARG_PRESENT(dims) then begin
    if varinfo.Ndims lt 1 then ndims = N_ELEMENTS(varinfo.dim) else ndims =varinfo.Ndims
    dims = LONARR(Ndims)
    for i = 0, Ndims - 1 do begin
      NCDF_DIMINQ, self.Cdfid, varinfo.Dim[i], dimName, dimSize
      dims[i] = dimSize
    endfor
  endif
  
  if ARG_PRESENT(dimnames) then begin
    if varinfo.Ndims lt 1 then ndims = N_ELEMENTS(varinfo.dim) else ndims =varinfo.Ndims
    dimnames = STRARR(Ndims)
    for i = 0, Ndims - 1 do begin
      NCDF_DIMINQ, self.Cdfid, varinfo.Dim[i], dimName, dimSize
      dimnames[i] = dimName
    endfor
  endif
  
  return, TRUE
  
end


;+
; :Description:
;    Extracts the desired attribute from the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    attid: in, required, type = long/str
;           the netCDF attribute ID, or the name of the attribute
;           (CASE INDEPENDENT)
;       
; :Keywords:
;
; :Returns:
;    The global attribute
;
; :History:
;     Written by FaM, 2010.
;-
function w_NCDF::get_Gatt, attid 
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  ON_ERROR, 2  
  
  if ~self->w_NCDF::get_Gatt_Info(attid, OUT_id = outid) then Message, '$attid is not a correct attribute ID'

  NCDF_ATTGET, self.Cdfid , outid, Value, /GLOBAL
  
  return, value
  
end

;+
; :Description:
;    This function checks if an attribute ID is valid and returns 1 if it is.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    attid: in, required, type = string/ integer
;           the attribute ID (string or integer) to check
;
; :Keywords:
;   out_id: out, type = string
;           the netcdf attribute ID (string)
; :Returns:
;         1 if the attribute id is valid, 0 if not
;
;       Modified::
;         Written by FaM, 2010.
;
;-
function w_NCDF::get_Gatt_Info, attid, OUT_ID = out_id
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, FALSE
  ENDIF
  out_id = -1
  
  if arg_okay(attid, TYPE=IDL_STRING, /SCALAR) then begin
    p = WHERE(str_equiv(*self.gattNames) eq str_equiv(attid), cnt)
    if cnt ne 0 then out_id = (*self.gattNames)[p[0]] else return, FALSE
  endif else MESSAGE, WAVE_Std_Message('attid', /ARG)
   
  return, TRUE
  
end

;+
; :Description:
;    Extracts the desired dimension from the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    attid: in, required, type = long/str
;           the netCDF dimension ID, or the name of the dimension
;           (CASE INDEPENDENT)
;       
; :Keywords:
;
; :Returns:
;    The dimension
;
; :History:
;     Written by FaM, 2010.
;-
function w_NCDF::get_Dim, dimid 
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  ON_ERROR, 2  
  
  if ~self->w_NCDF::get_Dim_Info(dimid, OUT_id=outid) then Message, '$dimid is not a correct dimension ID'
  
  return, (*self.dimSizes)[outid]
  
end

;+
; :Description:
;    This function checks if an dimension ID is valid and returns 1 if it is.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    attid: in, required, type = string/ integer
;           the dimension ID (string or integer) to check
;
; :Keywords:
;   dimid: out, type = string
;          the netcdf dimension ID (string)
; :Returns:
;         1 if the attribute id is valid, 0 if not
;
;       Modified::
;         Written by FaM, 2010.
;
;-
function w_NCDF::get_Dim_Info, dimid, OUT_ID = out_id
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, FALSE
  ENDIF
  out_id = -1
  
  if arg_okay(dimid, TYPE=IDL_STRING, /SCALAR) then begin
    p = WHERE(str_equiv(*self.dimNames) eq str_equiv(dimid), cnt)
    if cnt ne 0 then out_id = p[0] else return, FALSE
  endif else MESSAGE, WAVE_Std_Message('dimid', /ARG)
   
  return, TRUE
  
end


;+
; :Description:
;    Extracts the desired variable attribute from the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    varid: in, required, type = long/str
;
;    attid: in, required, type = str
;       
; :Keywords:
;
; :Returns:
;    The variable
;       
; :History:
;     Written by RoF, 2011.
;-
function w_NCDF::get_VAtt, varid, attid

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  if ~arg_okay(attid) then message, WAVE_Std_Message('attid', /ARG)
  
  if not self->w_NCDF::get_Var_Info(varid, out_id = var_id) then MESSAGE, WAVE_Std_Message('VarId', /ARG)
  if not self->w_NCDF::get_VAtt_Info(var_id, attid, OUT_ID = att_id) then MESSAGE, WAVE_Std_Message('attid', /ARG)
  
  NCDF_ATTGET, self.cdfid, var_id, att_id, value
  
  return, value
  
end


;+
; :Description:
;    Extracts the desired variable attribute from the NCDF file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    varid: in, required, type = long/str
;
;    attid: in, required, type = str
;           (case independent)
;       
; :Keywords: 
; 
;    OUT_ID: out, optional, type = str
;            the equivalent attribute ID (case dependent)
;
; :Returns:
;    The variable
;
; :History:
;     Written by RoF,2011.
;-
function w_NCDF::get_VAtt_Info, varid, attid, OUT_ID = out_id

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, FALSE
  ENDIF
    
  if not self->w_NCDF::get_Var_Info(varid, out_id = var_id) then return, false
  self->w_NCDF::get_VattsList, var_id, vattsids, vatssnames
  
  if arg_okay(attid, TYPE=IDL_STRING, /SCALAR) then begin
    p = WHERE(str_equiv(vatssnames) eq str_equiv(attid), cnt)
    if cnt ne 0 then out_id = vatssnames[p[0]] else return, FALSE
  endif else MESSAGE, WAVE_Std_Message('attid', /ARG) 
  
  return, TRUE
  
end

;+
; :Description:
;    Plots a desired variable for quick visualisation purposes.
;    Output: a plot
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid : in, required, type = integer/ string
;            variable index or name of the desired variable
;
; :Keywords:
;    UPSIDEDOWN: in, optional
;                to rotate the variable before plotting it
;    WID: out
;         the widget id
;         
; :History:
;     Written by FaM, 2010.
;-
pro w_NCDF::QuickPlotVar, Varid, UPSIDEDOWN = UPSIDEDOWN, WID = wid

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if ~self->w_NCDF::get_Var_Info(Varid) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'

  var = self->w_NCDF::get_Var(Varid, varname = varname, dimnames = dimnames, units = units, DESCRIPTION=DESCRIPTION)
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  if KEYWORD_SET(UPSIDEDOWN) then var = ROTATE(var, 7)
  
  w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='NCDF view: ' + self.fname, dimnames = dimnames, CBARTITLE=units, WID = wid

end

;+
; :Description:
;    Writes all infos contained in the ncdf file to an ASCII file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;    FILE: in, optional, type = string
;          An optional string containing the path to the output ASCII file. 
;          If not set, a dialog window will open
;    SAMEDIR: in, optional
;             if set the dumpfile is creaated in the same directory as 
;             the original file
; :History:
;     Written by FaM, 2010.
;-
PRO w_NCDF::dump, FILE=file, SAMEDIR=samedir

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if KEYWORD_SET(SAMEDIR) then begin
    file = file_dirname(self.path) + path_sep() + file_basename(self.fname, '.nc') + '_dump.txt'
  endif

  if N_ELEMENTS(file) eq 0 then begin
    isPoint = STRPOS(self.fname, '.')
    if isPoint ne -1 then file = Dialog_Pickfile(/Write, File= (STRSPLIT(self.fname,'.', /EXTRACT))[0] + '_dump.txt') $
      else file = Dialog_Pickfile(/Write, File= self.fname + '_dump.txt')
  endif
  
  if file eq '' then return
  
  sid = self.cdfid

  ; Create dump file
  OPENW, lu, file, /GET_LUN
  
  text = 'netcdf : ' + self.fname 
  printf, lu, text 
  text = 'directory : ' + FILE_DIRNAME(self.path) 
  printf, lu, text 
  printf, lu, '{'
  printf, lu, ''
  printf, lu, '---------------'
  printf, lu, '* Dimensions * ' 
  printf, lu, '---------------'
  printf, lu, ''
  
  ;Go threw the dimensions. 
  for i =0, self.ndims-1 do begin
    NCDF_DIMINQ, sid, i, sName, sSize
    if N_ELEMENTS(t_dim_names) eq 0 then t_dim_names = sName else t_dim_names=[t_dim_names,sName]
    if N_ELEMENTS(t_dim_Sizes) eq 0 then t_dim_Sizes = sSize else t_dim_Sizes=[t_dim_Sizes,sSize] 
    if i eq self.RecDim then printf, lu, '       ', sName, ' = UNLIMITED ; // ' + STR_equiv(sSize) + ' currently' $ 
     else printf, lu, '       ', sName, ' = ', STR_equiv(sSize) + ' ;'
  endfor ; Dimensions OK
  

  printf, lu, ' '
  printf, lu, '----------------------'
  printf, lu, '* Global attributes * ' 
  printf, lu, '----------------------'
  printf, lu, ' '
  
  ;Go threw the global Attributes.
  for i =0, self.Ngatts-1 do begin  
  
    sName = NCDF_ATTNAME(sid, i , /GLOBAL)        
    sAtt_info = NCDF_attINQ(sid, sName, /GLOBAL)
    NCDF_ATTGET, sid, sName, sValue, /GLOBAL
    
    text = '       ' + sName + ' = ' + STRING(sValue)
    printf, lu, text

  endfor ; Att OK
    
  printf, lu, ' '
  printf, lu, '--------------'
  printf, lu, '* Variables * ' 
  printf, lu, '--------------'
  printf, lu, ' '
    
  for svid =0, self.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,svid)
   
    ; If the dimension names are present, use them to get the dimension IDs, which are needed to define the variable.
    dimsIds = s_var_info.dim
    
    text = '       ' + STRLOWCASE(s_var_info.DATATYPE) + ' ' + s_var_info.name + '(' 
    for i =0, N_ELEMENTS(s_var_info.dim) - 1 do begin
      text += STRLOWCASE(str_equiv(t_dim_names[s_var_info.dim[i]]))
      if i ne N_ELEMENTS(s_var_info.dim) - 1 then  text += ','      
    endfor
    text += ')  ;'
    printf, lu, text
    
    if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
    
    ; Copy the variable attributes
    for sattid = 0, s_var_info.NATTS - 1 do begin
    
      sName = NCDF_ATTNAME(sid, svid, sattid)
      sAtt_info = NCDF_attINQ(sid, svid, sName)
      NCDF_ATTGET, sid, svid, sName, sValue
                          
      text = '               ' + s_var_info.name + ':' + string(sName) + ' = ' + STRLOWCASE(str_equiv(sValue)) + ' ;'
      printf, lu, text

    endfor
    
  endfor
  
  printf, lu, ''
  printf, lu, '}'
  
  close, lu ; close file  
  free_lun, lu
  
end

;+
; :Description:
;   Defines the attributes of the class w_NCDF. Attributes::
;    
;    NCDF                     
;            path:               ''    ,  $ ; complete path of the active ncdf file
;            cdfid:              0L    ,  $ ; id of the NCDF file as given by the NCDF_OPEN procedure
;            fname:              ''    ,  $ ; name of the active ncdf file
;            Ndims:              0L    ,  $ ; The number of dimensions defined for this NetCDF file. 
;            Nvars:              0L    ,  $ ; The number of variables defined for this NetCDF file. 
;            Ngatts:             0L    ,  $ ; The number of global attributes defined for this NetCDF file. 
;            varNames:    PTR_NEW()    ,  $ ; An array of (nVars) strings containing the variable names. 
;            dimNames:    PTR_NEW()    ,  $ ; An array of (nDims) strings containing the dimension names. 
;            gattNames:   PTR_NEW()    ,  $ ; An array of (Ngatts) strings containing the dimension names. 
;            dimSizes:    PTR_NEW()    ,  $ ; An array of (nDims) longs containing the dimension sizes. 
;            RecDim:             0L       $ ; The ID of the unlimited dimension, if there is one, for this NetCDF file. If there is no unlimited dimension, RecDim is set to -1. 
;            
;
; :Categories:
;         WAVE/OBJ_GIS   
; :Version:
;       WAVE V0.1
;       
; :History:
;     Written by FaM, 2010.
;-
pro w_NCDF__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {w_NCDF                    ,  $
            path:               ''    ,  $ ; complete path of the active ncdf file
            cdfid:              0L    ,  $ ; id of the NCDF file as given by the NCDF_OPEN procedure
            fname:              ''    ,  $ ; name of the active ncdf file
            Ndims:              0L    ,  $ ; The number of dimensions defined for this NetCDF file. 
            Nvars:              0L    ,  $ ; The number of variables defined for this NetCDF file. 
            Ngatts:             0L    ,  $ ; The number of global attributes defined for this NetCDF file. 
            varNames:    PTR_NEW()    ,  $ ; An array of (nVars) strings containing the variable names. 
            dimNames:    PTR_NEW()    ,  $ ; An array of (nDims) strings containing the dimension names. 
            gattNames:   PTR_NEW()    ,  $ ; An array of (Ngatts) strings containing the attribute names. 
            dimSizes:    PTR_NEW()    ,  $ ; An array of (nDims) longs containing the dimension sizes. 
            RecDim:             0L       $ ; The ID of the unlimited dimension, if there is one, for this NetCDF file. If there is no unlimited dimension, RecDim is set to -1. 
            }
    
end

