; docformat = 'rst'
;+
;  
; Computes the altitudinal gradient for a product variable and stores it
; as a new product.
;        
; :Categories:
;    WPP
;    
; :Author:
;       FaM
;        
;-
;
;+
; :Description:
;    Computes the altitudinal gradient for a product variable and stores it
;    as a new product in the same directory.
;    
;    The gradient is computed
;
; :Params:
;    varid: in, required
;           the variable id to compute the gradient from
;
; :Keywords:
;    DIRECTORY: in, required
;               the path to the product directory where to take the variable from
;    FORCE: in, optional
;           set this keyword to force overwriting old products
;    KERNEL_SIZE: in, optional, default=3
;                 size of the kernel where to compute the gradient 
;                 should be uneven
;
;-
pro w_pr_AltitudinalGradient, varid, $
    DIRECTORY=directory, $
    FORCE=force, $
    KERNEL_SIZE=kernel_size
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  wpr = OBJ_NEW('w_WPR', DIRECTORY=directory)  
  if ~ OBJ_VALID(wpr) then return  
    
  ;Check if the variable is available
  if ~ wpr->hasVar(varid, INFO=info) then Message, 'Did not find variable: ' + str_equiv(varid)
    
  height = wpr->getVarData('hgt')
  
  if N_ELEMENTS(KERNEL_SIZE) eq 0 then kernel_size = 3
  if kernel_size mod 2 ne 1 then Message, 'Kernel size not uneven'
  
  gvn = 'grad_' + STRLOWCASE(varid) + '_ks' + str_equiv(kernel_size)
  grn = 'gradsig_' + STRLOWCASE(varid) + '_ks' + str_equiv(kernel_size)
    
  _y = wpr->getProperty('YEARS')
  for y=0, N_ELEMENTS(_y)-1 do begin
  
    obj = wpr->getVarObj(varid, (_y)[y], INFO=info)
    ok = obj->define_subset()
    obj->getProperty, Nvars=Nvars, PATH=fPath
    oPath = utils_replace_string(fPath, varid, gvn)
    orPath = utils_replace_string(fPath, varid, grn)
    
    ; Check
    if FILE_TEST(oPath) and ~KEYWORD_SET(FORCE) then Message, 'Output file already here. Set /FORCE if you want to overwrite it.'
    if FILE_TEST(orPath) and ~KEYWORD_SET(FORCE) then Message, 'Output file already here. Set /FORCE if you want to overwrite it.'
    
    ; Make File
    ; Open the source file in read-only mode.
    sObj = Obj_New('NCDF_FILE', fPath, $
      ErrorLoggerName='sourcefilelogger', /TIMESTAMP)
    IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
    
    ; Open the destination files for writing.
    dObj = Obj_New('NCDF_FILE', oPath, /CREATE, CLOBBER=force, /NETCDF4_FORMAT, $
      ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
    dObjSig = Obj_New('NCDF_FILE', orPath, /CREATE, CLOBBER=force, /NETCDF4_FORMAT, $
      ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObjSig) EQ 0 THEN Message, 'Destination object cannot be created.'
    
    ; Find all the global attributes in the source file and copy them.
    attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
    FOR j=0,attrCount-1 DO BEGIN
      if str_equiv(attrNames[j]) eq 'CREATION_DATE' then begin
        dObj->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
        dObjSig->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
        continue
      endif
      if str_equiv(attrNames[j]) eq 'VARNAME' then begin
        dObj->WriteGlobalAttr, 'VARNAME', gvn, DATATYPE='CHAR'
        dObjSig->WriteGlobalAttr, 'VARNAME', grn, DATATYPE='CHAR'
        continue
      endif
      sObj->CopyGlobalAttrTo, attrNames[j], dObj
      sObj->CopyGlobalAttrTo, attrNames[j], dObjSig      
    ENDFOR
    
    ; Find all the dimensions in the source file and copy them.
    dimNames = sObj->GetDimNames(COUNT=dimCount)
    FOR j=0,dimCount-1 DO BEGIN
      if str_equiv(dimNames[j]) eq 'TIME' then begin
        dObj->WriteDim, 'time', /UNLIMITED
        dObjSig->WriteDim, 'time', /UNLIMITED        
        continue
      endif
      sObj->CopyDimTo, dimNames[j], dObj
      sObj->CopyDimTo, dimNames[j], dObjSig      
    ENDFOR
    
    ; Find all the variable definitions, attributes and data in the
    ; source file and copy them.
    varNames = sObj->GetVarNames(COUNT=varCount)
    FOR j=0,varCount-1 DO BEGIN
      if str_equiv(varNames[j]) eq str_equiv(varId) then continue
      sObj->CopyVarDefTo, varNames[j], dObj
      sObj->CopyVarDefTo, varNames[j], dObjSig
      varAttrNames = sObj -> GetVarAttrNames(varNames[j], COUNT=varAttrCount)
      FOR k=0,varAttrCount-1 DO BEGIN
        sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObj
        sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObjSig        
      ENDFOR
      sObj->CopyVarDataTo, varNames[j], dObj
      sObj->CopyVarDataTo, varNames[j], dObjSig      
    ENDFOR
    
    vn = gvn
    des_str = 'Altitudinal gradient from var ' + varId
    des_str += '; KERNEL_SIZE='+str_equiv(KERNEL_SIZE)
    dObj->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT'
    dObj->WriteVarAttr, vn, 'long_name', des_str
    dObj->WriteVarAttr, vn, 'units', info.unit + '.m-1'
    dObj->WriteVarAttr, vn, 'agg_method', 'MEAN'
    
    vn = grn
    des_str = 'Altitudinal gradient significance from var ' + varId
    des_str += '; KERNEL_SIZE='+str_equiv(KERNEL_SIZE)
    dObjSig->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT'
    dObjSig->WriteVarAttr, vn, 'long_name', des_str
    dObjSig->WriteVarAttr, vn, 'units', '-'
    dObjSig->WriteVarAttr, vn, 'agg_method', 'MEAN'
    
    ; Compute
    data = reform(obj->get_Var(Nvars-1, t))
    grad = w_altitudinal_gradient(data, height, $
      KERNEL_SIZE=kernel_size, $
      DEFAULT_VAL=0., $
      SIG=sig)
       
    ; Fill with data
    dObj->SetMode, /DATA
    dObj->WriteVarData, gvn, grad
    dObjSig->SetMode, /DATA
    dObjSig->WriteVarData, grn, sig
    
    undefine, dObj, sObj, dObjSig
    
  endfor
  
  undefine, wpr
  
end