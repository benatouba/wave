pro w_convert_netcdf4, sourceFile, destFile, CLOBBER=clobber

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2

  ; Open the source file in read-only mode.
  sObj = Obj_New('NCDF_FILE', sourceFile, $
    ErrorLoggerName='sourcefilelogger', /TIMESTAMP)
  IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
  
  ; Open the destination file for writing.
  dObj = Obj_New('NCDF_FILE', destFile, /CREATE, CLOBBER=clobber, $
    ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
  
  ; Find all the global attributes in the source file and copy them.
  attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
  FOR j=0,attrCount-1 DO BEGIN
    sObj->CopyGlobalAttrTo, attrNames[j], dObj
  ENDFOR
  
  ; Find all the dimensions in the source file and copy them.
  dimNames = sObj->GetDimNames(COUNT=dimCount)
  FOR j=0,dimCount-1 DO BEGIN
    sObj->CopyDimTo, dimNames[j], dObj
  ENDFOR
  
  ; Find all the variable definitions, attributes and data in the
  ; source file and copy them.
  varNames = sObj->GetVarNames(COUNT=varCount)
  FOR j=0,varCount-1 DO BEGIN
    sObj->CopyVarDefTo, varNames[j], dObj
    varAttrNames = sObj->GetVarAttrNames(varNames[j], COUNT=varAttrCount)
    FOR k=0,varAttrCount-1 DO BEGIN
      sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObj
    ENDFOR
    sObj->CopyVarDataTo, varNames[j], dObj
  ENDFOR
  
  ; Sync the destination file.
  dObj->Sync
  
  ; Destroy both the source and destination objects.
  Obj_Destroy, dObj
  Obj_Destroy, sObj
  
end