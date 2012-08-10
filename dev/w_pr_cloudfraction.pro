pro w_pr_CloudFraction, $ 
    DIRECTORY=directory, $
    FORCE=force, $
    RADIUS=radius
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  wpr = OBJ_NEW('w_WPR', DIRECTORY=directory)  
  if ~ OBJ_VALID(wpr) then return  
  
  if N_ELEMENTS(RADIUS) eq 0 then radius = 55000
    
  ;Check if the variable is available
  varid = 'cldfra_eta'
  if ~ wpr->hasVar(varid, INFO=info) then Message, 'Did not find variable: ' + str_equiv(varid)
  gvn = 'sfc_cldfra'
  _y = wpr->getProperty('YEARS')
  wpr->Get_XY, x, y, nx, ny
  for y=0, N_ELEMENTS(_y)-1 do begin
  
    obj = wpr->getVarObj(varid, (_y)[y], INFO=info)
    ok = obj->define_subset()
    obj->getProperty, Nvars=Nvars, PATH=fPath
    oPath = utils_replace_string(fPath, 'cldfra', gvn)
    oPath = utils_replace_string(oPath, '/3d_eta/', '/2d/')
    
    ; Check
    if FILE_TEST(oPath) and ~KEYWORD_SET(FORCE) then Message, 'Output file already here. Set /FORCE if you want to overwrite it.'
    
    ; Make File
    ; Open the source file in read-only mode.
    sObj = Obj_New('NCDF_FILE', fPath, $
      ErrorLoggerName='sourcefilelogger', /TIMESTAMP)
    IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
    
    ; Open the destination files for writing.
    dObj = Obj_New('NCDF_FILE', oPath, /CREATE, CLOBBER=force, /NETCDF4_FORMAT, $
      ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
    
    ; Find all the global attributes in the source file and copy them.
    attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
    FOR j=0,attrCount-1 DO BEGIN
      if str_equiv(attrNames[j]) eq 'CREATION_DATE' then begin
        dObj->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
        continue
      endif
      if str_equiv(attrNames[j]) eq 'VARNAME' then begin
        dObj->WriteGlobalAttr, 'VARNAME', gvn, DATATYPE='CHAR'
        continue
      endif
      sObj->CopyGlobalAttrTo, attrNames[j], dObj
    ENDFOR
    
    ; Find all the dimensions in the source file and copy them.
    dimNames = sObj->GetDimNames(COUNT=dimCount)
    FOR j=0,dimCount-1 DO BEGIN
      if str_equiv(dimNames[j]) eq 'TIME' then begin
        dObj->WriteDim, 'time', /UNLIMITED
        continue
      endif
      sObj->CopyDimTo, dimNames[j], dObj
    ENDFOR
    
    ; Find all the variable definitions, attributes and data in the
    ; source file and copy them.
    varNames = sObj->GetVarNames(COUNT=varCount)
    FOR j=0,varCount-1 DO BEGIN
      if str_equiv(varNames[j]) eq str_equiv(varId) then continue
      sObj->CopyVarDefTo, varNames[j], dObj
      varAttrNames = sObj -> GetVarAttrNames(varNames[j], COUNT=varAttrCount)
      FOR k=0,varAttrCount-1 DO BEGIN
        sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObj
      ENDFOR
      sObj->CopyVarDataTo, varNames[j], dObj
    ENDFOR
    
    vn = gvn
    des_str = 'Surface cloudfraction from model CLDFRA. Radius: ' + str_equiv(radius/1000) + ' km'
    dObj->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT'
    dObj->WriteVarAttr, vn, 'long_name', des_str
    dObj->WriteVarAttr, vn, 'units', '-'
    dObj->WriteVarAttr, vn, 'agg_method', 'MEAN'
    
    ; Compute
    cld = reform(obj->get_Var(Nvars-1, time, nt))
    cld = TOTAL(cld, 3) < 1.
    cld = reform(cld, [nx * ny, nt])
    data = cld * 0.
    
    
    npix = LONARR(nx, ny)
    
    phi = Findgen(360) * (!PI * 2 / 360.)
    phi = [phi, phi(0)]
    
    for i = 0L, N_ELEMENTS(lon)-1 do begin
      ; at my pooint
      x = xx[i]
      y = yy[i]
      _cld = cld
      pol = [transpose(cos(phi)*radius + x), transpose(sin(phi)*radius + y)]
      ok = tpl->set_ROI(POLYGON=pol, SRC=wrf_proj)
      tpl->get_ROI, MASK=mask
      _cld[where(mask eq 0.),*] = 0.
      data[i, *] = TOTAL(_cld,1) / TOTAL(mask)
      npix[i] = TOTAL(mask)
    endfor
    
    data = reform(data, [nx, ny, nt])
    
    ; Fill with data
    dObj->SetMode, /DATA
    dObj->WriteVarData, radius = 55000, grad
    
    undefine, dObj, sObj
    
  endfor
  
  undefine, wpr
  
end