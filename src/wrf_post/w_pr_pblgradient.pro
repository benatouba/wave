; docformat = 'rst'
;+
;  
; Computes the pbl gradient for a product variable and stores it
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
;    Computes the pbl gradient for a product variable and stores it
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
;    MIN_PBL: in, optional, default=2
;             the gradient is computed below the pbl height. If the height is 
;             too low, at least MIN_PBL levels are used for the computation
;
;-
pro w_pr_PBLGradient, $
    DIRECTORY=directory, $
    FORCE=force, $
    MIN_PBL=min_pbl
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  wpr = OBJ_NEW('w_WPR', DIRECTORY=directory, /IGNORE_ALTERNATE)  
  if ~ OBJ_VALID(wpr) then return  
  
  varid = 't2'
     
  ;Check if the variable is available
  if ~ wpr->hasVar('tk_eta', INFO=info) then Message, 'Did not find variable: ' + 'TK'
  if ~ wpr->hasVar('pblh', INFO=info) then Message, 'Did not find variable: ' + 'PBLH'
  if ~ wpr->hasVar('zag_eta', INFO=info) then Message, 'Did not find variable: ' + 'ZAG'
    
  if N_ELEMENTS(MIN_PBL) eq 0 then min_pbl = 2
  if min_pbl lt 2 then Message, '$MIN_PBL not valid'
  
  gvn = 'gradpbl_mp' + str_equiv(min_pbl)
  grn = 'gradpblsig_mp' + str_equiv(min_pbl)
    
  _y = wpr->getProperty('YEARS')
  for y=0, N_ELEMENTS(_y)-1 do begin
  
    obj = wpr->getVarObj(varid, (_y)[y], INFO=info)
    ok = obj->define_subset()
    obj->getProperty, Nvars=Nvars, PATH=fPath    
    aldir = utils_clean_path(FILE_DIRNAME(FILE_DIRNAME(fPath))) + '/2d_alternate/'
    if ~ FILE_TEST(aldir, /DIRECTORY) then FILE_MKDIR, aldir
    oPath = aldir + utils_replace_string(FILE_BASENAME(fPath), varid, gvn)
    orPath = aldir + utils_replace_string(FILE_BASENAME(fPath), varid, grn)
  
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
    des_str = 'PBL gradient from TK' 
    des_str += '; MIN_PBL='+str_equiv(min_pbl)
    dObj->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT', GZIP=5, SHUFFLE=1
    dObj->WriteVarAttr, vn, 'long_name', des_str
    dObj->WriteVarAttr, vn, 'units', info.unit + '.m-1'
    dObj->WriteVarAttr, vn, 'agg_method', 'MEAN'
    
    vn = grn
    des_str = 'PBL gradient significance from TK'
    des_str += '; MIN_PBL='+str_equiv(min_pbl)
    dObjSig->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT', GZIP=5, SHUFFLE=1
    dObjSig->WriteVarAttr, vn, 'long_name', des_str
    dObjSig->WriteVarAttr, vn, 'units', '-'
    dObjSig->WriteVarAttr, vn, 'agg_method', 'MEAN'
    
    ; Compute
    tk = wpr->getVarData('tk_eta', t, nt, YEARS=(_y)[y])
    z = wpr->getVarData('zag_eta', YEARS=(_y)[y])
    pblh = wpr->getVarData('pblh', YEARS=(_y)[y])
    dims = SIZE(tk, /DIMENSIONS)
    grad = FLTARR(dims[0], dims[1], nt)
    sig = FLTARR(dims[0], dims[1], nt)
   
    for t=0, nt-1 do begin
      tmptk = REFORM(tk[*,*,*,t], dims[0]*dims[1], dims[2])
      tmpz = REFORM(z[*,*,*,t], dims[0]*dims[1], dims[2])
      tmppblh = REFORM(pblh[*,*,t], dims[0]*dims[1])
      tmpgrad = FLTARR(dims[0], dims[1])
      tmpcorr = FLTARR(dims[0], dims[1])
      for e=0, dims[0]*dims[1]-1 do begin
        ph = where(tmpz[e,*] le (tmppblh[e])[0], cnth)
        if cnth le 1 then ph = indgen(min_pbl)
        tmptmptk = (tmptk[e,ph])[*]
        if max(ABS(tmptmptk-mean(tmptmptk))) lt 0.000001 then begin
          a = 0.
          lr_corr = 1.
        endif else begin
          a = REGRESS((tmpz[e,ph])[*], tmptmptk, CORRELATION=lr_corr)
        endelse
        tmpgrad[e] = a
        tmpcorr[e] = lr_corr
      endfor
      grad[*,*,t] = tmpgrad
      sig[*,*,t] = tmpcorr
    endfor  
       
    ; Fill with data
    dObj->SetMode, /DATA
    dObj->WriteVarData, gvn, grad
    dObjSig->SetMode, /DATA
    dObjSig->WriteVarData, grn, sig
    
    undefine, dObj, sObj, dObjSig
    
  endfor
  
  undefine, wpr
  
end