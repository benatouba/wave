;+
; :Description:
;    Generates new min and max products in the 2d_alternate directory.
;    With a hourly directory, min and max are generated in the daily directory, 
;    with the daily in the monthly, etc.
;
; :Params:
;    varid: in, required
;           the 2d variable to compute the max from
;
; :Keywords:
;    DIRECTORY: in, required
;               the product directory
;    FORCE: in, optional
;           force overwrite of the file if it exists already
;
; :Author: FM
; 
;-
pro w_pr_MinMax, varid, $
    DIRECTORY=directory, $
    FORCE=force
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  wpr = OBJ_NEW('w_WPR', DIRECTORY=directory)  
  if ~ OBJ_VALID(wpr) then return  
  
  wpr->GetProperty, tres=tres

  case tres of
    'h': my_tres = 'd'
    'd': my_tres = 'm'
    'm': my_tres = 'y'
    else: Message, 'tres not OK'
  endcase

      
  ;Check if the variable is available
  if ~ wpr->hasVar(varid, INFO=info) then Message, 'Did not find variable: ' + str_equiv(varid)
  if info.type ne '2d' then Message, 'works only on 2d variables'
      
  minvn = STRLOWCASE(varid) + '_min'
  maxvn = STRLOWCASE(varid) + '_max'
    
  _y = wpr->getProperty('YEARS')
  for y=0, N_ELEMENTS(_y)-1 do begin
  
    t0 = QMS_TIME(year=(_y)[y],month=1,day=1)
    t1 = QMS_TIME(year=(_y)[y]+1,month=1,day=1)
  
    obj = wpr->getVarObj(varid, (_y)[y], INFO=info)
    ok = obj->define_subset()
    obj->getProperty, Nvars=Nvars, PATH=fPath
    ominPath = utils_replace_string(utils_replace_string(utils_replace_string(utils_replace_string(fPath, varid, minvn), '_' + tres + '_', '_' + my_tres + '_'), '/' + tres + '/', '/' + my_tres + '/'), '/2d/', '/2d_alternate/')
    omaxPath = utils_replace_string(utils_replace_string(utils_replace_string(utils_replace_string(fPath, varid, maxvn), '_' + tres + '_', '_' + my_tres + '_'), '/' + tres + '/', '/' + my_tres + '/'), '/2d/', '/2d_alternate/')
    
    if ~ FILE_TEST(FILE_DIRNAME(ominPath), /DIRECTORY) then FILE_MKDIR, FILE_DIRNAME(ominPath)
    
    ; Check
    if FILE_TEST(ominPath) and ~KEYWORD_SET(FORCE) then Message, 'Output file already here. Set /FORCE if you want to overwrite it.'
    if FILE_TEST(omaxPath) and ~KEYWORD_SET(FORCE) then Message, 'Output file already here. Set /FORCE if you want to overwrite it.'
    
    ; Make File
    ; Open the source file in read-only mode.
    sObj = Obj_New('NCDF_FILE', fPath, $
      ErrorLoggerName='sourcefilelogger', /TIMESTAMP)
    IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
    
    ; Open the destination files for writing.
    dObjmin = Obj_New('NCDF_FILE', ominPath, /CREATE, CLOBBER=force, /NETCDF4_FORMAT, $
      ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObjmin) EQ 0 THEN Message, 'Destination object cannot be created.'
    dObjmax = Obj_New('NCDF_FILE', omaxPath, /CREATE, CLOBBER=force, /NETCDF4_FORMAT, $
      ErrorLoggerName='destinationfilelogger', /TIMESTAMP)
    IF Obj_Valid(dObjmax) EQ 0 THEN Message, 'Destination object cannot be created.'
         
    ; Find all the global attributes in the source file and copy them.
    attrNames = sObj->GetGlobalAttrNames(COUNT=attrCount)
    FOR j=0,attrCount-1 DO BEGIN
      if str_equiv(attrNames[j]) eq 'CREATION_DATE' then begin
        dObjmin->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
        dObjmax->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
        continue
      endif
      if str_equiv(attrNames[j]) eq 'VARNAME' then begin
        dObjmin->WriteGlobalAttr, 'VARNAME', minvn, DATATYPE='CHAR'
        dObjmax->WriteGlobalAttr, 'VARNAME', maxvn, DATATYPE='CHAR'
        continue
      endif
      if str_equiv(attrNames[j]) eq 'PRODUCT_LEVEL' then begin
        dObjmin->WriteGlobalAttr, 'PRODUCT_LEVEL', my_tres, DATATYPE='CHAR'
        dObjmax->WriteGlobalAttr, 'PRODUCT_LEVEL', my_tres, DATATYPE='CHAR'
        continue
      endif
      sObj->CopyGlobalAttrTo, attrNames[j], dObjmin
      sObj->CopyGlobalAttrTo, attrNames[j], dObjmax      
    ENDFOR
    
    ; Find all the dimensions in the source file and copy them.
    dimNames = sObj->GetDimNames(COUNT=dimCount)
    FOR j=0,dimCount-1 DO BEGIN
      if str_equiv(dimNames[j]) eq 'TIME' then begin
        dObjmin->WriteDim, 'time', /UNLIMITED
        dObjmax->WriteDim, 'time', /UNLIMITED        
        continue
      endif
      sObj->CopyDimTo, dimNames[j], dObjmin
      sObj->CopyDimTo, dimNames[j], dObjmax      
    ENDFOR
    
    ; Find all the variable definitions, attributes and data in the
    ; source file and copy them.
    varNames = sObj->GetVarNames(COUNT=varCount)
    FOR j=0,varCount-1 DO BEGIN
      if str_equiv(varNames[j]) eq str_equiv(varId) then continue
      if str_equiv(varNames[j]) eq 'TIME' then begin
        time_str = TIME_to_STR(t0, MASK=' since YYYY-MM-DD HH:TT:SS')
        case my_tres of
          'd': time_str = 'days'   + time_str
          'm': time_str = 'months' + time_str
          'y': time_str = 'years'  + time_str
        endcase
        sObj->CopyVarDefTo, varNames[j], dObjmin
        dObjmin->WriteVarAttr, varNames[j], 'long_name', 'Time'
        dObjmin->WriteVarAttr, varNames[j], 'units', time_str
        sObj->CopyVarDefTo, varNames[j], dObjmax
        dObjmax->WriteVarAttr, varNames[j], 'long_name', 'Time'
        dObjmax->WriteVarAttr, varNames[j], 'units', time_str
        continue
      end
      sObj->CopyVarDefTo, varNames[j], dObjmin
      sObj->CopyVarDefTo, varNames[j], dObjmax
      varAttrNames = sObj -> GetVarAttrNames(varNames[j], COUNT=varAttrCount)
      FOR k=0,varAttrCount-1 DO BEGIN
        sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObjmin
        sObj->CopyVarAttrTo, varNames[j], varAttrNames[k], dObjmax
      ENDFOR
      sObj->CopyVarDataTo, varNames[j], dObjmin
      sObj->CopyVarDataTo, varNames[j], dObjmax
    ENDFOR
    
    vn = minvn
    des_str = 'Minimal value from var ' + varId
    dObjmin->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT'
    dObjmin->WriteVarAttr, vn, 'long_name', des_str
    dObjmin->WriteVarAttr, vn, 'units', info.unit
    dObjmin->WriteVarAttr, vn, 'agg_method', 'MIN'
    
    vn = maxvn
    des_str = 'Maximal value from var ' + varId
    dObjmax->WriteVarDef, vn, ['west_east','south_north','time'], DATATYPE='FLOAT'
    dObjmax->WriteVarAttr, vn, 'long_name', des_str
    dObjmax->WriteVarAttr, vn, 'units', info.unit
    dObjmax->WriteVarAttr, vn, 'agg_method', 'MAX'
        
    ; Compute
    data = reform(obj->get_Var(Nvars-1, t))
    case my_tres of
      'd': new_time = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=D_QMS)
      'm': new_time = MAKE_ENDED_TIME_SERIE(t0, t1, MONTH=1)
      'y': new_time = [t0, t1]
    endcase

    
    TS_AGG_GRID, data, t, data_agg, agg_time, AGG_METHOD='MIN', NEW_TIME=new_time    
    agg_time = new_time[0:N_ELEMENTS(new_time)-2]
    
    case my_tres of
      'd': agg_time = LONG((agg_time-t0)/D_QMS)
      'm': agg_time = INDGEN(12)
      'y': agg_time = 0
    endcase
    
    ; Fill with data
    dObjmin->SetMode, /DATA
    dObjmin->WriteVarData, 'time', agg_time
    dObjmin->WriteVarData, minvn, data_agg
    
    ; Compute
    TS_AGG_GRID, data, t, data_agg, dummy, AGG_METHOD='MAX', NEW_TIME=new_time    
    ; Fill with data
    dObjmax->SetMode, /DATA    
    dObjmax->WriteVarData, 'time', agg_time
    dObjmax->WriteVarData, maxvn, data_agg
    
    undefine, dObj, dObjmin, dObjmax
    
  endfor
  
  undefine, wpr
  
end