;+
; :Description:
;    Converts a file between NetCDF4 and NetCDF3 file formats (IDL8+ only!). 
;    Converting a file from NetCDF3 to NetCDF4 is advantageous if you want 
;    to use the compression features offered by NetCDF4. Converting
;    a file from NetCDF4 to NetCDF3 maybe useful if you want to distribute 
;    your files to users who cannot read NetCDF4. 
;    
;    The default behavior of the procedure is to check the format 
;    of the input file and set the keywords accordingly::
;      - if the file format is NetCDF3 then /TONETCDF4 is set automatically
;      - if the file format is NetCDF4 then /TONETCDF3 is set automatically
;    You will have to set TONETCDF3=0 or TONETCDF4=0 to prevent this (probably never)
;
; :Params:
;    sourceFile: in, required
;                path to the original file
;    destFile: in, required
;              path to the file to create
;
; :Keywords:
;    CLOBBER: in, optional, default=0
;             set this keyword to force overwriting of the output file
;    TONETCDF3:  in, optional
;                convert to netcdf3. Default behiavor is based on the 
;                input file format, see the doc
;    TONETCDF4:  in, optional
;                convert to netcdf4. Default behiavor is based on the 
;                input file format, see the doc
;    CHUNK_DIMENSIONS: in, optional, default=auto
;                For TONETCDF4 cases only.
;                Set this keyword equal to a vector containing the chunk dimensions for the variable.
;                A new NetCDF variable is chunked by default, using a default chunk value that is 
;                the full dimension size for limited dimensions, and 1 for unlimited dimensions.
;                CHUNK_DIMENSIONS must have the same number of elements as the number of dimensions 
;                specified by Dim. If the CONTIGUOUS keyword is set, the value of the 
;                CHUNK_DIMENSIONS keyword is ignored. Available only in IDL 8.0 and higher.
;    GZIP:       in, optional, default=5
;                For TO_NETCDF4 cases only.
;                Set this keyword to an integer between zero and nine to specify the level 
;                of GZIP compression applied to the variable. Lower compression values result 
;                in faster but less efficient compression. This keyword is ignored if the 
;                CHUNK_DIMENSIONS keyword is not set. This keyword is ignored if the CONTIGUOUS 
;                keyword is set. If the GZIP keyword is set, the CONTIGUOUS keyword is ignored.
;                You can only use GZIP compression with NCDF 4 files. Available only in 
;                IDL 8.0 and higher.    
;    SHUFFLE:    in, optional, default=1
;                For TO_NETCDF4 cases only.
;                Set this keyword to apply the shuffle filter to the variable. If the GZIP 
;                keyword is not set, this keyword is ignored. The shuffle filter de-interlaces blocks 
;                of data by reordering individual bytes. Byte shuffling can sometimes 
;                increase compression density because bytes in the same block positions 
;                often have similar values, and grouping similar values together often 
;                leads to more efficient compression. Available only in IDL 8.0 and higher.      
;                
;-
pro w_convert_netcdf, sourceFile, destFile, $
  CLOBBER=clobber, $
  TONETCDF3=tonetcdf3, $
  TONETCDF4=tonetcdf4, $
  CHUNK_DIMENSIONS=chunk_dimensions, $
  GZIP=gzip, $
  SHUFFLE=shuffle

  ; Set Up environnement
  COMPILE_OPT idl2
  ON_ERROR, 2
  
  ; Check input and decide which conversion to do
  format = w_ncdf_format(sourceFile)
  if format eq 'UNKNOWN' then Message, 'File format unknown'
  
  if format eq 'FORMAT_CLASSIC' or format eq 'FORMAT_64BIT' then SetDefaultValue, tonetcdf4, 1
  if format eq 'FORMAT_NETCDF4' then SetDefaultValue, tonetcdf3, 1
  
  _tonetcdf3 = KEYWORD_SET(tonetcdf3)
  _tonetcdf4 = KEYWORD_SET(tonetcdf4)
  
  if ~ (_tonetcdf3 xor _tonetcdf4) then Message, 'Conflicting keyword combination with file format: ' + format
  
  ; Set the default value for NetCDF4 keywords. 
  ; They are ignored if netcdf3 is asked
  SetDefaultValue, gzip, 5
  SetDefaultValue, shuffle, 1
  
  ; Open the source file in read-only mode.
  sObj = Obj_New('NCDF_FILE', sourceFile, $
    ErrorLoggerName='w_convert_netcdf_sourcefilelogger', /TIMESTAMP)
  IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
  
  ; Open the destination file for writing.
  dObj = Obj_New('NCDF_FILE', destFile, /CREATE, CLOBBER=clobber, $
      ErrorLoggerName='w_convert_netcdf_destinationfilelogger', /TIMESTAMP, NETCDF4_FORMAT=_tonetcdf4)

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
    if _tonetcdf3 then sObj->CopyVarDefTo, varNames[j], dObj
    if _tonetcdf4 then sObj->CopyVarDefTo, varNames[j], dObj, $
      CHUNK_DIMENSIONS=chunk_dimensions, $
      GZIP=gzip, $
      SHUFFLE=shuffle
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