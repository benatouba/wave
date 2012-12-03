pro w_aggTrmm, directory, outFile, CLOBBER=clobber, COMPRESS=compress, _EXTRA=extra

   ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  ; Check arguments
  if ~ arg_okay(directory, TYPE=IDL_STRING) then message, 'Argument not ok'
  if ~ FILE_TEST(directory, /DIRECTORY) then message, directory + ' is not a directory'
    
  ; Check ncdf files
  fileList = FILE_SEARCH(directory, '*.7.nc', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT, count=cfiles)
  if cfiles eq 0 then Message, 'No TRMM V7 files found in the directory.'
  
  fnames = FILE_BASENAME(fileList)
  so = SORT(fnames)
  if N_ELEMENTS(fnames[UNIQ(fnames, so)]) ne cfiles then Message, 'TRMM files are not unique.'
  fileList = fileList[so]
  
  ; Read the Netcdf template file
  template = OBJ_NEW('w_TRMM', filelist[0], _EXTRA=extra)
  template->getProperty, TYPE=type
  template->get_Lonlat, lon, lat, nx, ny
  obj_destroy, template
  
  if KEYWORD_SET(COMPRESS) then begin
    chunk_dimensions = [nx, ny, cfiles]
    gzip = 5
    shuffle = 1
    netcdf4_format = 1
  endif
  
  ; Create the Netcdf out file
  dObj = Obj_New('NCDF_FILE', outFile, NETCDF4_FORMAT=netcdf4_format, /TIMESTAMP, CLOBBER=clobber, /CREATE)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
  
  ;Define dimensions
  tdimName = 'time'
  xdimName = 'longitude'
  ydimName = 'latitude'
  
  dObj->WriteDim, tdimName, /UNLIMITED
  dObj->WriteDim, xdimName, nx
  dObj->WriteDim, ydimName, ny
  
  ; Define global attributes
  dObj->WriteGlobalAttr, 'creation_date', TIME_to_STR(QMS_TIME()), DATATYPE='STRING' 
  dObj->WriteGlobalAttr, 'conventions', 'COARDS', DATATYPE='STRING' 
  dObj->WriteGlobalAttr, 'type', type, DATATYPE='STRING' 
      
  ; Define variables
  vn = 'time'
  dObj->WriteVarDef, vn, tdimName, DATATYPE='LONG'
  dObj->WriteVarAttr, vn, 'long_name', 'Time'
  dObj->WriteVarAttr, vn, 'units', 'hours since 1998-01-01 00:00:00' 
  t0 = QMS_TIME(YEAR=1998, MONTH=1, DAY=1)
  vn = 'longitude'
  dObj->WriteVarDef, vn, xdimName, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Longitude'
  dObj->WriteVarAttr, vn, 'units', 'degree'
  vn = 'latitude'
  dObj->WriteVarDef, vn, ydimName, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Latitude'
  dObj->WriteVarAttr, vn, 'units', 'degree'
  vn = 'pcp'
  dObj->WriteVarDef, vn, [xdimName, ydimName, tdimName], DATATYPE='FLOAT', $
                        CHUNK_DIMENSIONS=chunk_dimensions, GZIP=gzip, SHUFFLE=shuffle
  dObj->WriteVarAttr, vn, 'long_name', 'Precipitation'
  dObj->WriteVarAttr, vn, 'units', 'mm.h-1'
  
  dObj->WriteVarData, 'longitude', REFORM(lon[*,0])
  dObj->WriteVarData, 'latitude', REFORM(lat[0,*])
  
  ;Let's go
  for i=0, cfiles-1 do begin
    tpl = OBJ_NEW('w_TRMM', filelist[i], _EXTRA=extra)
    tpl->getTime, time
    time = long((time - t0) / H_QMS)
    p = tpl->getVarData()
    if type eq '3B42d' then p = p / 24. 
    dObj->WriteVarData, 'time', time, OFFSET=i 
    dObj->WriteVarData, 'pcp', p, OFFSET=[0, 0, i]
    undefine, tpl
  endfor  
  
  undefine, dObj

end