;+
; :Description:
;    Aggregates all TRMM V7 files in a directory into a single output file,
;    possibly for a subset of the region
;
; :Params:
;    directory: in, required
;               the path to the directory containing all TRMM v7 files
;    outFile: in, required
;             the path to the output file to write
;
; :Keywords:
;    CLOBBER: in, optional
;             set this keyword to force overwriting if the output file 
;             already exists
;    COMPRESS: in, optional
;              set this keyword to make a compressed NetCDF file
;    ADD_ERROR: in, optional
;               set this keyword to add the TRMM v7 error variable 
;               in addition to precipitation
;    YEAR: in, optional
;          default is to keep all files in the directory. Set this keyword
;          to an array of years to *keep* for aggregation.
;
;    MONTH: in, optional
;          default is to keep all files in the directory. Set this keyword
;          to an array of months to *keep* for aggregation.
;          
;    _EXTRA: in, optional
;            any keyword accepted by w_TRMM::defineSubset()
;
;-
pro w_aggTrmm, directory, outFile, CLOBBER=clobber, COMPRESS=compress, ADD_ERROR=add_error, YEAR=year, MONTH=month, _EXTRA=extra

   ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  ; Check arguments
  if ~ arg_okay(directory, TYPE=IDL_STRING) then message, 'Argument not ok'
  if ~ FILE_TEST(directory, /DIRECTORY) then message, directory + ' is not a directory'
  add_error = KEYWORD_SET(ADD_ERROR)
    
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
    chunk_dimensions = [nx, ny, 1]
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
 
  if add_error then begin
    vn = 'err'
    dObj->WriteVarDef, vn, [xdimName, ydimName, tdimName], DATATYPE='FLOAT', $
      CHUNK_DIMENSIONS=chunk_dimensions, GZIP=gzip, SHUFFLE=shuffle
    dObj->WriteVarAttr, vn, 'long_name', 'Random error estimate'
    dObj->WriteVarAttr, vn, 'units', 'mm.h-1'
  endif
  
  dObj->WriteVarData, 'longitude', REFORM(lon[*,0])
  dObj->WriteVarData, 'latitude', REFORM(lat[0,*])
  
  do_month = N_ELEMENTS(MONTH) ne 0
  do_year = N_ELEMENTS(YEAR) ne 0
  
  ;Let's go
  off = 0L
  for i=0, cfiles-1 do begin
    tpl = OBJ_NEW('w_TRMM', filelist[i], _EXTRA=extra)
    tpl->getTime, time
    if do_month or do_year then begin
      tt = MAKE_ABS_DATE(QMS=time)
      if do_month then begin
        pm = where(month eq tt.month, cntm)
        if cntm eq 0 then continue
      endif
      if do_year then begin
        pm = where(year eq tt.year, cntm)
        if cntm eq 0 then continue
      endif
    endif
    time = long((time - t0) / H_QMS)
    p = tpl->getVarData()
    if type eq '3B42d' then p = p / 24.
    dObj->WriteVarData, 'time', time, OFFSET=off
    dObj->WriteVarData, 'pcp', p, OFFSET=[0, 0, off]
    if add_error then begin
       p = tpl->getVarData('err')
       dObj->WriteVarData, 'err', p, OFFSET=[0, 0, off]
    endif
    off += 1
    undefine, tpl
  endfor  
  
  undefine, dObj

end