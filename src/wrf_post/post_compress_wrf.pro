pro post_compress_wrf, input_dir, CACHE=cache

  
  fileList = FILE_SEARCH(input_dir, 'wrfpost_d*_25h.nc', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT, COUNT=nfiles)
  
  if N_ELEMENTS(CACHE) eq 0 then CD, CURRENT=_d else _d = cache
  
  if ~ FILE_TEST(_d, /DIRECTORY) then Message, WAVE_Std_Message('CACHE', /FILE)
  
  _d = utils_clean_path(_d, /MARK_DIRECTORY)
  
  OPENW, unit, _d +'comp_'+ utils_replace_string(input_dir, '/', '_') +'.log', /GET_LUN
  
  text = 'WRF output compression ' + TIME_to_STR(QMS_TIME()) 
  printf, unit, text 
  text = 'Directory: ' + input_dir
  printf, unit, text 
  text = 'Nfiles: ' + str_equiv(nfiles)
  printf, unit, text 
  
  t0 = SYSTIME(/SECONDS)
    
  for i=0, nfiles-1 do begin
  
    file = fileList[i]
    odir = FILE_DIRNAME(file, /MARK_DIRECTORY)
    bname = FILE_BASENAME(file)
    bzipname = utils_replace_string(bname, '.nc', '.zip')
    
    lfile =  _d + bname
    cfile = _d + bzipname
    
    cd,  _d    
    FILE_COPY, file, lfile    
    spawn, 'zip ' + bzipname + ' ' + bname, ret, err, EXIT_STATUS=status
    if err ne '' then Message, 'error on compression message'
    if status ne 0 then Message, 'error on compression status'
    
    ;Check    
    FILE_MKDIR, _d + 'tmp'
    spawn, 'unzip ' + bzipname + ' -d ' +  _d + 'tmp/', ret, err, EXIT_STATUS=status
    if err ne '' then Message, 'error on uncomp message'
    if status ne 0 then Message, 'error on uncompstatus'
    infc = FILE_INFO(file)  
    info = FILE_INFO(_d + 'tmp/' + bname)
    if infc.size ne info.size then Message, 'error on file test'
    
    ofile = odir + bzipname   
    FILE_COPY, cfile, ofile
    infc = FILE_INFO(cfile)  
    info = FILE_INFO(ofile)
    if infc.size ne info.size then Message, 'error on file copy'
    
    FILE_DELETE, lfile, cfile, _d + 'tmp', /RECURSIVE    
    FILE_DELETE, file

    printf, unit, 'Done: ' + file 
          
  endfor
  
  printf, unit, ''
  printf, unit, 'Success! Needed ' + str_equiv((SYSTIME(/SECONDS)-t0)/60./60.) + ' hours to complete'
  close, unit ; close log file  
  free_lun, Unit
  
end