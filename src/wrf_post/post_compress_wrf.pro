pro post_compress_wrf, input_dir, CACHE=cache

  
  fileList = FILE_SEARCH(input_dir, 'wrfpost_d*_25h.nc', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT, COUNT=nfiles)
  if N_ELEMENTS(CACHE) eq 0 then CD, CURRENT=_d else _d = cache
  
  if ~ FILE_TEST(_d, /DIRECTORY) then Message, WAVE_Std_Message('CACHE', /FILE)
  
  _d = utils_clean_path(_d, /MARK_DIRECTORY)
  
  for i=0, nfiles-1 do begin
    file = fileList[i]
    odir = FILE_DIRNAME(file, /MARK_DIRECTORY)
    bname = FILE_BASENAME(file)
    
    lfile =  _d + bname
    cfile = _d + utils_replace_string(bname, '.nc', '.zip')
    
    FILE_COPY, file, lfile    
    spawn, 'zip ' + cfile + ' ' + lfile, ret, err, EXIT_STATUS=status
    if err ne '' then Message, 'error on compression message'
    if status ne 0 then Message, 'error on compression status'
    
    ffile = odir + utils_replace_string(bname, '.nc', '.zip')    
    FILE_COPY, cfile, ffile 
      
  endfor
  
end