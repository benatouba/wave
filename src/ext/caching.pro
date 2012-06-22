;+
; :Description:
;   This function creates a copy of a NCDF or ZIP file
;   on a local storage and unzips the file if needed. 
;   This routine HAVE to be called if the original file is zipped or 
;   if the original file is located on a network storage.
;       
; :Author:
;   FaM
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification: 12 Jan 2012
;     Inspired from:: 
;       caching.pro, 2011 R.Bauer
;       Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
;       
;-

;+
; :Description:
;    This function will combine path and file, change '\' to '/' and add '/' to the path if necessary
;
; :Params:
;    path_in: in, required, type=string
;             directory path
;    file_in: in, required, type=string
;             file name
;             
; :Returns:
;    correct combination of path and file
;
;-
function caching_combine_path_file, path_in, file_in
   
   path=path_in[0]
   file=file_in[0]
   flag=''
   if strpos(path,'\\') eq 0 then begin
     path=strmid(path,2)
     flag='\\'
   endif
   if strlen(path) gt 0 then  new=path+'/'+file else new=file
   new=utils_replace_string(new,'\','/')
   new=utils_replace_string(new,'//','/')
   new=flag+new
   
   return,new
   
end

;+
; :Description:
;    Logs a message
;    
; :Params:
;    text: in, required, type=string
;          the message
;
; :Keywords:
;    LOGGER: in, optional, type={ErrorLogger}
;            the logger to log into
;    PRINT: in, optional, type=BOOLEAN
;           if the logging must occur on the console, too
;    QUIET: in, optional, type=BOOLEAN
;           shut up
;             
;-
pro caching_log, text, LOGGER=logger, PRINT=print, QUIET=quiet

  ON_ERROR, 2

  if KEYWORD_SET(QUIET) then return
  
  if N_ELEMENTS(logger) ne 0 then begin
    logger->addText, '% CACHING: ' + text, PRINT=print
  endif else message, text, /INFO
  
end


;+
; :Description:
;    This function creates a copy of a NCDF or ZIP file
;   on a local storage and unzips the file if needed. 
;   This routine HAVE to be called if the original file is zipped or 
;   if the original file is located on a network storage.
;
; :Params:
;    filename: in, required, type=STRING
;              the path to the file to cache
;
; :Keywords:
;    DELETE: in, optional, type=BOOLEAN
;            set this keyword to delete the cached file
;    CACHEPATH: in, required, type=STRING
;               path to the cache directory (if the directory doesnt exist, it will be created)
;    LOGGER: in, optional, type={ErrorLogger}
;            the logger to log into
;    PRINT: in, optional, type=BOOLEAN
;           if the logging must occur on the console, too
;    QUIET: in, optional, type=BOOLEAN, default=1
;            set this keyword to 0 to log the caching actions
;    NO_ZIP: in, optional, type=BOOLEAN
;            set this keyword to prevent unzipping the file (should not be set normally)
;    CHECK: in, optional, type=BOOLEAN
;           set this keyword to check if the file is already cached or beeing cached.
;           output is 1 if everything is clear and you can cache it, 0 if not (in this
;           case you may want to wait or do something else)
;           
; :Returns:
;   The path to the cached file
;
; :Examples:
;   Cache the file and delete it::
;    IDL> ofile ='/cfs/nas2/UAC/Standard_V0_d01/2010/2010.02.20/7a7cd512-3f40-11e1-9aff-00151728ccc9.415/wrfpost_d01_2010-02-20_00-00-00_25h.zip'
;    IDL> f = caching(ofile, CACHEPATH='/home/mowglie/cache')
;    IDL> print, f
;    /home/mowglie/cache/wrfpost_d01_2010-02-20_00-00-00_25h.nc
;    IDL> f = caching(ofile, CACHEPATH='/home/mowglie/cache', /DELETE)
;    
;-
function caching, filename , $
    DELETE=delete, $
    CACHEPATH=cachepath, $
    LOGGER=logger, $
    PRINT=print, $
    QUIET=quiet, $
    NO_ZIP=no_zip, $
    CHECK=check
  
  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  if n_elements(filename) eq 0 then message, WAVE_Std_Message('filename', /ARG)
  if not file_test(filename) then message, 'filename: ' + filename + ' does not exist'
  if n_elements(cachepath) eq 0 then message, 'set a cachepath'  
  if ~ FILE_TEST(cachepath) then FILE_MKDIR, cachepath
  if ~ FILE_TEST(cachepath, /DIRECTORY) then message, '$CACHEPATH must be a directory'  
  if n_elements(quiet) eq 0 then quiet = 1    
  
  ending = strsplit(filename, '.', /extract, count=cnt)
  if cnt eq 0 then Message, 'Type not recognized (must be .nc or .zip): ' + filename
  
  _filename = utils_replace_string(filename, '/', '_')
  cachefile = caching_combine_path_file(cachepath, _filename)
  lockfile = caching_combine_path_file(cachepath, _filename + '.lck')
  
  if KEYWORD_SET(CHECK) then begin
    if ~ file_test(lockfile) then return, 1 ; everything is ok
    return, 0 ; not ok
  endif
  
  if ~keyword_set(no_zip) and ~keyword_set(CHECK) then begin
    ending = ending[cnt-1]
    if str_equiv(ending) eq 'ZIP' then begin
      origname = caching(filename, /NO_ZIP, $
        DELETE=delete,  $
        CACHEPATH=cachepath,  $
        LOGGER=logger,  $
        PRINT=print,  $
        QUIET=quiet)
      lfile = utils_replace_string(origname, '.zip', '.sav')
      if keyword_set(delete) then begin
        if ~ FILE_TEST(lfile) then message, 'Something went wrong. Did you try to delete the file before caching it?'
        restore, filename=lfile
        file_delete, outname, lfile
        caching_log, 'uncompressed file: ' + outname + ' deleted', logger=logger, print=print, quiet=quiet
        return, origname
      endif else begin
        if FILE_TEST(lfile) then begin ; Already here
          restore, filename=lfile
          return, outname
        endif
        openw,lun,lockfile, /get_lun
        free_lun,lun
        spawn, 'unzip ' + origname + ' -d '+ cachepath, ret, err, exit_status=status
        if err[0] ne '' then message, 'Error on uncompress: ' + err
        outname = strcompress((strsplit(ret[1], ':', /extract))[1], /remove_all)
        if ~ file_test(outname) then message, 'Error on uncompress filename'
        save, outname, filename=lfile
        ; if it was deleted by someone else
        if file_test(lockfile) then file_delete, lockfile
        caching_log, 'file: ' + origname + ' uncompressed', logger=logger, print=print, quiet=quiet
        return, outname
      endelse
    endif
    if str_equiv(ending) eq 'GZ' then begin
      if ~ KEYWORD_SET(DELETE) then begin
        origname = caching(filename, /NO_ZIP, $
          DELETE=delete,  $
          CACHEPATH=cachepath,  $
          LOGGER=logger,  $
          PRINT=print,  $
          QUIET=quiet)
      endif else begin
        origname = caching_combine_path_file(cachepath, utils_replace_string(filename, '/', '_'))
      endelse
      lfile = utils_replace_string(origname, '.gz', '.sav')
      if keyword_set(delete) then begin
        if ~ FILE_TEST(lfile) then message, 'Something went wrong. Did you try to delete the file before caching it?'
        restore, filename=lfile
        file_delete, outname, lfile
        caching_log, 'uncompressed file: ' + outname + ' deleted', logger=logger, print=print, quiet=quiet
        return, origname
      endif else begin
        if FILE_TEST(lfile) then begin ; Already here
          restore, filename=lfile
          return, outname
        endif
        openw,lun,lockfile, /get_lun
        free_lun,lun
        spawn, 'gunzip ' + origname, ret, err, exit_status=status
        if err[0] ne '' then message, 'Error on uncompress: ' + err
        outname = utils_replace_string(origname, '.gz', '')
        if ~ file_test(outname) then message, 'Error on uncompress filename'
        save, outname, filename=lfile
        ; if it was deleted by someone else
        if file_test(lockfile) then file_delete, lockfile
        caching_log, 'file: ' + origname + ' uncompressed', logger=logger, print=print, quiet=quiet
        return, outname
      endelse
    endif
  endif
  
  ; Normal case  
  if keyword_set(delete) and file_test(cachefile) then begin
    file_delete, cachefile
    caching_log, 'cachefile: ' + cachefile + ' deleted', logger=logger, print=print, quiet=quiet
    if file_test(lockfile) then begin
      file_delete, lockfile
      caching_log, 'lockfile: ' + lockfile + ' deleted', logger=logger, print=print, quiet=quiet
    endif
    return, cachefile
  endif
  
  case 1 of
    ; order is important!!!
    file_test(cachefile) eq 1 and file_test(lockfile) eq 0: return, cachefile
    file_test(cachefile) eq 0 and file_test(lockfile) eq 1: begin
      ite = 0
      while file_test(lockfile) do begin
        caching_log, 'no cachefile: ' + cachefile + '  yet lockfile: '+ lockfile + ' still exists, we wait a second', logger=logger, print=print, quiet=quiet
        ite += 1
        if ite ge 720 then message, 'no cachefile: ' + cachefile + '  yet lockfile: '+ lockfile + ' still exists. big problem'
        wait,1        
      endwhile
      return, cachefile
    end
    file_test(cachefile) eq 0: begin
      openw,lun,lockfile, /get_lun
      free_lun,lun
      file_copy, filename, cachefile
      ; if it was deleted by someone else
      if file_test(lockfile) then file_delete, lockfile
      if not file_test(cachefile) then begin
        message, "someone has to early deleted your cache file: " + cachefile
      endif
      return, cachefile
    end
    file_test(cachefile) eq 1 and file_test(lockfile) ne 0: begin
      ite = 0
      while file_test(lockfile) do begin
        caching_log, 'cachefile: '+ cachefile +' exists but also lockfile: '+ lockfile + ' still exists, we wait a second', logger=logger, print=print, quiet=quiet
        ite += 1
        if ite ge 720 then message, 'no cachefile: ' + cachefile + '  exists but also lockfile: '+ lockfile + ' still exists. big problem'
        wait,1
      endwhile
      return, cachefile
    end
    else: begin
      message, "something is going really bad"
    end
  endcase
  
end