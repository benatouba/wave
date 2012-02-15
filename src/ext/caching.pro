FUNCTION caching_file_exist,filename
;+
; NAME:
;          caching_file_exist
;
;
; PURPOSE:
;          return 1 if filename exists, 0 if not
;
;
; CATEGORY:
;          file handling
;
;
; CALLING SEQUENCE:
;          caching_file_exist(filename)
;
; 
; INPUTS:
;          filename: name of file 
;
;
; OPTIONAL INPUTS:
;          none
;
; 
; KEYWORD PARAMETERS:
;          none
;
;
; OUTPUTS:
;          1 if filename exists, 0 if not
;
;
; OPTIONAL OUTPUTS:
;          none
;
;
; COMMON BLOCKS:
;          none
;
;
; SIDE EFFECTS:
;          one LUN is temporarily used
;
;
; RESTRICTIONS:
;          none
;
;
; PROCEDURE:
;          an attempt to open the file with a read is undertaken, if
;          the attempt succeeds, the file exists.
;
;
; EXAMPLE:
;          if (caching_file_exist("/etc/services")) then begin
;             print,'/etc/services exists'
;          endif
;
;
; MODIFICATION HISTORY:
;          Version 1.0, 1997/10/06, Joern Wilms
;-
   openr,unit,filename,/get_lun,error=err
   IF (err EQ 0) THEN free_lun,unit
   return,(err EQ 0)
END 
; 
; Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-3
; All rights reserved.
; Unauthorized reproduction prohibited.
;
;+
; USERLEVEL:
;   TOPLEVEL
;
; NAME:
;   combine_path_file
;
; PURPOSE:
;   This function will combine path and file, change '\' to '/' and add '/' to the path if necessaray
;
; CATEGORY:
;   PROG_TOOLS/STRINGS
;
; CALLING SEQUENCE:
;   result=combine_path_file(path,file)
;
; INPUTS:
;   path,file : scalar strings
;
; OUTPUTS:
;    result: correct combination of path and file
;
; SIDE EFFECTS
;   will resolve multiple // into /
;
; EXAMPLE:
;   result=combine_path_file('d:\/temp','test.xxx')
;   will return result:'d:/temp/test.xxx'
;
;   result=combine_path_file('d:\/temp','')
;   will return result:'d:/temp/'
;
;   result=combine_path_file('','test.pro')
;   will return result: 'test.pro'
;
; MODIFICATION HISTORY:
;   Written by Franz Rohrer Aug 1998
;   Modified June 2001 : path and file are converted to scalars prior to handling
;   2002-04-13 : Syntax changed to replace_string() (saves 30 lines)
;   2002-04-24 : FR special handling of '\\' at the begin of the path reintroduced
;-
FUNCTION caching_combine_path_file,path_in,file_in,win=win
   if keyword_set(win) eq 0 then win=0
   path=path_in[0]
   file=file_in[0]
   flag=''
   if strpos(path,'\\') eq 0 then begin
     path=strmid(path,2)
     flag='\\'
   endif
   IF STRLEN(path) GT 0 THEN  new=path+'/'+file ELSE new=file
   new=utils_replace_string(new,'\','/')
   new=utils_replace_string(new,'//','/')
   if win then new=utils_replace_string(new,'/','\')
   new=flag+new
   RETURN,new
END

pro caching_log, text, LOGGER=logger, PRINT=print, QUIET=quiet
  
  if KEYWORD_SET(QUIET) then return
  if N_ELEMENTS(logger) ne 0 then logger->addText, '% CACHING: ' + text, PRINT=print else message, text, /INFO
  
end

;+
; NAME:
;   caching
;
; PURPOSE:
;   This routine creates a cache file of a file on a local storage.
;   If no env var CACHE is given it creates the cache dir in the /tmp folder
;
; CATEGORY:
;   CASE_TOOLS
;
; CALLING SEQUENCE:
;   result=caching(filename)
;
; INPUTS:
;   filename: file to cache
;
; KEYWORDS:
;   delete: if set it deletes the existing cache file and also the lock file
;
; EXAMPLE:
;   result=caching(filename)
;
; MODIFICATION HISTORY:
;   @copyright: 2011 R.Bauer
;   @license: LGPL V2 or later, see LICENSE.txt
;-

FUNCTION caching, filename, delete=delete, cachepath=cachepath, no_zip=no_zip, LOGGER=logger, PRINT=print, QUIET=quiet
  
  if n_elements(filename) eq 0 then begin
    message, 'no filename given'
  endif
  if not caching_file_exist(filename) then begin
    message, 'filename: ' + filename + ' does not exist'
  endif
  
  if N_ELEMENTS(cachepath) eq 0 then message, 'Set a cachepath'
  
  if ~KEYWORD_SET(no_zip) then begin
    ending = STRSPLIT(filename, '.', /EXTRACT, COUNT=cnt)
    if cnt ne 0 then begin
      ending = ending[cnt-1]
      if str_equiv(ending) eq 'ZIP' then begin
        origname = caching(filename, delete=delete, cachepath=cachepath, /no_zip, LOGGER=logger, PRINT=print, QUIET=quiet)
        lfile = utils_replace_string(origname, '.zip', '.sav')
        if KEYWORD_SET(delete) then begin
          restore, FILENAME=lfile
          FILE_DELETE, outname
          FILE_DELETE, lfile
          caching_log, 'uncompressed file: ' + outname + ' deleted', LOGGER=logger, PRINT=print, QUIET=quiet
          return, origname
        endif else begin
          spawn, 'unzip ' + origname + ' -d '+ cachepath, ret, err, EXIT_STATUS=status
          if err ne '' then Message, 'error on uncomp'
          outname = STRCOMPRESS((STRSPLIT(ret[1], ':', /EXTRACT))[1], /REMOVE_ALL)
          if ~ FILE_TEST(outname) then Message, 'error on uncomp filename'
          save, outname, FILENAME=lfile
          caching_log, 'file: ' + origname + ' uncompressed', LOGGER=logger, PRINT=print, QUIET=quiet
          return, outname
        endelse
      endif
    endif
  endif
   
  _filename = utils_replace_string(filename, '/', '_')
  cachefile = caching_combine_path_file(cachepath, _filename)
  lockfile = caching_combine_path_file(cachepath, _filename + '.lck')
  
  IF KEYWORD_SET(delete) and caching_file_exist(cachefile) THEN BEGIN
    file_delete, cachefile
    caching_log, 'cachefile: ' + cachefile + ' deleted', LOGGER=logger, PRINT=print, QUIET=quiet
    IF caching_file_exist(lockfile) THEN BEGIN
      file_delete, lockfile
      caching_log, 'lockfile: ' + lockfile + ' deleted', LOGGER=logger, PRINT=print, QUIET=quiet
    ENDIF
    return, cachefile
  ENDIF
  
  CASE 1 OF
    ; ORDER is important!!!
    caching_file_exist(cachefile) eq 1 and caching_file_exist(lockfile) eq 0: return, cachefile
    caching_file_exist(cachefile) eq 0 and caching_file_exist(lockfile) eq 1: BEGIN
      WHILE caching_file_exist(lockfile) DO BEGIN
        caching_log, 'no cachefile: ' + cachefile + '  yet lockfile: '+ lockfile + ' still exists, we wait a second', LOGGER=logger, PRINT=print, QUIET=quiet
        wait,1
      ENDWHILE
      return, cachefile
    END
    caching_file_exist(cachefile) eq 0: BEGIN
      openw,lun,lockfile, /get_lun
      free_lun,lun
      file_copy, filename, cachefile
      ; if it was deleted by someone else
      if caching_file_exist(lockfile) then file_delete, lockfile
      if not caching_file_exist(cachefile) then begin
        message, "someone has to early deleted your cache file: " + cachefile
      endif
      return, cachefile
    END
    caching_file_exist(cachefile) eq 1 and caching_file_exist(lockfile) ne 0: BEGIN
      WHILE caching_file_exist(lockfile) DO BEGIN
        caching_log, 'cachefile: '+ cachefile +' exists but also lockfile: '+ lockfile + ' still exists, we wait a second', LOGGER=logger, PRINT=print, QUIET=quiet
        wait,1
      ENDWHILE
      return, cachefile
    END
    ELSE: BEGIN
      message, "something is going really bad"
    END
  ENDCASE
END