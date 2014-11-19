function tryget, oURL, tmpf

  catch, Error_status
  if Error_status ne 0 then begin
    catch, /CANCEL
    return, 0
  endif
  dummy =  oURL->Get(FILENAME=tmpf)
  return, 1
end

;+
; :Description:
;    This routine looks for compressed NCDC station data 
;    on the GSOD ftp, uncompress the files and generates an ASCII
;    file per selected station on a local directory.
;
; :Params:
;    usaf: in, required, type=long
;          an array of the requested station usaf ids (see `w_ncdc_gsod_select_ids`)
;    wban: in, required, type=long
;          an array of the requested station wban ids (same size as `usaf`)
;    gsod_directory: in, required, type=string
;                    the path to the local or remote GSOD directory 
;                    (e.g. KLIMA_FS1:/data/Global/NCDC/GSOD)
;    out_directory: in, required, type=string
;                   the path to the directory where to store the 
;                   uncompressed files. If it doesn't exist, 
;                   it will be created. If gsod files of corresponding
;                   ids are found in the directory (from e.g. a previous
;                   call of this routine), they will be deleted.
;    S_YEAR: in, optional, type=integer
;            If set, the routine extracts data beginning from the given start year.
;            Stations without data for the given time period are ignored.       
;    E_YEAR: in, optional, type=integer
;            If set, the routine extracts data including the given end year.
;            Stations without data for the given time period are ignored.                 
;    NOLOG: in, optional, type=boolean
;         Per default, the routine logs what it does in
;         a log file saved in the output directory.
;         Set this keyword to avoid this.                  
;                  
; :Author:
;   FaM, CoK
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification: 12 Jan 2012
;-
pro w_ncdc_download_gsod, usaf, wban, out_directory, S_YEAR=s_year, E_YEAR=e_year, NOLOG=nolog

  ; Set Up environment
  COMPILE_OPT idl2
  @WAVE.inc
  ;ON_ERROR, 2
  
  ; Check params
  if N_ELEMENTS(usaf) ne N_ELEMENTS(wban) then MESSAGE, 'USAF and WBAN are not of the same length.' ;are usaf and wban fron the same length?
  if ~ utils_is_dir(out_directory) then MESSAGE, 'Output data directory not valid.'
  
  if N_ELEMENTS(S_YEAR) ne 0 then start_year=s_year else start_year=1901
  if N_ELEMENTS(E_YEAR) ne 0 then end_year=e_year else end_year=2014
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  
  nostat = N_ELEMENTS(usaf)
  test_id = ncdc_history.usaf+ncdc_history.wban
  for i=0, nostat-1 do begin
    uw = where(test_id eq usaf[i]+wban[i], cnt)
    if cnt eq 0 then MESSAGE, 'The station with USAF '+ usaf[i] + ' and WBAN ' + wban[i] + ' does not exist in NCDC history file.'
  endfor

  talk = ~ KEYWORD_SET(NOLOG)
  
  ; Define output file names
  str_usaf = usaf
  str_wban = wban
  str_ofiles = 'gsod-'+str_usaf+'-'+str_wban+'.dat'
  years = w_str(start_year + lindgen(end_year - start_year + 1))
  
  ;For the log
  if talk then begin
    ac_date = time_to_str(QMS_TIME(), MASK='YYYY-MM-DD_HHTT')
    logfile='GSOD_EXTRACT_'+ac_date+'.log'
  endif
  
  oUrl = OBJ_NEW('IDLnetUrl', URL_SCHEME='ftp', URL_HOST='ftp.ncdc.noaa.gov/pub/data/gsod')
  
  tmpf = out_directory + '/tmp.op.gz'
  nostat = N_ELEMENTS(str_usaf)
  nvalidstat = 0L
  for s=0, nostat-1 do begin
   
    ;Begin search for station number
    print, 'Searching data for station id '+str_usaf[s]+'. ' + str_equiv(nostat-s) + ' left.'
    nvalidyears = 0L
    
    foreach y, years do begin
      
      ;Begin search for station ASCII file in every single folder
      fp = y + '/' + str_usaf[s]+'-'+str_wban[s]+'-'+ y + '.op.gz'
     
      
      oURL->SetProperty, URL_PATH=fp
      ok = tryget(oURL, tmpf)
      if ok eq 0 then continue 
      
      nvalidyears += 1
      
      ;Start writing ASCII file
      OPENR, lun, tmpf, /GET_LUN, /COMPRESS
      line = ''
      readf, lun, line
      
      if nvalidyears eq 1 then begin
        OPENW, luns, out_directory+'/'+str_ofiles[s], /GET_LUN
        printf, luns, line ; write the header
      endif else OPENU, luns, out_directory+'/'+str_ofiles[s], /GET_LUN, /APPEND
      
      while not eof(lun) do begin
        readf, lun, line
        printf, luns, line
      endwhile
      free_lun, lun
      free_lun, luns
      file_delete, tmpf
    endforeach
    
    if nvalidyears gt 0 then nvalidstat +=1 else continue
   
    ;Start writing log file
    if talk then begin
    
      if nvalidstat eq 1 then begin
        OPENW, lun, out_directory+'/'+ logfile, /GET_LUN
        header='#######GSOD_EXTRACT_NCDC LOG FILE_'+ac_date+'#######'
        descr= 'ID (USAF-WBAN), NAME, LON, LAT, START_YEAR, END_YEAR'
        printf, lun, header
        printf, lun, descr
        free_lun, lun
      endif
      
    endif
  endfor
  undefine, oURL
  Print, 'Done. ' + str_equiv(nvalidstat)+ ' NCDC stations were saved in the output directory.'
  
end