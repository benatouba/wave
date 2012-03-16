;+
; :Description:
;    This routine looks for compressed NCDC station data 
;    in a local or remote Global Summary of the Day (GSOD) 
;    directory, uncompress the files and generates an ASCII
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
pro w_ncdc_extract_gsod, usaf, wban, gsod_directory, out_directory, S_YEAR=s_year, E_YEAR=e_year, NOLOG=nolog

  ; Set Up environment
  COMPILE_OPT idl2
  @WAVE.inc
  ;ON_ERROR, 2
  
  ; Check params
  if N_ELEMENTS(usaf) ne N_ELEMENTS(wban) then MESSAGE, 'USAF and WBAN are not of the same length.' ;are usaf and wban fron the same length?
  if ~ utils_is_dir(gsod_directory) then gsod_directory = DIALOG_PICKFILE(TITLE='Please select GSOD data directory', /MUST_EXIST, /DIRECTORY)
  if ~ utils_is_dir(gsod_directory) then MESSAGE, 'GSOD data directory not valid.' ; make sure that it contains GSOD data! (test)
  if ~ utils_is_dir(out_directory) then out_directory = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)
  if ~ utils_is_dir(out_directory) then MESSAGE, 'Output data directory not valid.'
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  
  ;Check existence of USAF/WBAN combination in history
  nostat = N_ELEMENTS(usaf)
  st_names = STRARR(nostat)
  st_lons = STRARR(nostat)
  st_lats = STRARR(nostat)
  test_id = ncdc_history.usaf+ncdc_history.wban
  for i=0, nostat-1 do begin
    uw=where(test_id eq usaf[i]+wban[i], cnt)
    if cnt eq 0 then MESSAGE, 'The station with USAF '+ usaf[i] + ' and WBAN ' + wban[i] + ' does not exist in NCDC history file.'
    st_names[i] = ncdc_history.name[uw]
    st_lons[i] = STRING(ncdc_history.lon[uw], FORMAT='(F10.5)')
    st_lats[i] = STRING(ncdc_history.lat[uw], FORMAT='(F10.5)')    
  endfor

  talk = ~ KEYWORD_SET(NOLOG)
  
  ; Define output file names
  str_usaf = usaf
  str_wban = wban
  str_ofiles = 'gsod-'+str_usaf+'-'+str_wban+'.dat'
  
  ;For the log
  if talk then begin
    ac_date = time_to_str(QMS_TIME(), MASK='YYYY-MM-DD_HHTT')
    logfile='GSOD_EXTRACT_'+ac_date+'.log'
  endif
  ; Read available years
  years=FILE_BASENAME(FILE_SEARCH(gsod_directory+'/*',  COUNT=nyears, /TEST_DIRECTORY))
  if nyears eq 0 then MESSAGE, 'GSOD data directory not valid.'
  
   
  if N_ELEMENTS(S_YEAR) ne 0 then start_year=s_year else start_year=years[0]
  if N_ELEMENTS(E_YEAR) ne 0 then end_year=e_year else end_year=years[nyears-1]
  nyears = LONG(end_year)-LONG(start_year)+1
  years = str_equiv(INDGEN(nyears) + LONG(start_year))  
  
  nvalidstat = 0L
  for s=0, nostat-1 do begin
  
    ;Begin search for station number
    print, 'Searching data for station id '+str_usaf[s]+'. ' + str_equiv(nostat-s) + ' left.'
    nvalidyears = 0L
    
    for y=0, nyears-1 do begin
      ;Begin search for station ASCII file in every single folder
      search_folder = gsod_directory+'/'+years[y]+'/'
      search_file = search_folder + str_usaf[s]+'-'+str_wban[s]+'-'+years[y]
      
      ;Check for variable file extension
      file_op=FILE_TEST(search_file+'.op.gz')
      file_gz=0
      if file_op eq 0 then file_gz=FILE_TEST(search_file+'.gz')
      if (file_op eq 0) and (file_gz eq 0) then continue
      nvalidyears += 1
      
      if file_op eq 1 then file_path=search_file+'.op.gz' else file_path=search_file+'.gz'
      
      ;Start writing ASCII file
      OPENR, lun, file_path, /GET_LUN, /COMPRESS
      line = ''
      readf, lun, line
      
      if nvalidyears eq 1 then begin
        y0 = years[y]
        OPENW, luns, out_directory+'/'+str_ofiles[s], /GET_LUN
        printf, luns, line ; write the header
      endif else OPENU, luns, out_directory+'/'+str_ofiles[s], /GET_LUN, /APPEND
      
      while not eof(lun) do begin
        readf, lun, line
        printf, luns, line
      endwhile
      free_lun, lun
      free_lun, luns
      y1 = years[y]
    endfor
    
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
      
      stat_info=str_usaf[s]+'-'+str_wban[s]+', '+st_names[s]+', '+ st_lons[s]+', '+ st_lats[s]+', '+ y0+', '+ y1
      OPENU, lun, out_directory+'/'+ logfile, /GET_LUN, /APPEND
      printf, lun, stat_info
      free_lun, lun
    endif
  endfor
  
  Print, 'Done. ' + str_equiv(nvalidstat)+ ' NCDC stations were saved in the output directory.'
  
end