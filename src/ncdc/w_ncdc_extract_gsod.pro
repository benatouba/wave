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
;    LOG: in, optional, type=boolean
;                   Per default, the routine logs what it does in
;                   a log file saved in the output directory.
;                   Set this keyword to avoid this.                  
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
pro w_ncdc_extract_gsod, usaf, wban, gsod_directory, out_directory, S_YEAR=s_year, E_YEAR=e_year, LOG=log

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
  test_id = ncdc_history.usaf+ncdc_history.wban
  for i=0, nostat-1 do begin
    uw=where(test_id eq usaf[i]+wban[i], cnt)
    if cnt eq 0 then MESSAGE, 'The station with USAF '+ usaf[i] + ' and WBAN ' + wban[i] + ' does not exist in NCDC history file.'
    st_names[i] = ncdc_history.name[uw]
  endfor
  
  talk = ~ KEYWORD_SET(LOG)
  
  ; Define output file names
  str_usaf = usaf
  str_wban = wban
  str_ofiles = 'gsod-'+str_usaf+'-'+str_wban+'.dat'
  nyears = 0L
  
  ; Read available years
  years=FILE_BASENAME(FILE_SEARCH(gsod_directory+'/*',  COUNT=nyears, /TEST_DIRECTORY))
  if nyears eq 0 then MESSAGE, 'GSOD data directory not valid.'
  
  nvalidstat = 0L
  for s=0, nostat-1 do begin
    ;Begin search for station number
    print, 'Searching data for station id '+str_usaf[s]+'. ' + str_equiv(nostat-s) + ' left.'
    nvalidyears = 0L
    selyear = 0L
    for y=0, nyears-1 do begin
      ;Begin search for station ASCII file in every single folder
      search_folder = gsod_directory+'/'+years[y]+'/'
      search_file = search_folder + str_usaf[s]+'-'+str_wban[s]+'-'+years[y]
      
      ;Check for variable file extension
      file_op=FILE_TEST(search_file+'.op.gz')
      file_gz=0L
      if file_op eq 0 then file_gz=FILE_TEST(search_file+'.gz')
      
      if (file_op eq 0) and (file_gz eq 0) then continue
      nvalidyears += 1
      if (N_ELEMENTS(S_YEAR) eq 0) or (N_ELEMENTS(E_YEAR) eq 0) then selyear+=1
      
      if nvalidyears eq 1 then y0=years[y]
      y1= years[y]
      end_year=TIME_to_STR(QMS_TIME(), MASK='YYYY')
      start_year=y0
      
      if N_ELEMENTS(S_YEAR) ne 0 then start_year=s_year
      if N_ELEMENTS(E_YEAR) ne 0 then end_year=e_year
      diff=long(end_year)-long(start_year)
      
      if (N_ELEMENTS(S_YEAR) ne 0) or (N_ELEMENTS(E_YEAR) ne 0) then begin
      
        yarr=INDGEN(diff+1)+long(S_YEAR)
        yt=where(yarr eq y1, ycnt)
        if ycnt eq 0 then continue
        if ycnt eq 1 then selyear+=1
        
      endif
      
      if file_op eq 1 then file_path=search_file+'.op.gz' else file_path=search_file+'.gz'
      
      ;Start writing ASCII file
      OPENR, lun, file_path, /GET_LUN, /COMPRESS
      if selyear eq 1 then OPENW, luns, out_directory+'/'+str_ofiles[s], /GET_LUN
      if selyear gt 1 then OPENU, luns, out_directory+'/'+str_ofiles[s], /GET_LUN, /APPEND
      line = ''
      linecnt=0
      while not eof(lun) do begin
        linecnt+=1
        readf, lun, line
        if selyear eq 1 then printf, luns, line
        if (selyear gt 1) and (linecnt gt 1) then printf, luns, line
      endwhile
      free_lun, lun
      free_lun, luns
      
    endfor
    
    if selyear gt 0 then nvalidstat +=1
    if nvalidstat eq 0 then continue
    
    ;Start writing log file
    if talk then begin
      if nvalidstat eq 1 then begin
        ac_date=time_to_str(QMS_TIME(), MASK='YYYY-MM-DD_HHTT')
        logfile='GSOD_EXTRACT_'+ac_date+'.log'
        
        OPENW, lun, out_directory+'/'+ logfile, /GET_LUN
        
        header='#######GSOD_EXTRACT_NCDC LOG FILE_'+ac_date+'#######'
        descr= 'USAF, WBAN, NAME, START_YEAR, END_YEAR'
        
        printf, lun, header
        printf, lun, descr
      endif
      
      stat_info=str_usaf[s]+', '+str_wban[s]+', '+st_names[s]+', '+y0+', '+y1
      
      if (nvalidstat gt 1) and (selyear gt 0) then OPENU, lun, out_directory+'/'+ logfile, /GET_LUN, /APPEND
      if selyear gt 0 then printf, lun, stat_info
      
      free_lun, lun
    endif
  endfor
  
  Print, 'Done. ' + str_equiv(nvalidstat)+ ' NCDC stations were saved in the output directory.'
  
end