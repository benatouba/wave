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
;    out_directory: in, optional, type=boolean
;                   Per default, the routine logs what it does in
;                   the standard output. Set this keyword to avoid this
;                  
; :Author: CoK, FaM
;
; :History:
;     Written by CoK, FaM, 2011.
;-
pro w_ncdc_gsod_extract, usaf, wban, gsod_directory, out_directory, QUIET=quiet
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ;ON_ERROR, 2
  
  ; Check params
   ;are usaf and wban fron the same length?
   ;are the two directories valid? 
   ;are all usaf-wban combinations to find in the history file
  
  talk = ~ KEYWORD_SET(QUIET)
  
  
  ; Define output file names  gsod-485540-99999.dat
  str_usaf = STRING(usaf, FORMAT='(I06)')
  str_wban = STRING(wban, FORMAT='(I05)')
  str_ofiles = 'gsod-'+str_usaf+'-'+str_wban+'.dat'
  nostat = N_ELEMENTS(str_ofiles)
  

  
  ; See how many possible years we have
   ; look in the gsod directory for the years
  nyears = 0L 
    
  ; Go threw the stations and the years
  nvalidstations = 0L
  
  for s=0, nostat-1 do begin
   ; Check output diretory
    ; delete possible ouput file
   
   for y=0, nyears-1 do begin
    
    ; check if station-year combination is found
    ; if not, jump to the next year
    
    ; we found one
    nvalidstations += 1
    
    ; copy the file localy and uncompress it
         
    ; check if output file is there. if not, copy the uncompressed file in the output directory AND rename it. Jump to the next year
    
    ; If there is one, parse the lines after the header and put them at the end of the existing file
      ; (openR, openW)
;      openr, lun, filepath, /GET_LUN
;      line = ''
;      'while ...
;      free_lun


   endfor
    
  endfor
  
  if talk then Print, 'Done. Found ' + str_equiv(nvalidstations) + ' valid NCDC stations in the GSOD repository.'

end