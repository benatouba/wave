;+
; :Description:
;    This routine reads the NCDC history file from the NCDF ftp server
;    and replaces the WAVE resource file with the updated information.     
;    
;    This routine must not be called very often (only when the history 
;    file changed on the server).
;
;
; :History:
;     Written by FaM, 2012.
;
;-

;+
; :Description:
;    To remove the " string from ascii fields
;
;-
function w_ncdc_read_ish_history_csv_clean_str, str

  n = N_ELEMENTS(str)
  for i=0, N_ELEMENTS(str)-1 do str[i] = STRJOIN(STRTOK(str[i], '"', /REGEX, /EXTRACT))
  return, str
  
end

pro w_ncdc_read_ish_history_csv, CACHE_DIRECTORY=cache_directory

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ;  ON_ERROR, 2
  
  if N_ELEMENTS(CACHE_DIRECTORY) eq 0 then CACHE_DIRECTORY = ''
  local_path = 'tmp_ish-history.csv'

  oUrl = OBJ_NEW('IDLnetUrl', URL_SCHEME='ftp', URL_HOST='ftp.ncdc.noaa.gov/pub/data/gsod/')
  oURL->SetProperty, URL_PATH = 'ish-history.csv'
  dummy = oURL->Get(FILENAME = local_path) 
  UNDEFINE, ourl
    
  ; Read the CSV file (READ_CSV not possible because of the wrong datatypes)
  restore, WAVE_RESOURCE_DIR + '/ncdc/ascii_template_ish_history_file.tpl'
  ascii_data = READ_ASCII(local_path, TEMPLATE=template)
  FILE_DELETE, local_path
  
  ;Read the needed tags and remove the "
  usaf= STRMID(ascii_data.usaf, 1, 6)
  wban= STRMID(ascii_data.wban, 1, 5)
  
  name = w_ncdc_read_ish_history_csv_clean_str(ascii_data.name)
  lon = w_ncdc_read_ish_history_csv_clean_str(ascii_data.lon)
  lat = w_ncdc_read_ish_history_csv_clean_str(ascii_data.lat)
  elev = w_ncdc_read_ish_history_csv_clean_str(ascii_data.elev)
  begind = w_ncdc_read_ish_history_csv_clean_str(ascii_data.begind)
  endd = w_ncdc_read_ish_history_csv_clean_str(ascii_data.endd)
  
  ; Select valid stations
  pnok = where(lon eq '' or lat eq '' or elev eq '' or lon eq '-99999' or lat eq '-99999' or elev eq '-99999', cntnok, COMPLEMENT=pok, NCOMPLEMENT=cntok)
  if cntok ne 0 then begin
    usaf = usaf[pok]
    wban = wban[pok]
    name = name[pok]
    lon = lon[pok] * 0.001
    lat = lat[pok] * 0.001
    elev = elev[pok] * 0.1
    begind = begind[pok]
    endd = endd[pok]
  endif else message, WAVE_Std_Message('ish_history_file', /FILE)
  
  ; for the stations where we have a period information, take it
  pdok = where(begind ne '' and endd ne '', cntdok)
  t0 = LON64ARR(cntok)
  t1 = LON64ARR(cntok)
  tvalid = BYTARR(cntok)
  if cntdok ne 0 then begin
    begind = begind[pdok]
    endd = endd[pdok]   
    ;19640101
    _t0 = QMS_TIME(YEAR=STRMID(begind,0,4) > 1900,MONTH=STRMID(begind,4,2),day=STRMID(begind,6,2))
    _t1 = QMS_TIME(YEAR=STRMID(endd,0,4) > 1900,MONTH=STRMID(endd,4,2),day=STRMID(endd,6,2))
    t0[pdok] = _t0
    t1[pdok] = _t1
    tvalid[pdok] = 1B
  endif
  
  ;Fill the structure and save it
  ncdc_history = {n_stations:cntok, $
                  usaf:usaf ,$
                  wban:wban ,$
                  name:name ,$
                  lon:lon ,$
                  lat:lat ,$
                  elev:elev ,$
                  tvalid:tvalid ,$
                  t0:t0 ,$
                  t1:t1  $
                  }
    
  save, ncdc_history, FILENAME= WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  
end