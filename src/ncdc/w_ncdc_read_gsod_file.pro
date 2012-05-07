;+
; 
; The `w_ncdc_read_gsod_file` routine parses Global Summary of the Day (GSOD) NCDC ascii files. 
; The files must have the same data format as downloaded from the NCDC website
; (`http://www.ncdc.noaa.gov/oa/climate/climatedata.html#daily`) 
; or as generated by the 'w_ncdc_extract_gsod' routine. 
;       
; The `w_ncdc_read_gsod_file` function will parse all gsod-*.dat files in a 
; directory or one selected file, and gets additional info
; such has lat and lon or elevation from the resource history file.
;       
; The `NCDC_DAILY_DATA` function returns an array of structures of the form
; according to the `w_ts_MakeStation` routine for every selected station: 
; 
;       
;           data = { 
;                   NAME=name, $ ;                The name of the station
;                   ID=id, $ ;                    Station ID
;                   DESCRIPTION=description, $ ;  A short description of the station 
;                   ELEVATION=elevation, $ ;      Altitude in m
;                   LOC_X=loc_x, $ ;              X Location in SRC
;                   LOC_Y=loc_y, $ ;              Y Location in SRC
;                   SRC=src, $ ;                  Location information ({TNT_DATUM}, {TNT_PROJ})
;                   OP_TIME=op_time, $ ;          Operating period [QMS,QMS]
;                   NVARS=nvars, $ ;              Number of variables
;                   VARNAMES=varnames,  $ ;       Variable names
;                   VARS=vars   ;                 Variables (W_WAR)
;                   }
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
        
;+
; 
; :Description:
;       This function is called internally. It is called to retrieve the value in a NCDC File
;       and convert it to the right format and unit.
;
; :Categories:
;       General/ NCDC 
;       
; :Private:
;
; :Params:
;     ascii_data: in, type=, default=none
;           The data of the NCDC file.
;     ascii_tag: in, optional, type=string (array), default=none
;         The  tag(s) of the value(s) to retrieve of the NCDC file.
;       
; :Returns:
;       The corrected value(s) of the NCDC file.
;
; :History:
;       Written by FaM, CoK, 2012.
;-
function w_ncdc_read_gsod_file_parse_val_from_ascii, data, ascii_tag

  ; Set up environment
  compile_opt idl2
  @WAVE.inc
    
  val = '' 
  missing = !VALUES.F_NAN
  agg_method = 'MEAN'
  
  CASE ascii_tag OF
  
    'TEMP': begin
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Mean temperature for the day in degC', missing:missing, agg_method:agg_method}
    end
    
    'DEWP': begin
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Mean dew point for the day in degC', missing:missing, agg_method:agg_method}
    end
    
    'MAX': begin
      data = float(data)
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Maximum temperature reported during the day in degC', missing:missing, agg_method:agg_method}
    end
    
    'MIN': begin
      data = float(data)
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Minimum temperature reported during the day in degC', missing:missing, agg_method:agg_method}
    end
    
    'DIR': begin
      data = float(data)
      p = where(data gt 360 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'deg', description:'Wind direction in compass degrees', missing:missing, agg_method:agg_method}   ; tag for hourly only
    end
    
    'VISIB': begin
      data = data * 1.609344  ; to km
      p = where(data gt 999 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'km', description:'Mean visibility for the day in km', missing:missing, agg_method:agg_method}
    end
    
    'WDSP': begin
      data = data * 0.514  ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Mean wind speed for the day in m/s', missing:missing, agg_method:agg_method}
    end
    
    'SPD': begin
      data = data * 0.44704  ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Wind speed in m/s', missing:missing, agg_method:agg_method}   ;tag for hourly only
    end
    
    'MXSPD': begin
      data = data * 0.514  ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Maximum sustained wind speed reported for the day in m/s', missing:missing, agg_method:agg_method}  
    end
    
    'GUST': begin
      data = data * 0.514  ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Maximum wind gust reported for the day in m/s', missing:missing, agg_method:agg_method} 
    end
    
    'PRCP': begin
      data = float(data)
      data = data * 2.54 * 10 ; to mm
      p = where(data gt 500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'mm', description:'Total precipitation (rain/melted snow) reported during the day in mm', missing:missing, agg_method:agg_method} 
    end
    
    'PCP06': begin
      data = float(data)
      data = data * 2.54 * 10 ; to mm
      p = where(data gt 500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'mm', description:'6-hour liquid precipitation report in mm', missing:missing, agg_method:agg_method} 
    end
    
    'PCP24': begin
      data = float(data)
      data = data * 2.54 * 10 ; to mm
      p = where(data gt 500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'mm', description:'24-hour liquid precipitation report in mm', missing:missing, agg_method:agg_method} 
    end
    
    'PRCPFLAG': begin
       data = ascii_data.prcp[index]
       val = {data:data, vname:ascii_tag, unit:'', description:'Number of prcp reports in time interval', missing:missing, agg_method:agg_method} 
    end
    
    'SNDP': begin
      data = data * 2.54 * 10 ; to mm
      p = where(data gt 500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'mm', description:'Snow depth in mm', missing:missing, agg_method:agg_method} 
    end    
;    'FRSHTT': begin
;      missing = ''
;      agg_method = 'NONE'
;      frshtt = data
;      str = STRARR(N_ELEMENTS(ascii_data.FRSHTT))  ;  ' 011000'
;      res = STRMID(frshtt, 0, 1)
;      p = where(res eq 1, cnt)
;      if cnt ne 0 then str[p] += 'Fog, '
;      res = STRMID(frshtt, 1, 1)
;      p = where(res eq 1, cnt)
;      if cnt ne 0 then str[p] +='Rain or Drizzle, '
;      res = STRMID(frshtt, 2, 1)
;      p = where(res eq 1, cnt)
;      if cnt ne 0 then str[p] +='Snow or Ice Pellets, '
;      res = STRMID(frshtt, 3, 1)
;      p = where(res eq 1, cnt)
;      if cnt ne 0 then str[p] += 'Hail, '
;      res = STRMID(frshtt, 4, 1)
;      p = where(res eq 1, cnt)
;      if cnt ne 0 then str[p] += 'Thunder, '
;      res = STRMID(frshtt, 5, 1)
;      p = where(res eq 1, cnt)
;      if cnt ne 0 then str[p] += 'Tornado or Funnel Cloud, '
;      val = {data:res, vname:ascii_tag, unit:'', description:'Indicator for certain occurences during the day', missing:missing, agg_method:agg_method} 
;    end
    
    ELSE :
  ENDCASE
 
  return, val

end

;+
; 
; :Description:
;        This function is private. It is called to retrieve the IDL formatted julian day      
;       from the long yearmoda coming from the DAILY NCDC ascii string 
;
; :Categories:
;       General/ NCDC 
;
; :Private:
;
; :Params:
;     YEARMODA: in, type=long, default=none
;             Date of the NCDC file in the form YRMODA.
;           
;  :Returns:
;       The date in julian day format.
;
; :History:
;       Written by FaM, 2009.
;-
function w_ncdc_read_gsod_file_parse_time, yearmoda


  ; Set up environment

  compile_opt idl2
  @WAVE.inc

  year =  yearmoda / 10000
  month = (yearmoda - year * 10000) / 100
  day = yearmoda - year * 10000 - month * 100
  
  return, QMS_TIME(MONTH = month, DAY = day, YEAR = year)
  
end

;+
;
; :Description:
;         This procedure parses daily NCDC files (GSOD) and creates a data structure containing all
;         values for the available period. 
;
; :Categories:
;       General/ NCDC
;
; :Keywords:
;     FILE: in, optional, type=string
;           The ASCII file to parse, containing GSOD data of one or several NCDC stations. 
;           Either FILE or DIRECTORY must be set. 
; 
;     DIRECTORY: in, optional, type=string
;                The directory containing all ASCII files to read. The file names must 
;                be 'gsod-*.dat'. Appropriate GSOD files can be created with the
;                'w_ncdc_extract_gsod' procedure. 
;                Either FILE or DIRECTORY must be set. 
;     KEEP_VARS: in, optional, type=string
;                 array of variable names that have to be kept for each station
;     REMOVE_VARS: in, optional, type=string
;                  array of variable names that have to be removed for each station
;                 
; :Returns:
;       Data in the form of a structure (see `General information` for more info).
;       If more than one station is parsed, an array of structures is returned.
;
; :Examples:
;
;   The data is retrieved by calling::
;      IDL> help, data, /str
; ** Structure W_TS_STATION, 11 tags, length=104, data length=92:
;     NAME            STRING    'LISTA FLYPLASS'
;     ID              STRING    '014280'
;     DESCRIPTION     STRING    'NCDC Station'
;     ELEVATION       DOUBLE           10.000000
;     LOC_X           DOUBLE           6.6330004
;     LOC_Y           DOUBLE           58.100002
;     SRC             POINTER   <PtrHeapVar23446>
;     OP_TIME         LONG64    Array[2]
;     NVARS           LONG                10
;     VARNAMES        POINTER   <PtrHeapVar23447>
;     VARS            POINTER   <PtrHeapVar23448>
;
; :History:
;       Written by FaM, CoK, 2012.
;-
function w_ncdc_read_gsod_file, FILE=file, $
    DIRECTORY=directory, $
    KEEP_VARS=keep_vars, $
    REMOVE_VARS=remove_vars
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
    
  ; RETRIEVE file and template  
  if (N_ELEMENTS(file) eq 0) and (N_ELEMENTS(directory) eq 0) then MESSAGE, 'Either of the keywords DIRECTORY or FILE must be set!'
  if N_ELEMENTS(KEEP_VARS) ne 0 and N_ELEMENTS(REMOVE_VARS) ne 0 then Message, 'Ambiguous keywords combination.'
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ascii_template_ncdc_gsod.tpl'
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  
    
  stat_val=0L
  if N_ELEMENTS(DIRECTORY) ne 0 then file_list=FILE_SEARCH(directory, 'gsod-*.dat', count=filecnt)
  if (N_ELEMENTS(FILE) ne 0) then begin
  if STRMATCH(FILE_BASENAME(FILE), 'gsod-*.dat', /FOLD_CASE) then $
  file_list=file
  endif
  filecnt = N_ELEMENTS(file_list)
  
  if filecnt eq 0 then message, 'No file(s) found.'
  
  if filecnt gt 1 then station_list = Obj_New('w_ts_StatSet')
  
  for t=0, filecnt-1 do begin
    ascii_data = READ_ASCII(file_list[t], TEMPLATE=template)
    
    ascii_tags = tag_names(ascii_data)
    nvals = N_ELEMENTS(ascii_tags)
    
    date = w_ncdc_read_gsod_file_parse_time(ascii_data.yearmoda)+D_QMS ;NCDC mean time issue
    
    nb_entries=N_ELEMENTS(date)
    if nb_entries le 2 then continue 
    
    stat_val=+1
        
    for k = 0, nvals-1 do begin
      val = w_ncdc_read_gsod_file_parse_val_from_ascii(ascii_data.(k), ascii_tags[k])
      if arg_okay(val,TYPE=IDL_STRING) then continue
      if (N_ELEMENTS(stat_vars) eq 0) then stat_vars = ptr_new(val) else stat_vars = [stat_vars, ptr_new(val)]
    endfor
    
    stn=ascii_data.stn
    wban=ascii_data.wban
    uswb=stn+wban
    
    undefine, ascii_data
    
    ids = uswb[UNIQ(uswb, SORT(uswb))]
    nstats = N_ELEMENTS(ids)
    
    for i=0, nstats-1 do begin
    
      id = ids[i]
      id_str=stn[i]+'-'+wban[i]
      
      s=where(ncdc_history.usaf+ncdc_history.wban eq id, cnt)
      if cnt eq 0 then Message, id + ' not found, please update your NCDC history file!'
      
      lat = ncdc_history.lat[s]
      lon = ncdc_history.lon[s]
      elev= ncdc_history.elev[s]
      sname = ncdc_history.name[s]
      p = where(uswb eq id)
      _t = date[p]
     
      
      un = uniq(_t, sort(_t))
      if N_ELEMENTS(un) ne N_ELEMENTS(_t) then print, sname, ' ' , str_equiv(id), ' not unique'
      _t = _t[un]
      print, sname, id_str
      
      ncdc_station = OBJ_NEW('w_ts_Station',NAME=sname, $ ; The name of the station
         ID=id_str, $ ; Station ID
         DESCRIPTION='NCDC Station', $ ; A short description of the station
         ELEVATION=elev, $ ; altitude in m
         LOC_X=lon, $ ; X location in SRC
         LOC_Y=lat )
            
      for j=0, N_ELEMENTS(stat_vars)-1 do begin
        _var = *(stat_vars[j])
        _d = (_var.data[p])[un]
        if KEYWORD_SET(KEEP_VARS) then begin
           ok=where(keep_vars eq _var.vname, cnt)
           if cnt eq 0 then continue
        endif   
        if KEYWORD_SET(REMOVE_VARS) then begin
           ok=where(remove_vars eq _var.vname, cnt)
           if cnt ne 0 then continue
        endif
        ; TODO: keep and remvove vars
        var = OBJ_NEW('w_ts_Data', _d, _t, NAME=_var.vname, DESCRIPTION=_var.description, UNIT=_var.unit, $
          VALIDITY='INTERVAL', AGG_METHOD=_var.agg_method, MISSING=_var.missing, TIMESTEP=MAKE_TIME_STEP(day=1))
        if OBJ_VALID(var) then ncdc_station->addVar, var
      endfor
        
        if filecnt gt 1 then begin 
        if OBJ_VALID(ncdc_station) then station_list->AddStat, ncdc_station 
        endif else $
        station_list=ncdc_station
        
      undefine, stat_vars
    endfor
        
  endfor
  
  if stat_val eq 0 then message, 'No valid station.'    
  return, station_list
  
end

