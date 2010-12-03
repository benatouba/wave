; docformat = 'rst'
;+
; NCDC provides a few tools to parse NCDC ascii files. The files must 
; have the same data format as downloaded from the NCDC website
; (`http://www.ncdc.noaa.gov/oa/climate/climatedata.html#daily`).
;       
; The `NCDC_DAILY_DATA` function will parse all *.dat files in a 
; directory or any number of selected files, and gets additional info
; such has lat and lon or elevation from the ressource history file.
;       
; The `NCDC_DAILY_DATA` function returns an array of structures of the form::
;       
;           data = { 
;                    station_name = string : the name of the station 
;                    station_id   = Long   : the NCDC id of the station 
;                    lat          = float  : latitude of the station
;                    lon          = lon    : longitude of the station
;                    elev         = elev   : altitude of the station
;                    tag_list     = ['TEMP','TEMP_COUNT','DEWP','DEWP_COUNT','SLP','SLP_COUNT','STP','STP_COUNT','VISIB','VISIB_COUNT','WDSP','WDSP_COUNT','MXSPD','GUST','MAX','MIN','PRCP','SNDP'] 
;                    units        = ['C'   ,' '         ,'C'   ,' '         ,'hPa',' '        ,'hPa',' '        ,'km'   ,' '          ,'m/s' ,' '         ,'m/s ' ,'m/s' ,'C'  ,'C'  ,'mm'  ,'mm'  ] 
;                    time         = array of {ABS_DATE} : dimension nb_days 
;                    nb_days      = nb_days             : number of elements in the time array.
;                    values       = FLTARR(nb_days, 18) - 9999.0 : an array of dimension [nb_days, nb_tags] containing the parsed data or -9999.0 when not valid
;                    weather_ind  = STRARR(nb_days) : an array of dimension [nb_days] containing the weather indicator for that day
;                    }
;       
; The values from a specific tag (e.g. "TEMP") can be retrieved from all stations using `NCDC_get_Values`.
; 
; Careful! These routines are old and may be a bit outdated.
;       
; :Author:
; Fabien Maussion, Roman Finkelnburg, Cornelia Klein::
;     FG Klimatologie
;     TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification: 24 Nov 2010
;-

;+
; 
; :Description:
;       This function is called internally.
;       It creates an empty NCDC structure container for daily data.
;
; :Categories:
;       General/ NCDC 
;       
; :Private:
;
; :Params:
;     name: in, optional, type=string, default=none
;           The name of the station.
;     id: in, optional, type=long, default=none
;         The ID of the station.
;     time: in, optional, type={ABS_DATE} array, default=none
;           Dates of desired time period.
;     lat: in, optional, type=float, default=none
;           Latitude of the station.
;     lon: in, optional, type=float, default=none
;           Longitude of the station.
;     elev: in, optional, type=integer, default=none
;            Elevation of the station.  
;       
;
; :Returns:
;       An empty NCDC structure.
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_make_daily_data_struct, name, id, time, lat, lon, elev

 
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
    
    nb_days = N_ELEMENTS(time)
  
    data = { $
    station_name : name , $
    station_id   : id , $
    lat          : lat, $
    lon          : lon, $
    elev         : elev, $
    tag_list     : ['TEMP','TEMP_COUNT','DEWP','DEWP_COUNT','SLP','SLP_COUNT','STP','STP_COUNT','VISIB','VISIB_COUNT','WDSP','WDSP_COUNT','MXSPD','GUST','MAX','MIN','PRCP','SNDP'] , $
    units        : ['C'   ,' '         ,'C'   ,' '         ,'hPa',' '        ,'hPa',' '        ,'km'   ,' '          ,'m/s' ,' '         ,'m/s ' ,'m/s' ,'C'  ,'C'  ,'mm'  ,'mm'  ] , $
    time         : time , $
    nb_days      : nb_days , $
    values       : FLTARR(nb_days, 18) - 9999.0 , $
    weather_ind  : STRARR(nb_days) $
    }

  return, data
  
end


;+
; 
; :Description:
;       This function is called internally.
;       It creates an empty NCDC structure container for hourly data.
;
; :Categories:
;       General/ NCDC 
;
; :Private:
;
; :Params:
;     name: in, optional, type=string, default=none
;           The name of the station.
;     id: in, optional, type=long, default=none
;         The ID of the station.
;     time: in, optional, type={ABS_DATE} array, default=none
;           Dates of desired time period.
;     lat: in, optional, type=float, default=none
;           Latitude of the station.
;     lon: in, optional, type=float, default=none
;           Longitude of the station.
;     elev: in, optional, type=integer, default=none
;            Elevation of the station.  
;       
;
;  :Returns:
;       An empty NCDC structure.
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_make_hourly_data_struct, name, id, time, lat, lon, elev
    
  ; Set up environment

  compile_opt idl2
  @WAVE.inc
    
    nb_steps = N_ELEMENTS(time)
    nb_stat = N_ELEMENTS(file_list)
  
    data = { $
    station_name : name , $
    station_id   : id , $
    lat          : lat, $
    lon          : lon, $
    elev         : elev, $
    tag_list     : ['PCP06','PCP24','TEMP','SPD','DIR','DEWP','STP'] , $
    units        : ['mm'   ,'mm'   ,'C'   ,'m/s' ,' ' ,'C'   ,'hPa'] , $
    time         : time , $
    nb_steps     : nb_steps , $
    values       : FLTARR(nb_steps, 7) - 9999.0 , $
    weather_ind  : STRARR(nb_steps) $
    }

  return, data
  
end

;+
; 
; :Description:
;        This function is called internally. It is called to retrieve the value in a NCDC File
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
;  :Returns:
;       The corrected value(s) of the NCDC file.
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_VAL_from_ASCII, ascii_data, ascii_tag


  ; Set up environment

  compile_opt idl2
  @WAVE.inc

  if ascii_tag ne 'PRCPFLAG' then begin
    p = WHERE(TAG_NAMES(ascii_data) eq ascii_tag)
    if p eq -1 then print, ascii_tag, ' not found'
    val = ascii_data.(p)
  end
  
  CASE ascii_tag OF
  
    'TEMP': begin
      val = (val - 32.0) * 5.0/9.0 ; to celcius
      p = where(val gt 50 or val lt -80, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'DEWP': begin
      val = (val - 32.0) * 5.0/9.0 ; to celcius
      p = where(val gt 50 or val lt -80, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'MAX': begin
;      pos = STREGEX(val, '[0123456789\.]+', /FOLD_CASE, length=len)
;      val = FLOAT(STRMID(val, pos, len))
      val = float(val)
      p = where(val gt 50 or val lt -80, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'MIN': begin
;      pos = STREGEX(val, '[0123456789\.]+', /FOLD_CASE, length=len)
;      val = FLOAT(STRMID(val, pos, len))
      val = float(val)
      val = (val - 32.0) * 5.0/9.0 ; to celcius
      p = where(val gt 50 or val lt -80, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'DIR': begin
;      pos = STREGEX(val, '[0123456789\.]+', /FOLD_CASE, length=len)
;      val = FLOAT(STRMID(val, pos, len))
      val = float(val)
      p = where(val gt 360 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'VISIB': begin
      val = val * 1.609344  ; to km
      p = where(val gt 999 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'WDSP': begin
      val = val * 0.514  ; to m/s
      p = where(val gt 90 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'SPD': begin
      val = val * 0.44704  ; to m/s
      p = where(val gt 90 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'MXSPD': begin
      val = val * 0.514  ; to m/s
      p = where(val gt 90 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'GUST': begin
      val = val * 0.514  ; to m/s
      p = where(val gt 90 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'PRCP': begin
;      pos = STREGEX(val, '[0123456789\.]+', /FOLD_CASE, length=len)
;      val = FLOAT(STRMID(val, pos, len))
      val = float(val)
      val = val * 2.54 * 10 ; to mm
      p = where(val gt 500 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'PCP06': begin
;      pos = STREGEX(val, '[0123456789\.]+', /FOLD_CASE, length=len)
;      val = FLOAT(STRMID(val, pos, len))
      val = float(val)
      val = val * 2.54 * 10 ; to mm
      p = where(val gt 500 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'PCP24': begin
;      pos = STREGEX(val, '[0123456789\.]+', /FOLD_CASE, length=len)
;      val = FLOAT(STRMID(val, pos, len))
      val = float(val)
      val = val * 2.54 * 10 ; to mm
      p = where(val gt 500 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    'PRCPFLAG': begin
;      val = ascii_data.prcp[index]
;      pos = STREGEX(val, '[ABCDEFGHI]', /FOLD_CASE, length=len)
;      val = STRMID(val, pos, len)
       val = ascii_data.prcp[index]
       
    end
    
    'SNDP': begin
      val = val * 2.54 * 10 ; to mm
      p = where(val gt 500 or val lt 0, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
    
    ELSE : begin
      p = where(val gt 9990 or val lt -9990, cnt)
      if cnt gt 0 then val[p] = -9999.
    end
  ENDCASE

  return, val

end

;+
; 
; :Description:
;        This function is private. It is called to retrieve the IDL formatted julian day      
;       from the long yearmoda coming from the HOURLY NCDC ascii string. 
;
; :Categories:
;       General/ NCDC 
;       
; :Private:
;
; :Params:
;     YRMODAHRMN: in, type=long, default=none
;                 Date of the NCDC file in the form YRMODAHRMN.
;           
;  :Returns:
;       The date in julian day format.
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_parse_htime, YRMODAHRMN

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  year =  YRMODAHRMN / 100000000
  month = (YRMODAHRMN - year * 100000000) / 1000000
  day = (YRMODAHRMN - year * 100000000 - month * 1000000) / 10000
  hour = (YRMODAHRMN - year * 100000000 - month * 1000000 - day * 10000) / 100
  min = (YRMODAHRMN - year * 100000000 - month * 1000000 - day * 10000 - hour * 100)
  
  return, MAKE_ABS_DATE(MONTH = month, DAY = day, YEAR = year, hour = hour, min = min)
  
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
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_parse_time, yearmoda


  ; Set up environment

  compile_opt idl2
  @WAVE.inc

  year =  yearmoda / 10000
  month = (yearmoda - year * 10000) / 100
  day = yearmoda - year * 10000 - month * 100
  
  return, MAKE_ABS_DATE(MONTH = month, DAY = day, YEAR = year)
  
end

;+
; 
; :Description:
;         This procedure reads an array of NCDC data structures and 
;       returns a new structure containing the values of the chosen 
;       tag for a specified time period and for each station of 
;       the array. 
;
; :Categories:
;       General/ NCDC 
;
; :Params:
;     data: in, type=structure, default=none
;           NCDC array which might contain the data of several stations.
;     tag: in, type=string, default=none
;          The tag name of the desired values as a string.
;    
; :Keywords:   
;     START_TIME: in, optional, type={ABS_DATE}, default=01.01.2000
;                 The start date of the time period to retrieve.
;     END_TIME: in, optional, type={ABS_DATE}, default=31.12.2009
;               The end date of the time period to retrieve.
;                     
;  :Returns:
;       The values from all stations from the specified tag in a structure.
;
; :Author:
;       Fabien Maussion, Cornelia Klein::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, CoK 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_get_VALUES, data, tag, START_TIME = startT, END_TIME = endT


  ; Set up environment

  compile_opt idl2
  @WAVE.inc

  if not KEYWORD_SET(startT) then startT = MAKE_ABS_DATE(YEAR=2000, MONTH=01, DAY=01)
  if not KEYWORD_SET(endT) then endT = MAKE_ABS_DATE(YEAR=2009, MONTH=12, DAY=31)
  
  input_time = (data[0]).time

  tag_list=string((data[0]).tag_list)
  station_names = data.station_name
  
  p1 = where(input_time.qms eq startT.qms, cnt1)
  p2 = where(input_time.qms eq endT.qms, cnt2)
  if cnt1 eq 0 or cnt2 eq 0 then message, 'StartT or endT not in data'
  
  pt = where(tag_list eq tag, cntt)
  if cntt ne 1 then message, 'Tag not found or more than one tag given'
  
  values = reform(data.values[p1:p2,pt,*])
  times = input_time[p1:p2]
    
  return, {tag:tag, $
           station_names : station_names, $
           time : times, $
           values : values  $
           }
  
end

;+
; 
; :Description:
;         This procedure parses daily NCDC ASCII files and creates a data structure containing all
;       values for a given period. 
;
; :Categories:
;       General/ NCDC 
;       
; :Keywords:   
;     DIR_PATH: in, optional, type=string, default=DIALOG_PICKFILE()
;               Directory containing all *.dat files to read. If not set, a dialog window will open.
;     START_TIME: in, optional, type={ABS_DATE}, default=01.01.2000
;                 The start date of the time period to retrieve.          
;     END_TIME: in, optional, type={ABS_DATE}, default=31.12.2009
;               The end date of the time period to retrieve.
;                     
; :Returns:
;       Data in the form of a structure (see `General information` for more info). 
;       If more than one station is parsed, an array of structures is returned.
;       
; :Examples:
;  
;   The data is retrieved by calling::
;       IDL> data=ncdc_daily_data()
;       % Program caused arithmetic error: Floating illegal operand
;       IDL> help, data, /STRUCTURES 
;       ** Structure <dfbda68>, 11 tags, length=497440, data length=482820, refs=1:
;          STATION_NAME    STRING    'LHUNZE'
;          STATION_ID      STRING    '556960'
;          LAT             FLOAT     Array[1]
;          LON             FLOAT     Array[1]
;          ELEV            FLOAT     Array[1]
;          TAG_LIST        STRING    Array[18]
;          UNITS           STRING    Array[18]
;          TIME            STRUCT    -> ABS_DATE Array[3653]
;          NB_DAYS         LONG              3653
;          VALUES          FLOAT     Array[3653, 18]
;          WEATHER_IND     STRING    Array[3653]
;
; :Author:
;       Fabien Maussion, Cornelia Klein::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, CoK 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_DAILY_DATA, DIR_PATH = dir, START_TIME = startT, END_TIME = endT

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  ; RETRIEVE files and templates
  if KEYWORD_SET(dir) then $
    file_list = FILE_SEARCH(dir, '*.dat', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT) $
  else $
    file_list = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file(s) to parse', /MULTIPLE_FILES, /FIX_FILTER, /MUST_EXIST)
    
  if N_ELEMENTS(file_list) eq 0 then message, 'no *.dat files encountered'
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc.tpl'
  
  step = MAKE_TIME_STEP(DAY = 1)
  if KEYWORD_SET(startT) then begin
    time = MAKE_ENDED_TIME_SERIE(startT, endT, TIMESTEP= step) ; the time array. Daily values
  endif else begin
    time = MAKE_ENDED_TIME_SERIE(MAKE_ABS_DATE(YEAR=2000, MONTH=01, DAY=01), $
      MAKE_ABS_DATE(YEAR=2009, MONTH=12, DAY=31), TIMESTEP= step) ; the time array. Daily values
  endelse  
  
  nb_stat = N_ELEMENTS(file_list)
  nb_days = N_ELEMENTS(time)
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_hist.sav'
  
  for i=0L, nb_stat-1 do begin
  
    ascii_data = READ_ASCII(file_list[i], TEMPLATE=template)
    
    name = FILE_BASENAME(file_list[i], '.dat', /FOLD_CASE)
    id = STRING(ascii_data.stn[0])
    p = where(LONG(ncdc_history.usaf) eq id, cnt)
    lat = ncdc_history.lat[p]*0.001
    lon = ncdc_history.lon[p]*0.001
    elev= ncdc_history.elev[p]*0.1
    
    stat_data = NCDC_make_daily_data_struct(name, id, time,lat, lon, elev)
    ascii_tags = tag_names(ascii_data)
    
    date = NCDC_parse_time(ascii_data.yearmoda)
    
    for k = 0, N_ELEMENTS(ascii_tags)-1 do begin
    
      if (ascii_tags[k] ne 'YEARMODA') and (ascii_tags[k] ne 'STN') and (ascii_tags[k] ne 'WBAN') and (ascii_tags[k] ne 'FRSHTT') then begin
        val = NCDC_VAL_from_ASCII(ascii_data, ascii_tags[k])
        f = where(stat_data.tag_list eq ascii_tags[k], cnt)
        if cnt gt 0 then begin
          loc = VALUE_LOCATE(time.qms, date.qms)
          t = where(loc ge 0 and loc lt nb_days-1, cnt)
          if cnt ne 0 then stat_data.values[loc[t], f[0]] = val[t]
          pd = where(loc eq nb_days-1, cnt)
          if cnt ne 0 then stat_data.values[loc[pd[0]], f] = val[pd[0]]          
        endif
      endif
      if (ascii_tags[k] eq 'FRSHTT') then begin
      
        frshtt = ascii_data.FRSHTT
        str = STRARR(N_ELEMENTS(ascii_data.FRSHTT))
        ;  ' 011000'
        res = STRMID(frshtt, 0, 1)
        p = where(res eq 1, cnt)
        if cnt ne 0 then str[p] += 'Fog, '
        res = STRMID(frshtt, 1, 1)
        p = where(res eq 1, cnt)
        if cnt ne 0 then str[p] +='Rain or Drizzle, '
        res = STRMID(frshtt, 2, 1)
        p = where(res eq 1, cnt)
        if cnt ne 0 then str[p] +='Snow or Ice Pellets, '
        res = STRMID(frshtt, 3, 1)
         p = where(res eq 1, cnt)
        if cnt ne 0 then str[p] += 'Hail, '
        res = STRMID(frshtt, 4, 1)
        p = where(res eq 1, cnt)
        if cnt ne 0 then str[p] += 'Thunder, '
        res = STRMID(frshtt, 5, 1)
        p = where(res eq 1, cnt)
        if cnt ne 0 then str[p] += 'Tornado or Funnel Cloud, '
        loc = VALUE_LOCATE(time.qms, date.qms)
        t = where(loc ge 0 and loc lt nb_days-1, cnt)
        if cnt ne 0 then stat_data.weather_ind[loc[t]] = str[t]
        pd = where(loc eq nb_days, cnt)
        if cnt ne 0 then stat_data.weather_ind[loc[pd[0]], f] = str[pd[0]]
      endif
      
    endfor
    
    if (N_ELEMENTS(list_values) eq 0) then list_values = stat_data else list_values = [list_values, stat_data]
    
  endfor
  
  return, list_values
  
end

;+
; 
; :Description:
;       This function parses hourly NCDC ASCII files and creates a data structure containing all
;       values for a given period. This procedure is quite old and should be actualised and tested before use.
;
; :Categories:
;       General/ NCDC 
;       
; :Keywords:   
;     DIR_PATH: in, optional, type=string, default=DIALOG_PICKFILE()
;               Directory containing all *.dat files to read. If not set, a dialog window will open.
;     TPL_PATH: in, optional, type=string, default=DIALOG_PICKFILE()
;               Directory containing the template file. If not set, a dialog window will open.          
;     START_TIME: in, optional, type={ABS_DATE}, default=01.01.2000
;                 The start date of the time period to retrieve.          
;     END_TIME: in, optional, type={ABS_DATE}, default=31.12.2009
;               The end date of the time period to retrieve.
;                     
; :Returns:
;       Data in the form of a structure (see the comment header for more info). 
;       If more than one station is parsed, an array of structures is returned.
;
; :Author:
;       Fabien Maussion, Cornelia Klein::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, CoK 2009.
;       
;       Modified::
;          24-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function NCDC_HOURLY_DATA, DIR_PATH = dir_path, TPL_PATH = tpl, START_TIME = startT, END_TIME = endT

;TODO: a lot of "for" loops to remove  

  ; Set up environment

  compile_opt idl2
  @WAVE.inc
  
  ; RETRIVE files and templates
  if KEYWORD_SET(dir_path) then $
    file_list = FILE_SEARCH(dir_path, '*.dat', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT) $
  else $
    file_list = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file(s) to store', /MULTIPLE_FILES, /FIX_FILTER, /MUST_EXIST)
    
  if N_ELEMENTS(file_list) eq 0 then begin
    print, 'no *.dat files encountered'
    stop
  endif
  
  if KEYWORD_SET(tpl) then $
    tpl_path = tpl $
  else begin
    WAVE_root, root 
    RESTORE, root + '/res/ncdc/hourly.tpl'
  endelse  
    
  file = strsplit(tpl_path, '.', /EXTRACT)
  if(file[n_elements(file)-1] ne 'tpl') then begin
    print, tpl_path, ' is not a *.tpl'
    stop
  endif
  
  RESTORE, tpl_path ; template var is resetted
  
  step = MAKE_TIME_STEP(HOUR = 3)
  
  if KEYWORD_SET(startT) then begin
   time = MAKE_ENDED_TIME_SERIE(startT, endT, TIMESTEP= step) ; the time array. Daily values
  endif else begin
   time = MAKE_ENDED_TIME_SERIE(MAKE_ABS_DATE(YEAR=2000, MONTH=01, DAY=01, HOUR=00), $
            MAKE_ABS_DATE(YEAR=2009, MONTH=12, DAY=31, HOUR=00), TIMESTEP=step) ; the time array. 3-hourly values
  endelse
  
  
  nb_stat = N_ELEMENTS(file_list)
  nb_steps = N_ELEMENTS(time)
  

  
  for i=0L, nb_stat - 1 do begin
  
    ascii_data = READ_ASCII(file_list[i], TEMPLATE=template)
    
    name = FILE_BASENAME(file_list[i], '.dat', /FOLD_CASE)
    id = STRING(ascii_data.USAF[0])
    
    WAVE_root, root 
    RESTORE, root + '/res/ncdc/ncdc_hist.sav'
    
    p = where(LONG(ncdc_history.usaf) eq id, cnt)
    lat = ncdc_history.lat[p]*0.001
    lon = ncdc_history.lon[p]*0.001
    elev= ncdc_history.elev[p]*0.1
    
    stat_data = NCDC_make_hourly_data_struct(name, id, time,lat, lon, elev)
    ascii_tags = tag_names(ascii_data)
    
    
    if (N_ELEMENTS(name_list) eq 0) then name_list = name else name_list = [name_list, name]
    if (N_ELEMENTS(id_list) eq 0) then id_list = id else id_list = [id_list, id]
    
    for j=0L, N_ELEMENTS(ascii_data.YRMODAHRMN) - 1 do begin
    
      date = NCDC_parse_htime(LONG64(ascii_data.YRMODAHRMN[j]))
            
      p = WHERE(time.qms eq date.qms, cnt)
      
      if cnt ne 0 then begin
      
      for k = 0, N_ELEMENTS(ascii_tags)-1 do begin
        
          if (ascii_tags[k] eq 'PCP06') or (ascii_tags[k] eq 'PCP24') or (ascii_tags[k] eq 'TEMP') or (ascii_tags[k] eq 'SPD') or (ascii_tags[k] eq 'DIR') or (ascii_tags[k] eq 'DEWP')or (ascii_tags[k] eq 'STP') then begin
            val = NCDC_VAL_from_ASCII(ascii_data, ascii_tags[k], j)
            f = where(stat_data.tag_list eq ascii_tags[k])
            if f ne -1 then stat_data.values[p, f] = val
            endif
        end
      endif
    endfor      
      
     if (N_ELEMENTS(list_values) eq 0) then list_values = stat_data else list_values = [list_values, stat_data]            

      
    endfor
          
   return, list_values 
    
end



