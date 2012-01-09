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
; :History:
;       Written by FaM, 2009.
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
; :History:
;       Written by FaM, CoK 2009.
;-
function w_ncdc_read_gsod_file, FILE = file

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  ; RETRIEVE file and template

  if N_ELEMENTS(file) eq 0 then FILE=DIALOG_PICKFILE(TITLE='Please select NCDC file to read', /MUST_EXIST)
  
  if file eq '' then message, 'No file found'
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ascii_template_ncdc_gsod.tpl'
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  
  ascii_data = READ_ASCII(file, TEMPLATE=template)
  
  ascii_tags = tag_names(ascii_data)
  nvals = N_ELEMENTS(ascii_tags)
  
  date = w_ncdc_read_gsod_file_parse_time(ascii_data.yearmoda)+D_QMS ;NCDC mean time issue
  
  nb_entries=N_ELEMENTS(date)
  if nb_entries eq 0 then message, 'No data in file'
  
  stn = ascii_data.stn  
  for k = 0, nvals-1 do begin    
    val = w_ncdc_read_gsod_file_parse_val_from_ascii(ascii_data.(k), ascii_tags[k])  
    if arg_okay(val,TYPE=IDL_STRING) then continue
    if (N_ELEMENTS(stat_vars) eq 0) then stat_vars = ptr_new(val) else stat_vars = [stat_vars, ptr_new(val)]
  endfor
  
  undefine, ascii_data
  
  ids = stn[UNIQ(stn, SORT(stn))]
  nstats = N_ELEMENTS(ids)  
  
  for i=0, nstats-1 do begin
  
    id = ids[i]    
    s=where(NCDC_HISTORY.usaf eq id, cnt)    
    if cnt eq 0 then Message, id + ' not found, please update your NCDC history file!'   
       
    lat = ncdc_history.lat[s]*0.001
    lon = ncdc_history.lon[s]*0.001
    elev= ncdc_history.elev[s]*0.1
    sname = ncdc_history.name[s]        
    p = where(stn eq id)        
    _t = date[p]
    
    un = uniq(_t, sort(_t))
    if N_ELEMENTS(un) ne N_ELEMENTS(_t) then print, sname, ' ' , str_equiv(id), ' not unique'
    _t = _t[un]
    
    for j=0, N_ELEMENTS(stat_vars)-1 do begin    
      _var = *(stat_vars[j])
      _d = (_var.data[p])[un]
      var = w_ts_MakeData(_d, _t, NAME=_var.vname, DESCRIPTION=_var.description, UNIT=_var.unit, $
                           VALID='INTERVAL', AGG_METHOD=_var.agg_method, MISSING=_var.missing)        
      if j eq 0 then variables=var else variables = [variables, var]      
    endfor
    
    ncdc_station=w_ts_MakeStation(variables, NAME=sname, $ ; The name of the station
      ID=id, $ ; Station ID
      DESCRIPTION='NCDC Station', $ ; A short description of the station
      ELEVATION=elev, $ ; altitude in m
      LOC_X=lon, $ ; X location in SRC
      LOC_Y=lat )
      
    if (N_ELEMENTS(list_values) eq 0) then list_values = ncdc_station else list_values = [list_values, ncdc_station]
    
  endfor
  
  undefine, stat_vars
  return, list_values
  
end

