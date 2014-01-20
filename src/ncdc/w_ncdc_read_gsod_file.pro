;+
; 
; Parses Global Summary of the Day (GSOD) NCDC ascii files. 
; The files must have the same data format as downloaded from the NCDC website
; (`http://www.ncdc.noaa.gov/oa/climate/climatedata.html#daily`) 
; or as generated by the 'w_ncdc_extract_gsod' routine. 
;       
; The `w_ncdc_read_gsod_file` function will parse all gsod-*.dat files in a 
; directory or one selected file, and gets additional info
; such has lat and lon or elevation from the resource history file.
;       
; The output of the function is an instance of `w_ts_statset` if a directory is 
; given as argument, or a `w_ts_station` instance if one single file is read.
;       
; :Author:
;   FaM, CoK
;  
;-
        
;+
; 
; :Description:
;       This function is called internally. It is called to retrieve the value in a NCDC File
;       and convert it to the right format and unit.
;       
; :Private:
;
; :Params:
;     ascii_data: in, type=struct, default=none
;                 the data of the NCDC file.
;     ascii_tag: in, optional, type=string, default=none
;                the  tag(s) of the value(s) to retrieve of the NCDC file.
; 
; :Keywords:
;     prcp_filter: in, type=long, default=0
;                  level of filtering of the prcp data.
;                  0 = no filtering (default)
;                  1 = accepting only D, F, G, I flags (ok quality)
;                  2 = accepting only D, F, G flags (max quality)
;                  (see: http://www1.ncdc.noaa.gov/pub/data/gsod/readme.txt)
;       
; :Returns:
;       The corrected value(s) of the NCDC file.
;
;-
function w_ncdc_read_gsod_file_parse_val_from_ascii, data, ascii_tag, PRCP_FILTER=prcp_filter

  ; Set up environment
  compile_opt idl2
  @WAVE.inc
    
  val = '' 
  missing = !VALUES.F_NAN
  
  CASE ascii_tag OF
  
    'TEMP': begin
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Mean temperature', missing:missing, agg_method:'MEAN'}
    end
    
    'TEMP_COUNT': begin
      data = FLOAT(data)
      val = {data:data, vname:ascii_tag, unit:'-', description:'Number of daily Temp measurements', missing:missing, agg_method:'MEAN'}
    end
    
    'DEWP': begin
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Mean dew point', missing:missing, agg_method:'MEAN'}
    end
        
    'MAX': begin
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Maximum temperature', missing:missing, agg_method:'MAX'}
    end
    
    'MIN': begin
      data = (data - 32.0) * 5.0/9.0 ; to celcius
      p = where(data gt 50 or data lt -80, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'degC', description:'Minimum temperature', missing:missing, agg_method:'MIN'}
    end
    
    'DIR': begin
      data = float(data)
      p = where(data gt 360 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'deg', description:'Wind direction', missing:missing, agg_method:'MEAN'}   ; tag for hourly only
    end
    
    'VISIB': begin
      data = data * 1.609344  ; to km
      p = where(data gt 999 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'km', description:'Mean visibility', missing:missing, agg_method:'MEAN'}
    end
    
    'WDSP': begin
      data = data * 0.514 ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Mean wind speed', missing:missing, agg_method:'MEAN'}
    end
    
    'WDSP_COUNT': begin
      data = FLOAT(data)
      val = {data:data, vname:ascii_tag, unit:'-', description:'Number of daily WDSP measurements', missing:missing, agg_method:'MEAN'}
    end
    
    'MXSPD': begin
      data = data * 0.514 ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Maximum sustained wind speed', missing:missing, agg_method:'MAX'}  
    end
    
    'GUST': begin
      data = data * 0.514 ; to m/s
      p = where(data gt 90 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'m/s', description:'Maximum wind gust', missing:missing, agg_method:'MAX'} 
    end
    
    'PRCP': begin
      flag = strmid(data, 0, 1, /REVERSE_OFFSET)
      data = float(data)
      data = data * 25.4 ; to mm
      p = where(data gt 2500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      case (prcp_filter) of
        0:
        1: begin
          pyes = where((flag eq 'D') or (flag eq 'F') or (flag eq 'G') or (flag eq 'I'), cntyes, COMPLEMENT=p, NCOMPLEMENT=cnt)
          if cnt gt 0 then data[p] = missing
        end
        2: begin
          pyes = where((flag eq 'D') or (flag eq 'F') or (flag eq 'G'), cntyes, COMPLEMENT=p, NCOMPLEMENT=cnt)
          if cnt gt 0 then data[p] = missing
        end
        else: Message, 'Prcp filter not valid'
      endcase    
      val = {data:data, vname:ascii_tag, unit:'mm/d', description:'Precipitation (rain/melted snow)', missing:missing, agg_method:'MEAN'} 
    end
           
    'SNDP': begin
      data = data * 2.54 * 10 ; to mm
      p = where(data gt 500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'mm', description:'Snow depth', missing:missing, agg_method:'MEAN'} 
    end  
    
    'STP': begin
      p = where(data gt 1500 or data lt 0, cnt)
      if cnt gt 0 then data[p] = missing
      val = {data:data, vname:ascii_tag, unit:'hPa', description:'Mean station pressure', missing:missing, agg_method:'MEAN'} 
    end    
    
    'FRSHTT': begin
      agg_method = 'MEAN'
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
      frshtt = data
      data = FLTARR(N_ELEMENTS(data)) ;  ' 011000'
      res = STRMID(frshtt, 2, 1)
      p = where(res eq 1, cnt)
      if cnt ne 0 then data[p] = 1
      res = STRMID(frshtt, 3, 1)
      p = where(res eq 1, cnt)
      if cnt ne 0 then data[p] = 1
      val = {data:data, vname:'SNOW', unit:'', description:'Frozen prcp: yes or no', missing:missing, agg_method:agg_method} 
    end    

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
; :Private:
;
; :Params:
;     yearmoda: in, type=long, default=none
;               Date of the NCDC file in the form YRMODA.
;           
;  :Returns:
;       The time in qms
;
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
;     DIRECTORY: in, optional, type=string
;                The directory containing all ASCII files to read. The file names must 
;                be 'gsod-*.dat'. Appropriate GSOD files can be created with the
;                'w_ncdc_extract_gsod' procedure. 
;                Either FILE or DIRECTORY must be set.
;     VERBOSE: in, optional, type=boolean, default=1
;              if the routine has to talk while doing things. Default is yes,
;              so set to zero to avoid this
;     KEEP_VARS: in, optional, type=string
;                array of variable names that have to be kept for each station
;     REMOVE_VARS: in, optional, type=string
;                  array of variable names that have to be removed for each station
;     PRCP_FILTER: in, type=long, default=0
;                  level of filtering of the prcp data.
;                  0 = no filtering (default)
;                  1 = filtering of A, B, C, E, H
;                  2 = filtering of A, B, C, E, H, I (max quality)
;                  (see: http://www1.ncdc.noaa.gov/pub/data/gsod/readme.txt)   
;                   
; :Returns:
;       
; :Examples:
;
;
; :History:
;       Written by FaM, CoK, 2012.
;-
function w_ncdc_read_gsod_file, FILE=file, $
    DIRECTORY=directory, $
    VERBOSE=verbose, $
    KEEP_VARS=keep_vars, $
    REMOVE_VARS=remove_vars, $
    PRCP_FILTER=prcp_filter
    
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
  if N_ELEMENTS(VERBOSE) eq 0 then verbose=1
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ascii_template_ncdc_gsod.tpl'
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  
  SetDefaultValue, prcp_filter, 0
    
  stat_val=0L
  if N_ELEMENTS(DIRECTORY) ne 0 then file_list=FILE_SEARCH(directory, '*gsod-*.dat', count=filecnt)
  if (N_ELEMENTS(FILE) ne 0) then begin
  if STRMATCH(FILE_BASENAME(FILE), '*gsod-*.dat', /FOLD_CASE) then $
  file_list=file
  endif
  filecnt = N_ELEMENTS(file_list)
  
  if filecnt eq 0 then message, 'No file(s) found.'
  
  if filecnt gt 1 then station_list = Obj_New('w_ts_StatSet')
  
  for t=0, filecnt-1 do begin
    
    ascii_data = READ_ASCII(file_list[t], TEMPLATE=template)
    
    ascii_tags = tag_names(ascii_data)
    nvals = N_ELEMENTS(ascii_tags)
    
    date = w_ncdc_read_gsod_file_parse_time(ascii_data.yearmoda) + D_QMS ; NCDC mean time issue
    
    nb_entries=N_ELEMENTS(date)
    if nb_entries le 2 then continue 
    
    stat_val=+1
        
    for k = 0, nvals-1 do begin
      val = w_ncdc_read_gsod_file_parse_val_from_ascii(ascii_data.(k), ascii_tags[k], PRCP_FILTER=prcp_filter)
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
      if verbose then print, sname + ' ' + id_str + '. ' + str_equiv(filecnt-1-t) + ' left.'
      
      ncdc_station = OBJ_NEW('w_ts_Station',NAME=sname, $ ; The name of the station
        ID=id_str, $ ; Station ID
        DESCRIPTION='NCDC Station', $ ; A short description of the station
        ELEVATION=elev, $ ; altitude in m
        LOC_X=lon, $ ; X location in SRC
        LOC_Y=lat )
        
      for j=0, N_ELEMENTS(stat_vars)-1 do begin
        _var = *(stat_vars[j])
        _d = (_var.data[p])[un]
        if N_ELEMENTS(KEEP_VARS) ne 0 then begin
          ok=where(str_equiv(keep_vars) eq str_equiv(_var.vname), cnt)
          if cnt eq 0 then continue
        endif
        if N_ELEMENTS(REMOVE_VARS) ne 0 then begin
          ok=where(str_equiv(remove_vars) eq str_equiv(_var.vname), cnt)
          if cnt ne 0 then continue
        endif
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
  
  station_list->setPeriod
  return, station_list
  
end

