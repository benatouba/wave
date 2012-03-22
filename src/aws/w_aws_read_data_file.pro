; docformat = 'rst'
;+
;  The purpose of this function is to parse automatically an ASCII file containing
;  AWS data. The files to parse must follow certain rules.
;  
;  Comment lines start with a % and are ignored by the parsing routine.
;  The first line is allways ignored.
;  Three lines are ALLWAYS required, in this order: varnames, units, type.
;  If the column type is not understood, the default FLOAT is used. 
;  One column must contain the time information. Default is to look for a tag
;  named 'TIMESTAMP' but other tags can be specified.
;  
;  Here an example of file header that would work just fine::
;  
;        "TOA5","AWS1","CR1000","22314","CR1000.Std.16","CPU:ECS1_VCDef.CR1","10680","Control"
;        "TIMESTAMP","RECORD","Bat12V","Panels","CaseT"
;        "TS","RN","V","V","DegC"
;        "","","Smp","Smp","Smp"
;        "2009-09-30 07:50:00",0,12.95,0,18.23
;        "2009-09-30 08:00:00",1,12.94,0,15.39
;        ...
;
;    or a second example::
;    
;        % AWS1, corrected. File created with IDL. See metadata for more info. Careful:  Careful: time in Beijing time (UTC+8).
;        "TIMESTAMP","SR50","SR50_QUAL","TEMP_2M"
;        "-","cm","-","C"
;        "string","float","long","float"
;        "2009-04-27 00:10:00",107.565,172,-8.762
;        "2009-04-27 00:20:00",107.559,168,-8.861
;        ...
;        
; :Categories:
;    aws
;    
; :Examples:
;    
;
; :Author:
;       Fabien Maussion
;
; :History:
;     Change History::
;        Written by: FaM, 2011
;        
;-
;
;+
; :Private:
; 
; :Description:
;    Similiar to IDL's ASCII_TEMPLATE, but automatic. The AWS file must have a specific format.
;    
;        
; :Params:
;    file: the file to template
;
; :Returns:
;    an IDL ASCII template structure
;
;-
function w_aws_read_data_file_auto_template, file

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  on_Error, 2
    
  OPENR, lun, file, /GET_LUN
  line = ''
  k = 0LL
  m = 0LL
  stop_header = FALSE
  station_name = ''
  station_des = ''
  station_locX = 0.
  station_locY = 0.
  station_h = 0.
  data_val = 'POINT'
  is_ts = FALSE
  readf, lun, line ; ignore the first line
  while ~stop_header do begin
    readf, lun, line
    k += 1
    if (BYTE(line))[0] eq BYTE('%') then begin
      l = STRSPLIT(line, ':', /EXTRACT, /PRESERVE_NULL)
      if str_equiv(l[0]) eq '% STATION_NAME' then station_name = GEN_strtrim(l[1], /ALL)
      if str_equiv(l[0]) eq '% STATION_DESCRIPTION' then station_des = GEN_strtrim(l[1], /ALL)
      if str_equiv(l[0]) eq '% STATION_LOCATION' then begin
        l = STRSPLIT(l[1], ';', /EXTRACT, /PRESERVE_NULL)
        station_locX = DOUBLE(utils_replace_string(l[0], 'Longitude', ''))
        station_locY = DOUBLE(utils_replace_string(l[1], 'Latitude', ''))
        station_h = FLOAT(utils_replace_string(l[2], 'Altitude', ''))
      endif
      if str_equiv(l[0]) eq '% DATA_VALIDITY' then data_val = GEN_strtrim(l[1], /ALL)
      if str_equiv(l[0]) eq '% FILE_FORMAT' then is_ts = TRUE
      continue
    endif
    line = utils_replace_string(line, ',:', ':')
    els = utils_replace_string(STRSPLIT(line, ',' ,/EXTRACT, /PRESERVE_NULL), '"', '')
    if m eq 0 then var_names = els
    if is_ts then begin
      if m eq 1 then var_des = els
      if m eq 2 then var_units = els
      if m eq 3 then var_type = els
      if m ge 4 and AWS_time_is_valid(els[0]) then stop_header = TRUE
    endif else begin
      if m eq 1 then var_units = els
      if m eq 2 then var_type = els
      if m ge 3 and AWS_time_is_valid(els[0]) then stop_header = TRUE
    endelse
    m += 1
  endwhile
  FREE_LUN, lun
  var_names = utils_replace_string(var_names, '(', '_')  
  var_names = utils_replace_string(var_names, ')', '_')  
  nf = N_ELEMENTS(var_names)
  if nf ne N_ELEMENTS(var_units) then message, 'Units and variables do not match.'
  if nf ne N_ELEMENTS(var_type) then message, 'Types and variables do not match.'
  types = LONARR(nf)  
  for i = 0, nf-1 do begin
     strs = ['-'       ,''        , 'string'  ,'long'  ,'float'  ,'int'   ,'integer','double']
     idlt = [IDL_STRING,IDL_STRING,IDL_STRING,IDL_LONG,IDL_FLOAT,IDL_LONG,IDL_LONG ,IDL_DOUBLE]
     p = where(str_equiv(strs) eq str_equiv(var_type[i]), cnt)
     if cnt eq 1 then  types[i] = idlt[p] else types[i] = IDL_FLOAT  
  endfor
  
  field_locations = lonarr(nf)
  fpos = 0L

  bline = [byte(line), 32b]
  nptr = where(bline eq (byte(','))[0], ncount)
  if (ncount eq 0) then fptr = [-1, n_elements(bline)] else fptr = [-1, nptr, n_elements(bline)]
  add = [1,1]
  for j=0, nf-1 do field_locations[j] = fptr[j] + add[0]
  
  if N_ELEMENTS(var_des) eq 0 then var_des = STRARR(nf)
  
  template = { $
    version:            1.0, $
    dataStart:          k, $
    delimiter:          44B, $
    missingValue:       !VALUES.F_NAN, $
    commentSymbol:      '', $
    fieldCount:         nf, $
    fieldTypes:         types, $
    fieldNames:         str_equiv(var_names), $
    fieldUnits:         str_equiv(var_units), $
    station_name:       station_name, $
    station_des:        station_des, $
    station_locX:       station_locX, $
    station_locY:       station_locY, $
    station_h:          station_h, $
    data_val:           data_val, $
    is_ts:              is_ts, $
    fieldDes:           var_des, $
    fieldLocations:     field_locations, $
    fieldGroups:        LINDGEN(nf) $
    }
     
  return, template
  
end

;+
; :Private:
; 
; :Description:
;    Reads the timestamp field from an AWS ascii file and turns it into a QMS time.
;    FORMAT: "2009-04-26 04:10:00"
;
; :Params:
;    stimes: in, required, type = string
;            the campbell string to parse
; 
; :Returns:
;     the time in QMS
;-
function w_aws_read_data_file_time, stimes

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  on_error, 2
  
  if ~arg_okay(stimes, TYPE = IDL_SRING) then Message, WAVE_Std_Message('stimes', /STRING)
  
  isthere=WHERE(BYTE(stimes[0]) EQ ((BYTE('"'))[0]), count)
  if count eq 0 then s = 0 else s = 1
    
  return, QMS_TIME(YEAR=STRMID(stimes,0+s,4), MONTH=STRMID(stimes,5+s,2),DAY=STRMID(stimes,8+s,2), $
    HOUR=STRMID(stimes,11+s,2),MINUTE=STRMID(stimes,14+s,2),SECOND=STRMID(stimes,17+s,2))

end

;+
; :Description: 
;    This function reads an AWS ascii file and returns a `w_ts_Station` object
;    containing the the parsed data. The ASCII file must respect certain rules 
;    (see the file documentation)
;
; :Keywords:    
;    FILE: in, optional, type = string
;          the path of file to parse (if not set, a dialog window will open)
;    DELTA_QMS: in, optional, type = qms
;               the delta in qms to apply to the read time (for example, 
;               for UTC+8: DELTA_QMS = (MAKE_TIME_STEP(HOUR = 8)).dms)
;    STATION_OBJ: in, optional, type = w_ts_Station
;                 a station object to add the variables to. If not given, a new object is created
;    TIMETAG: in, optional, string, default = 'TIMESTAMP'
;             the tag corresponding to the time column
;    IGNORETAG: in, optional, string, default = 'RECORD'
;               a string (or array of string) of tags to ignore from the ASCII file.
;               Default is to ignore the tag 'RECORD', set IGNORETAG to '' to prevent this
;  :Returns:
;    a `w_ts_Station` object
;  
;-
function w_aws_read_data_file, FILE=file, DELTA_QMS=delta_qms, STATION_OBJ=station_obj, TIMETAG=timetag, IGNORETAG=ignoretag

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  on_error, 2
  
  ; Check input
  if N_ELEMENTS(file) eq 0 then file = DIALOG_PICKFILE(TITLE='Please select the ASCII data file to read', /FIX_FILTER, /MUST_EXIST)
  if ~QUERY_ASCII(file) then MESSAGE, WAVE_Std_Message(/FILE) 
  if N_ELEMENTS(TIMETAG) ne 0 then _timetag = timetag  else _timetag = 'TIMESTAMP'
  if ~ arg_okay(_timetag, TYPE=IDL_STRING, /SCALAR) then MESSAGE, WAVE_Std_Message('TIMETAG', /ARG)  
  if N_ELEMENTS(IGNORETAG) ne 0 then _ignoretag = ignoretag  else _ignoretag = 'RECORD'
  if ~arg_okay(_ignoretag, TYPE=IDL_STRING) then MESSAGE, WAVE_Std_Message('IGNORETAG', /ARG)
    
  ; Parse the file  
  template = w_aws_read_data_file_auto_template(file)  
  ascii_data = read_ascii(file, TEMPLATE=template)  
  n = n_tags(ascii_data)
  tags = tag_names(ascii_data)
  
  ; Read the time
  pt = where(str_equiv(tags) eq _timetag, cnt)
  if cnt eq 0 then message, 'The ASCII file does not contain a time tag corresponding to ' + _timetag  
  time = w_aws_read_data_file_time(ascii_data.(pt))
  nt = N_ELEMENTS(time)  
  if N_ELEMENTS(delta_qms) ne 0 then begin
   if arg_okay(delta_qms, /SCALAR, /NUMERIC) then time = time + delta_qms else Message, WAVE_Std_Message('DELTA_QMS', /ARG) 
  endif
  
  ; Where to put the data
  if N_ELEMENTS(STATION_OBJ) eq 0 then begin
    if TEMPLATE.is_ts then begin
      _station_obj = OBJ_NEW('w_ts_Station', DESCRIPTION=template.station_des, ELEVATION=template.station_h, $
                     LOC_X=template.station_locX, LOC_Y=template.station_locY, NAME=template.station_name)
    endif else begin
      _station_obj = OBJ_NEW('w_ts_Station')
    endelse
  endif else begin
    if ~OBJ_VALID(station_obj) and ~OBJ_ISA(station_obj, 'w_ts_Station') then Message, WAVE_Std_Message('STATION_OBJ', /ARG)
    _station_obj = station_obj
  endelse
  
  ; Go through all tags now 
  for i=0,n-1 do begin
    tag = tags[i]
    if str_equiv(tag) eq str_equiv(_timetag) then continue
    pi = where(str_equiv(_ignoretag) eq str_equiv(tag), cnti)
    if cnti ne 0 then continue
    ;create new data object and add it to the station
    _var = OBJ_NEW('w_ts_Data', ascii_data.(i), time, NAME=tag, UNIT=template.fieldUnits[i], $
                      DESCRIPTION=template.fieldDes[i], VALIDITY=template.data_val)
    _station_obj->addVar, _var    
  endfor  
  
  return, _station_obj
  
end