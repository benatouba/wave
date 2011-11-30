; docformat = 'rst'
;+
;
;  This bundle of procedures is a tool set available to the WAVE user for reading 
;  ASCII files from Campbell loggers, aggregate the data, fill data gaps, etc. 
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Written by FaM, 2011.
;     
;-


;+
; :Description:   
;    This is to initialise the two named structures {W_VAR} and {W_AWS}.
;   
; :History:
;     Written by FaM, 2011.
;
;-
pro AWS_Init

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

end

;+
; :Description:
;    Reads the timestamp field from an AWS ascii file and turns it into a QMS time.
;       It also works with vectors, it can last a few seconds if the time serie is long.
;
; :Params:
;    stimes: in, required, type = string
;            the campbell string to parse
;
;
; :History:
;     Written by FaM, 2010.
;
;-
function AWS_parse_logger_time, stimes

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

function AWS_time_is_valid, stimes

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    RETURN, 0
  ENDIF 
  
  if ~arg_okay(stimes, TYPE = IDL_SRING) then Message, WAVE_Std_Message('stimes', /STRING)
  
  isthere=WHERE(BYTE(stimes[0]) EQ ((BYTE('"'))[0]), count)
  if count eq 0 then s = 0 else s = 1
  dummy = QMS_TIME(YEAR=STRMID(stimes,0+s,4), MONTH=STRMID(stimes,5+s,2),DAY=STRMID(stimes,8+s,2), $
    HOUR=STRMID(stimes,11+s,2),MINUTE=STRMID(stimes,14+s,2),SECOND=STRMID(stimes,17+s,2))
    
  return, 1

end

;+
; :Description:
;    This function reads an AWS ascii file and returns a structure containing
;    the parsed data. The file must previously having been templated (see
;    'utils_make_template'). The template MUST contain ONE AND ONLY ONE
;    file named "TIMESTAMP" for the time string parsing. 
;    
;    the structure contains the new tags: "time" and "nt". The other fields
;    are simply read from the template.
;
;
; :Keywords:
;    FILE_PATH: in, optional, type = string
;               the file to parse (if not set, dialog window)
;    TPL_path: in, optional, type = string
;               the file template. The procedure looks for a template of 
;               the same name as the file. If not found, dialog window.
;    DELTA_QMS: in, optional, type = qms
;               the delta in qms to apply to the read time (for example, 
;               for UTC+8: DELTA_QMS = (MAKE_TIME_STEP(HOUR = 8)).dms)
;
;
; :History:
;     Written by FaM, 2010.
;
;-
function AWS_parse_file, FILE_PATH = file_path, TPL_path = TPL_PATH, DELTA_QMS = delta_qms

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ; Data file first
  if ~KEYWORD_SET(file_path) then file_path = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file to read', /FIX_FILTER, /MUST_EXIST)
  
  ; TPL File then
  if ~KEYWORD_SET(TPL_PATH) then begin
    ; Is it in the same directory?
    GEN_str_subst, ret, file_path, '.dat', '.tpl', TPL_path
    test = FILE_INFO(TPL_path)
    ; if not then ask
    if test.exists eq 0 then TPL_path = DIALOG_PICKFILE(FILTER='*.tpl', TITLE='Please select corresponding template', /FIX_FILTER, /MUST_EXIST)
  endif
    
  RESTORE, tpl_path ; template var is resetted
  ascii_data = READ_ASCII(file_path, TEMPLATE=template)
  
  n = n_tags(ascii_data)
  names = tag_names(ascii_data)
  
  foundtimes = FALSE
  
  for i=0,n-1 do begin ; Go threw all infos
  
    case size(ascii_data.(i),/TYPE) of
    ; check for strings
    7 : begin ; If this is a string, it must be time. Otherwize, we don't now how to handle it.
          
          if str_equiv(names[i]) eq 'TIMESTAMP' then begin
          
            time = AWS_parse_logger_time(ascii_data.(i))
            nt = N_ELEMENTS(time)
            
            if KEYWORD_SET(delta_qms) then time = time + delta_qms

            ;create new structure/ add entry to existing structure
            if n_elements(ostr) eq 0 then ostr = create_struct('time',time,'nt',nt) $
            else  ostr = create_struct(ostr,'time',time,'nt',nt)
            
            foundtimes = TRUE
            
          endif
          
        end
        
    ; check for all other data types
    else: begin
            ;create new structure/ add entry to existing structure
            if n_elements(ostr) eq 0 then ostr = create_struct(names[i],ascii_data.(i)) $
            else  ostr = create_struct(ostr,names[i],ascii_data.(i))
          end
    endcase

  ; End of Loop
  endfor
  
  if foundtimes eq FALSE then message, 'The given structure did not contain a TIMESTAMP field. I would be happy to find one'
  
  return, ostr

end


;+
; :Description:
; 
;    Similiar to IDL's ASCII_TEMPLATE, but automatic. The AWS file must have a specific format.
;    Commented lines start with a %. The first line is allways ignored.
;    All the other lines are required, in this order: varnames, units, type. 
;    If type is not understood, the default FLOAT is used.The field "TIMESTAMP" 
;    is required, all the other fields are flexible. Here an example::
;    
;        % AWS1, corrected. File created with IDL. See metadata for more info. Careful:  Careful: time in Beijing time (UTC+8).
;        "TIMESTAMP","SR50","SR50_QUAL","TEMP_2M"
;        "-","cm","-","C"
;        "string","float","long","float"
;        "2009-04-27 00:10:00",107.565,172,-8.762
;        "2009-04-27 00:20:00",107.559,168,-8.861
;        ....
;        
;        
;    or a second example::
;        
;        "TOA5","AWS1","CR1000","22314","CR1000.Std.16","CPU:ECS1_VCDef.CR1","10680","Control"
;        "TIMESTAMP","RECORD","Bat12V","Panels","CaseT"
;        "TS","RN","V","V","DegC"
;        "","","Smp","Smp","Smp"
;        "2009-09-30 07:50:00",0,12.95,0,18.23
;        "2009-09-30 08:00:00",1,12.94,0,15.39
;        
; :Keywords:
;    FILE_PATH: the file to template (*.dat)
;
; :Returns:
;    an IDL ASCII template 
;    
; :History:
;     Written by FaM, 2011.
;
;
;-
function AWS_auto_template, FILE_PATH = file_path

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ; Data file first
  if ~KEYWORD_SET(file_path) then file_path = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file to template', /FIX_FILTER, /MUST_EXIST)
  
  OPENR, lun, file_path, /GET_LUN
  line = ''
  k = 0LL
  m = 0LL
  stop_header = FALSE
  readf, lun, line ; ignore the first line
  while ~stop_header do begin
     readf, lun, line
     k += 1 
     if (BYTE(line))[0] eq BYTE('%') then continue
     line = utils_replace_string(line, ',:', ':') 
     els = utils_replace_string(STRSPLIT(line, ',' ,/EXTRACT, /PRESERVE_NULL), '"', '')     
     if m eq 0 then var_names = els
     if m eq 1 then var_units = els
     if m eq 2 then var_type = els
     if m ge 3 and AWS_time_is_valid(els[0]) then stop_header = TRUE
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
     strs = [''        , 'string'  ,'long'  ,'float'  ,'int'   ,'integer','double']
     idlt = [IDL_STRING,IDL_STRING,IDL_LONG,IDL_FLOAT,IDL_LONG,IDL_LONG ,IDL_DOUBLE]
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
    fieldLocations:     field_locations, $
    fieldGroups:        LINDGEN(nf) $
   }
     
  return, template
  
end

;+
; :Description:
; 
;    This function reads an AWS ascii file and returns a structure containing
;    the parsed data. The structure contains the new tags: "time" and "nt". 
;    The other fields are simply read from the file header (see #AWS_auto_template).
;
;
; :Keywords:
;    FILE_PATH: in, optional, type = string
;               the file to parse (if not set, dialog window)
;    DELTA_QMS: in, optional, type = qms
;               the delta in qms to apply to the read time (for example, 
;               for UTC+8: DELTA_QMS = (MAKE_TIME_STEP(HOUR = 8)).dms)
;
;
; :History:
;     Written by FaM, 2010.
;
;-
function AWS_parse_file_auto, FILE_PATH = file_path, DELTA_QMS = delta_qms

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ; Data file first
  if ~KEYWORD_SET(file_path) then file_path = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file to read', /FIX_FILTER, /MUST_EXIST)
  
  template = AWS_auto_template(FILE_PATH = file_path)
  
  ascii_data = READ_ASCII(file_path, TEMPLATE=template)
  
  n = n_tags(ascii_data)
  names = tag_names(ascii_data)
  
  foundtimes = FALSE
   
  for i=0,n-1 do begin ; Go threw all infos
  
    case size(ascii_data.(i),/TYPE) of
    ; check for strings
    7 : begin ; If this is a string, it must be time. Otherwise, we don't now how to handle it.
          
          if str_equiv(names[i]) eq 'TIMESTAMP' then begin
            time = AWS_parse_logger_time(ascii_data.(i))
            nt = N_ELEMENTS(time)
            
            if KEYWORD_SET(delta_qms) then time = time + delta_qms

            ;create new structure/ add entry to existing structure
            if n_elements(ostr) eq 0 then ostr = create_struct('time',time,'nt',nt) $
            else  ostr = create_struct(ostr,'time',time,'nt',nt)
            foundtimes = TRUE
          endif
        end
        
    ; check for all other data types
    else: begin
            ;create new structure/ add entry to existing structure
            if n_elements(ostr) eq 0 then ostr = create_struct(names[i],ascii_data.(i)) $
            else  ostr = create_struct(ostr,names[i],ascii_data.(i))
            if N_ELEMENTS(units) eq 0 then units=template.fieldUnits[i] else units=[units,template.fieldUnits[i]] 
          end
    endcase

  ; End of Loop
  endfor
 
  if foundtimes eq FALSE then message, 'The given structure did not contain a TIMESTAMP field. I would be happy to find one'
  
  ostr = create_struct(ostr,'unit',units)
  
  return, ostr

end


;+
; :Description:
;    This functions crops an AWS structure (see 'AWS_parse_file'), looks
;    for the "time" tags and crops the structure between selected times.
;    The all the fields are actualized in the returned structure.
;
; :Params:
;    struct: in, required
;            AWS structure to crop
;
; :Keywords:
;    t0: in, optional, type = {ABS_DATE}/qms
;        the first time in the croped structure (default=first available time)
;    t1: in, optional, type = {ABS_DATE}/qms
;        the last time in the croped structure (default=last available time)
;
; :History:
;     Written by FaM, 2010.
;
;-
function AWS_crop_struct, struct, t0 = t0, t1 = t1

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  n = n_tags(struct)
  names = tag_names(struct)
  for i=0,n-1 do begin ; Go threw all infos
  
    if str_equiv(names[i]) ne 'TIME' then continue
    time = struct.(i)
    
    if KEYWORD_SET(t0) then begin
      if check_WTIME(t0, OUT_QMS= it0) then begin
        p0 = where(time eq it0, cnt)
        if cnt ne 1 then Message, 'T0 not found'
      endif
    endif else p0 = 0
    
    if KEYWORD_SET(t1) then begin
      if check_WTIME(t1, OUT_QMS= it1) then begin
        p1 = where(time eq it1, cnt)
        if cnt ne 1 then Message, 'T1 not found'
      endif
    endif else p1 = N_ELEMENTS(time) - 1
    
  endfor
  
  for i=0,n-1 do begin ; Go threw all infos
  
    if str_equiv(names[i]) eq 'NT' then continue
    if str_equiv(names[i]) eq 'UNIT' then continue
    
    ;create new structure/ add entry to existing structure
    if n_elements(ostr) eq 0 then ostr = create_struct(names[i],(struct.(i))[p0:p1]) $
    else  ostr = create_struct(ostr,names[i],(struct.(i))[p0:p1])
    
  ; End of Loop
  endfor
  
  return, create_struct(ostr,'nt',p1-p0+1)
  
end

;+
; :Description:
;    This function merges two strucutres into one.
;
; :Params:
;    struct1: in, required
;            first AWS structure to merge
;    struct1: in, required
;            second AWS structure to merge
;
; :Keywords:
;
; :History:
;     Written by FaM, 2011.
;
;-
function AWS_merge_struct, struct1, struct2

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  n = n_tags(struct1)
  if N_TAGS(struct2) ne n then message, 'The two structures do not match.'
  names = tag_names(struct1)
  names2 = tag_names(struct2)
  if TOTAL(str_equiv(names[sort(names)]) eq str_equiv(names2[sort(names2)])) ne n then message, 'The two structures do not match.'
    
  for i=0,n-1 do begin ; Go threw all infos
  
    if str_equiv(names[i]) eq 'NT' then continue
    if str_equiv(names[i]) eq 'UNIT' then continue
    j = where(str_equiv(names2) eq str_equiv(names[i]), cnt)
    if cnt ne 1 then message, 'Problem.'
    
    ;create new structure/ add entry to existing structure
    if n_elements(ostr) eq 0 then ostr = create_struct(names[i], [struct1.(i), struct2.(j)]) $
    else  ostr = create_struct(ostr,names[i],[struct1.(i), struct2.(j)])
    
  ; End of Loop
  endfor
  
  ostr =  create_struct(ostr,'NT', N_ELEMENTS(ostr.time))
  return, create_struct(ostr, 'UNIT', struct1.unit)
  
end

;+
; :Description:
;    This interpolates all missing data in a structure
;
; :Params:
;    struct: in, required
;            first AWS structure to repair
;
; :Keywords:
;
; :History:
;     Written by FaM, 2011.
;
;-
function AWS_interp_struct, struct

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  n = n_tags(struct)
  names = tag_names(struct)
  
  it = struct.time
  if check_TimeSerie(it, timestep, FULL_TS = full_ts, IND_MISSING = IND_missing) then begin
     MESSAGE, 'time serie is complete, nothing to do', /INFORMATIONAL
     return, struct     
  end
  
  fnt = N_ELEMENTS(FULL_TS)
  ostr = create_struct('TIME', full_ts)
  
  for i=0,n-1 do begin ; Go threw all infos
  
    if str_equiv(names[i]) eq 'NT' then continue
    if str_equiv(names[i]) eq 'UNIT' then continue
    if str_equiv(names[i]) eq 'TIME' then continue
   
    ;add entry to existing structure
    ostr = create_struct(ostr,names[i], INTERPOL(struct.(i), it, full_ts))
    
  ; End of Loop
  endfor
  
  ostr =  create_struct(ostr,'NT', N_ELEMENTS(ostr.time))
  return, create_struct(ostr, 'UNIT', struct.unit)
  
end

;+
; :Description:
;    Reads a tag from an AWS structure.
;    
;    (TODO: make it mor beautiful)
;
; :Params:
;    struct: in, required, Type=struct
;            the AWS structure to read
;    tag: in, required, Type=str
;         the required tag name
;    data: out
;          the corresponding data
;    time: out
;          the time (qms)
;    unit: out
;          the variable unit
;
;
;
; :Returns:
;
; :History:
;     Written by FaM, 2011.
;
;-
pro AWS_read_tag, struct, tag, data, time, unit
 
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ON_ERROR, 2
  
  ; data
  tags = tag_names(struct)
  p = WHERE(str_equiv(tags) eq str_equiv(tag), cnt)
  if cnt ne 1 then Message, 'Tag not found in structure.'  
  data=struct.(p)
  time=struct.time
  
  ; unit (ugly programming)
  ptr = WHERE(str_equiv(tags) ne str_equiv('NT'), cnt)
  if cnt ne 0 then tags_wt = tags[ptr]
  ptr = WHERE(str_equiv(tags_wt) ne str_equiv('TIME'), cnt)
  if cnt ne 0 then tags_wt = tags_wt[ptr]
  ptr = WHERE(str_equiv(tags_wt) ne str_equiv('UNIT'), cnt)
  if cnt ne 0 then tags_wt = tags_wt[ptr]
  
  p = WHERE(str_equiv(tags_wt) eq str_equiv(tag), cnt)
  unit = struct.unit[p]
  
end

;+
; :Description:
;    Computes the angle of the mast, in degrees (0° = perfectly vertical)
;
; :Params:
;    x: in, required
;       inclinometer angle (degrees)
;    y: in, required
;       inclinometer angle (degrees)
;
; :History:
;     Written by FaM, 2010.
;     
;-
function AWS_mast_angle, x, y
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not array_processing(x, y) then message, WAVE_Std_Message(/ARG)
  
  ;X and Y are in degrees, they should be in RADs. Also, the difference to the vertical should be computed, not the horizontal.  
  alpha = DOUBLE(!pi / 2d - ABS(x) * !pi / 180d )
  beta = DOUBLE(!pi / 2d - ABS(y) * !pi / 180d )
  
  angle = ABS(atan((tan(alpha)*tan(beta))/ SQRT(tan(alpha)^2+tan(beta)^2)) * 180d / (!pi))
  
  return, 90d - angle ; Also the same, the difference to vertical should be computed.
     
end


;+
; :Description:
;    Routine to perform the basic corrections on a SR50 time serie.
;
; :Params:
;    distance: in, required
;              the sr50 distance to ground 
;    time: in, required
;          the time
;    x: in, required
;       the inclinometer X angle
;    y: in, required
;       the inclinometer Y angle
;    airtemp: in, required
;             the air temperature
;    quality: in, required
;             the sr50 quality flag (if not known, just set to LONG(SR50 * 0 + 1))
;    corrected: out
;               the corrected sr50 Time serie (possibly non-regular)
;    new_time: out
;              the associated Time serie (possibly non-regular)
;    out_qual: out
;              the associated quality (possibly non-regular)
;
; :Keywords:
;    NO_TEMP_COR: in
;                 set this keyword if you do not want to perform temperature correction
;    NO_ANGLE_COR: in
;                 set this keyword if you do not want to perform mast angle correction
;    CORRECTED_NAN: out
;                   the corrected sr50 Time serie (same size as input) with NANs where the quality was not good
;    CORRECTED_INTERP: out
;                   the corrected sr50 Time serie (same size as input) interpolated where the quality was not good
;    CORRECTED_QUAL: out
;                   the associated quality flag (same size as input) set to 0 where the quality was not good
;
;
; :History:
;     Written by FaM, 2010.
;
;
;-
pro AWS_corr_sr50_basics, distance, time, x, y, airtemp, quality, corrected, new_time, out_qual, NO_TEMP_COR = NO_TEMP_COR, NO_ANGLE_COR =NO_ANGLE_COR,  $
                          CORRECTED_NAN = corrected_nan, CORRECTED_INTERP = corrected_interp, CORRECTED_QUAL = corrected_qual
                         
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~check_WTIME(time, OUT_QMS=ttime, WAS_ABSDATE=was_absdate) then Message, WAVE_Std_Message('time', /ARG)
  if ~array_processing(distance, x, y, airtemp, quality) then Message, WAVE_Std_Message(/ARG)
  if ~array_processing(distance, time) then Message, WAVE_Std_Message(/ARG)
  
  ; Temperature correction
  if ~ KEYWORD_SET(NO_TEMP_COR) then dis_tcorr = distance * SQRT((273.15D + airtemp) / 273.15D) else dis_tcorr = distance
  
  ; Angle correction
  angle  = AWS_MAST_ANGLE(x,y)      
  if ~ KEYWORD_SET(NO_ANGLE_COR) then dis_acorr = dis_tcorr * cos(angle * !pi / 180d) else dis_acorr = dis_tcorr
  
  ; Top filter 
  pqual_210  = where(quality gt 0 and quality le 210, cqual) 

  corrected = dis_acorr[pqual_210]   
  new_time = ttime[pqual_210]
  out_qual = quality[pqual_210]
  
    
  CORRECTED_NAN = TS_FILL_MISSING(corrected, new_time, ttime, INDEXES = indexes)
  CORRECTED_QUAL = quality
  CORRECTED_QUAL[indexes] = 0
  CORRECTED_INTERP = INTERPOL(corrected, new_time, ttime)
  
  if was_absdate then new_time = MAKE_ABS_DATE(QMS=new_time)

end


function AWS_irts, att, sb

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2

  if ~ array_processing(att, sb) then Message, WAVE_Std_Message(/ARG)

  PSB = 49.9092 + 0.59237 * SB + 0.00558 * SB * SB
  HSB = 4.2828 + 0.4248 * SB - 0.00077 * SB * SB
  KSB = 52.0705 - 5.3816 * SB + 0.387 * SB * SB
  
  SEC = (0.25 / PSB )*(( ATT - HSB )^2 - KSB) ;  

  return, ATT - SEC
  
end


function AWS_RH_campbell, rh, t  

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~ array_processing(rh, t) then Message, WAVE_Std_Message(/ARG)
  
  ;linfit
  a = 0.84285714d
  b = 99.5d
  fac = 100. / (a * t + b) 
  
  ;polyfit
  x0 = 100.00000
  x1 = 0.78000000d
  x2 = -0.13177778d
  x3 = -0.025666667d
  x4 = -0.0019111111d
  x5 = -6.1333334d-05
  x6 = -7.1111111d-07
  fac = 100. / (x0 + x1*t + x2*t^2 + x3*t^3 + x4*t^4 + x5*t^5 + x6*t^6) 
  
  p = where(t lt 0., cnt)
  out = rh 
  if cnt ne 0 then out[p] = rh[p] * fac[p]
  
  return, out

end

function AWS_linear_interp, top, bot, top_h, bot_h, H = h 
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~ array_processing(top, bot, top_h, bot_h) then Message, WAVE_Std_Message(/ARG)
    
  ; T = a * H + b
  a = (top - bot) / (top_h - bot_h)
  b = (bot * top_h  - top * bot_h) / (top_h - bot_h)
  
  if ~KEYWORD_SET(h) then h = 2
  
  return, a * double(h) + b
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       TIB_AWS_write_file
;
; PURPOSE:
;       TIB_AWS_write_file writes an ascii file in the usual AWS file format
;
; CATEGORY:
;       AWS
;
; CALLING SEQUENCE:
;       ATIB_AWS_write_file, FILE = file, time, data, tag, unit, type, TITLE = title
;
; KEYWORDS:
;       FILE: the path of the file to create. if not given, a dialog window will open
;       TITLE: the header line of the file.  if not given, a std text will be written (not good)
;
; INPUT:
;       time: the time serie in absolute date format. (one dimensional array of dimension N)
;       data: the data to write (two dimensional array of dimension N, V)
;       tag: string array with the variable names (one dimensional array of dimension V)
;       unit : string array with the variable units (one dimensional array of dimension V)
;       type: string array with the variable types ('float' or 'long')
;
; OUTPUT:
;       an scii file
;
;-
;-----------------------------------------------------------------------
pro AWS_write_file, data_struct, TITLE = title, FILE = file

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  if ~KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please name the file you want to create')
  
  _d = data_struct
  
  time = _d.time
  n = _d.nt  
  unit = _d.unit
  utils_remove_tag, _d, 'TIME'
  utils_remove_tag, _d, 'NT'
  utils_remove_tag, _d, 'UNIT'
   
  tags = TAG_NAMES(_d)
  nvar = N_ELEMENTS(tags)
  
  openw, id, file, /GET_LUN
  
  if ~KEYWORD_SET(title) then title = '% File created with IDL' else title = '%' + title
  printf, id, title
  
  sep = '","'
  
  text = '"TIMESTAMP","'
  for i = 0, nvar - 2 do text +=tags[i] + sep
  text += tags[nvar- 1]  + '"'
  printf, id, text
 
  text = '"-","'
  for i = 0, nvar - 2 do text +=unit[i] + sep
  text += unit[nvar- 1]  + '"'
  printf, id, text
  
  text = '"string","'
  for i = 0, nvar - 2 do text += type_name(var_info((_d.(i))[0])) + sep
  text += type_name(var_info((_d.(nvar-1))[0])) + '"'
  printf, id, text
  
  sep = ','
  for l = 0, n-1 do begin
    ;"2011-08-16 12:30:00"  
    t = TIME_to_STR(time[l], MASK='YYYY-MM-DD HH:TT:SS')

    text = '"' + t + '",'
    for i = 0, nvar - 1 do begin
      val = (_d.(i))[l]
      if var_info(val) eq IDL_FLOAT then v = strcompress(STRING(val,FORMAT = '(F8.3)'),/REMOVE_ALL)
      if var_info(val) eq IDL_LONG  then v = strcompress(STRING(val,FORMAT = '(I8)'),/REMOVE_ALL)
      if i lt nvar - 1 then text += v + sep else text += v
    endfor    
    printf, id, text
  endfor
  
  close, id
  FREE_LUN, id
end
