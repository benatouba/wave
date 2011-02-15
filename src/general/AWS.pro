; docformat = 'rst'
;+
;
;This bundle of procedures is a tool set available to the WAVE user for reading ASCII files from Campbell loggers
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
;     Last modification:  10-Feb-2011 FaM
;-


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
function AWS_parse_time, stimes

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  on_error, 2
  
  if ~arg_okay(stimes, TYPE = IDL_SRING) then Message, WAVE_Std_Message('stimes', /STRING)
  
  return, QMS_TIME(YEAR=STRMID(stimes,1,5), MONTH=STRMID(stimes,6,2),DAY=STRMID(stimes,9,2), $
    HOUR=STRMID(stimes,12,2),MINUTE=STRMID(stimes,15,2),SECOND=STRMID(stimes,18,2))

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
          
            time = AWS_PARSE_TIME(ascii_data.(i))
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
    
    ;create new structure/ add entry to existing structure
    if n_elements(ostr) eq 0 then ostr = create_struct(names[i],(struct.(i))[p0:p1]) $
    else  ostr = create_struct(ostr,names[i],(struct.(i))[p0:p1])
    
  ; End of Loop
  endfor
  
  return, create_struct(ostr,'nt',p1-p0+1)
  
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
;
; :Keywords:
;    CORRECTED_NAN: out
;                   the corrected sr50 Time serie (same size as input) with NANs where the quality was not good
;    CORRECTED_INTERP: out
;                      the corrected sr50 Time serie (same size as input) interpolated where the quality was not good
;    CORRECTED_QUAL: out
;                    the associated quality flag (same size as input) set to 0 where the quality was not good
;
;
; :History:
;     Written by FaM, 2010.
;
;
;-
pro AWS_corr_sr50_basics, distance, time, x, y, airtemp, quality, corrected, new_time, $
                          CORRECTED_NAN = corrected_nan, CORRECTED_INTERP = corrected_interp, CORRECTED_QUAL = corrected_qual
                         
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~check_WTIME(time, OUT_QMS=ttime, WAS_ABSDATE=was_absdate) then Message, WAVE_Std_Message('time', /ARG)
  if ~array_processing(distance, x, y, airtemp, quality) then Message, WAVE_Std_Message(/ARG)
  if ~array_processing(distance, time) then Message, WAVE_Std_Message(/ARG)
  
  ; Temperature correction
  dis_tcorr = distance * SQRT((273.15D + airtemp) / 273.15D)
  
  ; Angle correction
  angle  = AWS_MAST_ANGLE(x,y)      
  dis_acorr = dis_tcorr * cos(angle * !pi / 180d)
  
  ; Top filter 
  pqual_210  = where(quality gt 0 and quality le 210, cqual) 

  corrected = dis_acorr[pqual_210]   
  new_time = ttime[pqual_210]
  
    
  CORRECTED_NAN = TS_FILL_MISSING(corrected, new_time, ttime, INDEXES = indexes)
  CORRECTED_QUAL = quality
  CORRECTED_QUAL[indexes] = 0
  CORRECTED_INTERP = INTERPOL(corrected, new_time, ttime)
  
  if was_absdate then new_time = MAKE_ABS_DATE(QMS=new_time)

end