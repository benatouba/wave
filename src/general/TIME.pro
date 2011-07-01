; docformat = 'rst'
;+
; 
; This bundle of structures and procedures gives a background to the WAVE user
; for time and time-series handling. These tools are based on the TNT time 
; structure, but add a millisecond precison and some usefull functions.
;       
; The Absolute Date {ABS_DATE} is the structure containing all information, defining 
; an absolute date in time and could be used when exchanging time infos between       
; functions, procedures and programs in the WAVE library. 
;       
; The structure is defined as::      
;        
;   {ABS_Date, 
;    year:        0L,        Gregorian year 
;    month:       0L,        Gregorian month
;    day:         0L,        Gregorian day 
;    hour:        0L,        Hour of day 
;    minute:      0L,        Minute of hour
;    second:      0L,        Second of minute
;    millisecond: 0L,        Millisecond of second 
;    qt:          0d,        Quattro number. absolute time in days since 30.12.1899 at 00:00:00 (kept for compatibility reasons with the TNT)
;    qms:        0ll         Quattro millisecond. absolute time in milliseconds since 30.12.1899 at 00:00:00 (to be used as reference)
;    }
;        
; It should never be created manualy, but always using the `Make_Abs_Date` function, 
; providing a bunch of ways to create an Absolute date structure.
; 
; Time zones are currently not supported by the Wave library.
;
; In addition to this, a relative time difference for time series handling is defined as an interval in time::       
;        
;   {TIME_STEP,
;    day:         0L,        relative day(s)
;    hour:        0L,        relative hour(s) of day (0-23)
;    minute:      0L,        relative minute(s) of hour (0-59)
;    second:      0L,        relative second(s) of minute (0-59)
;    millisecond: 0L,        relative millisecond(s) of second (0-999)
;    dms:        0ll         delta millisecond : total relative time in milliseconds
;    }
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
;     Last modification:  22-Nov-2010 FaM
;-

;+
;
; :Description:
;    This procedure initializes the TIME system.
;
;    Usually, you should not call this procedure yourself, unless you
;    really wish to reinitialize the TIME system.
;
; :Categories:
;    General/Time
;    
; :Private: 
;           
; :History:
;       Written by FaM, 2009
;-
pro TIME_init

  @WAVE.inc

  abs_date =   {ABS_DATE,          $
    zone:        '',      $
    year:        0L,      $
    month:       0L,      $
    day:         0L,      $
    hour:        0L,      $
    minute:      0L,      $
    second:      0L,      $
    millisecond: 0L,      $
    qt:          0d,      $
    qms:         0ll      $
    }
    
  time_step =  {TIME_STEP, $
    day:         0L,  $
    hour:        0L,  $
    minute:      0L,  $
    second:      0L,  $
    millisecond: 0L,  $
    dms:         0ll  $
    }
    
    ; Basics
    S_QMS = 1000LL
    M_QMS = S_QMS * 60LL
    H_QMS = M_QMS * 60LL
    D_QMS = H_QMS * 24LL
    
    ;ret = WAVE_Error_Message('TIME structure definitions successfull', /INFO)
    
end

;+
; :Description:
;       
;       This function returns WAVE qms number(s) from gregorian time. It is
;       similar to the `MAKE_ABS_DATE` function but handles only qms numbers.
;       
;       It should be used as default date definition method, as arrays of qms
;       are less computation demanding then arays of {ABS_DATE} structures.
;       
;       (temporarily replaces the GEN_quattro_time routine)
;       
; :Categories:
;    General/Time
;
; :Keywords:
;    YEAR:  in, optional, type=integer vector, default=1899
;           The gregorian year(s).
;    MONTH: in, optional, type=integer vector, default=12
;           The gregorian month(s).
;    DAY: in, optional, type=integer vector, default=30
;           The gregorian days(s).
;    HOUR: in, optional, type=integer vector, default=0
;           The hour(s) of day.
;    MINUTE: in, optional, type=integer vector, default=0
;           The minute(s) of hour.
;    SECOND: in, optional, type=integer vector, default=0
;           The second(s) of minute.
;    MILLISECOND: in, optional, type=integer vector, default=0
;           The milliseconds(s).
;    TNT_T: in, optional, type={TNT_TIME}/integer vector, default = none
;           The TNT time structures(s). If the argument is of numeric type
;           it will be understood as quattro numbers.
;    DATE_Str: in, optional, type=sring vector, default = none
;           The date string in the format 'DD.MM.YYYY'. (More formats will come soon)
;    TIME_Str: in, optional, type=sring vector, default ='00:00:00'
;           The time string in the format 'HH:MM:SS'. (More formats will come soon)
;    JULIAN_DAY: in, optional, type=float vector, default = none
;           The time in julian days
; 
; :Returns:
;   A scalar or vector of type LONG64, representing an absolute date in time.
;           
; :Examples:
;    
;    There are many ways to use the QMS_time function. Here are some possibilities::
;     
;     IDL> qms = QMS_TIME()
;     IDL> print, TIME_to_STR(qms)
;     22.11.2010 11:11:04
;    
;     IDL> qms = QMS_TIME(DATE_Str = '14.10.1984')
;     IDL> print, TIME_to_STR(qms)
;     14.10.1984 00:00:00
;    
;     IDL> qms = QMS_TIME(DATE_Str = '14.10.1984', TIME_STR='05:18:58')
;     IDL> print, TIME_to_STR(qms)
;     14.10.1984 05:18:58
;    
;     IDL> qms = QMS_TIME(year = 2003, month=2, day = 24, hour = 06)
;     IDL> print, TIME_to_STR(qms)
;     24.02.2003 06:00:00
;    
;     IDL> qms = QMS_TIME(year = 2003, month=2, day = INDGEN(4) + 1, hour = 06)
;     IDL> for i=0, N_ELEMENTS(qms)-1 do print, TIME_to_STR(qms[i])
;     01.02.2003 06:00:00
;     02.02.2003 06:00:00
;     03.02.2003 06:00:00
;     04.02.2003 06:00:00
;    
; :History:
;       Written by FaM, 2010.
;-
function QMS_TIME, YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond, $
                      TNT_T = tnt_t, DATE_Str = DATE_Str, TIME_Str = TIME_Str, JULIAN_DAY = julian_day


  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  mytime = 0LL
  
  if KEYWORD_SET(TNT_T) then begin ; Make an absolute date from a TNT time
    
    if arg_okay(TNT_T, /NUMERIC) then GEN_quattro_time, ret, TNT_T, tt  $ 
     else if arg_okay(TNT_T, STRUCT={TNT_TIME}) then tt =TNT_T   $
       else Message, WAVE_Std_Message('TNT_T', /ARG)    
      
    n = N_ELEMENTS(tt)
    my_time = LON64ARR(n)    
    my_time = LONG64(tt.qt)* D_QMS + LONG64(tt.hour)* H_QMS + LONG64(tt.minute)* M_QMS + LONG64(tt.second)* S_QMS + LONG64(1000.*tt.fraction)
        
  endif else if KEYWORD_SET(JULIAN_DAY) then begin  ; Make an absolute date from a string
  
    if not arg_okay(JULIAN_DAY, /NUMERIC) then  Message, WAVE_Std_Message('jd', /NUMERIC) 
    
    CALDAT, JULIAN_DAY, Month, Day, Year, Hour, Minute, Second
    my_time = QMS_TIME(year=year, day=day, month=month, minute=minute, second=second, hour = hour)
        
  endif else if KEYWORD_SET(DATE_Str) then begin  ; Make an absolute date from a string
  
    if not arg_okay(DATE_Str, type = IDL_STRING) then Message, WAVE_Std_Message('DATE_Str', /STR)    
    nD = N_ELEMENTS(DATE_Str)
    
    if not KEYWORD_SET(TIME_Str) then time_Str = '00:00:00'
    if not arg_okay(time_str, type = IDL_STRING) then Message, WAVE_Std_Message('time_str', /STR)    
    nT = N_ELEMENTS(TIME_Str)
    
    n = max([nT, nD])    
    if nT lt n then time_str = [time_str, STRARR(n - nT) + time_str[nT-1]]
    if nD lt n then date_str = [date_str, STRARR(n - nD) + date_str[nD-1]]
         
    ; TODO: Update routine: unefficient to use GEN_str_time here ...
    GEN_str_time, ret, tt, DSTR=DATE_Str, DMASK='DD.MM.YYYY',TSTR=time_str,TMASK='HH:MM:SS' 
    my_time = LONG64(tt.qt)* D_QMS + LONG64(tt.hour)* H_QMS + LONG64(tt.minute)* M_QMS + LONG64(tt.second)* S_QMS
        
  endif else if KEYWORD_SET(year) or keyword_set(month) or $
                 keyword_set(day) or keyword_set(hour)  or $
                 keyword_set(minute) or  keyword_set(second) or $
                 keyword_set(millisecond) then begin  ; Make an absolute date from gregorian params
  
    if not keyword_set(year) then year = 1899L
    if not keyword_set(month) then month = 12L
    if not keyword_set(day) then day = 30L
    if not keyword_set(hour) then hour = 0L
    if not keyword_set(minute) then minute = 0L
    if not keyword_set(second) then second = 0L
    if not keyword_set(millisecond) then millisecond = 0L
    
    if not arg_okay(year, /NUMERIC) then Message, WAVE_Std_Message('year', /NUMERIC)
    if not arg_okay(month, /NUMERIC) then Message, WAVE_Std_Message('month', /NUMERIC)
    if not arg_okay(day, /NUMERIC) then Message, WAVE_Std_Message('day', /NUMERIC)
    if not arg_okay(hour, /NUMERIC) then Message, WAVE_Std_Message('hour', /NUMERIC)
    if not arg_okay(minute, /NUMERIC) then Message, WAVE_Std_Message('minute', /NUMERIC)
    if not arg_okay(second, /NUMERIC) then Message, WAVE_Std_Message('second', /NUMERIC)
    if not arg_okay(millisecond, /NUMERIC) then Message, WAVE_Std_Message('millisecond', /NUMERIC)
    
    n_year = n_elements(year)
    n_month = n_elements(month)
    n_day = n_elements(day)
    n_hour = n_elements(hour)
    n_minute = n_elements(minute)
    n_month = n_elements(month)
    n_second = n_elements(second)
    n_millisecond = n_elements(millisecond)
    
    n = max([n_year,n_month,n_day,n_hour,n_minute,n_month,n_second,n_second,n_millisecond])
    
    if n ne 1 and n_year lt n then year = [year, LONARR(n-n_year) + year[n_year-1]]
    if n ne 1 and n_month lt n then month = [month, LONARR(n-n_month) + month[n_month-1]]
    if n ne 1 and n_day lt n then day = [day, LONARR(n-n_day) + day[n_day-1]]
    if n ne 1 and n_hour lt n then hour = [hour, LONARR(n-n_hour) + hour[n_hour-1]]
    if n ne 1 and n_minute lt n then minute = [minute, LONARR(n-n_minute) + minute[n_minute-1]]
    if n ne 1 and n_second lt n then second = [second, LONARR(n-n_second) + second[n_second-1]]
    if n ne 1 and n_millisecond lt n then millisecond = [millisecond, LONARR(n-n_millisecond) + millisecond[n_millisecond-1]]
        
    if TOTAL(year gt 9999L) ne 0 or TOTAL(year lt 1899L) ne 0 then Message, WAVE_Std_Message('year', /RANGE)
    if TOTAL(month gt 12) ne 0 or TOTAL(month lt 1) ne 0 then Message, WAVE_Std_Message('month', /RANGE)
    if TOTAL(day gt GEN_month_days(month,year)) ne 0 or TOTAL(day lt 1) ne 0 then Message, WAVE_Std_Message('day', /RANGE)
    if TOTAL(hour gt 23) ne 0 or TOTAL(hour lt 0) ne 0 then Message, WAVE_Std_Message('hour', /RANGE)
    if TOTAL(minute gt 59) ne 0 or TOTAL(minute lt 0) ne 0 then Message, WAVE_Std_Message('minute', /RANGE)
    if TOTAL(second gt 59) ne 0 or TOTAL(second lt 0) ne 0 then Message, WAVE_Std_Message('second', /RANGE)
    if TOTAL(millisecond gt 999) ne 0 or TOTAL(millisecond lt 0) ne 0 then Message, WAVE_Std_Message('millisecond', /RANGE)
    
    epsilon = 1d-8
    jt = julday(month,day,year,hour,minute,second)
    qt = floor(jt) + epsilon*round((jt mod 1d)/epsilon) 
    qt -= 2415018.5        
    my_time = LONG64(qt)* D_QMS + LONG64(hour)* H_QMS + LONG64(minute)* M_QMS + LONG64(second)* S_QMS + LONG64(millisecond) 
    
  endif else begin ;Default system time
   GEN_quattro_time, ret, GEN_quattro_number(), tt
   my_time = QMS_TIME(TNT_T = tt)
  endelse
  
  return, my_time
  
end

;+
; :Description:
; 
;    This function generates a new time (in QMS) defined by a given offset 
;    to a reference time.
;    
;    You can give an array of reference time as input and shift all elements 
;    of a given offset or give a scalar time as input and make an array of 
;    shifted elements. (see examples)
;    
;    Positive and negative offsets are accepted. (see examples)
;    
;    It is similar to the `MAKE_REL_DATE` function but uses qms (less 
;    computation demanding). It is more limited than the `MAKE_REL_DATE` 
;    function as is does not accept "irregular" offsets such as months
;    and years.  
;       
; :Categories:
;    General/Time
;
; :Params:
;    refTime: in, optional, type=integer/{ABS_DATE} vector, default=current system time
;            The reference time from which the shifted time should be generated. It can be
;            a scalar or a vector, in QMS or {ABS_DATE} format.
;
; :Keywords:
;    DAY: in, optional, type=integer vector, default=0
;           The offset in day(s).
;    HOUR: in, optional, type=integer vector, default=0
;           The offset in hour(s).
;    MINUTE: in, optional, type=integer vector, default=0
;           The offset in minutes(s).
;    SECOND: in, optional, type=integer vector, default=0
;           The offset in second(s).
;    MILLISECOND: in, optional, type=integer vector, default=0
;           The offset in millisecond(s).
;
; :Returns:
;   A scalar or vector of type LONG64, representing an absolute date in time.
;
; :Examples:
; 
;  REL_TIME can be usefull in various cases::
;    
;      1. Simple case
;    IDL> qms = QMS_TIME(DATE_Str = '28.02.1984', TIME_STR='23:59:59')
;    IDL> print, TIME_to_STR(qms)
;    28.02.1984 23:59:59
;    IDL> rel_qms = REL_TIME(qms, second = 1)
;    IDL> print, TIME_to_STR(rel_qms)
;    29.02.1984 00:00:00
;    IDL> rel_qms_new = REL_TIME(rel_qms, minute = - 30)
;    IDL>  print, TIME_to_STR(rel_qms_new)
;    28.02.1984 23:30:00   
;    
;      2. Single reference date and array of intervals
;    IDL> qms = QMS_TIME(DATE_Str = '14.10.1984')
;    IDL> print, TIME_to_STR(qms)
;    14.10.1984 00:00:00
;    IDL> rel_qms = REL_TIME(qms, day = INDGEN(4), hour = INDGEN(4) * 2, MINUTE=INDGEN(4) * 3)
;    IDL> for i=0, N_ELEMENTS(rel_qms)-1 do print, TIME_to_STR(rel_qms[i])
;    14.10.1984 00:00:00
;    15.10.1984 02:03:00
;    16.10.1984 04:06:00
;    17.10.1984 06:09:00
;    
;      3. Array of reference dates and single intervals
;    IDL> qms = QMS_TIME(DATE_Str = ['14.10.1984','15.10.1984','16.10.1984'])
;    IDL> for i=0, N_ELEMENTS(qms)-1 do print, TIME_to_STR(qms[i])
;    14.10.1984 00:00:00
;    15.10.1984 00:00:00
;    16.10.1984 00:00:00
;    IDL> rel_qms = REL_TIME(qms, hour = 1, minute = 2, second = 3)
;    IDL> for i=0, N_ELEMENTS(rel_qms)-1 do print, TIME_to_STR(rel_qms[i])
;    14.10.1984 01:02:03
;    15.10.1984 01:02:03
;    16.10.1984 01:02:03
;
; :History:
;       Written by FaM, 2010.
;-
function REL_TIME, refTime, DAY = day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  if N_ELEMENTS(refTime) eq 0 then refTime = QMS_TIME()
  
  if ~check_WTIME(refTime, OUT_QMS=outDate) then Message, WAVE_Std_Message('refTime', /ARG)
  
  if not keyword_set(day) then day = 0L
  if not keyword_set(hour) then hour = 0L
  if not keyword_set(minute) then minute = 0L
  if not keyword_set(second) then second = 0L
  if not keyword_set(millisecond) then millisecond = 0L
  
  n_day = n_elements(day)
  n_hour = n_elements(hour)
  n_minute = n_elements(minute)
  n_month = n_elements(month)
  n_second = n_elements(second)
  n_millisecond = n_elements(millisecond)
  n = max([n_day,n_hour,n_minute,n_month,n_second,n_second,n_millisecond])
  if N_ELEMENTS(outDate) gt 1 and N ne 1 then MESSAGE, 'If Reftime is an array, only scalar keywords are accepted'
  
  if n ne 1 and n_day lt n then day = [day, LONARR(n-n_day) + day[n_day-1]]
  if n ne 1 and n_hour lt n then hour = [hour, LONARR(n-n_hour) + hour[n_hour-1]]
  if n ne 1 and n_minute lt n then minute = [minute, LONARR(n-n_minute) + minute[n_minute-1]]
  if n ne 1 and n_second lt n then second = [second, LONARR(n-n_second) + second[n_second-1]]
  if n ne 1 and n_millisecond lt n then millisecond = [millisecond, LONARR(n-n_millisecond) + millisecond[n_millisecond-1]]
  
  if not arg_okay(day, /NUMERIC) then  Message, WAVE_Std_Message('day', /NUMERIC)
  dqmsi = LONG64(day) * D_QMS
  outDate = outDate + dqmsi
  
  if not arg_okay(hour, /NUMERIC) then  Message, WAVE_Std_Message('hour', /NUMERIC)
  dqmsi = LONG64(hour) * H_QMS
  outDate = outDate + dqmsi
  
  if not arg_okay(minute, /NUMERIC) then  Message, WAVE_Std_Message('minute', /NUMERIC)
  dqmsi = LONG64(minute) * M_QMS
  outDate = outDate + dqmsi
  
  if not arg_okay(second, /NUMERIC) then  Message, WAVE_Std_Message('second', /NUMERIC)
  dqmsi = LONG64(second) * S_QMS
  outDate = outDate + dqmsi
  
  if not arg_okay(millisecond, /NUMERIC) then  Message, WAVE_Std_Message('millisecond', /NUMERIC)
  dqmsi = LONG64(millisecond)
  outDate = outDate + dqmsi
  
  return, outDate
  
end

;+
; :Description:
;    This function is the major tool to handle time within the 
;    WAVE environment. It returns a WAVE Absolute date structure
;    filled with the user specifications. 
;    
;    It should be used as standard date definition method after
;    `QMS_TIME` which is faster and less memory demanding.
;    
;    (temporarily replaces the GEN_MAKE_TIME function)
;       
; :Categories:
;    General/Time
;
; :Keywords:
;    YEAR:  in, optional, type=integer vector, default=1899
;           The gregorian year(s).
;    MONTH: in, optional, type=integer vector, default=12
;           The gregorian month(s).
;    DAY: in, optional, type=integer vector, default=30
;           The gregorian days(s).
;    HOUR: in, optional, type=integer vector, default=0
;           The hour(s) of day.
;    MINUTE: in, optional, type=integer vector, default=0
;           The minute(s) of hour.
;    SECOND: in, optional, type=integer vector, default=0
;           The second(s) of minute.
;    MILLISECOND: in, optional, type=integer vector, default=0
;           The milliseconds(s).
;    QMS: in, optional, type=integer vector, default=0
;           The time in qms(s). If set, other keywords are ignored.
;    TNT_T: in, optional, type={TNT_TIME}/integer vector, default = none
;           The TNT time structures(s). If the argument is of numeric type
;           it will be understood as quattro numbers.
;    DATE_Str: in, optional, type=sring vector, default = none
;           The date string in the format 'DD.MM.YYYY'. (More formats will come soon)
;    TIME_Str: in, optional, type=sring vector, default = '00:00:00'
;           The time string in the format 'HH:MM:SS'. (More formats will come soon)
;    REFdate:  in, optional, type={ABS_DATE}/qms vector, default = none
;           If set, the keywords YEAR, MONTH, DAY, MINUTE, SECOND, MILLISECOND are understood
;           as offsets to the reference time (see `MAKE_REL_DATE`). Other keywords are ignored.
;    JULIAN_DAY: in, optional, type=float vector, default = none
;           The time in julian days
;
; :Returns:
;   A scalar or vector of type {ABS_DATE}, representing an absolute date in time.
;   
; :Examples:
;   
; Very similar to the `QMS_TIME` function. However, it adds interesting features not 
; available in `QMS_TIME` such as the `refDate` keyword which enwraps the `MAKE_REL_DATE`
; function and adds the possibility to handle array of refDates::
; 
;     1. Array of refDates and scalar offset   
;   IDL> refdate = MAKE_ABS_DATE(year = 2008, month = [1,2], day = 1)
;   IDL> for i=0, N_ELEMENTS(refDate) - 1 do print, TIME_to_STR(refDate[i])
;   01.01.2008 00:00:00
;   01.02.2008 00:00:00
;   IDL> bad_shift = MAKE_REL_DATE(refDate, MONTH=2)
;   %MAKE_REL_DATE: ERROR!  refDate has to be a scalar!
;   
;   Traceback Report from MAKE_REL_DATE:
;   
;       %  refDate has to be a scalar!
;   IDL> good_shift = MAKE_ABS_DATE(REFDATE = refDate, month = 2)
;   IDL> for i=0, N_ELEMENTS(good_shift) - 1 do print, TIME_to_STR(good_shift[i])
;   01.03.2008 00:00:00
;   01.04.2008 00:00:00
;   
;     2. Scalar refDate and array of offsets   
;   IDL> refdate = MAKE_ABS_DATE(year = 2008, month = 1, day = 1)
;   IDL> bad_shift = MAKE_REL_DATE(refDate, MONTH=[0,1,2,3])
;   %MAKE_REL_DATE: ERROR!  arguments have to be scalars!.
;   
;   Traceback Report from MAKE_REL_DATE:
;   
;        %  arguments have to be scalars!.
;   IDL> good_shift = MAKE_ABS_DATE(REFDATE = refDate, month = [0,1,2,3])
;   IDL> for i=0, N_ELEMENTS(good_shift) - 1 do print, TIME_to_STR(good_shift[i])
;   01.01.2008 00:00:00
;   01.02.2008 00:00:00
;   01.03.2008 00:00:00
;   01.04.2008 00:00:00
;   
; It is also usefull if you want to convert your qms(s) in date structures.
; 
; :History:
;       Written by FaM, 2009.
;-
function MAKE_ABS_DATE, YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond, $
                          QMS = qms, TNT_T = tnt_t, DATE_Str = DATE_Str, TIME_Str = TIME_Str, REFdate = refdate, JULIAN_DAY = julian_day


  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  if KEYWORD_SET(qms) then begin ; Make an absolute date from QMS
  
    if not arg_okay(qms, /NUMERIC) then message, WAVE_Std_Message('qms', /NUMERIC)
    
    n = n_elements(qms)
    my_time = REPLICATE({ABS_DATE}, n)
        
    my_time.qms = qms
    
    nbday = my_time.qms / D_QMS
    lday = nbday * D_QMS
    frac = DOUBLE(my_time.qms-lday) / D_QMS
    qt = double(nbday) + frac
    undefine, month,day,year,hour,minute,second
    caldat, qt + 2415018.5d, month,day,year,hour,minute,second    
    
    my_time.year = year
    my_time.month = month
    my_time.day = day
    
    my_time.hour = LONG(my_time.qms - lday) / H_QMS
    lhour =  my_time.hour * H_QMS
    my_time.minute = LONG(my_time.qms - lday - lhour) / M_QMS
    lmin = my_time.minute*M_QMS
    my_time.second =  LONG(my_time.qms - lday - lhour - lmin) / S_QMS
    my_time.millisecond =  LONG(my_time.qms - lday - lhour - lmin - my_time.second * S_QMS)
    
    my_time.qt = qt
        
  endif else if keyword_set(REFdate) then begin
  
    n_ref = N_ELEMENTS(REFdate)
    
    if n_ref eq 1 then Begin
    
      if not keyword_set(year) then year = 0L
      if not keyword_set(month) then month = 0L
      if not keyword_set(day) then day = 0L
      if not keyword_set(hour) then hour = 0L
      if not keyword_set(minute) then minute = 0L
      if not keyword_set(second) then second = 0L
      if not keyword_set(millisecond) then millisecond = 0L
      
      n_year = n_elements(year)
      n_month = n_elements(month)
      n_day = n_elements(day)
      n_hour = n_elements(hour)
      n_minute = n_elements(minute)
      n_month = n_elements(month)
      n_second = n_elements(second)
      n_millisecond = n_elements(millisecond)
      
      n = max([n_year,n_month,n_day,n_hour,n_minute,n_month,n_second,n_second,n_millisecond])
      
      if n ne 1 and n_year lt n then year = [year, LONARR(n-n_year) + year[n_year-1]]
      if n ne 1 and n_month lt n then month = [month, LONARR(n-n_month) + month[n_month-1]]
      if n ne 1 and n_day lt n then day = [day, LONARR(n-n_day) + day[n_day-1]]
      if n ne 1 and n_hour lt n then hour = [hour, LONARR(n-n_hour) + hour[n_hour-1]]
      if n ne 1 and n_minute lt n then minute = [minute, LONARR(n-n_minute) + minute[n_minute-1]]
      if n ne 1 and n_second lt n then second = [second, LONARR(n-n_second) + second[n_second-1]]
      if n ne 1 and n_millisecond lt n then millisecond = [millisecond, LONARR(n-n_millisecond) + millisecond[n_millisecond-1]]
      
      my_time = REPLICATE({ABS_DATE}, n)
      for i = 0, n-1 do begin
        my_time[i] = MAKE_REL_DATE(REFdate, YEAR=year[i], MONTH=month[i], DAY=day[i], HOUR=hour[i], MINUTE=minute[i], SECOND=second[i], MILLISECOND = millisecond[i])
      endfor
      
    endif else begin
      n = n_ref
      my_time = REPLICATE({ABS_DATE}, n) 
      for i = 0, n-1 do begin
        my_time[i] = MAKE_REL_DATE(REFdate[i], YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond)
      endfor
    endelse
  
  endif else if KEYWORD_SET(year) or keyword_set(month) or $
                keyword_set(day) or keyword_set(hour)  or $
                keyword_set(minute) or  keyword_set(second) or $
                keyword_set(millisecond) or  keyword_set(tnt_t) or $
                keyword_set(date_str) or  keyword_set(time_str) or $
                keyword_set(julian_day) then begin  ; Make an absolute date from other cases
                  
    myqms = QMS_TIME(YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, $
                      MILLISECOND = millisecond,TNT_T=tnt_t, DATE_STR=date_str, TIME_STR = time_str, JULIAN_DAY=julian_day)
                      
    my_time = MAKE_ABS_DATE(qms = myqms)  
    
  endif else begin ; We just want the current time
     my_time = MAKE_ABS_DATE(qms = QMS_TIME())  
  endelse
  
  return, my_time
  
end

;+
; :Description:
; 
;    This function generates a new date ({ABS_DATE}) defined by a given offset 
;    to a reference time.
;        
;    It is similar to the `REL_TIME` function but accepts "irregular" offsets 
;    such as months and years, but is more computation demanding. ;    
;    At the sam time, it only accepts scalar arguments.
;    
;    If you want to use arrays use the `MAKE_ABS_DATE` function
;    with the RefDAte keyword.
;       
; :Categories:
;    General/Time
;
; :Params:
;    refDate: in, optional, type=integer/{ABS_DATE}, default=current system time
;            The reference time from which the shifted time should be generated. It must be
;            a scalar, in QMS or {ABS_DATE} format.
;
; :Keywords:
;    YEAR: in, optional, type=integer, default=0
;           The offset in year(s).
;    MONTH: in, optional, type=integer, default=0
;           The offset in months(s).
;    DAY: in, optional, type=integer, default=0
;           The offset in day(s).
;    HOUR: in, optional, type=integer, default=0
;           The offset in hour(s).
;    MINUTE: in, optional, type=integer, default=0
;           The offset in minutes(s).
;    SECOND: in, optional, type=integer, default=0
;           The offset in second(s).
;    MILLISECOND: in, optional, type=integer, default=0
;           The offset in millisecond(s).
;
; :Returns:
;   A scalar of type {ABS_DATE}, representing an absolute date in time.
;
; :Examples:
; 
; Very similar to `REL_TIME` with the added keywords `MONTH` and `YEAR`::
;  
;  IDL> refDate = MAKE_ABS_DATE(year = 2000, day = 01, month = 01)
;  IDL> shift = MAKE_ABS_DATE(REFDATE = refDate, month = - 4)
;  IDL> print, TIME_to_STR(shift)
;  01.09.1999 00:00:00
;    
; :History:
;       Written by FaM, 2009.
;-
function MAKE_REL_DATE, refDate, YEAR=year, MONTH=month, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  if N_ELEMENTS(refDate) eq 0 then refDate = MAKE_ABS_DATE()
  
  if ~check_WTIME(refDate, OUT_ABSDATE=outDate) then Message, WAVE_Std_Message('refDate', /ARG)
   
  n_year = n_elements(year)
  n_month = n_elements(month)
  n_day = n_elements(day)
  n_hour = n_elements(hour)
  n_minute = n_elements(minute)
  n_month = n_elements(month)
  n_second = n_elements(second)
  n_millisecond = n_elements(millisecond)
  
  ns = [n_year, n_month, n_day, n_hour, n_minute, n_second, n_second, n_millisecond]
  if max(ns) gt 1 then MESSAGE, 'arguments have to be scalars!.'    
  if N_ELEMENTS(outDate) ne 1 then MESSAGE, 'refDate has to be a scalar!'
  
  if keyword_set(year) then begin
    if not arg_okay(year, /NUMERIC) then Message, WAVE_Std_Message('year', /NUMERIC)
    outDate = MAKE_ABS_DATE(YEAR=outDate.year+year, MONTH=outDate.month, DAY=outDate.day, HOUR=outDate.hour, MINUTE=outDate.minute, SECOND=outDate.second, MILLISECOND=outDate.millisecond)
  end
  
  if keyword_set(month) then begin
    if not arg_okay(month, /NUMERIC) then  Message, WAVE_Std_Message('month', /NUMERIC)
    newmonth = outDate.month + LONG(month)
    
    while newmonth gt 12 or newmonth lt 1 do begin
    
      if newmonth eq 0 then begin
        newmonth = 12
        outDate = MAKE_REL_DATE(outDate, YEAR=-1)
      endif else begin
        sign = LONG(newmonth/ABS(newmonth))
        outDate = MAKE_REL_DATE(outDate, YEAR=sign)
        newmonth = newmonth - sign * 12l
      endelse
      
    endwhile
    
    outDate = MAKE_ABS_DATE(YEAR=outDate.year, MONTH=newmonth, DAY=outDate.day, HOUR=outDate.hour, MINUTE=outDate.minute, SECOND=outDate.second, MILLISECOND=outDate.millisecond)
    
  endif
  
  if keyword_set(day) then begin
  
    if not arg_okay(day, /NUMERIC) then  Message, WAVE_Std_Message('day', /NUMERIC)
    
    dqmsi = LONG64(day) * D_QMS
    outDate = MAKE_ABS_DATE(QMS = LONG64(LONG64(outDate.qms) + dqmsi))
    
  endif
  
  
  if keyword_set(hour) then begin
  
    if not arg_okay(hour, /NUMERIC) then  Message, WAVE_Std_Message('hour', /NUMERIC)
    
    dqmsi = LONG64(hour) * H_QMS
    outDate = MAKE_ABS_DATE(QMS = LONG64(LONG64(outDate.qms) + dqmsi))
    
  endif
  
  
  if keyword_set(minute) then begin
  
    if not arg_okay(minute, /NUMERIC) then  Message, WAVE_Std_Message('minute', /NUMERIC)
    
    dqmsi = LONG64(minute) * M_QMS
    outDate = MAKE_ABS_DATE(QMS = LONG64(LONG64(outDate.qms) + dqmsi))
    
  endif
  
  if keyword_set(second) then begin
  
    if not arg_okay(second, /NUMERIC) then  Message, WAVE_Std_Message('second', /NUMERIC)
    
    dqmsi = LONG64(second) * S_QMS
    outDate = MAKE_ABS_DATE(QMS = LONG64(LONG64(outDate.qms) + dqmsi))
    
  endif
  
  if keyword_set(millisecond) then begin
  
    if not arg_okay(millisecond, /NUMERIC) then  Message, WAVE_Std_Message('millisecond', /NUMERIC)
    
    dqmsi = LONG64(millisecond)
    outDate = MAKE_ABS_DATE(QMS = LONG64(LONG64(outDate.qms) + dqmsi))
    
  endif
  
  return, outDate
  
end

;+
; :Description:
; This function makes a string from a time.
;    
; Currently, the format is fixed to either::
;   22.11.2010 16:46:13
; Or::
;   2010.11.22 16:46:13
; But more formats will be implemented soon.
;       
; :Categories:
;    General/Time
; 
; :Params:
;    time: in, optional, type=integer/{ABS_DATE} vector, default=current system time
;           The time to convert to string
;
; :Keywords:
;    NODATE: in, optional, type=boolean, default=0
;             Set this keyword to prevent printing the date
;    NOTIME: in, optional, type=boolean, default=0
;             Set this keyword to prevent printing the time
;    YMD: in, optional, type=boolean, default=0
;             Set this keyword to use the Year-Month-Day format
;
; :Returns:
;    The time as a string or array of strings
;
; :Examples:
; The usage of this function is really easy::
;   IDL> time = MAKE_ABS_DATE()
;   IDL> print, TIME_to_STR(time, /NODATE)
;   16:46:13
;   IDL> print, TIME_to_STR(time, /NOTIME)
;   22.11.2010
;   IDL> print, TIME_to_STR(time, /NOTIME, /YMD)
;   2010.11.22
;   IDL> print, TIME_to_STR(time, /YMD)
;   2010.11.22 16:46:13
;   IDL> print, TIME_to_STR(time)
;   22.11.2010 16:46:13
;   
; :History:
;       Written by FaM, 2009.
;-
function TIME_to_STR, time, NODATE=nodate, NOTIME=notime, YMD = ymd

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  if N_ELEMENTS(time) eq 0 then mytime = MAKE_ABS_DATE()
  
  if ~check_WTIME(time, OUT_ABSDATE=mytime) then Message, WAVE_Std_Message('time', /ARG)
  
  n = N_ELEMENTS(mytime)

  nodate = keyword_set(nodate)
  notime = keyword_set(notime)

  d = mytime.day
  mon = mytime.month
  y = mytime.year
  h = mytime.hour
  min = mytime.minute
  s = mytime.second
  
  date = replicate('',n)
  timestr = date

  if not nodate then begin
    if KEYWORD_SET(YMD) then begin
    date = string(y,format='(I4)')   + '.' + $
           string(mon,format='(I2)') + '.' + $
           string(d,format='(I2)')   
    endif else begin
    date = string(d,format='(I2)')   + '.' + $
           string(mon,format='(I2)') + '.' + $
           string(y,format='(I4)')
    endelse
    
    date = byte(date)
    i = where(date eq 32b,cnt)
    if cnt gt 0 then date[i] = 48b
    date = string(date)
  endif

  if not notime then begin
    timestr = string(h,format='(I2)')   + ':' + $
           string(min,format='(I2)') + ':' + $
           string(s,format='(I2)')

    timestr = byte(timestr)
    i = where(timestr eq 32b,cnt)
    if cnt gt 0 then timestr[i] = 48b
    timestr = string(timestr)
  endif

  str = strtrim(date+replicate(' ',n)+timestr,2)

  if n eq 1 then str = str[0]

  return, str

end

;+
; :Description:
;    This function transforms a WAVE time in julian days
;
; :Params:
;    time: in, optional, type={ABS_DATE}/qms vector, default = system time
;       
; :Categories:
;    General/Time
;
; :Returns:
;    the time in Dulian Days
;
; :History:
;       Written by FaM, 2009.
;-
function TIME_to_JD, time

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
 
  if N_ELEMENTS(time) eq 0 then TIME = QMS_TIME()
  
  if arg_okay(time, /NUMERIC) then begin
    nbday = time / D_QMS
    lday = nbday * D_QMS
    frac = DOUBLE(time-lday) / D_QMS
    jd = double(nbday) + frac + 2415018.5d     
  endif else if arg_okay(time, STRUCT={ABS_DATE}) then begin
    jd = julday(time.month,time.day,time.year,time.hour,time.minute,time.second)
  endif else Message, WAVE_Std_Message('time' ,/ARG)  
    
  return, jd
  
end

;+
; :Description:
; 
;    This function makes a WAVE formated time step. It should always be used as 
;    default method to create WAVE compliant time steps.
;       
; :Categories:
;    General/Time
;
; :Keywords:
;    DAY: in, optional, type=integer, default=0
;           The offset in day(s).
;    HOUR: in, optional, type=integer, default=0
;           The offset in hour(s).
;    MINUTE: in, optional, type=integer, default=0
;           The offset in minutes(s).
;    SECOND: in, optional, type=integer, default=0
;           The offset in second(s).
;    MILLISECOND: in, optional, type=integer, default=0
;           The offset in millisecond(s).
;    DMS: in, optional, type=integer, default=0
;           The offset in millisecond(s). If set, all other keywords are ignored.
;
; :Returns:
;   A {TIME_STEP} structure.
;
; :Examples:
; 
;  Create a Time step::
;  
;     1. First possibility
;    IDL> help, MAKE_TIME_STEP(hour = 25, minute  = 78, second = 1), /str
;    ** Structure TIME_STEP, 6 tags, length=32, data length=28:
;       DAY             LONG                 1
;       HOUR            LONG                 2
;       MINUTE          LONG                18
;       SECOND          LONG                 1
;       MILLISECOND     LONG                 0
;       DMS             LONG64                  94681000   
;       
;      2. Second possibility
;     IDL> t1 = QMS_TIME(year = 2008, day = 28, month = 02)
;     IDL> t2 = QMS_TIME(year = 2008, day = 01, month = 03)
;     IDL> help, MAKE_TIME_STEP(dms = t2-t1), /str
;     ** Structure TIME_STEP, 6 tags, length=32, data length=28:
;        DAY             LONG                 2
;        HOUR            LONG                 0
;        MINUTE          LONG                 0
;        SECOND          LONG                 0
;        MILLISECOND     LONG                 0
;        DMS             LONG64                 172800000
;        
;      3. Negative offsets
;     IDL> help, MAKE_TIME_STEP(hour = -1, minute  = 1), /str
;     ** Structure TIME_STEP, 6 tags, length=32, data length=28:
;        DAY             LONG                 0
;        HOUR            LONG                 0
;        MINUTE          LONG               -59
;        SECOND          LONG                 0
;        MILLISECOND     LONG                 0
;        DMS             LONG64                  -3540000
;        
; :History:
;       Written by FaM, 2009.
;-
function MAKE_TIME_STEP, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond, DMS = dms

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
    
  tstep = {TIME_STEP}
  
  if N_ELEMENTS(dms) ne 0 then begin
    
    if not arg_okay(dms, /NUMERIC) then Message, WAVE_Std_Message('dms', /NUMERIC)
    if not arg_okay(dms, /SCALAR) then Message, WAVE_Std_Message('dms', /SCALAR)
    
    tstep.dms = LONG64(dms)
    tstep.day = LONG(tstep.dms / D_QMS)
    lday = tstep.day*D_QMS
    tstep.hour = LONG(tstep.dms - lday) / H_QMS
    lhour =  tstep.hour * H_QMS
    tstep.minute = LONG(tstep.dms - lday - lhour) / M_QMS
    lmin = tstep.minute*M_QMS
    tstep.second =  LONG(tstep.dms - lday - lhour - lmin) / S_QMS
    tstep.millisecond =  LONG(tstep.dms - lday - lhour - lmin - tstep.second * S_QMS)    
    
  endif else begin

    if not keyword_set(day) then day = 0L
    if not keyword_set(hour) then hour = 0L
    if not keyword_set(minute) then minute = 0L
    if not keyword_set(second) then second = 0L
    if not keyword_set(millisecond) then millisecond = 0L    

    if not arg_okay(day, /NUMERIC) then Message, WAVE_Std_Message('day', /NUMERIC)
    if not arg_okay(day, /SCALAR) then Message, WAVE_Std_Message('day', /SCALAR)
    if not arg_okay(hour, /NUMERIC) then Message, WAVE_Std_Message('hour', /NUMERIC)
    if not arg_okay(hour, /SCALAR) then Message, WAVE_Std_Message('hour', /SCALAR)
    if not arg_okay(minute, /NUMERIC) then Message, WAVE_Std_Message('minute', /NUMERIC)
    if not arg_okay(minute, /SCALAR) then Message, WAVE_Std_Message('minute', /SCALAR)
    if not arg_okay(second, /NUMERIC) then Message, WAVE_Std_Message('second', /NUMERIC)
    if not arg_okay(second, /SCALAR) then Message, WAVE_Std_Message('second', /SCALAR)
    if not arg_okay(millisecond, /NUMERIC) then Message, WAVE_Std_Message('millisecond', /NUMERIC)
    if not arg_okay(millisecond, /SCALAR) then Message, WAVE_Std_Message('millisecond', /SCALAR)
    
    tstep = MAKE_TIME_STEP(DMS = LONG64(day)* D_QMS + LONG64(hour)* H_QMS + LONG64(minute)* M_QMS + LONG64(second)* S_QMS + LONG64(millisecond))
        
  endelse

  return, tstep
  
end

;+
; :Description:
;    This function returns an array containing times with a choosen offset between them.
;    
;    This offset can be positive or negative, and regular (daily, hourly, etc.) as well as
;    date dependant (monthly, yearly).
;       
; :Categories:
;    General/Time
;
; :Params:
;    startTime:  in, required , type={ABS_DATE}/qms vector, default = none
;      The first time of the serie
;      
; :Keywords:
;    NSTEPS: in, optional, type=integer, default=1
;           The number of elements of the time serie
;    TIMESTEP: in, optional, type={TIME_STEP}, default=none
;           The timestep
;    YEAR: in, optional, type=integer, default=0
;           The timestep in number of years (if set, `TIMESTEP` is ignored)
;    MONTH: in, optional, type=integer, default=0
;           The timestep in number of months (if set, `TIMESTEP` is ignored)
;
; :Returns:
;    A nsteps elements array of qms or {ABS_DATE}
;
; :Examples:
; 
;  Create a time serie::
;  
;       1. First possibility
;     IDL> timestep = MAKE_TIME_STEP(day = 1, hour = 12)
;     IDL> nsteps = 4
;     IDL> startTime = QMS_TIME(year = 2008, day = 01, month = 01)
;     IDL> serie = MAKE_TIME_SERIE(startTime, NSTEPS = 4, TIMESTEP=TIMESTEP)
;     IDL> for i=0, N_ELEMENTS(serie) - 1 do print, TIME_to_STR(serie[i])
;     01.01.2008 00:00:00
;     02.01.2008 12:00:00
;     04.01.2008 00:00:00
;     05.01.2008 12:00:00
; 
;       2. Second possibility
;     IDL> startTime = QMS_TIME(year = 2008, day = 01, month = 01)
;     IDL> serie = MAKE_TIME_SERIE(startTime, NSTEPS = 5, month = -3)
;     IDL> for i=0, N_ELEMENTS(serie) - 1 do print, TIME_to_STR(serie[i])
;     01.01.2008 00:00:00
;     01.10.2007 00:00:00
;     01.07.2007 00:00:00
;     01.04.2007 00:00:00
;     01.01.2007 00:00:00
;     
; :History:
;       Written by FaM, 2009.
;-
function MAKE_TIME_SERIE, startTime, NSTEPS = nsteps, TIMESTEP=timestep, YEAR=year, MONTH=month

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
  
  if N_ELEMENTS(startTime) ne 1 then  Message, WAVE_Std_Message('startTime', /SCALAR)
  if N_ELEMENTS(nsteps) ne 1 then nsteps = 1
  
  if ~check_WTIME(startTime, OUT_QMS=t, WAS_ABSDATE=was_absdate) then Message, WAVE_Std_Message('startTime', /ARG)
  
  ;KEYWORDS Handling
  mode = 0
  
  if keyword_set(timestep) then mode = 1
  if keyword_set(year) then mode = 2
  if keyword_set(month) then mode = 3
  if keyword_set(month) and keyword_set(year) then mode = 0
  if not keyword_set(nsteps) then mode = 0
  
  
  CASE mode OF
  
    0: begin ; NOT POSSIBLE
    
      Message, WAVE_Std_Message(/NARG)
      
    end
    
    1: begin ; TIMESTEP
    
      if not arg_okay(timestep, STRUCT={TIME_STEP}) then Message, WAVE_Std_Message('timestep', STRUCT={TIME_STEP})
      if not arg_okay(nsteps, /NUMERIC) then Message, WAVE_Std_Message('nsteps', /NUMERIC)     
      if nsteps lt 1 then Message,'nsteps should be greater than zero.'
      
      serie = INDGEN(nsteps, /L64) * TIMESTEP.dms + t
      if WAS_ABSDATE then serie = MAKE_ABS_DATE(QMS = serie)     
      
    end
    
    2: begin  ; YEARLY STEP
    
      if not arg_okay(nsteps, /NUMERIC) then Message, WAVE_Std_Message('nsteps', /NUMERIC)     
      if nsteps lt 1 then Message,'nsteps should be greater than zero.'
      
      serie = MAKE_ABS_DATE(REFDATE = t, YEAR=INDGEN(nsteps) * LONG(YEAR))
      if ~ WAS_ABSDATE then serie = serie.QMS     
      
    end
    
    3: begin ; MONTHLY STEP
    
      if not arg_okay(nsteps, /NUMERIC) then Message, WAVE_Std_Message('nsteps', /NUMERIC)     
      if nsteps lt 1 then Message,'nsteps should be greater than zero.'
      
      serie = MAKE_ABS_DATE(REFDATE = t, MONTH=INDGEN(nsteps) * LONG(month))
      if ~ WAS_ABSDATE then serie = serie.QMS  
      
    end
    
  ENDCASE

  return, serie
  
end

;+
; :Description:
;    This function returns an array containing times within a given interval 
;    of time having a choosen offset between them.
;    
;    This offset can be positive or negative, and regular (daily, hourly, etc.) as well as
;    date dependant (monthly, yearly).
;       
; :Categories:
;    General/Time
;
; :Params:
;    startTime:  in, required , type={ABS_DATE}/qms vector, default = none
;      The first time of the serie
;    endTime:  in, required , type={ABS_DATE}/qms vector, default = none
;      The end time of the serie
;      
; :Keywords:
;    TIMESTEP: in, optional, type={TIME_STEP}, default=none
;           The timestep
;    YEAR: in, optional, type=integer, default=0
;           The timestep in number of years (if set, `TIMESTEP` is ignored)
;    MONTH: in, optional, type=integer, default=0
;           The timestep in number of months (if set, `TIMESTEP` is ignored)
;    QMSTIME: in, optional, type=boolean, default=0
;           If the output is to be written in qms instead of {ABS_DATE} (better)
;    NSTEPS: out, optional, type=integer, default=none
;           The number of elements of the time serie
;
; :Returns:
;    A nsteps elements array of qms or {ABS_DATE}. result[0] is allways equal to startTime
;    but result[nstep-1] is the closest time le the given endTime.
;
; :Examples:
; 
;  Create a time serie::
;  
;       1. First possibility
;      IDL> startTime = make_abs_date(YEAR=2008, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
;      IDL> endTime = make_abs_date(YEAR=2008,  MONTH=01, DAY=01, HOUR=04, MINUTE=00, SECOND=00)
;      IDL> TIMESTEP = MAKE_TIME_STEP(hour = 1, minute = 1)
;      IDL> serie = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=TIMESTEP, nsteps =nsteps)
;      IDL> for i=0, nsteps-1 do print, TIME_to_STR(serie[i])
;      01.01.2008 00:00:00
;      01.01.2008 01:01:00
;      01.01.2008 02:02:00
;      01.01.2008 03:03:00
;      
;       2. Second possibility
;      IDL> startTime = make_abs_date(YEAR=2008, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
;      IDL> endTime = make_abs_date(YEAR=2004,  MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
;      IDL> serie = MAKE_ENDED_TIME_SERIE(startTime, endTime, nsteps =nsteps, year = -1)
;      IDL> for i=0, nsteps-1 do print, TIME_to_STR(serie[i])
;      01.01.2008 00:00:00
;      01.01.2007 00:00:00
;      01.01.2006 00:00:00
;      01.01.2005 00:00:00
;      01.01.2004 00:00:00
;      
; :History:
;       Written by FaM, 2009.
;-
function MAKE_ENDED_TIME_SERIE, startTime, endTime, TIMESTEP=timestep, NSTEPS = nsteps, YEAR = year, MONTH = month, QMSTIME = qmstime

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  if ~check_WTIME(startTime, OUT_QMS=t1) then Message, WAVE_Std_Message('startTime', /ARG)
  if ~check_WTIME(endTime, OUT_QMS=t2) then Message, WAVE_Std_Message('endTime', /ARG)
      
  if t1 le t2 then sign = 1 else sign = -1
  
  if KEYWORD_SET(timestep) then begin
  
    if not arg_okay(timestep, STRUCT={TIME_STEP}) then Message, WAVE_Std_Message('timestep', STRUCT={TIME_STEP})    
    if sign ne LONG(ABS(timestep.dms)/timestep.dms) then  Message, '$timestep not compatible with start and end times'
    
    nsteps = ABS((t2 - t1) / timestep.dms) + 1
    qms = INDGEN(nsteps, /L64) * timestep.dms + t1
    if KEYWORD_SET(QMSTIME) then serie = qms else serie = MAKE_ABS_DATE(qms = qms)
          
  endif else if KEYWORD_SET(month) then begin
  
    if sign ne LONG(ABS(month)/month) then Message, '$month not compatible with start and end times'
    
    i = 0
    stopFlag = 0
    serie = MAKE_ABS_DATE(QMS = t1)
    
    while stopFlag eq 0 do begin
    
      next = MAKE_REL_DATE(serie[i], MONTH = month)
      if next.qms lt t2 then compare = 1 else if next.qms eq t2 then compare = 0 else compare = -1
      
      if compare eq sign then serie = [serie, next]      
      if compare eq 0 then begin
        serie = [serie, next]
        stopFlag = 1
      endif      
      if compare ne sign then stopFlag = 1
      
      i+=1l
      
    endwhile
    
    if KEYWORD_SET(QMSTIME) then serie = serie.qms
    nsteps = n_elements(serie)
    
  endif else if KEYWORD_SET(year) then begin
  
    if sign ne LONG(ABS(year)/year) then Message, '$year not compatible with start and end times'
    
    i = 0
    stopFlag = 0
    serie = MAKE_ABS_DATE(QMS = t1)
    
    while stopFlag eq 0 do begin
    
      next = MAKE_REL_DATE(serie[i], YEAR = year)
      if next.qms lt t2 then compare = 1 else if next.qms eq t2 then compare = 0 else compare = -1
     
      if compare eq sign then serie = [serie, next]
      if compare eq 0 then begin
        serie = [serie, next]
        stopFlag = 1
      endif
      
      if compare ne sign then stopFlag = 1
      
      i+=1l
      
    endwhile
    
    if KEYWORD_SET(QMSTIME) then serie = serie.qms
    nsteps = n_elements(serie)
    
  endif else begin
    Message, WAVE_Std_Message(/NARG)
  endelse

  return, serie
  
end

;+
; :Description:
;    
;    This function checks if a time serie is regular and returns TRUE if it is.
;    The time step is computed and returned if desired. If the time serie is
;    regular but not sorted, the function also returns TRUE and sorts the array in
;    FULL_TS
;    
;    If the time serie has gaps, FALSE is returned. A "probable" time step 
;    is computed (most occurences using histogramm). Be carefull: if the 
;    time serie is too short or "really" irregular, the timestep computed
;    this way will have no signification. 
;    
;    Moreover, this function can compute the supposed complete time serie 
;    and indicate where the gaps are found (usefull if one wants to interpolate
;    the data afterwards).
;       
; :Categories:
;    General/Time
;    
; :Params:
;    ts: in, required , type={ABS_DATE}/qms vector, default = none
;        The time serie to check
;    timestep: out, optional, type={TIME_STEP}, default=none
;        The "probable" timestep
;
; :Keywords:
;    FULL_TS: out, optional, type={ABS_DATE}/qms vector, default=none
;         The "probable" complete time serie, of the same type as `ts`
;    IND_MISSING: out, optional, type=integer vector, default=none
;         The indexes in `FULL_TS` where `ts` is incomplete. -1 if 
;         the ts is regular.
;
; :Returns:
;    TRUE if the time serie is regular, FALSE if not
;
; :Examples:
; 
;  Create a time serie, check it, remove some steps and repair it::
;  
;    IDL> startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
;    IDL> endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=03, HOUR=00, MINUTE=01, SECOND=00)
;    IDL> step = MAKE_TIME_STEP(hour = 1)
;    IDL> goodTS = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
;    mydata = INDGEN(nsteps)^2
;    
;    IDL> if check_TimeSerie(goodTS, probableStep) eq TRUE then print, 'TS is regular' else print, 'TS is NOT regular'
;    TS is regular
;    IDL> help, PROBABLESTEP, /STR
;    ** Structure TIME_STEP, 6 tags, length=32, data length=28:
;       DAY             LONG                 0
;       HOUR            LONG                 1
;       MINUTE          LONG                 0
;       SECOND          LONG                 0
;       MILLISECOND     LONG                 0
;       DMS             LONG64                   3600000
;       
;       IDL> badTS = [goodTS[0:15],goodTS[25:*]]
;       IDL> badData = [mydata[0:15],mydata[25:*]]
;       IDL> if check_TimeSerie(badTS, probableStep, FULL_TS=fullTS, IND_MISSING=missing) eq TRUE then print, 'TS is regular' else print, 'TS is NOT regular'
;       TS is NOT regular
;       IDL> help, PROBABLESTEP, /STR
;       ** Structure TIME_STEP, 6 tags, length=32, data length=28:
;          DAY             LONG                 0
;          HOUR            LONG                 1
;          MINUTE          LONG                 0
;          SECOND          LONG                 0
;          MILLISECOND     LONG                 0
;          DMS             LONG64                   3600000
;       IDL> print, MISSING
;                 16          17          18          19          20          21          22          23          24       
;       IDL> reconstructedData = INTERPOL(badData, badTS.qms, fullTS.qms)
;       IDL> plot, MYDATA, color = cgCOLOR('blue'), BACKGROUND = cgCOLOR('white')
;       IDL> oplot, RECONSTRUCTEDDATA, color = cgCOLOR('red'), LINESTYLE = 2
;    
;
; :History:
;       Written by FaM, 2009.
;-
function check_TimeSerie, ts, timestep, FULL_TS = full_ts, IND_MISSING = IND_missing

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  if N_PARAMS() lt 1 then Message, WAVE_Std_Message(/NARG)
  if N_ELEMENTS(ts) lt 2 then Message, WAVE_Std_Message(/NARG)
  
  if ~check_WTIME(ts, OUT_QMS=mytime, WAS_ABSDATE=was_Str) then Message, WAVE_Std_Message('ts', /ARG)
  
  mytime = mytime[SORT(mytime)]
  n = N_ELEMENTS(mytime)
  steps = mytime[1:n-1] - mytime[0:n-2]
  h = HISTOGRAM(steps, omin = om, /L64, BINSIZE=min(steps))
  m = MAX(h, p)
  steps = steps - p - om
  timestep = MAKE_TIME_STEP(DMS = p + om)
  if TOTAL(steps) eq 0 then ret = TRUE else ret = FALSE
  
  if ARG_PRESENT(FULL_TS) or ARG_PRESENT(IND_missing) then begin
  
    if ret eq TRUE then begin
    
     full_TS = mytime
     IND_missing = -1
     
    endif else begin
    
      full_TS = MAKE_ENDED_TIME_SERIE(mytime[0], mytime[n-1], TIMESTEP = timestep, /QMSTIME)
      s = VALUE_LOCATE(mytime, full_TS) < (n-1) ;Subscript intervals.
      sd = s[1:*] - s[0:N_ELEMENTS(s)-2]
      p1 = WHERE(sd eq 0, cnt)
      p2 = WHERE(s eq -1, cnt)
  
      indexes = [p1,p2]
      indexes = indexes[UNIQ(indexes, SORT(indexes))]
  
      p = WHERE(INDEXES ne -1, cnt)
      if cnt ne 0 then IND_missing = INDEXES[p] + 1 else IND_missing = -1
      
    endelse  
    
  endif
  
  if was_Str then full_TS = MAKE_ABS_DATE(QMS=FULL_TS)
  
  return, ret
  
end

;+
; :Description:
;    This function checks if a time (e.g. a parameter of a procedure) is 
;    in a Format that WAVE can understand and retrieves the time in one 
;    of the both formats (qms or {ABS_DATE}) if desired. 
;       
; :Categories:
;    General/Time
;
; :Params:
;    time: in, required , type={ABS_DATE}/qms vector, default = none
;          the parameter to check
;
;      
; :Keywords:
;    OUT_QMS: out, optional, type=qms, default=none
;             if set to a named variable, returns the 'time' in qms
;    OUT_ABSDATE: out, optional, type={ABS_DATE}, default=none
;                 if set to a named variable, returns the 'time' in {ABS_DATE}
;    WAS_ABSDATE: out, optional, type=boolean, default=none
;                 if set to a named variable, returns True if 'time' is a {ABS_DATE} or False in all other cases
;    WAS_QMS: out, optional, type=boolean, default=none
;             if set to a named variable, returns True if 'time' is a QMS or False in all other cases
;
; :Returns:
;    TRUE if the 'time' is a good WAVE time format, FALSE in all other cases.
;
;
; :History:
;       Written by FaM, 2010.
;-
function check_WTIME, time, OUT_QMS = OUT_QMS, OUT_ABSDATE = out_absdate, WAS_ABSDATE = was_absdate, WAS_QMS = was_qms

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ; Standard error handling.
  ON_ERROR, 2
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /CANCEL
;    void = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF  
  
  was_qms = FALSE
  was_absdate = FALSE
  
  if arg_okay(time, /NUMERIC) then begin
    if ARG_PRESENT(OUT_QMS) then OUT_QMS = time
    if ARG_PRESENT(OUT_ABSDATE) then OUT_ABSDATE = MAKE_ABS_DATE(QMS = time)
    was_qms = TRUE
    return, TRUE
  endif 
  
  if arg_okay(time, STRUCT={ABS_DATE}) then begin
    if ARG_PRESENT(OUT_QMS) then OUT_QMS = time.qms
    if ARG_PRESENT(OUT_ABSDATE) then OUT_ABSDATE = time
    was_absdate = TRUE
    return, TRUE
  endif
  
  ; If we are here there is a problem
  OUT_QMS = QMS_TIME()
  out_absdate = MAKE_ABS_DATE()
  return, FALSE

end

;+
; :Description:
;    Simple function to search occurences in a time serie (like where())
;    but a bit more flexible
;
; :Params:
;    ts: in, required, type = {ABS_DATE}/qms vector
;        time serie to look into
;    time: in, required, type = {ABS_DATE}/qms scalar
;        time to find in the serie
;
; :Keywords:
;    pos: out
;         the index(es) where the time has been found (-1 if not found)
;    cnt: out
;         the number of index(es) found
; :Returns:
;    TRUE if the time was found, FALSE if not.
;     
; :History:
;     Written by FaM, 2011.
;-
function search_Wtime, ts, time, pos = pos, cnt = cnt
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ; Standard error handling.
  ON_ERROR, 2
  
  if ~check_WTIME(ts, OUT_QMS=myts) then Message, WAVE_Std_Message('ts', /ARG)
  if ~check_WTIME(time, OUT_QMS=mytime) then Message, WAVE_Std_Message('time', /ARG)
  if N_ELEMENTS(mytime) ne 1 then Message, WAVE_Std_Message('time', NELEMENTS = 1)
  
  pos = where(myts eq mytime, cnt)
  
  if cnt ge 1 then return, TRUE else return, FALSE
  
end

;+
; :Description:
;    This function fills gaps in a time serie with a user defined or NaN values.
;    It is based on the IDL value_locate function, which is pretty fast.
;
; :Params:
;    data: in, required, type = numeric
;          the data serie to complete
;    time: in, required, type = {ABS_DATE}/qms
;          the time serie associated to data
;    new_time: in, required, type = {ABS_DATE}/qms
;              the output time serie (complete)
;              
; :Keywords:
;    FILL_VALUE: in, optional, default = !VALUES.F_NAN
;                the value to insert into the gaps
;    INDEXES: out, type = long
;             the indexes in 'new_time' where the gaps ere filled (-1 if the time series was complete)           
; 
; :Returns:
;    An array of same size as 'new_time' similar to 'data' but with gaps filled.
;  
; :History:
;     Written by FaM, 2011.
;-
function TS_FILL_MISSING, data, time, new_time, FILL_VALUE =  fill_value, INDEXES = indexes

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC) then message, WAVE_Std_Message('data', /NUMERIC)
  if ~ check_WTIME(time, OUT_QMS=qms1) then message, WAVE_Std_Message('time', /ARG)
  if ~ check_WTIME(new_time, OUT_QMS=qms2) then message, WAVE_Std_Message('new_time', /ARG)
   
  n = N_elements(data) 
  if n_elements(qms1) ne n then message, 'data and time arrays must have same number of elements'
  
  s = VALUE_LOCATE(qms1, qms2) < (n-1) ;Subscript intervals
  If N_ELEMENTS(FILL_VALUE) eq 0 then FILL_VALUE = !VALUES.F_NAN
  
  out = data[s > 0]  
  sd = s[1:*] - s[0:N_ELEMENTS(s)-2]
  
  indexes = -1
  
  p1 = WHERE(sd eq 0, cnt)
  if cnt ne 0 then begin
   out[p1+1] = FILL_VALUE 
   indexes = [indexes, p1+1]
  endif
  
  p2 = WHERE(s eq -1, cnt)
  if cnt ne 0 then begin
   out[p2] = FILL_VALUE 
   indexes = [indexes, p2]
  endif  
  
  if N_ELEMENTS(indexes) gt 1 then indexes = indexes[1:*]
  indexes = indexes[UNIQ(indexes, SORT(indexes))]
  
  RETURN, out

end

;+
; :Description:
;    Computes interval mean values and other statistics from a time serie. 
;    Both input and output time series can be irregular. You can compute hourly 
;    means by e.g. setting the `HOUR` keyword to 1, the first and last time
;    being computed automatically, or by setting the desired output
;    time serie using the `NEW_TIME` keyword (only way to obtain an irregular
;    output time serie).
;    
;    !CAREFULL: it can be confusing: The value at 14:00 is the mean value from 13:01 to 14:00 !
;
; :Params:
;    data: in, required, type = array
;          the data serie to analyse
;    time: in, required, type = {ABS_DATE}/qms
;          the associated time (same size as data)
;
; :Keywords:
;    MISSING: in, optional, default = NaN
;             if no value is found within an interval, the missing
;             value is assigned the the statistics
;    DAY: in, optional, default = none
;         set to an day interval (e.g: 1, or 7) to compute 
;         daily or seven-daily statistics
;    HOUR: in, optional, default = none
;         set to an hourly interval (e.g: 1, or 6) to compute 
;         hourly or six-hourly statistics
;    NEW_TIME: in, optional, type = {ABS_DATE}/qms ,default = none
;              ignored i `DAY` or `HOUR` are set. set this value to 
;              any time serie of n+1 elements. The ouptut will contain
;              n elements of the statistics for each interval [t, t+1]
;              (t excluded) 
; 
; :Returns:
;     A structure of the form::
;     
;        {nt: number of elements in the time serie
;         time: the time in qms
;         mean: the mean value for each time interval
;         min: the min value for each time interval
;         max: the max value for each time interval
;         tot: the sum of all values for each time interval
;         nel: the number of elements found in each interval
;         stddev: the standard deviation of the data within the intervals
;         }
;
; :History:
;     Written by FaM, 2011.
;-
function TS_MEAN_STATISTICS, data, time, MISSING = missing, $
           DAY = day, HOUR = hour, NEW_TIME = new_time


  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC, /ARRAY) then message, WAVE_Std_Message('data', /ARG)
  if ~ check_WTIME(time, OUT_QMS=qms1, WAS_ABSDATE=was_absdate) then message, WAVE_Std_Message('time', /ARG)
 
  n = n_elements(qms1)
  if ~ array_processing(qms1, data, REP_A1=_data) then message, '$DATA and $TIME arrays must have same number of elements'
  
  sor = SORT(qms1)
  qms1 = qms1[sor]
  _data = _data[sor]
    
  if KEYWORD_SET(hour) or KEYWORD_SET(day) then begin  
  
    if KEYWORD_SET(hour) then qms = H_QMS * LONG64(HOUR) $
     else if KEYWORD_SET(day) then qms = D_QMS * LONG64(DAY) 
    
    qmstart = FLOOR((qms1[0]-1LL) / double(qms)) * qms
    qmsend = CEIL(qms1[n-1] / double(qms)) * qms
    qms2 = qmstart + INDGEN((qmsend-qmstart )/qms + 1) * qms   
    
    regular = TRUE
    
  endif else if check_WTIME(new_time, OUT_QMS=qms2) then begin
    
    if N_ELEMENTS(new_time) lt 2 then MESSAGE, '$NEW_TIME must have at least two elements.'
    qms2 = qms2[sort(qms2)]
    
    regular = check_TimeSerie(qms2)
    
  endif else message, 'One of the positionnal keywords must be set.'  
  
  if not KEYWORD_SET(MISSING) then begin
    dataTypeName = Size(data, /TNAME)
    CASE dataTypeName OF
        'FLOAT': MISSING = !VALUES.F_NAN
        'DOUBLE': MISSING = !VALUES.D_NAN
         else: missing = -999
    endcase
  endif
  
  regular = false ;TODO: Update routine: implement regular with hitogram
  
  if regular then begin
  
  endif else begin
      
      s = VALUE_LOCATE(qms1, qms2)
      
      nnt = N_ELEMENTS(qms2)
      means = REPLICATE(data[0], nnt-1) * 0
      maxs = means & mins = means
      tots = means & nels = LONG(means) & stddevs = means
      
      for i = 0,  N_ELEMENTS(s) - 2 do begin
        a = s[i]+1
        b = s[i+1]
        if a le b then begin
          means[i] = MEAN(data[a:b] , /NAN)
          mins[i] = MIN(data[a:b], MAX=m, /NAN)
          maxs[i] = m
          tots[i] = TOTAL(data[a:b], /NAN)
          stddevs[i] = stddev(data[a:b], /NAN)
          dumm = where(FINITE(data[a:b]) eq 1, cnt)
          nels[i] = cnt
        endif else begin
          means[i] =  missing
          mins[i] =  missing
          maxs[i] =  missing
          tots[i] =  missing
          stddevs[i] =  missing
          nels[i] =  0
        endelse
      endfor
      
      qms2 = qms2[1: nnt-1]

  endelse
  
  RETURN, {nt: N_ELEMENTS(qms2), $
           time:qms2, $
           mean:means, $
           min:mins, $
           max:maxs, $
           tot:tots, $
           nel:nels,$ 
           stddev:stddevs}

end

;+
; :Description:
;    Computes interval mean values and other statistics from a gridded time serie. 
;    Both input and output time series can be irregular. You can compute hourly 
;    means by e.g. setting the `HOUR` keyword to 1, the first and last time
;    being computed automatically, or by setting the desired output
;    time serie using the `NEW_TIME` keyword (only way to obtain an irregular
;    output time serie).
;    
;    The last dimension of the variable (usually 3 or 4) will be understood as the time dimension.
;    
;    !CAREFULL: it can be confusing: The value at 14:00 is the mean value from 13:01 to 14:00 !
;
; :Params:
;    data: in, required, type = array
;          the data
;    time: in, required, type = {ABS_DATE}/qms
;          the associated time (same size as the last dimension of data)
;
; :Keywords:
;    MISSING: in, optional, default = NaN
;             if no value is found within an interval, the missing
;             value is assigned the the statistics
;    DAY: in, optional, default = none
;         set to an day interval (e.g: 1, or 7) to compute 
;         daily or seven-daily statistics
;    HOUR: in, optional, default = none
;         set to an hourly interval (e.g: 1, or 6) to compute 
;         hourly or six-hourly statistics
;    NEW_TIME: in, optional, type = {ABS_DATE}/qms ,default = none
;              ignored i `DAY` or `HOUR` are set. set this value to 
;              any time serie of n+1 elements. The ouptut will contain
;              n elements of the statistics for each interval [t, t+1]
;              (t excluded) 
; 
; :Returns:
;     A structure of the form::
;     
;        {nt: number of elements in the time serie
;         time: the time in qms
;         mean: the mean value for each time interval
;         min: the min value for each time interval
;         max: the max value for each time interval
;         tot: the sum of all values for each time interval
;         nel: the number of elements found in each interval
;         stddev: the standard deviation of the data within the intervals
;         }
;
; :History:
;     Written by FaM, 2011.
;-
function TS_GRID_STATISTICS, data, time, MISSING = missing, $
           DAY = day, HOUR = hour, NEW_TIME = new_time

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
;  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC) then message, WAVE_Std_Message('data', /ARG)
  if ~ check_WTIME(time, OUT_QMS=qms1, WAS_ABSDATE=was_absdate) then message, WAVE_Std_Message('time', /ARG)
 
  n = n_elements(qms1)
  siz = SIZE(data)
  ndims = siz[0]
  if ndims lt 2 and ndims gt 4 then message, WAVE_Std_Message('data', /DIMARRAY)
  if ndims eq 2 then begin
   if N_ELEMENTS(qms1) ne 1 then message, '$DATA and $TIME arrays do not match' 
  endif else if N_ELEMENTS(qms1) ne siz[ndims] then message, '$DATA and $TIME arrays do not match'
  
  sor = SORT(qms1)
  qms1 = qms1[sor]
  if ndims eq 3 then _data = data[*,*,sor] else data = data[*,*,*,sor]
    
  if KEYWORD_SET(hour) or KEYWORD_SET(day) then begin  
  
    if KEYWORD_SET(hour) then qms = H_QMS * LONG64(HOUR) $
     else if KEYWORD_SET(day) then qms = D_QMS * LONG64(DAY) 
    
    qmstart = FLOOR((qms1[0]-1LL) / double(qms)) * qms
    qmsend = CEIL(qms1[n-1] / double(qms)) * qms
    qms2 = qmstart + INDGEN((qmsend-qmstart )/qms + 1) * qms   
    
    regular = TRUE
    
  endif else if check_WTIME(new_time, OUT_QMS=qms2) then begin
    
    if N_ELEMENTS(new_time) lt 2 then MESSAGE, '$NEW_TIME must have at least two elements.'
    qms2 = qms2[sort(qms2)]
    
    regular = check_TimeSerie(qms2)
    
  endif else message, 'One of the positionnal keywords must be set.'  
  
  if not KEYWORD_SET(MISSING) then begin
    dataTypeName = Size(data, /TNAME)
    CASE dataTypeName OF
        'FLOAT': MISSING = !VALUES.F_NAN
        'DOUBLE': MISSING = !VALUES.D_NAN
         else: missing = -999
    endcase
  endif
  
  regular = false ;TODO: Update routine: implement regular with histogram
  
  if regular then begin
  
  endif else begin
  
    s = VALUE_LOCATE(qms1, qms2)
        
    nnt = N_ELEMENTS(qms2)
    if ndims eq 3 then means = REPLICATE(data[0], siz[1], siz[2], nnt-1) * 0 $
    else means = REPLICATE(data[0], siz[1], siz[2], siz[3],nnt-1) * 0
    maxs = means & mins = means
    tots = means & nels = LONG(means) & stddevs = means
    
    for i = 0,  N_ELEMENTS(s) - 2 do begin
      a = s[i]+1
      b = s[i+1]
      
      if ndims eq 3 then begin
      
        if a le b then begin
          tdata = data[*,*,a:b]
          tots[*,*,i] = TOTAL(tdata, 3, /NAN)
          nels[*,*,i] = TOTAL(FINITE(tdata), 3)
          means[*,*,i] = tots[*,*,i]/nels[*,*,i]
          mins[*,*,i] = MIN(tdata, MAX=m, DIMENSION=3, /NAN)
          maxs[*,*,i] = m
          stddevs[*,*,i] = utils_SIG_ARRAY(tdata, 3)          
        endif else begin
          means[*,*,i] =  missing
          mins[*,*,i] =  missing
          maxs[*,*,i] =  missing
          tots[*,*,i] =  missing
          stddevs[*,*,i] =  missing
          nels[*,*,i] =  0
        endelse
        
      endif else begin

        if a le b then begin
          tdata = data[*,*,*,a:b]
          tots[*,*,*,i] = TOTAL(tdata, 4, /NAN)
          nels[*,*,*,i] = TOTAL(FINITE(tdata), 4)
          means[*,*,*,i] = tots[*,*,*,i]/nels[*,*,*,i]
          mins[*,*,*,i] = MIN(tdata, MAX=m, DIMENSION=4, /NAN)
          maxs[*,*,*,i] = m
          stddevs[*,*,*,i] = utils_SIG_ARRAY(tdata, 4)          
        endif else begin
          means[*,*,*,i] =  missing
          mins[*,*,*,i] =  missing
          maxs[*,*,*,i] =  missing
          tots[*,*,*,i] =  missing
          stddevs[*,*,*,i] =  missing
          nels[*,*,*,i] =  0
        endelse
     
      endelse
    endfor
    
    qms2 = qms2[1: nnt-1]
    
  endelse

  RETURN, {nt: N_ELEMENTS(qms2), $
           time:qms2, $
           mean:means, $
           min:mins, $
           max:maxs, $
           tot:tots, $
           nel:nels,$ 
           stddev:stddevs}

end



function TS_RESAMPLE, data, time, $
           DAY = day, HOUR = hour, M10 = m10, NEW_TIME = new_time

  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC, /ARRAY) then message, WAVE_Std_Message('data', /ARG)
  if ~ check_WTIME(time, OUT_QMS=qms1, WAS_ABSDATE=was_absdate) then message, WAVE_Std_Message('time', /ARG)
 
  n = n_elements(qms1)
  if ~ array_processing(qms1, data, REP_A1=_data) then message, '$DATA and $TIME arrays must have same number of elements'
  
  sor = SORT(qms1)
  qms1 = qms1[sor]
  _data = _data[sor]
 
  if ~ check_TimeSerie(qms1, its) then message, '$TIME not regular'
  iqms = its.dms  
  qms1 = qms1 - iqms
 
  if KEYWORD_SET(hour) or KEYWORD_SET(day) or KEYWORD_SET(M10) then begin    
    if KEYWORD_SET(hour) then oqms = H_QMS $
     else if KEYWORD_SET(day) then oqms = D_QMS $
       else if KEYWORD_SET(m10) then oqms = M_QMS * 10LL 
    
    qmstart = FLOOR((qms1[0]-1LL) / double(oqms)) * oqms
    qmsend = (CEIL((qms1[n-1] + iqms) / double(oqms)) - 1) * oqms
    qms2 = qmstart + INDGEN((qmsend-qmstart)/oqms + 1) * oqms + oqms
       
    regular = TRUE
    
  endif else if check_WTIME(new_time, OUT_QMS=qms2) then begin
  
    if ~check_TimeSerie(qms2, tso) then MESSAGE, '$NEW_TIME not regular'
    if qms2[0] le qms1[0] then Message, '$NEW_TIME[0] out of range'
    if qms2[N_ELEMENTS(qms2)-1] gt qms1[n-1] + iQMS then Message, '$NEW_TIME[1] out of range'
    
    oqms = tso.dms
    qms2 = [qms2[0] - oqms, qms2]
    
  endif else message, 'One of the positionnal keywords must be set.' 
  
  s = VALUE_LOCATE(qms1,qms2) < (n-1) ;Subscript intervals.
  
;  sample = 0D  
;  for i = 0, N_ELEMENTS(newtime) - 1 do sample = [sample,value[s[i]]]
  sample = _data[s]
  
  sample = sample[0: N_ELEMENTS(sample)-2]
  qms2 = qms2[1: N_ELEMENTS(qms2)-1] 
  if N_ELEMENTS(qms2) ne N_ELEMENTS(sample) then message, 'oups'
  
  RETURN, {data:sample,time:qms2,nt:N_ELEMENTS(qms2)} 

end


;+
; :Description:
;    This function fits two time series together. If needed, mean values are built,
;    and time periods are selected to match to two together. Timeseries must be 
;    regular. 
;    Carefull: the function modifies the input variables and doesn't throw any error.
;    It returns 1 if the fitting was successfull (or if no fitting was needed), and 
;    0 if something prevented the fitting. Set the verbose keword if you want to know 
;    why it didn't work.
;     
;
; :Params:
;    data1: the data of the ts 1 (may be modified by the function!)
;    time1: the time of the ts 1 (may be modified by the function!)
;    data2: the data of the ts 2 (may be modified by the function!)
;    time2: the time of the ts 2 (may be modified by the function!)
;
; :Keywords:
;    CUMUL: set this keyword if cumulated values have to be calculated instead of means
;    VERBOSE: set this keyword so that the function tells you what it does
;    
; :Returns:
;     1 if the fitting was successful, 0 if not
;     
; :History:
;     Written by FaM, 2011.
;
;
;-
function TS_fit_series, data1, time1,  data2, time2, CUMUL = cumul, VERBOSE = verbose
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    if KEYWORD_SET(VERBOSE) then print, 'TS_fit_series: could not fit the series: ' + !Error_State.Msg
    RETURN, FALSE
  ENDIF  
  
  if N_PARAMS() ne 4 then Message, WAVE_Std_Message(/NARG)
  
  if ~ check_WTIME(time1, OUT_QMS = qms1, WAS_ABSDATE=wasad1) then Message, 'Time1 is not ok'
  if ~ check_WTIME(time2, OUT_QMS = qms2, WAS_ABSDATE=wasad2) then Message, 'Time2 is not ok'
  
  if ~ check_TimeSerie(qms1, step1) then Message, 'Timeserie 1 is not regular'
  if ~ check_TimeSerie(qms2, step2) then Message, 'Timeserie 2 is not regular'
  if ~ array_processing(data1, time1, REP_A0=_data1) then Message, 'Data1 and time1 not matching'
  if ~ array_processing(data2, time2, REP_A0=_data2) then Message, 'Data2 and time2 not matching'
  
  dataTypeName = Size(_data1, /TNAME)
  dataTypeName_ = Size(_data2, /TNAME)
  if dataTypeName ne dataTypeName_ then message, 'The two datasets must be of the same type.'
  
  if step1.dms gt step2.dms then begin
  
    mts = [qms1[0] - step1.dms, qms1]
    stat = TS_MEAN_STATISTICS(_data2, qms2, NEW_TIME=mts)
    tel = MAX(stat.nel)
    if tel eq 0 then return, FALSE
    
    pok = where(stat.nel eq tel)
    if KEYWORD_SET(CUMUL) then _data2 = stat.total[pok] else _data2 = stat.mean[pok]
    qms2 = stat.time[pok]
      
    if ~ check_TimeSerie(qms2) then Message, 'Mean values not regular'
    if KEYWORD_SET(VERBOSE) then begin
      sst = '{Day: ' + str_equiv(step1.day) + '. Hour: ' + str_equiv(step1.hour) + '. Minute: ' + str_equiv(step1.minute) +'}'  
      print, 'TS_fit_series: TS1 was of greater time step than TS2. TS2 was adapted the the step: ' + sst
    endif  
  endif else if step1.dms lt step2.dms then begin
  
    mts = [qms2[0] - step2.dms, qms2]
    stat = TS_MEAN_STATISTICS(_data1, qms1, NEW_TIME=mts)
    tel = MAX(stat.nel)
    if tel eq 0 then return, FALSE
    
    pok = where(stat.nel eq tel)
    if KEYWORD_SET(CUMUL) then _data1 = stat.total[pok] else _data1 = stat.mean[pok]
    qms1 = stat.time[pok]
    
    if ~ check_TimeSerie(qms1) then Message, 'Mean values not regular'
    if KEYWORD_SET(VERBOSE) then begin
      sst = '{Day: ' + str_equiv(step2.day) + '. Hour: ' + str_equiv(step2.hour) + '. Minute: ' + str_equiv(step2.minute) +'}' 
      print, 'TS_fit_series: TS2 was of greater time step than TS1. TS1 was adapted the the step: ' + sst
    endif  
  endif
  
  mt0 = MIN(qms2) > MIN(qms1)
  mt1 = MAX(qms2) < MAX(qms1)
  
  p0 = where(qms1 eq mt0)
  p1 = where(qms1 eq mt1)
  data1 = _data1[p0:p1]
  time1 = qms1[p0:p1]
  p0 = where(qms2 eq mt0)
  p1 = where(qms2 eq mt1)
  data2 = _data2[p0:p1]
  time2 = qms2[p0:p1]
  
  if wasad1 then time1 = MAKE_ABS_DATE(qms=time1)
  if wasad2 then time2 = MAKE_ABS_DATE(QMS=time2)
  
  if KEYWORD_SET(VERBOSE) then print, 'TS_fit_series: success! available times: ' + TIME_to_STR(time1[0]) + ' - ' +TIME_to_STR(time1[N_ELEMENTS(time1)-1])
  
  return, 1
  
  
end

function TS_wrf_to_mean, data, time
  
  _data = data*1.
  d1 = TS_RESAMPLE(_data, time, /M10)
  d2 = INTERPOL(_data, time, d1.time)
  out = (TS_MEAN_STATISTICS(d2, d1.time, HOUR = 1)).mean
  out[0] = data[0]
  return, out

end


;+
; :Description:
;    This function computes the diurnal statistics of a time serie. It tries to guess
;    the series time step using `check_TimeSerie` and returns the statistics
;    ot the time serie based on the hour of day.  
;
; :Params:
;    data: in, required
;          the data to analyse
;    time: in, required
;          the time
;
; :Keywords:
;    MISSING: in, optional
;             value to give to missing data. Default NaN
;    
; :Returns:
;     A structure of the form::
;     
;        {nt: number of elements in the time
;         time: the time of DAY in HOURS
;         mean: the mean value for each time of day
;         min: the min value for each time of day
;         max: the max value for each time of day
;         tot: the sum of all values for each time of day
;         nel: the number of elements found in each time of day
;         stddev: the standard deviation of the data within each time of day
;         }
;
;
; :History:
;     Written by FaM, 2011.
;
;-
function TS_DIURNAL_MEANS, data, time, MISSING = missing

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC, /ARRAY) then message, WAVE_Std_Message('data', /ARG)
  if ~ check_WTIME(time, OUT_QMS=qms1, WAS_ABSDATE=was_absdate) then message, WAVE_Std_Message('time', /ARG)
  
  n = n_elements(qms1)
  if ~ array_processing(qms1, data, REP_A1=_data) then message, '$DATA and $TIME arrays must have same number of elements'
  
  sor = SORT(qms1)
  qms1 = qms1[sor]
  _data = _data[sor]
  
  ok = check_TimeSerie(qms1, tstep)
  
  dqms = 86400000ll
  timeofday = qms1 - (qms1 / dqms) * dqms
  
  if tstep.dms lt D_QMS then sqms = tstep.dms else Message, 'Timeserie not accepted for diurnal means.'
  
  if not KEYWORD_SET(MISSING) then begin
    dataTypeName = Size(data, /TNAME)
    CASE dataTypeName OF
      'FLOAT': MISSING = !VALUES.F_NAN
      'DOUBLE': MISSING = !VALUES.D_NAN
      else: missing = -999
    endcase
  endif
  
  NSTEPS = dqms / sqms
  newtime = INDGEN(NSTEPS) * sqms
  
  means = REPLICATE(data[0], NSTEPS) * 0
  maxs = means & mins = means
  tots = means & nels = LONG(means) & stddevs = means
  
  for i = 0, N_ELEMENTS(newtime) - 1 do begin
    p = where(timeofday eq newtime[i], cnt)
    if cnt ne 0 then begin
      means[i] = MEAN(data[p] , /NAN)
      mins[i] = MIN(data[p], MAX=m, /NAN)
      maxs[i] = m
      tots[i] = TOTAL(data[p], /NAN)
      stddevs[i] = stddev(data[p], /NAN)
      dumm = where(FINITE(data[p]) eq 1, cnt)
      nels[i] = cnt
    endif else begin
      means[i] =  missing
      mins[i] =  missing
      maxs[i] =  missing
      tots[i] =  missing
      stddevs[i] =  missing
      nels[i] =  0
    endelse
  endfor
  
  newtime = newtime / DOUBLE(dqms) * 24.
  
  RETURN, {nt: nsteps, $
    time:newtime, $
    mean:means, $
    min:mins, $
    max:maxs, $
    tot:tots, $
    nel:nels,$
    stddev:stddevs}
    
    
end

;+
; :Description:
;    This function computes the diurnal statistics of a time serie. It tries to guess
;    the series time step using `check_TimeSerie` and returns the statistics
;    ot the time serie based on the hour of day.  
;
; :Params:
;    data: in, required
;          the data to analyse
;    time: in, required
;          the time
;
; :Keywords:
;    MISSING: in, optional
;             value to give to missing data. Default NaN
;    
; :Returns:
;     A structure of the form::
;     
;        {nt: number of elements in the time
;         time: the time of 7 DAYs in HOURS
;         mean: the mean value for each time of day
;         min: the min value for each time of day
;         max: the max value for each time of day
;         tot: the sum of all values for each time of day
;         nel: the number of elements found in each time of day
;         stddev: the standard deviation of the data within each time of day
;         }
;
;
; :History:
;     Written by FaM, 2011.
;
;-
function TS_7DIURNAL_MEANS, data, time, MISSING = missing

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if ~ arg_okay(data, /NUMERIC, /ARRAY) then message, WAVE_Std_Message('data', /ARG)
  if ~ check_WTIME(time, OUT_QMS=qms1, WAS_ABSDATE=was_absdate) then message, WAVE_Std_Message('time', /ARG)
  
  n = n_elements(qms1)
  if ~ array_processing(qms1, data, REP_A1=_data) then message, '$DATA and $TIME arrays must have same number of elements'
  
  sor = SORT(qms1)
  qms1 = qms1[sor]
  _data = _data[sor]
  
  ok = check_TimeSerie(qms1, tstep)
  
  dqms = 7LL*86400000ll
  timeofday = qms1 - (qms1 / dqms) * dqms
  
  if tstep.dms lt D_QMS then sqms = tstep.dms else Message, 'Timeserie not accepted for diurnal means.'
  
  if not KEYWORD_SET(MISSING) then begin
    dataTypeName = Size(data, /TNAME)
    CASE dataTypeName OF
      'FLOAT': MISSING = !VALUES.F_NAN
      'DOUBLE': MISSING = !VALUES.D_NAN
      else: missing = -999
    endcase
  endif
  
  NSTEPS = dqms / sqms
  newtime = INDGEN(NSTEPS) * sqms
  
  means = REPLICATE(data[0], NSTEPS) * 0
  maxs = means & mins = means
  tots = means & nels = LONG(means) & stddevs = means
  
  for i = 0, N_ELEMENTS(newtime) - 1 do begin
    p = where(timeofday eq newtime[i], cnt)
    if cnt ne 0 then begin
      means[i] = MEAN(data[p] , /NAN)
      mins[i] = MIN(data[p], MAX=m, /NAN)
      maxs[i] = m
      tots[i] = TOTAL(data[p], /NAN)
      stddevs[i] = stddev(data[p], /NAN)
      dumm = where(FINITE(data[p]) eq 1, cnt)
      nels[i] = cnt
    endif else begin
      means[i] =  missing
      mins[i] =  missing
      maxs[i] =  missing
      tots[i] =  missing
      stddevs[i] =  missing
      nels[i] =  0
    endelse
  endfor

  newtime = newtime / DOUBLE(dqms) * 24.*7.
  
  RETURN, {nt: nsteps, $
    time:newtime, $
    mean:means, $
    min:mins, $
    max:maxs, $
    tot:tots, $
    nel:nels,$
    stddev:stddevs}   
    
end

