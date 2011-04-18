function TEST_file_directory, RESET = reset

  common W_TEST_CMN, TEST_CMN_ROOT_DIR
  ; Put the WAVE test pack path here.
  
  if N_ELEMENTS(TEST_CMN_ROOT_DIR) eq 0  or KEYWORD_SET(RESET) then TEST_CMN_ROOT_DIR = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/'
  
  if  ~FILE_TEST(TEST_CMN_ROOT_DIR) then TEST_CMN_ROOT_DIR = DIALOG_PICKFILE(TITLE='Please indicate the test directory', /MUST_EXIST, /DIRECTORY)
  
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/WRF/') then TEST_CMN_ROOT_DIR = TEST_file_directory( /RESET)
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/MAPPING/') then TEST_CMN_ROOT_DIR = TEST_file_directory( /RESET)
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/MODIS/') then TEST_CMN_ROOT_DIR = TEST_file_directory( /RESET)
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/TRMM/') then TEST_CMN_ROOT_DIR = TEST_file_directory( /RESET)
  
  return, TEST_CMN_ROOT_DIR
  
end

pro TEST_MAKE_ABS_DATE

  error= 0
  
  ;-------------------------
  ; Test normal construction
  ;-------------------------
  time = make_abs_date(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59, MILLISECOND = 999)
  if TIME_to_STR(time) ne '31.12.1984 23:59:59' then error+=1
  if time.millisecond ne 999 then error+=1
  
  time = make_abs_date(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59, MILLISECOND = 500)
  if TIME_to_STR(time) ne '31.12.1984 23:59:59' then error+=1
  
  jt = JULDAY(12,31,1984,23,59,59)
  GEN_julian_time, ret,jt,td  
  if abs(td.qt -  time[0].qt) gt 1e-5 then  error+=1
  
  time = make_abs_date(YEAR=1985, MONTH=1, DAY=1, HOUR=0, MINUTE=0, SECOND=0, MILLISECOND = 0)
  if TIME_to_STR(time) ne '01.01.1985 00:00:00' then error+=1
  
  time = make_abs_date(YEAR=[1985,1986], MONTH=1, DAY=1, HOUR=0, MINUTE=0, SECOND=0, MILLISECOND = 0)
  if TIME_to_STR(time[0]) ne '01.01.1985 00:00:00' then error+=1
  if TIME_to_STR(time[1]) ne '01.01.1986 00:00:00' then error+=1  
  
  time = make_abs_date(YEAR=1985, MONTH=1, DAY=1, HOUR=[0,3,6], MINUTE=0, SECOND=0, MILLISECOND = 0)
  timestr = TIME_to_STR(time)
  if timestr[0] ne '01.01.1985 00:00:00' then error+=1  
  if timestr[1] ne '01.01.1985 03:00:00' then error+=1  
  if timestr[2] ne '01.01.1985 06:00:00' then error+=1

  ;-------------------------
  ; Test string construction
  ;------------------------- 
  time = make_abs_date(DATE_STR='02.11.1998')
  timestr = TIME_to_STR(time)
  if timestr ne '02.11.1998 00:00:00' then error+=1
  
  time = make_abs_date(DATE_STR=['02.11.1998','02.12.1998'], TIME_STR='05:02:58')
  timestr = TIME_to_STR(time)
  if timestr[0] ne '02.11.1998 05:02:58' then error+=1
  if timestr[1] ne '02.12.1998 05:02:58' then error+=1
  
  time = make_abs_date(DATE_STR='02.11.1998', TIME_STR=['05:02:58','06:03:57'])
  timestr = TIME_to_STR(time)
  if timestr[0] ne '02.11.1998 05:02:58' then error+=1
  if timestr[1] ne '02.11.1998 06:03:57' then error+=1
  
  jt = JULDAY(11,02,1998,05,02,58)
  GEN_julian_time, ret,jt,td  
  if abs(td.qt -  time[0].qt) gt 1e-5 then  error+=1
  
  ;-------------------------
  ; Test tnt time construction
  ;-------------------------  
  tntt = GEN_make_time(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=58, FRACTION = 0.500)
  time = make_abs_date(TNT_T=tntt)
  
  if abs(tntt.qt -  time[0].qt) gt 1e-5 then  error+=1
  if TIME_to_STR(time) ne '31.12.1984 23:59:58' then error+=1
  if time.millisecond ne 500 then error+=1
  
  
  tntt = [GEN_make_time(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=58, FRACTION = 0.500), $
          GEN_make_time(YEAR=2005, MONTH=02, DAY=24, HOUR=12, MINUTE=36, SECOND=41, FRACTION = 0.900D)]
  
  time = make_abs_date(TNT_T=tntt)
  
  if abs(tntt[0].qt -  time[0].qt) gt 1e-5 then  error+=1
  if TIME_to_STR(time[0]) ne '31.12.1984 23:59:58' then error+=1
  if time[0].millisecond ne 500 then error+=1
  
  if abs(tntt[1].qt -  time[1].qt) gt 1e-4 then  error+=1
  if TIME_to_STR(time[1]) ne '24.02.2005 12:36:41' then error+=1
  if time[1].millisecond ne 900 then error+=1
  
  ;-------------------------
  ; Test relative time construction
  ;-------------------------  
  reftime = make_abs_date(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59, MILLISECOND = 999)
  
  time = make_abs_date(REFDATE=reftime)
  if reftime.qms -  time.qms ne 0 then  error+=1
  
  time = make_abs_date(REFDATE=reftime, year = 1, day = 1, hour = 1, minute = 2, second = 3, MILLISECOND= 4)
  if TIME_to_STR(time[0]) ne '02.01.1986 01:02:03' then error+=1
  if time[0].millisecond ne 003 then error+=1
  
  reftime = [make_abs_date(YEAR=1985, MONTH=1, DAY=1, HOUR=0, MINUTE=0, SECOND=0, MILLISECOND = 0), $
             make_abs_date(YEAR=1986, MONTH=1, DAY=1, HOUR=0, MINUTE=0, SECOND=0, MILLISECOND = 0)]
  
  time = make_abs_date(REFDATE=reftime, month = 1, day = 1)   
  
  if TIME_to_STR(time[0]) ne '02.02.1985 00:00:00' then error+=1   
  if TIME_to_STR(time[1]) ne '02.02.1986 00:00:00' then error+=1   
  
  ;---------------------------
  ; Check if all tests passed?
  ;---------------------------   
  if error ne 0 then message, '% TEST_MAKE_ABS_DATE NOT passed', /CONTINUE else print, 'TEST_MAKE_ABS_DATE passed'
  
end

pro TEST_JULIAN_DAYS
   
  error= 0
  
  time = qms_Time(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59)
  jd = TIME_to_JD(time)
  
  CALDAT,jd, month,day,year,hour,minute,second    
  
  if month ne 12 then error +=1
  if year ne 1984 then error +=1
  if day ne 31 then error +=1
  if HOUR ne 23 then error +=1
  if MINUTE ne 59 then error +=1
  if floor(SECOND) ne 59 then error +=1
  
  time = MAKE_ABS_DATE(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59)
  jd = TIME_to_JD(time)
  
  CALDAT,jd, month,day,year,hour,minute,second    
  
  if month ne 12 then error +=1
  if year ne 1984 then error +=1
  if day ne 31 then error +=1
  if HOUR ne 23 then error +=1
  if MINUTE ne 59 then error +=1
  if floor(SECOND) ne 59 then error +=1
  
  if error ne 0 then message, '% TEST_JULIAN_DAYS NOT passed', /CONTINUE else print, 'TEST_JULIAN_DAYS passed'
      
end


pro TEST_QMS_TIME

  error= 0
  
  ;-------------------------
  ; Test normal construction
  ;-------------------------
  time = qms_Time(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59, MILLISECOND = 999)
  if TIME_to_STR(time) ne '31.12.1984 23:59:59' then error+=1
  if (MAKE_ABS_DATE(QMS = time)).millisecond ne 999 then error+=1
  
  time = qms_Time(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=59, MILLISECOND = 500)
  if TIME_to_STR(time) ne '31.12.1984 23:59:59' then error+=1
  
  jt = JULDAY(12,31,1984,23,59,59)
  GEN_julian_time, ret,jt,td  
  if abs(td.qt -  (MAKE_ABS_DATE(QMS = time))[0].qt) gt 1e-5 then  error+=1
  
  time = make_abs_date(YEAR=1985, MONTH=1, DAY=1, HOUR=0, MINUTE=0, SECOND=0, MILLISECOND = 0)
  if TIME_to_STR(time) ne '01.01.1985 00:00:00' then error+=1
  
  time = make_abs_date(YEAR=[1985,1986], MONTH=1, DAY=1, HOUR=0, MINUTE=0, SECOND=0, MILLISECOND = 0)
  if TIME_to_STR(time[0]) ne '01.01.1985 00:00:00' then error+=1
  if TIME_to_STR(time[1]) ne '01.01.1986 00:00:00' then error+=1  
  
  time = make_abs_date(YEAR=1985, MONTH=1, DAY=1, HOUR=[0,3,6], MINUTE=0, SECOND=0, MILLISECOND = 0)
  timestr = TIME_to_STR(time)
  if timestr[0] ne '01.01.1985 00:00:00' then error+=1  
  if timestr[1] ne '01.01.1985 03:00:00' then error+=1  
  if timestr[2] ne '01.01.1985 06:00:00' then error+=1

  ;-------------------------
  ; Test string construction
  ;------------------------- 
  time = qms_time(DATE_STR='02.11.1998')
  timestr = TIME_to_STR(time)
  if timestr ne '02.11.1998 00:00:00' then error+=1
  
  time = qms_time(DATE_STR=['02.11.1998','02.12.1998'], TIME_STR='05:02:58')
  timestr = TIME_to_STR(time)
  if timestr[0] ne '02.11.1998 05:02:58' then error+=1
  if timestr[1] ne '02.12.1998 05:02:58' then error+=1
  
  time = qms_time(DATE_STR='02.11.1998', TIME_STR=['05:02:58','06:03:57'])
  timestr = TIME_to_STR(time)
  if timestr[0] ne '02.11.1998 05:02:58' then error+=1
  if timestr[1] ne '02.11.1998 06:03:57' then error+=1
  
  jt = JULDAY(11,02,1998,05,02,58)
  GEN_julian_time, ret,jt,td  
  if abs(td.qt -  (MAKE_ABS_DATE(QMS = time))[0].qt) gt 1e-6 then  error+=1
  
  ;-------------------------
  ; Test tnt time construction
  ;-------------------------  
  tntt = GEN_make_time(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=58, FRACTION = 0.500)
  time = qms_time(TNT_T=tntt)
  
  if abs(tntt.qt -  (MAKE_ABS_DATE(QMS = time))[0].qt) gt 1e-5 then  error+=1
  if TIME_to_STR(time) ne '31.12.1984 23:59:58' then error+=1
  if (MAKE_ABS_DATE(QMS = time)).millisecond ne 500 then error+=1
  
  
  ;---------------------------
  ; Check if all tests passed?
  ;---------------------------   
  if error ne 0 then message, '% TEST_QMS_TIME NOT passed', /CONTINUE else print, 'TEST_QMS_TIME passed'
  
end

pro TEST_MAKE_REL_DATE

  ; Set Up environnement
  @TNT.inc
  COMPILE_OPT IDL2
  
  error= 0
  
  refDate = make_abs_date(YEAR=1984, MONTH=10, DAY=14, HOUR=5, MINUTE=10, SECOND=7)
  
  totest = MAKE_REL_DATE(refDate, YEAR=1)
  if TIME_to_STR(totest.qms) ne '14.10.1985 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, MONTH=1)
  if TIME_to_STR(totest.qms) ne '14.11.1984 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, MONTH=3)
  if TIME_to_STR(totest.qms) ne '14.01.1985 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, DAY=3)
  if TIME_to_STR(totest.qms) ne '17.10.1984 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, DAY=17)
  if TIME_to_STR(totest.qms) ne '31.10.1984 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, DAY=18)
  if TIME_to_STR(totest.qms) ne '01.11.1984 05:10:07' then error+=1
  
  
  totest = MAKE_REL_DATE(refDate, DAY=31)
  if TIME_to_STR(totest.qms) ne '14.11.1984 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, DAY=61)
  if TIME_to_STR(totest.qms) ne '14.12.1984 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, DAY=92)
  if TIME_to_STR(totest.qms) ne '14.01.1985 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, HOUR=1)
  if TIME_to_STR(totest.qms) ne '14.10.1984 06:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, HOUR=24)
  if TIME_to_STR(totest.qms) ne '15.10.1984 05:10:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, SECOND=2)
  if TIME_to_STR(totest.qms) ne '14.10.1984 05:10:09' then error+=1
  
  totest = MAKE_REL_DATE(refDate, SECOND=60)
  if TIME_to_STR(totest.qms) ne '14.10.1984 05:11:07' then error+=1
  
  totest = MAKE_REL_DATE(refDate, MILLISECOND=30)
  if TIME_to_STR(totest.qms) ne '14.10.1984 05:10:07' then error+=1
  if totest.millisecond ne 30 then error+=1
  
  totest = MAKE_REL_DATE(refDate, MILLISECOND=1001)
  if TIME_to_STR(totest.qms) ne '14.10.1984 05:10:08' then error+=1
  if totest.millisecond ne 1 then error+=1
  
  refDate = make_abs_date(YEAR=1984, MONTH=12, DAY=31, HOUR=23, MINUTE=59, SECOND=58)
  
  totest = MAKE_REL_DATE(refDate, SECOND=1)
  if TIME_to_STR(totest.qms) ne '31.12.1984 23:59:59' then error+=1
  
  totest = MAKE_REL_DATE(refDate, SECOND=2)
  if TIME_to_STR(totest.qms) ne '01.01.1985 00:00:00' then error+=1
  
  totest = MAKE_REL_DATE(refDate, MINUTE=1)
  if TIME_to_STR(totest.qms) ne '01.01.1985 00:00:58' then error+=1
  
  totest = MAKE_REL_DATE(refDate, HOUR=1)
  if TIME_to_STR(totest.qms) ne '01.01.1985 00:59:58' then error+=1
  
  totest = MAKE_REL_DATE(refDate, DAY=1)
  if TIME_to_STR(totest.qms) ne '01.01.1985 23:59:58' then error+=1
  if totest.second ne 58 then error+=1
  
  totest = MAKE_REL_DATE(refDate, MONTH=1)
  if TIME_to_STR(totest.qms) ne '31.01.1985 23:59:58' then error+=1
  if totest.second ne 58 then error+=1
  
  totest = MAKE_REL_DATE(refDate, YEAR=1)
  if TIME_to_STR(totest.qms) ne '31.12.1985 23:59:58' then error+=1
  if totest.second ne 58 then error+=1
  
  refDate = make_abs_date(YEAR=1984, MONTH=11, DAY=30, HOUR=00, MINUTE=00, SECOND=00)
  
  totest = MAKE_REL_DATE(refDate, YEAR=1, MONTH =1, DAY=1, HOUR=1, MINUTE=1, SECOND=1)
  if TIME_to_STR(totest.qms) ne '31.12.1985 01:01:01' then error+=1
  
  ; NEGATIVE DELTAS
  
  refDate = make_abs_date(YEAR=1984, MONTH=11, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  totest = MAKE_REL_DATE(refDate, MONTH =-1)
  if TIME_to_STR(totest.qms) ne '01.10.1984 00:00:00' then error+=1
  
  
  refDate = make_abs_date(YEAR=1984, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  totest = MAKE_REL_DATE(refDate, MONTH =-1)
  if TIME_to_STR(totest.qms) ne '01.12.1983 00:00:00' then error+=1
  
  refDate = make_abs_date(YEAR=1984, MONTH=01, DAY=02, HOUR=00, MINUTE=00, SECOND=00)
  totest = MAKE_REL_DATE(refDate, DAY =-1)
  if TIME_to_STR(totest.qms) ne '01.01.1984 00:00:00' then error+=1
  
  refDate = make_abs_date(YEAR=1984, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  totest = MAKE_REL_DATE(refDate, DAY =-1)
  if TIME_to_STR(totest.qms) ne '31.12.1983 00:00:00' then error+=1
  
  refDate = make_abs_date(YEAR=1984, MONTH=02, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  totest = MAKE_REL_DATE(refDate, DAY =-1)
  if TIME_to_STR(totest.qms) ne '31.01.1984 00:00:00' then error+=1
  
  refDate = make_abs_date(YEAR=1984, MONTH=02, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  totest = MAKE_REL_DATE(refDate, MILLISECOND = -1)
  if TIME_to_STR(totest.qms) ne '31.01.1984 23:59:59' then error+=1
  if totest.millisecond ne 999 then error+=1
  
  ;TODO
  hour = INDGEN(79) * 3 
  time0 = make_abs_date(YEAR=2008, MONTH=10, DAY=20, HOUR=03, MINUTE=00, SECOND=00)
  totest = MAKE_ABS_DATE(REFDATE=time0, HOUR = hour)
  if N_ELEMENTS(totest) ne 79 then error+=1
  if TIME_to_STR(totest[0]) ne '20.10.2008 03:00:00' then error+=1
  if TIME_to_STR(totest[1]) ne '20.10.2008 06:00:00' then error+=1
  if TIME_to_STR(totest[3]) ne '20.10.2008 12:00:00' then error+=1
  if TIME_to_STR(totest[78]) ne '29.10.2008 21:00:00' then error+=1
  
  min = INDGEN(61) 
  time0 = make_abs_date(YEAR=2008, MONTH=10, DAY=20, HOUR=03, MINUTE=00, SECOND=00)
  totest = MAKE_ABS_DATE(REFDATE=time0, MINUTE= min)
  if N_ELEMENTS(totest) ne 61 then error+=1
  if TIME_to_STR(totest[0]) ne '20.10.2008 03:00:00' then error+=1
  if TIME_to_STR(totest[1]) ne '20.10.2008 03:01:00' then error+=1
  if TIME_to_STR(totest[3]) ne '20.10.2008 03:03:00' then error+=1
  if TIME_to_STR(totest[60]) ne '20.10.2008 04:00:00' then error+=1
  
  sec = INDGEN(181) 
  time0 = make_abs_date(YEAR=2008, MONTH=2, DAY=28, HOUR=23, MINUTE=59, SECOND=00)
  totest = MAKE_ABS_DATE(REFDATE=time0, sec= sec)
  if N_ELEMENTS(totest) ne 181 then error+=1
  if TIME_to_STR(totest[0]) ne '28.02.2008 23:59:00' then error+=1
  if TIME_to_STR(totest[1]) ne '28.02.2008 23:59:01' then error+=1
  if TIME_to_STR(totest[60]) ne '29.02.2008 00:00:00' then error+=1
  if TIME_to_STR(totest[61]) ne '29.02.2008 00:00:01' then error+=1
  if TIME_to_STR(totest[180]) ne '29.02.2008 00:02:00' then error+=1
  
  if error ne 0 then message, '% TEST_MAKE_REL_DATE NOT passed', /CONTINUE else print, 'TEST_MAKE_REL_DATE passed'
  
end

pro TEST_MAKE_TIME_STEP
  ;MAKE_TIME_STEP, DAY=day, HOUR=hour, MINUTE=minute, SECOND=second, MILLISECOND = millisecond, DELTAQMS = deltaQms

  error = 0
  
  totest = MAKE_TIME_STEP(DAY=1, HOUR=3, MINUTE=45, SECOND=2, MILLISECOND = 1)
  if totest.dms ne 1000L*60l*60l*24l+1000L*60l*60l*3l+1000L*60l*45l+1000L*2l+1 then error+=1
  
  totest = MAKE_TIME_STEP(DAY=-1, HOUR=3, MINUTE=45, SECOND=2, MILLISECOND = 1)
  if totest.dms ne -1000L*60l*60l*24l+1000L*60l*60l*3l+1000L*60l*45l+1000L*2l+1 then error+=1
  
  totest = MAKE_TIME_STEP(DMS = 1000L*60l*60l*24l+1000L*60l*60l*3l+1000L*60l*45l+1000L*2l+1)
  if totest.day ne 1 then error+=1
  if totest.hour ne 3 then error+=1
  if totest.minute ne 45 then error+=1
  if totest.second ne 2 then error+=1
  if totest.millisecond ne 1 then error+=1
  
  totest = MAKE_TIME_STEP(DMS = -(1000L*60l*60l*24l+1000L*60l*60l*3l+1000L*60l*45l+1000L*2l+1))
  if totest.day ne -1 then error+=1
  if totest.hour ne -3 then error+=1
  if totest.minute ne -45 then error+=1
  if totest.second ne -2 then error+=1
  if totest.millisecond ne -1 then error+=1
  
  totest = MAKE_TIME_STEP(DMS = -1000L*60l*60l*24l+1000L*60l*60l*3l+1000L*60l*45l+1000L*2l+1)
  
  if totest.day ne 0 then error+=1
  if totest.hour ne -20 then error+=1
  if totest.minute ne -14 then error+=1
  if totest.second ne -57 then error+=1
  if totest.millisecond ne -999 then error+=1
  
  if error ne 0 then message, '% TEST_MAKE_TIME_STEP NOT passed', /CONTINUE else print, 'TEST_MAKE_TIME_STEP passed'
  
end

pro TEST_MAKE_TIME_SERIE

  error = 0
  
  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=02, HOUR=00, MINUTE=00, SECOND=00)
  
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 13, MONTH=1)
  if n_elements(totest) ne 13 then error+=1
  if TIME_to_STR(toTest[2]) ne '02.03.2005 00:00:00' then error+=1
  if TIME_to_STR(toTest[12]) ne '02.01.2006 00:00:00' then error+=1
  
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 7, MONTH=-2)
  if n_elements(totest) ne 7 then error+=1
  if TIME_to_STR(toTest[1]) ne '02.11.2004 00:00:00' then error+=1
  if TIME_to_STR(toTest[6]) ne '02.01.2004 00:00:00' then error+=1
  
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 6, YEAR=1)
  if n_elements(totest) ne 6 then error+=1
  if TIME_to_STR(toTest[2]) ne '02.01.2007 00:00:00' then error+=1
  if TIME_to_STR(toTest[5]) ne '02.01.2010 00:00:00' then error+=1
  
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 6, YEAR=-1)
  if n_elements(totest) ne 6 then error+=1
  if TIME_to_STR(toTest[1]) ne '02.01.2004 00:00:00' then error+=1
  if TIME_to_STR(toTest[5]) ne '02.01.2000 00:00:00' then error+=1
  
  
  tstep = MAKE_TIME_STEP(HOUR = 1)
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 25, TIMESTEP=tstep)
  if n_elements(totest) ne 25 then error+=1
  if TIME_to_STR(toTest[1]) ne '02.01.2005 01:00:00' then error+=1
  if TIME_to_STR(toTest[24]) ne '03.01.2005 00:00:00' then error+=1
  
  tstep = MAKE_TIME_STEP(HOUR = 1, MINUTE = 10)
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 7, TIMESTEP=tstep)
  if n_elements(totest) ne 7 then error+=1
  if TIME_to_STR(toTest[1]) ne '02.01.2005 01:10:00' then error+=1
  if TIME_to_STR(toTest[6]) ne '02.01.2005 07:00:00' then error+=1
  
  
  startTime = make_abs_date(YEAR=2004, MONTH=02, DAY=28, HOUR=00, MINUTE=00, SECOND=00)
  tstep = MAKE_TIME_STEP(HOUR = 1)
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 25+24+24, TIMESTEP=tstep)
  if n_elements(totest) ne 73 then error+=1
  if TIME_to_STR(toTest[24]) ne '29.02.2004 00:00:00' then error+=1
  
  startTime = make_abs_date(YEAR=2004, MONTH=03, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  tstep = MAKE_TIME_STEP(DAY =-1)
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 3, TIMESTEP=tstep)
  if n_elements(totest) ne 3 then error+=1
  if TIME_to_STR(toTest[1]) ne '29.02.2004 00:00:00' then error+=1
  
  startTime = make_abs_date(YEAR=2005, MONTH=02, DAY=28, HOUR=00, MINUTE=00, SECOND=00)
  tstep = MAKE_TIME_STEP(HOUR = 1)
  totest = MAKE_TIME_SERIE(startTime, NSTEPS = 25+24+24, TIMESTEP=tstep)
  if n_elements(totest) ne 73 then error+=1
  if TIME_to_STR(toTest[24]) ne '01.03.2005 00:00:00' then error+=1
  
  if error ne 0 then message, '% TEST_MAKE_TIME_SERIE NOT passed', /CONTINUE else print, 'TEST_MAKE_TIME_SERIE passed'
  
end

pro TEST_MAKE_ENDED_TIME_SERIE

  error = 0
  
  startTime = make_abs_date(YEAR=2004, MONTH=12, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  step = MAKE_TIME_STEP(day = 1)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  
  if nsteps ne 32 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[nsteps-1]) ne '01.01.2005 00:00:00' then error+=1
  if TIME_to_STR(toTest[1]) ne '02.12.2004 00:00:00' then error+=1
  
  endTime = make_abs_date(YEAR=2004, MONTH=12, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  step = MAKE_TIME_STEP(day = -1)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  
  if nsteps ne 32 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[nsteps-1]) ne '01.12.2004 00:00:00' then error+=1
  if TIME_to_STR(toTest[1]) ne '31.12.2004 00:00:00' then error+=1
  
  startTime = make_abs_date(YEAR=2004, MONTH=12, DAY=01, HOUR=08, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  step = MAKE_TIME_STEP(day = 1)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  
  if nsteps ne 31 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[nsteps-1]) ne '31.12.2004 08:00:00' then error+=1
  if TIME_to_STR(toTest[1]) ne '02.12.2004 08:00:00' then error+=1
  
  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=03, HOUR=00, MINUTE=01, SECOND=00)
  step = MAKE_TIME_STEP(hour = 1)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  
  if nsteps ne 49 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[nsteps-1]) ne '03.01.2005 00:00:00' then error+=1
  if TIME_to_STR(toTest[1]) ne '01.01.2005 01:00:00' then error+=1

  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=12, DAY=31, HOUR=00, MINUTE=01, SECOND=00)
  step = MAKE_TIME_STEP(DAY = 1)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  if nsteps ne 365 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[nsteps-2]) ne '30.12.2005 00:00:00' then error+=1
  if TIME_to_STR(toTest[1]) ne '02.01.2005 00:00:00' then error+=1
  
  startTime = make_abs_date(YEAR=2004, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2004, MONTH=12, DAY=31, HOUR=00, MINUTE=01, SECOND=00)
  step = MAKE_TIME_STEP(DAY = 1)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  if nsteps ne 366 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[nsteps-2]) ne '30.12.2004 00:00:00' then error+=1
  if TIME_to_STR(toTest[1]) ne '02.01.2004 00:00:00' then error+=1
  
  
  startTime = make_abs_date(YEAR=2004, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005,  MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, nsteps =nsteps, month = 3)
  if nsteps ne 5 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[1]) ne '01.04.2004 00:00:00' then error+=1
  if TIME_to_STR(toTest[2]) ne '01.07.2004 00:00:00' then error+=1
  if TIME_to_STR(toTest[4]) ne '01.01.2005 00:00:00' then error+=1
  
  startTime = make_abs_date(YEAR=2004, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2008,  MONTH=02, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, nsteps =nsteps, year = 1 )
  if nsteps ne 5 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[1]) ne '01.01.2005 00:00:00' then error+=1
  if TIME_to_STR(toTest[2]) ne '01.01.2006 00:00:00' then error+=1
  if TIME_to_STR(toTest[4]) ne '01.01.2008 00:00:00' then error+=1
    
  startTime = make_abs_date(YEAR=2008, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2004,  MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, nsteps =nsteps, year = -1)
  if nsteps ne 5 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[1]) ne '01.01.2007 00:00:00' then error+=1
  if TIME_to_STR(toTest[2]) ne '01.01.2006 00:00:00' then error+=1
  if TIME_to_STR(toTest[4]) ne '01.01.2004 00:00:00' then error+=1
  
  if error ne 0 then message, '% TEST_MAKE_ENDED_TIME_SERIE NOT passed', /CONTINUE else print, 'TEST_MAKE_ENDED_TIME_SERIE passed'
  
end

pro TEST_check_TimeSerie

  @WAVE.inc

  error = 0  
  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=03, HOUR=00, MINUTE=01, SECOND=00)
  step = MAKE_TIME_STEP(hour = 1)
  
  goodTS = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  
  ok = check_TimeSerie(goodTS, probableStep)
  
  if ok eq false then error+=1
  if probableStep.dms ne step.dms then error+=1
  
  badTS = [goodTS[0:4],goodTS[6:9],goodTS[11:*]]
  
  ok = check_TimeSerie(badTS, probableStep, FULL_TS=fullTS, IND_MISSING=mis)
  
  if ok eq TRUE then error+=1
  if step.dms ne probableStep.dms then error+=1
  if total(fullTS.qms-goodTS.qms) ne 0 then error+=1
  if N_ELEMENTS(mis) ne 2 then  error+=1
  if mis[0] ne 5 then error+=1
  if mis[1] ne 10 then error+=1  
  
  if error ne 0 then message, '% TEST_check_TimeSerie NOT passed', /CONTINUE else print, 'TEST_check_TimeSerie passed'
  
end

pro TEST_TS_FILL_MISSING
  
   ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc   

  error = 0  
  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=03, HOUR=00, MINUTE=01, SECOND=00)
  step = MAKE_TIME_STEP(hour = 1)
  
  goodTS = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  goodData = (cgDemoData(17))[0:nsteps-1]

  ok = check_TimeSerie(goodTS, probableStep)  
  if ok eq false then error+=1
  if probableStep.dms ne step.dms then error+=1

  badTS = [goodTS[1:4],goodTS[6:9],goodTS[11:40]]
  badData = [goodData[1:4],goodData[6:9],goodData[11:40]]
  missT = [0,5,10,41,42,43,44,45,46,47,48]  
  ok = check_TimeSerie(badTS, probableStep, FULL_TS=fullTS, IND_MISSING=mis)
  if ok eq TRUE then error+=1  
  filled_Data = TS_FILL_MISSING(badData, badTS, goodTS, INDEXES=inds)
  if N_ELEMENTS(inds) ne N_ELEMENTS(missT) then error +=1
  if N_ELEMENTS(filled_Data) ne N_ELEMENTS(goodData) then error +=1
  if total(inds - missT) ne 0 then error +=1
  if GOODDATA[3] ne filled_Data[3] then error +=1
  if GOODDATA[40] ne filled_Data[40] then error +=1
  if total(filled_data[where(FINITE(filled_Data) eq 1)] - badData) ne 0 then error +=1
  
  badTS = [goodTS[0:4],goodTS[6:9],goodTS[11:40],goodTS[48]]
  badData = [goodData[0:4],goodData[6:9],goodData[11:40],goodData[48]]
  missT = [5,10,41,42,43,44,45,46,47]  
  ok = check_TimeSerie(badTS, probableStep, FULL_TS=fullTS, IND_MISSING=mis)
  if ok eq TRUE then error+=1  
  filled_Data = TS_FILL_MISSING(badData, badTS, goodTS, INDEXES=inds)
  if N_ELEMENTS(inds) ne N_ELEMENTS(missT) then error +=1
  if N_ELEMENTS(filled_Data) ne N_ELEMENTS(goodData) then error +=1
  if total(inds - missT) ne 0 then error +=1
  if GOODDATA[3] ne filled_Data[3] then error +=1
  if GOODDATA[40] ne filled_Data[40] then error +=1
  if total(filled_data[where(FINITE(filled_Data) eq 1)] - badData) ne 0 then error +=1  
  
  filled_Data = TS_FILL_MISSING(GOODDATA, goodTS, goodTS, INDEXES=inds)
  if total(GOODDATA - filled_Data) ne 0 then error +=1  
  if N_ELEMENTS(inds) ne 1 then error +=1 
  if inds[0] ne -1 then error +=1 
  
  filled_Data = TS_FILL_MISSING(GOODDATA, goodTS, badTS, INDEXES=inds)
  if total(badData - filled_Data) ne 0 then error +=1  
  if N_ELEMENTS(inds) ne 1 then error +=1 
  if inds[0] ne -1 then error +=1   
  
  
  if error ne 0 then message, '% TEST_TS_FILL_MISSING NOT passed', /CONTINUE else print, 'TEST_TS_FILL_MISSING passed'
  
end

pro TEST_TS_MEAN
  
   ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc   

  error = 0  
  
  ; Very simple tests
  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 6, minute = 33)]   
  st = TS_MEAN_STATISTICS(data, time, /HOUR)
  if st.nt ne 1 then error += 1 
  if st.mean[0] ne 3.5 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 1, hour = 7) then error += 1 
  
  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 7, minute = 0)]   
  st = TS_MEAN_STATISTICS(data, time, /HOUR)
  if st.nt ne 1 then error += 1 
  if st.mean[0] ne 3.5 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 1, hour = 7) then error += 1 
  
  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6), QMS_TIME(year = 2009, day = 1, hour = 6, minute = 33)]   
  st = TS_MEAN_STATISTICS(data, time, /HOUR)
  if st.nt ne 2 then error += 1 
  if st.mean[0] ne 3 then error += 1 
  if st.mean[1] ne 4 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 1, hour = 6) then error += 1 
  if st.time[1] ne QMS_TIME(year = 2009, day = 1, hour = 7) then error += 1 
 
  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 7, minute = 33)]   
  st = TS_MEAN_STATISTICS(data, time, /HOUR)
  if st.nt ne 2 then error += 1 
  if st.mean[0] ne 3 then error += 1 
  if st.mean[1] ne 4 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 1, hour = 7) then error += 1 
  if st.time[1] ne QMS_TIME(year = 2009, day = 1, hour = 8) then error += 1 
  
  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 7, minute = 33)]   
  st = TS_MEAN_STATISTICS(data, time, NEW_TIME=[QMS_TIME(year = 2009, day = 1), QMS_TIME(year = 2009, day = 2)])
  if st.nt ne 1 then error += 1 
  if st.mean[0] ne 3.5 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 2) then error += 1 

  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 7, minute = 33)]   
  st = TS_MEAN_STATISTICS(data, time, /DAY)
  if st.nt ne 1 then error += 1 
  if st.mean[0] ne 3.5 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 2) then error += 1 

  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 7, minute = 33)]   
  st = TS_MEAN_STATISTICS(data, time, NEW_TIME=[QMS_TIME(year = 2009, day = 2), QMS_TIME(year = 2009, day = 3)])
  if st.nt ne 1 then error += 1 
  if FINITE(st.mean[0]) ne 0 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 3) then error += 1 

  data = [3.,4.,5.]
  time = [QMS_TIME(year = 2009, day = 1, hour = 6, minute = 32), QMS_TIME(year = 2009, day = 1, hour = 8, minute = 33), QMS_TIME(year = 2009, day = 1, hour = 8, minute = 59)]   
  st = TS_MEAN_STATISTICS(data, time, /HOUR)
  if st.nt ne 3 then error += 1 
  if TOTAL(FINITE(st.mean) - [1,0,1]) ne 0 then error += 1 
  if TOTAL(ABS(st.mean - [3.,0,4.5]), /NAN) ne 0 then error += 1 
  
  data = [1.,1.,1.,1.,1.,1.,2.,3.,3.,1.,1.,2.]
  time =  MAKE_TIME_SERIE(QMS_TIME(year = 2009, day = 1, hour = 6, minute = 10), NSTEPS=12, TIMESTEP=MAKE_TIME_STEP(MINUTE=10))
  st = TS_MEAN_STATISTICS(data, time, /HOUR)
  if st.nt ne 2 then error += 1 
  if st.mean[0] ne 1. then error += 1 
  if st.mean[1] ne 2. then error += 1 
  if st.stddev[1] ne STDDEV([2.,3.,3.,1.,1.,2.]) then error += 1 
  if st.max[1] ne 3. then error += 1 
  if st.min[1] ne 1. then error += 1 
  if st.nel[1] ne 6. then error += 1 
  if st.tot[1]/st.nel[1]  ne st.mean[1] then error += 1 
  
  if error ne 0 then message, '% TEST_TS_MEAN NOT passed', /CONTINUE else print, 'TEST_TS_MEAN passed'
  
end

pro TEST_TS_RESAMPLE
  
   ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc   

  error = 0  
  
  ; Very simple tests
  data = [3.,4.]
  time = [QMS_TIME(year = 2009, day = 2, hour = 0, minute = 00), QMS_TIME(year = 2009, day = 3, hour = 0, minute = 00)]   
  st = TS_resample(data, time, /HOUR)
  if st.nt ne 48 then error += 1 
  if st.data[0] ne 3 then error += 1 
  if st.data[1] ne 3 then error += 1 
  if st.data[23] ne 3 then error += 1 
  if st.data[24] ne 4 then error += 1 
  if st.data[47] ne 4 then error += 1 
  if st.time[0] ne QMS_TIME(year = 2009, day = 1, hour = 01) then error += 1 
  if st.time[23] ne QMS_TIME(year = 2009, day = 2, hour = 00) then error += 1 
  if st.time[47] ne QMS_TIME(year = 2009, day = 3, hour = 00) then error += 1 
  data = [3.,4.]
 
  time = [QMS_TIME(year = 2009, day = 2, hour = 0, minute = 00), QMS_TIME(year = 2009, day = 3, hour = 0, minute = 00)]   
  st = TS_resample(data, time, /M10)
  if st.nt ne 48*6 then error += 1 
  if st.data[0] ne 3 then error += 1 
  if st.data[1] ne 3 then error += 1 
  if st.data[23] ne 3 then error += 1 
  if st.data[24] ne 3 then error += 1 
  
  if st.data[143] ne 3 then error += 1   
  if st.time[0] ne QMS_TIME(year = 2009, day = 1, hour = 00, minute = 10) then error += 1 
  if st.time[143] ne QMS_TIME(year = 2009, day = 2, hour = 00, minute = 00) then error += 1 
  if st.data[144] ne 4 then error += 1   
  if st.time[144] ne QMS_TIME(year = 2009, day = 2, hour = 00, minute = 10) then error += 1 
  if st.time[48*6-1] ne QMS_TIME(year = 2009, day = 3, hour = 00, minute = 00) then error += 1 
 
  t = TS_MEAN_STATISTICS(st.data, st.time, /DAY)
  if t.time[0] ne  QMS_TIME(year = 2009, day = 2, hour = 0, minute = 00) then error +=1
  if t.time[1] ne  QMS_TIME(year = 2009, day = 3, hour = 0, minute = 00) then error +=1
  if t.mean[0] ne  3 then error +=1
  if t.mean[1] ne  4 then error +=1
  if t.nt ne 2 then error +=1
  
  st_d = TS_resample(data, time, NEW_TIME=st.time)  
  if st_d.nt ne st.nt then error +=1
  if total(ABS(st_d.time - st.time)) ne 0 then error +=1
  if total(ABS(st_d.data - st.data)) ne 0 then error +=1

  st_d = TS_resample(data, time, NEW_TIME=st.time[3:149])  
  if st_d.nt ne N_ELEMENTS(st.time[3:149]) then error +=1
  if st_d.time[0] ne st.time[3] then error +=1
  if st_d.time[N_ELEMENTS(st.time[3:149])-1] ne st.time[149] then error +=1
  
  if st_d.data[0] ne 3 then error +=1
  if st_d.data[1] ne 3 then error +=1
  if st_d.data[56] ne 3 then error +=1
  p = where(st_d.time eq QMS_TIME(year = 2009, day = 2, hour = 0, minute = 00))
  
  if st_d.data[p] ne 3 then error +=1
  if st_d.data[p+1] ne 4 then error +=1
  if st_d.data[N_ELEMENTS(st.time[3:149])-1] ne 4 then error +=1

  if error ne 0 then message, '% TEST_TS_RESAMPLE NOT passed', /CONTINUE else print, 'TEST_TS_RESAMPLE passed'
  
end

pro TEST_TS_DIURNAL_MEANS
  
   ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc   

  error = 0  
  
  ;TODO: TEST: TS_DIURNAL_MEANS
  
  if error ne 0 then message, '% TEST_TS_DIURNAL_MEANS NOT passed', /CONTINUE else print, 'TEST_TS_DIURNAL_MEANS passed'
  
end

pro time_bug


  print, 'refDate: 14.10.1984 05:10:08'   
  
  refDate = GEN_MAKE_TIME(YEAR=1984, MONTH=10, DAY=14, HOUR=5, MINUTE=10, SECOND=8)
  print, 'STIME(refDate.qt): ' + STIME(refDate.qt)
  
  myrefDate = make_abs_date(YEAR=1984, MONTH=10, DAY=14, HOUR=5, MINUTE=10, SECOND=8)
  print, 'TIME_to_STR(myrefDate.qms): ' + TIME_to_STR(myrefDate.qms)
  
  
end


pro TEST_TRMM_3B42
    
    fdir = TEST_file_directory() + 'TRMM/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    trmm_3B42 = OBJ_NEW('w_TRMM', FILE=fdir+'3B42.081001.3.6A.nc')
    
    trmm_3B42->get_time, time, nt, t0, t1    
    if nt ne 1 then error += 1    
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 01, hour = 3) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 01, hour = 3) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time ne QMS_TIME(year = 2008, month = 10, day = 01, hour = 3) then error += 1
    
    trmm_3B42->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne -179.875 then error += 1
    if lon[0,399] ne -179.875 then error += 1
    if lon[1439,0] ne 179.875 then error += 1
    if lon[1439,399] ne 179.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1
    
    ;TEST ts
    if trmm_3B42->get_TimeSerie('lon', 0, 0)  ne -179.875 then error += 1
    if trmm_3B42->get_TimeSerie('lon', 0, 399) ne -179.875 then error += 1
    if trmm_3B42->get_TimeSerie('lon', 1439,0) ne 179.875 then error += 1
    if trmm_3B42->get_TimeSerie('lon', 1439,399) ne 179.875 then error += 1    
    if trmm_3B42->get_TimeSerie('lat', 0, 0)  ne -49.875 then error += 1
    if trmm_3B42->get_TimeSerie('lat', 1439,0) ne -49.875 then error += 1
    if trmm_3B42->get_TimeSerie('lat', 0,399) ne 49.875 then error += 1
    if trmm_3B42->get_TimeSerie('lat', 1439,399) ne 49.875 then error += 1
    
    trmm_3B42->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    ; Test transforms
    GIS_make_datum, ret, datum, NAME='WGS-84'
    trmm_3B42->transform_LonLat, -179.870, -49.870, datum, i, j, /NEAREST
    if i ne 0 then error += 1
    if j ne 0 then error += 1
    trmm_3B42->transform_LonLat, -179.600, -49.600, datum, i, j, /NEAREST
    if i ne 1 then error += 1
    if j ne 1 then error += 1
    
    varPcp = trmm_3B42->get_Var('precipitation', vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    
    trmm_3B42->GetProperty, cdfid = trmm_3B42_ID, TYPE=type
    NCDF_VARGET, trmm_3B42_ID,  'precipitation', varOrig        
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1
    if TYPE ne '3B42_h' then  error += 1
    
    undefine, vtime, vn
    varPcp = trmm_3B42->get_prcp(vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1
    
    ts = trmm_3B42->get_Prcp_TimeSerie(1,1, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
    if ts ne varPcp[1,1] then error += 1
    if pi ne 1 then error += 1
    if pj ne 1 then error += 1    
    if plon ne -179.875+0.250 then error += 1
    if plat ne -49.875+0.250  then error += 1
    
    GIS_make_datum, ret, datum, NAME='WGS-84'
    ts = trmm_3B42->get_Prcp_TimeSerie(-179.870,-49.870, SRC=datum, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
    if ts ne varPcp[0,0] then error += 1
    if pi ne 0 then error += 1
    if pj ne 0 then error += 1    
    if plon ne -179.875 then error += 1
    if plat ne -49.875 then error += 1
    
    GIS_make_datum, ret, datum, NAME='WGS-84'
    ts = trmm_3B42->get_Prcp_TimeSerie(94.875,19.875, SRC=datum, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
    if ts ne varPcp[pi,pj] then error += 1
    if abs(plon - lon[pi,pj]) gt 1e-10 then error += 1
    if abs(plat - lat[pi,pj]) gt 1e-10 then error += 1
    if abs(plon - 94.875) gt 1e-10 then error += 1
    if abs(plat - 19.875) gt 1e-10 then error += 1
   
    ;-------------
    ; Test Subset
    ;-------------    
    
    ; Simple subset
    ok = trmm_3B42->define_subset(SUBSET_IJ=[4,400,4,100])
    if ok ne 1 THEN error +=1    
    varPcp = trmm_3B42->get_prcp(vtime, vnt)
    varOrigCrop = varOrig[4:4+399,4:4+99]
    if total(abs(varOrigCrop-varPcp)) ne 0 then  error += 1
    
    trmm_3B42->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 400 then  error += 1
    if ny ne 100 then  error += 1
    if lon[0,0] ne -179.875 + 1. then  error += 1
    if lat[0,0] ne -49.875 + 1. then  error += 1
    
    if lon[399,99] ne -179.875 + (399. + 4.) * 0.25 then  error += 1
    if lat[399,99] ne -49.875  + (99. + 4.) * 0.25 then  error += 1
        
    trmm_3B42->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    ; reset subset
    ok = trmm_3B42->define_subset()
    if ok ne 1 THEN error +=1  
    varPcp = trmm_3B42->get_Var('precipitation', vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1    
    trmm_3B42->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne -179.875 then error += 1
    if lon[0,399] ne -179.875 then error += 1
    if lon[1439,0] ne 179.875 then error += 1
    if lon[1439,399] ne 179.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1    
    trmm_3B42->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    ; Lat-Lon subset
    ok = trmm_3B42->define_subset(SUBSET_LL=[70.01,10.02,120.01,45.0001], SUBSET_IJ=mysubs)
    if ok ne 1 THEN error +=1  
            
    trmm_3B42->QuickPlotPrcp, wid = wid
    ok = DIALOG_MESSAGE('Do you see a plot?', /QUESTION)
    if ok eq 'No' then error += 1
    WIDGET_CONTROL, wid, /DESTROY
    trmm_3B42->get_ncdf_coordinates, lon, lat, nx, ny  
    if lon[0,0] ne 70.125 then error += 1
    if lon[0,ny-1] ne 70.125 then error += 1
    if lon[nx-1,0] ne 120.125 then error += 1
    if lon[nx-1,ny-1] ne 120.125 then error += 1    
    if lat[0,0] ne 10.125 then error += 1
    if lat[nx-1,0] ne 10.125 then error += 1
    if lat[0,ny-1] ne 45.125 then error += 1
    if lat[nx-1,ny-1] ne 45.125 then error += 1   
    
        ;TEST ts
    if trmm_3B42->get_TimeSerie('lon', 0, 0)  ne 70.125 then error += 1
    if trmm_3B42->get_TimeSerie('lon', 0, ny-1) ne 70.125 then error += 1
    if trmm_3B42->get_TimeSerie('lon', nx-1,0) ne 120.125 then error += 1
    if trmm_3B42->get_TimeSerie('lon', nx-1,ny-1) ne 120.125 then error += 1    
    if trmm_3B42->get_TimeSerie('lat', 0, 0)  ne 10.125 then error += 1
    if trmm_3B42->get_TimeSerie('lat', nx-1,0) ne 10.125 then error += 1
    if trmm_3B42->get_TimeSerie('lat', 0,ny-1) ne 45.125 then error += 1
    if trmm_3B42->get_TimeSerie('lat', nx-1,ny-1) ne 45.125 then error += 1
    
    trmm_3B42->Get_LonLat, gislon, gislat, gisnx, gisny
    if gisnx ne nx then  error += 1
    if gisny ne ny then  error += 1    
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    trmm_3B42->transform_LonLat, 120.01, 45.01, datum, i, j, /NEAREST
    if i ne nx-1 then error += 1
    if j ne ny-1 then error += 1
    
    trmm_3B42->transform, 120.01, 45.01, i, j, SRC = datum, LAT_DST=la, LON_DST=lo, /NEAREST
    if i ne nx-1 then error += 1
    if j ne ny-1 then error += 1
    if lo ne 120.01 then error += 1
    if la ne 45.01 then error += 1
     
    
    varPcp = trmm_3B42->get_prcp(vtime, vnt)
    varOrigCrop = varOrig[mysubs[0]:mysubs[0]+mysubs[1]-1,mysubs[2]:mysubs[2]+mysubs[3]-1]
    if total(abs(varOrigCrop-varPcp)) ne 0 then  error += 1
    
    GIS_make_datum, ret, datum, NAME='WGS-84'
    ts = trmm_3B42->get_Prcp_TimeSerie(94.875,19.875, SRC=datum, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
    if ts ne varPcp[pi,pj] then error += 1
    if abs(plon - lon[pi,pj]) gt 1e-10 then error += 1
    if abs(plat - lat[pi,pj]) gt 1e-10 then error += 1
    if abs(plon - 94.875) gt 1e-10 then error += 1
    if abs(plat - 19.875) gt 1e-10 then error += 1
    
    ; reset subset
    ok = trmm_3B42->define_subset()
    if ok ne 1 THEN error +=1  
    varPcp = trmm_3B42->get_Var('precipitation', vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1    
    trmm_3B42->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne -179.875 then error += 1
    if lon[0,399] ne -179.875 then error += 1
    if lon[1439,0] ne 179.875 then error += 1
    if lon[1439,399] ne 179.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1    
    trmm_3B42->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    OBJ_DESTROY, trmm_3B42     
    if error ne 0 then message, '% TEST_TRMM_3B42 NOT passed', /CONTINUE else print, 'TEST_TRMM_3B42 passed'
  
end

pro TEST_TRMM_3B42_daily
    
    fdir = TEST_file_directory() + 'TRMM/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    TRMM_3B42_daily = OBJ_NEW('w_TRMM', FILE=fdir+'3B42_daily.2008.10.01.6.nc')
    
    TRMM_3B42_daily->get_time, time, nt, t0, t1    
    if nt ne 1 then error += 1    
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 01) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 01) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time ne QMS_TIME(year = 2008, month = 10, day = 01) then error += 1
    
    TRMM_3B42_daily->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne 0.125 then error += 1
    if lon[0,399] ne 0.125 then error += 1
    if lon[1439,0] ne 359.875 then error += 1
    if lon[1439,399] ne 359.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1
    
    TRMM_3B42_daily->Get_LonLat, gislon, gislat, nx, ny
    if gislon[0,0] ne -179.875 then error += 1
    if gislon[0,399] ne -179.875 then error += 1
    if gislon[1439,0] ne 179.875 then error += 1
    if gislon[1439,399] ne 179.875 then error += 1    
    if gislat[0,0] ne -49.875 then error += 1
    if gislat[1439,0] ne -49.875 then error += 1
    if gislat[0,399] ne 49.875 then error += 1
    if gislat[1439,399] ne 49.875 then error += 1        
    
    if total(abs(gislon[0:719,*]-(lon[720:*,*]-360.))) gt 1e-6 then  error += 1
    if total(abs(gislat[0:719,*]-lat[720:*,*])) gt 1e-6 then  error += 1
    
    !QUIET = 1
    varOrig = TRMM_3B42_daily->get_Var('hrf', vtime, vnt)  
    !QUIET = 0    
    undefine, vtime, vn      
    varPcp = TRMM_3B42_daily->get_prcp(vtime, vnt) 
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig[0:719,*]-varPcp[720:*,*])) ne 0 then  error += 1
    
    OBJ_DESTROY, TRMM_3B42_daily     
    if error ne 0 then message, '% TEST_TRMM_3B42_daily NOT passed', /CONTINUE else print, 'TEST_TRMM_3B42_daily passed'
  
end

pro TEST_TRMM_3B43
    
    fdir = TEST_file_directory() + 'TRMM/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    trmm_3B43 = OBJ_NEW('w_TRMM', FILE=fdir+'3B43.000801.6.nc')
    
    trmm_3B43->get_time, time, nt, t0, t1    
    if nt ne 1 then error += 1    
    if t0 ne QMS_TIME(year = 2000, month = 08, day = 01, hour = 0) then error += 1
    if t1 ne QMS_TIME(year = 2000, month = 08, day = 01, hour = 0) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time ne QMS_TIME(year = 2000, month = 08, day = 01, hour = 0) then error += 1
    
    trmm_3B43->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne -179.875 then error += 1
    if lon[0,399] ne -179.875 then error += 1
    if lon[1439,0] ne 179.875 then error += 1
    if lon[1439,399] ne 179.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1
    
    trmm_3B43->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    varPcp = trmm_3B43->get_Var('pcp', vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    
    trmm_3B43->GetProperty, cdfid = trmm_3B43_ID, TYPE=type
    NCDF_VARGET, trmm_3B43_ID,  'pcp', varOrig        
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1
    if TYPE ne '3B43' then  error += 1
    
    undefine, vtime, vn
    varPcp = trmm_3B43->get_prcp(vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1
    
    ;-------------
    ; Test Subset
    ;-------------    
    
    ; Simple subset
    ok = trmm_3B43->define_subset(SUBSET_IJ=[4,400,4,100])
    if ok ne 1 THEN error +=1    
    varPcp = trmm_3B43->get_prcp(vtime, vnt)
    varOrigCrop = varOrig[4:4+399,4:4+99]
    if total(abs(varOrigCrop-varPcp)) ne 0 then  error += 1
    
    trmm_3B43->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 400 then  error += 1
    if ny ne 100 then  error += 1
    if lon[0,0] ne -179.875 + 1. then  error += 1
    if lat[0,0] ne -49.875 + 1. then  error += 1
    
    if lon[399,99] ne -179.875 + (399. + 4.) * 0.25 then  error += 1
    if lat[399,99] ne -49.875  + (99. + 4.) * 0.25 then  error += 1
    
    trmm_3B43->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    ; reset subset
    ok = trmm_3B43->define_subset()
    if ok ne 1 THEN error +=1  
    varPcp = trmm_3B43->get_Var('pcp', vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1    
    trmm_3B43->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne -179.875 then error += 1
    if lon[0,399] ne -179.875 then error += 1
    if lon[1439,0] ne 179.875 then error += 1
    if lon[1439,399] ne 179.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1    
    trmm_3B43->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    ; Lat-Lon subset
    ok = trmm_3B43->define_subset(SUBSET_LL=[-70.01,-10.02,-29.76,25.0001], SUBSET_IJ=mysubs)
    if ok ne 1 THEN error +=1  
            
    trmm_3B43->QuickPlotPrcp, wid = wid
    ok = DIALOG_MESSAGE('Do you see a plot?', /QUESTION)
    if ok eq 'No' then error += 1
    WIDGET_CONTROL, wid, /DESTROY
    trmm_3B43->get_ncdf_coordinates, lon, lat, nx, ny    
    if lon[0,0] ne -70.125 then error += 1
    if lon[0,ny-1] ne -70.125 then error += 1
    if lon[nx-1,0] ne -29.875 then error += 1
    if lon[nx-1,ny-1] ne -29.875 then error += 1    
    if lat[0,0] ne -10.125 then error += 1
    if lat[nx-1,0] ne -10.125 then error += 1
    if lat[0,ny-1] ne 25.125 then error += 1
    if lat[nx-1,ny-1] ne 25.125 then error += 1   
    
    trmm_3B43->Get_LonLat, gislon, gislat, gisnx, gisny
    if gisnx ne nx then  error += 1
    if gisny ne ny then  error += 1    
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    varPcp = trmm_3B43->get_prcp(vtime, vnt)
    varOrigCrop = varOrig[mysubs[0]:mysubs[0]+mysubs[1]-1,mysubs[2]:mysubs[2]+mysubs[3]-1]
    if total(abs(varOrigCrop-varPcp)) ne 0 then  error += 1
    
    ; reset subset
    ok = trmm_3B43->define_subset()
    if ok ne 1 THEN error +=1  
    varPcp = trmm_3B43->get_Var('pcp', vtime, vnt)  
    if vnt ne nt then error += 1
    if vtime ne time then error += 1
    if total(abs(varOrig-varPcp)) ne 0 then  error += 1    
    trmm_3B43->get_ncdf_coordinates, lon, lat, nx, ny
    if nx ne 1440 then error += 1
    if ny ne 400 then error += 1        
    if lon[0,0] ne -179.875 then error += 1
    if lon[0,399] ne -179.875 then error += 1
    if lon[1439,0] ne 179.875 then error += 1
    if lon[1439,399] ne 179.875 then error += 1    
    if lat[0,0] ne -49.875 then error += 1
    if lat[1439,0] ne -49.875 then error += 1
    if lat[0,399] ne 49.875 then error += 1
    if lat[1439,399] ne 49.875 then error += 1    
    trmm_3B43->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    OBJ_DESTROY, trmm_3B43     
    if error ne 0 then message, '% TEST_TRMM_3B43 NOT passed', /CONTINUE else print, 'TEST_TRMM_3B43 passed'
  
end

pro TEST_TRMM_AGG
    
    fdir = TEST_file_directory() + 'TRMM/'
    error = 0 
    
    ;-------------------------
    ; TEST without Subset
    ;-------------------------        
    agg_file = fdir + '3B42_agg.2008_10_01.nc'
    if FILE_TEST(agg_file) then FILE_DELETE, agg_file
    log_file = fdir + 'trmm_agg_2008_10_01.log'
    if FILE_TEST(log_file) then FILE_DELETE, log_file
    
    utils_TRMM_aggregate_3B42, fdir, /NOSHIFT
    if ~FILE_TEST(agg_file) then error += 1
    if ~FILE_TEST(log_file) then error += 1
    
    dailyF = fdir + '3B42_daily.2008.10.01.6.nc'
    daily_trmm = OBJ_NEW('w_TRMM', FILE=dailyF)
    agg_trmm = OBJ_NEW('w_TRMM', FILE=agg_file)    
    
    dpcp = daily_trmm->get_prcp()
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 8 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 21) then error += 1 
    
    testF = fdir + '3B42.081001.12.6A.nc'
    test_o =  OBJ_NEW('w_TRMM', FILE=testF)
    t = QMS_TIME(year = 2008, month = 10, day = 1, hour = 12)
    if TOTAL(ABS(agg_trmm->get_prcp(t0 = t, t1 = t) - test_o->get_prcp()*3)) ne 0 then error += 1
    OBJ_DESTROY, test_o    
    if TOTAL(ABS(TOTAL(aggpcp,3) - dpcp)) ne 0 then error += 1    
    OBJ_DESTROY, daily_trmm
    OBJ_DESTROY, agg_trmm
    
    ;-----------------
    ; TEST with Subset
    ;-----------------        
    agg_file = fdir + '3B42_agg.2008_10_01.nc'
    if FILE_TEST(agg_file) then FILE_DELETE, agg_file
    log_file = fdir + 'trmm_agg_2008_10_01.log'
    if FILE_TEST(log_file) then FILE_DELETE, log_file
    
    utils_TRMM_aggregate_3B42, fdir, /NOSHIFT, SUBSET_LL=[120.,10.,150.,45.]
    if ~FILE_TEST(agg_file) then error += 1
    if ~FILE_TEST(log_file) then error += 1
    
    dailyF = fdir + '3B42_daily.2008.10.01.6.nc'
    daily_trmm = OBJ_NEW('w_TRMM', FILE=dailyF, SUBSET_LL=[120.,10.,150.,45.])
    agg_trmm = OBJ_NEW('w_TRMM', FILE=agg_file)    
    
    dpcp = daily_trmm->get_prcp()
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 8 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 21) then error += 1 
    
    testF = fdir + '3B42.081001.12.6A.nc'
    test_o =  OBJ_NEW('w_TRMM', FILE=testF, SUBSET_LL=[120.,10.,150.,45.])
    t = QMS_TIME(year = 2008, month = 10, day = 1, hour = 12)
    if TOTAL(ABS(agg_trmm->get_prcp(t0 = t, t1 = t) - test_o->get_prcp()*3)) ne 0 then error += 1
    OBJ_DESTROY, test_o    
    if TOTAL(ABS(TOTAL(aggpcp,3) - dpcp)) ne 0 then error += 1    
    OBJ_DESTROY, daily_trmm
    OBJ_DESTROY, agg_trmm
    
    ;-------------------------
    ; TEST with start and end time
    ;-------------------------        
    agg_file = fdir + '3B42_agg.2008_10_01.nc'
    if FILE_TEST(agg_file) then FILE_DELETE, agg_file
    log_file = fdir + 'trmm_agg_2008_10_01.log'
    if FILE_TEST(log_file) then FILE_DELETE, log_file
      
    utils_TRMM_aggregate_3B42, fdir, /NOSHIFT, START_TIME= QMS_TIME(year = 2008, month = 10, day = 01, hour = 03), $
                                               END_TIME= QMS_TIME(year = 2008, month = 10, day = 01, hour = 18)
    if ~FILE_TEST(agg_file) then error += 1
    if ~FILE_TEST(log_file) then error += 1
    
    agg_trmm = OBJ_NEW('w_TRMM', FILE=agg_file)    
    
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 6 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 03) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 18) then error += 1 
    
    testF = fdir + '3B42.081001.3.6A.nc'
    test_o =  OBJ_NEW('w_TRMM', FILE=testF)
    if TOTAL(ABS(aggpcp[*,*,0] - test_o->get_prcp()*3)) ne 0 then error += 1
    OBJ_DESTROY, test_o    
    
    testF = fdir + '3B42.081001.18.6A.nc'
    test_o =  OBJ_NEW('w_TRMM', FILE=testF)
    if TOTAL(ABS(aggpcp[*,*,5] - test_o->get_prcp()*3)) ne 0 then error += 1
    OBJ_DESTROY, test_o        
    OBJ_DESTROY, agg_trmm
    
    ;-------------------------
    ; TEST with SHIFT
    ;-------------------------        
    agg_file = fdir + '3B42_agg.2008_10_01.nc'
    if FILE_TEST(agg_file) then FILE_DELETE, agg_file
    log_file = fdir + 'trmm_agg_2008_10_01.log'
    if FILE_TEST(log_file) then FILE_DELETE, log_file
    
    utils_TRMM_aggregate_3B42, fdir
    if ~FILE_TEST(agg_file) then error += 1
    if ~FILE_TEST(log_file) then error += 1

    agg_trmm = OBJ_NEW('w_TRMM', FILE=agg_file)        
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 7 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1, hour =3) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 21) then error += 1 
    
    testF = fdir + '3B42.081001.12.6A.nc'
    test_o1 =  OBJ_NEW('w_TRMM', FILE=testF)
    testF = fdir + '3B42.081001.15.6A.nc'
    test_o2 =  OBJ_NEW('w_TRMM', FILE=testF)
    test_pcp = test_o1->get_prcp()*1.5 + test_o2->get_prcp()*1.5    
    t = QMS_TIME(year = 2008, month = 10, day = 1, hour = 15)
    if TOTAL(ABS(agg_trmm->get_prcp(t0 = t, t1 = t) - test_pcp)) ne 0 then error += 1
    OBJ_DESTROY, test_o1
    OBJ_DESTROY, test_o2
    
    testF = fdir + '3B42.081001.0.6A.nc'
    test_o1 =  OBJ_NEW('w_TRMM', FILE=testF)
    testF = fdir + '3B42.081001.3.6A.nc'
    test_o2 =  OBJ_NEW('w_TRMM', FILE=testF)
    test_pcp = test_o1->get_prcp()*1.5 + test_o2->get_prcp()*1.5
    if TOTAL(ABS(aggpcp[*,*,0] - test_pcp)) ne 0 then error += 1
    OBJ_DESTROY, test_o1
    OBJ_DESTROY, test_o2
    
    testF = fdir + '3B42.081001.18.6A.nc'
    test_o1 =  OBJ_NEW('w_TRMM', FILE=testF)
    testF = fdir + '3B42.081001.21.6A.nc'
    test_o2 =  OBJ_NEW('w_TRMM', FILE=testF)
    test_pcp = (test_o1->get_prcp() > 0) * 1.5 + (test_o2->get_prcp() > 0) * 1.5
    if TOTAL(ABS(aggpcp[*,*,6] - test_pcp)) ne 0 then error += 1
    t = QMS_TIME(year = 2008, month = 10, day = 1, hour = 21)
    if TOTAL(ABS(agg_trmm->get_prcp(t0 = t, t1 = t) - test_pcp)) ne 0 then error += 1
    
    OBJ_DESTROY, test_o1
    OBJ_DESTROY, test_o2
    
    OBJ_DESTROY, agg_trmm
    
    agg_file = fdir + '3B42_agg.2008_10_01.nc'
    if FILE_TEST(agg_file) then FILE_DELETE, agg_file
    log_file = fdir + 'trmm_agg_2008_10_01.log'
    if FILE_TEST(log_file) then FILE_DELETE, log_file
    
    if error ne 0 then message, '% TEST_TRMM_AGG NOT passed', /CONTINUE else print, 'TEST_TRMM_AGG passed'

end

pro TEST_WRF_OUT
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------    
    dom1 = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d01_2008-10-26')    
    dom1->get_time, time, nt, t0, t1    
    if nt ne 13 then error += 1    
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 12) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 28, hour = 0) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time[1] ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 15) then error += 1

    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver     
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if nx ne 150 then error += 1
    if ny ne 150 then error += 1
    if c.nx ne 150 then error +=1
    if c.ny ne 150 then error +=1
    if jpar ne 1 then error +=1
    if ipar ne 1 then error +=1
    if rat ne 1 then error +=1
    if dom ne 1 then error +=1
    if hstep.hour ne 3 then error += 1
    if bt ne 27 then error += 1
    if typ ne 'WRF' then error += 1
    if ver ne 'OUTPUT FROM WRF V3.1.1 MODEL' then error+=1 
    
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    GIS_make_datum, ret, dat, NAME='WGS-84'
    dom1->transform_LonLat, lon, lat, dat, i, j, /NEAREST
    
    utils_1d_to_2d, INDGEN(150), INDGEN(150), ti, tj
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_LonLat, lon+0.01, lat-0.01, dat, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_IJ, ti, tj, dom1, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    ;temoin    
    id = NCDF_OPEN(fdir+'wrfout_d01_2008-10-26', /NOWRITE)
    NCDF_VARGET, id, 'XLONG', olon
    NCDF_VARGET, id, 'XLAT', olat
    NCDF_VARGET, id, 'T2', ot2
    NCDF_VARGET, id, 'P', op
    NCDF_CLOSE, id
    
    olon = olon[*,*,0]
    olat = olat[*,*,0]        
    if max(abs(olat-lat)) ne 0 then  error += 1
    if max(abs(olon-lon)) ne 0 then  error += 1   
    
    if max(abs(dom1->get_var('t2')-ot2)) ne 0 then  error += 1
    if max(abs(dom1->get_var('p')-op)) ne 0 then  error += 1
    
    p = dom1->get_var('p', stime, snt, t0 = time[3], t1 = time[8])
    if snt ne 6 then error +=1
    if max(abs(p-op[*,*,*,3:8])) ne 0 then  error += 1
    
    dom1->QuickPlotVar, 'T2', t0 = time[3], t1 = time[8], wid = wid
    ok = DIALOG_MESSAGE('Do you see a temperature plot?', /QUESTION)
    if ok eq 'No' then error += 1
     WIDGET_CONTROL, wid, /DESTROY
     
     
     ;----------------------------
     ; CROP BORDER
     ;----------------------------
     t2_bef_crop = dom1->get_var('t2')     
     ok = dom1->define_subset(CROPBORDER=5)
     if ~ok then error+=1
     
    dom1->get_time, time, nt, t0, t1    
    if nt ne 13 then error += 1    
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 12) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 28, hour = 0) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time[1] ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 15) then error += 1

    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver
     
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if nx ne 140 then error += 1
    if ny ne 140 then error += 1
    if c.nx ne 140 then error +=1
    if c.ny ne 140 then error +=1
    if crop ne 'BORDER' then error += 1
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    if max(abs(olon[5:144,5:144]-lon)) gt 1e-4 then  error += 1
    if max(abs(olat[5:144,5:144]-lat)) gt 1e-4 then  error += 1
    
    
    GIS_make_datum, ret, dat, NAME='WGS-84'
    dom1->transform_LonLat, lon, lat, dat, i, j, /NEAREST
    
    utils_1d_to_2d, INDGEN(140), INDGEN(140), ti, tj
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_LonLat, lon+0.01, lat-0.01, dat, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_IJ, ti, tj, dom1, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
 
    if max(abs(dom1->get_var('t2',t0 = time[2],t1 = time[4])-ot2[5:144,5:144,2:4])) ne 0 then  error += 1
    t2_after_crop = dom1->get_var('t2')
    dom1_crop = dom1->reGrid()
    
    ;----------------------------
    ; RESET
    ;----------------------------
    ok = dom1->define_subset()
    if ~ok then error+=1    
    
    ; TEST grid regrid
    ok = dom1->subset(t2_bef_crop, OUT_GRID = out_grid, OUT_DATA=out_data)  
    if ~ ok then error+=1
    if TOTAL(ABS(out_data-t2_bef_crop) ne 0) then error+=1
    if ~utils_compare_grid(dom1, out_grid) then error+=1 
    OBJ_DESTROY, out_grid
    ; TEST grid regrid
    ok = dom1->subset(t2_bef_crop, CROPBORDER=5, OUT_GRID = out_grid, OUT_DATA=out_data)    
    if ~ ok then  error+=1
    if TOTAL(ABS(out_data-t2_after_crop)) ne 0 then error+=1
    if ~utils_compare_grid(dom1_crop, out_grid) then error+=1
    OBJ_DESTROY, out_grid
    
    ; TEST grid regrid
    ok = dom1->subset(t2_bef_crop, CORNERS=[5,5,144,144], OUT_GRID = out_grid, OUT_DATA=out_data)    
    if ~ ok then  error+=1
    if TOTAL(ABS(out_data-t2_after_crop)) ne 0 then error+=1
    if ~utils_compare_grid(dom1_crop, out_grid) then error+=1
    
    OBJ_DESTROY, out_grid
    OBJ_DESTROY, dom1_crop       
    
    
    dom1->get_time, time, nt, t0, t1    
    if nt ne 13 then error += 1    
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 12) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 28, hour = 0) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time[1] ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 15) then error += 1

    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver
     
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if nx ne 150 then error += 1
    if ny ne 150 then error += 1
    if c.nx ne 150 then error +=1
    if c.ny ne 150 then error +=1
    if jpar ne 1 then error +=1
    if ipar ne 1 then error +=1
    if rat ne 1 then error +=1
    if dom ne 1 then error +=1
    if hstep.hour ne 3 then error += 1
    if bt ne 27 then error += 1
    if typ ne 'WRF' then error += 1
    if ver ne 'OUTPUT FROM WRF V3.1.1 MODEL' then error+=1 
    
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    GIS_make_datum, ret, dat, NAME='WGS-84'
    dom1->transform_LonLat, lon, lat, dat, i, j, /NEAREST
    
    utils_1d_to_2d, INDGEN(150), INDGEN(150), ti, tj
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_LonLat, lon+0.01, lat-0.01, dat, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_IJ, ti, tj, dom1, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    

     ;----------------------------
     ; CROP CHILD
     ;----------------------------
     ok = dom1->define_subset(/CROPCHILD)
     if ~ok then error+=1
     
    dom1->get_time, time, nt, t0, t1    
    if nt ne 13 then error += 1    
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 12) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 28, hour = 0) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1
    if time[1] ne QMS_TIME(year = 2008, month = 10, day = 26, hour = 15) then error += 1

    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver
    if crop ne 'CROPCHILD' then error += 1 
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if nx ne 50 then error += 1
    if ny ne 50 then error += 1
    if c.nx ne 50 then error +=1
    if c.ny ne 50 then error +=1
    
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    ;temoin    
    id = NCDF_OPEN(fdir+'wrfout_d02_2008-10-26', /NOWRITE)
    NCDF_VARGET, id, 'XLONG', lon2
    NCDF_VARGET, id, 'XLAT', lat2
    NCDF_CLOSE, id
    
    if max(abs(gislon[0,0]-lon2[1,1,0])) gt 1e-4 then  error += 1
    if max(abs(gislat[0,0]-lat2[1,1,0])) gt 1e-4 then  error += 1
    if max(abs(gislon[49,49]-lon2[148,148,0])) gt 1e-4 then  error += 1
    if max(abs(gislat[49,49]-lat2[148,148,0])) gt 1e-4 then  error += 1
    if max(abs(gislat[0,1]-lat2[1,4,0])) gt 1e-4 then  error += 1
    
    td2 = dom1->reGrid(factor=3)
    td2->get_LONLAT, tdlon, tdlat, tdx, tdy
    if max(abs(tdlon[0:20,0:20]-lon2[0:20,0:20,0])) gt 1e-4 then  error += 1 ;because dom2-3 test not good everywhere
    if max(abs(tdlat[0:20,0:20]-lat2[0:20,0:20,0])) gt 1e-4 then  error += 1 
    if tdx ne 150 then error+=1
    if tdy ne 150 then error+=1
    OBJ_DESTROY, td2
    
    ;----------------------------
    ; CROP SUBSET
    ;----------------------------
    ok = dom1->define_subset(SUBSET_LL=[91.,31.,93.,33.], SUBSET_IJ=sij)
    if ~ok then error+=1
    dom1->get_LONLAT, tdlon, tdlat, tdx, tdy
        
    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver
     if crop ne 'SUBSET' then error += 1 
    
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    if abs(GISLON[0,0]-91.) gt MEAN(GISLON[1:*,0]-GISLON[0:NX-2,0])/2. then error +=1
    if abs(GISLON[NX-1,Ny-1]-93.) gt MEAN(GISLON[1:*,ny-1]-GISLON[0:NX-2,ny-1])/2. then error +=1
    if abs(GISLAT[0,0]-31.) gt MEAN(GISLAT[0,1:*]-GISLAT[0,0:Ny-2])/2. then error +=1
    if abs(GISLAT[NX-1,Ny-1]-33.) gt MEAN(GISLAT[nx-1,1:*]-GISLAT[nx-1,0:Ny-2])/2. then error +=1
    
    t2_after_crop = dom1->get_var('t2')
    dom1_crop = dom1->reGrid()
    
    
    ; TEST grid regrid
    ok = dom1->define_subset()
    if ~ok then error+=1    
    ok = dom1->subset(t2_bef_crop, OUT_GRID = out_grid, OUT_DATA=out_data)  
    if ~ ok then error+=1
    if TOTAL(ABS(out_data-t2_bef_crop) ne 0) then error+=1
    if ~utils_compare_grid(dom1, out_grid) then error+=1 
    OBJ_DESTROY, out_grid
    
    ; TEST grid regrid
    GIS_make_datum, ret, src, NAME='WGS-84'
    ok = dom1->subset(t2_bef_crop, CORNERS=[91.,31.,93.,33.], src =src, OUT_GRID = out_grid, OUT_DATA=out_data)    
    if ~ ok then  error+=1
    if TOTAL(ABS(out_data-t2_after_crop)) ne 0 then error+=1
    if ~utils_compare_grid(dom1_crop, out_grid) then error+=1    
    OBJ_DESTROY, out_grid
    OBJ_DESTROY, dom1_crop         
    
    ;----------------------------
    ; TEST TS 
    ;----------------------------
    ok = dom1->define_subset(/CROPBORDER)
    if ~ok then error+=1
    dom1->get_LONLAT, lon, lat, nx, ny
    
    plon = DOM1->get_TimeSerie('XLONG', 90.,29.,SRC=dat)
    plat = DOM1->get_TimeSerie('XLAT', 90.,29.,SRC=dat)
    
    if abs(pLON[0]-90.) gt MEAN(LON[1:*,0]-LON[0:NX-2,0])/2. then error +=1
    if abs(pLAT[0]-29.) gt MEAN(LAT[0,1:*]-LAT[0,0:Ny-2])/2. then error +=1
    
    dom1->plot_TimeSerie, 'T2', 90.1, 31.2, src = dat 
    ok = DIALOG_MESSAGE('Do you see a temperature time serie?', /QUESTION)
    if ok eq 'No' then error += 1
    cgDelete, /ALL
    OBJ_DESTROY, dom1     
    if error ne 0 then message, '% TEST_WRF_OUT NOT passed', /CONTINUE else print, 'TEST_WRF_OUT passed'
        
end

pro TEST_MODIS
    
    fdir = TEST_file_directory() + 'MODIS/'
    error = 0 
        
    lst = OBJ_NEW('w_MODIS', FILE=fdir+'MOD11A2.A2008297.h25v05.005.2008311141349.hdf')
    
    lst->get_time, t0, t1       
    if t0 ne QMS_TIME(year = 2008, month = 10, day = 23, hour = 00) then error += 1
    if t1 ne QMS_TIME(year = 2008, month = 10, day = 30, hour = 00) then error += 1
    lst->Get_LonLat, lon, lat, nx, ny, dat
    
    if ABS(lon[0,0] - 80.8358300128709d) gt abs(lon[1,0]-lon[0,0])/2.then error +=1    
    if ABS(lat[0,0] - 30.0041666666667d) gt abs(lat[0,1]-lat[0,0])/2. then error +=1 
    if ABS(lon[nx-1,ny-1] - 104.421709972251d) gt abs((lon[nx-1,ny-1]-lon[nx-2,ny-1])/2.) then error +=1 
    if ABS(lat[nx-1,ny-1] - 39.9958333333333d) gt abs((lat[nx-1,ny-1]-lat[nx-1,ny-2])/2.) then error +=1 
       
    dom2 = OBJ_NEW('w_WRF', FILE= TEST_file_directory() + 'WRF/wrfout_d02_2008-10-26', CROPBORDER=12)
    map = OBJ_NEW('w_Map', dom2, YSIZE=400)   
    GIS_make_proj, ret, utm, PARAM='2, 46, WGS-84'
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    d = map->set_shading_params(RELIEF_FACTOR=1.)
    d = map->set_map_params(INTERVAL=5)
    d = map->set_shape_file(SHPFILE= TEST_file_directory() + '/MAPPING/namco_shore.shp', SHP_SRC=utm, REMOVE_ENTITITES=53)    
    d = map->set_data(lst->get_var('LST_Day_1km')-273.15, lst, missing = -273.15)
    CTLOAD, 13
    d=map->set_Plot_Params(N_LEVELS=126, MIN_VALUE=-24, MAX_VALUE = 29)
    map->show_img, /RESIZABLE
    map->show_color_bar, /RESIZABLE
    ok = DIALOG_MESSAGE('Do you see a modis projected image?', /QUESTION)
    if ok eq 'No' then error += 1
    
    ok =  lst->define_subset(SUBSET_LL= [90, 31.8, 91, 30.1])
    d = map->set_data(lst->get_var('LST_Day_1km')-273.15, lst, missing = -273.15)
    
    map->show_img, /RESIZABLE
    map->show_color_bar, /RESIZABLE
    ok = DIALOG_MESSAGE('Do you now see a subset of it?', /QUESTION)
    if ok eq 'No' then error += 1
    cgDelete, /ALL
    
    OBJ_DESTROY, lst     
    OBJ_DESTROY, map 
    OBJ_DESTROY,  dom2  
    
    ;====================
    ; MARCO file
    ;====================
    marco = OBJ_NEW('w_MODIS', FILE=fdir+'MOD13Q1.A2006113.h11v10.005.2008109011450.hdf')
    
    marco->transform, 0, 0, dummy1, dummy2, src=marco, LON_DST=lon00, LAT_DST=lat00
    marco->transform, 4799,4799, dummy1, dummy2, src=marco, LON_DST=lon11, LAT_DST=lat11    
    
    lon_envi00 = (- 74.492444D - 74.489241) / 2d 
    lat_envi00 = (- 20.D - 19.997917D) / 2d 
    lon_envi11 = (- 60.925597D - 60.928103D) / 2d 
    lat_envi11 = (- 10.D - 10.002083D) / 2d 
    
    if ABS(lon00 - lon_envi00) ge 1e-5 then error +=1 
    if ABS(lat00 - lat_envi00) ge 1e-5 then error +=1 
    if ABS(lon11 - lon_envi11) ge 1e-5 then error +=1 
    if ABS(lat11 - lat_envi11) ge 1e-5 then error +=1 
    
    OBJ_DESTROY, marco
    if error ne 0 then message, '% TEST_MODIS NOT passed', /CONTINUE else print, 'TEST_MODIS passed'
        
end

pro TEST_WRF_GEO
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    dom1 = OBJ_NEW('w_WRF', FILE=fdir+'geo_em.d03.nc')
    
    dom1->get_time, time, nt, t0, t1    
    if nt ne 1 then error += 1    
    if t0 ne QMS_TIME(year = 2000, month = 01, day = 1, hour = 0) then error += 1
    if N_ELEMENTS(time) ne nt then error += 1

    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver
     
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if nx ne 140 then error += 1
    if ny ne 100 then error += 1
    if c.nx ne 140 then error +=1
    if c.ny ne 100 then error +=1
    if rat ne 5 then error +=1
    if dom ne 3 then error +=1
    if hstep.hour ne 0 then error += 1
    if bt ne 1 then error += 1
    if typ ne 'GEO' then error += 1
    
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    GIS_make_datum, ret, dat, NAME='WGS-84'
    dom1->transform_LonLat, lon, lat, dat, i, j, /NEAREST
    
    utils_1d_to_2d, INDGEN(140), INDGEN(100), ti, tj
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_LonLat, lon+0.0001, lat-0.0001, dat, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
    dom1->transform_IJ, ti, tj, dom1, i, j, /NEAREST
    if max(abs(ti-i)) ne 0 then  error += 1
    if max(abs(tj-j)) ne 0 then  error += 1    
    
     ;----------------------------
     ; CROP BORDER
     ;----------------------------
     ok = dom1->define_subset(CROPBORDER=5)
     if ~ok then error+=1
     
    dom1->GetProperty, BOTTOM_TOP=bt, CROPPED=crop, HSTEP=hstep, LON=gislon, lat=GISlat, $
     dom = dom, I_PARENT_START=ipar, J_PARENT_START=jpar, PARENT_GRID_RATIO=rat, TNT_C = c, $
      type = typ, version = ver
     
    dom1->get_ncdf_coordinates, lon, lat, nx, ny
    
    if nx ne 130 then error += 1
    if ny ne 90 then error += 1
    if c.nx ne 130 then error +=1
    if c.ny ne 90 then error +=1
    if crop ne 'BORDER' then error += 1
    if max(abs(gislon-lon)) gt 1e-4 then  error += 1
    if max(abs(gislat-lat)) gt 1e-4 then  error += 1
    
    OBJ_DESTROY, dom1     
    if error ne 0 then message, '% TEST_WRF_GEO NOT passed', /CONTINUE else print, 'TEST_WRF_GEO passed'
        
end


pro TEST_W_MAP
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    wrf = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d01_2008-10-26')
    map =  OBJ_NEW('w_map', wrf, YSIZE=400)
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    d = map->set_shading_params(RELIEF_FACTOR=1)
    
    ok = wrf->define_subset(CROPBORDER=20)
    if not ok then error +=1
    
    T2 = (wrf->get_Var('T2'))[*,*,8]
    cgLoadCT, 13   
    ok = map->set_plot_params(N_LEVELS=125)
    if not ok then error +=1
    ok = map->set_data(t2, wrf, MISSING = -999.)
    if not ok then error +=1
    u = TOTAL(wrf->get_Var('U10', time, nt), 3)
    v = TOTAL(wrf->get_Var('V10'), 3)
    u = u / nt
    v = v / nt
    ok = map->set_wind(u, v, wrf, density = 3)
    if not ok then error +=1
    
    map->show_img, /RESIZABLE
    ok = DIALOG_MESSAGE('Do you see a temperature plot with wind vectors?', /QUESTION)
    if ok eq 'No' then error += 1
       
    ok = map->set_wind()
    map->show_img, /RESIZABLE
    if not ok then error +=1
    ok = DIALOG_MESSAGE('Do you see a temperature plot without wind vectors?', /QUESTION)
    if ok eq 'No' then error += 1
    cgDelete, /ALL
    OBJ_DESTROY, map  
    
    ok = wrf->define_subset(CROPBORDER=70) 
    map =  OBJ_NEW('w_map', wrf, YSIZE=400)
    d = map->set_shading_params(RELIEF_FACTOR=1)
    
    T2 = (wrf->get_Var('T2'))[*,*,8]
    cgLoadCT, 13   
    ok = map->set_plot_params(N_LEVELS=125)
    if not ok then error +=1
    ok = map->set_data(t2, wrf, MISSING = -999.)
    if not ok then error +=1
    u = TOTAL(wrf->get_Var('U10', time, nt), 3)
    v = TOTAL(wrf->get_Var('V10'), 3)
    u = u / nt
    v = v / nt
    ok = map->set_wind(u, v, wrf, density = 1)
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    if not ok then error +=1
    
    map->show_img, /RESIZABLE
    ok = DIALOG_MESSAGE('Do you see a ZOOMED temperature plot with wind vectors?', /QUESTION)
    if ok eq 'No' then error += 1
    cgDelete, /ALL
    
    OBJ_DESTROY, wrf     
    OBJ_DESTROY, map  
    
    if error ne 0 then message, '% TEST_W_MAP NOT passed', /CONTINUE else print, 'TEST_W_MAP passed'
        
end

pro TEST_W_STANDARD_PLOT
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
        
    wrf = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d01_2008-10-26')
    
    map =  OBJ_NEW('w_map', wrf, YSIZE=400)
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    d = map->set_shading_params(RELIEF_FACTOR=1)
       
    T2 = (wrf->get_Var('T2'))[*,*,8] - 273.15
    
    cgLoadCT, 13   
    ok = map->set_data(t2, wrf, MISSING = -999., VAL_MIN=-5.)
    if not ok then error +=1
    
    ok = map->set_plot_params(N_LEVELS=10, NEUTRAL_COLOR='pink')
    if not ok then error +=1
    
    w_standard_2d_plot, map, TITLE='My Test',$
                             BAR_TITLE='DegC',  $
                             /BAR_OPEN,  $
                             SOURCE_INFO='(c) FG Klimatologie', $
                             /RESIZABLE,  $
                             PNG='test_400.png', $
                            IM_RESIZE = 75
                            
    OBJ_DESTROY, map
    
    map =  OBJ_NEW('w_map', wrf, YSIZE=600)
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    d = map->set_shading_params(RELIEF_FACTOR=1)    
    cgLoadCT, 13   
    ok = map->set_data(t2, wrf, MISSING = -999., VAL_MIN=-5.)
    if not ok then error +=1    
    ok = map->set_plot_params(N_LEVELS=10, NEUTRAL_COLOR='white')
    if not ok then error +=1    
    w_standard_2d_plot, map, TITLE='My Test',$
                             BAR_TITLE='DegC',  $
                             /BAR_OPEN,  $
                             SOURCE_INFO='(c) FG Klimatologie', $
                             /RESIZABLE,  $
                             PNG='test_600.png', $
                            IM_RESIZE = 75                            
    OBJ_DESTROY, map
    
    map =  OBJ_NEW('w_map', wrf, YSIZE=800)
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    d = map->set_shading_params(RELIEF_FACTOR=1)    
    cgLoadCT, 13   
    ok = map->set_data(t2, wrf, MISSING = -999., VAL_MIN=-5.)
    if not ok then error +=1    
    ok = map->set_plot_params(N_LEVELS=10, NEUTRAL_COLOR='white')
    if not ok then error +=1    
    w_standard_2d_plot, map, TITLE='My Test',$
                             BAR_TITLE='DegC',  $
                             /BAR_OPEN,  $
                             SOURCE_INFO='(c) FG Klimatologie', $
                             /RESIZABLE,  $
                             PNG='test_800.png', $
                            IM_RESIZE = 75                            
    OBJ_DESTROY, map
    
    map =  OBJ_NEW('w_map', wrf, YSIZE=1000)
    d = map->set_topography(GRDFILE=TEST_file_directory() + '/MAPPING/TiP.grd')
    d = map->set_shading_params(RELIEF_FACTOR=1)    
    cgLoadCT, 13   
    ok = map->set_data(t2, wrf, MISSING = -999., VAL_MIN=-5.)
    if not ok then error +=1    
    ok = map->set_plot_params(N_LEVELS=10, NEUTRAL_COLOR='white')
    if not ok then error +=1    
    w_standard_2d_plot, map, TITLE='My Test',$
                             BAR_TITLE='DegC',  $
                             /BAR_OPEN,  $
                             SOURCE_INFO='(c) FG Klimatologie', $
                             /RESIZABLE,  $
                             PNG='test_1000.png', $
                            IM_RESIZE = 75                            
    OBJ_DESTROY, map
    
    
    OBJ_DESTROY, wrf     

        
end


pro TEST_POST_COPY_CROP, REDO = redo
   
    fdir = TEST_file_directory()
    error = 0 
    
    @WAVE.inc
    
    ;----------------
    ; TEST copy crop
    ;----------------     
    INPUT_DIR= fdir + '/WRF_POST/'
    OUTPUT_DIR= fdir + '/WRF_CPY_CROP/'
    
    if ~ KEYWORD_SET(REDO) then redo = false    
    if FILE_TEST(OUTPUT_DIR) and REDO then FILE_DELETE, OUTPUT_DIR, /RECURSIVE 
    if ~FILE_TEST(OUTPUT_DIR) then REDO = true else REDO = false
    
    if redo then POST_cpy_crop_directory, INPUT_DIR=INPUT_DIR, OUTPUT_DIR=OUTPUT_DIR
    
    if ~FILE_TEST(OUTPUT_DIR) then error+=1
    if ~FILE_TEST(OUTPUT_DIR+'/2009.09.13/') then error+=1
    if ~FILE_TEST(OUTPUT_DIR+'/2009.09.14/') then error+=1
    if ~FILE_TEST(OUTPUT_DIR+'/wrf_cpy_crop.log') then error+=1
    
    
    ;----------------
    ; D1 day one
    ;----------------        
    origf =  fdir + '/WRF_POST/d1/wrfout_d01_2009-09-12_12_00_00'
    outf =  fdir + '/WRF_CPY_CROP/2009.09.13/wrfout_d01_2009-09-13_00_00_00_24h.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 9 then error += 1
    if to1 ne ta1 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09) then error += 1
    
    orig->get_Varlist, oid, onames
    out->get_Varlist, aid, anames
    
    if TOTAL(oid - aid) ne 0 then error += 1
    for i = 0, N_ELEMENTS(onames) - 1 do if onames[i] ne anames[i] then error += 1 
    
    t2o = orig->get_Var('t2', t0 = ta0 , t1 = ta1)
    t2a = out->get_Var('t2')    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    t2o = orig->get_Var('U', t0 = ta0 , t1 = ta1)
    t2a = out->get_Var('U')    
    if total(ABS(t2o-t2a)) ne 0 then error += 1    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    ;----------------
    ; D3 day one
    ;----------------        
    origf =  fdir + '/WRF_POST/d1/wrfout_d03_2009-09-12_12_00_00'
    outf =  fdir + '/WRF_CPY_CROP/2009.09.13/wrfout_d03_2009-09-13_00_00_00_24h.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 25 then error += 1
    if to1 ne ta1 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09) then error += 1
    
    orig->get_Varlist, oid, onames
    out->get_Varlist, aid, anames
    
    if TOTAL(oid - aid) ne 0 then error += 1
    for i = 0, N_ELEMENTS(onames) - 1 do if onames[i] ne anames[i] then error += 1 
    
    t2o = orig->get_Var('t2', t0 = ta0 , t1 = ta1)
    t2a = out->get_Var('t2')    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    t2o = orig->get_Var('U', t0 = ta0 , t1 = ta1)
    t2a = out->get_Var('U')    
    if total(ABS(t2o-t2a)) ne 0 then error += 1    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    ;----------------
    ; D1 day two
    ;----------------        
    origf =  fdir + '/WRF_POST/d2/wrfout_d01_2009-09-13_12_00_00'
    outf =  fdir + '/WRF_CPY_CROP/2009.09.14/wrfout_d01_2009-09-14_00_00_00_24h.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 9 then error += 1
    if to1 ne ta1 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 14, month = 09) then error += 1
    
    orig->get_Varlist, oid, onames
    out->get_Varlist, aid, anames
    
    if TOTAL(oid - aid) ne 0 then error += 1
    for i = 0, N_ELEMENTS(onames) - 1 do if onames[i] ne anames[i] then error += 1 
    
    t2o = orig->get_Var('t2', t0 = ta0 , t1 = ta1)
    t2a = out->get_Var('t2')    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    t2o = orig->get_Var('U', t0 = ta0 , t1 = ta1)
    t2a = out->get_Var('U')    
    if total(ABS(t2o-t2a)) ne 0 then error += 1    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    if error ne 0 then message, '% TEST_POST_COPY_CROP NOT passed', /CONTINUE else print, 'TEST_POST_COPY_CROP passed'
    
end


pro TEST_POST_AGG, REDO = redo
   
    fdir = TEST_file_directory()
    error = 0 
    
    @WAVE.inc
    
    ;----------------
    ; TEST copy crop
    ;----------------     
    INPUT_DIR= fdir + '/WRF_POST/'
    OUTPUT_DIR= fdir + '/WRF_AGG/'
    
    if ~ KEYWORD_SET(REDO) then redo = false    
    if FILE_TEST(OUTPUT_DIR) and REDO then FILE_DELETE, OUTPUT_DIR, /RECURSIVE 
    if ~FILE_TEST(OUTPUT_DIR) then REDO = true else REDO = false
      
    if redo then POST_aggregate_directory, 1, INPUT_DIR, OUTDIR=OUTPUT_DIR + '/dom1/'    
    if redo then POST_aggregate_directory, 3, INPUT_DIR, OUTDIR=OUTPUT_DIR + '/dom3/'    
    if ~FILE_TEST(OUTPUT_DIR) then error+=1
    
    ; DOM 1 day one
    if ~FILE_TEST(OUTPUT_DIR+'/dom1//wrf_cpy_2009_09_13_d01.log') then error+=1    
    origf =  INPUT_DIR+ '/d1/wrfout_d01_2009-09-12_12_00_00'
    outf =  OUTPUT_DIR + '/dom1/wrf_agg_2009_09_13_d01.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 16 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09, hour = 3) then error += 1
        
    t2o = orig->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 3))
    t2a = out->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 3), t1 = t01)    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    ; DOM 1 day two
    if ~FILE_TEST(OUTPUT_DIR+'/dom1//wrf_cpy_2009_09_13_d01.log') then error+=1    
    origf =  INPUT_DIR+ '/d2/wrfout_d01_2009-09-13_12_00_00'
    outf =  OUTPUT_DIR + '/dom1/wrf_agg_2009_09_13_d01.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 16 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09, hour = 3) then error += 1
    if ta1 ne to1 then error += 1
       
    t2o = orig->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 3))
    t2a = out->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 3), t1 = t01)    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    ; DOM 3 day two
    if ~FILE_TEST(OUTPUT_DIR+'/dom3//wrf_cpy_2009_09_13_d03.log') then error+=1    
    origf =  INPUT_DIR+ '/d2/wrfout_d03_2009-09-13_12_00_00'
    outf =  OUTPUT_DIR + '/dom3/wrf_agg_2009_09_13_d03.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 48 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09, hour = 1) then error += 1
    if ta1 ne to1 then error += 1
       
    t2o = orig->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 1))
    t2a = out->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 1), t1 = t01)    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    if error ne 0 then message, '% TEST_POST_AGG NOT passed', /CONTINUE else print, 'TEST_POST_AGG passed'
    
end

pro TEST_POST_AGG_CROPPED, REDO = redo
   
    fdir = TEST_file_directory()
    error = 0 
    
    @WAVE.inc
    
    ;----------------
    ; TEST copy crop
    ;----------------     
    INPUT_DIR= fdir + '/WRF_CPY_CROP/'
    OUTPUT_DIR= fdir + '/WRF_AGG_CROPPED/'
    
    if ~FILE_TEST(INPUT_DIR) then TEST_POST_COPY_CROP
    
    if ~ KEYWORD_SET(REDO) then redo = false    
    if FILE_TEST(OUTPUT_DIR) and REDO then FILE_DELETE, OUTPUT_DIR, /RECURSIVE 
    if ~FILE_TEST(OUTPUT_DIR) then REDO = true else REDO = false
      
    if redo then POST_aggregate_directory, 1, INPUT_DIR, OUTDIR=OUTPUT_DIR + '/dom1/', SPINUP_INDEX=1, END_INDEX=8 , START_TIME =   QMS_TIME(year = 2009, day = 13, month = 09, hour = 3), END_TIME=QMS_TIME(year = 2009, day = 15, month = 09, hour = 0)
    if redo then POST_aggregate_directory, 3, INPUT_DIR, OUTDIR=OUTPUT_DIR + '/dom3/', SPINUP_INDEX=1, END_INDEX=24, START_TIME =   QMS_TIME(year = 2009, day = 13, month = 09, hour = 1), END_TIME=QMS_TIME(year = 2009, day = 15, month = 09, hour = 0)
    if ~FILE_TEST(OUTPUT_DIR) then error+=1
    
    ; DOM 1 day one
    if ~FILE_TEST(OUTPUT_DIR+'/dom1//wrf_cpy_2009_09_13_d01.log') then error+=1    
    origf =  INPUT_DIR+ '/2009.09.13/wrfout_d01_2009-09-13_00_00_00_24h.nc'
    outf =  OUTPUT_DIR + '/dom1/wrf_agg_2009_09_13_d01.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 16 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09, hour = 3) then error += 1
        
    t2o = orig->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 3))
    t2a = out->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 3), t1 = t01)    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    t2o = orig->get_Var('RAINNC', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 00))
    t2o = t2o[*,*,8] - t2o[*,*,0]
    t2a = out->get_Var('RAINNC', t0 = to1, t1 = to1)    
    if total(ABS(t2o-t2a)) gt 1e-3 then error += 1
    
    t2o = orig->get_Var('RAINNC', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 00))
    t2o = (utils_ACC_TO_STEP(t2o))[*,*,1:*]    
    t2a = out->get_Var('RAINNC_STEP', t0 = QMS_TIME(year = 2009, day = 13, month = 09, hour = 03), t1 = t01)    
    if total(ABS(t2o-t2a)) gt 1e-3 then error += 1
    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    ; DOM 1 day two
    if ~FILE_TEST(OUTPUT_DIR+'/dom1//wrf_cpy_2009_09_13_d01.log') then error+=1    
    origf =  INPUT_DIR+ '/2009.09.14/wrfout_d01_2009-09-14_00_00_00_24h.nc'
    outf =  OUTPUT_DIR + '/dom1/wrf_agg_2009_09_13_d01.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 16 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09, hour = 3) then error += 1
    if ta1 ne to1 then error += 1
       
    t2o = orig->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 3))
    t2a = out->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 3), t1 = t01)    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
        
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    ; DOM 3 day two
    if ~FILE_TEST(OUTPUT_DIR+'/dom3//wrf_cpy_2009_09_13_d03.log') then error+=1    
    origf =  INPUT_DIR+ '/2009.09.14/wrfout_d03_2009-09-14_00_00_00_24h.nc'
    outf =  OUTPUT_DIR + '/dom3/wrf_agg_2009_09_13_d03.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('w_WRF', FILE=origf)
    out = OBJ_NEW('w_WRF', FILE=outf)
    
    orig->get_time, to, nto, to0, to1
    out->get_time, ta, nta, ta0, ta1    
    if nta ne 48 then error += 1
    if ta0 ne QMS_TIME(year = 2009, day = 13, month = 09, hour = 1) then error += 1
    if ta1 ne to1 then error += 1
       
    t2o = orig->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 1))
    t2a = out->get_Var('t2', t0 = QMS_TIME(year = 2009, day = 14, month = 09, hour = 1), t1 = t01)    
    if total(ABS(t2o-t2a)) ne 0 then error += 1
    
    OBJ_DESTROY, orig
    OBJ_DESTROY, out
    
    if error ne 0 then message, '% TEST_POST_AGG_CROPPED NOT passed', /CONTINUE else print, 'TEST_POST_AGG_CROPPED passed'
    
end

pro TEST_WRF_AGG_MASSGRID
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    dom1 = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d01_2008-10-26')
    dom2 = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d02_2008-10-26', CROPBORDER=3)
    
    d2pcp = (dom2->get_var('prcp'))[*,*,36]
    d1pcp = dom2->map_gridded_data((dom1->get_var('prcp'))[*,*,12], dom1)
    
    d2pcp = UTILS_aggregate_Grid_data(d2pcp, 3)
    d1pcp = UTILS_aggregate_Grid_data(d1pcp, 3)
    
    ok = DOM1->define_subset(/CROPCHILD)
    orig = (DOM1->get_var('prcp'))[*,*,12]
    
    if max(abs(d1pcp - d2pcp)) gt 0.2 then error +=1
    if max(abs(d1pcp - orig[1:48,1:48])) gt 0.001 then error +=1
    if max(abs(d2pcp - orig[1:48,1:48])) gt 0.2 then error +=1

    OBJ_DESTROY, dom1     
    OBJ_DESTROY, dom2     
    
    if error ne 0 then message, '% TEST_WRF_AGG_MASSGRID NOT passed', /CONTINUE else print, 'TEST_WRF_AGG_MASSGRID passed'
        
end


pro TEST_REGRID
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    dom1 = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d01_2008-10-26', /CROPCHILD)
    dom2 = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d02_2008-10-26')    
    reg = dom1->reGrid(FACTOR=3)
   
    reg->get_LonLat, rlon, rlat, rnx, rny
    dom2->get_LonLat, lon, lat, nx, ny
    
    if rnx ne nx then error +=1
    if rny ne ny then error +=1
    if max(abs(lon - rlon)) gt 0.0001 then error +=1
    if max(abs(lat - rlat)) gt 0.0001 then error +=1
    
    
    luN = (dom2->get_Var('LU_INDEX'))[*,*,0]
    OBJ_DESTROY, reg
    
    reg = dom2->reGrid(FACTOR=5)
    reg->GetProperty, TNT_C=c
    
    luN_congrid = congrid(luN, c.nx, c.ny, /CENTER)
    luN_trans = reg->map_gridded_data(luN, dom2)
    
    if TOTAL(ABS(luN_congrid-luN_trans)) ne 0 then error +=1

    OBJ_DESTROY, dom1     
    OBJ_DESTROY, dom2     
    OBJ_DESTROY, reg     
    
    if error ne 0 then message, '% TEST_REGRID NOT passed', /CONTINUE else print, 'TEST_REGRID passed'
        
end

pro TEST_NEIREST_NEIGHBOR

  @WAVE.inc
  error = 0
  
  fdir = TEST_file_directory() 
  error = 0 

  dom1 = OBJ_NEW('w_WRF', FILE=fdir+ '/WRF/wrfout_d01_2008-10-26', CROPBORDER=45)
  dom1->Get_LonLat, wrflon, wrflat, wx, wy
  trmm = OBJ_NEW('w_TRMM', FILE=fdir+'/TRMM/3B42.081001.3.6A.nc', SUBSET_LL = [min(wrflon)-2,min(wrflat)-2,max(wrflon)+2,max(wrflat)+2])
  trmm->Get_LonLat, trmmlon, trmmlat, tx, ty
  data = trmm->get_prcp()  
  
  syst = QMS_TIME()
  p_c = utils_nearest_neighbor(trmmlon, trmmlat, wrflon, wrflat, /CLASSICAL, DISTANCES = dis_c)
  dc = utils_compute_nearest_neighbor(p_c, data)
;  print, 'T1 : ' + str_equiv((QMS_TIME()-syst)/S_QMS)
  
  syst = QMS_TIME()
  p_t = utils_nearest_neighbor(trmmlon, trmmlat, wrflon, wrflat, /TRIANGULATION, DISTANCES = dis_t)
  dt = utils_compute_nearest_neighbor(p_t, data)
;  print, 'T2 : ' + str_equiv((QMS_TIME()-syst)/S_QMS)

  if total(p_c-p_t) ne 0 then error += 1
  if total(dis_t-dis_c) ne 0 then error += 1
  if total(dc-dt) ne 0 then error += 1

  syst = QMS_TIME()
  dgis = DOM1->map_gridded_data(data, trmm)
;  print, 'T3 : ' + str_equiv((QMS_TIME()-syst)/S_QMS)
  
  if total(dc-dgis) ne 0 then error += 1

  OBJ_DESTROY, trmm
  OBJ_DESTROY, dom1
  
  if error ne 0 then message, '% TEST_NEIREST_NEIGHBOR NOT passed', /CONTINUE else print, 'TEST_NEIREST_NEIGHBOR passed'
  
  
end

pro TEST_MOSAIC

  @WAVE.inc
  error = 0
  
  fdir = TEST_file_directory() 
   
  ; Map
  dom1 = OBJ_NEW('w_WRF', FILE=fdir+'WRF/wrfout_d01_2008-10-26', CROPB=35)
  map = OBJ_NEW('w_Map', dom1, XSIZE= 400)
  OBJ_DESTROY, dom1  
  
  CTLOAD, 13
  ok = map->set_plot_params(N_LEVELS=255, MAX_VALUE=240, MIN_VALUE=1)
  
  ; Modiss
  !QUIET = 1
  h25v05 = OBJ_NEW('w_MODIS', FILE=fdir+'MODIS/MOSAIC/MOD10A1.A2008294.h25v05.005.2008299202523.hdf', SUBSET_LL=[89.,33.,96.,28.])
  h25v05->getProperty, tnt_c = c
  data1 = BYTARR(c.nx, c.ny) + 20B
  ok = map->set_data(data1, h25v05, MISSING=0)
  map->show_img, /RESIZABLE, TITLE= 'h25v05'
  h25v06 = OBJ_NEW('w_MODIS', FILE=fdir+'MODIS/MOSAIC/MOD10A1.A2008294.h25v06.005.2008299213852.hdf', SUBSET_LL=[89.,33.,94.,28.])
  h25v06->getProperty, tnt_c = c
  data2 = BYTARR(c.nx, c.ny) + 80B
  ok = map->set_data(data2, h25v06, MISSING=0)
  map->show_img , /RESIZABLE, TITLE= 'h25v06' 
  h26v05 = OBJ_NEW('w_MODIS', FILE=fdir+'MODIS/MOSAIC/MOD10A1.A2008294.h26v05.005.2008299222304.hdf', SUBSET_LL=[89.,33.,94.,28.])
  h26v05->getProperty, tnt_c = c
  data3 = BYTARR(c.nx, c.ny) + 160
  ok = map->set_data(data3, h26v05, MISSING=0)
  map->show_img , /RESIZABLE, TITLE= 'h26v05' 
  h26v06 = OBJ_NEW('w_MODIS', FILE=fdir+'MODIS/MOSAIC/MOD10A1.A2008294.h26v06.005.2008299220621.hdf', SUBSET_LL=[89.,33.,94.,28.])
  h26v06->getProperty, tnt_c = c
  data4 = BYTARR(c.nx, c.ny) + 240B
  ok = map->set_data(data4, h26v06, MISSING=0)
  map->show_img  , /RESIZABLE, TITLE= 'h26v05'
  !QUIET = 0  
  
  grids = [h25v05,h25v06,h26v05,h26v05]  
  mosaic = utils_MOSAIC_grid(grids)
  
  modata = MOSAIC->map_gridded_data(data1, h25v05, MISSING = 0)
  modata = MOSAIC->map_gridded_data(data2, h25v06, DATA_DST = modata)
  modata = MOSAIC->map_gridded_data(data3, h26v05, DATA_DST = modata)
  modata = MOSAIC->map_gridded_data(data4, h26v06, DATA_DST = modata)
  
  p = where(modata eq 0, cnt)
  if cnt ne 0 then error+=1
  
  ok = map->set_data(modata, mosaic, MISSING=0)
  map->show_img, /RESIZABLE, TITLE= 'Mosaic'    
  
  ok = DIALOG_MESSAGE('Do you see nice mosaic?', /QUESTION)
  if ok eq 'No' then error += 1
  cgDelete, /all
  
  OBJ_DESTROY, map
  OBJ_DESTROY, h25v05
  OBJ_DESTROY, h25v06
  OBJ_DESTROY, h26v05
  OBJ_DESTROY, h26v06
  OBJ_DESTROY, mosaic

  if error ne 0 then message, '% TEST_MOSAIC NOT passed', /CONTINUE else print, 'TEST_MOSAIC passed'
  
  
end

pro TEST_WRF_GETVAR
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------    
    wrf = OBJ_NEW('w_WRF', FILE=fdir+'wrfout_d01_2008-10-26')
    
    t0 = QMS_TIME(year = 2008, day = 26, month = 10, hour = 21)    
    tk_wrf = wrf->get_Var('tk', T0 = t0, T1 = t0)
    tk_ncl = tk_wrf * 0.    
    tk_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/tk/tk_d1_2008-10-26_21:00:00'
    OPENR, lun, tk_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     tk_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    if MAX(ABS(tk_ncl-tk_wrf)) gt 1e-3 then error +=1 
    
    
    t0 = QMS_TIME(year = 2008, day = 26, month = 10, hour = 18)    
    var_wrf = wrf->get_Var('rh2', T0 = t0, T1 = t0)
    var_ncl = var_wrf * 0.    
    var_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/rh/rh2_d1_2008-10-26_18:00:00'
    OPENR, lun, var_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     var_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun

    if MAX(ABS(var_ncl-var_wrf)) gt 1e-3 then error +=1 
    
    t0 = QMS_TIME(year = 2008, day = 26, month = 10, hour = 21)    
    var_wrf = wrf->get_Var('rh', T0 = t0, T1 = t0)
    var_ncl = var_wrf * 0.    
    var_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/rh/rh_d1_2008-10-26_21:00:00'
    OPENR, lun, var_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     var_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    if MAX(ABS(var_ncl-var_wrf)) gt 1e-3 then error +=1 
    
    var_wrf =(wrf->get_Var('rh'))[*,*,*,8]
    var_ncl = var_wrf * 0.    
    var_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/rh/rh_d1_2008-10-27_12:00:00'
    OPENR, lun, var_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     var_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    if MAX(ABS(var_ncl-var_wrf)) gt 1e-3 then error +=1 
    
    
    t0 = QMS_TIME(year = 2008, day = 26, month = 10, hour = 21)
    slp_wrf = wrf->get_Var('slp', T0 = t0, T1 = t0)
    slp_ncl = slp_wrf * 0.    
    slp_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/slp/slp_d1_2008-10-26_21:00:00'
    OPENR, lun, slp_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     slp_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun    
    if MAX(ABS(slp_ncl-slp_wrf)) gt 1e-3 then error +=1 
           
    t0 = QMS_TIME(year = 2008, day = 27, month = 10, hour = 06)
    slp_wrf = wrf->get_Var('slp', T0 = t0, T1 = t0)
    slp_ncl = slp_wrf * 0.    
    slp_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/slp/slp_d1_2008-10-27_06:00:00'
    OPENR, lun, slp_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     slp_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun   
    if MAX(ABS(slp_ncl-slp_wrf)) gt 1e-3 then error +=1 
    
    
    slp_wrf = (wrf->get_Var('slp', DIMNAMES=dm))[*,*,8]
    slp_ncl = slp_wrf * 0.    
    slp_f = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/WRF/slp/slp_d1_2008-10-27_12:00:00'
    OPENR, lun, slp_f, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     slp_ncl[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun   
    if MAX(ABS(slp_ncl-slp_wrf)) gt 1e-3 then error +=1 
    
    t0 = QMS_TIME(year = 2008, day = 27, month = 10, hour = 06)
    slp_wrf = wrf->get_Var('slp', T0 = t0, T1 = t0)
    slp_wrf_b = wrf->get_Var('slp_b', T0 = t0, T1 = t0)
    if mean(ABS(slp_wrf-slp_wrf_b)/slp_wrf) gt 1e-2 then error +=1 
    if max(ABS(slp_wrf-slp_wrf_b)/slp_wrf) gt 0.06 then error +=1
    
    t2 =  wrf->get_TimeSerie('t2', 54, 75) - 273.15
    t2c =  wrf->get_TimeSerie('t2c', 54, 75)
    if total(t2-t2c) ne 0 then  error +=1
    

    OBJ_DESTROY, wrf    
    if error ne 0 then message, '% TEST_WRF_GETVAR NOT passed', /CONTINUE else print, 'TEST_WRF_GETVAR passed'
end

pro TEST_TIME
  TEST_MAKE_ABS_DATE
  TEST_QMS_TIME
  TEST_JULIAN_DAYS
  TEST_MAKE_REL_DATE
  TEST_MAKE_TIME_STEP
  TEST_MAKE_TIME_SERIE
  TEST_MAKE_ENDED_TIME_SERIE
  TEST_check_TimeSerie
  TEST_TS_FILL_MISSING
  TEST_TS_MEAN
  TEST_TS_RESAMPLE
  TEST_TS_DIURNAL_MEANS  
end

pro TEST_DATASETS, NCDF = ncdf
  TEST_TRMM_3B42
  TEST_TRMM_3B42_daily
  TEST_TRMM_3B43  
  if KEYWORD_SET(NCDF) then  TEST_TRMM_AGG
  TEST_WRF_OUT
  TEST_WRF_GEO
  TEST_MODIS  
  TEST_W_MAP
end

pro TEST_UTILS
  TEST_WRF_AGG_MASSGRID
  TEST_NEIREST_NEIGHBOR
  TEST_MOSAIC
  TEST_REGRID
  TEST_WRF_GETVAR
end

pro TEST_POST, REDO = redo
  TEST_POST_COPY_CROP, REDO = redo
  TEST_POST_AGG, REDO = redo
  TEST_POST_AGG_CROPPED, REDO = redo
end

pro w_TEST, NCDF = ncdf,  REDO = redo
  TEST_TIME
  TEST_DATASETS, NCDF = ncdf
  TEST_UTILS
  if KEYWORD_SET(NCDF) then TEST_POST, REDO = redo
end

