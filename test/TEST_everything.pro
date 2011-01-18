function TEST_file_directory
   ; Put the WAVE test pack path here.
   return, '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/'     
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

pro TEST_check_TS

  @WAVE.inc

  error = 0  
  startTime = make_abs_date(YEAR=2005, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2005, MONTH=01, DAY=03, HOUR=00, MINUTE=01, SECOND=00)
  step = MAKE_TIME_STEP(hour = 1)
  
  goodTS = MAKE_ENDED_TIME_SERIE(startTime, endTime, TIMESTEP=step, NSTEPS = nsteps)
  
  ok = check_TS(goodTS, probableStep)
  
  if ok eq false then error+=1
  if probableStep.dms ne step.dms then error+=1
  
  badTS = [goodTS[0:4],goodTS[6:9],goodTS[11:*]]
  
  ok = check_TS(badTS, probableStep, FULL_TS=fullTS, IND_MISSING=mis)
  
  if ok eq TRUE then error+=1
  if step.dms ne probableStep.dms then error+=1
  if total(fullTS.qms-goodTS.qms) ne 0 then error+=1
  if N_ELEMENTS(mis) ne 2 then  error+=1
  if mis[0] ne 5 then error+=1
  if mis[1] ne 10 then error+=1  
  
  if error ne 0 then message, '% TEST_check_TS NOT passed', /CONTINUE else print, 'TEST_check_TS passed'
  
end

pro time_bug


  print, 'refDate: 14.10.1984 05:10:08'   
  
  refDate = GEN_MAKE_TIME(YEAR=1984, MONTH=10, DAY=14, HOUR=5, MINUTE=10, SECOND=8)
  print, 'STIME(refDate.qt): ' + STIME(refDate.qt)
  
  myrefDate = make_abs_date(YEAR=1984, MONTH=10, DAY=14, HOUR=5, MINUTE=10, SECOND=8)
  print, 'TIME_to_STR(myrefDate.qms): ' + TIME_to_STR(myrefDate.qms)
  
  
end

pro TEST_TIME
  TEST_MAKE_ABS_DATE
  TEST_QMS_TIME
  TEST_JULIAN_DAYS
  TEST_MAKE_REL_DATE
  TEST_MAKE_TIME_STEP
  TEST_MAKE_TIME_SERIE
  TEST_MAKE_ENDED_TIME_SERIE
  TEST_check_TS
end


pro TEST_TRMM_3B42
    
    fdir = TEST_file_directory() + 'TRMM/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    trmm_3B42 = OBJ_NEW('TRMM_nc', FILE=fdir+'3B42.081001.3.6A.nc')
    
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
    if trmm_3B42->get_TS('lon', 0, 0)  ne -179.875 then error += 1
    if trmm_3B42->get_TS('lon', 0, 399) ne -179.875 then error += 1
    if trmm_3B42->get_TS('lon', 1439,0) ne 179.875 then error += 1
    if trmm_3B42->get_TS('lon', 1439,399) ne 179.875 then error += 1    
    if trmm_3B42->get_TS('lat', 0, 0)  ne -49.875 then error += 1
    if trmm_3B42->get_TS('lat', 1439,0) ne -49.875 then error += 1
    if trmm_3B42->get_TS('lat', 0,399) ne 49.875 then error += 1
    if trmm_3B42->get_TS('lat', 1439,399) ne 49.875 then error += 1
    
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
    
    ts = trmm_3B42->get_Pcp_TS(1,1, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
    if ts ne varPcp[1,1] then error += 1
    if pi ne 1 then error += 1
    if pj ne 1 then error += 1    
    if plon ne -179.875+0.250 then error += 1
    if plat ne -49.875+0.250  then error += 1
    
    GIS_make_datum, ret, datum, NAME='WGS-84'
    ts = trmm_3B42->get_Pcp_TS(-179.870,-49.870, SRC=datum, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
    if ts ne varPcp[0,0] then error += 1
    if pi ne 0 then error += 1
    if pj ne 0 then error += 1    
    if plon ne -179.875 then error += 1
    if plat ne -49.875 then error += 1
    
    GIS_make_datum, ret, datum, NAME='WGS-84'
    ts = trmm_3B42->get_Pcp_TS(94.875,19.875, SRC=datum, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
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
            
    trmm_3B42->QuickPlotPrcp
    ok = DIALOG_MESSAGE('Do you see a plot?', /QUESTION)
    if ok eq 'No' then error += 1
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
    if trmm_3B42->get_TS('lon', 0, 0)  ne 70.125 then error += 1
    if trmm_3B42->get_TS('lon', 0, ny-1) ne 70.125 then error += 1
    if trmm_3B42->get_TS('lon', nx-1,0) ne 120.125 then error += 1
    if trmm_3B42->get_TS('lon', nx-1,ny-1) ne 120.125 then error += 1    
    if trmm_3B42->get_TS('lat', 0, 0)  ne 10.125 then error += 1
    if trmm_3B42->get_TS('lat', nx-1,0) ne 10.125 then error += 1
    if trmm_3B42->get_TS('lat', 0,ny-1) ne 45.125 then error += 1
    if trmm_3B42->get_TS('lat', nx-1,ny-1) ne 45.125 then error += 1
    
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
    ts = trmm_3B42->get_Pcp_TS(94.875,19.875, SRC=datum, POINT_I=pi, POINT_J=pj, POINT_LAT=plat, POINT_LON=plon)
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
    
    TRMM_3B42_daily = OBJ_NEW('TRMM_nc', FILE=fdir+'3B42_daily.2008.10.01.6.nc')
    
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
    
    trmm_3B43 = OBJ_NEW('TRMM_nc', FILE=fdir+'3B43.000801.6.nc')
    
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
            
    trmm_3B43->QuickPlotPrcp
    ok = DIALOG_MESSAGE('Do you see a plot?', /QUESTION)
    if ok eq 'No' then error += 1
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
    daily_trmm = OBJ_NEW('TRMM_nc', FILE=dailyF)
    agg_trmm = OBJ_NEW('TRMM_nc', FILE=agg_file)    
    
    dpcp = daily_trmm->get_prcp()
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 8 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 21) then error += 1 
    
    testF = fdir + '3B42.081001.12.6A.nc'
    test_o =  OBJ_NEW('TRMM_nc', FILE=testF)
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
    daily_trmm = OBJ_NEW('TRMM_nc', FILE=dailyF, SUBSET_LL=[120.,10.,150.,45.])
    agg_trmm = OBJ_NEW('TRMM_nc', FILE=agg_file)    
    
    dpcp = daily_trmm->get_prcp()
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 8 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 21) then error += 1 
    
    testF = fdir + '3B42.081001.12.6A.nc'
    test_o =  OBJ_NEW('TRMM_nc', FILE=testF, SUBSET_LL=[120.,10.,150.,45.])
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
    
    agg_trmm = OBJ_NEW('TRMM_nc', FILE=agg_file)    
    
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 6 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 03) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 18) then error += 1 
    
    testF = fdir + '3B42.081001.3.6A.nc'
    test_o =  OBJ_NEW('TRMM_nc', FILE=testF)
    if TOTAL(ABS(aggpcp[*,*,0] - test_o->get_prcp()*3)) ne 0 then error += 1
    OBJ_DESTROY, test_o    
    
    testF = fdir + '3B42.081001.18.6A.nc'
    test_o =  OBJ_NEW('TRMM_nc', FILE=testF)
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

    agg_trmm = OBJ_NEW('TRMM_nc', FILE=agg_file)        
    aggpcp = agg_trmm->get_prcp(time, nt)
    if nt ne 7 then error += 1
    if time[0] ne QMS_TIME(year = 2008, month = 10, day = 1, hour =3) then error += 1
    if time[nt-1] ne QMS_TIME(year = 2008, month = 10, day = 1, hour = 21) then error += 1 
    
    testF = fdir + '3B42.081001.12.6A.nc'
    test_o1 =  OBJ_NEW('TRMM_nc', FILE=testF)
    testF = fdir + '3B42.081001.15.6A.nc'
    test_o2 =  OBJ_NEW('TRMM_nc', FILE=testF)
    test_pcp = test_o1->get_prcp()*1.5 + test_o2->get_prcp()*1.5    
    t = QMS_TIME(year = 2008, month = 10, day = 1, hour = 15)
    if TOTAL(ABS(agg_trmm->get_prcp(t0 = t, t1 = t) - test_pcp)) ne 0 then error += 1
    OBJ_DESTROY, test_o1
    OBJ_DESTROY, test_o2
    
    testF = fdir + '3B42.081001.0.6A.nc'
    test_o1 =  OBJ_NEW('TRMM_nc', FILE=testF)
    testF = fdir + '3B42.081001.3.6A.nc'
    test_o2 =  OBJ_NEW('TRMM_nc', FILE=testF)
    test_pcp = test_o1->get_prcp()*1.5 + test_o2->get_prcp()*1.5
    if TOTAL(ABS(aggpcp[*,*,0] - test_pcp)) ne 0 then error += 1
    OBJ_DESTROY, test_o1
    OBJ_DESTROY, test_o2
    
    testF = fdir + '3B42.081001.18.6A.nc'
    test_o1 =  OBJ_NEW('TRMM_nc', FILE=testF)
    testF = fdir + '3B42.081001.21.6A.nc'
    test_o2 =  OBJ_NEW('TRMM_nc', FILE=testF)
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
    
    dom1 = OBJ_NEW('WRF_nc', FILE=fdir+'wrfout_d01_2008-10-26')
    
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
    
    dom1->quickPlotVar, 'T2', t0 = time[3], t1 = time[8]
    ok = DIALOG_MESSAGE('Do you see a temperature plot?', /QUESTION)
    if ok eq 'No' then error += 1
     
     ;----------------------------
     ; CROP BORDER
     ;----------------------------
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
 
     
    ;----------------------------
    ; RESET
    ;----------------------------
    ok = dom1->define_subset()
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
     
    ;----------------------------
    ; TEST TS 
    ;----------------------------
    ok = dom1->define_subset(/CROPBORDER)
    if ~ok then error+=1
    dom1->get_LONLAT, lon, lat, nx, ny
    
    plon = DOM1->get_TS('XLONG', 90.,29.,SRC=dat)
    plat = DOM1->get_TS('XLAT', 90.,29.,SRC=dat)
    
    if abs(pLON[0]-90.) gt MEAN(LON[1:*,0]-LON[0:NX-2,0])/2. then error +=1
    if abs(pLAT[0]-29.) gt MEAN(LAT[0,1:*]-LAT[0,0:Ny-2])/2. then error +=1
    
    dom1->plot_TS, 'T2', 90.1, 31.2, src = dat 
    ok = DIALOG_MESSAGE('Do you see a temperature time serie?', /QUESTION)
    if ok eq 'No' then error += 1
        
    OBJ_DESTROY, dom1     
    if error ne 0 then message, '% TEST_WRF_OUT NOT passed', /CONTINUE else print, 'TEST_WRF_OUT passed'
        
end

pro TEST_WRF_GEO
    
    fdir = TEST_file_directory() + 'WRF/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    dom1 = OBJ_NEW('WRF_nc', FILE=fdir+'geo_em.d03.nc')
    
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

pro TEST_MODIS_SNOW
   
    fdir = TEST_file_directory()
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    dom1 = OBJ_NEW('WRF_nc', FILE=fdir+'WRF/wrfout_d01_2008-10-26', CROPB=25)
    modis = OBJ_NEW('MODIS_Grid', FILE=fdir+'MODIS/MOD10A1.A2008294.h26v06.005.2008299220621.hdf')
    map = dom1->reGrid(FACTOR=10)
    
    
;    MODIS->Get_LonLat, lon, lat, nx, ny
;    if nx ne 2400 then error +=1
;    if ny ne 2400 then error +=1
    
    s = modis->get_Var('Snow_Cover_Daily_Tile')
    ts =map->map_gridded_data(s, modis)
    
    QuickPLot, ts, COLORTABLE=13
    
    OBJ_DESTROY, dom1     
    OBJ_DESTROY, map     
    OBJ_DESTROY, modis     
    if error ne 0 then message, '% TEST_MODIS_SNOW NOT passed', /CONTINUE else print, 'TEST_MODIS_SNOW passed'
    
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
    if ~FILE_TEST(OUTPUT_DIR+'/d1/') then error+=1
    if ~FILE_TEST(OUTPUT_DIR+'/d2/') then error+=1
    if ~FILE_TEST(OUTPUT_DIR+'/wrf_cpy_crop.log') then error+=1
    
    
    ;----------------
    ; D1 day one
    ;----------------        
    origf =  fdir + '/WRF_POST/d1/wrfout_d01_2009-09-12_12_00_00'
    outf =  fdir + '/WRF_CPY_CROP/d1/wrfout_d01_2009-09-13_00_00_00_24h.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    outf =  fdir + '/WRF_CPY_CROP/d1/wrfout_d03_2009-09-13_00_00_00_24h.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    outf =  fdir + '/WRF_CPY_CROP/d2/wrfout_d01_2009-09-14_00_00_00_24h.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    origf =  INPUT_DIR+ '/d1/wrfout_d01_2009-09-13_00_00_00_24h.nc'
    outf =  OUTPUT_DIR + '/dom1/wrf_agg_2009_09_13_d01.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    origf =  INPUT_DIR+ '/d2/wrfout_d01_2009-09-14_00_00_00_24h.nc'
    outf =  OUTPUT_DIR + '/dom1/wrf_agg_2009_09_13_d01.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    origf =  INPUT_DIR+ '/d2/wrfout_d03_2009-09-14_00_00_00_24h.nc'
    outf =  OUTPUT_DIR + '/dom3/wrf_agg_2009_09_13_d03.nc'
    if ~FILE_TEST(origf) then error+=1
    if ~FILE_TEST(outf) then error+=1
    
    orig = OBJ_NEW('WRF_nc', FILE=origf)
    out = OBJ_NEW('WRF_nc', FILE=outf)
    
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
    
    dom1 = OBJ_NEW('WRF_nc', FILE=fdir+'wrfout_d01_2008-10-26')
    dom2 = OBJ_NEW('WRF_nc', FILE=fdir+'wrfout_d02_2008-10-26', CROPBORDER=3)
    
    d2pcp = (dom2->get_prcp())[*,*,36]
    d1pcp = dom2->map_gridded_data((dom1->get_prcp())[*,*,12], dom1)
    
    d2pcp = UTILS_aggregate_Grid_data(d2pcp, 3)
    d1pcp = UTILS_aggregate_Grid_data(d1pcp, 3)
    
    ok = DOM1->define_subset(/CROPCHILD)
    orig = (DOM1->get_prcp())[*,*,12]
    
    if max(abs(d1pcp - d2pcp)) gt 0.2 then error +=1
    if max(abs(d1pcp - orig[1:48,1:48])) gt 0.001 then error +=1
    if max(abs(d2pcp - orig[1:48,1:48])) gt 0.2 then error +=1

    OBJ_DESTROY, dom1     
    OBJ_DESTROY, dom2     
    
    if error ne 0 then message, '% TEST_WRF_AGG_MASSGRID NOT passed', /CONTINUE else print, 'TEST_WRF_AGG_MASSGRID passed'
        
end

pro TEST_NEIREST_NEIGHBOR
  @WAVE.inc
  error = 0
  
  fdir = TEST_file_directory() 
  error = 0 

  dom1 = OBJ_NEW('WRF_nc', FILE=fdir+ '/WRF/wrfout_d01_2008-10-26', CROPBORDER=45)
  dom1->Get_LonLat, wrflon, wrflat, wx, wy
  trmm = OBJ_NEW('TRMM_nc', FILE=fdir+'/TRMM/3B42.081001.3.6A.nc', SUBSET_LL = [min(wrflon)-2,min(wrflat)-2,max(wrflon)+2,max(wrflat)+2])
  trmm->Get_LonLat, trmmlon, trmmlat, tx, ty
  data = trmm->get_prcp()  
  
  syst = QMS_TIME()
  p_c = utils_POS_NEAREST_NEIGHBORHOOD(trmmlon, trmmlat, wrflon, wrflat, /CLASSICAL, DISTANCES = dis_c)
  dc = utils_COMPUTE_NEAREST_NEIGHBORHOOD(p_c, data)
;  print, 'T1 : ' + str_equiv((QMS_TIME()-syst)/S_QMS)
  
  syst = QMS_TIME()
  p_t = utils_POS_NEAREST_NEIGHBORHOOD(trmmlon, trmmlat, wrflon, wrflat, /TRIANGULATION, DISTANCES = dis_t)
  dt = utils_COMPUTE_NEAREST_NEIGHBORHOOD(p_t, data)
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

pro TEST_PLOT_MAP
  
  @WAVE.inc
  
  fdir = TEST_file_directory() 
  error = 0 

  dom1 = OBJ_NEW('WRF_nc', FILE=fdir+ '/WRF/wrfout_d03_2008-10-26')
  map = OBJ_NEW('PLOT_MAP', dom1, Ysize = 600)  
  
;  d = map->set_topography(GRDFILE=fdir+'/MAPPING/TiP.grd')  
  d = map->set_topography(GRDFILE='/home/fab/disk/Data/TOPO/Namco_SRTM/Namco.grd', ROTATE_SLOPE=3)  
  d = map->set_shading_params(RELIEF_FACTOR=2.)
  d = map->set_map_params(INTERVAL=1.)
  
  d = map->set_shape_file(/COUNTRIES, /COLOR)
  GIS_make_proj, ret, utm, PARAM='2, 46, WGS-84'
  d = map->set_shape_file(SHPFILE=fdir+'/MAPPING/namco_shore.shp', SHP_SRC=utm, REMOVE_ENTITITES=53)
  
  CTLOAD, 13
  pcp = (dom1->get_Var('T2'))[*,*,12] - 273.15
  d = map->set_data(pcp, /BILINEAR)    
  d = map->set_Plot_Params(N_LEVELS=12)
  
  ud = (dom1->get_Var('U10'))[*,*,12] ;* 0 + 0.1 
  vd = (dom1->get_Var('V10'))[*,*,12] ;* 0 ; + 1 
  
;  map->show_img
;  map->draw_wind, dom1, ud, vd, 3, LENGTH=length, LEGEND = legend
  
  MAKE_WPLOT, map, TITLE = 'Temperature', BAR_TITLE = 'Celsius', PNG = png, PIXMAP = pixmap
;  MAKE_WPLOT_WIND, map, dom1, ud, vd, 3, TITLE = 'Temperature and wind', BAR_TITLE = 'Celsius', PNG = png, PIXMAP = pixmap
  
  OBJ_DESTROY, dom1  
  OBJ_DESTROY, map  
  
  if error ne 0 then message, '% TEST_PLOT_MAP NOT passed', /CONTINUE else print, 'TEST_PLOT_MAP passed'
  
end

pro TEST_PLOT_MAP_KIN
  
  @WAVE.inc
  
  fdir = TEST_file_directory() 
  error = 0 

  dom1 = OBJ_NEW('WRF_nc', FILE=fdir+ '/WRF/wrfinput_d03', CROPBORDER=5)
  map = OBJ_NEW('PLOT_MAP', dom1, Ysize = 700)  
  
  d = map->set_topography(GRDFILE='/home/fab/disk/Data/TOPO/KINNVI/KiN.grd', rotate = 0)  
  d = map->set_shading_params(RELIEF_FACTOR=5.)  
  d = map->set_map_params(C_INTERVAL=5.)
  
  CTLOAD, 13
  d = map->set_Colors(NCOLORS=127)  
  pcp = (dom1->get_var('TSK'))
    map->getProperty, XSIZE = xsize, YSIZE = ysize   
  pcp = CONGRID(pcp, xsize, ysize, /CENTER, /INTERP)
  d = map->set_img(pcp)  
  d = map->set_shape_file(/COUNTRIES, thick = 2)
  d = map->set_shape_file(SHPFILE='/home/fab/Downloads/SJM_adm/SJM_adm1.shp', COLOR = 'dark red', thick = 2)
  d = map->set_shape_file(SHPFILE='/home/fab/Downloads/world_adm0/world_adm0.shp', COLOR = 'dark green', thick = 2)
  
  map->show_img
  OBJ_DESTROY, dom1  
  OBJ_DESTROY, map  
  
  if error ne 0 then message, '% TEST_PLOT_MAP NOT passed', /CONTINUE else print, 'TEST_PLOT_MAP passed'
  
end

pro TEST_PLOT_CASA
  
  @WAVE.inc
  
  fdir = TEST_file_directory() 
  error = 0 

  dom1 = OBJ_NEW('WRF_nc', FILE=fdir+ '/WRF/geo_em.d03.nc', CROPBORDER=5)
  map = OBJ_NEW('PLOT_MAP', dom1, Ysize = 700)  
  
;  d = map->set_topography(GRDFILE=fdir+'/MAPPING/KiN.grd')  
  d = map->set_shading_params(RELIEF_FACTOR=2.)
  d = map->set_map_params(C_INTERVAL=1)
  
  CTLOAD, 13
  d = map->set_Colors(NCOLORS=256, /INVERTCOLORS)
  
  pcp = dom1->get_Var('LU_INDEX')
  map->getProperty, XSIZE = xsize, YSIZE = ysize   
  pcp = CONGRID(pcp, xsize, ysize, /CENTER)
  d = map->set_img(pcp)  
  d = map->set_shape_file(/COUNTRIES)
    
  map->show_img
  OBJ_DESTROY, dom1  
  OBJ_DESTROY, map  
  
  if error ne 0 then message, '% TEST_PLOT_MAP NOT passed', /CONTINUE else print, 'TEST_PLOT_MAP passed'
  
end

pro TEST_PLOT_MAP_TRMM
  
  @WAVE.inc
  
  fdir = TEST_file_directory() 
  error = 0 

  dom1 = OBJ_NEW('TRMM_nc', FILE=fdir+'TRMM/3B43.000801.6.nc')
  map = OBJ_NEW('PLOT_MAP', dom1, Xsize = 1200)  

  d = map->set_shading_params(RELIEF_FACTOR=2.)
  
  CTLOAD, 1
  d = map->set_Colors(NCOLORS=256, /INVERTCOLORS)
  map->getProperty, XSIZE = xsize, YSIZE = ysize   
  
  pcp = (dom1->get_prcp())
  pcp = CONGRID(pcp, xsize, ysize, /CENTER, /INTERP)
  d = map->set_img(pcp)  
  d = map->set_shape_file(/COUNTRIES)
  
    
  map->show_img
  OBJ_DESTROY, dom1  
  OBJ_DESTROY, map  
  
  if error ne 0 then message, '% TEST_PLOT_MAP NOT passed', /CONTINUE else print, 'TEST_PLOT_MAP passed'
  
end


pro TEST_DATASETS
  TEST_TRMM_3B42
  TEST_TRMM_3B42_daily
  TEST_TRMM_3B43  
  TEST_TRMM_AGG
  TEST_WRF_OUT
end

pro TEST_UTILS
  TEST_WRF_AGG_MASSGRID
  TEST_NEIREST_NEIGHBOR
end

pro TEST_POST
  TEST_POST_COPY_CROP
  TEST_POST_AGG
  TEST_POST_AGG_CROPPED
end

pro TEST_everything
  TEST_TIME
  TEST_DATASETS
  TEST_UTILS
  TEST_POST
end

