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
  if error ne 0 then message, 'TEST_MAKE_ABS_DATE NOT passed', /CONTINUE else print, 'TEST_MAKE_ABS_DATE passed'
  
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
  
  if error ne 0 then message, 'TEST_JULIAN_DAYS NOT passed', /CONTINUE else print, 'TEST_JULIAN_DAYS passed'
      
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
  if error ne 0 then message, 'TEST_QMS_TIME NOT passed', /CONTINUE else print, 'TEST_QMS_TIME passed'
  
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
  
  if error ne 0 then message, 'TEST_MAKE_REL_DATE NOT passed', /CONTINUE else print, 'TEST_MAKE_REL_DATE passed'
  
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
  
  if error ne 0 then message, 'TEST_MAKE_TIME_STEP NOT passed', /CONTINUE else print, 'TEST_MAKE_TIME_STEP passed'
  
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
  
  if error ne 0 then message, 'TEST_MAKE_TIME_SERIE NOT passed', /CONTINUE else print, 'TEST_MAKE_TIME_SERIE passed'
  
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
  
  if error ne 0 then message, 'TEST_MAKE_ENDED_TIME_SERIE NOT passed', /CONTINUE else print, 'TEST_MAKE_ENDED_TIME_SERIE passed'
  
  startTime = make_abs_date(YEAR=2008, MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  endTime = make_abs_date(YEAR=2004,  MONTH=01, DAY=01, HOUR=00, MINUTE=00, SECOND=00)
  
  totest = MAKE_ENDED_TIME_SERIE(startTime, endTime, nsteps =nsteps, year = -1)
  if nsteps ne 5 then error+=1
  if n_elements(totest) ne nsteps then error+=1
  if TIME_to_STR(toTest[1]) ne '01.01.2007 00:00:00' then error+=1
  if TIME_to_STR(toTest[2]) ne '01.01.2006 00:00:00' then error+=1
  if TIME_to_STR(toTest[4]) ne '01.01.2004 00:00:00' then error+=1
  
  if error ne 0 then message, 'TEST_MAKE_ENDED_TIME_SERIE NOT passed', /CONTINUE else print, 'TEST_MAKE_ENDED_TIME_SERIE passed'
  
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
  
  if error ne 0 then message, 'TEST_check_TS NOT passed', /CONTINUE else print, 'TEST_check_TS passed'
  
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
    
    trmm_3B42->Get_LonLat, gislon, gislat, nx, ny
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
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
    
    trmm_3B42->Get_LonLat, gislon, gislat, gisnx, gisny
    if gisnx ne nx then  error += 1
    if gisny ne ny then  error += 1    
    if total(abs(gislon-lon)) gt 1e-6 then  error += 1
    if total(abs(gislat-lat)) gt 1e-6 then  error += 1
    
    varPcp = trmm_3B42->get_prcp(vtime, vnt)
    varOrigCrop = varOrig[mysubs[0]:mysubs[0]+mysubs[1]-1,mysubs[2]:mysubs[2]+mysubs[3]-1]
    if total(abs(varOrigCrop-varPcp)) ne 0 then  error += 1
    
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
    if error ne 0 then message, 'TEST_TRMM_3B42 NOT passed', /CONTINUE else print, 'TEST_TRMM_3B42 passed'
  
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
    if error ne 0 then message, 'TEST_TRMM_3B42_daily NOT passed', /CONTINUE else print, 'TEST_TRMM_3B42_daily passed'
  
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
    if error ne 0 then message, 'TEST_TRMM_3B43 NOT passed', /CONTINUE else print, 'TEST_TRMM_3B43 passed'
  
end

pro TEST_TRMM_AGG
    
    fdir = TEST_file_directory() + 'TRMM/'
    error = 0 
    
    ;-------------------------
    ; Test 3Hourly product
    ;-------------------------
    
    fname = fdir + '3B42.081001.0.6A.nc'
;    utils_TRMM_aggregate, fname, /NOSHIFT
    
    if error ne 0 then message, 'TEST_TRMM_AGG NOT passed', /CONTINUE else print, 'TEST_TRMM_AGG passed'

end

pro TEST_DATASETS
  TEST_TRMM_3B42
  TEST_TRMM_3B42_daily
  TEST_TRMM_3B43
  
end

pro TEST_everything
  TEST_TIME
  TEST_DATASETS
end

