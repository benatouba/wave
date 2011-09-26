;+
; :Description:
;    This routine prepares the X axis for a time serie plot. 
;
; :Params: 
;
; :Keywords:
;    HOURS: in, optional, type = long
;           the ticks will be put each X hours and have the format "01Jan.00h"
;    DAYS: in, optional, type = long
;           the ticks will be put each X days and have the format "01Jan"
;    MONTHS: in, optional, type = long
;            the ticks will be put each X months and have the format "Jan"
;    YEARS: in, optional, type = long
;           the ticks will be put each X months and have the format "2011"
;    MONTHYEARS: in, optional, type = long
;                the ticks will be put each X months and have the format "Jan" and a second axis for years
;    TIME: in, optional, type = {ABS_DATE}/qms
;          this keywords is ignored if any of the other keywords is set. Set it to the time of 
;          the time serie to define automatically the time axis.
;          
; :Returns: 
;    xtickformat: out
;                 set as input to the following plot routine
;    xtickunits: out
;                set as input to the following plot routine
;    xtickinterval: out
;                   set as input to the following plot routine
;
; :History:
;     Written by FaM, 2011.
;
;
;-
pro w_date_tickformat, xtickformat, xtickunits, xtickinterval, HOURS = hours, DAYS = days, MONTHS = months, YEARS = years, MONTHYEARS = monthyears, TIME = time

  ;Check args
  if N_ELEMENTS(hours) ne 0 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
    xtickformat = ['LABEL_DATE']
    xtickunits = 'Hours' 
    xtickinterval = hours
  endif else if N_ELEMENTS(days) ne 0 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
    xtickformat = ['LABEL_DATE']
    xtickunits = 'Days'
    xtickinterval = days
  endif else if N_ELEMENTS(months) ne 0 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M'])
    xtickformat = ['LABEL_DATE']
    xtickunits = 'Months'
    xtickinterval = months
  endif else if N_ELEMENTS(years) ne 0 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%Y'])
    xtickformat = ['LABEL_DATE']
    xtickunits = 'Years'
    xtickinterval = years
  endif else if N_ELEMENTS(MONTHYEARS) ne 0 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M','%Y'])
    xtickformat = ['LABEL_DATE','LABEL_DATE']
    xtickunits = ['Months','Year']
    xtickinterval = monthyears
  endif else begin 
  
    ;Check args
    if ~ check_WTIME(time, OUT_QMS=tqms) then message, WAVE_Std_Message('time', /ARG)
    s = MAKE_TIME_STEP(DMS=MAX(tqms)-MIN(tqms))
   
    if s.day gt 1800 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'years'
      xtickinterval = 2
    endif else if s.day gt 1200 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'years'
      xtickinterval = 1
    endif else if s.day gt 900 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Months'
      xtickinterval = 6
    endif else if s.day gt 600 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Months'
      xtickinterval = 4
    endif else if s.day gt 300 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Months'
      xtickinterval = 3
    endif else if s.day gt 150 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%M'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Months'
      xtickinterval = 1
    endif else if s.day gt 90 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Days'
      xtickinterval = 30
    endif else if s.day gt 60 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Days'
      xtickinterval = 15
    endif else if s.day gt 45 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Days'
      xtickinterval = 15
    endif else if s.day gt 30 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Days'
      xtickinterval = 7
    endif else if s.day gt 12 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Days'
      xtickinterval = 3
    endif else if s.day ge 2 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) 
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Days'
      xtickinterval = 1
    endif else if s.hour gt 6 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Hours'
      xtickinterval = 6
    endif else if s.hour gt 1 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Hours'
      xtickinterval = 1
    endif else begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
      xtickformat = ['LABEL_DATE']
      xtickunits = 'Hours'
      xtickinterval = 6
    endelse
  endelse
  
end