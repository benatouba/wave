;+
; :Description:
;    This is a simple routine based on the Coyote cg* commands to create a simple
;    plot with a time axis. Inputs are the serie to plot and time in absolute date.
;    One can plot up to 8 time series and add a new axis, legend, etc. Therefore, 
;    the number of parameters is very high but also very repetitive (see examples).
;
; :Params:
; 
;    data: in, required, type = numeric
;          the data to plot
;    time: in, required, type = qms/{ABS_DATE}
;          the time (same size as data)
;    tag: in, required, type = string
;          the tag related to data for the legend
;          
;    data2: in, optional, type = numeric
;          the data to overplot
;    time2: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data2)
;    color2: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag2: in, optional, type = string
;          the tag related to data2 for the legend
;
;    data3: in, optional, type = numeric
;          the data to overplot
;    time3: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data3)
;    color3: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag3: in, optional, type = string
;          the tag related to data3 for the legend
;
;    data4: in, optional, type = numeric
;          the data to overplot
;    time4: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data4)
;    color4: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag4: in, optional, type = string
;          the tag related to data4 for the legend
;
;    data5: in, optional, type = numeric
;          the data to overplot
;    time5: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data5)
;    color5: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag5: in, optional, type = string
;          the tag related to data5 for the legend
;
;    data6: in, optional, type = numeric
;          the data to overplot
;    time6: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data6)
;    color6: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag6: in, optional, type = string
;          the tag related to data6 for the legend
;
;    data7: in, optional, type = numeric
;          the data to overplot
;    time7: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data7)
;    color7: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag7: in, optional, type = string
;          the tag related to data7 for the legend
;
;    data8: in, optional, type = numeric
;          the data to overplot
;    time8: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data8)
;    color8: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag8: in, optional, type = string
;          the tag related to data8 for the legend
;
; :Keywords:
; 
;    color1: in, optional, type = string, default = 'black'
;           the color for the first plot (string accepted by cgColor)
;    style1: in, optional, type = numeric, default = 0
;            the style for the first plot
;    psym1: in, optional, type = numeric, default = 0
;            the psym for the first plot
;    COMENT1: in, optional, type = string, default = ''
;             a subtitle for the legend
;             
;    TITLE: in, optional, type = string, default = 'Data'
;             a title for the plot
;    XTITLE: in, optional, type = string, default = ''
;             a title for the X axis
;    YTITLE: in, optional, type = string, default = ''
;             a title for the Y axis 
;    MONTHS: in, optional, type = numeric
;            interval in months for the time axis ticks
;    DAYS: in, optional, type = numeric
;            interval in days for the time axis ticks
;    YEARS: in, optional, type = numeric
;            interval in years for the time axis ticks
;    MONYEAR: in, optional, type = numeric
;            interval in months and year for the time axis ticks
;    HOURS: in, optional, type = numeric
;            interval in hours for the time axis ticks
;    ZOOM: in, optional, type = qms/{ABS_DATA}
;            a two element array [date, date] wher to zoom in the time serie
;    RANGE: in, optional, type = numeric
;            a two element array for the range for the Y axis. Default: [min(data), max(data)]
;    NEWAXIS: in, optional, type = numeric
;             if a second axis has to be drawn, NEWAXIS = x means that the new axis is drawn 
;             starting at dataX plot.
;    NEWRANGE: in, optional, type = numeric
;              a two element array for the range for the new Y axis. Default: [min(dataX), max(dataX)]
;    NEWTITLE: in, optional, type = string, default = ''
;              the title for the new Y axis
;    THICKNESS: in, optional, type = numeric
;               the line thickness
;    HORILINE: in, optional, type = numeric
;               if one wants to draw a horizontal dashed line in the plot device (e.g HORILINE = 0)
;    VERTILINE: in, optional, type = qms/{abs_date}
;               if one wants to draw a vertical dashed line in the plot device (e.g VERTILINE = MAKE_ABS_DATE(year = 2008))
;
;    COMENT2: in, optional, type = string, default = ''
;             a subtitle for the legend of data2
;    style2: in, optional, type = numeric, default = 0
;            the style for the plot 2
;    psym2: in, optional, type = numeric, default = 0
;            the psym for the plot 2
;
;    COMENT3: in, optional, type = string, default = ''
;             a subtitle for the legend of data3
;    style3: in, optional, type = numeric, default = 0
;            the style for the plot 3
;    psym3: in, optional, type = numeric, default = 0
;            the psym for the plot 3
;
;    COMENT4: in, optional, type = string, default = ''
;             a subtitle for the legend of data4
;    style4: in, optional, type = numeric, default = 0
;            the style for the plot 4
;    psym4: in, optional, type = numeric, default = 0
;            the psym for the plot 4
;
;    COMENT5: in, optional, type = string, default = ''
;             a subtitle for the legend of data5
;    style5: in, optional, type = numeric, default = 0
;            the style for the plot 5
;    psym5: in, optional, type = numeric, default = 0
;            the psym for the plot 5
;
;    COMENT6: in, optional, type = string, default = ''
;             a subtitle for the legend of data6
;    style6: in, optional, type = numeric, default = 0
;            the style for the plot 6
;    psym6: in, optional, type = numeric, default = 0
;            the psym for the plot 6
;
;    COMENT7: in, optional, type = string, default = ''
;             a subtitle for the legend of data7
;    style7: in, optional, type = numeric, default = 0
;            the style for the plot 7
;    psym7: in, optional, type = numeric, default = 0
;            the psym for the plot 7
;
;    COMENT8: in, optional, type = string, default = ''
;             a subtitle for the legend of data8
;    style8: in, optional, type = numeric, default = 0
;            the style for the plot 8
;    psym8: in, optional, type = numeric, default = 0
;            the psym for the plot 8
;
; :Author: FaM
;
; :History:
;     Written by FaM, 2011.
;
;-
pro w_TimeLinePlot, data,$  ; array to plot
                    time, $   ; time (same size as data)
                    tag,  $   ; legend (str)
                    color1 = color1,$ ; All FSC colors strings accepted. default: black
                    style1 = style1,$ ; linestyle. default: line
                    psym1 = psym1, $
                    COMENT1 = coment1,$ ; comment below the legend
                    TITLE = title, $ ; graph title
                    XTITLE = xtitle,$ ; x axis title (not necessary)
                    MONTHS = months,$ ; X tick lenght in months
                    DAYS = days, $; X tick lenght in days 
                    YEARS = years,$ ; X tick lenght in years 
                    MONYEAR = monyear, $ ; X tick lenght in month + years 
                    HOURS = hours,$ ; X tick lenght in hours 
                    ZOOM=zoom, $ ; format [{abs_date}1,{abs_date}2]
                    YTITLE = Ytitle,$ ; title of the y axis
                    RANGE = range, $ ; Y data range
                    NEWAXIS = newaxis, NEWRANGE = newrange, NEWTITLE = newtitle, $ ; if a second axis is to be drawn
                    THICKNESS = thickness, PIXMAP = pixmap, $ ; line thickness
                    HORILINE = HORILINE, VERTILINE = VERTILINE, $ ; if horizontal or vertical lines have to be drawn
                    data2, time2, color2, tag2, COMENT2 = coment2, style2 = style2, psym2 = psym2, $ 
                    data3, time3, color3, tag3, COMENT3 = coment3, style3 = style3, psym3 = psym3, $ 
                    data4, time4, color4, tag4, COMENT4 = coment4, style4 = style4, psym4 = psym4, $ 
                    data5, time5, color5, tag5, COMENT5 = coment5, style5 = style5, psym5 = psym5, $
                    data6, time6, color6, tag6, COMENT6 = coment6, style6 = style6, psym6 = psym6, $
                    data7, time7, color7, tag7, COMENT7 = coment7, style7 = style7, psym7 = psym7, $
                    data8, time8, color8, tag8, COMENT8 = coment8, style8 = style8, psym8 = psym8
                     
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc            
         
   ; prepare the plot  
   device, DECOMPOSED=0, RETAIN=2  
   pp = !ORDER ;To restore later
   !ORDER = 0
   
   ;Check args
   if ~ check_WTIME(time, OUT_QMS=tqms) then message, WAVE_Std_Message('time', /ARG)
   nt = N_ELEMENTS(tqms)
   if N_ELEMENTS(data) ne nt then message, WAVE_Std_Message('data', /ARG)
   
   if ~KEYWORD_SET(title) then title = 'Data'
   if ~KEYWORD_SET(color1) then color1 = 'black'
   if ~KEYWORD_SET(style1) then style = 0 else style = style1
   if ~KEYWORD_SET(psym1) then psym = 0 else psym = psym1
   if ~KEYWORD_SET(thickness) then thickness = 1.8
   if ~KEYWORD_SET(newaxis) then newaxis = 0
   if newaxis ne 0 then YSTYLE = 1 + 8 else YSTYLE = 1
   
   if N_ELEMENTS(tag) eq 0 then tag = 'Data'

   IF KEYWORD_SET(zoom) then begin
     if ~ check_WTIME(zoom, OUT_QMS=tzoom) then message, WAVE_Std_Message('zoom', /ARG)
     if N_ELEMENTS(zoom) ne 2 then message, WAVE_Std_Message('zoom', NELEMENTS=2)
     p1 = VALUE_LOCATE(tqms,tzoom[0])
     p2 = VALUE_LOCATE(tqms,tzoom[1])
   endif else begin
     p1 = 0
     p2 = nt - 1
   endelse
  
  if ~KEYWORD_SET(range) then range = [MIN(data[p1:p2]), MAX(data[p1:p2])]
  
  ; Make X AXIS
  if KEYWORD_SET(hours) then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Hours'
    xtinter = hours 
  ENDIF else if KEYWORD_SET(monYEAR) then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M','%Y'])
    XTICKFORMAT = ['LABEL_DATE','LABEL_DATE']
    xtunits = ['Months','Year']
    xtinter = monYEAR 
  ENDIF else if KEYWORD_SET(months) then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Months'
    xtinter = months 
  ENDIF else if KEYWORD_SET(years) then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%Y'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Years'
    xtinter = years 
  ENDIF else if KEYWORD_SET(Days) then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis 
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Days'
    xtinter = days 
  endif else begin ;TODO: meliorate the automatic time AXIS definition
    s = MAKE_TIME_STEP(DMS=tqms[p2] - tqms[p1])
    if s.day gt 900 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Months'
    xtinter = 6
    endif else if s.day gt 600 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Months'
    xtinter = 4
    endif else if s.day gt 300 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M%Y'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Months'
    xtinter = 3
    endif else if s.day gt 150 then begin
    dummy = LABEL_DATE(DATE_FORMAT=['%M'])
    XTICKFORMAT = ['LABEL_DATE']
    xtunits = 'Months'
    xtinter = 1
    endif else if s.day gt 90 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
      XTICKFORMAT = ['LABEL_DATE']
      xtunits = 'Days'
      xtinter = 30
    endif else if s.day gt 60 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
      XTICKFORMAT = ['LABEL_DATE']
      xtunits = 'Days'
      xtinter = 15
    endif else if s.day gt 45 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
      XTICKFORMAT = ['LABEL_DATE']
      xtunits = 'Days'
      xtinter = 15
    endif else if s.day gt 30 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
      XTICKFORMAT = ['LABEL_DATE']
      xtunits = 'Days'
      xtinter = 7
    endif else if s.day gt 12 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
      XTICKFORMAT = ['LABEL_DATE']
      xtunits = 'Days'
      xtinter = 3
    endif else if s.day gt 1 then begin
      dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
      XTICKFORMAT = ['LABEL_DATE']
      xtunits = 'Days'
      xtinter = 1
    endif else if s.hour gt 6 then begin
     dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
     XTICKFORMAT = ['LABEL_DATE']
     xtunits = 'Hours'
     xtinter = 12
    endif else begin
     dummy = LABEL_DATE(DATE_FORMAT=['%D%M.%Hh'])
     XTICKFORMAT = ['LABEL_DATE']
     xtunits = 'Hours'
     xtinter = 6
    endelse
  endelse 

  ; Legend    
  x = [0.74, 0.785]
  y = [0.85, 0.85]  
  dx1 = 0.012
  dx2 = 0.018
  dy1 = 0.008
  dy2 = 0.04
  tsiz = 2.
  tthi = 1.
  csiz = 1.3
  cthi = 1.
  
  plo_siz = 1.8
  plo_thi = 1.2

  jd = TIME_to_JD(time[p1:p2])  

  cgWindow, 'cgPlot', jd, data[p1:p2], title = title,  CHARSIZE=plo_siz, /NORMAL, $
    CHARTHICK = plo_thi, XTITLE = xtitle, Ytitle = Ytitle, YRANGe = range,  POSITION = [0.1,0.09,0.65,0.92], XTICK_GET=xs, YTICK_GET=ys, $
     /NODATA, XTICKFORMAT= XTICKFORMAT, XTICKUNITS=xtunits, XTICKINTERVAL = [xtinter], YSTYLE = YSTYLE, xstyle = 1, PSYM=psym, $
       WXSize = 1200, WYSize = 600, WTITLE = 'w_TimeLinePlot resizable window'
;  cgControl, Execute=0   
        
  if N_ELEMENTS(HORILINE) eq 1 then cgPlots, [min(jd),max(jd)], [HORILINE,HORILINE], color = cgColor('Black'), LINESTYLE=5, /WINDOW
  if N_ELEMENTS(VERTILINE) eq 1 then $
    for i =0, N_ELEMENTS(VERTILINE)-1 do cgPlots, [TIME_to_JD(VERTILINE[i]),TIME_to_JD(VERTILINE[i])], $
           range, color = cgColor('black'), LINESTYLE=5, /WINDOW
  ; real plot
  if ~KEYWORD_SET(psym1) then begin
    cgplot, jd, data[p1:p2], COLOR = cgColor(color1), THI = thickness, LINESTYLE=style, PSYM=psym, /OVERPLOT, /WINDOW
  endif else begin
    cgPlot, jd, data[p1:p2], COLOR = cgColor(color1), PSYM=psym, SYMSIZE=thickness, /OVERPLOT, /WINDOW
  endelse

  if ~KEYWORD_SET(psym1) then begin
    cgplots, x, y,  COLOR = cgColor(color1), /NORMAL , THICK=thickness, LINESTYLE=style, /WINDOW
  endif else begin
    cgplots, x, y,  COLOR = cgColor(color1), /NORMAL , THICK=thickness, PSYM=psym, SYMSIZE=thickness, /WINDOW
  endelse

  cgtext, x[1]+ dx1 ,  y[0]-dy1, tag, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color1), /NORMAL , /WINDOW
  if KEYWORD_SET(coment1) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment1, CHARSIZE=csiz, CHARTHICK = cthi, COLOR = cgColor(color1), /NORMAL, /WINDOW
  
  news = 2
  
  ; OVERPLOT
  if N_ELEMENTS(data2) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 2 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data2), MAX(data2)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style2) eq 1 then style = style2 
    y = y - 0.1
    if ~KEYWORD_SET(psym2) then begin
      cgplot, TIME_to_JD(time2), data2, COLOR = cgColor(color2), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color2), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time2)], data2, COLOR = cgColor(color2), PSYM=psym2, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM2 eq 10 then psym2 = 0
      cgplots, x, y,  COLOR = cgColor(color2), THICK=thickness, /NORMAL, PSYM=psym2, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag2, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color2), /NORMAL, /WINDOW
    if KEYWORD_SET(coment2) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment2, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color2), /NORMAL, /WINDOW
  endif  
  
  if N_ELEMENTS(data3) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 3 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data3), MAX(data3)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style3) eq 1  then style = style3 
    y = y - 0.1
    if ~KEYWORD_SET(psym3) then begin
      cgplot, TIME_to_JD(time3), data3, COLOR = cgColor(color3), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color3), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time3)], data3, COLOR = cgColor(color3), PSYM=psym3, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM3 eq 10 then psym3 = 0
      cgplots, x, y,  COLOR = cgColor(color3), THICK=thickness, /NORMAL, PSYM=psym3, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag3, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color3), /NORMAL, /WINDOW
    if KEYWORD_SET(coment3) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment3, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color3), /NORMAL, /WINDOW
  endif  
  
  if N_ELEMENTS(data4) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 4 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data4), MAX(data4)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style4) eq 1  then style = style4 
    y = y - 0.1
    if ~KEYWORD_SET(psym4) then begin
      cgplot, TIME_to_JD(time4), data4, COLOR = cgColor(color4), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color4), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time4)], data4, COLOR = cgColor(color4), PSYM=psym4, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM4 eq 10 then psym4 = 0
      cgplots, x, y,  COLOR = cgColor(color4), THICK=thickness, /NORMAL, PSYM=psym4, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag4, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color4), /NORMAL, /WINDOW
    if KEYWORD_SET(coment4) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment4, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color4), /NORMAL, /WINDOW
  endif  
  
  if N_ELEMENTS(data5) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 5 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data5), MAX(data5)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style5) eq 1  then style = style5 
    y = y - 0.1
    if ~KEYWORD_SET(psym5) then begin
      cgplot, TIME_to_JD(time5), data5, COLOR = cgColor(color5), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color5), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time5)], data5, COLOR = cgColor(color5), PSYM=psym5, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM5 eq 10 then psym5 = 0
      cgplots, x, y,  COLOR = cgColor(color5), THICK=thickness, /NORMAL, PSYM=psym5, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag5, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color5), /NORMAL, /WINDOW
    if KEYWORD_SET(coment5) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment5, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color5), /NORMAL, /WINDOW
  endif  
  
  if N_ELEMENTS(data6) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 6 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data6), MAX(data6)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style6) eq 1  then style = style6 
    y = y - 0.1
    if ~KEYWORD_SET(psym6) then begin
      cgplot, TIME_to_JD(time6), data6, COLOR = cgColor(color6), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color6), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time6)], data6, COLOR = cgColor(color6), PSYM=psym6, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM6 eq 10 then psym6 = 0
      cgplots, x, y,  COLOR = cgColor(color6), THICK=thickness, /NORMAL, PSYM=psym6, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag6, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color6), /NORMAL, /WINDOW
    if KEYWORD_SET(coment6) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment6, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color6), /NORMAL, /WINDOW
  endif  
  
  if N_ELEMENTS(data7) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 7 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data7), MAX(data7)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style7) eq 1  then style = style7 
    y = y - 0.1
    if ~KEYWORD_SET(psym7) then begin
      cgplot, TIME_to_JD(time7), data7, COLOR = cgColor(color7), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color7), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time7)], data7, COLOR = cgColor(color7), PSYM=psym7, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM7 eq 10 then psym7 = 0
      cgplots, x, y,  COLOR = cgColor(color7), THICK=thickness, /NORMAL, PSYM=psym7, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag7, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color7), /NORMAL, /WINDOW
    if KEYWORD_SET(coment7) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment7, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color7), /NORMAL, /WINDOW
  endif  
  
  if N_ELEMENTS(data8) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 8 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data8), MAX(data8)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, /WINDOW
      style = news
    endif
    if N_ELEMENTS(style8) eq 1  then style = style8 
    y = y - 0.1
    if ~KEYWORD_SET(psym8) then begin
      cgplot, TIME_to_JD(time8), data8, COLOR = cgColor(color8), THI =  thickness, LINESTYLE=style, /OVERPLOT, /WINDOW
      cgplots, x, y,  COLOR = cgColor(color8), THICK=thickness, /NORMAL, LINESTYLE=style, /WINDOW
    endif else begin
      cgplot, [TIME_to_JD(time8)], data8, COLOR = cgColor(color8), PSYM=psym8, SYMSIZE=thickness, /WINDOW, /OVERPLOT
      if PSYM8 eq 10 then psym8 = 0
      cgplots, x, y,  COLOR = cgColor(color8), THICK=thickness, /NORMAL, PSYM=psym8, SYMSIZE=thickness, /WINDOW
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag8, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color8), /NORMAL, /WINDOW
    if KEYWORD_SET(coment8) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment8, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color8), /NORMAL, /WINDOW
  endif  
;  cgControl, Execute=1
  
  !ORDER = pp
  
end