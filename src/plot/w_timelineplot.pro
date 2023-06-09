;+
; :Description:
;    This is a simple routine based on the Coyote cg* commands to create a simple
;    plot with a time axis. Inputs are the serie to plot and time in absolute date.
;    One can plot up to 9 time series and add a new axis, legend, etc. Therefore, 
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
;    data9: in, optional, type = numeric
;          the data to overplot
;    time9: in, optional, type = qms/{ABS_DATE}
;          the time (same size as data9)
;    color9: in, optional, type = string
;          the color for the overplot (string accepted by cgColor)
;    tag9: in, optional, type = string
;          the tag related to data9 for the legend
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
;             a subtitle for the legend;             
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
;    FORCE_XAXIS: in, optional, type = boolean
;                 if you want to force the exact X axis range 
;    FORCE_YAXIS: in, optional, type = boolean
;                 if you want to force the exact Y axis range 
;    NEWAXIS: in, optional, type = numeric
;             if a second axis has to be drawn, NEWAXIS = x means that the new axis is drawn 
;             starting at dataX plot.
;    NEWRANGE: in, optional, type = numeric
;              a two element array for the range for the new Y axis. Default: [min(dataX), max(dataX)]
;    NEWTITLE: in, optional, type = string, default = ''
;              the title for the new Y axis
;    THICKNESS: in, optional, type = numeric
;               the line thickness (default: 2.3)
;    HORILINE: in, optional, type = numeric
;               if one wants to draw a horizontal dashed line in the plot device (e.g HORILINE = 0)
;    VERTILINE: in, optional, type = qms/{abs_date}
;               if one wants to draw a vertical dashed line in the plot device (e.g VERTILINE = MAKE_ABS_DATE(year = 2008))
;               
;    PNG: in, optional, type = string
;         set to a filename to generate a png output (uses image magick)
;    EPS: in, optional, type = string
;         set to a filename to generate an encapsulated postscript output
;    IM_RESIZE: in, optional, type=integer, default=25
;                Set this keyword to percentage that the raster image file created my ImageMagick
;                 from PostScript output should be resized.
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
;-
pro w_TimeLinePlot, data,$  ; array to plot
                    time, $   ; time (same size as data)
                    tag,  $   ; legend (str)
                    color1 = color1,$ ; All cg colors strings accepted. default: black
                    style1 = style1,$ ; linestyle. default: line
                    psym1 = psym1, $
                    COMENT1 = coment1,$ ; comment below the legend
                    TITLE = title, $ ; graph title
                    XTITLE = xtitle,$ ; x axis title (not necessary)
                    MONTHS = months,$ ; X tick lenght in months
                    DAYS = days, $; X tick lenght in days 
                    YEARS = years,$ ; X tick lenght in years 
                    MONTHYEARS = monthyears, $ ; X tick lenght in month + years 
                    HOURS = hours,$ ; X tick lenght in hours 
                    ZOOM=zoom, $ ; format [{abs_date}1,{abs_date}2]
                    YTITLE = Ytitle,$ ; title of the y axis
                    RANGE = range, $ ; Y data range
                    FORCE_XAXIS = force_xAxis, $ ; force axis range
                    FORCE_YAXIS = force_yAxis, $ ; force axis range
                    NEWAXIS = newaxis, NEWRANGE = newrange, NEWTITLE = newtitle, $ ; if a second axis is to be drawn
                    THICKNESS = thickness, $ ; line thickness
                    HORILINE = HORILINE, VERTILINE = VERTILINE, $ ; if horizontal or vertical lines have to be drawn
                    EPS = eps, PNG = png, PIXMAP = pixmap, IM_RESIZE = im_resize,  $ ; outputs
                    NO_RESIZE = no_resize, $ ; If resizable
                    data2, time2, color2, tag2, COMENT2 = coment2, style2 = style2, psym2 = psym2, $ 
                    data3, time3, color3, tag3, COMENT3 = coment3, style3 = style3, psym3 = psym3, $ 
                    data4, time4, color4, tag4, COMENT4 = coment4, style4 = style4, psym4 = psym4, $ 
                    data5, time5, color5, tag5, COMENT5 = coment5, style5 = style5, psym5 = psym5, $
                    data6, time6, color6, tag6, COMENT6 = coment6, style6 = style6, psym6 = psym6, $
                    data7, time7, color7, tag7, COMENT7 = coment7, style7 = style7, psym7 = psym7, $
                    data8, time8, color8, tag8, COMENT8 = coment8, style8 = style8, psym8 = psym8, $
                    data9, time9, color9, tag9, COMENT9 = coment9, style9 = style9, psym9 = psym9
                     
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc            
  ON_ERROR, 2

   
  ; prepare the plot 
  device, DECOMPOSED=1, RETAIN=2
  pp = !ORDER ;To restore later
  !ORDER = 0  
  xsiz = 1000
  ysiz = 600
  
  ; Check what we want to do  
  visible = ~KEYWORD_SET(PIXMAP)   
  cgDisplay, /FREE, XSIZE=xsiz, YSIZE=ysiz, /PIXMAP, Title='w_ScatterPlot'
  xwin = !D.WINDOW  
  cgWIN = FALSE    
  if visible then begin
    WDELETE, xwin
    cgWindow, WXSIZE=xsiz, WYSIZE=ysiz, Title='w_ScatterPlot resizable window'
    cgControl, EXECUTE=0
    cgWIN = true
  endif else begin
    if KEYWORD_SET(EPS) and KEYWORD_SET(PNG) then Message, 'In pixmap mode you have to choose between EPS and PNG'
    if KEYWORD_SET(EPS) then cgPS_Open, FILENAME= eps, Decomposed=1, /Encapsulated, /Metric
    if KEYWORD_SET(PNG) then cgPS_Open, FILENAME= png, Decomposed=1
  endelse
   
   
   ;Check args
   if ~ check_WTIME(time, OUT_QMS=tqms) then message, WAVE_Std_Message('time', /ARG)
   nt = N_ELEMENTS(tqms)
   if N_ELEMENTS(data) ne nt then message, WAVE_Std_Message('data', /ARG)
   
   if ~KEYWORD_SET(title) then title = 'Data'
   if ~KEYWORD_SET(color1) then color1 = 'black'
   if ~KEYWORD_SET(style1) then style = 0 else style = style1
   if ~KEYWORD_SET(psym1) then psym = 0 else psym = psym1
   if ~KEYWORD_SET(thickness) then thickness = 2
   if ~KEYWORD_SET(newaxis) then newaxis = 0
   if KEYWORD_SET(force_xAxis) then xstyle = 1 else xstyle = 0
   if KEYWORD_SET(force_yAxis) then yforce = 1 else yforce = 0
   if newaxis ne 0 then YSTYLE = yforce + 8 else YSTYLE = yforce
   
   if N_ELEMENTS(tag) eq 0 then tag = 'Data'

   IF KEYWORD_SET(zoom) then begin
     if ~ check_WTIME(zoom, OUT_QMS=tzoom) then message, WAVE_Std_Message('zoom', /ARG)
     if N_ELEMENTS(zoom) ne 2 then message, WAVE_Std_Message('zoom', NELEMENTS=2)
     p1 = VALUE_LOCATE(tqms,tzoom[0]) > 0
     p2 = VALUE_LOCATE(tqms,tzoom[1])
   endif else begin
     p1 = 0
     p2 = nt - 1
   endelse
  
  
  if ~KEYWORD_SET(range) then range = [MIN(data[p1:p2], /NAN), MAX(data[p1:p2],/NAN)]
  
  ; Make X AXIS
  w_date_tickformat, xtickformat, xtickunits, xtickinterval, TIME = tqms, HOURS = hours, DAYS = days, MONTHS = months, YEARS = years, MONTHYEARS = monthyears
  
  ; Legend    
  x = [0.75, 0.79] ; Line for legend 1
  y = [0.85, 0.85 ] ; Line for legend 1
  
  dx1 = 0.012  ; delta X between line end and Tag Text
  dx2 = 0.018  ; delta X between line end and Comment Text
  dy1 = 0.008  ; delta Y between line end and Tag Text (negative)
  dy2 = 0.045  ; delta Y between line end and Comment Text (negative)
  
  tsiz = 1.5   ; Tag Font size 
  tthi = 1.    ; Tag Font thick 
  csiz = 1.    ; Comment Font size 
  cthi = 1.    ; Comment Font thick 
  
  plo_siz = 1.3 ; Plot Font size
  plo_thi = 1.  ; Plot Font thick
  
  ddy = 0.1 ; Delta between Tag n and Tag n+1
  
  ppos = [0.1,0.15,0.67,0.90] ; Plot position
     
  jd = TIME_to_JD(time[p1:p2])  
  
  cgPlot, jd, data[p1:p2], title = title,  CHARSIZE=plo_siz, /NORMAL, $
   CHARTHICK = plo_thi, XTITLE = xtitle, Ytitle = Ytitle, YRANGe = range,  POSITION = ppos, XTICK_GET=xs, YTICK_GET=ys, $
    /NODATA, XTICKFORMAT= XTICKFORMAT, XTICKUNITS=xtickunits, XTICKINTERVAL = [xtickinterval], YSTYLE = YSTYLE, xstyle = xstyle, PSYM=psym, WINDOW = cgWin
       
 if N_ELEMENTS(HORILINE) ne 0 then $
    for i =0, N_ELEMENTS(HORILINE)-1 do cgPlots, [min(jd)-max(jd),max(jd)+max(jd)], [HORILINE[i],HORILINE[i]], $ ;TODO: make this beautiful
      color = cgColor('dark grey'), LINESTYLE=5, WINDOW = cgWin, NOCLIP = 0

  ; real plot
  if ~KEYWORD_SET(psym1) then begin
    cgplot, jd, data[p1:p2], COLOR = cgColor(color1), THI = thickness, LINESTYLE=style, PSYM=psym1, /OVERPLOT, WINDOW = cgWin
  endif else begin
    cgPlot, jd, data[p1:p2], COLOR = cgColor(color1), PSYM=psym1, SYMSIZE=thickness, /OVERPLOT, WINDOW = cgWin
  endelse

  if ~KEYWORD_SET(psym1) then begin
    cgplots, x, y,  COLOR = cgColor(color1), /NORMAL , THICK=thickness, LINESTYLE=style, WINDOW = cgWin
  endif else begin
    if psym1 eq 10 then _psym1 = 0 else _psym1 = psym1
    cgplots, x, y,  COLOR = cgColor(color1), /NORMAL , THICK=thickness, PSYM=_psym1, SYMSIZE=thickness, WINDOW = cgWin
  endelse
  

  cgtext, x[1]+ dx1 ,  y[0]- dy1, tag, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color1), /NORMAL , WINDOW = cgWin
  if KEYWORD_SET(coment1) then cgtext, x[1]+dx2 ,  y[0]- dy2, coment1, CHARSIZE=csiz, CHARTHICK = cthi, COLOR = cgColor(color1), /NORMAL, WINDOW = cgWin
  
  news = 2
  
  ; OVERPLOT
  if N_ELEMENTS(data2) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 2 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data2), MAX(data2)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style2) eq 1 then style = style2 
    y = y - ddy
    if ~KEYWORD_SET(psym2) then begin
      cgplot, TIME_to_JD(time2), data2, COLOR = cgColor(color2), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color2), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time2)], data2, COLOR = cgColor(color2), PSYM=psym2, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM2 eq 10 then psym2 = 0
      cgplots, x, y,  COLOR = cgColor(color2), THICK=thickness, /NORMAL, PSYM=psym2, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag2, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color2), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment2) then cgtext, x[1]+dx2 ,  y[0]- dy2, coment2, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color2), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data3) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 3 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data3), MAX(data3)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style3) eq 1  then style = style3 
    y = y - ddy
    if ~KEYWORD_SET(psym3) then begin
      cgplot, TIME_to_JD(time3), data3, COLOR = cgColor(color3), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color3), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time3)], data3, COLOR = cgColor(color3), PSYM=psym3, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM3 eq 10 then psym3 = 0
      cgplots, x, y,  COLOR = cgColor(color3), THICK=thickness, /NORMAL, PSYM=psym3, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag3, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color3), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment3) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment3, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color3), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data4) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 4 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data4), MAX(data4)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style4) eq 1  then style = style4 
    y = y - ddy
    if ~KEYWORD_SET(psym4) then begin
      cgplot, TIME_to_JD(time4), data4, COLOR = cgColor(color4), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color4), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time4)], data4, COLOR = cgColor(color4), PSYM=psym4, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM4 eq 10 then psym4 = 0
      cgplots, x, y,  COLOR = cgColor(color4), THICK=thickness, /NORMAL, PSYM=psym4, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag4, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color4), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment4) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment4, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color4), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data5) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 5 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data5), MAX(data5)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style5) eq 1  then style = style5 
    y = y - ddy
    if ~KEYWORD_SET(psym5) then begin
      cgplot, TIME_to_JD(time5), data5, COLOR = cgColor(color5), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color5), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time5)], data5, COLOR = cgColor(color5), PSYM=psym5, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM5 eq 10 then psym5 = 0
      cgplots, x, y,  COLOR = cgColor(color5), THICK=thickness, /NORMAL, PSYM=psym5, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag5, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color5), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment5) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment5, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color5), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data6) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 6 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data6), MAX(data6)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style6) eq 1  then style = style6 
    y = y - ddy
    if ~KEYWORD_SET(psym6) then begin
      cgplot, TIME_to_JD(time6), data6, COLOR = cgColor(color6), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color6), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time6)], data6, COLOR = cgColor(color6), PSYM=psym6, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM6 eq 10 then psym6 = 0
      cgplots, x, y,  COLOR = cgColor(color6), THICK=thickness, /NORMAL, PSYM=psym6, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag6, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color6), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment6) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment6, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color6), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data7) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 7 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data7), MAX(data7)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style7) eq 1  then style = style7 
    y = y - ddy
    if ~KEYWORD_SET(psym7) then begin
      cgplot, TIME_to_JD(time7), data7, COLOR = cgColor(color7), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color7), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time7)], data7, COLOR = cgColor(color7), PSYM=psym7, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM7 eq 10 then psym7 = 0
      cgplots, x, y,  COLOR = cgColor(color7), THICK=thickness, /NORMAL, PSYM=psym7, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag7, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color7), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment7) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment7, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color7), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data8) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 8 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data8), MAX(data8)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style8) eq 1  then style = style8 
    y = y - ddy
    if ~KEYWORD_SET(psym8) then begin
      cgplot, TIME_to_JD(time8), data8, COLOR = cgColor(color8), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color8), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time8)], data8, COLOR = cgColor(color8), PSYM=psym8, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM8 eq 10 then psym8 = 0
      cgplots, x, y,  COLOR = cgColor(color8), THICK=thickness, /NORMAL, PSYM=psym8, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag8, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color8), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment8) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment8, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color8), /NORMAL, WINDOW = cgWin
  endif  
  
  if N_ELEMENTS(data9) ne 0 then begin    
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 9 then begin
      if ~KEYWORD_SET(newrange) then newrange = [MIN(data9), MAX(data9)]
      cgAxis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=cgColor('Black'), CHARSIZE=plo_siz, CHARTHICK = plo_thi, YMINOR = 10, YRANGE = newrange, WINDOW = cgWin
      style = news
    endif
    if N_ELEMENTS(style9) eq 1  then style = style9 
    y = y - ddy
    if ~KEYWORD_SET(psym9) then begin
      cgplot, TIME_to_JD(time9), data9, COLOR = cgColor(color9), THI =  thickness, LINESTYLE=style, /OVERPLOT, WINDOW = cgWin
      cgplots, x, y,  COLOR = cgColor(color9), THICK=thickness, /NORMAL, LINESTYLE=style, WINDOW = cgWin
    endif else begin
      cgplot, [TIME_to_JD(time9)], data9, COLOR = cgColor(color9), PSYM=psym9, SYMSIZE=thickness, WINDOW = cgWin, /OVERPLOT
      if PSYM9 eq 10 then psym9 = 0
      cgplots, x, y,  COLOR = cgColor(color9), THICK=thickness, /NORMAL, PSYM=psym9, SYMSIZE=thickness, WINDOW = cgWin
    endelse   
    cgtext, x[1]+dx1 ,  y[0]-dy1, tag9, CHARSIZE=tsiz, CHARTHICK = tthi, COLOR = cgColor(color9), /NORMAL, WINDOW = cgWin
    if KEYWORD_SET(coment9) then cgtext, x[1]+dx2 ,  y[0]-dy2, coment9, CHARSIZE=csiz, CHARTHICK = tthi, COLOR = cgColor(color9), /NORMAL, WINDOW = cgWin
  endif  
  
  ; Vertical lines
  if N_ELEMENTS(VERTILINE) ne 0 then $
    for i =0, N_ELEMENTS(VERTILINE)-1 do cgPlots, [TIME_to_JD(VERTILINE[i]),TIME_to_JD(VERTILINE[i])], $
           range, color = cgColor('black'), LINESTYLE=2, WINDOW = cgWin, NOCLIP = 0
  
  
  
  ; output  
  if visible then begin
    cgControl, EXECUTE=1
    if KEYWORD_SET(PNG) then cgControl, CREATE_PNG=png, IM_RESIZE=im_resize, /IM_RASTER
    if KEYWORD_SET(EPS) then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
  endif else begin
    if KEYWORD_SET(PNG) then cgPS_Close, /PNG, resize = im_resize
    if KEYWORD_SET(EPS) then cgPS_Close
    WDELETE, xwin
  endelse
  
  !ORDER = pp
  
end