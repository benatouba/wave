;-----------------------------------------------------------------------
;+
; NAME:
;       WTimeLine_plot
;
; PURPOSE:
;       plots some data in a (relatively) nice form
;       ! Far from being finished ! 
;
; CATEGORY:
;       plots
;
; CALLING SEQUENCE:
;       WTimeLine_plot, 
;
; INPUT:
;       
;
; OUTPUT:
;       
;
; KEYWORDS:
; 
; EXAMPLE:
; 
; 
;       
;-
;-----------------------------------------------------------------------
pro WTimeLine_plot, toplot,$  ; array to plot
                    time, $   ; time (ABS_DATE) (dame size as toplot)
                    tag,  $   ; legend (str)
                    color1 = color1,$ ; All FSC colors strings accepted. default: black
                    style1 = style1,$ ; linestyle. default: line
                    psym1 = psym1, $
                    COMENT1 = coment1,$ ; comment below the legend
                    TITLE = title, $ ; graph title
                    XTITLE = xtitle,$ ; x axis title (not necessary)
                    MONTHS = months,$ ; X tick lenght in months
                    DAYS = days, $; X tick lenght in days 
                    Years = years,$ ; X tick lenght in years 
                    MONYEAR = monyear, $ ; X tick lenght in month + years 
                    HOURS = hours,$ ; X tick lenght in hours 
                    ZOOM=zoom, $ ; format [{abs_date}1,{abs_date}2]
                    YTITLE = Ytitle,$ ; title of the y axis
                    RANGE = range,$ ; Y data range
                    NEWAXIS = newaxis, newrange = newrange, NEWTITLE = newtitle, $ ; if a second axis is to be drawn
                    THICKNESS = thickness, $; line thicknesses
                    HORILINE = HORILINE, VERTILINE = VERTILINE, $ ; if horizontal or vertical lines have to be drawn
                    PNG = png, PIXMAP = pixmap, $  ; pngs or pixmaps
                    toplot2, time2, color2, tag2, COMENT2 = coment2, style2 = style2, psym2 = psym2, $ 
                    toplot3, time3, color3, tag3, COMENT3 = coment3, style3 = style3, psym3 = psym3, $ 
                    toplot4, time4, color4, tag4, COMENT4 = coment4, style4 = style4, psym4 = psym4, $ 
                    toplot5, time5, color5, tag5, COMENT5 = coment5, style5 = style5, psym5 = psym5, $
                    toplot6, time6, color6, tag6, COMENT6 = coment6, style6 = style6, psym6 = psym6, $
                    toplot7, time7, color7, tag7, COMENT7 = coment7, style7 = style7, psym7 = psym7, $
                    toplot8, time8, color8, tag8, COMENT8 = coment8, style8 = style8, psym8 = psym8
                     
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

  ; prepare the plot 
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2  
  win = 1
  window, win, XSIZE=1150, YSIZE=700, Title='Plots', PIXMAP=pixmap
  !order = 0

  
   IF KEYWORD_SET(zoom) then begin
    p1 = VALUE_LOCATE(time.qms,zoom[0].qms)
    p2 = VALUE_LOCATE(time.qms,zoom[1].qms)
  endif else begin
    p1 = 0
    p2 = N_ELEMENTS(time.qms) - 1
  endelse
  
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
  endif else begin
    s = MAKE_TIME_STEP(DMS=time[p2].qms - time[p1].qms)
    if s.day gt 90 then begin
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
    endif else if s.hour gt 24 then begin
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
    
  if ~KEYWORD_SET(title) then title = 'AWS data'
  if ~KEYWORD_SET(color1) then color1 = 'black'
  if ~KEYWORD_SET(style1) then style = 0 else style = style1
  if ~KEYWORD_SET(psym1) then psym = 0 else psym = psym1
  if ~KEYWORD_SET(range) then range = [MIN(toplot[p1:p2]), MAX(toplot[p1:p2])]
  if ~KEYWORD_SET(thickness) then thickness = 1.8
  if ~KEYWORD_SET(newaxis) then newaxis = 0
  if newaxis ne 0 then YSTYLE = 1 + 8 else YSTYLE = 1
  
  if N_ELEMENTS(tag) eq 0 then tag = 'Data'
  
  jd = TIME_to_JD(time[p1:p2])
  
  loadct, 0, /SILENT
  plot, jd, toplot[p1:p2], title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, $
    CHARTHICK = 1.2, XTITLE = xtitle, Ytitle = Ytitle, YRANGe = range,  POSITION = [100,80,870,650], XTICK_GET=xs, YTICK_GET=ys, $
     /NODATA, XTICKFORMAT= XTICKFORMAT, XTICKUNITS=xtunits, XTICKINTERVAL = [xtinter], YSTYLE = YSTYLE, xstyle = 1, PSYM=psym
        
  if KEYWORD_SET(HORILINE) then plots, [min(xs),max(xs)], [HORILINE,HORILINE], color = 0, LINESTYLE=5
  if KEYWORD_SET(VERTILINE) then $
    for i =0, N_ELEMENTS(VERTILINE)-1 do plots, [TIME_to_JD(VERTILINE[i]),TIME_to_JD(VERTILINE[i])], range, color = FSC_Color('black'), LINESTYLE=5
    
  ; real plot
  if ~KEYWORD_SET(psym1) then begin
    oplot, jd, toplot[p1:p2], COLOR = FSC_Color(color1), THI = thickness, LINESTYLE=style, PSYM=psym
  endif else begin
    oplot, jd, toplot[p1:p2], COLOR = FSC_Color(color1), PSYM=psym, SYMSIZE=thickness
  endelse
  
  ; Mark the nan values ?
  if KEYWORD_SET(MARKNAN) then begin
    pnan = WHERE(~FINITE(toplot[p1:p2]), cntnan)
    if cntnan ne 0 then begin
      for k = 0, cntnan-1 do begin
        x =  jd[pnan[k]]
        y = (range[1]-range[0])/2.
        inter = 50. * (range[1]-range[0])/100.
        plots,[x,x], [y+inter,y-inter], COLOR = 0, THI = 2
      endfor
    endif
  endif
  
  ; Legend    
  x = [960, 1010]
  y = [600, 600]  
  if ~KEYWORD_SET(psym1) then begin
    plots, x, y,  COLOR = FSC_Color(color1), /DEVICE , THICK=2.5, LINESTYLE=style
  endif else begin
    plots, x, y,  COLOR = FSC_Color(color1), /DEVICE , THICK=2.5, PSYM=psym, SYMSIZE=thickness
  endelse
;  plots, x, y,  COLOR = FSC_Color(color1), /DEVICE , THICK=2.5, LINESTYLE=style
  XYOUTS, x[1]+5 ,  y[0]-5, tag, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color1), /DEVICE 
  if KEYWORD_SET(coment1) then XYOUTS, x[1]+5 ,  y[0]-20, coment1, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color1), /DEVICE 
  
  if N_ELEMENTS(toplot2) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 2 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style2) then style = style2 
    y = y - 60
    if ~KEYWORD_SET(psym2) then begin
      oplot, TIME_to_JD(time2), toplot2, COLOR = FSC_Color(color2), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color2), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin
      oplot, [TIME_to_JD(time2)], toplot2, COLOR = FSC_Color(color2), PSYM=psym2, SYMSIZE=thickness
      if PSYM2 eq 10 then psym2 = 0
      plots, x, y,  COLOR = FSC_Color(color2), THICK=2.5, /DEVICE, PSYM=psym2, SYMSIZE=thickness
    endelse    
;    oplot, TIME_to_JD(time2), toplot2, COLOR = FSC_Color(color2), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color2), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag2, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color2), /DEVICE
    if KEYWORD_SET(coment2) then XYOUTS, x[1]+5 ,  y[0]-20, coment2, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color2), /DEVICE
  endif
  if N_ELEMENTS(toplot3) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 3 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style3) then style = style3 
    y = y - 60
    if ~KEYWORD_SET(psym3) then begin
      oplot, TIME_to_JD(time3), toplot3, COLOR = FSC_Color(color3), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color3), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin 
      oplot, TIME_to_JD(time3), toplot3, COLOR = FSC_Color(color3), PSYM=psym3, SYMSIZE=thickness
      if PSYM3 eq 10 then psym3 = 0
      plots, x, y,  COLOR = FSC_Color(color3), THICK=2.5, /DEVICE, PSYM=psym3, SYMSIZE=thickness
    endelse
;    oplot, TIME_to_JD(time3), toplot3, COLOR = FSC_Color(color3), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color3), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag3, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color3), /DEVICE
    if KEYWORD_SET(coment3) then XYOUTS, x[1]+5 ,  y[0]-20, coment3, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color3), /DEVICE
  endif
  if N_ELEMENTS(toplot4) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 4 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style4) then style = style4 
    y = y - 60
    if ~KEYWORD_SET(psym4) then begin
      oplot, TIME_to_JD(time4), toplot4, COLOR = FSC_Color(color4), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color4), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin
      oplot, TIME_to_JD(time4), toplot4, COLOR = FSC_Color(color4), PSYM=psym4, SYMSIZE=thickness
      plots, x, y,  COLOR = FSC_Color(color4), THICK=2.5, /DEVICE, PSYM=psym4, SYMSIZE=thickness
    endelse
;    oplot, TIME_to_JD(time4), toplot4, COLOR = FSC_Color(color4), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color4), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag4, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color4), /DEVICE
    if KEYWORD_SET(coment4) then XYOUTS, x[1]+5 ,  y[0]-20, coment4, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color4), /DEVICE
  endif
  if N_ELEMENTS(toplot5) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 5 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style5) then style = style5 
    y = y - 60
    if ~KEYWORD_SET(psym5) then begin
      oplot, TIME_to_JD(time5), toplot5, COLOR = FSC_Color(color5), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color5), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin 
      oplot, TIME_to_JD(time5), toplot5, COLOR = FSC_Color(color5), PSYM=psym5, SYMSIZE=thickness
      plots, x, y,  COLOR = FSC_Color(color5), THICK=2.5, /DEVICE, PSYM=psym5, SYMSIZE=thickness
    endelse
;    oplot, TIME_to_JD(time5), toplot5, COLOR = FSC_Color(color5), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color5), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag5, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color5), /DEVICE
    if KEYWORD_SET(coment5) then XYOUTS, x[1]+5 ,  y[0]-20, coment5, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color5), /DEVICE
  endif
  if N_ELEMENTS(toplot6) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 6 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style6) then style = style6 
    y = y - 60
    if ~KEYWORD_SET(psym6) then begin 
      oplot, TIME_to_JD(time6), toplot6, COLOR = FSC_Color(color6), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color6), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin
      oplot, TIME_to_JD(time6), toplot6, COLOR = FSC_Color(color6), PSYM=psym6, SYMSIZE=thickness
      plots, x, y,  COLOR = FSC_Color(color6), THICK=2.5, /DEVICE, PSYM=psym6, SYMSIZE=thickness
    endelse
;    oplot, TIME_to_JD(time6), toplot6, COLOR = FSC_Color(color6), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color6), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag6, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color6), /DEVICE
    if KEYWORD_SET(coment6) then XYOUTS, x[1]+5 ,  y[0]-20, coment6, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color6), /DEVICE
  endif
  if N_ELEMENTS(toplot7) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 7 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style7) then style = style7 
    y = y - 60
    if ~KEYWORD_SET(psym7) then begin
      oplot, TIME_to_JD(time7), toplot7, COLOR = FSC_Color(color7), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color7), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin
      oplot, TIME_to_JD(time7), toplot7, COLOR = FSC_Color(color7), PSYM=psym7, SYMSIZE=thickness
      plots, x, y,  COLOR = FSC_Color(color7), THICK=2.5, /DEVICE, PSYM=psym7, SYMSIZE=thickness
    endelse
;    oplot, TIME_to_JD(time7), toplot7, COLOR = FSC_Color(color7), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color7), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag7, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color7), /DEVICE
    if KEYWORD_SET(coment7) then XYOUTS, x[1]+5 ,  y[0]-20, coment7, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color7), /DEVICE
  endif
  if N_ELEMENTS(toplot8) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 8 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style8) then style = style8 
    y = y - 60
    if ~KEYWORD_SET(psym8) then begin
      oplot, TIME_to_JD(time8), toplot8, COLOR = FSC_Color(color8), THI =  thickness, LINESTYLE=style
      plots, x, y,  COLOR = FSC_Color(color8), THICK=2.5, /DEVICE, LINESTYLE=style
    endif else begin 
      oplot, TIME_to_JD(time8), toplot8, COLOR = FSC_Color(color8), PSYM=psym8, SYMSIZE=thickness
      plots, x, y,  COLOR = FSC_Color(color8), THICK=2.5, /DEVICE, PSYM=psym8, SYMSIZE=thickness
    endelse
;    oplot, TIME_to_JD(time8), toplot8, COLOR = FSC_Color(color8), THI =  thickness, LINESTYLE=style
;    plots, x, y,  COLOR = FSC_Color(color8), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag8, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color8), /DEVICE
    if KEYWORD_SET(coment8) then XYOUTS, x[1]+5 ,  y[0]-20, coment8, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color8), /DEVICE
  endif
   
  PLO_show_img, PIXMAP=pixmap, WINDOW = w, PNG = png
  
  
end

;TODO: make this procedure more generic
pro WDiurnal_plot, toplot, time, tag, color1 = color1, style1 = style1, COMENT1 = coment1, TITLE = title, $
                   YTITLE = Ytitle, RANGE = range, NEWAXIS = newaxis, newrange = newrange, NEWTITLE = newtitle, $
                    THICKNESS = thickness, HORILINE = HORILINE, PNG = png, PIXMAP = pixmap, zoom = zoom, HOURS = hours, $           
                    toplot2, time2, color2, tag2, COMENT2 = coment2, style2 = style2,$ 
                    toplot3, time3, color3, tag3, COMENT3 = coment3, style3 = style3,$ 
                    toplot4, time4, color4, tag4, COMENT4 = coment4, style4 = style4,$ 
                    toplot5, time5, color5, tag5, COMENT5 = coment5, style5 = style5,$
                    toplot6, time6, color6, tag6, COMENT6 = coment6, style6 = style6,$
                    toplot7, time7, color7, tag7, COMENT7 = coment7, style7 = style7,$
                    toplot8, time8, color8, tag8, COMENT8 = coment8, style8 = style8
                     
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

  ; prepare the plot 
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2  
  window, 1, XSIZE=950, YSIZE=600, Title='Plots', PIXMAP=pixmap
  !order = 0
 
   IF KEYWORD_SET(zoom) then begin
    p1 = VALUE_LOCATE(time,zoom[0])
    p2 = VALUE_LOCATE(time,zoom[1])
  endif else begin
    p1 = 0
    p2 = N_ELEMENTS(time) - 1
  endelse
   
  if ~KEYWORD_SET(title) then title = 'AWS data'
  if ~KEYWORD_SET(color1) then color1 = 'black'
  if ~KEYWORD_SET(style1) then style = 0 else style = style1
  if ~KEYWORD_SET(range) then range = [MIN(toplot[p1:p2]), MAX(toplot[p1:p2])]
  if ~KEYWORD_SET(thickness) then thickness = 1.8
  if ~KEYWORD_SET(newaxis) then newaxis = 0
  if ~KEYWORD_SET(hours) then xtinter = 1 else xtinter = hours
  if newaxis ne 0 then YSTYLE = 1 + 8 else YSTYLE = 1
  
  if N_ELEMENTS(tag) eq 0 then tag = 'Data'
    
  loadct, 5
  ; dummy plot to avoid plotting in black
  plot, time[p1:p2], toplot[p1:p2], title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, $
    CHARTHICK = 1.2, XTITLE = 'Hours of day', Ytitle = Ytitle, YRANGe = range, POSITION = [80,60,700,540], $
      /NODATA, XTICKINTERVAL = xtinter, YSTYLE = YSTYLE,  XTICK_GET=xs, YTICK_GET=ys
  
  if KEYWORD_SET(HORILINE) then plots, [min(xs),max(xs)], [HORILINE,HORILINE], color = 0, LINESTYLE=5
   
  ; real plot
  oplot, time[p1:p2], toplot[p1:p2], COLOR = FSC_Color(color1), THI = thickness, LINESTYLE=style
  
  ; Mark the nan values ?
  if KEYWORD_SET(MARKNAN) then begin
    pnan = WHERE(~FINITE(toplot[p1:p2]), cntnan)
    if cntnan ne 0 then begin
      for k = 0, cntnan-1 do begin
        x =  jd[pnan[k]]
        y = (range[1]-range[0])/2.
        inter = 50. * (range[1]-range[0])/100.
        plots,[x,x], [y+inter,y-inter], COLOR = 0, THI = 2
      endfor
    endif
  endif
  
  ; Legend    
  x = [770, 800]
  y = [500, 500]  
  plots, x, y,  COLOR = FSC_Color(color1), /DEVICE , THICK=2.5, LINESTYLE=style
  XYOUTS, x[1]+5 ,  y[0]-5, tag, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color1), /DEVICE 
  if KEYWORD_SET(coment1) then XYOUTS, x[1]+5 ,  y[0]-20, coment1, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color1), /DEVICE 
  
  if N_ELEMENTS(toplot2) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 2 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style2) then style = style2 
    y = y - 60
    oplot, time2, toplot2, COLOR = FSC_Color(color2), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color2), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag2, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color2), /DEVICE
    if KEYWORD_SET(coment2) then XYOUTS, x[1]+5 ,  y[0]-20, coment2, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color2), /DEVICE
  endif
    if N_ELEMENTS(toplot3) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 3 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style3) then style = style3 
    y = y - 60
    oplot, time3, toplot3, COLOR = FSC_Color(color3), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color3), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag3, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color3), /DEVICE
    if KEYWORD_SET(coment3) then XYOUTS, x[1]+5 ,  y[0]-20, coment3, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color3), /DEVICE
  endif
  if N_ELEMENTS(toplot4) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 4 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style4) then style = style4 
    y = y - 60
    oplot, time4, toplot4, COLOR = FSC_Color(color4), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color4), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag4, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color4), /DEVICE
    if KEYWORD_SET(coment4) then XYOUTS, x[1]+5 ,  y[0]-20, coment4, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color4), /DEVICE
  endif
  if N_ELEMENTS(toplot5) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 5 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style5) then style = style5 
    y = y - 60
    oplot, time5, toplot5, COLOR = FSC_Color(color5), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color5), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag5, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color5), /DEVICE
    if KEYWORD_SET(coment5) then XYOUTS, x[1]+5 ,  y[0]-20, coment5, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color5), /DEVICE
  endif
  if N_ELEMENTS(toplot6) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 6 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style6) then style = style6 
    y = y - 60
    oplot, time6, toplot6, COLOR = FSC_Color(color6), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color6), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag6, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color6), /DEVICE
    if KEYWORD_SET(coment6) then XYOUTS, x[1]+5 ,  y[0]-20, coment6, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color6), /DEVICE
  endif
  if N_ELEMENTS(toplot7) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 7 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style7) then style = style7 
    y = y - 60
    oplot, time7, toplot7, COLOR = FSC_Color(color7), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color7), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag7, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color7), /DEVICE
    if KEYWORD_SET(coment7) then XYOUTS, x[1]+5 ,  y[0]-20, coment7, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color7), /DEVICE
  endif
  if N_ELEMENTS(toplot8) ne 0 then begin
    ; Are the next plots on a new axis ?
    if NEWAXIS eq 8 then begin
      Axis, YAxis=1, YTitle=NEWTITLE, /save, COLOR=0, CHARSIZE=1.8, /DEVICE, CHARTHICK = 1.2, YMINOR = 10, YRANGE = newrange
      style = 2
    endif
    if KEYWORD_SET(style8) then style = style8 
    y = y - 60
    oplot, time8, toplot8, COLOR = FSC_Color(color8), THI =  thickness, LINESTYLE=style
    plots, x, y,  COLOR = FSC_Color(color8), THICK=2.5, /DEVICE, LINESTYLE=style
    XYOUTS, x[1]+5 ,  y[0]-5, tag8, CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color(color8), /DEVICE
    if KEYWORD_SET(coment8) then XYOUTS, x[1]+5 ,  y[0]-20, coment8, CHARSIZE=1.3, CHARTHICK = 1., COLOR = FSC_Color(color8), /DEVICE
  endif
  
  
  if KEYWORD_SET(PNG) then begin
    img = TVRD(/true)
    WRITE_PNG, png , img
  end
  
end

;TODO: make this procedure more generic
pro WHisto_plot, h, loc, tag, color1 = color1, COMENT1 = coment1, THICKNESS = thickness, $
                     PNG = png, YTITLE = Ytitle, TITLE = title, PIXMAP = pixmap, XRANGE = xrange, xtitle = xtitle, $           
                     h2, loc2, color2, tag2, COMENT2 = coment2, $ 
                     h3, loc3, color3, tag3, COMENT3 = coment3, $ 
                     h4, loc4, color4, tag4, COMENT4 = coment4
                     
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

  ; prepare the plot  
  window, 1, XSIZE=900, YSIZE=600, Title='Plots', PIXMAP=pixmap
  !order = 0
  
  xtinter = 1
  
  ; colors (we should make more)
  str_colors = ['black','red','blue','orange','yellow','purple','brown','green']
  colors = [0,111,47,159,188,66,95,175]
  
  if ~KEYWORD_SET(title) then title = 'Histogramm'
  if ~KEYWORD_SET(color1) then color1 = 'black'
  if ~KEYWORD_SET(thickness) then thickness = 1.8
  if ~KEYWORD_SET(xtitle) then xtitle = 'Bins'
  
  if N_ELEMENTS(tag) eq 0 then tag = 'Data'
      
  loadct, 5
  ; dummy plot to avoid plotting in black
  plot, loc, h, title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, $
    CHARTHICK = 1.2, XTITLE = xtitle, Ytitle = Ytitle, YRANGe = range, POSITION = [80,60,700,540], $
      /NODATA, XTICKS =8, XRANGE = xrange, xstyle = 1, PSYM=10;, XMINOR = 1, YMINOR = 10
      
  ; real plot
  p = (where(str_equiv(str_colors) eq str_equiv(color1)))[0]
  oplot, loc, h, COLOR = colors[p], THI = thickness
  
  ; Legend    
  x = [720, 750]
  y = [500, 500]  
  plots, x, y,  COLOR = colors[p], /DEVICE , THICK=2.5
  XYOUTS, x[1]+5 ,  y[0]-5, tag, CHARSIZE=2, CHARTHICK = 1.3, COLOR = colors[p], /DEVICE 
  if KEYWORD_SET(coment1) then XYOUTS, x[1]+5 ,  y[0]-20, coment1, CHARSIZE=1.3, CHARTHICK = 1., COLOR = colors[p], /DEVICE 
  
  if N_ELEMENTS(h2) ne 0 then begin
    y = y - 60
    p = (where(str_equiv(str_colors) eq str_equiv(color2)))[0]
    oplot, loc2, h2, COLOR = colors[p], THI = thickness
    plots, x, y,  COLOR = colors[p], THICK=2.5, /DEVICE
    XYOUTS, x[1]+5 ,  y[0]-5, tag2, CHARSIZE=2, CHARTHICK = 1.3, COLOR = colors[p], /DEVICE
    if KEYWORD_SET(coment2) then XYOUTS, x[1]+5 ,  y[0]-20, coment2, CHARSIZE=1.3, CHARTHICK = 1., COLOR = colors[p], /DEVICE
  endif
  if N_ELEMENTS(h3) ne 0 then begin
    y = y - 60
    p = (where(str_equiv(str_colors) eq str_equiv(color3)))[0]
    oplot, loc3, h3, COLOR = colors[p], THI = thickness
    plots, x, y,  COLOR = colors[p], THICK=2.5, /DEVICE
    XYOUTS, x[1]+5 ,  y[0]-5, tag3, CHARSIZE=2, CHARTHICK = 1.3, COLOR = colors[p], /DEVICE 
    if KEYWORD_SET(coment3) then XYOUTS, x[1]+5 ,  y[0]-20, coment3, CHARSIZE=1.3, CHARTHICK = 1., COLOR = colors[p], /DEVICE 
  endif
  if N_ELEMENTS(h4) ne 0 then begin
    y = y - 60
    p = (where(str_equiv(str_colors) eq str_equiv(color4)))[0]
    oplot, loc4, h4, COLOR = colors[p], THI = thickness
    plots, x, y,  COLOR = colors[p], THICK=2.5, /DEVICE
    XYOUTS, x[1]+5 ,  y[0]-5, tag4, CHARSIZE=2, CHARTHICK = 1.3, COLOR = colors[p], /DEVICE 
    if KEYWORD_SET(coment4) then XYOUTS, x[1]+5 ,  y[0]-20, coment4, CHARSIZE=1.3, CHARTHICK = 1., COLOR = colors[p], /DEVICE 
  endif
  
  if KEYWORD_SET(PNG) then begin
    img = TVRD(/true)
    WRITE_PNG, png , img
  end
  
end

;TODO: make this procedure more generic
pro WScatter_plot, x, y, xtitle, ytitle, PNG = png, title= title, PIXMAP = pixmap, range = range, NOFIT = nofit, NOCORR = nocorr

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

  ; prepare the plot  
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2  
  window, 1, XSIZE=600, YSIZE=600, Title='Plots', PIXMAP=pixmap
  !order = 0
  
  if ~KEYWORD_SET(range) then range = [min([x,y]),max([x,y])]
    
  loadct, 5
  plot, x, y, title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, PSYM=1, XTICK_GET=xs, $
   CHARTHICK = 1.6, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [80,80,520,520], XRange=range, yrange= range
  
  plots, xs, xs, color = 0, LINESTYLE=5
  
  if not KEYWORD_SET(NOFIT) then begin
  
    l = LINFIT(x, y)
    
    plots, xs, (l[0] + l[1]*xs), color = 0, NOCLIP = 0
    
    if l[0] lt 0 then sign = '- ' else sign = '+ '
    
    text = 'y = ' + STRING(l[1], FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(l[0]), FORMAT='(F6.2)')
    XYOUTS, 95, 490, text, CHARSIZE= 2., COLOR=0, CHARTHICK=1., /DEVICE
        
  endif
  
  if NOt KEYWORD_SET(nocorr) then begin
          r2 = CORRELATE(x,y)
    r2 = r2*r2
      text = 'r2 = ' + STRING(r2, FORMAT='(F5.2)')
    XYOUTS, 95, 460, text, CHARSIZE= 2., COLOR=0, CHARTHICK=1., /DEVICE
  endif
  
  if KEYWORD_SET(PNG) then begin
    img = TVRD(/true)
    WRITE_PNG, png , img
  end
  
end

;TODO: make this procedure more generic
pro Wcloud_plot, x, y, xtitle, ytitle, PNG = png, title= title, PIXMAP = pixmap, xrange = xrange, yrange = yrange

  ; prepare the plot
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2
  window, 1, XSIZE=900, YSIZE=600, Title='Plots', PIXMAP=pixmap
  !order = 0
  
  loadct, 5
  plot, x, y, title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, PSYM=1, XTICK_GET=xs, $
    CHARTHICK = 1.6, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [80,80,820,520], XRange=xrange, yrange=yrange, YSTYLE = 1, XSTYLE = 1
    
  if KEYWORD_SET(PNG) then begin
    img = TVRD(/true)
    WRITE_PNG, png , img
  end
  
end

;TODO: make this procedure more generic
pro WwindMatrix, windspeed, winddir, time
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  dqms = 86400000ll
  
  qms1 = time.qms
  n = N_elements(qms1)
  if time[0].hour ne 1 then message, 'For the moment, the time serie must begin at 01H'
  if time[n-1].hour ne 0 then message, 'For the moment, the time serie must end at 00H'
    
  qmstart = FLOOR(DOUBLE(qms1[0]) / dqms) * dqms
  qmsend = CEIL(qms1[n-1] / dqms) * dqms
  qms2 = qmstart + INDGEN((qmsend-qmstart )/dqms + 1) * dqms
  
  newtime = MAKE_ABS_DATE(qms = qms2)
  newtime = newtime[0:N_ELEMENTS(newtime) - 2]
  nbdays = N_ELEMENTS(newtime)
  
  matspeed = rotate(reform(windspeed,24,nbdays),4)
  matdir = rotate(reform(winddir,24,nbdays),4)
  
  ; prepare the plot
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2
  window, 1, XSIZE=1150, YSIZE=750, Title='Plots', PIXMAP=pixmap
  !order = 0
  dummy = LABEL_DATE(DATE_FORMAT=['%D%M']) ; For the time axis
  XTICKFORMAT = ['LABEL_DATE']
  xtunits = 'Days'
  xtinter = 6
  
  
  title = 'Wind speed'
  
  ystyle = 1  
  jd = TIME_to_JD(newtime)
  jd = [jd,jd[nbdays-1]+1.]
  plotp = [100,130,1020,650]
  
  loadct, 0, /SILENT
  range = [0, 24]
  plot, jd, matspeed, title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, $
    CHARTHICK = 1.2, XTITLE = xtitle, Ytitle = 'Hour of day', YRANGe = range,  POSITION = PLOTP, XTICK_GET=xs, YTICK_GET=ys, YMINOR = 4, YTICKINTERVAL = 4, XMINOR = 6, $
    /NODATA, xstyle = 1, xtickLen= -0.01, yticklen = -0.01,XTICKFORMAT= XTICKFORMAT, XTICKUNITS=xtunits, XTICKINTERVAL = [xtinter], YSTYLE = YSTYLE, PSYM=psym
    
  Loadct , 13, /SILENT
  TVLCT, r, g, b, /Get
  
  utils_data_levels, MATSPEED, data_levels, colors, NLEVS = 9, COLOR_RANGE=[47,255], range = [0,8]
  
  N_Colors = 9
  color_levs = [255,colors]
  
  s_r = bytarr(N_Colors + 1) + 255B
  s_g = bytarr(N_Colors + 1) + 255B
  s_b = bytarr(N_Colors + 1) + 255B
  
  for i=0, N_Colors do begin
    s_r[i] = r[color_levs[i]]
    s_g[i] = g[color_levs[i]]
    s_b[i] = b[color_levs[i]]
  endfor
  
  t = CONGRID(MATSPEED, plotp[2]-plotp[0] - 1, plotp[3]-plotp[1] - 1, /CENTER)
  
  re = LONG(t) * 0L - 1L
  img = bytarr(plotp[2]-plotp[0], plotp[3]-plotp[1],3)
  
  for l=0, N_Colors-1 do begin
    if l lt N_Colors-1 then p = where(t ge data_levels[l] and t lt data_levels[l+1], cnt) $
    else p = where(t ge data_levels[l], cnt)
    if cnt gt 0 then re[p]= l
  endfor
  
  re+=1
  
  r = byte(0 > s_r[re] < 255)
  g = byte(0 > s_g[re] < 255)
  b = byte(0 > s_b[re] < 255)
  img = [[[r]],[[g]],[[b]]]
  
  tv, img, plotp[0]+1, plotp[1]+1, /DEVICE, TRUE=3
  
  DCBar, colors, COLOR="BLACK", LABELS=STRING(DATA_LEVELS, FORMAT='(F5.2)'), Position=[0.2,0.04,0.8,0.06], TITLE='speed (m/s)', CHARSIZE = 2, $
    MYCHARDIFAC = 0.9, CHARTHICK=1
    
  title = 'Wind direction'
  ; prepare the plot
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2
  window, 2, XSIZE=1150, YSIZE=750, Title='Plots', PIXMAP=pixmap
  
  loadct, 0, /SILENT
  plot, jd, matspeed, title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, $
    CHARTHICK = 1.2, XTITLE = xtitle, Ytitle = 'Hour of day', YRANGe = range,  POSITION = PLOTP, XTICK_GET=xs, YTICK_GET=ys, YMINOR = 4, YTICKINTERVAL = 4, XMINOR = 6, $
    /NODATA, xstyle = 1, xtickLen= -0.01, yticklen = -0.01,XTICKFORMAT= XTICKFORMAT, XTICKUNITS=xtunits, XTICKINTERVAL = [xtinter], YSTYLE = YSTYLE, PSYM=psym
    
  Loadct , 13, /SILENT
  TVLCT, r, g, b, /Get
  
  utils_data_levels, matdir, data_levels, colors, NLEVS = 8, COLOR_RANGE=[17,255], range = [0,8]
  
  p = where(matdir gt 337.5, cnt)
  if cnt ne 0 then matdir[p] = matdir[p] - 360.
  data_levels = indgen(8) * 45. - 22.5
  tags = ['N','NE','E','SE','S','SW','W','NW']
  
  N_Colors = 8
  color_levs = [255,colors]
  
  s_r = bytarr(N_Colors + 1) + 255B
  s_g = bytarr(N_Colors + 1) + 255B
  s_b = bytarr(N_Colors + 1) + 255B
  
  for i=0, N_Colors do begin
    s_r[i] = r[color_levs[i]]
    s_g[i] = g[color_levs[i]]
    s_b[i] = b[color_levs[i]]
  endfor
  
  t = CONGRID(matdir, plotp[2]-plotp[0] - 1, plotp[3]-plotp[1] - 1, /CENTER)
  
  re = LONG(t) * 0L - 1L
  img = bytarr(plotp[2]-plotp[0], plotp[3]-plotp[1],3)
  
  for l=0, N_Colors-1 do begin
    if l lt N_Colors-1 then p = where(t ge data_levels[l] and t lt data_levels[l+1], cnt) $
    else p = where(t ge data_levels[l], cnt)
    if cnt gt 0 then re[p]= l
  endfor
  
  re+=1
  
  r = byte(0 > s_r[re] < 255)
  g = byte(0 > s_g[re] < 255)
  b = byte(0 > s_b[re] < 255)
  img = [[[r]],[[g]],[[b]]]
  
  tv, img, plotp[0]+1, plotp[1]+1, /DEVICE, TRUE=3
  
  DCBar, colors, COLOR="BLACK", LABELS=tags, Position=[0.2,0.04,0.8,0.06], TITLE='Wind direction', CHARSIZE = 2, $
    MYCHARDIFAC = 0.9, CHARTHICK=1
  
end