;+
;
; Wrapper for the cgZPlot routine, but with the x parameter being time. Everything
; else remains unchanged.
;
; :Params:
;    x: in, required, type=time
;         Time
;    y: in, required, type=any
;         A vector representing the dependent values to be plotted.
;
; :Keywords:
;    object: out, optional, type=objref
;         The object reference to the underlying object.
;     parent: in, optional, type=long
;         The identifer of the parent widget for this program's draw widget. If not
;         provided, the program will create it's own top-level base widget as the parent widget.
;     xsize: in, optional, type=int, default=640
;         The X size of the program's draw widget.
;     ysize: in, optional, type=int, default=512
;         The Y size of the program's draw widget.
;     zoomfactor: in, optional, type=float
;         Set this keyword to a number between 0.01 and 0.25. This affects the amount
;         of zooming when the X axis and Y axis are zoomed with the LEFT mouse button.
;         The default value is 0.05 or five percent of the current axis range on each
;         end of the axis, resulting in a 10 percent change in the axis length.
;     _ref_extra: in, optional, type=any
;        Any keyword appropriate for the IDL Plot or Coyote Graphic cgPlot command is
;        allowed in the program.
;-
pro w_gr_tzplot, x, y, $
    OBJECT=object, $
    PARENT=parent, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    ZOOMFACTOR=zoomfactor, $
    HOURS=hours, $
    DAYS=days, $
    MONTHS=months, $
    YEARS=years, $
    MONTHYEARS=monthyears, $
    _REF_EXTRA=extra
    
  @WAVE.inc
  compile_opt idl2
  
  ;Check args
  if ~ check_WTIME(x, OUT_QMS=_x) then message, WAVE_Std_Message('x', /ARG)
  nt = N_ELEMENTS(_x)
  if N_ELEMENTS(y) ne nt then message, WAVE_Std_Message('y', /ARG)
     
  dummy = LABEL_DATE(DATE_FORMAT=['%H:%I', '%D.%N.%Y'])
  
  cgZPlot, TIME_to_JD(_x), y, $
    OBJECT=Object, $
    PARENT=parent, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    ZOOMFACTOR=zoomfactor, $
    XTICKFORMAT=['LABEL_DATE','LABEL_DATE'], $ 
    XTICKUNITS = ['Hour', 'Day'], $    
    _EXTRA=extra
    
    
end