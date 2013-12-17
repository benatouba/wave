;+
; :Description:
;    This is a simpe wrapper for the cgContour procedure but 
;    which takes an INFO structure from w_gr_datalevels as argument. 
;    Since all color handling is done by w_gr_Datalevels, all 
;    color-related keywords from cgContour have been removed. Furthermore,
;    /CELL_FILL is set by default if you use /FILL or /CELL_FILL, just because
;    it works better
;
; :Params:
;    info: in, required
;          the structure obtained from w_gr_datalevels
;    data: in, required
;          the data to contour
;    x: in, optional, type=any
;         A vector or two-dimensional array specifying the X coordinates for
;         the contour surface.
;    y: in, optional, type=any
;         A vector or two-dimensional array specifying the Y coordinates for
;         the contour surface.
;  
;  :Keywords:
;    See cgContour
;
; :Author: FM
;-
pro w_gr_contour, info, data, x, y, $
  ADDCMD=addcmd, $
  ASPECT=aspect, $
  AXISCOLOR=saxiscolor, $
  AXESCOLOR=saxescolor, $
  BACKGROUND=sbackground, $
  C_ANNOTATION=c_annotation, $
  C_CHARSIZE=c_charsize, $
  C_LABELS=c_labels, $
  C_ORIENTATION=c_orientation, $
  C_SPACING=c_spacing, $
  CELL_FILL=cell_fill, $
  CHARSIZE=charsize, $
  COLOR=scolor, $
  FILL=fill, $
  FONT=font, $
  ISOTROPIC=isotropic, $
  IRREGULAR=irregular, $
  LABEL=label, $
  LAYOUT=layout, $
  NOCLIP=noclip, $
  NOERASE=noerase, $
  ONIMAGE=onImage, $
  OUTCOLOR=outcolor, $
  OUTFILENAME=outfilename, $
  OUTLINE=outline, $
  OUTPUT=output, $
  OVERPLOT=overplot, $
  POSITION=position, $
  RESOLUTION=resolution, $
  T3D=t3d, $
  TITLE=title, $
  TRADITIONAL=traditional, $
  WINDOW=window, $
  XRANGE=xrange, $
  XSTYLE=xstyle, $
  XTHICK=xthick, $
  XTICKLEN=xticklen, $
  XTICKV=xtickv, $
  XTICKS=xticks, $
  XTITLE=xtitle, $
  YLOG=ylog, $
  YRANGE=yrange, $
  YSTYLE=ystyle, $
  YTHICK=ythick, $
  YTICKLEN=yticklen, $
  YTICKV=ytickv, $
  YTICKS=yticks, $
  YTITLE=ytitle, $
  ZVALUE=zvalue, $
  _REF_EXTRA=extra

  @WAVE.inc
  compile_opt idl2
  
  if ~arg_okay(info, /STRUCT) then Message, '$INFO should be a structure.'
  
  ; If fill is set and cell_fill is NOT set to 0, then force things
  if KEYWORD_SET(FILL) && (N_ELEMENTS(CELL_FILL) ne 0 && (CELL_FILL eq 0)) then begin
    _fill = 1
    _cell_fill = 0
  endif else if KEYWORD_SET(FILL) || KEYWORD_SET(CELL_FILL) then begin
    _fill = 0
    _cell_fill = 1
  endif
  
  if ~ tag_exist(info, 'colors') then  Message, '$INFO should contain a "COLORS" tag.'
  colors = info.colors
  if ~ tag_exist(info, 'levels') then  Message, '$INFO should contain a "LEVELS" tag.'
  levels = info.levels
  
  if info.is_Missing then colors = colors[1:*]
  if ~ info.is_ooTopColor then colors = [colors, colors[N_elements(colors)-1]] 
  if info.is_ooBotColor then begin
    ; the trick
    mlev = info.data_min - (10*info.epsilon)
    levels = [mlev, levels]
  endif  
  
  missingvalue = info.missing_value
  
  palette = w_gr_ColorToRGB(colors)

  cgContour, data, x, y, $
  ADDCMD=addcmd, $
  ASPECT=aspect, $
  AXISCOLOR=saxiscolor, $
  AXESCOLOR=saxescolor, $
  BACKGROUND=sbackground, $
  C_ANNOTATION=c_annotation, $
  C_CHARSIZE=c_charsize, $
  C_LABELS=c_labels, $
  C_ORIENTATION=c_orientation, $
  C_SPACING=c_spacing, $
  CELL_FILL=_cell_fill, $
  CHARSIZE=charsize, $
  COLOR=scolor, $
  FILL=_fill, $
  FONT=font, $
  ISOTROPIC=isotropic, $
  IRREGULAR=irregular, $
  LABEL=label, $
  LAYOUT=layout, $
  LEVELS=levels, $
  NOCLIP=noclip, $
  NOERASE=noerase, $
  MAP_OBJECT=map_object, $
  MISSINGVALUE=missingvalue, $
  ONIMAGE=onImage, $
  OUTCOLOR=outcolor, $
  OUTFILENAME=outfilename, $
  OUTLINE=outline, $
  OUTPUT=output, $
  OVERPLOT=overplot, $
  PALETTE=palette, $
  POSITION=position, $
  RESOLUTION=resolution, $
  T3D=t3d, $
  TITLE=title, $
  TRADITIONAL=traditional, $
  WINDOW=window, $
  XRANGE=xrange, $
  XSTYLE=xstyle, $
  XTHICK=xthick, $
  XTICKLEN=xticklen, $
  XTICKV=xtickv, $
  XTICKS=xticks, $
  XTITLE=xtitle, $
  YLOG=ylog, $
  YRANGE=yrange, $
  YSTYLE=ystyle, $
  YTHICK=ythick, $
  YTICKLEN=yticklen, $
  YTICKV=ytickv, $
  YTICKS=yticks, $
  YTITLE=ytitle, $
  ZVALUE=zvalue, $
  _REF_EXTRA=extra
  
end  