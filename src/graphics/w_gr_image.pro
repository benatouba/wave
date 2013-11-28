;+
; :Description:
;    This is a simpe wrapper for the cgImage procedure but 
;    which takes an INFO structure from w_gr_datalevels as argument. 
;    Since all color handling is done by w_gr_Datalevels, all 
;    color-related keywords from cgImage have been removed.
;
; :Params:
;    info: in, required
;          the structure obtained from w_gr_datalevels
;    x: in, optional, type=integer
;        The X position of the lower-left corner of the image in device
;        coordinates. This parameter is only recognized if the TV keyword 
;        is set. If the Y position is not used, X is taken to be the image
;        "position" in the window. See the TV command documenation for details. 
;    y: in, optional, type=integer      
;        The Y position of the lower-left corner of the image in device
;        coordinates. This parameter is only recognized if the TV keyword 
;        is set.
;  
;  :Keywords:
;    See cgimage
;
; :Author: FM
;-
pro w_gr_image, info, x, y, $
  ADDCMD=addcmd, $
  ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
  ALPHABGPOSITION=alphabgpos, $
  ALPHAFGPOSITION=alphafgpos, $
  AXIS=axis, $
  AXES=axes, $
  AXKEYWORDS=axkeywords, $
  BACKGROUND=background, $
  CHARSIZE=charsize, $
  CLIP=clip, $
  DISPLAY=display, $
  EXCLUDE=exclude, $
  FILENAME=filename, $
  FIT_INSIDE=fit_inside, $
  FONT=font, $
  KEEP_ASPECT_RATIO=keep_aspect, $
  LAYOUT=layout, $
  MAPCOORD=mapcoord, $
  MARGIN=margin, $
  MULTIMARGIN=multimargin, $
  NOERASE=noerase, $
  NORMAL=normal, $
  OUTFILENAME=outfilename, $
  OUTPUT=output, $
  OPOSITION=oposition, $
  OVERPLOT=overplot, $
  POSITION=position, $
  QUIET=quiet, $
  SAVE=save, $
  TITLE=title, $
  TRANSPARENT=transparent, $
  TV=tv, $
  WINDOW=window, $
  XRANGE=plotxrange, $
  XTITLE=plotxtitle, $
  YRANGE=plotyrange, $
  YTITLE=plotytitle, $
  _REF_EXTRA=extra
  
  @WAVE.inc
  compile_opt idl2
  
  if ~arg_okay(info, /STRUCT) then Message, '$INFO should be a structure.'
  if ~ tag_exist(info, 'loc') then  Message, '$INFO should contain a "LOC" tag.'
  if ~ tag_exist(info, 'colors') then  Message, '$INFO should contain a "COLORS" tag.'
    
  cgImage, info.loc, x, y, PALETTE=w_gr_ColorToRGB(info.colors), MINUS_ONE=0, $
    ADDCMD=addcmd, $
    ALPHABACKGROUNDIMAGE=alphaBackgroundImage, $
    ALPHABGPOSITION=alphabgpos, $
    ALPHAFGPOSITION=alphafgpos, $
    AXIS=axis, $
    AXES=axes, $
    AXKEYWORDS=axkeywords, $
    BACKGROUND=background, $
    CHARSIZE=charsize, $
    CLIP=clip, $
    DISPLAY=display, $
    EXCLUDE=exclude, $
    FILENAME=filename, $
    FIT_INSIDE=fit_inside, $
    FONT=font, $
    KEEP_ASPECT_RATIO=keep_aspect, $
    LAYOUT=layout, $
    MAPCOORD=mapcoord, $
    MARGIN=margin, $
    MULTIMARGIN=multimargin, $
    NOERASE=noerase, $
    NORMAL=normal, $
    OUTFILENAME=outfilename, $
    OUTPUT=output, $
    OPOSITION=oposition, $
    OVERPLOT=overplot, $
    POSITION=position, $
    QUIET=quiet, $
    SAVE=save, $
    TITLE=title, $
    TRANSPARENT=transparent, $
    TV=tv, $
    WINDOW=window, $
    XRANGE=plotxrange, $
    XTITLE=plotxtitle, $
    YRANGE=plotyrange, $
    YTITLE=plotytitle, $
    _REF_EXTRA=extra
    
end