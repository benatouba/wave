;-----------------------------------------------------------------------
;+
; NAME:
;       PLO_show_img
;
; PURPOSE:
;       This function takes the pixmaped windows in a row and put them together
;
; CATEGORY:
;       PLO
;
; CALLING SEQUENCE:
;       PLO_show_img [, WINDOW = window, N_WINDOWS = N_windows, /VERTICAL, /PIXMAP, PNG = png]
;
; KEYWORDS:
;       WINDOW       : Window ID (default : 1). If an array is given, the different windows will be put together
;       OUT_WINDOW   : Id of the output window (default : the same as the first id of the Window Keyword). 
;       PIXMAP       : if the window has to be hidden
;       VERTICAL     : if the plots should be aggregated Verticaly instead of horizontaly
;       PNG          : the path of the PNG file to store the image in it. If setted, the image will not be shown.
;       
; OUTPUT:
;       
;       a beautiful plot 
;-
; MODIFICATION HISTORY:
;       Written by: FM, 2009
;-  
;-----------------------------------------------------------------------
pro plo_show_img, WINDOW = window, OUT_WINDOW = out_window, VERTICAL = vertical, PIXMAP = pixmap, PNG = png

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  ;--------------------------
  ; Check input
  ;--------------------------  
  if ~KEYWORD_SET(WINDOW) then win = 1 else win = window 
  
  N_win = N_ELEMENTS(win)
  
  if N_win gt 1 then begin
    for i = 0, N_win-1 do begin
        temp = win[i]
        WSET, temp
        tempI = Transpose(tvrd(/TRUE), [1,2,0])
        if KEYWORD_SET(VERTICAL) then begin
          if N_ELEMENTS(finalI) eq 0 then finalI = tempI else finalI = [[finalI],[tempI]]
        endif else begin 
          if N_ELEMENTS(finalI) eq 0 then finalI = tempI else finalI = [finalI,tempI]
        endelse
        WDELETE, temp
    endfor
  endif else begin  
    WSET, win
    finalI = Transpose(tvrd(/TRUE), [1,2,0])
    WDELETE, win
  end
  
  if KEYWORD_SET(OUT_WINDOW) then win = OUT_WINDOW else win = win[0]
  
  if KEYWORD_SET(PNG) then PIXMAP = TRUE
  Window, win, XSIZE=N_ELEMENTS(finalI[*,0,0]), YSIZE=N_ELEMENTS(finalI[0,*,0]), YPos=Window_pos, PIXMAP=pixmap
  tv, finalI, TRUE=3
  if KEYWORD_SET(PNG) then WRITE_PNG, png, tvrd(/TRUE)
  if ~KEYWORD_SET(pixmap) then pixmap = FALSE
  
  if (pixmap eq FALSE or KEYWORD_SET(PNG)) and OBJ_VALID(imgG) then OBJ_DESTROY, imgG
  
end