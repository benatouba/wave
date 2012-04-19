;+
; :Description:
;    Describe the procedure.
;
;
;
; :Keywords:
;    POSITION
;    ADDCMD
;    DATA
;    NORMAL
;    SIZE
;    
;-
pro w_add_RGB_legend, POSITION=position, ADDCMD=addcmd, DATA=data, NORMAL=normal, RADIUS=radius

  sqr = BYTARR(150,150) +255
  sqr[10:140,10:140] = 55
  circle = [[[sqr]],[[sqr]],[[sqr]]]
  
  if N_ELEMENTS(POSITION) eq 0 then position = [0.1,0,1]
  if N_ELEMENTS(radius) eq 0 then radius = [0.1,0,1]
  
  mypos = [POSITION[0]-RADIUS,POSITION[1]-RADIUS,POSITION[0]+RADIUS,POSITION[1]+RADIUS] 
  
  cgImage, circle, POSITION=mypos, NORMAL=normal, DATA=data, /SAVE, /KEEP_ASPECT_RATIO
  
end