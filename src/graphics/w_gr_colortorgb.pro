;+
; :Description:
;    Converts a 24 bit color vector into a rgb triple.
;
; :Params:
;    color: in, required, type=LONG
;           the colors to convert
;    r: out, optional
;       if you want the R array
;    g: out, optional
;       if you want the G array
;    b: out, optional
;       if you want the B array
;    
;  :Keywords:
;    ROW: in, optional, type=boolean, default=0
;         Set this keyword to indicate you are getting the RGB_TABLE vectors
;         for use in the IDL's object graphics routines. Whereas TVLCT expects color 
;         tables to be 256x3 (column vectors), the object graphics routines expect them 
;         to be 3x256 (row vectors). Setting this keyword will transpose the vectors 
;         before they are returned.
;  
;  :Returns:
;    The palette
;   
; :History:
;     Written by FaM, 2011.
;-
function w_gr_ColorToRGB, color, r, g, b, ROW=row
   
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
   
  if ~ arg_okay(color, TYPE=IDL_LONG) then message, WAVE_Std_Message('color', /ARG)
    
  UNDEFINE, r, g, b
  r = BYTE(color) * 0B
  g = r
  b = r
  
  for i = 0, N_ELEMENTS(color)-1 do begin  
    bi = ROTATE(BitGet(LONG(color[i])), 2)  
    tr = 0L
    tg = 0L
    tb = 0L
    for j = 0, 7 do tr += bi[j] * 2 ^ j
    for j = 8, 15 do tg += bi[j] * 2 ^ (j-8)
    for j = 16, 23 do tb += bi[j] * 2 ^ (j-16)     
    r[i] = tr
    g[i] = tg
    b[i] = tb     
  endfor
  
  get_rgb_table = [[r], [g], [b]]
  IF Keyword_Set(row) THEN get_rgb_table = Transpose(get_rgb_table)
  
  return, reform(get_rgb_table)
  
end