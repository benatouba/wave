;+
; :Description:
;    Merges two RGB images together using a tranpsaperent alpha channel 
;    with the rule::
;      final_img = img1 * alpha + img2 * (1. - alpha)
;
; :Params:
;    img1: in, required
;          RGB image, dimensions = [nx, ny, 3]
;    img2: in, required
;          can be a RGB image, dimensions = [nx, ny, 3] or a COLOR (string or RGB triplet)
;
; :Keywords:
;    ALPHA: in, otional, default=0.5
;           alpha channel, float between 0 and 1. 
;           Alpha can be a::
;             - scalar: in this case the alpha chanel is the same everywhere
;             - [nx,ny] array: in this case the alpha chanel ca be pixel dependant
;             - [nx,ny,3] array: in this case the alpha chanel ca be pixel and color dependant
;    COLOR_TO_ALPHA: in, otional, default=none
;                    set this to a color (string, or rgb triplet) *IN IMG2*
;                    where the alpha channel has to be set to 1.
;  
; :Author: FaM, 2013
;-
function w_transparentImages, img1, img2, ALPHA=alpha, COLOR_TO_ALPHA=color_to_alpha

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ;ON_ERROR, 2
  if N_ELEMENTS(img2) eq 1 then begin
    rgb = cgColor(img2, /TRIPLE)
    _img2 = img1 * 0B
    _img2[*,*,0] = rgb[0]
    _img2[*,*,1] = rgb[1]
    _img2[*,*,2] = rgb[2]
  endif else if N_ELEMENTS(img2) eq 3 then begin
    _img2 = img1 * 0B
    _img2[*,*,0] = img2[0]
    _img2[*,*,1] = img2[1]
    _img2[*,*,2] = img2[2]
  endif else begin
    _img2 = img2
  endelse
  s1 = SIZE(img1)
  s2 = SIZE(_img2)
  if s1[0] ne 3 then Message, WAVE_Std_Message('img1', NDIMS=3)
  if s2[0] ne 3 then Message, WAVE_Std_Message('img2', NDIMS=3)
  nx = s1[1]
  ny = s1[2]
  if s1[3] ne 3 then Message, '$IMG1 should be a [nx, ny, 3] array'
  if s2[3] ne 3 then Message, '$IMG2 should be a [nx, ny, 3] array'
  if s2[1] ne nx then Message, '$IMG1 and $IMG2 not compatible'
  if s2[2] ne ny then Message, '$IMG1 and $IMG2 not compatible'
  
  SetDefaultValue, alpha, 0.5
  
  sa = SIZE(alpha)
  case (sa[0]) of
    0: begin
      _alpha = FLTARR(nx, ny, 3) + alpha
    end
    1: begin
      _alpha = FLTARR(nx, ny, 3) + alpha[0]
    end
    2: begin
      if sa[1] ne nx then Message, '$ALPHA should be a [nx, ny] array'
      if sa[2] ne ny then Message, '$ALPHA should be a [nx, ny] array'
      _alpha = [[[alpha]],[[alpha]],[[alpha]]]
    end
    3: begin
      if sa[1] ne nx then Message, '$ALPHA should be a [nx, ny, 3] array'
      if sa[2] ne ny then Message, '$ALPHA should be a [nx, ny, 3] array'
      if sa[3] ne 3 then Message, '$ALPHA should be a [nx, ny, 3] array'
      _alpha = alpha
    end
    else: Message, WAVE_Std_Message('ALPHA', /ARG)
  endcase
  _alpha = 0. > FLOAT(_alpha) < 1.
  
  if N_ELEMENTS(COLOR_TO_ALPHA) ne 0 then begin
    case N_ELEMENTS(COLOR_TO_ALPHA) of
      1: _ca = cgColor(COLOR_TO_ALPHA, /TRIPLE)
      3: _ca = COLOR_TO_ALPHA
      else: Message, WAVE_Std_Message('COLOR_TO_ALPHA', /ARG)
    endcase
    p = where((_img2[*,*,0] eq _ca[0]) and (_img2[*,*,1] eq _ca[1]) and (_img2[*,*,2] eq _ca[2]), cnt)
    if cnt ne 0 then for i=0, 2 do begin
      _tmp = _alpha[*,*,i]
      _tmp[p] = 1.
      _alpha[*,*,i] = _tmp
    endfor
  endif
  
  return, byte(0 > img1 * _alpha + _img2 * (1.-_alpha) < 255)
  
end