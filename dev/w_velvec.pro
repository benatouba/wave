;+
; :Description:
;    Plot velocity vectors wherever you want.
;
; :Params:
;    posx: in, required
;          An array of any dimension, containing the x-components
;          of the vector positions
;    posy: in, required
;          An array of the same dimension as posx, containing the 
;          y-components of the vector positions
;    velx: in, required
;          An array of the same dimension as posx, containing the 
;          x-components of the particle velocities
;    vely: in, required
;          An array of the same dimension as posx, containing the 
;          y-components of the particle velocities
;
; :Keywords:
;    COLORS: in, optional
;            the vector colors. Can be either a scalar, or
;            a vector (nmeric or string) the same size as posx
;    STDVEL: in, optional, default=max velocity
;            the velocity (in velocity units) associated to
;            the standard length (see) the length keyword.
;            set this to be sure to have always the same length
;            between different plots in the same window (or 
;            different windows but with the same X size!)
;    LENGTH: in, optional, default=0.08
;            the length of a vector of STDVEL velocity,
;            in X-normal coordinates 
;    DATA: in, optional, default=true
;          plot in data coordinates
;    NORMAL: in, optional, default=false
;            plot in normal coordinates
;    THICK: in, optional
;           thickness of the vectors
;    NOCLIP: in, optional
;            set to 0 to clip the vectors drawing
;    WINDOW: in, optional
;            draw the vectors in a cgWindow
;            
; :Examples:
; 
;    Very simple::
;        px = [2, 4, 2, 4, 6, 8, 10, 12]
;        py = [2, 2, 4, 4, 3, 3, 3, 3]
;        vx = [1., 2., 0, 0, SQRT(0.5), SQRT(0.5), -SQRT(0.5), -SQRT(0.5)]
;        vy = [0., 0., 1., -2., SQRT(0.5), SQRT(0.5), -SQRT(0.5), -SQRT(0.5)]
;        cgWindow, /WINDOW
;        cgImage, BYTARR(15, 6) + 255, /KEEP_ASPECT_RATIO, /AXES, MARGIN=1, /WINDOW, /SAVE
;        w_velvec, px, py, vx, vy, STDVEL=1., LENGTH=0.04, /WINDOW
;        w_velvec, 0.1, 0.1, 1, 0, STDVEL=1., /NORMAL, LENGTH=0.04, /WINDOW
;        cgText, 0.1, 0.05, '1 m.s!U-1!N', /WINDOW, /NORMAL
;
; MODIFICATION HISTORY:
;       Largely inspired by the IDL Astronomy Library.
;       Adapted to be able to plot anywhere (instead of just /DATA),
;       so that a legend can be drawn afterwards.
;-
pro w_velvec, posx, posy, velx, vely, $
  COLORS=colors, $
  STDVEL=stdvel, $
  LENGTH=length, $
  DATA=data, $
  NORMAL=normal, $
  THICK=thick, $
  NOCLIP=noclip, $
  WINDOW=window  
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc            
;  ON_ERROR, 2
  
  ; Check input   
  SetDefaultValue, normal, 0, /BOOLEAN
  SetDefaultValue, data, ~ KEYWORD_SET(NORMAL), /BOOLEAN
  if N_ELEMENTS(length) eq 0 then ll = 0.08 else ll = length
  SetDefaultValue, lengtharrow, 0.3 ; Length of arrowhead legs relative to vectorlength
  SetDefaultValue, angle, 22.5 ; 1/2 times the angle between the arrowhead legs.
    
  IF n_params() NE 4 THEN Message, WAVE_Std_Message(/NARG)
  if ~ array_processing(velx, vely, posx, posy) then $
     Message, 'All arguments must have the same dimension and size!'
  
  
  nvecs = n_elements(velx)  ; Number of particles.
  
  if N_elements(colors) eq 0 then begin
    colors = replicate(cgcolor('black'), nvecs)
  endif else begin
    nvc = n_elements(colors)
    case nvc of
      1: colors = replicate(colors, nvecs)
      nvecs:
      else: message, 'Vector color array must be same size as velx.'
    endcase
  endelse
  
  ; Prepare plot  
  vel = sqrt(velx^2+vely^2)  ; Total velocity.
  SetDefaultValue, stdvel, max(vel, /NAN)  ; Maximum velocity.
  
  ; if needed, compute length of vectors in /DATA coordinates
  if KEYWORD_SET(data) then begin
    if KEYWORD_SET(WINDOW) then WSet, cgQuery(/Current)
    ll = convert_coord([0., ll], [0., 0.], /NORMAL, /TO_DATA)
    ll = ABS(ll[0,0] - ll[0,1])
  endif
  
  ; Convert velocities.
  vx = ll*velx/stdvel
  vy = ll*vely/stdvel
  vel = ll*temporary(vel)/stdvel
    
  ; End of vector
  x1 = posx+vx 
  y1 = posy+vy
  
  angle = angle*!dtor  ; Convert from degrees to radians.
  sinangle = sin(angle)  ; Need these.
  cosangle = cos(angle)
  
  ; Plot vectors  
  for i=0l, nvecs-1l do begin  ; Loop over particles.
    ; Note that we cannot put the next three lines outside the loop,
    ; because we want the arrow size to be relative to the vector length.
    r = lengtharrow * vel[i]  ; Length of arrow head.
    rsin = r * sinangle
    rcos = r * cosangle
    
    ; Draw basis, arrow leg, same arrow leg, other arrow leg.
    ; One arrow leg is drawn twice, because we need to return to the end
    ; of the vector to draw the other leg.    
    cgPlots,[posx[i],x1[i],x1[i]-(vx[i]*rcos+vy[i]*rsin)/vel[i], $
      x1[i],x1[i]-(vx[i]*rcos-vy[i]*rsin)/vel[i]], $
      [posy[i],y1[i],y1[i]-(vy[i]*rcos-vx[i]*rsin)/vel[i], $
      y1[i],y1[i]-(vy[i]*rcos+vx[i]*rsin)/vel[i]],COLOR=colors[i],$
      NORMAL=normal, DATA=data, ADDCMD=window, THICK=thick, NOCLIP=noclip    
  endfor

end