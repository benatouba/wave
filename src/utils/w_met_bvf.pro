;+
; :Description:
;    Computes the Brunt-Väisälä frequency for a given profile and 
;    produces a stability diagram if requested.
;
; :Params:
;    theta: in, required
;           array of potential temperature values
;    pressure: in, required
;              array of pressure values
;    height: in, required
;             array of height values
;
; :Keywords:
;    DO_PLOT: in, optional, default =0,
;             Set this keyword to produce a stability profile plot
; 
; :Returns: 
;    an array of nz-1 elements 
; 
; :Author: Hinners
;-
function w_met_bvf, theta, pressure, height, DO_PLOT=do_plot


  ;variables
  pT = theta
  p = pressure
  z = height
  nz = N_elements(pT)
   
  d_pT = pT[1:*] - pT[0:nz-2]
  d_z = z[1:*] - z[0:nz-2]
  
  ;Brunt-Väisälä frequency    
  g = 9.81  
  pT=pT[1:*]
  BVF = sqrt( (g * d_pT)  / (pT  * d_z)  )
  
  ; plot
  if KEYWORD_SET(DO_PLOT) then begin
    p = p[0:nz-2]
    cgplot, BVF,  p,  YRANGE=[max(p), min(p)], Title='stability diagram !C', position=[0.12, 0.12, 0.9, 0.85],$
      xtitle=ansi_value('Brunt-Väisälä-frequency [1/s]'), ytitle='pressure [hPa]', /WINDOW
  endif
  
  return, BVF
  
end