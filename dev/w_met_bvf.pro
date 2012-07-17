
;+
; :Description:
;    This procedure creates a stability diagram based on the Brunt-Väisälä frequency.
;
; :Params:
;    theta = in, required, array of potential temperature values
;    pressure = in, required, array of pressure values
;    height = in, required, arrays of height values
;
; :Keywords:
;    STD_PNG = in, optional, default =0,
;              Set this keyword to save the figure as a standard png in the output directory
;    OUTPUT_DIR = in optional, default = 0,
;                 Set this keyword to determine an output directory.
;                 If this keyword is not set although the std_png keyword is set, a window opens to choose an output directory
;
; :Author: Hinners
;-
pro w_met_bvf, theta, pressure, height, STD_PNG=std_png, OUTPUT_DIR=output_dir


  ; choose output directory if no keyword is set
  if N_Elements(STD_PNG) ne 0 then $
      if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)

  ;variables
  pT = theta
  p = pressure
  z = height
  d_pT = fltarr(N_elements(pT)-1)
  d_z  = fltarr(N_elements(z)-1)
  for i = 0,(N_Elements(pT)-2) do begin
    d_pT[i] = pT[i+1] - pT[i]
    d_z[i] = z[i+1] - z[i]
  endfor 

  
  ;Brunt-Väisälä frequency    
  g = 9.81  
  pT=pT[1:*]
  p = p[1:*]
  BVF = sqrt( (g * d_pT)  / (pT  * d_z)  )

  ; plot
  prange = [ max(p), min(p) ]  
  cgplot, BVF,  p,  yrange=prange,   Title='stability diagram !C', position=[0.12, 0.12, 0.9, 0.85],$
  xtitle=ansi_value('Brunt-Väisälä-frequency [1/s]'), ytitle='pressure [hPa]', /WINDOW
 
  ; save figure
  if N_ELEMENTS(std_png) ne 0 then begin
    pngname='Stability_diagram'
    STD_PNG=output_dir+pngname+'.png'  
    cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif


end