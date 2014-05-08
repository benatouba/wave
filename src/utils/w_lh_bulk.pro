;+
; :Description:
;   Computes the latent heat flux over a glacier surface using the 
;   aerodynamical bulk formulas.
;   
;   The "over a glacier surface" is important, since the saturation
;   vapor pressure is computed as a function of surface temperature 
;   st and if the surface is melting or es below the melting point
;   (Mölg and Hardy 2004).
;   
;   Some references::
;     
;     Mölg, T. and Hardy, D. (2004): Ablation and associated energy balance of 
;       a horizontal glacier surface on Kilimanjaro, Journal of Geophysical 
;       Research, 109(D16), D16104. doi:10.1029/2003JD004338
;     
;     Wagnon, P., Sicart, J.-E., Berthier, E., & Charazin, J.-P. (2003):
;      Wintertime high-altitude surface energy balance of a Bolivian glacier, 
;      Illimani, 6340 m above sea level. Journal of Geophysical Research, 
;      108(D6), 4177. doi:10.1029/2002JD002088
;
; :Params:
;    ws: in, required
;        wind speed at height Z_WS [m s-1]
;    t: in, required
;       air temperature at height Z_T [K]
;    st: in, required
;        surface temperature [K]
;    vp: in, required
;        vapor pressure [hPa]
; 
; :Returns:
;    The latent heat flux [W m-2]
; 
; :Keywords:
;    Z_M: in, optional, default=2.
;          measurement height [m] of wind speed (array or scalar)
;    Z_HV: in, optional, default=2.
;         measurement height [m] of temperature (array or scalar)
;    Z0_M: in, optional, default=0.002
;          surface roughness length of momentum [m]
;    Z0_HV: in, optional, default=0.002
;           surface roughness length of humidity [m]
;    PHIFUNC: in, optional, default=1.
;             nondimensional stability function (Wagnon et al, 2003)
;
; :Author: Fabien Maussion 2014
;          Last modification: FaM, May 3, 2014
;-
function w_lh_bulk, ws, t, st, vp, $
  Z_M=z_m, $
  Z_HV=z_hv, $
  Z0_M=z0_m, $
  Z0_HV=z0_hv, $
  PHIFUNC=phifunc
  
  compile_opt idl2
  ;on_error, 2
  
  ; Param check
  if n_params() ne 4 then Message, 'Incorrect number of arguments.'
  nt = n_elements(ws)
  
  _z_m = n_elements(z_m) ne 0 ? z_m : 2.
  _z_hv = n_elements(z_hv) ne 0 ? z_hv : 2.
  _z0_m = n_elements(z0_m) ne 0 ? z0_m : 0.002
  _z0_hv = n_elements(z0_hv) ne 0 ? z0_hv : 0.002
  _phifunc = n_elements(phifunc) ne 0 ? phifunc : 1.
  if n_elements(_z_m) eq 1 then _z_m = replicate(_z_m, nt)
  if n_elements(_z_hv) eq 1 then _z_hv = replicate(_z_hv, nt)
  if n_elements(_z0_m) eq 1 then _z0_m = replicate(_z0_m, nt)
  if n_elements(_z0_hv) eq 1 then _z0_hv = replicate(_z0_hv, nt)
  if n_elements(_phifunc) eq 1 then _phifunc = replicate(_phifunc, nt)
  
  ; Constants
  k = 0.40 ; von Karman constant ;TODO: should be 0.41
  lvvap = 2.514e6 ; Latent heat of vaporisation [J kg-1] 
  lvsub = 2.848e6 ; Latent heat of sublimation [J kg-1] 
  rho0 = 1.29   ; Air density at p0 [kg m-3] 
  p0 = 1013.25  ; Reference pressure [hPa] 
  gamma = 0.622 ; psychrometric constant to convert vp in spec hum [kPa K-1]
  eps = 1e-5 ; Rounding errors?
  tmelt = 273.15-eps ; Metling point temp [K]
  
  ; Melting or not?
  pcold = where(st lt tmelt, cntcold)

  ; Latent heat depending on surface conditions.
  lv = fltarr(nt) + lvvap
  if cntcold ne 0 then lv[pcold] =  lvsub[pcold]

  ; Saturation vapor pressure over ice or water? TODO: not used in MBM V1.6 (2010)
  vps = w_vp_water(st)
  if cntcold ne 0 then vps[pcold] = w_vp_ice(st[pcold])
  
  return, _phifunc * gamma * lv * ( rho0 / p0 ) * ( k^2 * ws * (vp - vps) ) / $
        ( alog(_z_m/_z0_m) * alog(_z_hv/_z0_hv) )
  
end