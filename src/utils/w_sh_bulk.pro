;+
; :Description:
;   Computes the sensible heat flux using the aerodynamical bulk formulas.
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
;    p: in, required
;       air pressure [hPa]
;    ws: in, required
;        wind speed at height Z_WS [m s-1]
;    t: in, required
;       air temperature at height Z_T [K]
;    st: in, required
;        surface temperature [K]
;
; :Returns:
;    The sensible heat flux [W m-2]
;
; :Keywords:
;    Z_M: in, optional, default=2.
;          measurement height [m] of wind speed (array or scalar)
;    Z_HV: in, optional, default=2.
;         measurement height [m] of temperature (array or scalar)
;    Z0_M: in, optional, default=0.002
;          surface roughness length of momentum [m]
;    Z0_HV: in, optional, default=0.002
;           surface roughness length of temperature [m]
;    PHIFUNC: in, optional, default=1.
;             nondimensional stability function (Wagnon et al, 2003)
;
; :Author: Fabien Maussion 2014
;          Last modification: FaM, May 3, 2014
;-
function w_sh_bulk, p, ws, t, st, $
  Z_M=z_m, $
  Z_HV=z_hv, $
  Z0_M=z0_m, $
  Z0_HV=z0_hv, $
  PHIFUNC=phifunc

  ; Param check
  if n_params() ne 4 then message, 'Incorrect number of arguments.'
  nt = n_elements(p)

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
  cp = 1004. ; Specific heat at constant pressure of dry air [K kg-1 K-1]
  k = 0.40 ; von Karman constant ;TODO: should be 0.41
  rho0 = 1.29   ; Air density at p0 [kg m-3]
  p0 = 1013.25  ; Reference pressure [hPa]

  return, _phifunc * cp * rho0 * (p / p0) * k^2 * ws * (t - st) / $
    ( alog(_z_m/_z0_m) * alog(_z_hv/_z0_hv) )

end