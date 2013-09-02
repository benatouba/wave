;+
; :Description:
;    Computes the moisture flux (SOLID, LIQUID or VAPOR) for any variable.
;    !! This function is meant to be called by wrf->get_Var() ONLY!!!
;    
; :Private:
;
; :Params:
;    vid: in
;         variable ID
;    wrf: in
;         the wrf object
;    time: out 
;          the time
;    nt: out 
;        N time
;
; :Keywords:
;    t0
;    t1
;    dims
;    dimnames
;
;-
function w_wrf_moisture_flux, vid, wrf, time, nt, T0=t0, T1=t1, DIMS=dims, DIMNAMES=dimnames
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  ; parse the variabl ID to know
  ; which direction and which meteore to compute
  _vid = str_equiv(vid)  
  wind = STRMID(_vid, 0, 1)
  flux = STRMID(_vid, 2, 5)
  if flux eq 'LIQUI' then flux = 'LIQUID' ;because liquid is on letter more
  
  ; Check if ok
  case flux of
    'VAPOR':
    'LIQUID':
    'SOLID':
    else: Message, 'Variable ID not understood or not valid'
  endcase  
  flux = 'Q' + flux  
  
  case wind of
    'U':
    'V':
    'W':
    else: Message, 'Variable ID not understood or not valid'
  endcase
  
  wind = wrf->get_Var(wind, time, nt, t0=t0, t1=t1,  $
    dims = dims, $
    dimnames = dimnames, /UNSTAGG)
  ; Set some tolerance level to avoid underflows
  pu = where(ABS(wind) lt (machar()).eps, cntu)
  if cntu ne 0 then wind[pu] = 0.
  
  q = wrf->get_Var(flux, T0=t0, T1=t1)
  ; Set some tolerance level to avoid underflows
  pu = where(q lt (machar()).eps, cntu)
  if cntu ne 0 then q[pu] = 0.
  
  t = wrf->get_Var('TK', T0=t0, T1=t1)
  p = wrf->get_Var('P', T0=t0, T1=t1) + wrf->get_Var('PB', T0=t0, T1=t1)
  
  ; Go for specific humidity
  q = q / (q + 1.)
  ; rho: air density
  rho = p / (287.058 * t)   ; 287.058 Dry air gaz constant
 
  return, rho * q * wind
  
end