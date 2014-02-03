;+
; :Description:
;    Computes the integrated moisture flux (SOLID, LIQUID or VAPOR) for any variable.
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
; :Author: mowglie
;-
function w_wrf_integrate_flux, vid, wrf, time, nt, t0=t0, t1=t1, dims=dims, dimnames=dimnames

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  ; parse the variabl ID to know
  ; which direction and which meteore to compute
  _vid = str_equiv(vid)
  wind = STRMID(_vid, 0, 1)
  flux = STRMID(_vid, 5, 5)
  if flux eq 'LIQUI' then flux = 'LIQUID'
  
  ; Check if ok
  case flux of
    'VAPOR':
    'LIQUID':
    'SOLID':
    else: Message, 'Variable ID not understood or not valid'
  endcase
  
  case wind of
    'U':
    'V':
    'W':
    else: Message, 'Variable ID not understood or not valid'
  endcase
  
  vn = wind + '_' + flux + 'FLUX'
  u = wrf->get_Var(vn, time, nt, t0=t0, t1=t1,  $
        dims = dims, $
        dimnames = dimnames)
        
  if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
          
  ; Set some tolerance level to avoid underflows
  pu = where(ABS(u) lt (machar()).eps, cntu)
  if cntu ne 0 then u[pu] = 0.
 
  z = wrf->get_Var('ZAG', T0=t0, T1=t1)
  nz = (SIZE(z, /DIMENSIONS))[2]
  ; Height of grid point boundaries  
  z = (z[*,*,1:nz-1,*] + z[*,*,0:nz-2,*])/2.
  ; For all levels above Lev 1, dh is eual to the 
  ; height beween boundaries.
  z[*,*,1:nz-2,*] = z[*,*,1:nz-2,*] - z[*,*,0:nz-3,*]
   ; Midpoint approximation - rectangle method  
  return, TOTAL(u[*,*,0:nz-2,*] * z, 3)
  
end