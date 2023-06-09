;+
; :Description:
;    This procedure computes the local altitudinal gradient for any variable for every value of a 2D or 3D array.
;    
;    For a 10x5 array and a kernel_size of 3 the routine works like follows::
;
;      ***-------    with x: current value for which the altitudinal gradient is computed
;      *x*oooooo-         *: all values that are used for current computation (including x!) - defined by kernel size
;      ***oooooo-         o: values for which an altitudinal gradient will be computed according to kernel size
;      -oooooooo-         -: array boundaries, for which a default gradient is set
;      ----------
;
; :Params:
;    var: in, required, type=2D/3D array
;         the variable array
;    height: in, required, type=2D array
;            the grid point height
;            
; :Keywords:
;    KERNEL_SIZE: in, optional, type=integer, default=9
;                 the edge length of the kernel. Must be an odd number 
;                 (e.g. KERNEL_SIZE=3 equates to a kernel size of 3x3=9 values)
;    DEFAULT_VAL: in, optional, default=0
;                 the default value set where no alt gradient can be computed (boundaries, invalid values).
;                 A reasonable value for temperature would be -0.0098
;    VALID_MASK: in, optional, type=byte array
;                mask of the same size as var, with 0 for values where 
;                no gradient should be computed (e.g. lakes)
;    MEAN: in, optional, type=boolean, default=0
;          compute the gradient with mean values (REGRESS is recommended)
;    REGRESS: in, optional, type=boolean, default=1
;             compute the alt gradient with linear regression (default, recommended)
;    CLIP_MIN: in, optional
;              clip the computed gradient to a minimum value
;    CLIP_MAX: in, optional
;              clip the computed gradient to a maximum value
;    SIG: out, optional
;         set to a named variable to obtain an array of the same size as var, containing significances.
;         If /REGRESS is set, significances are the r2 values of the linear fit. If /MEAN is set, it is
;         the number of available grid points for computation
;
; :History:
;     Written by CoK, 2012.
;-
function w_altitudinal_gradient, var, height, $
    KERNEL_SIZE=kernel_size, $ 
    DEFAULT_VAL=default_val, $
    VALID_MASK=valid_mask, $
    MEAN=mean, $
    REGRESS=regress, $
    CLIP_MIN=clip_min, $
    CLIP_MAX=clip_max, $
    SIG=sig
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  ; Check inputs and set kernel size
  v_check=size(var)
  h_check=size(height)
  
  if N_ELEMENTS(KERNEL_SIZE) eq 0 then kernel_size = 9
  if N_ELEMENTS(DEFAULT_VAL) eq 0 then default_val = 0.
  if (KERNEL_SIZE/2.) eq long(KERNEL_SIZE/2.) then message, 'KERNEL_SIZE must be an odd number of grids (e.g. 3, 5, 7 ...)'
  
  ngrid=long(KERNEL_SIZE/2)
  ncol=N_ELEMENTS(var[*,0,0])
  nrow=N_ELEMENTS(var[0,*,0])
  ntime=N_ELEMENTS(var[0,0,*])
  nk = KERNEL_SIZE^2
    
  ;Loop over all time steps
  if ntime eq 1 then begin
    
    do_mean = KEYWORD_SET(mean)
  
    valid = intarr(ncol,nrow) + 1
    if n_elements(valid_mask) ne 0 then valid = valid_mask
    
    edges = intarr(ncol,nrow)
    edges[ngrid:ncol-1-ngrid, ngrid:nrow-1-ngrid]=1
    
    out_arr= fltarr(ncol,nrow) + default_val
    ptocompute = where(valid ne 0 and edges eq 1, ntocompute)
    
    sig = fltarr(ncol,nrow)
    
    if ntocompute eq 0 then begin
      message, 'I have nothing to compute. Return default value.'
      return, out_arr
    endif
    
    inds = ARRAY_INDICES(edges, ptocompute)
        
    for i=0, ntocompute-1 do begin
    
      m = ptocompute[i]
      curcol=inds[0,i]
      currow=inds[1,i]
      
      subarr_valid = valid[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
      subarr_height = height[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
      
      pv = where(subarr_valid, cntv)
      if cntv lt nk then continue ; how many points per kernel should be valid?
      
      subarr_var = var[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
      
      ;compute alt grad by mean if keyword is set
      if do_mean then begin
        diff_h = subarr_height-height[m]
        p = where(ABS(diff_h) lt 10., cnt)
        if cnt ne 0 then subarr_valid[p] = 0
        out_arr[m] = mean(subarr_var[pv]-var[m])/(diff_h[pv])
        sig[m] = float(cntv)/nk
      endif else begin      
        ;compute alt grad by regress, default setting
        if TOTAL(subarr_height[pv]) ne 0 then begin
          out_arr[m] = regress(subarr_height[pv], subarr_var[pv], CORRELATION=lr_corr)
          sig[m] = lr_corr*lr_corr
        endif else begin
          out_arr[m] = default_val
          sig[m] = 0
        endelse
      endelse            
    endfor
    
  endif else begin
  
    ;call procedure recursively to compute all time steps
    out_arr = fltarr(ncol,nrow,ntime)
    sig = fltarr(ncol,nrow,ntime)
    for t=0, ntime-1 do begin
      out_arr[*,*,t] = w_altitudinal_gradient(var[*,*,t], height, KERNEL_SIZE=kernel_size, DEFAULT_VAL=default_val, $
        VALID_MASK=valid_mask, MEAN=mean, REGRESS=regress, CLIP_MIN=clip_min, CLIP_MAX=clip_max, SIG=tsig)
       sig[*,*,t] = tsig
    endfor
    
  endelse
  
  if (N_ELEMENTS(CLIP_MIN) ne 0) then begin
    p = where(out_arr lt CLIP_MIN, cnt)
    if cnt ne 0 then out_arr[p] = CLIP_MIN
  endif
  if (N_ELEMENTS(CLIP_MAX) ne 0) then begin
    p = where(out_arr gt CLIP_MAX, cnt)
    if cnt ne 0 then out_arr[p] = CLIP_MAX
  endif
  
  return, out_arr
  
end