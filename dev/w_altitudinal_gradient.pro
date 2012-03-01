function w_altitudinal_gradient, var, height, KERNEL_SIZE=kernel_size, DEFAULT_VAL=default_val, $
                                   VALID_MASK=valid_mask, MEAN=mean, REGRESS=regress, CLIP_MIN=clip_min,$
                                     CLIP_MAX=clip_max, SIG=sig
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if N_ELEMENTS(KERNEL_SIZE) eq 0 then message, 'KERNEL_SIZE must be set.'
  if N_ELEMENTS(DEFAULT_VAL) eq 0 then message, 'DEFAULT_VAL must be set.'
  if (KERNEL_SIZE/2.) eq long(KERNEL_SIZE/2.) then message, 'KERNEL_SIZE must be an odd number of grids (e.g. 3, 5, 7 ...)'
  
  ngrid=long(KERNEL_SIZE/2)
  ncol=N_ELEMENTS(var[*,0,0])
  nrow=N_ELEMENTS(var[0,*,0])
  ntime=N_ELEMENTS(var[0,0,*])
  nk = KERNEL_SIZE^2
  
  if ntime eq 1 then begin
  
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
      if cntv lt 1 then continue ; how many points per kernel should be valid?
      
      subarr_var = var[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
      
      if KEYWORD_SET(MEAN) then begin
        diff_h = subarr_height-height[m]
        p = where(ABS(diff_h) lt 10., cnt)
        if cnt ne 0 then subarr_valid[p] = 0
        pv = where(subarr_valid, cntv)
        if cntv lt 1 then continue ; how many points per kernel should be valid?
        out_arr[m] = mean(subarr_var[pv]-var[m])/(diff_h[pv])
        sig[m] = float(cntv)/nk
      endif else begin
        out_arr[m] = regress(subarr_height[pv], subarr_var[pv], CORRELATION=lr_corr)
        sig[m] = lr_corr*lr_corr
      endelse
      
    endfor
    
  endif else begin
    out_arr = fltarr(ncol,nrow,ntime)-default_val
    sig = fltarr(ncol,nrow,ntime)
    for t=0, ntime-1 do begin
      out_arr[*,*,t] = w_altitudinal_gradient(var[*,*,t], height, KERNEL_SIZE=kernel_size, DEFAULT_VAL=default_val, $
        VALID_MASK=valid_mask, MEAN=mean, REGRESS=regress, CLIP_MIN=clip_min,$
        CLIP_MAX=clip_max, SIG=_sig)
      sig[*,*,t] = _sig
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