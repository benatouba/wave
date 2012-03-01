pro w_altitudinal_gradient, var, height, out_arr, KERNEL_SIZE=kernel_size, STD_VAL=std_val, $
    VALID_MASK=valid_mask, MEAN=mean, REGRESS=regress, CLIP_MIN=clip_min,$
    CLIP_MAX=clip_max
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if N_ELEMENTS(KERNEL_SIZE) eq 0 then message, 'KERNEL_SIZE must be set.'
  if N_ELEMENTS(STD_VAL) eq 0 then message, 'STD_VAL must be set.'
  if (KERNEL_SIZE/2.) eq long(KERNEL_SIZE/2.) then message, 'KERNEL_SIZE must be an odd number of grids (e.g. 3, 5, 7 ...)'
  
  ngrid=long(KERNEL_SIZE/2)
  ncol=N_ELEMENTS(height[*,0,0])
  nrow=N_ELEMENTS(height[0,*,0])
  ntime=N_ELEMENTS(height[0,0,*])
  
  lr_arr=bytarr(ncol,nrow,ntime)-1
  if N_ELEMENTS(VALID_MASK) ne 0 then lr_arr[ngrid:ncol-1-ngrid, ngrid:nrow-1-ngrid, *]=VALID_MASK[ngrid:ncol-1-ngrid, ngrid:nrow-1-ngrid, *] else $
    lr_arr[ngrid:ncol-1-ngrid, ngrid:nrow-1-ngrid, *]=1
    
  out_arr=FLTARR(ncol,nrow,ntime)-STD_VAL
  
  for i=0, N_ELEMENTS(height[*,*,0])-1 do begin
  
    inds = ARRAY_INDICES(lr_arr, LINDGEN(N_ELEMENTS(lr_arr)))
    curcol=inds[0,i]
    currow=inds[1,i]
    
    if lr_arr[i] ne 1 then continue
    
    subarr_valid=lr_arr[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid, *]
    subarr_var=var[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid,*]
    subarr_height=height[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid, *]
    
    diff_h = subarr_height-height[i]
    p = where(ABS(diff_h) lt 10.)
    
    subarr_valid[p]=0
    
    vacc=where(subarr_valid ne 0)
    
    subarr_inds = ARRAY_INDICES(subarr_valid, LINDGEN(N_ELEMENTS(subarr_valid)))
    
    
    for k=0, ntime-1 do begin
    
      subtime=subarr_inds[2,vacc]
      st_lp=where(subtime eq k, cntok)
      
      if cntok lt 5 then lr_new=STD_VAL else begin    ; how many points per kernel should be valid? 
      
        if KEYWORD_SET(MEAN) then begin
          lr_subarr=(subarr_var[vacc[st_lp]]-var[i])/(subarr_height[vacc[st_lp]]-height[i])
          lr_new=mean(lr_subarr)
        endif else begin
        
          lr_new=regress(subarr_height[vacc[st_lp]], subarr_var[vacc[st_lp]], CORRELATION=lr_corr)
          r2=lr_corr*lr_corr
          if r2 lt 0.95 then lr_new=STD_VAL           ; check significance, when to overwrite value?
        endelse
        
        if (N_ELEMENTS(CLIP_MIN) ne 0) then if (lr_new lt CLIP_MIN) then lr_new=CLIP_MIN
        if (N_ELEMENTS(CLIP_MAX) ne 0) then if (lr_new gt CLIP_MAX) then lr_new=CLIP_MAX
        
        out_arr[curcol, currow, k]=lr_new
      endelse
    endfor
    
  endfor
  
end