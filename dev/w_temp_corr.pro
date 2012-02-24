function w_temp_corr, temp, height, KERNEL_SIZE=kernel_size

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if N_ELEMENTS(KERNEL_SIZE) eq 0 then message, 'KERNEL_SIZE must be set.'
  
  sgrid=sqrt(kernel_size)
  if (sgrid ne long(sgrid)) or (sgrid/2 eq long(sgrid/2)) then message, 'KERNEL_SIZE must be a number of grid cells forming a square with an odd number of grids for each edge length (e.g. 3x3=9, 5x5=25, 7x7=49..)'
  
  ngrid=long(sgrid/2)
  ncol=N_ELEMENTS(Height[*,0])
  nrow=N_ELEMENTS(Height[0,*])
  
  lr_arr=BYTARR(ncol,nrow)
  lr_arr[ngrid:ncol-1-ngrid, ngrid:nrow-1-ngrid]=1
  
  out_arr=FLTARR(ncol,nrow)-0.0098
  
  inds = ARRAY_INDICES(lr_arr, LINDGEN(N_ELEMENTS(lr_arr)))
  
  for i=0, N_ELEMENTS(temp)-1 do begin
    
    if lr_arr[i] ne 1 then continue
    
    curcol=inds[0,i]
    currow=inds[1,i]
    
    subarr_temp=temp[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
    subarr_height=height[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
    
    diff_h = subarr_height-height[i]
    p = where(ABS(diff_h) gt 10, cntok)
    if cntok eq 0 then begin
      lr_mean=-0.0098
    endif else begin    
      lr_subarr=(subarr_temp[p]-temp[i])/(subarr_height[p]-height[i]) ;divides by 0 for [i]...      
      lr_mean=(total(lr_subarr)/(N_ELEMENTS(lr_subarr))) ; LR is temperature decrease with height
    endelse    
    
    if lr_mean lt -0.0098 then lr_mean=-0.0098
    
    out_arr[i]=lr_mean
    
  endfor
  
  return, out_arr
end