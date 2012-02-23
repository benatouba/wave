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
  
  lr_arr=fltarr(ncol,nrow)
  lr_arr[ngrid:ncol-1-ngrid, ngrid:nrow-1-ngrid]=1.
  
  for i=0, N_ELEMENTS(temp)-1 do begin
  
    if lr_arr[i] ne 1. then continue
    
    currow=long(i/ncol)
    curcol=i-currow*ncol
    
    subarr_temp=temp[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
    subarr_height=height[curcol-ngrid:curcol+ngrid, currow-ngrid:currow+ngrid]
    
    lr_subarr=(subarr_temp-temp[i])/(subarr_height-height[i]) ;divides by 0 for [i]...
    
    lr_mean=(total(lr_subarr, /NAN)/(N_ELEMENTS(lr_subarr)-1))*(-1) ; LR is temperature decrease with height
    
    if lr_mean gt 0.0098 then lr_mean=0.0098
    
    lr_arr[i]=lr_mean
    
  endfor
  
  return, lr_arr
end