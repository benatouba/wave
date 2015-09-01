;+
; PURPOSE:
;  Returns the range (max - min) of an array
;
; CATEGORY:
;  utilities
;
; CALLING SEQUENCE:
;  result = range(array)
;
; INPUTS:
;  array: Array to find the range of
;
; OUTPUTS:
;  the range of array
;
; MODIFICATION HISTORY:
;  Written by: Chris Beaumont, Mar 2009
;- 
function w_range, array
compile_opt idl2
on_error, 2

if n_elements(array) eq 0 then message, 'array is not defined'
siz=size(array)

if siz[0] lt 3 then begin
max = max(array, min = min, /nan)
out= max - min
endif

if siz[0] eq 3 then begin
   arr=fltarr(siz[1], siz[2])
  for i=0, siz[1]-1 do begin
    for j=0, siz[2]-1 do begin
      max = max(array, min = min, /nan)
      arr[i,j]=max-min  
    endfor
  endfor
  
  out= arr
endif

return, out
end