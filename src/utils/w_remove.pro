;+
; :Description:
;   Remove indices from an an array, using a list() iternally for this
;   
;   TODO: add more than one arg wike rmove from IDLASTRO
;
; :Params:
;    indices: in, required
;             the indieces to remove from array
;    array: in, required
;           the 1D array to be shortened
;           
; :Keywords:
;    REMOVED: out
;             if wanted, the removed values
;
;
; :Author: Fabien Maussion 2014
;          Last modification: FaM, May 4, 2014
;
;-
function w_remove, indices, array, REMOVED=removed
  
  compile_opt idl2
  on_error, 2
  
  li = list(array, /EXTRACT)
  removed = li.remove(indices)
  return, li.ToArray()
  
end