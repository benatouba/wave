;+
; :Description:
;   Wrapper for the the IDL LABEL_REGION function, but returning a list of
;   region indexes instead of an array
;   
;   Furthermore, the border problem is solved by adding a 0 at
;   each side of the input array
;
; :Params:
;    array: in, required, type=boolean
;           the mask to labelise (should be zero and ones)
;
; :Keywords:
;    NREGIONS: out
;              the number of regions found
;    REGIONSIZE: out
;                an array of nregions elements with the number
;                of elements per region
;    ALL_NEIGHBORS: in, optional, type=boolean
;                   Set this keyword to indicate that all adjacent neighbors 
;                   to a given pixel should be searched. The default is to 
;                   search only the neighbors that are exactly 
;                   one unit in distance from the current pixel. Irrelevant for
;                   1D arrays.
;                  
; :Returns:
;    a list() with nregions elements containing the indices of each region
;    
; :Author: Fabien Maussion 2014
;          Last modification: FaM, May 18, 2014
;
;-
function w_label_region, array, $
  NREGIONS=nregions, $
  REGIONSIZE=regionsize, $
  ALL_NEIGHBORS=all_neighbors

  ; Set Up environnement
  compile_opt idl2
  @WAVE.inc
  on_error, 2

  dim = size(array, /DIMENSIONS)
  nd = n_elements(dim)

  tmp = lonarr(dim+2)
  case nd of
    1: tmp[1] = array
    2: tmp[1,1] = array
    else: message, 'dims not tested yet'
  endcase
  tmp = label_region(tmp, ALL_NEIGHBORS=all_neighbors)
  case nd of
    1: tmp = tmp[1:-2]
    2: tmp = tmp[1:-2,1:-2]
    else: message, 'dims not tested yet'
  endcase
  
  nregions = max(tmp)
  out = list()
  regionsize = list()
  for i=1, nregions do begin
    out->add, where(tmp eq i, cnt)
    regionsize->add, cnt
  endfor
  
  regionsize = regionsize->ToArray()
  
  return, out
  
end