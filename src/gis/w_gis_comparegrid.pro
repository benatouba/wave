;+
; :Description:
;    Compares two w_grid_2d objects
;
; :Params:
;    grid1: in, required
;           the first grid
;    grid2: in, required
;           the second grid
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the grids are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function w_gis_compareGrid, grid1, grid2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
    
  if ~ OBJ_VALID(grid1) then return, 0
  if ~ OBJ_ISA(grid1, 'w_Grid2D') then  return, 0
  if ~ OBJ_VALID(grid2) then return, 0
  if ~ OBJ_ISA(grid2, 'w_Grid2D') then  return, 0
  
  grid1->GetProperty, TNT_C=c1
  grid2->GetProperty, TNT_C=c2
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR()).eps
  
  if ABS(c1.nx - c2.nx) gt 0 then return, 0
  if ABS(c1.ny - c2.ny) gt 0 then return, 0
  if ABS(c1.dx - c2.dx) gt epsilon then return, 0
  if ABS(c1.dy - c2.dy) gt epsilon then return, 0
  if ABS(c1.x0 - c2.x0) gt epsilon then return, 0
  if ABS(c1.y0 - c2.y0) gt epsilon then return, 0
  if ABS(c1.y1 - c2.y1) gt epsilon then return, 0
  if ABS(c1.x1 - c2.x1) gt epsilon then return, 0
  if ~ w_gis_compareProj(c1.proj, c2.proj, epsilon = epsilon)  then return, 0 
        
  return, 1
    
end