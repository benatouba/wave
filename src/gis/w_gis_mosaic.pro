;+
;    This functions combines one or more grids to make the "biggest possible"
;    grid with it.
;
; :Params:
;    grids: in, required
;           an array of grid objects
;
; :Returns:
;    a W_GRID_2D object, mosaic of the input grids.
;
;
;-
function w_gis_mosaic, grids

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR,2
  
  if N_ELEMENTS(grids) eq 0 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~ OBJ_VALID(grids[0]) then MESSAGE, WAVE_Std_Message('grids', /ARG)
  if ~ OBJ_ISA(grids[0], 'w_Grid2D') then MESSAGE, WAVE_Std_Message('grids', /ARG)
  
  (grids[0])->getProperty, TNT_C=c, META=meta
  x0 = c.x0
  y0 = c.y0
  x1 = c.x1
  y1 = c.y1
  dx = c.dx
  dy = c.dy
  proj = c.proj
  
  for i = 1, N_ELEMENTS(grids)-1 do begin
    if ~ OBJ_VALID(grids[i]) then MESSAGE, WAVE_Std_Message('grids', /ARG)
    if ~ OBJ_ISA(grids[i], 'w_Grid2D') then MESSAGE, WAVE_Std_Message('grids', /ARG)
    (grids[i])->getProperty, TNT_C=c  
    if ~ w_gis_compareProj(proj, c.proj) then MESSAGE, 'Projections do not match.'
    if ABS(dx - c.dx) gt (MACHAR()).eps then MESSAGE, 'Dxs do not match.'
    if ABS(dy - c.dy) gt (MACHAR()).eps then MESSAGE, 'Dys do not match.'
    x0 = min([x0,c.x0]) 
    y0 = max([y0,c.y0])
    x1 = max([x1,c.x1])
    y1 = min([y1,c.y1])    
  endfor
  
  return, OBJ_NEW('w_Grid2D', x0=x0, y0=y0, x1=x1, y1=y1, dx=dx, dy=dy, PROJ=proj, META=meta + ' mosaic')
      
end