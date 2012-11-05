function w_make_mercamap, $
    CENTER=center, $
    EASTWEST=eastwest, $
    SOUTHNORTH=southnorth, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    GRID_out=grid_out

  if N_ELEMENTS(center) eq 0 then center = [13.4, 52.52]
  if N_ELEMENTS(EASTWEST) eq 0 then eastwest = 2000000.
  if N_ELEMENTS(SOUTHNORTH) eq 0 then southnorth = 2000000.
  
  if N_ELEMENTS(YSIZE) eq 0 and N_ELEMENTS(XSIZE) eq 0 then ysize = 600
  if N_ELEMENTS(DX) eq 0 then dx = 1000
  if N_ELEMENTS(DY) eq 0 then dy = 1000

  GIS_make_datum, ret, wgs, NAME='WGS-84'
  
; 20- Mercator
;   a, b, lat0, lon0, x0, y0, [datum], name

  str = '3, '
  str += Number_Formatter(wgs.ellipsoid.a) + ', '
  str += Number_Formatter(wgs.ellipsoid.b) + ', '
  str += str_equiv(center[1]) + ', '
  str += str_equiv(center[0]) + ', '
  str += '0, 0, 0.9996, WGS-84, Wave mercator'
  GIS_make_proj, ret, proj, PARAM=str
  
  GIS_coord_trafo, ret, center[0], center[1], e, n, SRC=wgs, DST=proj
  
  x0 = - eastwest/2.
  x1 = eastwest/2.
  y0 = - southnorth/2.
  y1 = southnorth/2.
   
  
  grid = OBJ_NEW('w_Grid2D', PROJ=proj, X0=x0, Y0=y0, X1=x1, y1=y1, DX=dx, DY=dy)
  if ~ OBJ_VALID(grid) then Message, 'Grid not ok'
  
  map = OBJ_NEW('w_Map', grid, XSIZE=xsize, YSIZE=ysize)
  if ~ OBJ_VALID(map) then Message, 'map not ok'
  
  if ARG_PRESENT(GRID_OUT) then grid_out = grid else undefine, grid
  
  return, map 
  
  
end