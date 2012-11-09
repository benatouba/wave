;+
; :Description:
;    Make a Transverse Mercator map anywhere in the world.
;
; :Keywords:
;    CENTER: in, required, default=[13.4, 52.52]
;            Lon-Lat of the center o the map
;    XX: in, required, default=2000000
;        West to East extent of the map (in m) 
;    YY: in, required, default=2000000
;        south to North extent of the map (in m) 
;    XSIZE: in, required, default=600
;           X size (in window pixels) of the graphic map
;    YSIZE: in, required, default=600
;           Y size (in window pixels) of the graphic map
;    GRID_out: out, optional
;              the Mercator grid, if you want it for any reason
;
; :History:
;     Written by FaM, 2012.
;
;-
function w_make_mercamap, $
    CENTER=center, $
    XX=xx, $
    YY=yy, $
    XSIZE=xsize, $
    YSIZE=ysize, $
    GRID_out=grid_out

  if N_ELEMENTS(center) eq 0 then center = [13.4, 52.52]
  if N_ELEMENTS(XX) eq 0 then xx = 2000000.
  if N_ELEMENTS(YY) eq 0 then yy = 2000000.
  
  if N_ELEMENTS(YSIZE) eq 0 and N_ELEMENTS(XSIZE) eq 0 then ysize = 600
  if N_ELEMENTS(DX) eq 0 then dx = 1000
  if N_ELEMENTS(DY) eq 0 then dy = 1000

  GIS_make_datum, ret, wgs, NAME='WGS-84'
  
  
; 3 - Transverse Mercator
;   a, b, lat0, lon0, x0, y0, k0, [datum], name

  str = '3, '
  str += Number_Formatter(wgs.ellipsoid.a) + ', '
  str += Number_Formatter(wgs.ellipsoid.b) + ', '
  str += str_equiv(center[1]) + ', '
  str += str_equiv(center[0]) + ', '
  str += '0, 0, 0.9996, WGS-84, Mercator'
  GIS_make_proj, ret, proj, PARAM=str
  
  GIS_coord_trafo, ret, center[0], center[1], e, n, SRC=wgs, DST=proj
  
  x0 = - xx/2.
  x1 = xx/2.
  y0 = - yy/2.
  y1 = yy/2.
   
  
  grid = OBJ_NEW('w_Grid2D', PROJ=proj, X0=x0, Y0=y0, X1=x1, y1=y1, DX=dx, DY=dy)
  if ~ OBJ_VALID(grid) then Message, 'Grid not ok'
  
  map = OBJ_NEW('w_Map', grid, XSIZE=xsize, YSIZE=ysize)
  if ~ OBJ_VALID(map) then Message, 'map not ok'
  
  if ARG_PRESENT(GRID_OUT) then grid_out = grid else undefine, grid
  
  return, map 
  
  
end