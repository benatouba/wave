;+
; :Description:
;    Simple but nice tool to define WRF domains.
;    
;    Parses the WPS namelist and makes maps and grids for 
;    all the domains defined by the user.
;    
; :Params:
;    namelist: in, required
;              the path to the wps namelist
;
; :Keywords:
;    GRIDS: out
;           an array of grid objects
;    MAPS: out
;          an array of map objects
;    DOPLOT: in, optional
;            if you want to show the generated maps
;            
; :Author: FaM, 2012
;-
pro w_geogrid_simulator, namelist, GRIDS=grids, MAPS=maps, DOPLOT=doplot

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  openr, lun, namelist, /GET_LUN
  l = ''

  while ~EOF(lun) do begin
    readf, lun, l
    s = STRSPLIT(l, '=', /EXTRACT, COUNT=nn)
    if nn le 1 then continue
    s0 = str_equiv(s[0])
    s1 = STRSPLIT(str_equiv(s[1]), ',', /EXTRACT)
    if s0 eq 'PARENT_ID' then parent_id = long(s1)
    if s0 eq 'PARENT_GRID_RATIO' then parent_grid_ratio = long(s1)
    if s0 eq 'I_PARENT_START' then i_parent_start = long(s1)
    if s0 eq 'J_PARENT_START' then j_parent_start = long(s1)
    if s0 eq 'E_WE' then e_we = long(s1)
    if s0 eq 'E_SN' then e_sn = long(s1)
    if s0 eq 'DX' then dx = float(s1[0])
    if s0 eq 'DY' then dy = float(s1[0])
    
    if s0 eq 'MAP_PROJ' then map_proj = utils_replace_string(s1[0], "'", '')
    
    if s0 eq 'REF_LAT' then ref_lat = float(s1[0])
    if s0 eq 'REF_LON' then ref_lon = float(s1[0])
    if s0 eq 'TRUELAT1' then truelat1 = float(s1[0])
    if s0 eq 'TRUELAT2' then truelat2 = float(s1[0])
    if s0 eq 'STAND_LON' then stand_lon = float(s1[0])
  endwhile
  free_lun, lun
  
  GIS_make_ellipsoid, ret, ell, NAME='WRF Sphere', RA=6370000.0, RB=6370000.0
  case map_proj of
    'LAMBERT': begin
      ; 4 - Lambert Conformal Conic
      ;   a, b, lat0, lon0, x0, y0, sp1, sp2, [datum], name
      envi_proj = 4
      proj_param = str_equiv(envi_proj) + ', ' + $            ;proj_id
        STRING(ell.a, FORMAT='(F16.8)') + ', ' + $            ;a
        STRING(ell.b, FORMAT='(F16.8)') + ', ' + $            ;b
        STRING(ref_lat, FORMAT='(F16.8)') + ', ' + $     ;lat0
        STRING(stand_lon, FORMAT='(F16.8)') + ', ' + $        ;lon0
        '0.0' + ', ' + $                                      ;x0
        '0.0' + ', ' + $                                      ;y0
        STRING(truelat1, FORMAT='(F16.8)') + ', ' + $         ;sp1
        STRING(truelat2, FORMAT='(F16.8)') + ', ' + $         ;sp2
        'WGS-84' + ', ' + $                                   ;datum
        'WRF Lambert Conformal'                               ;name
    end
    'MERCATOR': begin
      ; 20- Mercator
      ;   a, b, lat0, lon0, x0, y0, [datum], name
      envi_proj = 20
      proj_param = str_equiv(envi_proj) + ', ' + $            ;proj_id
        STRING(ell.a, FORMAT='(F16.8)') + ', ' + $            ;a
        STRING(ell.b, FORMAT='(F16.8)') + ', ' + $            ;b
        STRING(truelat1, FORMAT='(F16.8)') + ', ' + $       ;lat0
        STRING(ref_lon, FORMAT='(F16.8)') + ', ' + $        ;lon0
        '0.0' + ', ' + $                                      ;x0
        '0.0' + ', ' + $                                      ;y0
        'WGS-84' + ', ' + $                                   ;datum
        'WRF Mercator'                               ;name
    end
    else: Message, 'Projection not recognized: ' + map_proj
  endcase

  ; Make the projection
  GIS_make_proj, ret, proj, PARAM=proj_param
  
  ; Get easting and northings from dom center
  GIS_coord_trafo, ret, ref_lon, ref_lat, e, n, SRC=proj.datum, DST=proj
  nx = e_we[0]-1
  ny = e_sn[0]-1
  x0 =  - (nx-1) / 2. * dx + e ; UL corner
  y0 =    (ny-1) / 2. * dy + n ; UL corner
  
  grid1 = OBJ_NEW('w_Grid2D', nx = nx , $
    ny = ny                , $
    dx = dx                , $
    dy = dy                , $
    x0 = x0                , $
    y0 = y0                , $
    proj = proj)
    
  ndoms = N_ELEMENTS(parent_id)
  g = OBJARR(ndoms)
  g[0] = grid1
  
  for i = 1, ndoms-1 do begin
    ips = i_parent_start[i] - 1
    jps = j_parent_start[i] - 1
    pid = parent_id[i]
    we = e_we[i] - 1
    sn = e_sn[i] - 1
    ratio = parent_grid_ratio[i]
    
    nx = we/ratio
    ny = sn/ratio
    
    if nx ne we/FLOAT(ratio) then Message, 'for dom ' + str_equiv(i+1) + 'e_we and ratios are incompatible: (e_we - 1) / ratio must be integer!' 
    if ny ne sn/FLOAT(ratio) then Message, 'for dom ' + str_equiv(i+1) + 'e_sn and ratios are incompatible: (e_sn - 1) / ratio must be integer!' 
    
    pgrid = g[pid-1]
    pgrid->get_XY, xx, yy, nnx, nny
    
    mask = LONARR(nnx, nny)
    mask[ips:ips+nx-1, jps:jps+ny-1] = 1
    
    ok = pgrid->set_ROI(MASK=mask)
    if ~ ok then Message, 'Problem in dom ' + str_equiv(i+1)
    tmpgrid = pgrid->reGrid(/TO_ROI)
    ok = pgrid->set_ROI()    
    ngrid = tmpgrid->reGrid(FACTOR=ratio)
    undefine, tmpgrid    
    g[i] = ngrid    
  endfor
  
  m = OBJARR(ndoms)
  for i = 0, ndoms-1 do begin
    map = OBJ_NEW('w_Map', g[i], YSIZE=700)
    ok = map->set_shape_file(/LAKES)
    ok = map->set_topography(/DEFAULT, Z=z)
    w_LoadCT, 'wiki-schwarzwald-cont', TABLE_SIZE=ts
    ok = map->set_plot_params(N_L=127, CMAX=ts-1)
    ok = map->set_data(z)
    for j=i+1, ndoms-1 do begin
      (g[j])->Get_XY, x, y, nx, ny
      x = [0,nx-1,nx-1,0,0]
      y = [0,0,ny-1,ny-1,0]
      k = map->set_polygon(x, y, SRC=(g[j]), THICK=(ndoms-j)*1.5)
    endfor
    m[i] = map
    if KEYWORD_SET(DOPLOT) then w_standard_2d_plot, map, TITLE='Domain ' + str_equiv(i+1)
  endfor
       
  if ARG_PRESENT(grids) then grids = g else undefine, g
  if ARG_PRESENT(maps) then maps = m else undefine, m
     
end