PRO PLOT_MAP__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  struct = {MAP_PARAMS        , $
            type: ''          , $
            c_intervals : 0D  , $
            c_thick : 0D      , $
            c_style : 0D        $
            }
    
  struct = {MAP_SHAPE         , $
            thick : 0D        , $
            style : 0D        , $
            color : ''        , $
            n_coord: 0L       , $
            coords : PTR_NEW(), $
            conn :  PTR_NEW() $            
            }
    
  struct = { PLOT_MAP                   ,  $
             grid:  OBJ_NEW()           ,  $
             Xsize:  0L                 ,  $
             Ysize:  0L                 ,  $
             XYratio:  0D               ,  $
             img :   PTR_NEW()          ,  $
             slope:  PTR_NEW()          ,  $
             shapes: PTR_NEW()          ,  $             
             nshapes: 0L                ,  $             
             colors: PTR_NEW()          ,  $  
             ncolors : 0L               ,  $           
             map_params: {MAP_PARAMS}   ,  $
             relief_factor : 0D         ,  $             
             is_Shaped : FALSE          ,  $
             is_Shaded : FALSE          ,  $
             is_Mapped : FALSE             $             
             }
    
END

Function PLOT_MAP::Init, grid, Xsize = Xsize,  Ysize = Ysize, FACTOR = factor
     
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if not OBJ_ISA(grid, 'Grid2D')  then Message, WAVE_Std_Message('proj', OBJ='Grid2D')
  
  self.grid = grid->reGrid(Xsize = Xsize,  Ysize = Ysize, FACTOR = factor) 
  self.grid->getProperty, tnt_C = c
  self.Xsize = c.nx
  self.Ysize = c.ny
  self.XYratio= DOUBLE(c.nx) / c.ny
  
  dummy = self->set_img()
  dummy = self->set_map_params()  
  dummy = self->set_shading_params(RELIEF_FACTOR = 2.)  
                
  RETURN, 1
  
END

function PLOT_MAP::DestroyShapes

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  if PTR_VALID(self.shapes) then begin  
    shapes = *self.shapes
    for i = 0, N_ELEMENTS(shapes) - 1 do begin
      ptr_free, (shapes[i]).coords
      ptr_free, (shapes[i]).conn
    endfor
  endif
  ptr_free, self.shapes
  self.nshapes = 0L
  self.is_Shaped = FALSE
  
  return, 1
  
end

pro PLOT_MAP::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  OBJ_DESTROY, self.grid
  
  ok = self->DestroyShapes()
  
  PTR_FREE, self.img 
  PTR_FREE, self.slope    
  PTR_FREE, self.colors
        
  
END

PRO PLOT_MAP::GetProperty, XSIZE = xsize, YSIZE = ysize 
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if ARG_PRESENT(XSIZE) then xsize = self.Xsize
  if ARG_PRESENT(YSIZE) then ysize = self.Ysize
      
end

PRO PLOT_MAP::SetProperty

    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
      
end

function PLOT_MAP::set_Colors, COLORS = colors, NCOLORS = ncolors, CMIN=cmin, CMAX=cmax, INVERTCOLORS = invertcolors
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.colors
    ok = self->set_Colors()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  if N_ELEMENTS(colors) eq 0 and N_ELEMENTS(ncolors) eq 0 then begin
    self.colors = PTR_NEW(utils_color_convert())
    self.ncolors = 1
    return, 1
  endif
      
  _colors = utils_color_convert(COLORS = colors, NCOLORS = ncolors, CMIN=cmin, CMAX=cmax, INVERTCOLORS = invertcolors)
   
  self.colors = PTR_NEW(_colors)
  self.ncolors = ncolors
  
  return, 1

end

function PLOT_MAP::set_img, img
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.img
    ok = self->set_img()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if N_PARAMS() eq 0 then img = BYTARR(self.Xsize, self.Ysize)   
    
  if ~ arg_okay(img, DIM=[self.Xsize, self.Ysize], /NUMERIC) then Message, WAVE_Std_Message('img', /ARG)
  
  if self.ncolors gt 1 then SELF.img = PTR_NEW(round(Scale_Vector(img, 0, self.ncolors-1))) else SELF.img = PTR_NEW(img * 0)    
  
  return, 1

end

function PLOT_MAP::set_topography, GRDFILE = grdfile
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.slope
    self.is_Shaded = false
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(grdfile) then begin
    PTR_FREE, self.slope
    self.is_Shaded = false
    return, 1
  end
  
  if N_ELEMENTS(grdfile) eq 0 then grdfile = DIALOG_PICKFILE(TITLE='Please select .grd file to read', /MUST_EXIST)
  if GRDFILE eq '' then begin
    PTR_FREE, self.slope
    self.is_Shaded = false
    return, 1
  end
  
  spli = STRSPLIT(grdfile, '.', /EXTRACT)
  if str_equiv(spli[1]) ne 'GRD' then message, WAVE_Std_Message(/FILE)
  hdr = spli[0] + '.hdr'
  
  self.grid->get_Lonlat, lon, lat ; TODO: change this into GRID kind of things
  
  ; Open DEM grid
  !QUIET = 1
  GIS_open_grid, ret, info, id, FILE=hdr, /RONLY, /NO_STC
  !QUIET = 0
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message(/FILE)
  
  lat0 = info.coord.y0
  lon0 = info.coord.x0
  dlat = info.coord.dy
  dlon = info.coord.dx
  nlon = info.coord.nx
  nlat = info.coord.ny
  
  ilat = round((lat0-lat[*])/dlat)
  ilon = round((lon[*]-lon0)/dlon)
  rmin = min(ilat)
  rmax = max(ilat)

  topo = intarr(nlon,rmax-rmin+1)

  openr, lun, grdfile, /GET
  point_lun, lun, 2*rmin*nlon  
  readu, lun, topo
  free_lun, lun

  z = topo[ilon,ilat-rmin]

  p = where(z eq -9999, cnt)
  if cnt gt 0 then begin 
    z[p] = 0
    MESSAGE, 'Topography is not complete, setting some points to 0.', /INFORMATIONAL
  endif
  z = DOUBLE(reform(z, n_elements(lat[*,0]), n_elements(lat[0,*])))
  GIS_xy_derivatives, ret, z, DFDX=dhdx,DFDY=dhdy
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message('Error by calculating derivatives.')
  dhdx = 1 - Scale_Vector(dhdx , 0.0,  1.0)
  dhdy = 1 - Scale_Vector(dhdy , 0.0,  1.0)

  self.slope = PTR_NEW(dhdx - dhdy)
  self.is_Shaded = TRUE
 
  return, 1

end

function PLOT_MAP::set_shape_file, SHPFILE = shpfile, SHP_SRC = shp_src, COUNTRIES = countries

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = self->DestroyShapes()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if KEYWORD_SET(COUNTRIES) then return, self->set_shape_file(SHPFILE = WAVE_resource_dir+'/shapes/world_borders/world_borders.shp')
    
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(shpfile) then return, self->DestroyShapes()
  
  if N_ELEMENTS(shpfile) eq 0 then shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )
  if shpfile eq '' then return, self->DestroyShapes()
  
  if ~KEYWORD_SET(shp_src) then GIS_make_datum, ret, shp_src, NAME = 'WGS-84'
  if arg_okay(shp_src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(shp_src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('shp_src', /ARG)
  
  ;****************************************
  ; Make boundaries to spare computations *
  ;****************************************
  if is_dat then begin
   self.grid->get_LonLat, glon, glat
   range = [min(glon),max(glon),min(glat),max(glat)]
  end
  if is_proj then begin
   range = [-99999999999d,99999999999d,-99999999999d,99999999999d] ; TODO: this
  end
  
  ; read shp file and create polygon object from entities
  shpmodel = OBJ_NEW('IDLffShape',shpfile)
  if ~ OBJ_VALID(shpmodel) then MESSAGE, WAVE_Std_Message('shpfile', /FILE)
  
  ;Get the number of entities so we can parse through them
  shpModel->GetProperty, N_ENTITIES=N_ent
  
  n_coord = 0L
  for i=0L, N_ent-1 do begin

    ent = shpmodel->GetEntity(i, /ATTRIBUTES)
    
    if not ptr_valid(ent.vertices) then continue
    
    x = reform((*ent.vertices)[0,*])
    y = reform((*ent.vertices)[1,*])
    n_vert = n_elements(x)
    
    if n_vert lt 3 then continue
    if min(y) gt range[3] then continue  
    if max(y) lt range[2] then continue  
    if min(x) gt range[1] then continue  
    if max(x) lt range[0] then continue  
    
    self.grid->transform, x, y, x, y, SRC = shp_src, /NEAREST
    
    if n_elements(coord) eq 0 then coord = [1#x,1#y] else coord = [[coord],[[1#x,1#y]]]

    parts = *ent.parts
    for k=0L, ent.n_parts-1 do begin
      if k eq ent.n_parts-1 then n_vert = ent.n_vertices - parts[k]  else n_vert = parts[k+1]-parts[k]
      polyconn = (lindgen(n_vert)) + n_coord
      if n_elements(conn) eq 0 then begin
        conn = n_vert
        conn = [conn,polyconn]
      endif else begin
        conn = [conn,n_vert]
        conn = [conn,polyconn]
      endelse         
      n_coord += n_vert      
    endfor   
        
    shpmodel->DestroyEntity, ent 

  endfor

  ; clean unused objects
  obj_destroy, shpModel
  
  sh = {MAP_SHAPE}
  sh.color = 'black'
  sh.style = 0
  sh.thick = 1.5
  sh.conn = PTR_NEW(conn, /NO_COPY)
  sh.coords = PTR_NEW(coord, /NO_COPY)
  sh.n_coord = n_coord
  
  if self.nshapes eq 0 then begin
   self.nshapes = 1
   self.shapes = PTR_NEW(sh, /NO_COPY)
  endif else begin
   temp = *self.shapes
   PTR_FREE, self.shapes
   temp = [temp, sh]
   self.shapes = PTR_NEW(temp, /NO_COPY)
   self.nshapes = self.nshapes + 1
  endelse
    
  self.is_Shaped = TRUE
  return, 1
  
end

function PLOT_MAP::set_map_params, TYPE = type, C_INTERVAL = c_interval, C_THICK = c_thick, C_STYLE = c_style
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.img
    self.map_params = {MAP_PARAMS}
    self.is_Mapped = FALSE
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
    
  _type = 'LONLAT'
  _c_interval = 10.
  _c_thick = 1.
  _c_style = 2.
  
  if N_ELEMENTS(TYPE) eq 1 then _type = TYPE
  if N_ELEMENTS(C_INTERVAL) eq 1 then _c_interval = C_INTERVAL
  if N_ELEMENTS(C_THICK) eq 1 then _c_thick = C_THICK
  if N_ELEMENTS(C_STYLE) eq 1 then _c_style = C_STYLE
  
  self.map_params.type = _type
  self.map_params.c_intervals = _c_interval
  self.map_params.c_thick = _c_thick
  self.map_params.c_style = _c_style
                          
  self.is_Mapped = _type ne ''
  return, 1

end

function PLOT_MAP::set_shading_params, RELIEF_FACTOR = relief_factor
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
    
  _relief_factor = 0.  
  if N_ELEMENTS(RELIEF_FACTOR) eq 1 then _relief_factor = RELIEF_FACTOR                           
  
  self.relief_factor = _relief_factor
  
  return, 1

end

function PLOT_MAP::shading

  if self.relief_factor eq 0 then return, self->img_to_rgb()
  if self.ncolors eq 0 or self.ncolors gt 127 then MESSAGE, 'Number of colors should lie between 1 and 127.'
  
  rp = bindgen(256)
  gp = bindgen(256)
  bp = bindgen(256)
  
  utils_color_rgb, *self.colors, s_r, s_g, s_b  
  rp[0:self.ncolors-1] = s_r[*]
  gp[0:self.ncolors-1] = s_g[*]
  bp[0:self.ncolors-1] = s_b[*]
  
  ;******************
  ; Prepare shading *
  ;******************  
  sl = *self.slope
      
  min_sl  = min(sl)
  max_sl  = max(sl)
  mean_sl = moment(sl, SDEV=sdev_sl)
  
  p = where(sl gt 0, cnt)  
  if cnt gt 0 then sl[p] = 0.4*sin(0.5*!pi*(-1>(sl[p]/(2*sdev_sl))<1))
  p = 0  
  level = 1.0 - 0.1*self.relief_factor ; 1.0 for 0% and 0.9 for 100%
  sens  = 0.7*self.relief_factor       ; 0.0 for 0% and 0.7 for 100%
  
  ;****************
  ; Apply shading *
  ;****************  
  r = rp[*self.img]
  g = gp[*self.img]
  b = bp[*self.img]
  
  r = byte(0 > (level*r*(1+sens*sl) < 255))
  g = byte(0 > (level*g*(1+sens*sl) < 255))
  b = byte(0 > (level*b*(1+sens*sl) < 255))
  sl = 0
  
  img = bytarr(3, self.Xsize, self.Ysize)
  img[0,*,*] = r[*,*]
  img[1,*,*] = g[*,*]
  img[2,*,*] = b[*,*]    
  
  return, img
  
end

function PLOT_MAP::img_to_rgb

    utils_color_rgb, *self.colors, s_r, s_g, s_b
    r = byte(0 > s_r[*self.img] < 255)
    g = byte(0 > s_g[*self.img] < 255)
    b = byte(0 > s_b[*self.img] < 255)
    img = bytarr(3, self.Xsize, self.Ysize)
    img[0,*,*] = r[*,*]
    img[1,*,*] = g[*,*]
    img[2,*,*] = b[*,*]
    
    return, img
    
end    
    
function PLOT_MAP::mapping

  self.grid->get_Lonlat, lon, lat  
  
  intval = self.map_params.c_intervals
  thick = self.map_params.c_thick
  
  Nlevels = 360 / intval    
  levels = INDGEN(Nlevels) * intval - 170
  
  p = where(levels le floor(max(Lon)) and levels ge ceil(min(Lon)), cnt)
  if cnt gt 0 then lonlevels = levels[p]
  p = where(levels le floor(max(Lat)) and levels ge ceil(min(Lat)), cnt)
  if cnt gt 0 then latlevels = levels[p]
  
  _lon = ABS(lon)
  _lonlevels = ABS(lonlevels)
  _lonlevels = _lonlevels[UNIQ(_lonlevels, SORT(_lonlevels))]
  
  FSC_Contour, _lon, POSITION = [0,0,self.Xsize,self.Ysize], /DEVICE, /OVERPLOT, XTICKLEN = -0.2, COLOR = 'dark grey', C_LINESTYLE = 2, $
    LEVELS = _lonlevels, C_THICK = thick
  FSC_Contour, lat, POSITION = [0,0,self.Xsize,self.Ysize], /OVERPLOT, /DEVICE, XTICKLEN = -0.2, COLOR = 'dark grey', C_LINESTYLE = 2, $
    LEVELS = latlevels, C_THICK = thick
  
  return, 1
  
end

function PLOT_MAP::shaping
  
  shapes = *(self.shapes)
  
  for i = 0, self.nshapes-1 do begin
    sh = shapes[i]
    conn = *(sh.conn)
    coord = *(sh.coords)
    
    index = 0
    while index lt N_ELEMENTS(conn) do begin    
      nbElperConn = conn[index]      
      idx = conn[index+1:index+nbElperConn]      
      index += nbElperConn + 1       
      _coord = coord[*,idx]      
      x = _coord[0,*]
      y = _coord[1,*]            
      plots, X , Y , /DEVICE,  Color=FSC_Color(sh.color), THICK=sh.thick, LINESTYLE=sh.style      
    endwhile  
  endfor
  
  return, 1
  
end

pro PLOT_MAP::show_img
     
  window, XSIZE=self.Xsize, YSIZE=self.Ysize, Title='Map Plots', PIXMAP = pixmap, /FREE
  win = !D.WINDOW
  
  if self.is_Shaded then begin
    img = self->shading()
  endif else begin
    img = self->img_to_rgb()
  endelse
  dummy = bytarr(self.Xsize, self.Ysize)
  dummy[0] = 1
  if self.is_Mapped then  FSC_Contour, dummy, POSITION = [0,0,self.Xsize,self.Ysize], /DEVICE, COLOR = FSC_Color('white')
  
  pp = !ORDER
  !ORDER = 0
  tv, img, true = 1
  !ORDER = pp
      
  if self.is_Shaped then ok = self->shaping() 
  if self.is_Mapped then ok = self->mapping() 
  
end