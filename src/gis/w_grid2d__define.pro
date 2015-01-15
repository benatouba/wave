; docformat = 'rst'

;+
; 
; w_Grid2D is a central class in the WAVE library. All geolocalised
; gridded dataset inherit w_Grid2D and can use the w_Grid2D routines. 
;       
; The transformation computations are performed by the TNT GIS library 
; and the IDL map_proj engine. The w_Grid2D object simply provides 
; an object oriented encapsulation of the GIS routines, as well as 
; several improvements, such as remapping, interpolation, 
; irregular to regular grid transformations, etc.
;       
; Currently, only POINT valid grids are supported. This should be sufficient
; for most applications.
;          
; Careful!
; --------
;  
; Althought w_Grid2D is defined using UL and DR corners to be consistent
; with the GIS library, the convention used in the WAVE is based on the 
; WRF convention, that means: the (i,j) = (0,0) index refers to the 
; DL corner and the (i,j) = (nx-1,ny-1) index refers to the UR corner.
; 
; It is not recommended to work directly with {TNT_COORD} structures 
; anymore, but to define w_Grid2D objects using your {TNT_COORD} structures
; as parameter. Probably, your data will be uspide down with respect to 
; the WAVE conventions. One good method to test your gridded data is
; to use `w_QuickPlot`. If your image is upside down,the array have to be 
; rotated like this: array = rotate(array, 7) 
;        
; Here is a non-exhaustive list of dataset conventions::       
;   DATASET                    To ROTATE
;   -------------------------------------
;   WRF                        NO
;   TRMM                       NO
;   MODIS                      YES
;   AMSR                       YES
;   ArcLakeMask                YES
;   ======================================
; 
; :Properties: 
;    nx: in, optional, type = integer
;        number of elements in x-direction
;    ny: in, optional, type = integer
;        number of elements in y-direction
;    x0: in, optional, type = float
;        most western (most left) x-coordinate
;    y0: in, optional, type = float
;        most northern (most upper) y-coordinate
;    x1: in, optional, type = float
;        most eastern (most right) x-coordinate
;    y1: in, optional, type = float
;        most southern (most lower) y-coordinate
;    dx: in, optional, type = float
;        resolution in x-direction
;    dy: in, optional, type = float
;        resolution in y-direction
;    tnt_c: in, optional, type = {TNT_COORD}
;        TNT_GIS {TNT_COORD} struct. If set, all previous keywords are ignored.
;    proj: in, optional, type = {TNT_PROJ}
;        TNT_GIS {TNT_PROJ} struct. MUST be SET.
;    meta: in, optional, type = string
;         a string containing any kind of info
;    lat: out, optional, type = float array
;         the latitudes of each grid point (place holder, computed only at the first user request)
;    lon: out, optional, type = float array
;         the longitudes of each grid point (place holder, computed only at the first user request)
;       
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Written by FaM, 2010.
;-


;+
; :Description:
; 
;   Defines the attributes of the class w_Grid2D. Attributes::
;   
;       lon   : PTR_NEW()   
;               2D array containing the longitudes of the grid
;       lat   : PTR_NEW()   
;               2D array containing the latitudes of the grid
;       tnt_c : {TNT_COORD} 
;               intern {TNT_COORD} structure 
;       meta  : ''          
;               If set, a string containg infos about the grid.
;   
; :Categories:
;         WAVE/OBJ_GIS   
;
; :History:
;     Written by FaM, 2010.
;-
PRO w_Grid2D__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { w_Grid2D     ,  $
    lon    : PTR_NEW()    ,  $ ; 2D array containing the longitudes of the grid
    lat    : PTR_NEW()    ,  $ ; 2D array containing the latitudes of the grid
    roi    : PTR_NEW()    ,  $ ; 2D array containing the ROI mask
    is_roi : FALSE        ,  $ ; if a ROI has bee defined
    tnt_c  : {TNT_COORD}  ,  $ ; intern {TNT_COORD} structure 
    meta   : ''              $ ; If set, a string containg infos about the grid.
    }  
END


;+
; :Description:
;       This function is used to renitialize the grid object entirely. It is called e.g. by the init routine, 
;       and should be called possibly by inherited objects but not externally. 
;       Output: 1 if the w_Grid2D object is updated successfully, 0 if not
;       
; :Private: 
;            
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;    nx: in, optional, type = integer
;        number of elements in x-direction
;    ny: in, optional, type = integer
;        number of elements in y-direction
;    x0: in, optional, type = float
;        most western (most left) x-coordinate
;    y0: in, optional, type = float
;        most northern (most upper) y-coordinate
;    x1: in, optional, type = float
;        most eastern (most right) x-coordinate
;    y1: in, optional, type = float
;        most southern (most lower) y-coordinate
;    dx: in, optional, type = float
;        resolution in x-direction
;    dy: in, optional, type = float
;        resolution in y-direction
;    tnt_c: in, optional, type = {TNT_COORD}
;        TNT_GIS {TNT_COORD} struct. If set, all previous keywords are ignored.
;    proj: in, required, type = {TNT_PROJ}
;        TNT_GIS {TNT_PROJ} struct. MUST be SET.
;    grid: in, optional, type = w_Grid2D
;          if set, all other keywords are ignored and
;          the re-initialized grid will be a copy of the input grid 
;    meta: in, optional, type = string
;         a string containing any kind of info
;
; :Returns:
;    1 if the NCDF object is updated successfully, 0 if not
;
; :History:
;      Written by FaM, 2010.
;-
Function w_Grid2D::ReInit ,  $
    nx = nx           ,  $
    ny = ny           ,  $
    x0 = x0           ,  $
    y0 = y0           ,  $   
    x1 = x1           ,  $
    y1 = y1           ,  $   
    dx = dx           ,  $
    dy = dy           ,  $   
    proj = proj       ,  $ 
    tnt_c = tnt_c     ,  $     
    grid = grid       ,  $     
    meta = meta       

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont update the object. Returning... ')
    RETURN, 0
  ENDIF
  
  cas = -1
  
  ; Go threw the keywords ... 
  if KEYWORD_SET(nx) and KEYWORD_SET(ny) and N_ELEMENTS(x0) eq 1 and  N_ELEMENTS(y0)  eq 1 $
    and N_ELEMENTS(dx) eq 1 and N_ELEMENTS(dy) eq 1 then cas = 1
 
  if KEYWORD_SET(nx) and KEYWORD_SET(ny) and N_ELEMENTS(x1) eq 1 and  N_ELEMENTS(y1)  eq 1 $
    and N_ELEMENTS(dx) eq 1 and N_ELEMENTS(dy) eq 1 then cas = 2
  
  if N_ELEMENTS(x0) eq 1 and  N_ELEMENTS(y0)  eq 1 and N_ELEMENTS(x1) eq 1 and  N_ELEMENTS(y1)  eq 1  $
    and N_ELEMENTS(dx) eq 1 and N_ELEMENTS(dy) eq 1 then cas = 3
  
  if N_ELEMENTS(x0) eq 1 and  N_ELEMENTS(y0)  eq 1 and N_ELEMENTS(x1) eq 1 and  N_ELEMENTS(y1)  eq 1  $
   and N_ELEMENTS(nx) eq 1 and N_ELEMENTS(ny) eq 1 then cas = 4
  
  if N_ELEMENTS(tnt_c) eq 1 then cas = 5
  
  if N_ELEMENTS(grid) eq 1 then cas = 6  
  
  case (cas) of
    1: begin
      x1 = x0 + (nx - 1) * dx
      y1 = y0 - (ny - 1) * dy      
    end
    2: begin
      x0 = x1 - (nx - 1) * dx
      y0 = y1 + (ny - 1) * dy 
    end
    3: begin
      nx = (x1 - x0) / DOUBLE(dx) + 1
      ny = (y0 - y1) / DOUBLE(dy) + 1
    end
    4: begin
      dx = (x1 - x0) / DOUBLE(nx-1)
      dy = (y0 - y1) / DOUBLE(ny-1)
    end
    5: begin
      if not arg_okay(tnt_c, STRUCT={TNT_COORD}) then Message, WAVE_Std_Message('tnt_c', STRUCT = {TNT_COORD})
      nx = tnt_c.nx
      ny = tnt_c.ny
      x0 = tnt_c.x0
      y0 = tnt_c.y0
      x1 = tnt_c.x1
      y1 = tnt_c.y1
      dx = tnt_c.dx
      dy = tnt_c.dy
      proj = tnt_c.proj
      _tnt_c = tnt_c      
    end
    6: begin
      ok = (OBJ_VALID(grid)) and OBJ_ISA(grid, 'w_Grid2D')
      if ~ok then message, WAVE_Std_Message('grid', /ARG)
      grid->getProperty, tnt_c=c, meta=meta 
      nx = c.nx
      ny = c.ny
      x0 = c.x0
      y0 = c.y0
      x1 = c.x1
      y1 = c.y1
      dx = c.dx
      dy = c.dy
      proj = c.proj
      _tnt_c = c      
    end
    else: begin
      Message, WAVE_Std_Message(/NARG), /NoName
    end
  endcase
  
  if N_ELEMENTS(proj) eq 0 then proj = GIS_default_proj()
  if N_ELEMENTS(meta) eq 0 then meta = ''
  
  if not arg_okay(proj, STRUCT={TNT_PROJ}) then Message, WAVE_Std_Message('proj', STRUCT = {TNT_PROJ})
  ok = GIS_check_proj(proj,/CORR, ERROR=err)
  if not ok then  Message, 'Proj parameter not ok: no correction could be made: ' + TNT_err_txt(err)
  
  if N_ELEMENTS(_tnt_c) eq 0 then _tnt_c = {TNT_COORD}
  _tnt_c.system = 'PROJ'
  _tnt_c.valid = 'POINT'
  _tnt_c.nx = nx
  _tnt_c.ny = ny
  _tnt_c.x0 = x0
  _tnt_c.y0 = y0
  _tnt_c.x1 = x1
  _tnt_c.y1 = y1
  _tnt_c.dx = dx 
  _tnt_c.dy = dy
  _tnt_c.proj = proj
  
  ok = GIS_check_coord(_tnt_c, /CORR, ERROR=err)
  if not ok then  Message, 'Input grid parameters do not describe a correct grid: no correction could be made: ' + TNT_err_txt(err)
  
  ; Now check for the map info
   
  self.tnt_c = _tnt_c
  self.meta = meta
  
  ; Security check
  lon = -1.
  lat = -1.
  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  self.lon = PTR_NEW(lon, /NO_COPY)
  self.lat = PTR_NEW(lat, /NO_COPY)
  self->destroy_ROI
    
  RETURN, 1
  
END


;+
; :Description:
;       Build function. 
;
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;    nx: in, optional, type = integer
;        number of elements in x-direction
;    ny: in, optional, type = integer
;        number of elements in y-direction
;    x0: in, optional, type = float
;        most western (most left) x-coordinate
;    y0: in, optional, type = float
;        most northern (most upper) y-coordinate
;    x1: in, optional, type = float
;        most eastern (most right) x-coordinate
;    y1: in, optional, type = float
;        most southern (most lower) y-coordinate
;    dx: in, optional, type = float
;        resolution in x-direction
;    dy: in, optional, type = float
;        resolution in y-direction
;    tnt_c: in, optional, type = {TNT_COORD}
;        TNT_GIS {TNT_COORD} struct. If set, all previous keywords are ignored.
;    grid: in, optional, type = w_grid2d
;          a grid to copy. If set, all previous keywords are ignored.
;    proj: in, optional, type = {TNT_PROJ}
;        TNT_GIS {TNT_PROJ} struct. MUST be SET.
;    meta: in, optional, type = string
;         a string containing any kind of info
;       
; :Returns:
;    1 if the NCDF object is updated successfully, 0 if not
;
; :History:
;      Written by FaM, 2010.
;-
Function w_Grid2D::Init ,  $
    nx = nx           ,  $
    ny = ny           ,  $
    x0 = x0           ,  $
    y0 = y0           ,  $   
    x1 = x1           ,  $
    y1 = y1           ,  $   
    dx = dx           ,  $
    dy = dy           ,  $   
    proj = proj       ,  $ 
    tnt_c = tnt_c     ,  $     
    grid = grid       ,  $     
    meta = meta       

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF
  
  if not self->ReInit(   $
    nx = nx           ,  $
    ny = ny           ,  $
    x0 = x0           ,  $
    y0 = y0           ,  $   
    x1 = x1           ,  $
    y1 = y1           ,  $   
    dx = dx           ,  $
    dy = dy           ,  $   
    proj = proj       ,  $ 
    tnt_c = tnt_c     ,  $     
    grid = grid       ,  $     
    meta = meta       ) then return, 0  
    
  RETURN, 1
  
END

;+
; :Description:
;    Empty the roi.
;
; :Categories:
;    WAVE/OBJ_GIS   
;
; :History:
;      Written by FaM, 2010.
;-
pro w_Grid2D::destroy_ROI

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if PTR_VALID(self.roi) then undefine, *self.roi
  Ptr_Free, self.roi
  self.is_ROI = FALSE
  
END

;+
; :Description:
;    Destroy procedure. 
;
; :Categories:
;    WAVE/OBJ_GIS   
;
; :History:
;      Written by FaM, 2010.
;-
pro w_Grid2D::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->destroy_ROI
  if PTR_VALID(self.lon) then undefine, *self.lon
  if PTR_VALID(self.lat) then undefine, *self.lat
  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  
END


;+
; :Description:
;    Get access to some fields.
;
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;       lon: out, type = float array 
;               2D array containing the longitudes of the grid 
;               (the longitudes will be computed at the first call and stored
;               as a pointer: by large grids, this may cause some memory problems)
;       lat: out, type = float array 
;               2D array containing the latitudes of the grid 
;               (the latitudes will be computed at the first call and stored
;               as a pointer: by large grids, this may cause some memory problems)
;       nx: out, type = long
;           the number of grid points in X dimension
;       ny: out, type = long
;           the number of grid points in Y dimension
;       tnt_c: out,  type = {TNT_COORD}
;               grid {TNT_COORD} structure 
;       meta: out, type = string
;               a string containg infos about the grid.
;
; :History:
;      Written by FaM, 2010.
;-
pro w_Grid2D::GetProperty,    lon   =  lon   ,  $ ; 2D array containing the longitudes of the grid 
                              lat   =  lat   ,  $ ; 2D array containing the latitudes of the grid 
                              nx    =  nx    ,  $ 
                              ny    =  ny    ,  $
                              tnt_c =  tnt_c ,  $ ; {TNT_COORD} structure 
                              meta  =  meta       ; a string containg infos about the grid.
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  ; Set properties if keyword is present.  
  IF Arg_Present(lon) or ARG_PRESENT(lat) THEN self->Get_LonLat, lon, lat, datum
  IF Arg_Present(tnt_c) THEN tnt_c = self.tnt_c
  IF Arg_Present(meta) THEN meta = self.meta
  IF Arg_Present(nx) THEN  nx = self.tnt_c.nx
  IF Arg_Present(ny) THEN  ny = self.tnt_c.ny
  
END

;+
; :Description:
;    Get the latitudes and longitudes of the grid. 
;
; :Categories:
;            WAVE/OBJ_GIS  
;
; :Params:
;       lon: out, optional, type = float array 
;               2D array containing the longitudes of the grid 
;               (the longitudes will be computed at the first call and stored
;               as a pointer: by large grids, this may cause some memory problems)
;       lat: out, optional, type = float array 
;               2D array containing the latitudes of the grid 
;               (the latitudes will be computed at the first call and stored
;               as a pointer: by large grids, this may cause some memory problems)
;       nx : out, optional, type = integer
;            number of x elements 
;       ny : out, optional, type = integer
;            number of y elements
;       datum : out, optional, type = {TNT_DATUM}
;               the datum in which the lat and lon are defined
;               
; :History:
;      Written by FaM, 2010.
;-
pro w_Grid2D::Get_LonLat, lon, lat, nx, ny, datum

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  nx = self.tnt_c.nx
  ny = self.tnt_c.ny
  datum = self.tnt_c.proj.datum
  undefine, lon, lat
  
  if N_ELEMENTS(*self.lon) eq 1 and (*self.lon)[0] eq -1 then begin
    
    utils_1d_to_2d, INDGEN(nx, /LONG), -INDGEN(ny, /LONG)+ny-1, xi, yi
    
    ;***********************************************
    ; If the array is too big, subset the problem  *
    ;***********************************************
    finished = FALSE
    ind = 0L
    nxi = N_ELEMENTS(xi)
    while not finished do begin
      p1 = ind
      p2 = ind + 4000000L
      if p2 ge (nxi-1) then begin
        p2 = nxi-1
        finished = TRUE
      endif
      GIS_coord_trafo, ret, xi[p1:p2], yi[p1:p2], ilon, ilat, SRC=self.tnt_c, DST=self.tnt_c.proj.datum
      if N_ELEMENTS(lon) eq 0 then lon = TEMPORARY(ilon) else lon = [lon , TEMPORARY(ilon)]
      if N_ELEMENTS(lat) eq 0 then lat = TEMPORARY(ilat) else lat = [lat , TEMPORARY(ilat)]
      ind = p2 + 1
    endwhile
    undefine, xi, yi, ind, p1, p2
    
    lon = reform(lon, nx, ny)
    lat = reform(lat, nx, ny)
    Ptr_Free, self.lon
    Ptr_Free, self.lat
    self.lon = PTR_NEW(lon)
    self.lat = PTR_NEW(lat)
  endif else begin  
    ; We already computed it
    lon = *self.lon
    lat = *self.lat    
  endelse
  
END

;+
; :Description:
;    Get the latitudes and longitudes of the grid. 
;    
; :Categories:
;            WAVE/OBJ_GIS  
;
; :Params:
;       x: out, optional, type = float array 
;               2D array containing the eastings of the grid 
;       y: out, optional, type = float array 
;               2D array containing the northings of the grid 
;       nx : out, optional, type = integer
;            number of x elements 
;       ny : out, optional, type = integer
;            number of y elements
;       proj : out, optional, type = {TNT_PROJ}
;               the projection in which the E/N are defined
;
; :History:
;      Written by FaM, 2010.
;-
pro w_Grid2D::Get_XY, x, y, nx, ny, proj

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  nx = self.tnt_c.nx
  ny = self.tnt_c.ny
  proj = self.tnt_c.proj
    
  utils_1d_to_2d, INDGEN(nx, /LONG) * self.tnt_c.dx + self.tnt_c.x0, $
                  INDGEN(ny, /LONG) * self.tnt_c.dy + self.tnt_c.y1, $
                  x, y
  
END

function w_Grid2D::is_ROI

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  return, self.is_roi
  
end

;+
; :Description:
;   To retrieve the ROI in the form of a mask
;   (if a ROI has been previously set)
;
; :Keywords:
;    MASK: out
;          the ROI mask
;    SUBSET: out
;             the smallest subset ([x0_dl, nx, y0_dl, ny]) surrounding the ROI. 
;             If the ROI is empty
;    MARGIN: in
;            set to a positive integer value to add a margin to the subset
;            (MARGIN=1 will put one grid point on each side of the subset, so two
;             more columns per dimension in total)
; :History:
;     Written by FaM, 2011.
;
;-
pro w_Grid2D::get_ROI, MASK=mask, SUBSET=subset, MARGIN=margin

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, mask, subset
  if ~self.is_roi then mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny) else mask = *self.roi
  
  if ARG_PRESENT(SUBSET) then begin
  
    p = where(mask eq 1, cnt)
    if cnt eq 0 then begin
     undefine, subset
     return      
    endif
    inds = ARRAY_INDICES(mask, p)
    xmin = min(inds[0,*])
    xmax = max(inds[0,*])
    ymin = min(inds[1,*])
    ymax = max(inds[1,*])
    subset = [xmin, (xmax-xmin)+1, ymin, (ymax-ymin)+1]
    
    if arg_okay(MARGIN, /INTEGER) then subset += [-margin,margin*2,-margin,margin*2]
            
  endif
  
end


;+
; :Description:
;   To save the ROI as a shapefile in lon lat coordinates
;   
;   TODO: !CAREFULL: still in alpha version. Possible improvements needed, 
;   such as better smooth and such things... There are probable memory leaks
;   in IDL7- but wel...
;   
; :Params:
; 
;    file: the path to the file you want to write. Should be ending in *.shp
; 
; :History:
;     Written by FaM, 2015.
;
;-
pro w_Grid2D::roi_to_shape, file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  self->get_ROI, MASK=mask
  if total(mask) eq 0 then Message, 'Mask is empty'
  
;  This was for testing
;  mask = mask * 0.
;  mask[10:60, 10:60] = 1
;  mask[30:40, 30:40] = 0
;  mask[80:90, 80:90] = 1
;  mask[82:87, 82:87] = 0
;  mask[10:30, 70:80] = 1
  
  Contour, FLOAT(mask), PATH_INFO=info, PATH_XY=xy, XSTYLE=1, YSTYLE=1, /PATH_DATA_COORDS, LEVELS=0.5
  
  ; Transform to lon lat
  self->transform, xy[0, *], xy[1, *], LON_DST=xx, LAT_DST=yy
  xy[0, *] = xx
  xy[1, *] = yy
  
  pin = where(info.high_low eq 0, cnt_in, COMPLEMENT=pout, NCOMPLEMENT=cnt_out)
  
  outlist = ptrarr(cnt_out)
  for i=0, cnt_out-1 do begin
    inf = info[pout[i]]
    _xy = xy[*, inf.offset :(inf.offset+inf.N-1)]
    _xy = [[_xy], [_xy[*,0]]]
    _n = inf.N + 1
    s = {verts: ptr_new(_xy), $
         nverts: _n, $
         n_parts: 1, $
         parts: ptr_new([0, _n]) $
         }
    outlist[i] = ptr_new(s)
  endfor
  
  for k=0, cnt_in-1 do begin
    found = 0
    inf = info[pin[k]]
    _xy = xy[*, inf.offset :(inf.offset+inf.N-1)]
    _xy = [[_xy], [_xy[*,0]]]
    _n = inf.N + 1
    
    for i=0, cnt_out-1 do begin
      if found eq 1 then continue
      out = *(outlist[i])
      outv = *(out.verts)
      parts = *(out.parts)
      roi = OBJ_NEW('IDLanROI', outv[0, 0:parts[1]-1], outv[1, 0:parts[1]-1])
      p_in = where(roi->ContainsPoints(_xy[0, *], _xy[1, *]) eq 1, cnt_in)
      if cnt_in ne 0 then begin
        out.verts = ptr_new([[outv], [_xy]])
        out.parts = ptr_new([parts, out.nverts+_n])
        out.nverts = out.nverts + _n
        out.n_parts = out.n_parts + 1
        found = 1
        outlist[i] = ptr_new(out)
      endif
    endfor
    if found eq 0 then Message, 'Parent not found!'
  endfor
  
  dObj = obj_new('IDLffShape', file, /UPDATE, ENTITY_TYPE=5)
  dObj->AddAttribute, 'ROI_ID', 7, 5, PRECISION=0

  for i=0, cnt_out-1 do begin
    out = *(outlist[i])
    
    xy= *(out.verts)
    xx= reform(xy[0,*])
    yy= reform(xy[1,*])
    ; Create structure for new entity
    entNew = {IDL_SHAPE_ENTITY}

    ; Define the values for the new entity
    entNew.SHAPE_TYPE = 5
    entNew.BOUNDS[0] = min(xx)
    entNew.BOUNDS[1] = min(yy)
    entNew.BOUNDS[2] = 0.00000000
    entNew.BOUNDS[3] = 0.00000000
    entNew.BOUNDS[4] = max(xx)
    entNew.BOUNDS[5] = max(yy)
    entNew.BOUNDS[6] = 0.00000000
    entNew.BOUNDS[7] = 0.00000000

    entNew.N_VERTICES = out.nverts
    entNew.VERTICES = ptr_new(xy)
    
    entNew.N_PARTS = out.n_parts
    entNew.PARTS =  ptr_new(*(out.parts))
    
    ; Create structure for new attributes
    attrNew = dObj->GetAttributes(/ATTRIBUTE_STRUCTURE)

    ; Define the values for the new attributes
    attrNew.ATTRIBUTE_0 = w_str(i)

    ; Add the new entity to new shapefile
    dObj->PutEntity, entNew
    ; Add the Colorado attributes to new shapefile.
    dObj->SetAttributes, i, attrNew

    dObj->PutEntity, entNew
  endfor
  
  obj_destroy, dObj
  undefine, outlist
  
end


;+
; :Description:
;    Generic routine to do any kind of transformation into the object grid (the destination grid is the object grid itself). 
;    You can also compute eastings and northings in the grid projection, as well as lat and lons in the grid datum. 
;       
;    In many cases, the more specific 'transform_LonLat', 'transform_IJ' or 'transform_XY' can be used instead.
;
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    x: in, required, type = float/double 
;       x coordinates in SRC (if they are indexes,  be carefull with orientation! (0,0) is down left!)
;    y: in, required, type = float/double 
;       y coordinates in SRC (if they are indexes,  be carefull with orientation! (0,0) is down left!)
;    i_dst: out, type = double 
;           the i coordinates of (x,y) in the object grid 
;    j_dst: out, type = double 
;           the j coordinates of (x,y) in the object grid 
;
; :Keywords:
;    SRC: in, optional
;         src the initial coordinate system (w_Grid2D or {TNT_PROJ} or {TNT_DATUM}) 
;         in which x and y are defined. Default is the source grid, so that no transformation
;         is performed
;    LON_DST: out, type = float/double 
;             the longitudes of (x,y) in the object grid 
;    LAT_DST: out, type = float/double  
;             the latitudes of (x,y) in the object grid 
;    E_DST: out,  type = float/double 
;            the eastings of (x,y) in the object grid 
;    N_DST: out,  type = float/double 
;           the northings of (x,y) in the object grid 
;    NEAREST: in, optional
;             set if the nearest i,j couple is desired
;    MARK_EXTERIOR: in, optional
;                   if set, the i,j couples outside the grid are all marked as [-1, -1],
;                   the other outputs (*_DST) are kept same
;             
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform, x, y, i_dst, j_dst, SRC = src, LON_DST=lon_dst, LAT_DST=lat_dst, E_DST=E_dst, N_DST=N_dst, NEAREST=nearest, MARK_EXTERIOR=mark_exterior
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  ON_ERROR, 2
  
  ;*****************************************
  ; If src is a grid, y has to be rotated  *
  ;*****************************************
  if arg_okay(src, STRUCT={TNT_COORD}) then begin
    Message, 'Src is a {TNT_COORD} structure. We only accept w_Grid2D objects, please make one.' 
  endif else if (OBJ_VALID(src)) then begin
    if OBJ_ISA(src, 'w_Grid2D') then begin
      src->getProperty, TNT_C = mysrc
      _y = mysrc.ny - y  - 1
    endif else MESSAGE, 'SRC is an object but not a grid??'
  endif else if N_ELEMENTS(src) ne 0 then begin
      mysrc = src
      _y = y
  endif else begin
     mysrc = self.tnt_c
    _y = mysrc.ny - y  - 1
  endelse  
  _x = x
  
  cas = 0
  if (ARG_PRESENT(LAT_DST) or ARG_PRESENT(LON_DST)) and ~ARG_PRESENT(E_DST) and ~ARG_PRESENT(N_DST) then cas = 1
  if (ARG_PRESENT(LAT_DST) or ARG_PRESENT(LON_DST)) and (ARG_PRESENT(E_DST) or ARG_PRESENT(N_DST)) then cas = 2
  if ~ARG_PRESENT(LAT_DST) and ~ARG_PRESENT(LON_DST) and (ARG_PRESENT(E_DST) or ARG_PRESENT(N_DST)) then cas = 3
  undefine, LAT_DST, LON_DST, E_DST, N_DST
    
  ;***********************************************
  ; If the array is too big, subset the problem  *
  ;***********************************************
  finished = FALSE
  ind = 0L
  nxi = N_ELEMENTS(_x)
  undefine, i_dst, j_dst
  while not finished do begin
    p1 = ind
    p2 = ind + 4000000L ;2000*2000 is the limit
    if p2 ge (nxi-1) then begin
      p2 = nxi-1
      finished = TRUE
    endif
    
    case (cas) of
      0: begin
          GIS_coord_trafo, ret, _x[p1:p2], _y[p1:p2], ti_dst, tj_dst, SRC=mysrc, DST=self.tnt_c, NEAREST = nearest
      end
      1: begin
          GIS_coord_trafo, ret, _x[p1:p2], _y[p1:p2], ti_dst, tj_dst, SRC=mysrc, DST=self.tnt_c, $
               LON_DST=tlon_dst, LAT_DST=tlat_dst, NEAREST = nearest
          if N_ELEMENTS(tlon_dst) ne 0 then if N_ELEMENTS(LON_DST) eq 0 then LON_DST = TEMPORARY(tlon_dst) else LON_DST = [LON_DST , TEMPORARY(tlon_dst)]
          if N_ELEMENTS(tlat_dst) ne 0 then if N_ELEMENTS(LAT_DST) eq 0 then LAT_DST = TEMPORARY(tlat_dst) else LAT_DST = [LAT_DST , TEMPORARY(tlat_dst)]
      end
      2: begin
          GIS_coord_trafo, ret, _x[p1:p2], _y[p1:p2], ti_dst, tj_dst, SRC=mysrc, DST=self.tnt_c, $
               LON_DST=tlon_dst, LAT_DST=tlat_dst, E_DST= te_dst, N_DST=tN_DST, NEAREST = nearest
          if N_ELEMENTS(tlon_dst) ne 0 then if N_ELEMENTS(LON_DST) eq 0 then LON_DST = TEMPORARY(tlon_dst) else LON_DST = [LON_DST , TEMPORARY(tlon_dst)]
          if N_ELEMENTS(tlat_dst) ne 0 then if N_ELEMENTS(LAT_DST) eq 0 then LAT_DST = TEMPORARY(tlat_dst) else LAT_DST = [LAT_DST , TEMPORARY(tlat_dst)]
          if N_ELEMENTS(te_dst) ne 0 then if N_ELEMENTS(E_DST) eq 0 then E_DST = TEMPORARY(te_dst) else E_DST = [E_DST , TEMPORARY(te_dst)]
          if N_ELEMENTS(tN_DST) ne 0 then if N_ELEMENTS(N_DST) eq 0 then N_DST = TEMPORARY(tN_DST) else N_DST = [N_DST , TEMPORARY(tN_DST)]
      end
      3: begin
          GIS_coord_trafo, ret, _x[p1:p2], _y[p1:p2], ti_dst, tj_dst, SRC=mysrc, DST=self.tnt_c, $
               E_DST= te_dst, N_DST=tN_DST, NEAREST = nearest
          if N_ELEMENTS(te_dst) ne 0 then if N_ELEMENTS(E_DST) eq 0 then E_DST = TEMPORARY(te_dst) else E_DST = [E_DST , TEMPORARY(te_dst)]
          if N_ELEMENTS(tN_DST) ne 0 then if N_ELEMENTS(N_DST) eq 0 then N_DST = TEMPORARY(tN_DST) else N_DST = [N_DST , TEMPORARY(tN_DST)]
      end
      else: begin
          Message, 'Should never come here.'
      end
    endcase
    if N_ELEMENTS(i_dst) eq 0 then i_dst = TEMPORARY(ti_dst) else i_dst = [i_dst , TEMPORARY(ti_dst)]
    if N_ELEMENTS(j_dst) eq 0 then j_dst = TEMPORARY(tj_dst) else j_dst = [j_dst , TEMPORARY(tj_dst)]   
    ind = p2 + 1
  endwhile
  undefine, ind, p1, p2
  j_dst = self.tnt_c.ny - j_dst  - 1 ;up an down
  
  if KEYWORD_SET(MARK_EXTERIOR) then begin
    p = where((i_dst lt -0.5) or (j_dst lt -0.5) or (i_dst gt self.tnt_c.nx - 0.5) or (j_dst gt self.tnt_c.ny - 0.5), cnt)
    if cnt ne 0 then begin
      i_dst[p] = -1
      j_dst[p] = -1      
    endif    
  endif  

  s = size(x)  
  if s[0] eq 2 then begin ; they gave us something 2D, we should keep it like this
    i_dst = reform(i_dst,s[1],s[2])
    j_dst = reform(j_dst,s[1],s[2])
  endif
      
end

;+
; :Description:
;    Specific routine to transforms lons and lats into the object grid (the destination grid is the object grid itself).
;    (wrapper for the 'transform' routine)
;
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    lon: in, required, type = float/double 
;       lon coordinates in datum
;    lat: in, required, type = float/double 
;       lat coordinates in datum
;    datum: in, required, type = {TNT_DATUM}
;           the lon/lat datum system {TNT_DATUM}
;    i: out, type = double
;       the i coordinates of (lon,lat) in the object grid 
;    j: out, type = double
;       the j coordinates of (lon,lat) in the object grid 
;
; :Keywords:
;    NEAREST: in, optional
;             set if the nearest i,j couple is desired
;    MARK_EXTERIOR: in, optional
;                   if set, the i,j couples outside the grid are all marked as [-1, -1]
;                   
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform_LonLat, lon, lat, datum, i, j, NEAREST=nearest, MARK_EXTERIOR=mark_exterior
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if not arg_okay(datum, STRUCT={TNT_DATUM}) then Message, WAVE_Std_Message('datum', STRUCT={TNT_DATUM})
  
  self->transform, lon, lat, i, j, SRC = datum, NEAREST=nearest, MARK_EXTERIOR=mark_exterior
        
end

;+
; :Description:
;    Gpecific routine to transforms eastings and northings into the object grid (the destination grid is the object grid itself).    
;    (wrapper for the 'transform' routine)
;    
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    x: in, required, type = float/double 
;       northings in the proj
;    y: in, required, type = float/double 
;       eastings in the proj 
;    proj: in, required, type = {TNT_PROJ}
;          the x/y projection system {TNT_PROJ}
;    i: out, type = double
;       the i coordinates of (lon,lat) in the object grid 
;    j: out, type = double
;       the j coordinates of (lon,lat) in the object grid 
;
; :Keywords:
;    NEAREST: in, optional
;             set if the nearest i,j couple is desired
;    MARK_EXTERIOR: in, optional
;                   if set, the i,j couples outside the grid are all marked as [-1, -1]
;
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform_XY, x, y, proj, i, j, NEAREST=nearest, MARK_EXTERIOR=mark_exterior

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if not arg_okay(proj, STRUCT={TNT_PROJ}) then Message, WAVE_Std_Message('proj', STRUCT={TNT_PROJ})
  
  self->transform, x, y, i, j, SRC=proj, NEAREST=nearest, MARK_EXTERIOR=mark_exterior
  
end

;+
; :Description:
;    Specific routine to transforms i and j from a grid into the object grid (the destination grid is the object grid itself).      
;    (wrapper for the 'transform' routine)
;    
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    i_src: in, required, type = integer
;           i indexes into the grid (be carefull with orientation! (0,0) is down left!)
;    j_src: in, required, type = integer
;           j indexes into the grid (be carefull with orientation! (0,0) is down left!)
;    grid: in, required, type = w_Grid2D
;          the grid system (instance of w_Grid2D)
;    i: out, type = double
;       the i coordinates of (lon,lat) in the object grid 
;    j: out, type = double
;       the j coordinates of (lon,lat) in the object grid 
;
; :Keywords:
;    NEAREST: in, optional
;             set if the nearest i,j couple is desired
;    MARK_EXTERIOR: in, optional
;                   if set, the i,j couples outside the grid are all marked as [-1, -1]
;                   
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform_IJ, i_src, j_src, grid, i, j, NEAREST=nearest, MARK_EXTERIOR=mark_exterior

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if not OBJ_ISA(grid, 'w_Grid2D')  then Message, WAVE_Std_Message('grid', OBJ='w_Grid2D')
  
  self->transform, i_src, j_src, i, j, SRC = grid, NEAREST=nearest, MARK_EXTERIOR=mark_exterior
  
end

;+
; :Description:
;    Generic routine to transform any georeferenced grid without projection
;    (for example lon and lat grid from swath files) into the object grid. 
;              
;    The default method uses GRIDDATA to perform interpolation. This method 
;    is recommended. However, this implies using interpolation algorithms from
;    the irregular grid to the regular arrival grid, which is demanding in both memory 
;    and processor use. If your coordinates arrays are too large, the program may lag 
;    or even go out of memory. Moreover, you should be aware that there is no way to
;    know if a grid point is inside our outside the lonlat coordinates, so that 
;    extrapolation may be performed without warning. Be sure that the arrival grid
;    allways lies inside the data you want to interpolate.        
;    
;       
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    data: in, required, type = float array
;          the data to map on the object grid. The two first dimenstions are X and Y, the third is handled as time or Z dim
;    lon: in, required, type = float array
;             the longitudes of the data
;    lat: in, required, type = float array
;             the latitudes of the data
;               
; :Keywords:
;    SRC: in, optional, type = {TNT_DATUM}
;               the datum of the lonlats (default is WGS-84)
;    MISSING: in, optional
;             value to set to missing values in the final grid
;    LINEAR: in, optional
;            if linear interpolation has to be used
;    NOBOUNDS: in, optional
;              standard beavior is to replace bounds by MISSING data do avoid
;              stupid extrapolating. Set this keyword to avoid this 
;
; :Returns:
;    the transformed data array in the grid (same X Y dims as the grid)
;
; :History:
;      Written by FaM, 2011.
;-
function w_Grid2D::map_lonlat_data, data, lon, lat, SRC=src, MISSING=missing, LINEAR=linear, NOBOUNDS=nobounds              

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
    
  if not arg_okay(src, STRUCT={TNT_DATUM}) then begin
   GIS_make_datum, ret, src, NAME = 'WGS-84'
  endif
  
  if N_ELEMENTS(missing) eq 0 then begin
      dataTypeName = Size(data, /TNAME)
      CASE dataTypeName OF
        'FLOAT' : missing = !VALUES.F_NAN
        'DOUBLE': missing = !VALUES.D_NAN
        'BYTE': missing = 0B
        ELSE: missing = -999
      ENDCASE
  endif 
  
  if ~ KEYWORD_SET(LINEAR) then nearest_neighbor = 1B
  
  siz_src = SIZE(data) 
  if siz_src[0] le 2 then nt = 1 else if siz_src[0] eq 3 then nt = siz_src[3] else  Message, WAVE_Std_Message('data', /ARG)
  nc = N_ELEMENTS(data[*,*,0])
  
  if nc ne N_ELEMENTS(lon) or nc ne N_ELEMENTS(lat) $ 
   then message, 'Data and coordinates incompatible'
   
  ; Define output size
  type = SIZE(data,/TYPE)
  tdata = MAKE_ARRAY(self.tnt_c.nx, self.tnt_c.ny, nt, TYPE=type)
  tdata[*] = missing
  if ~KEYWORD_SET(NOBOUNDS) then begin
    data[0:(siz_src[1]-1),0,*] = missing
    data[0:(siz_src[1]-1),siz_src[2]-1,*] = missing
    data[0,0:(siz_src[2]-1),*] = missing
    data[siz_src[1]-1,0:(siz_src[2]-1),*] = missing
  endif
   
  self->transform_LonLat, lon, lat, src, i, j
  TRIANGULATE, i, j, triangles
  for t=0, nt-1 do begin
    temp = tdata[*,*,t]
    temp[*] = GRIDDATA(i, j, data[*,*,t], START=[0D,0D], DELTA=[1D,1D], DIMENSION=[self.tnt_c.nx,self.tnt_c.ny], $
    TRIANGLES=triangles, NEAREST_NEIGHBOR=nearest_neighbor, LINEAR=linear, MISSING=missing)        
    tdata[*,*,t] = TEMPORARY(temp)
  endfor   

  return, reform(tdata)
  
end

;+
; :Description:
;    Important routine to transform a data array defined in any other grid INTO the object grid. 
;    
;    This routines performs a standard backward projection transformation between two grids, the DESTINATION
;    grid beeing the current object. The default interpolation method is NEAREST NEIGHBOR. 
;    BILINEAR and CUBIC interpolation are also implemented.
;    
;    If a ROI has been previously defined into the SOURCE grid, the data will be cropped to this ROI
;    (this has no effect on the processing time).
;    
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    data: in, required, type = float array
;          the data to map on the grid itself. The two first dimenstions are X and Y, the third is handled as time
;    src_grid: in, required, type = w_Grid2D
;              the SOURCE grid associated to the data (w_Grid2D Object)
;
; :Keywords:
;    MISSING: in, optional
;             value to set to missing values in the final grid. NaN or 0 are default values depending on the data type
;    BILINEAR: in, optional
;             set to use bilinear interpolation instead of NN
;    CUBIC: in, optional
;             set to use cubic interpolation (significantly slower) instead of NN
;    DATA_DST: in, optional
;              if given, this array will be filled with the new mapped data. 
;              Only the concerned indexes will be overwritten. (usefull for mosaiking 
;              or overplotting, for example). This data must have the same X and Y dimensions
;              as the grid, and same z dimension as data
;    TO_ROI: in, optional
;            if set, the transformation will be made only into the defined ROI. 
;            (positive impact on the computation time)
;
; :Returns:
;    The remaped data array, of the same dimensions of the object grid, with an eventual 3rd dim (time/Z)
;    
;-
function w_Grid2D::map_gridded_data, data, src_grid, MISSING=missing, BILINEAR=bilinear, CUBIC=cubic, DATA_DST=data_dst, TO_ROI=to_roi
     
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if not OBJ_ISA(src_grid, 'w_Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='w_Grid2D')
  
  if ~ arg_okay(data, /NUMERIC) then Message, WAVE_Std_Message('data', /ARG)
  if N_ELEMENTS(data) eq 1 then data=reform(data, 1,1)
  
  siz_src = SIZE(data) 
  if siz_src[0] eq 2 then n = 1 else if siz_src[0] eq 3 then n = siz_src[3] else  Message, WAVE_Std_Message('data', /ARG)
  mx = siz_src[1] & my = siz_src[2]
  
  src_grid->getProperty, tnt_c=src_c
  if mx ne src_c.nx then Message, 'DATA array and GRID do not agree.'
  if my ne src_c.ny then Message, 'DATA array and GRID do not agree.'
  
  is_dst = FALSE
  if N_ELEMENTS(DATA_DST) ne 0 then begin
     if ~ arg_okay(DATA_DST, /NUMERIC) then Message, WAVE_Std_Message('DATA_DST', /ARG)
     siz_dst = SIZE(DATA_DST)
     if siz_dst[0] ne siz_src[0] then Message, '$DATA and $DATA_DST do not match'
     if siz_dst[1] ne self.tnt_c.nx then Message, '$DATA_DST not of suitable X dimension' 
     if siz_dst[2] ne self.tnt_c.ny then Message, '$DATA_DST not of suitable Y dimension' 
     is_dst = TRUE
  endif
  
  if ~ is_dst and N_ELEMENTS(missing) eq 0 then begin
      dataTypeName = Size(data, /TNAME)
      CASE dataTypeName OF
        'FLOAT' : missing = !VALUES.F_NAN
        'DOUBLE': missing = !VALUES.D_NAN
        'BYTE': missing = 0B
        ELSE: missing = -9999
      ENDCASE
  endif
  
  bili = KEYWORD_SET(BILINEAR)
  cubic = KEYWORD_SET(CUBIC)
  if ~cubic and ~bili then nearest = TRUE else nearest = false ;Otherwize, we need the decimals of course
  
  utils_1d_to_2d, INDGEN(self.tnt_c.nx, /LONG), -INDGEN(self.tnt_c.ny, /LONG) + self.tnt_c.ny - 1, xi, yi
  
  if self->is_ROI() and KEYWORD_SET(TO_ROI) then begin
    proi = where(*self.roi ne 0)
    xi =  xi[proi] 
    yi =  yi[proi]     
  endif
  
  ;***********************************************
  ; If the array is too big, subset the problem  *
  ;***********************************************
  finished = FALSE
  ind = 0L
  nxi = N_ELEMENTS(xi)
  while not finished do begin
    p1 = ind
    p2 = ind + 4000000L ;2000*2000 is the limit
    if p2 ge (nxi-1) then begin
      p2 = nxi-1
      finished = TRUE
    endif
    GIS_coord_trafo, ret, xi[p1:p2], yi[p1:p2], ti_dst, tj_dst, SRC=self.tnt_c, DST=src_c, NEAREST=nearest
    if N_ELEMENTS(i_dst) eq 0 then i_dst = TEMPORARY(ti_dst) else i_dst = [i_dst , TEMPORARY(ti_dst)]
    if N_ELEMENTS(j_dst) eq 0 then j_dst = TEMPORARY(tj_dst) else j_dst = [j_dst , TEMPORARY(tj_dst)]
    ind = p2 + 1
  endwhile
  undefine, xi, yi, ind, p1, p2
  j_dst = src_c.ny - j_dst - 1
  
  if self->is_ROI() and KEYWORD_SET(TO_ROI) then begin
    _i_dst = MAKE_ARRAY(self.tnt_c.nx*self.tnt_c.ny, TYPE=SIZE(i_dst, /TYPE))
    _j_dst = _i_dst
    _i_dst[proi] = TEMPORARY(i_dst)
    _j_dst[proi] = TEMPORARY(j_dst)
    i_dst = TEMPORARY(_i_dst)
    j_dst = TEMPORARY(_j_dst)    
  endif
  
  ;***********************************
  ; Get the data in the source grid  *
  ;***********************************
  if nearest then p_out = where((i_dst lt 0) or (j_dst lt 0) or (i_dst ge mx) or (j_dst ge my), cnt_out) $ ; OUT of range $
   else p_out = where((i_dst lt -0.5) or (j_dst lt -0.5) or (i_dst gt mx+0.5) or (j_dst gt my+0.5), cnt_out)
   
  if src_grid->is_roi() then begin ;Add the masked pixels to the indemove afterwards
    src_grid->get_ROI, MASK=mask
    poutm = where(mask[i_dst, j_dst] ne 1, cntoutm)
    if cntoutm ne 0 then begin
      if cnt_out eq 0 then begin
        cnt_out = cntoutm
        p_out = poutm
      endif else begin
        cnt_out += cntoutm
        p_out = [p_out, poutm]
      endelse
    endif
  endif
  
  ; Now perform the interpolation using the computed indexes
  if is_dst then type = SIZE(data_dst,/TYPE) else type = SIZE(data,/TYPE)
  tdata = MAKE_ARRAY(self.tnt_c.nx, self.tnt_c.ny, n, TYPE=type)
  for i = 0L, n-1 do begin
    if bili then tmp = BILINEAR((reform(data[*,*,i])), reform(i_dst, self.tnt_c.nx, self.tnt_c.ny), reform(j_dst, self.tnt_c.nx, self.tnt_c.ny)) $
      else if cubic then tmp = INTERPOLATE((reform(data[*,*,i])), reform(i_dst, self.tnt_c.nx, self.tnt_c.ny), reform(j_dst, self.tnt_c.nx, self.tnt_c.ny), CUBIC=-0.5) $ 
       else tmp = (data[*,*,i])[i_dst, j_dst]
    if cnt_out ne 0 then begin
      if is_dst then begin
        tmp2 = data_dst[*,*,i]
        tmp[p_out] = tmp2[p_out]
      endif else tmp[p_out] = missing
    endif
    tdata[*,*,i] = reform(tmp, self.tnt_c.nx, self.tnt_c.ny)
  endfor
   
  return, reform(tdata)
     
end

;+
; :Description:
;    This routine performs forward transformation of any grid into the object grid. 
;    
;    The principle of forward transform is to obtain, for each grid point of the
;    object grid, all the indexes in the source grid that are contained into the
;    given grid point. This transformation makes sense ONLY if the source grid has
;    a higher resolution than the object grid. Otherwize choose the more general
;    (and much faster) `w_grid2d->map_gridded_data` method. 
;    
;    This is usefull in combination with `w_grid2d->fwd_transform_data` or to gather any 
;    kind of information (see examples).
;       
;
; :Params:
;    src_grid: in,  type = w_Grid2D
;              the data grid (w_Grid2D Object)
;
; :Keywords:
;
; :Returns:
;    An array of pointers, of the same dimensions of the object grid. Each valid pointer contains 
;    an array of indices into the argument data grid (an invalid pointer means no element was found)
;    
; :Examples:
; 
;    Open data sets:: 
;      lst = OBJ_NEW('w_MODIS', FILE=TEST_file_directory() + 'MODIS/'+'MOD11A2.A2008297.h25v05.005.2008311141349.hdf') ; MODIS grid
;      wrf = OBJ_NEW('w_WRF', FILE= TEST_file_directory() + 'WRF/wrfout_d02_2008-10-26', CROPBORDER=12); WRF grid
;    
;    Get the array of pointers:: 
;      dd = wrf->fwd_transform_grid(lst) ; array of pointers into the modis grid
;      siz = SIZE(dd, /DIMENSIONS)
;    
;    This is to obtain the number of pixels under each grid point::
;      nels = LONARR(siz[0], siz[1])         
;      for i = 0, N_ELEMENTS(dd) - 1 do if PTR_VALID(dd[i]) then nels[i] = N_ELEMENTS(*(dd[i]))
;      w_QuickPlot, nels
;      
;    This is to obtain the mean temperature of all the pixels under each grid point::
;      lst_temp =  lst->get_var('LST_Day_1km')-273.15
;      pnok = where(lst_temp le 0, cnt)
;      if cnt ne 0 then lst_temp[pnok] = !VALUES.F_NAN
;      meantemp = fltARR(siz[0], siz[1]) 
;      for i = 0, N_ELEMENTS(dd) - 1 do if PTR_VALID(dd[i]) then meantemp[i] = MEAN(lst_temp[*(dd[i])], /NAN)
;      w_QuickPlot, MEANTEMP, COLORTABLE = 13
;    
;    This is to free the memory after using all this::
;      for i = 0, N_ELEMENTS(dd) - 1 do PTR_free, dd[i]
;    
;-
function w_Grid2D::fwd_transform_grid, src_grid
     
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if not OBJ_ISA(src_grid, 'w_Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='w_Grid2D')
  
  src_grid->getProperty, tnt_c = src_c  
  utils_1d_to_2d, INDGEN(src_c.nx, /LONG), INDGEN(src_c.ny, /LONG), xi, yi  
  self->transform_IJ, xi, yi, src_grid, i_dst, j_dst, /NEAREST
  
  pok = where(i_dst ge 0 and j_dst ge 0 and i_dst lt self.tnt_c.nx and j_dst lt self.tnt_c.ny, cnt)
  
  out = ptrarr(self.tnt_c.nx, self.tnt_c.ny)
  if cnt ne 0 then begin
    out_inds = i_dst[pok] + self.tnt_c.nx * j_dst[pok]
    orig_inds = xi[pok] + src_c.nx * yi[pok]
    h = HISTOGRAM(out_inds, MIN=0, MAX=self.tnt_c.nx*self.tnt_c.ny-1, BINSIZE=1, REVERSE_INDICES=r)
    pok = where(h ne 0, cnt) 
    for i=0LL, cnt - 1 do out[pok[i]] = PTR_NEW(orig_inds[r[r[pok[i]] : r[pok[i]+1]-1]])
  endif
  
  return, out
     
end

;+
; :Description:
;    Generic routine to make a copy of the object grid.
;    If the XSIZE, YSIZE or FACTOR are set, the grid will be resampled
;    If the `TO_ROI` keyword is set, the copy of the grid will be the 
;    smallest enclosing grid of the grid ROIs (if present).    
;    
; :Categories:
;         WAVE/OBJ_GIS
;
; :Keywords:
;    XSIZE:  in, optional, type = integer
;           the new X dimension size (the original X/Y ratio is conserved) 
;    YSIZE: in, optional, type = integer
;           the new Y dimension size (the original X/Y ratio is conserved)
;    FACTOR: in, optional, type = float
;           a factor to multiply to nx and ny (ignored if XSIZE or YSIZE are set)
;    TO_ROI: in, optional, type = boolean
;            if set, the returned grid will be will be the 
;            smallest enclosing grid of the grid ROIs (if present). 
;    ROIMARGIN: in 
;               set to a positive integer value to add a margin to the ROI subset
;               (ROIMARGIN=1 will put one grid point on each side of the ROI subset, 
;               so two more columns per dimension in total)
;    EXPAND: in 
;            set to a scalar integer N to expand the new grid of N pixels on each side
;            (note that setting a negative value could work too...)
;    MARGIN: in
;            decprecated. Now called ROIMARGIN
; :Returns:
;     A new w_Grid2D, which is a resampled version of the object grid
;     
;-
function w_Grid2D::reGrid, XSIZE=Xsize, $
  YSIZE=Ysize, $
  FACTOR=factor, $
  TO_ROI=to_roi, $
  ROIMARGIN=roimargin, $
  EXPAND=expand, $
  MARGIN=margin

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  if ~ arg_okay(FACTOR, /NUMERIC) then factor = 1
  if KEYWORD_SET(MARGIN) then ROIMARGIN=1 ; dperecated keyword
  
  if N_ELEMENTS(Xsize) ne 0 and N_ELEMENTS(Ysize) ne 0 then Message, 'Ambiguous keyword combination.' 
  if N_ELEMENTS(Xsize) ne 0 then factor = double(Xsize) / self.tnt_c.nx
  if N_ELEMENTS(Ysize) ne 0 then factor = double(Ysize) / self.tnt_c.ny
  
  x0 = self.tnt_c.x0
  y0 = self.tnt_c.y0
  nx = self.tnt_c.nx
  ny = self.tnt_c.ny
  
  if KEYWORD_SET(TO_ROI) then begin
    self->get_ROI, SUBSET=subset, MARGIN=margin
    x0 = self.tnt_c.x0 + subset[0] * self.tnt_c.dx
    y0 = self.tnt_c.y0 - (self.tnt_c.ny-(subset[2]+subset[3])) * self.tnt_c.dy
    nx = subset[1]
    ny = subset[3]
  endif
  
  nx = nx * FACTOR
  ny = ny * FACTOR
  dx = self.tnt_c.dx / double(FACTOR)
  dy = self.tnt_c.dy / double(FACTOR)
  
  x0 = x0 - 0.5d*self.tnt_c.dx + dx/2d
  y0 = y0 + 0.5d*self.tnt_c.dy - dy/2d
  
  if N_ELEMENTS(EXPAND) eq 1 then begin
    nx += 2 * long(expand)
    ny += 2 * long(expand)
    x0 = x0 - dx * float(expand)
    y0 = y0 + dy * float(expand)
  endif
  
  return, OBJ_NEW('w_Grid2D', x0=x0, y0=y0, nx=nx, ny=ny, dx=dx, dy=dy, PROJ=self.tnt_c.proj, META=self.meta + ' resampled (factor ' +str_equiv(factor) + ')')
    
end

;+
; :Description:
;   This function realizes the forward transformation of a gridded dataset 
;   into the object grid. The input dataset must be of higher resolution as 
;   the object grid otherwize this method makes no sense. See the object 
;   desciption for more information about the different approach
;   between `w_grid2d->map_gridded_data` and `w_grid2d->fwd_transform_data`
;
; :Params:
;    data: in, required, type = float array
;          the data to map on the grid itself. The two first dimenstions are X and Y, the third is handled as time
;    src_grid: in, required, type = w_Grid2D
;              the SOURCE grid associated to the data (w_Grid2D Object)
;
; :Keywords:
;    MISSING: in, optional
;             value to set to missing values in the final grid. NaN or 0 are default values depending on the data type
;    METHOD: in, optional, type = str, default = 'MEAN'
;            the transform method (currently 'MEAN', 'MIN', 'MAX', 'SUM' are implemented)
;    IN_FWD_TRAFO: in, optional
;                  to spare time in computation, the user may want want to perform
;                  once and only once the `w_Grid2D::fwd_transform_grid` and give its 
;                  output als argument here
;    N_VALID: out, optional, type = lonarr
;             array of the same size as the ouptut containing the number of valid pixels
;             used for the computation of the pixel value
;
;-
function w_Grid2D::fwd_transform_data, data, src_grid, $
    MISSING=missing, $
    METHOD=method, $
    N_VALID=n_valid, $
    IN_FWD_TRAFO=in_fwd_trafo
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if not OBJ_ISA(src_grid, 'w_Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='w_Grid2D')
  
  if ~ arg_okay(data, /NUMERIC) then Message, WAVE_Std_Message('data', /ARG)
  if N_ELEMENTS(data) eq 1 then data=reform(data, 1,1)
  
  siz_src = SIZE(data) 
  if siz_src[0] eq 2 then n = 1 else if siz_src[0] eq 3 then n = siz_src[3] else  Message, WAVE_Std_Message('data', /ARG)
  mx = siz_src[1] & my = siz_src[2]
  
  src_grid->getProperty, tnt_c=src_c
  if mx ne src_c.nx then Message, 'DATA array and GRID do not agree.'
  if my ne src_c.ny then Message, 'DATA array and GRID do not agree.'
  
  dataTypeName = Size(data, /TNAME)
  
  if N_ELEMENTS(METHOD) eq 0 then method = 'MEAN'
  
  if N_ELEMENTS(missing) eq 0 then begin
    CASE dataTypeName OF
      'FLOAT' : missing = !VALUES.F_NAN
      'DOUBLE': missing = !VALUES.D_NAN
      'BYTE': missing = 0B
      ELSE: missing = -9999
    ENDCASE
  endif
  case dataTypeName of
    'FLOAT' : epsilon = (MACHAR()).eps
    'DOUBLE': epsilon = (MACHAR(/DOUBLE)).eps
    else: epsilon = 0
  endcase
  
  ; First check for finite elements
  pFin = where(finite(data), cntFin, COMPLEMENT=pNoFin, NCOMPLEMENT=cntNoFin)
    
  ; Then add the test for missing values if needed
  if finite(missing) then begin
    pValid = where(Abs(data - missing) gt epsilon, cntValid, $
                       COMPLEMENT=pNoValid, NCOMPLEMENT=cntNoValid)
  endif else begin
    pValid = TEMPORARY(pFin)
    cntValid = TEMPORARY(cntFin)
    pNoValid = TEMPORARY(pNoFin)
    cntNoValid = TEMPORARY(cntNoFin)   
  endelse
  is_Missing = cntNoValid ne 0
  is_Valid = cntValid ne 0
  
  ; Mask
  valid = BYTARR(SIZE(data, /DIMENSIONS))
  if is_Valid then valid[pValid] = 1B
   
  CASE dataTypeName OF
    'FLOAT' : out_data = FLTARR(self.tnt_c.nx, self.tnt_c.ny, n)
    'DOUBLE': out_data = DBLARR(self.tnt_c.nx, self.tnt_c.ny, n)
    'BYTE': begin
      out_data = BYTARR(self.tnt_c.nx, self.tnt_c.ny, n)
      Message, 'Carefull. You give me a byte array, I give you a byte array.'
    end
    'LONG': out_data = LONARR(self.tnt_c.nx, self.tnt_c.ny, n)
    'INT': out_data = INTARR(self.tnt_c.nx, self.tnt_c.ny, n)
    ELSE: Message, 'Dataype a bit exotic. Make it better.'
  ENDCASE
  
  if ARG_PRESENT(N_VALID) then do_valid = TRUE else do_valid = FALSE
  
  if do_valid then n_valid = LONG(out_data)
  out_data[*] = missing
  
  if ~ is_Valid then return, out_data ;nothing to do
  
  if N_ELEMENTS(IN_FWD_TRAFO) ne 0 then begin
    dd = in_fwd_trafo
    if ~ arg_okay(dd, N_ELEM=N_ELEMENTS(out_data)) then Message, WAVE_Std_Message('IN_FWD_TRAFO', /NELEMENTS)
    if ~ arg_okay(dd, TYPE=10) then Message, WAVE_Std_Message('IN_FWD_TRAFO', /ARG)
  endif else dd = self->fwd_transform_grid(src_grid) ; array of pointers into the src grid

  for i = 0L, n-1 do begin
    _data = data[*,*,i]
    _out_data = out_data[*,*,i]
    _valid = valid[*,*,i]
    if do_valid then _n_valid = LONARR(self.tnt_c.nx, self.tnt_c.ny)
    for j = 0, N_ELEMENTS(dd) - 1 do begin
      if ~ PTR_VALID(dd[j]) then continue
      pv = where(_valid[*(dd[j])], cntv)
      if cntv eq 0 then continue      
      d = (_data[*(dd[j])])[pv]
      case str_equiv(METHOD) of
        'MEAN': _out_data[j] = MEAN(d, /NAN)
        'MIN': _out_data[j] = MIN(d, /NAN)
        'MAX': _out_data[j] = MAX(d, /NAN)
        'SUM': _out_data[j] = TOTAL(d, /NAN)
        'STDDEV': _out_data[j] = STDDEV(d, /NAN)
        else: Message, 'Method currently not supported: ' + str_equiv(METHOD)
      endcase
      if do_valid then _n_valid[j] = cntv
    endfor
    out_data[*,*,i] = _out_data
    if do_valid then n_valid[*,*,i] = _n_valid
  endfor
  
  if N_ELEMENTS(IN_FWD_TRAFO) eq 0 then undefine, dd
  
  if do_valid then n_valid = reform(n_valid)
  return, reform(out_data) 
  
end

;+
; :Description:
;    Same as transform_shape but returning a list of structures instead of an array
;    with connectivity. History will decide if it's better/faster or not...
;    (it's not, currently)
;  
;  
; :Params:
;    SHPFILE: in, required
;             the shapefile to read (.shp). If not set, a dialog window will open  
;  
; :Keywords:
;    
;    SHP_SRC: in, optional
;             the shapefile coordinate system (datum or proj) default is WGS-84
;    REMOVE_ENTITITES:in, optional, type = long
;                     an array containing the id of the shape entities to remove from the shape
;                     All other entities are plotted normally.
;    KEEP_ENTITITES:in, optional, type = long
;                   an array containing the id of the shape entities to keep for the shape. 
;                   All other entities are ignored.
;
; :Returns:
;    Not sure yet
; 
; :History:
;     Written by FaM, 2014
;- 
function w_Grid2D::get_shape, shpfile, $
  SRC=src, $
  REMOVE_ENTITITES=remove_entitites, $
  KEEP_ENTITITES=keep_entitites
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if N_ELEMENTS(shpfile) eq 0 then shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )

  if ~FILE_TEST(shpfile) then MESSAGE, WAVE_Std_Message('shpfile', /FILE)
  
  if N_ELEMENTS(src) eq 0 then GIS_make_datum, ret, src, NAME = 'WGS-84'
  
  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('src', /ARG)
  
  ; read shp file and create polygon object from entities
  shpmodel = OBJ_NEW('IDLffShape', shpfile)
  if ~obj_valid(shpmodel) then MESSAGE, WAVE_Std_Message('shpfile', /FILE)
  
  ;Get the number of entities so we can parse through them
  shpModel->GetProperty, N_ENTITIES=n_ent, ATTRIBUTE_NAMES=attnames, N_ATTRIBUTES=nattr
  _shift = keyword_set(NO_COORD_SHIFT)
  
  ent_indices= lindgen(n_ent)
  if N_ELEMENTS(REMOVE_ENTITITES) ne 0 then utils_array_remove, remove_entitites, ent_indices
  if N_ELEMENTS(KEEP_ENTITITES) ne 0 then ent_indices = keep_entitites
    
  vx = list()
  vy = list()
  inds = list()
  ents = shpmodel->GetEntity(/ALL, /ATTRIBUTES)
  nents = ents.n_vertices
  verts = ents.vertices
  i0 = 0L
  foreach v, verts, i do begin
    vx->Add, (*v)[0,*], /EXTRACT
    vy->Add, (*v)[1,*], /EXTRACT
    inds->Add, i0 + lindgen(nents[i])
    i0 += nents[i]
  endforeach
  vx = vx->ToArray()
  vy = vy->ToArray()
  self->transform, vx, vy, vx, vy, SRC=src
  
  out = list()
  n_ent = N_ELEMENTS(ent_indices)
  for i=0L, n_ent-1 do begin
    id = ent_indices[i]
    ent = ents[id]
    _x = vx[inds[id]]
    _y = vy[inds[id]]
    n_vert = n_elements(_x)
    h = {}
    for k=0, nattr-1 do h = create_struct(attnames[k], (*(ent.attributes)).(k), h)
    
    h = create_struct('n_parts', ent.n_parts, h)
    h = create_struct('parts', list(), h)
    if h.n_parts le 1 then begin
      h.n_parts = 1
      h.parts.add, {x:_x, y:_y}
    endif else begin
       parts = [*ent.parts, n_vert]
       for k=0L, ent.n_parts-1 do begin
         h.parts.add, {x:_x[(parts[k]):(parts[k+1]-1)], $
          y:_y[(parts[k]):(parts[k+1]-1)]}
       endfor
    endelse
    out->add, h
  endfor
  
  return, out
  
end

;+
; :Description:
;    Transforms the coordinates of a shape file into grid coordinates.
;    
;    The entities are organised using a connectivity array (see example).
;    
;    Default behavior is to consider the grid as an image grid, (grid coordinates 
;    located in the center of the pixel), and therefore to shift the transformed
;    coordinates of a half pixel down left. This is a good default for all common
;    applications (w_Map), but if you want to avoid this you can set the 
;    `NO_COORD_SHIFT` keyword to 1.
; 
; :Params:
;    SHPFILE: in, required
;             the shapefile to read (.shp). If not set, a dialog window will open  
;    
;    x: out
;       the X shape coordinates
;    y: out
;       the Y shape coordinates
;    conn: out
;          the connectivity array
;    
; :Keywords:
;    
;    SHP_SRC: in, optional
;             the shapefile coordinate system (datum or proj) default is WGS-84
;    
;    
;    ENTRULE:in, optional, type=string
;            the name of a compiled FUNCTION to call at each
;            iteration over the shapefile entities. It can be used
;            to e.g filter entities after shapefile specific criterias.
;            the function has to return 1 if the entity must be kept, 0 if not
;            the function must accept two arguments, entity and i (index of the entity),
;            and two keywords: ATTRIBUTE_NAMES and N_ATTRIBUTES
;                     
;    REMOVE_ENTITITES:in, optional, type = long
;                     an array containing the id of the shape entities to remove from the shape
;                     All other entities are plotted normally.
;                     
;    KEEP_ENTITITES:in, optional, type = long
;                   an array containing the id of the shape entities to keep for the shape. 
;                   All other entities are ignored.
;
;    NO_COORD_SHIFT: in, optional, type = boolean
;                    prevent the shift of the coordinates of a half pixel.
;
;    MARK_INTERIOR: in, optional, type = boolean
;                   this is for shapes with islands in it, and if you want
;                   to make ROIs with them or filled plot them
;
;
; :Examples:
;    
;    Plot the world boundaries on a device::
;    
;      grid = OBJ_NEW('w_TRMM')
;      
;      GIS_make_datum, ret, shp_src, NAME = 'WGS-84'
;      shpf = WAVE_resource_dir+'/shapes/world_borders/world_borders.shp'
;      grid->transform_shape, shpf, x, y, conn, SHP_SRC=shp_src
;      
;      grid->Get_XY, dummyx, dummyy, nx, ny, proj
;      cgDisplay, nx, ny, /FREE
;     
;      index = 0
;      while index lt N_ELEMENTS(conn) do begin
;        nbElperConn = conn[index]
;        idx = conn[index+1:index+nbElperConn]
;        index += nbElperConn + 1
;        cgPlots, x[idx], y[idx], /DEVICE, COLOR='black'
;      endwhile
;      
;      undefine, grid
; 
; :History:
;     Written by FaM, 2011.
;- 
pro w_Grid2D::transform_shape, shpfile, x, y, conn, $
  SHP_SRC=shp_src, $
  ENTRULE=entrule, $
  REMOVE_ENTITITES=remove_entitites, $
  KEEP_ENTITITES=keep_entitites, $
  NO_COORD_SHIFT=no_coord_shift, $
  MARK_INTERIOR=mark_interior
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  ON_ERROR, 2

  undefine, x, y, conn
  
  if N_ELEMENTS(shpfile) eq 0 then shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )
  
  if ~FILE_TEST(shpfile) then MESSAGE, WAVE_Std_Message('shpfile', FILE=shpfile)
  
  if N_ELEMENTS(shp_src) eq 0 then begin
   GIS_make_datum, ret, shp_src, NAME = 'WGS-84'
  endif
  
  _mi = KEYWORD_SET(MARK_INTERIOR)
  
  if arg_okay(shp_src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(shp_src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('shp_src', /ARG)
  
  ; Make boundaries to spare computations 
  if is_dat then begin
   self->get_LonLat, glon, glat
   p = where(glon gt 180, cnt)
   if cnt ne 0 then glon[p] = glon[p] - 360
   range = [min(glon),max(glon),min(glat),max(glat)]   
  end
  if is_proj then begin   
   range = [-99999999999d,99999999999d,-99999999999d,99999999999d] ; TODO: decide a range if the shape is not in LL coordinates
  end

  SetDefaultValue, _entrule, N_ELEMENTS(ENTRULE) ne 0
  
  ; read shp file and create polygon object from entities
  shpmodel = OBJ_NEW('IDLffShape',shpfile)
  if ~OBJ_VALID(shpmodel) then MESSAGE, WAVE_Std_Message('shpfile', /FILE)
  
  ;Get the number of entities so we can parse through them
  shpModel->GetProperty, N_ENTITIES=N_ent, ATTRIBUTE_NAMES=attnames, N_ATTRIBUTES=nattr
  
  entities = LINDGEN(N_ent)
  if N_ELEMENTS(REMOVE_ENTITITES) ne 0 then utils_array_remove, remove_entitites, entities
  if N_ELEMENTS(KEEP_ENTITITES) ne 0 then entities = keep_entitites
  
  N_ent = N_ELEMENTS(entities)
  n_coord = 0L

  mg_x = obj_new('MGcoArrayList', type=IDL_DOUBLE)
  mg_y = obj_new('MGcoArrayList', type=IDL_DOUBLE)
  mg_conn = obj_new('MGcoArrayList', type=IDL_LONG)
  for i=0L, N_ent-1 do begin
    ent = shpmodel->GetEntity(entities[i], /ATTRIBUTES)
    
    _x = (*ent.vertices)[0,*]
    _y = (*ent.vertices)[1,*]
    n_vert = n_elements(_x)
    
    if  min(_y) gt range[3] $
      || max(_y) lt range[2] $
      || min(_x) gt range[1] $
      || max(_x) lt range[0] then continue

    if _entrule && ~CALL_FUNCTION(ENTRULE, ent, i, $
      ATTRIBUTE_NAMES=attnames, N_ATTRIBUTES=nattr) then continue
    
    mg_x->add, _x[*]
    mg_y->add, _y[*]
    
    if _mi then begin
      parts = *ent.parts
      for k=0L, ent.n_parts-1 do begin
        if k eq ent.n_parts-1 then begin
          n_vert = ent.n_vertices - parts[k]
          next_is = 0
        endif else begin
          n_vert = parts[k+1]-parts[k]
          next_is = 1
        endelse
        mg_conn->add, [n_vert, next_is, (lindgen(n_vert)) + n_coord]
        n_coord += n_vert
      endfor
    endif else begin
      parts = [*ent.parts, ent.n_vertices]
      for k=0L, ent.n_parts-1 do begin
        n_vert = parts[k+1]-parts[k]
        mg_conn->add, [n_vert, (lindgen(n_vert)) + n_coord]
        n_coord += n_vert
      endfor
    endelse
    shpmodel->IDLffShape::DestroyEntity, ent
    
  endfor
  
  x = mg_x->get(/all)
  y = mg_y->get(/all)
  conn = mg_conn->get(/all)
  if N_ELEMENTS(conn) eq 1 and conn[0] eq -1 then undefine, conn
  
  ; clean unused objects
  undefine, shpModel, mg_x, mg_y, mg_conn
    
  if N_ELEMENTS(CONN) eq 0 then begin
   message, 'Did not find anything in the shapefile that matches to the grid (' + FILE_BASENAME(shpfile) +')', /INFORMATIONAL
   undefine, x, y, conn
   return
  endif  
  if FILE_BASENAME(shpfile) eq '10m_ocean.shp' then y = y < 90. ;TODO: dirty temporary workaround
  self->transform, x, y, x, y, SRC = shp_src
  
   ; Because Center point of the pixel is not the true coord
  if ~ KEYWORD_SET(NO_COORD_SHIFT) then begin
    x -= 0.5
    y -= 0.5
  endif

end

;+
; :Description:
;    Define a Region of Interest (ROI) within the object grid.
;    
;    This can be usefull in many cases, for example to compute a mask
;    from a polygon or a shape file. In this case, one can obtain the 
;    computed mask with the `w_Grid_2D::get_ROI` procedure. 
;    
;    It is recommended to delete the ROI afterwards (`w_Grid_2D::destroy_ROI` or `w_Grid_2D::set_ROI` 
;    without arguments), since the behavior of the grid is changed once a ROI have been
;    defined. If the grid is used as SOURCE grid in transformations between grids 
;    using `w_Grid_2D::map_gridded_data`, the transformation will concern only the
;    zone included into the ROI.
;    
;    Currently one can define a ROI with 6 different methods::
;      1. SHAPE: use a .shp file as imput, the ROI is generated using the 
;                IDLanROI object to transform the vertices into a mask
;                (don't forget to set the SRC keyword)
;      2. POLYGON: similar to SHAPE but the polygon is directly specified
;                 (don't forget to set the SRC keyword)
;      3. MASK: simply define the ROI using a mask (of dimensions corresponding to the grid)
;      4. CROPBORDER: remove a certain number of pixels from the borders of the grid
;      5. GRID: the ROI is defined by the position of another grid within the object grid
;      6. CORNERS: the ROI is defined by setting the LL and UR corners of the desired subset
;                 (don't forget to set the SRC keyword)
;
;
; :Keywords:
;    SHAPE: in, type = string
;           the shapefile to read (.shp), coordinate system defined by `SRC`
;    POLYGON: in, type = array
;             a 2xN array with the columns being the x, y coordinates, in the 
;             coordinate system defined by `SRC`
;    MASK: in, type = array
;          a mask of the desired ROI. If no grid SRC is given, the mask
;          is assumed to be in the orginal grid projection, so that the
;          dimensions of the mask must match the grid [nx, ny]. If the 
;          mask is in any other gridded projection, set SRC with the 
;          corresponding w_grid2d
;    CROPBORDER: in, type = long
;                number of pixels to remove from each border
;    GRID: in, type = w_Grid2D
;          a grig object
;    CORNERS: in, type = array 
;            LL and UR corners of the desired subset ([XLL,YLL,XDR,YDR]), coordinate system defined by `SRC`
;    NO_ERASE: in, type = boolean
;              set this keyword to update the ROI in place of replacing it
;    SRC: in, optional
;         the polygon or shape coordinate system (datum or proj) default is WGS-84
;    ENTRULE:in, optional, type=string
;            the name of a compiled FUNCTION to call at each
;            iteration over the shapefile entities. It can be used
;            to e.g filter entities after shapefile specific criterias.
;            the function has to return 1 if the entity must be kept, 0 if not
;            the function must accept two arguments, entity and i (index of the entity)
;    REMOVE_ENTITITES:in, optional, type = long
;                     an array containing the id of the shape entities to remove from the shape
;                     All other entities are plotted normally.
;    KEEP_ENTITITES:in, optional, type = long
;                   an array containing the id of the shape entities to keep for the shape. 
;                   All other entities are ignored.
;    ROI_MASK_RULE: Set this keyword to an integer specifying the rule used to determine whether
;                   a given pixel should be set within the mask when computing a mask with
;                   polygons or shapes. Valid values include::
;                       * 0 = Boundary only. All pixels falling on a region's boundary are set.
;                       * 1 = Interior only. All pixels falling within the region's boundary, but not on the boundary, are set.
;                       * 2 = Boundary + Interior. All pixels falling on or within a region's boundary are set.
;                       * 3 = Pixel center point is used to test the appartenance to the ROI (requires more computing time)
;                             This is the default!
;    CHECK_INTERIOR: in, type=boolean
;                    Standard shapes' entities use 'parts' do define interior (island) vertices.
;                    If this is not the case (corrupted or bad quality files), set this keyword 
;                    to systematically check if new verticies belong to a new entity or are in
;                    the interior of the previous one. This requires much more computing time.
;
; :Returns:
;   1 if the ROI has been set correctly, 0 if not
;
; :History:
;     Written by FaM, 2011.
;
;-
function w_Grid2D::set_ROI, SHAPE=shape,  $
                            POLYGON=polygon, MASK=mask,  $
                            CROPBORDER=cropborder,  $
                            GRID=grid,    $                          
                            CORNERS=corners, $
                            NO_ERASE=no_erase, $ 
                            SRC=src, $
                            ENTRULE=entrule, $
                            REMOVE_ENTITITES=remove_entitites, $ 
                            KEEP_ENTITITES=keep_entitites, $
                            ROI_MASK_RULE=roi_mask_rule, $
                            CHECK_INTERIOR=check_interior

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    self->Destroy_ROI
    ok = self->set_ROI()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  do_shape = N_ELEMENTS(SHAPE) ne 0
  do_mask = N_ELEMENTS(MASK) ne 0
  do_grid = N_ELEMENTS(GRID) ne 0
  do_polygon = N_ELEMENTS(POLYGON) ne 0
  do_border = N_ELEMENTS(CROPBORDER) ne 0
  do_corner = N_ELEMENTS(CORNERS) ne 0
  
  if N_ELEMENTS(ROI_MASK_RULE) eq 0 then _roi_mask_rule = 3 else _roi_mask_rule = roi_mask_rule
  
  check_k = [do_shape, do_polygon, do_mask, do_border, do_grid, do_corner]
  if total(check_k) eq 0 then begin
    self->Destroy_ROI
    return, 1
  endif  
  if total(check_k) ne 1 then MESSAGE, 'Ambiguous keyword combination. Set one and only one ROI definition method.'
  
  check_int = KEYWORD_SET(CHECK_INTERIOR)
  
  if KEYWORD_SET(NO_ERASE) and self.is_roi then _mask = *self.roi
  
  if do_shape then begin
    self->transform_shape, shape, x, y, conn, SHP_SRC=src, REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites, $
      /NO_COORD_SHIFT, /MARK_INTERIOR, ENTRULE=entrule
    if N_ELEMENTS(x) eq 0 then Message, 'Nothing usable in the shapefile: ' + shape
    if _roi_mask_rule eq 3 then begin
      utils_1d_to_2d, INDGEN(self.tnt_c.nx), INDGEN(self.tnt_c.ny), i, j
      if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
    endif
    index = 0
    is_int = 0
    while index lt N_ELEMENTS(conn) do begin
      nbElperConn = conn[index]
      next_is = conn[index+1]
      idx = conn[index+2:index+nbElperConn+1]
      index += nbElperConn + 2
      if ~ OBJ_VALID(roi) then roi = OBJ_NEW('w_ROIGroup')
      roi_ = OBJ_NEW('IDLanROI', x[idx], y[idx])
      if is_int and check_int then begin
        p_in = where(roi->ContainsPoints(x[idx[0]], y[idx[0]]) eq 1, cnt_in)
        if cnt_in eq 0 then roi_->SetProperty, INTERIOR=0 else roi_->SetProperty, INTERIOR=1
      endif else begin
        roi_->SetProperty, INTERIOR=is_int
      endelse
      roi->Add,roi_
      if next_is eq 0 then begin
        if _roi_mask_rule eq 3 then begin
          pok = where(roi->ComputeMask(DIMENSIONS=[self.tnt_c.nx,self.tnt_c.ny], MASK_RULE=2), cntok)
          if cntok ne 0 then begin
            _i = i[pok]
            _j = j[pok]
            cont = roi->ContainsPoints(_i, _j)
            p_in = where(cont ge 1, cnt_in)
            if cnt_in ne 0 then _mask[_i[p_in],_j[p_in]] = 1
          endif
        endif else begin
          if N_ELEMENTS(_mask) eq 0 then _mask = roi->ComputeMask(DIMENSIONS=[self.tnt_c.nx,self.tnt_c.ny], MASK_RULE=_roi_mask_rule) $
          else _mask = roi->ComputeMask(MASK_IN=_mask, MASK_RULE=_roi_mask_rule)
        endelse
        OBJ_DESTROY, roi
      endif
      is_int = next_is
    endwhile
    _mask = _mask < 1B
  endif
  
  if do_polygon then begin  
    if ~ arg_okay(polygon, N_DIM=2) then Message, WAVE_Std_Message("POLYGON", DIMARRAY=2)
    dims = SIZE(polygon, /DIMENSIONS)
    if dims[0] ne 2 then Message, '$POLYGON should be a 2xN array'
    if dims[1] lt 3 then Message, '$POLYGON should contain at least 3 points'
    x = reform(polygon[0,*])
    y = reform(polygon[1,*])
    if N_ELEMENTS(SRC) ne 0 then begin ; To the grid projetion
      self->transform, x, y, x, y, SRC=src
    endif    
    roi = OBJ_NEW('IDLanROI', x, y)
    if _roi_mask_rule eq 3 then begin
      utils_1d_to_2d, INDGEN(self.tnt_c.nx), INDGEN(self.tnt_c.ny), i, j
      if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
      cont = roi->ContainsPoints(i, j)
      p_in = where(cont ge 1, cnt_in)
      if cnt_in ne 0 then _mask[p_in] = 1
    endif else begin
      if N_ELEMENTS(_mask) eq 0 then _mask = roi->ComputeMask(DIMENSIONS=[self.tnt_c.nx,self.tnt_c.ny], MASK_RULE=_roi_mask_rule) $
      else _mask = roi->ComputeMask(MASK_IN=_mask, MASK_RULE=_roi_mask_rule)
    endelse
    OBJ_DESTROY, roi
    _mask = _mask < 1B
  endif
  
  if do_mask then begin
    if N_ELEMENTS(SRC) ne 0 then begin
      ; Transform case
      if ~arg_okay(mask, /NUMERIC, N_DIM=2) then Message, WAVE_Std_Message('MASK', NDIMS=2)
      if not OBJ_ISA(src, 'w_Grid2D')  then Message, 'With MASK the SRC should be a grid'
      if min(mask) lt 0 or max(mask) ne 1 then Message, 'The input mask is either empty or full.'
      self->Get_XY, xx, yy
      src->transform_XY, xx, yy, self.tnt_c.proj, ii, jj, /NEAREST, /MARK_EXTERIOR
      p1 = where(ii ne -1 and jj ne -1, cnt)
      if cnt ne 0 then begin
        ; ok, some points are in the mask grid. Now check if they are in the mask
        p2 = where(mask[ii[p1], jj[p1]], cnt)
        if cnt ne 0 then begin
          if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
          _mask[p1[p2]] = 1B
        endif     
      endif  
    endif else begin
      ;"easy" case
      if ~arg_okay(mask, /NUMERIC, DIM=[self.tnt_c.nx,self.tnt_c.ny]) then Message, WAVE_Std_Message('MASK', DIMARRAY=[self.tnt_c.nx,self.tnt_c.ny])
      if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
      p = where(mask ne 0, cnt)
      if cnt ne 0 then _mask[p] = 1B
    endelse
  endif
  
  if do_grid then begin
    if not OBJ_ISA(GRID, 'w_Grid2D')  then Message, WAVE_Std_Message('GRID', OBJ='w_Grid2D')
    grid->getProperty, NX=nx,NY=ny
    if N_ELEMENTS(_mask) eq 0 then _mask = self->map_gridded_data(BYTARR(nx,ny) + 1B, grid, MISSING=0B) $
     else _mask = self->map_gridded_data(BYTARR(nx,ny) + 1B, grid, MISSING=0B, DATA_DST=_mask)    
  endif
  
  if do_border then begin
    if ~ arg_okay(CROPBORDER, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('CROPBORDER', /ARG)
    if cropborder lt 0 or cropborder ge self.tnt_c.nx / 2 then message, WAVE_Std_Message('CROPBORDER', /RANGE)
    if cropborder lt 0 or cropborder ge self.tnt_c.ny / 2 then message, WAVE_Std_Message('CROPBORDER', /RANGE)
    mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
    mask[cropborder:self.tnt_c.nx-1-cropborder,cropborder:self.tnt_c.ny-1-cropborder] = 1B
    if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
    p = where(mask ne 0, cnt)
    if cnt ne 0 then _mask[p] = 1B
  endif
  
  if do_corner then begin
    if ~ arg_okay(CORNERS, /ARRAY, /NUMERIC, N_ELEM=4) then Message, WAVE_Std_Message('CORNERS', /ARG)
    x = CORNERS[[0,2]]
    y = CORNERS[[1,3]]
    self->transform, x, y, i, j, SRC=src, /NEAREST
    if i[0] gt i[1] then message, WAVE_Std_Message('CORNERS', /RANGE)
    if j[0] gt j[1] then message, WAVE_Std_Message('CORNERS', /RANGE)
    if i[0] lt 0 then begin
      message, 'X Down left corner out of bounds: set to 0', /INFORMATIONAL
      i[0] = 0
    endif
    if j[0] lt 0 then begin
      message, 'Y Down left corner out of bounds: set to 0', /INFORMATIONAL
      j[0] = 0
    endif
    if i[1] ge self.tnt_c.nx then begin
      message, 'X Upper right corner out of bounds: set to nx-1', /INFORMATIONAL
      i[1] = self.tnt_c.nx-1
    endif
    if j[1] ge self.tnt_c.ny then begin
      message, 'Y Upper right corner out of bounds: set to ny-1', /INFORMATIONAL
      j[1] = self.tnt_c.ny-1
    endif
    mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
    mask[i[0]:i[1],j[0]:j[1]] = 1B
    if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
    p = where(mask ne 0, cnt)
    if cnt ne 0 then _mask[p] = 1B
    isubs = [i[0], j[0], i[1], j[1]]
  endif
  
  if min(_mask) lt 0 or max(_mask) ne 1 then Message, 'The mask is either empty or full.'
  
  self->destroy_ROI 
  self.is_roi = TRUE
  self.roi = PTR_NEW(_mask, /NO_COPY)  
  
  return, 1
  
end

function w_Grid2D::roi_to_mask, SHAPE=shape,  $
                            POLYGON=polygon, MASK=mask,  $
                            CROPBORDER=cropborder,  $
                            GRID=grid,    $                          
                            CORNERS=corners, $
                            NO_ERASE=no_erase, $ 
                            SRC=src, $
                            REMOVE_ENTITITES=remove_entitites, $ 
                            KEEP_ENTITITES=keep_entitites, $
                            ROI_MASK_RULE=roi_mask_rule
   
   if ~ self->set_ROI(SHAPE=shape,  $
                            POLYGON=polygon, MASK=mask,  $
                            CROPBORDER=cropborder,  $
                            GRID=grid,    $                          
                            CORNERS=corners, $
                            NO_ERASE=no_erase, $ 
                            SRC=src, $
                            REMOVE_ENTITITES=remove_entitites, $ 
                            KEEP_ENTITITES=keep_entitites, $
                            ROI_MASK_RULE=roi_mask_rule) then message, 'Error in the ROI generation' 
  
  
  self->get_ROI, MASK=mask
  if ~ self->set_ROI() then message, 'Error in the ROI destruction' 
  
  return, mask
                            
end