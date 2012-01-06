; docformat = 'rst'

;+
; 
; w_Grid2D is a central class in the WAVE library. All geolocalised
; gridded dataset inherit w_Grid2D and can use the w_Grid2D methods. 
; These are mainly transformation tools from Grid to Grid or 
; Lon-Lat to grid, etc.       
;       
; The transformation computations are internally made by the TNT GIS library:
; the w_Grid2D class simply provides an object oriented encapsulation of
; the GIS routines, as well as several improvements, such as remapping,
; interpolation, LonLat to grid transformations, etc.
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
; One has to be carefull when defining new gridded datasets.
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
    tnt_c = tnt_c     ,  $   
    proj = proj       ,  $ 
    meta = meta ) then return, 0  
    
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
;   To retrieve the ROI in the form of a mask or a shape file
;   (if a ROI has been previously set)
;
; :Keywords:
;    MASK: out
;          the ROI mask
;    SUBSET: out
;           the smallest subset ([x0_dl, nx, y0_dl, ny]) surrounding the ROI
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
  if ~self.is_roi then mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny) + 1B else mask = *self.roi  
  
  p = where(mask eq 1, cnt)
  inds = ARRAY_INDICES(mask, p)   
  xmin = min(inds[0,*])
  xmax = max(inds[0,*])
  ymin = min(inds[1,*])
  ymax = max(inds[1,*])  
  subset = [xmin, (xmax-xmin)+1, ymin, (ymax-ymin)+1]
  
  IF arg_okay(MARGIN, /INTEGER) then subset += [-margin,margin*2,-margin,margin*2]
    
    
END

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
;             
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform, x, y, i_dst, j_dst, SRC = src, LON_DST=lon_dst, LAT_DST=lat_dst, E_DST=E_dst, N_DST=N_dst, NEAREST=nearest
 
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
;             
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform_LonLat, lon, lat, datum, i, j, NEAREST = nearest
 
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
  
  self->transform, lon, lat, i, j, SRC = datum, NEAREST=nearest
        
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

; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform_XY, x, y, proj, i, j, NEAREST = nearest

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
  
  self->transform, x, y, i, j, SRC = proj, NEAREST=nearest
  
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
;
; :History:
;      Written by FaM, 2010.
;-
PRO w_Grid2D::transform_IJ, i_src, j_src, grid, i, j, NEAREST = nearest

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
  
  self->transform, i_src, j_src, i, j, SRC = grid, NEAREST=nearest
  
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
;
; :Returns:
;    the transformed data array in the grid (same X Y dims as the grid)
;
; :History:
;      Written by FaM, 2011.
;-
function w_Grid2D::map_lonlat_data, data, lon, lat, SRC=src, MISSING=missing                 

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
  
  siz_src = SIZE(data) 
  if siz_src[0] le 2 then nt = 1 else if siz_src[0] eq 3 then nt = siz_src[3] else  Message, WAVE_Std_Message('data', /ARG)
  nc = N_ELEMENTS(data[*,*,0])
  
  if nc ne N_ELEMENTS(lon) or nc ne N_ELEMENTS(lat) $ 
   then message, 'Data and coordinates incompatible'
   
  ; Define output size
  type = SIZE(data,/TYPE)
  tdata = MAKE_ARRAY(self.tnt_c.nx, self.tnt_c.ny, nt, TYPE=type)
  tdata[*] = missing
   
  self->transform_LonLat, lon, lat, src, i, j
  TRIANGULATE, i, j, triangles
  for t=0, nt-1 do begin
    temp = tdata[*,*,t]
    temp[*] = GRIDDATA(i, j, data, START=[0D,0D], DELTA=[1D,1D], DIMENSION=[self.tnt_c.nx,self.tnt_c.ny], $
    TRIANGLES=triangles, /NEAREST_NEIGHBOR)        
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
;    src_grid: in,  type = w_Grid2D
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
; :History:
;      Written by FaM, 2010.
;-
function w_Grid2D::map_gridded_data, data, src_grid, MISSING = missing, BILINEAR = bilinear, CUBIC=cubic, DATA_DST=data_dst, TO_ROI=to_roi
     
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
        ELSE: missing = -999
      ENDCASE
  endif
  
  bili = KEYWORD_SET(BILINEAR)
  cubic = KEYWORD_SET(CUBIC)
  if ~cubic and ~bili then NEAREST = TRUE ;Otherwize, we need the decimals of course
  
  src_grid->getProperty, tnt_c = src_c  
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
  if NEAREST then p_out = where((i_dst lt 0) or (j_dst lt 0) or (i_dst ge mx) or (j_dst ge my), cnt_out) $ ; OUT of range $
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
;    This routine resamples a any other grid into the object grid. This is very
;    usefull to gather any kind of informations (see example).
;       
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    src_grid: in,  type = w_Grid2D
;              the data grid (w_Grid2D Object)
;
; :Keywords:
;
; :Returns:
;    An array of pointers, of the same dimensions of the object grid. Each valid pointer contains 
;    an array of indices into the argument data grid (an empty pointer means no element was found0)
;    
; :Examples:
; 
;    Open data sets:: 
;      lst = OBJ_NEW('w_MODIS', FILE=TEST_file_directory() + 'MODIS/'+'MOD11A2.A2008297.h25v05.005.2008311141349.hdf') ; MODIS grid
;      wrf = OBJ_NEW('w_WRF', FILE= TEST_file_directory() + 'WRF/wrfout_d02_2008-10-26', CROPBORDER=12); WRF grid
;    
;    Get the array of pointers:: 
;      dd = wrf->resample_grid(lst) ; array of pointers into the modis grid
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
; :History:
;      Written by FaM, 2011.
;-
function w_Grid2D::resample_grid, src_grid
     
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
  
  pok = where((i_dst ge 0) and (j_dst ge 0) and (i_dst lt self.tnt_c.nx) and (j_dst lt self.tnt_c.ny), cnt_ok)  ; in the range
  
  out=ptrarr(self.tnt_c.nx, self.tnt_c.ny)
  
  if cnt_ok ne 0 then begin ; we found pixels  
    o_inds = xi[pok] + src_c.nx * yi[pok]
    inds = self.tnt_c.nx * j_dst[pok] + i_dst[pok]
    avail_inds = (inds[UNIQ(inds, SORT(inds))]) ; some optimisation (not very nice)    
    for i=0, N_ELEMENTS(avail_inds) - 1 do out[avail_inds[i]] = PTR_NEW(o_inds[where(inds eq avail_inds[i])])
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
;    MARGIN: in
;            set to a positive integer value to add a margin to the subset
;            (MARGIN=1 will put one grid point on each side of the subset, so two
;             more columns per dimension in total)
; :Returns:
;     A new w_Grid2D, which is a resampled version of the object grid
;     
; :History:
;      Written by FaM, 2010.
;-
function w_Grid2D::reGrid, Xsize=Xsize, Ysize=Ysize, FACTOR=factor, TO_ROI=to_roi, MARGIN=margin

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  if ~ arg_okay(FACTOR, /NUMERIC) then factor = 1
  
  if N_ELEMENTS(Xsize) ne 0 and N_ELEMENTS(Ysize) ne 0 then Message, 'Ambiguous keyword combination.' 
  if N_ELEMENTS(Xsize) ne 0 then factor = double(Xsize) / self.tnt_c.nx
  if N_ELEMENTS(Ysize) ne 0 then factor = double(Ysize) / self.tnt_c.ny
  
  x0 = self.tnt_c.x0
  y0 = self.tnt_c.y0
  nx = self.tnt_c.nx
  ny = self.tnt_c.ny
  
  if self->is_ROI() and KEYWORD_SET(TO_ROI) then begin
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
  
  return, OBJ_NEW('w_Grid2D', x0=x0, y0=y0, nx=nx, ny=ny, dx=dx, dy=dy, PROJ=self.tnt_c.proj, META=self.meta + ' resampled (factor ' +str_equiv(factor) + ')')
    
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
pro w_Grid2D::transform_shape, shpfile, x, y, conn, SHP_SRC = shp_src, REMOVE_ENTITITES = remove_entitites, KEEP_ENTITITES = keep_entitites, NO_COORD_SHIFT = no_coord_shift
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  ON_ERROR, 2

  undefine, x, y, conn
  
  if N_ELEMENTS(shpfile) eq 0 then shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )
  
  if ~FILE_TEST(shpfile) then MESSAGE, WAVE_Std_Message('shpfile', /FILE)
  
  if N_ELEMENTS(shp_src) eq 0 then begin
   MESSAGE, '$SHP_SRC is not set. Setting to WGS-84' , /INFORMATIONAL
   GIS_make_datum, ret, shp_src, NAME = 'WGS-84'
  endif
  
  if arg_okay(shp_src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(shp_src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('shp_src', /ARG)
  
  ;****************************************
  ; Make boundaries to spare computations *
  ;****************************************
  if is_dat then begin
   self->get_LonLat, glon, glat
   p = where(glon gt 180, cnt)
   if cnt ne 0 then glon[p] = glon[p] - 360
   range = [min(glon),max(glon),min(glat),max(glat)]   
  end
  if is_proj then begin   
   range = [-99999999999d,99999999999d,-99999999999d,99999999999d] ; TODO: decide a range if the shape is not in LL coordinates
  end
  
  ; read shp file and create polygon object from entities
  shpmodel = OBJ_NEW('IDLffShape',shpfile)
  if ~OBJ_VALID(shpmodel) then MESSAGE, WAVE_Std_Message('shpfile', /FILE)
  
  ;Get the number of entities so we can parse through them
  shpModel->GetProperty, N_ENTITIES=N_ent
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
    if not ptr_valid(ent.vertices) then continue
    
    _x = reform((*ent.vertices)[0,*])
    _y = reform((*ent.vertices)[1,*])
    n_vert = n_elements(_x)    
    
    if n_vert lt 3 $
    or min(_y) gt range[3] $ 
    or max(_y) lt range[2] $ 
    or min(_x) gt range[1] $ 
    or max(_x) lt range[0] then begin
      shpmodel->IDLffShape::DestroyEntity, ent 
      continue
    endif
    
    mg_x->add, _x
    mg_y->add, _y    
        
    parts = *ent.parts
    for k=0L, ent.n_parts-1 do begin
      if k eq ent.n_parts-1 then n_vert = ent.n_vertices - parts[k]  else n_vert = parts[k+1]-parts[k]
      mg_conn->add, [n_vert, (lindgen(n_vert)) + n_coord]
      n_coord += n_vert      
    endfor           
    shpmodel->IDLffShape::DestroyEntity, ent 
  endfor  
  
  x = mg_x->get(/all)
  y = mg_y->get(/all)
  conn = mg_conn->get(/all)
  if N_ELEMENTS(conn) eq 1 and conn[0] eq -1 then undefine, conn
  
  ; clean unused objects
  undefine, shpModel, mg_x, mg_y, mg_conn
    
  if N_ELEMENTS(CONN) eq 0 then begin
   message, 'Did not find anything in the shapefile that matches to the grid.', /INFORMATIONAL
   undefine, x, y, conn
   return
  endif  
  
  self->transform, x, y, x, y, SRC = shp_src

  if ~ KEYWORD_SET(NO_COORD_SHIFT) then begin ; Because Center point of the pixel is not the true coord
    x = x + 0.5
    y = y + 0.5
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
;             not implemented yet, coordinate system defined by `SRC`
;    MASK: in, type = array
;          a mask of the desired ROI (dimensions must match the grid)
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
;                       * 2 = Boundary + Interior. All pixels falling on or within a region's boundary are set. This is the default.
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
                            REMOVE_ENTITITES=remove_entitites, $ 
                            KEEP_ENTITITES=keep_entitites, $
                            ROI_MASK_RULE=roi_mask_rule

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
  do_polygon = N_ELEMENTS(POLYGON)
  do_border = N_ELEMENTS(CROPBORDER) ne 0
  do_corner = N_ELEMENTS(CORNERS) ne 0
  
  check_k = [do_shape, do_polygon, do_mask, do_border, do_grid, do_corner]
  if total(check_k) eq 0 then begin
    self->Destroy_ROI
    return, 1
  endif  
  if total(check_k) ne 1 then MESSAGE, 'Ambiguous keyword combination. Set one and only one ROI definition method.'
  
  if KEYWORD_SET(NO_ERASE) and self.is_roi then _mask = *self.roi
  
  if do_shape then begin
    self->transform_shape, shape, x, y, conn, SHP_SRC = SRC, REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites, /NO_COORD_SHIFT
    if N_ELEMENTS(x) eq 0 then Message, 'Nothing usable in the shapefile'    
    index = 0
    while index lt N_ELEMENTS(conn) do begin      
      nbElperConn = conn[index]
      idx = conn[index+1:index+nbElperConn]
      index += nbElperConn + 1      
      roi = OBJ_NEW('IDLanROI', x[idx], y[idx])
      if N_ELEMENTS(_mask) eq 0 then _mask = roi->ComputeMask(DIMENSIONS=[self.tnt_c.nx,self.tnt_c.ny], MASK_RULE=roi_mask_rule) $
       else _mask = roi->ComputeMask(MASK_IN=_mask, MASK_RULE=roi_mask_rule)
      OBJ_DESTROY, roi
    endwhile       
    _mask = _mask < 1B    
  endif
  
  if do_mask then begin
    if ~arg_okay(mask, /NUMERIC, DIM=[self.tnt_c.nx,self.tnt_c.ny]) then Message, WAVE_Std_Message('MASK', /ARG)
    if N_ELEMENTS(_mask) eq 0 then _mask = BYTARR(self.tnt_c.nx,self.tnt_c.ny)
    p = where(mask ne 0, cnt)
    if cnt ne 0 then _mask[p] = 1B  
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
    if i[0] ge i[1] then message, WAVE_Std_Message('CORNERS', /RANGE)
    if j[0] ge j[1] then message, WAVE_Std_Message('CORNERS', /RANGE)
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
  
  if do_polygon then Message, 'POLYGON currently not implemented'
  
  if min(_mask) lt 0 or max(_mask) ne 1 then Message, 'The mask is either empty or full.'
  
  self->destroy_ROI 
  self.is_roi = TRUE
  self.roi = PTR_NEW(_mask, /NO_COPY)  
  
  return, 1
  
end