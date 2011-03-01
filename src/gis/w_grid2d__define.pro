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
  
  struct = { w_Grid2D      ,  $
    lon   : PTR_NEW()    ,  $ ; 2D array containing the longitudes of the grid
    lat   : PTR_NEW()    ,  $ ; 2D array containing the latitudes of the grid
    tnt_c : {TNT_COORD}  ,  $ ; intern {TNT_COORD} structure 
    meta  : ''              $ ; If set, a string containg infos about the grid.
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
    and KEYWORD_SET(dx) and KEYWORD_SET(dy) then cas = 1
 
  if KEYWORD_SET(nx) and KEYWORD_SET(ny) and N_ELEMENTS(x1) eq 1 and  N_ELEMENTS(y1)  eq 1 $
    and KEYWORD_SET(dx) and KEYWORD_SET(dy) then cas = 2
  
  if N_ELEMENTS(x0) eq 1 and  N_ELEMENTS(y0)  eq 1 and N_ELEMENTS(x1) eq 1 and  N_ELEMENTS(y1)  eq 1  $
    and KEYWORD_SET(dx) and KEYWORD_SET(dy) then cas = 3
  
  if N_ELEMENTS(x0) eq 1 and  N_ELEMENTS(y0)  eq 1 and N_ELEMENTS(x1) eq 1 and  N_ELEMENTS(y1)  eq 1  $
    and KEYWORD_SET(nx) and KEYWORD_SET(ny) then cas = 4
  
  if KEYWORD_SET(tnt_c) then cas = 5
  
  
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
      tnt_c = tnt_c      
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
  
  tnt_c = {TNT_COORD}
  tnt_c.system = 'PROJ'
  tnt_c.valid = 'POINT'
  tnt_c.nx = nx
  tnt_c.ny = ny
  tnt_c.x0 = x0
  tnt_c.y0 = y0
  tnt_c.x1 = x1
  tnt_c.y1 = y1
  tnt_c.dx = dx 
  tnt_c.dy = dy
  tnt_c.proj = proj
  
  ok = GIS_check_coord(tnt_c, /CORR, ERROR=err)
  if not ok then  Message, 'Input grid parameters do not describe a correct grid: no correction could be made: ' + TNT_err_txt(err)
   
  self.tnt_c = tnt_c
  self.meta = meta
  
  ; Security check
  lon = -1.
  lat = -1.
  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  self.lon = PTR_NEW(lon, /NO_COPY)
  self.lat = PTR_NEW(lat, /NO_COPY)
    
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
;    Destroy prcocedure. 
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
;       lon: out, optional, type = float array 
;               2D array containing the longitudes of the grid 
;               (the longitudes will be computed at the first call and stored
;               as a pointer: by large grids, this may cause some memory problems)
;       lat: out, optional, type = float array 
;               2D array containing the latitudes of the grid 
;               (the latitudes will be computed at the first call and stored
;               as a pointer: by large grids, this may cause some memory problems)
;       tnt_c: out,  optional, type = {TNT_COORD}
;               grid {TNT_COORD} structure 
;       meta: out, optional, type = string
;               a string containg infos about the grid.
;
; :History:
;      Written by FaM, 2010.
;-
pro w_Grid2D::GetProperty,    lon   =  lon   ,  $ ; 2D array containing the longitudes of the grid 
                            lat   =  lat   ,  $ ; 2D array containing the latitudes of the grid 
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
;    SRC: in, required
;         src the initial coordinate system (w_Grid2D or {TNT_PROJ} or {TNT_DATUM}) in which x and y are defined
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
  
  if not OBJ_ISA(grid, 'w_Grid2D')  then Message, WAVE_Std_Message('proj', OBJ='w_Grid2D')
  
  self->transform, i_src, j_src, i, j, SRC = grid, NEAREST=nearest
  
end

;+
; :Description:
;    Generic routine to transform any georeferenced grid without projection
;    into the object grid using nearest neighborhood. (for example lon and lat grid from swath files)
;       
;    The default method is to transform it forwards: the LatLon points are transformed into
;    the grid and the data value is affected to the neirest grid point. This method is not
;    recomended because the end grid may not be filled entirely if its resolution is finer
;    than the lat-lon grid, for example.
;       
;    The second method (BACKWARDS keyword) transforms the grid points into lat-lon and looks
;    for the neirest points into the input lat-lon grid to perform interpolation. This method 
;    is recomended. However, this implies using NN interpolation algorithm into irregular grids,
;    which is very demanding in both memory and processor use. If your LatLon arrays are too
;    large, the program may lag or even go out of memory.
;       
;    TODO: Update routine: ! 05.11.2010: BACKWARDS METHOD NOT IMPLEMENTED YET !
;    ====================================================
;       
;       
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    data: in, required, type = float array
;          the data to map on the object grid. The two first dimenstions are X and Y, the third is handled as time or Z dim 
;    src_datum: in, required, type = {TNT_DATUM}
;               the datum of the grid
;    src_lon: in, required, type = float array
;             the longitudes of the data
;    src_lat: in, required, type = float array
;             the latitudes of the data
;
; :Keywords:
;    MISSING: in, optional
;             value to set to missing values in the final grid. 0. is the default value
;    BACKWARDS: in, optional
;               set to use the backwards method (NOT IMPLEMENTED YET)
;
; :Returns:
;    the transformed data array in the grid (same dims)
;
; :History:
;      Written by FaM, 2010.
;-
function w_Grid2D::map_lonlat_data, data, src_datum, src_lon, src_lat, MISSING = missing, BACKWARDS = backwards

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if not arg_okay(src_datum, STRUCT={TNT_DATUM}) then Message, WAVE_Std_Message('src_datum', STRUCT={TNT_DATUM})
  if ~KEYWORD_SET(missing) then missing = 0
  
  siz = SIZE(data)
  
  if siz[0] eq 1 then nt = 1
  if siz[0] eq 2 then if siz[1] eq N_ELEMENTS(src_lon) then nt = siz[2] else nt = 1
  if siz[0] eq 3 then nt = siz[3]
  
  if KEYWORD_SET(BACKWARDS) then begin
   ;TODO: Update routine: implement this (idea is already here)
  endif else begin
  
    ntot =  N_ELEMENTS(data[*,*,0])
    tot = INDGEN(ntot, /LONG)
    self->transform_LonLat, src_lon[tot], src_lat[tot], src_datum, i, j, /NEAREST
    
    data_grid = DBLARR(self.tnt_c.nx, self.tnt_c.ny, nt)
    data_grid[*] = missing
    
    pok = WHERE(i ge 0 and j ge 0 and i lt self.tnt_c.nx and j lt self.tnt_c.ny, cnt)
    if cnt ne 0 then begin
      i = i[pok]
      j = j[pok]
      tot = tot[pok]
    endif
    
    if nt eq 1 then begin
      data_grid[i, j] = data[tot]
    endif else begin
      tmp = DBLARR(self.tnt_c.nx, self.tnt_c.ny)
      for i = 0L, nt-1 do begin
        tmp *= 0
        tmp[*] =  missing
        if siz[0] eq 2 then tmp[i, j] = (reform(data[*,i]))[tot] else tmp[i, j] = (reform(data[*,*,i]))[tot]
        data_grid[*,*,i] = tmp
      endfor
    endelse
  endelse
  
  return, data_grid
  
end

;+
; :Description:
;    Important routine to transform a data array defined in any other GRID the OBJECT GRID. 
;    Default is to use Neirest Neighbor algorithm, BILINEAR is also implemented.
;       
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    data: in, required, type = float array
;          the data to map on the grid itself. The two first dimenstions are X and Y, the third is handled as time
;    src_grid: in,  type = w_Grid2D
;              the data grid (w_Grid2D Object)
;
; :Keywords:
;    MISSING: in, optional
;             value to set to missing values in the final grid. NaN or 0 are default values depending on the data type
;    BILINEAR: in, optional
;             set to use bilinear interpolation instead of NN
;
; :Returns:
;    The remaped data array, of the same dimensions of the object grid, with an eventual 3rd dim (time/Z)
;    
; :History:
;      Written by FaM, 2010.
;-
function w_Grid2D::map_gridded_data, data, src_grid, MISSING = missing, BILINEAR = bilinear, DATA_DST = DATA_DST
     
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
        'BYTE': missing = 0
        ELSE: missing = -999
      ENDCASE
  endif 

  src_grid->getProperty, tnt_c = src_c  
  utils_1d_to_2d, INDGEN(self.tnt_c.nx, /LONG), -INDGEN(self.tnt_c.ny, /LONG) + self.tnt_c.ny - 1, xi, yi
  
  if ~KEYWORD_SET(BILINEAR) then NEAREST = TRUE
  
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
  
  ;***********************************
  ; Get the data in the source grid  *
  ;***********************************
  p_out = where((i_dst lt 0) or (j_dst lt 0) or (i_dst ge mx) or (j_dst ge my), cnt_out)  ; OUT of range
  bili = KEYWORD_SET(BILINEAR)
    
  for i = 0L, n-1 do begin
    if bili then tmp = BILINEAR((reform(data[*,*,i])), reform(i_dst, self.tnt_c.nx, self.tnt_c.ny), reform(j_dst, self.tnt_c.nx, self.tnt_c.ny)) $
      else tmp = (reform(data[*,*,i]))[i_dst, j_dst]
    if cnt_out ne 0 then begin
      if is_dst then begin
        tmp2 = data_dst[*,*,i]
        tmp[p_out] = tmp2[p_out]
      endif else tmp[p_out] = missing
    endif
    tmp = reform(tmp, self.tnt_c.nx, self.tnt_c.ny)
    if N_ELEMENTS(tdata) eq 0 then tdata = tmp else tdata =[[[tdata]],[[tmp]]]
  endfor
   
  return, tdata
     
end

;+
; :Description:
;    Generic routine to resample the object grid.
;    
; :Categories:
;         WAVE/OBJ_GIS
;
; :Keywords:
;    Xsize:  in, optional, type = integer
;           the new X dimension size (the original X/Y ratio is conserved) 
;    Ysize: in, optional, type = integer
;           the new Y dimension size (the original X/Y ratio is conserved) (if set, Xsize is ignored)
;    FACTOR: in, optional, type = float
;           a factor to multiply to nx and ny (if set, Xsize and Ysize are ignored)
;
; :Returns:
;     A new w_Grid2D, which is a resampled version of the object grid
;     
; :History:
;      Written by FaM, 2010.
;-
function w_Grid2D::reGrid, Xsize = Xsize,  Ysize = Ysize, FACTOR = factor

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  if ~ arg_okay(FACTOR, /NUMERIC) then factor = 1
  
  if KEYWORD_SET(Xsize) then factor = double(Xsize) / self.tnt_c.nx $
  else if KEYWORD_SET(Ysize) then factor = double(Ysize) / self.tnt_c.ny
    
  nx = self.tnt_c.nx * FACTOR
  ny = self.tnt_c.ny * FACTOR
  dx = self.tnt_c.dx / double(FACTOR)
  dy = self.tnt_c.dy / double(FACTOR)
  
  x0 = self.tnt_c.x0 - 0.5d*self.tnt_c.dx + dx/2d
  y0 = self.tnt_c.y0 + 0.5d*self.tnt_c.dy - dy/2d
  
  return, OBJ_NEW('w_Grid2D', x0=x0, y0=y0, nx=nx, ny=ny, dx=dx, dy=dy, PROJ=self.tnt_c.proj, META=self.meta + ' resampled (factor ' +str_equiv(factor) + ')')
    
end


;+
; :Description:
;    This function makes a subsets of the object grid, and of a data
;    array if needed.
;
; :Params:
;    data: in, optional
;          two or three dimensional data array associated to the object grid
;
; :Keywords:
;    CORNERS: in, optional
;             the subset corners in the form: [DLX,DLY,URX,URY].
;    SRC: in, optional
;         the source coordinates of the corners. If not set, the corners are assumed to be indices in the object grid
;    CROPBORDER: in, optional
;                to remove CROPBORDER indexes on the border of the grid
;    OUT_GRID: out
;              the new w_grid_2d object
;    OUT_DATA: out
;              the subseted data
; 
; :Returns:
;    TRUE if everything went fine
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function w_Grid2D::subset, data, CORNERS = corners, SRC = src, CROPBORDER = cropborder, OUT_GRID = out_grid, OUT_DATA = out_data

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF

  isubs = [0, 0, self.tnt_c.nx-1,self.tnt_c.ny-1]
  if KEYWORD_SET(CROPBORDER) then begin
     if ~ arg_okay(CROPBORDER, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('CROPBORDER', /ARG) 
     if cropborder lt 0 or cropborder ge self.tnt_c.nx / 2 then message, WAVE_Std_Message('CROPBORDER', /RANGE)
     if cropborder lt 0 or cropborder ge self.tnt_c.ny / 2 then message, WAVE_Std_Message('CROPBORDER', /RANGE)
     isubs = [cropborder, cropborder, self.tnt_c.nx-1-cropborder, self.tnt_c.ny-1-cropborder]
  endif else if KEYWORD_SET(CORNERS) then begin
     if ~ arg_okay(CORNERS, /ARRAY, /NUMERIC, N_ELEM=4) then Message, WAVE_Std_Message('SUBSET_ij', /ARG)
     x = CORNERS[[0,2]]
     y = CORNERS[[1,3]]     
     self->transform, x, y, i, j, SRC = src, /NEAREST
     if i[0] ge i[1] then message, WAVE_Std_Message('CORNERS', /RANGE)
     if j[0] ge j[1] then message, WAVE_Std_Message('CORNERS', /RANGE)
     if i[0] lt 0 then begin
       message, 'X Down left corner out of bounders: set to 0', /INFORMATIONAL
       i[0] = 0
     endif
     if j[0] lt 0 then begin
       message, 'Y Down left corner out of bounders: set to 0', /INFORMATIONAL
       j[0] = 0
     endif
     if i[1] ge self.tnt_c.nx then begin
       message, 'X Upper right corner out of bounders: set to nx-1', /INFORMATIONAL
       i[1] = self.tnt_c.nx-1
     endif
     if j[1] ge self.tnt_c.ny then begin
       message, 'Y Upper right corner out of bounders: set to ny-1', /INFORMATIONAL
       j[1] = self.tnt_c.ny-1
     endif     
     isubs = [i[0], j[0], i[1], j[1]]     
  endif
  
  x0 = self.tnt_c.x0 + isubs[0] * self.tnt_c.dx
  y0 = self.tnt_c.y0 - (self.tnt_c.ny-1-isubs[3]) * self.tnt_c.dy
  x1 = self.tnt_c.x0 + isubs[2] * self.tnt_c.dx
  y1 = self.tnt_c.y1 + isubs[1] * self.tnt_c.dy
  if ARG_PRESENT(OUT_GRID) then OUT_GRID = OBJ_NEW('w_Grid2D', x0=x0, y0=y0, x1=x1, y1=y1, dx=self.tnt_c.dx, dy=self.tnt_c.dy, PROJ=self.tnt_c.proj, META=self.meta + ' subset')
  if N_ELEMENTS(data) ne 0 then begin
    siz = SIZE(data)
    case (siz[0]) of
      2: begin
        if siz[1] ne self.tnt_c.nx then  MESSAGE, WAVE_Std_Message('data', /ARG)
        if siz[2] ne self.tnt_c.ny then  MESSAGE, WAVE_Std_Message('data', /ARG)
        OUT_DATA = data[isubs[0]:isubs[2],isubs[1]:isubs[3]]        
      end
      3: begin
        if siz[1] ne self.tnt_c.nx then  MESSAGE, WAVE_Std_Message('data', /ARG)
        if siz[2] ne self.tnt_c.ny then  MESSAGE, WAVE_Std_Message('data', /ARG)  
        OUT_DATA = data[isubs[0]:isubs[2],isubs[1]:isubs[3],*]            
      end
      else: begin
        MESSAGE, WAVE_Std_Message('data', /ARG)
      end
    endcase
  endif
    
  return, 1
    
end
