; docformat = 'rst'
;+
; 
; Grid2D is a central class in the WAVE library, superclass from all
; "geolocalised" dataset-classes. It provides the necessary tools to 
; make transformations from geolocalised information to himself.
;       
; Every gridded dataset in WAVE should inherit from this superclass.
;       
; The concrete transformations are actually made by the TNT GIS library:
; the Grid2D class simply provides an object oriented encapsulation of
; the GIS routines, as well as several improvements, such as remapping,
; interpolation, LonLat computation, etc.
;       
; Currently, only POINT valid grids are supported. This should be enough
; for most applications.
;          
; Careful! 
; Althought Grid2D is defined using UL and DR corners to be consistent
; with the GIS library, the convention used in the WAVE is based on the 
; WRF convention, that means: the (i,j) = (0,0) index refers to the 
; DL corner and the (i,j) = (nx-1,ny-1) index refers to the UR corner.
; One has to be carefull when parsing new gridded datasets. One good 
; method is to test the data using `Quickplot`. If it is upside down,
; the array have to be rotated like this: 
; array = rotate(array, 7) 
;        
; Here is a non-exhaustive list of dataset conventions::
;       
;                 DATASET                    To ROTATE
;                -------------------------------------
;                  WRF                        NO
;                  TRMM                       NO
;                  MODIS                      YES
;                  AMSR                       YES
;                =================================================================
;                
;                 Superclass
;                ----------------------
;                 none
;                =================================================================    
;                
;                 Attributes
;               ----------------------
;                 lon   : PTR_NEW()    ,  $ 2D array containing the longitudes of the grid
;                 lat   : PTR_NEW()    ,  $  2D array containing the latitudes  of the grid
;                 tnt_c : {TNT_COORD}  ,  $  intern {TNT_COORD} structure (see GIS.pro)
;                 meta  : ''              $  If set, a string containg infos about the grid.
;    
;                 Rq: lon and lat are not initialized per default, but are computed and 
;                 stored with the firt call of #Grid2D::Get_LonLat#
;               =================================================================
;                 
;                 Object initialisation
;               ----------------------
;                 KEYWORDS
;                 nx : elements/intervals in x-direction
;                 ny : elements/intervals in y-direction
;                 x0 : most western (most left) x-coordinate
;                 y0 : most northern (most upper) y-coordinate
;                 x1 : most eastern (most right) x-coordinate
;                 y1 : most southern (most lower) y-coordinate
;                 dx : resolution in x-direction
;                 dy : resolution in y-direction
;                 tnt_c : TNT_GIS {TNT_COORD} struct. If set, all previous keywords are ignored.
;                 proj : TNT_GIS {TNT_PROJ} struct. MUST be SET.
;                 meta : a string containing any kind of info
;                 
;                 A coherent combination of keywords must be given. For example,
;                 x1 and y1 can be computed using x0, y0, nx and ny
;      
;                =================================================================
;                  Methods
;                ----------------------
;                The following methods can be used directly. Non ducumented methods 
;                are not for external use.
;       
;                + General methods:
;                obj->ReInit()    : reinitialises completely the grid with new parameters
;                                  (should be used only is special cases by subclasses)
;                obj->GetProperty : get access to some attributes
;                obj->Get_LonLat  : computes and returns the lon and lat array of the grid
;                obj->Get_XY      : computes and returns the X and Y arrays of the grid (trivial)
;                obj->reGrid()    : returns a Grid2D instance of the object 
;                                  (usefull for resampling/plotting purposes)
;       
;                + From OUTSIDE to GRID indexes (i,j) methods:
;                obj->transform_LonLat: transform lons and lats into the grid (i,j) indexes
;                obj->transform_XY: transform xs and xs into the grid (i,j) indexes
;                obj->transform_IJ: transform is and is into the grid (i,j) indexes
;                obj->transform: transform anything into the grid (i,j) indexes
;                               (useful if the nature of the argument is not known, or if you want 
;                                to obtain other informations such as eastings/norhtings, lat and lons...)             
;       
;                + From OUTSIDE to GRID mapping methods (backwards, recommended):
;                obj->map_gridded_data: transforms a data array defined on ANY grid onto the OBJECT grid,
;                                       using several interpolation methods (currently NN and BILI)       
;                obj->map_latlon_data: transforms a data array defined only by its lat-lons (irregular)
;                                      onto the OBJECT grid     
;        
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V 0.1
;       
; :History:
;     Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-

;+
; :Description:
; 
;   Object attributes definition::
;     Grid2D    
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
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2010
;       Modified:   22-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
PRO Grid2D__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { Grid2D      ,  $
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
;       Output: 1 if the GRID2D object is updated successfully, 0 if not
;       
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;    nx: 
;        elements/intervals in x-direction
;    ny: 
;        elements/intervals in y-direction
;    x0: 
;        most western (most left) x-coordinate
;    y0: 
;        most northern (most upper) y-coordinate
;    x1: 
;        most eastern (most right) x-coordinate
;    y1: 
;        most southern (most lower) y-coordinate
;    dx: 
;        resolution in x-direction
;    dy: 
;        resolution in y-direction
;    proj:  
;        TNT_GIS {TNT_PROJ} struct.
;    tnt_c: 
;        TNT_GIS {TNT_COORD} struct. If set, all other parameters are ignored.
;    meta:  
;        a string containing any kind of info
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
Function Grid2D::ReInit ,  $
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
;       nx: 
;           elements/intervals in x-direction
;       ny: 
;           elements/intervals in y-direction
;       x0: 
;           most western (most left) x-coordinate
;       y0: 
;           most northern (most upper) y-coordinate
;       x1: 
;           most eastern (most right) x-coordinate
;       y1: 
;           most southern (most lower) y-coordinate
;       dx: 
;           resolution in x-direction
;       dy: 
;           resolution in y-direction
;       proj:  
;           TNT_GIS {TNT_PROJ} struct.
;       tnt_c: 
;           TNT_GIS {TNT_COORD} struct. If set, all other parameters are ignored.
;       meta:  
;           a string containing any kind of info
;       
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
Function Grid2D::Init ,  $
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
;    Destroy function. 
;
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
pro Grid2D::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  
END


;+
; :Description:
;    Get access to some params. 
;
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;       lon   : out, 
;               2D array containing the longitudes of the grid 
;       lat   : out, 
;               2D array containing the latitudes of the grid 
;       tnt_c : out, 
;               grid {TNT_COORD} structure 
;       meta  : out, 
;               a string containg infos about the grid.
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
pro Grid2D::GetProperty,    lon   =  lon   ,  $ ; 2D array containing the longitudes of the grid 
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
  IF Arg_Present(lon) or ARG_PRESENT(lat) THEN self->Get_LonLat, lon, lat
  IF Arg_Present(tnt_c) THEN tnt_c = self.tnt_c
  IF Arg_Present(meta) THEN meta = self.meta
  
END

;+
; :Description:
;    Get function. 
;
; :Categories:
;            WAVE/OBJ_GIS  
;
; :Params:
;       lon: out,
;            the longitudes 2D array
;       lat: out,
;            the latitudes 2D array
;       nx : out,
;            number of x elements 
;       ny : out, 
;            number of y elements
;       datum : out,
;               the datum in which the lat and lon are defined
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
pro Grid2D::Get_LonLat, lon, lat, nx, ny, datum

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  nx = self.tnt_c.nx
  ny = self.tnt_c.ny
  datum = self.tnt_c.proj.datum
  
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

;-----------------------------------------------------------------------
;+
; :Description:
;    TODO:Describe the procedure.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Params:
;       x: 
;          the x 2D array
;       y: 
;          the y 2D array
;       nx: 
;             number of x elements 
;       ny: 
;             number of y elements
;       proj: 
;             the projection in which x and y are defined
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
pro Grid2D::Get_XY, x, y, nx, ny, proj

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
;       You can also compute eastings and northings in the grid projection, as well as
;       lat and lons in the grid datum. 
;       
;       In many cases, the more specific #transform_LonLat#, #transform_IJ# or #transform_XY# can be used instead.
;
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    x: in, 
;       x coordinates in SRC (if they are indexes,  be carefull with orientation!)
;    y: in, 
;       y coordinates in SRC (if they are indexes,  be carefull with orientation!)
;    i_dst: out, 
;           the i coordinates of (x,y) in the object grid 
;    j_dst: out, 
;           the j coordinates of (x,y) in the object grid 
;
; :Keywords:
;    SRC: 
;         src the initial coordinate system ({TNT_COORD} or {TNT_PROJ} or {TNT_DATUM}) in which x and y are defined
;    LON_DST: out, 
;             the longitudes of (x,y) in the object grid 
;    LAT_DST: out, 
;             the latitudes of (x,y) in the object grid 
;    E_DST: out, 
;            the eastings of (x,y) in the object grid 
;    N_DST: out, 
;           the northings of (x,y) in the object grid 
;    NEAREST: 
;             if the nearest i,j couple is desired
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
PRO Grid2D::transform, x, y, i_dst, j_dst, SRC = src, LON_DST=lon_dst, LAT_DST=lat_dst, E_DST=E_dst, N_DST=N_dst, NEAREST=nearest
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  ;*****************************************
  ; If src is a grid, y has to be rotated  *
  ;*****************************************
  if arg_okay(STRUCT={TNT_COORD}) then begin
    Message, 'Src is a {TNT_COORD} structure. We only accept GRID2D objects, please make one.' 
  endif else if (OBJ_VALID(src)) then begin
    if OBJ_ISA(src, 'Grid2D') then begin
      myy = self.tnt_c.ny - y  - 1
      src->getProperty, TNT_C = mysrc
    endif else MESSAGE, 'SRC is an object but not a grid??'
  endif else begin
    myy = y
    mysrc = src
  endelse  
  
  ;***********************************************
  ; If the array is too big, subset the problem  *
  ;***********************************************
  finished = FALSE
  ind = 0L
  nxi = N_ELEMENTS(x)
  while not finished do begin
    p1 = ind
    p2 = ind + 4000000L ;2000*2000 is the limit
    if p2 ge (nxi-1) then begin
      p2 = nxi-1
      finished = TRUE
    endif
    GIS_coord_trafo, ret, x[p1:p2], myy[p1:p2], ti_dst, tj_dst, SRC=mysrc, DST=self.tnt_c, $
    LON_DST=lon_dst, LAT_DST=lat_dst, E_DST=E_dst, N_DST=N_dst, NEAREST = nearest     
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
;
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    lon: in, 
;         lon coordinates in the datum
;    lat: in, 
;         lat coordinates in the datum
;    datum: in, 
;           the lon/lat datum system {TNT_DATUM}
;    i: out, 
;       the i coordinates of (lon,lat) in the object grid 
;    j: out, 
;       the j coordinates of (lon,lat) in the object grid 
;
; :Keywords:
;    NEAREST: 
;             if the nearest i,j couple is desired
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
PRO Grid2D::transform_LonLat, lon, lat, datum, i, j, NEAREST = nearest
 
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
;    
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    x: in, 
;       northings in the proj
;    y: in, 
;       eastings in the proj 
;    proj: in, 
;          the x/y projection system {TNT_PROJ}
;    i: out, 
;       the i coordinates of (lon,lat) in the object grid 
;    j: out, 
;       the j coordinates of (lon,lat) in the object grid 
;
; :Keywords:
;    NEAREST: 
;             if the nearest i,j couple is desired
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
PRO Grid2D::transform_XY, x, y, proj, i, j, NEAREST = nearest

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
;
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    i_src: in, 
;           i indexes into the grid
;    j_src: in, 
;           j indexes into the grid
;    grid: in, 
;          the grid system (instance of Grid2D)
;    i: out, 
;       the i coordinates of (lon,lat) in the object grid 
;    j: out, 
;       the j coordinates of (lon,lat) in the object grid 
;
; :Keywords:
;    NEAREST: 
;             if the nearest i,j couple is desired
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
PRO Grid2D::transform_IJ, i_src, j_src, grid, i, j, NEAREST = nearest

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if not OBJ_ISA(grid, 'Grid2D')  then Message, WAVE_Std_Message('proj', OBJ='Grid2D')
  
  grid->GetProperty, tnt_c = c  
  self->transform, i_src, j_src, i, j, SRC = c, NEAREST=nearest
  
end

;+
; :Description:
;    Generic routine to transform any georeferenced grid without projection (e.g. lon and lat grid from swath files) 
;       into the object grid using nearest neighborhood.
;       
;       The default method is to transform it forwards: the LatLon points are transformed into
;       the grid and the data value is affected to the neirest grid point. This method is not
;       recomended because the end grid may not be filled entirely if its resolution is finer
;       than the lat-lon grid, for example.
;       
;       The second method (BACKWARDS keyword) transforms the grid points into lat-lon and looks
;       for the neirest points into the input lat-lon grid to perform interpolation. This method 
;       is recomended. However, this implies using interpolation algorithm into irregular grids,
;       which is very demanding in both memory and processor use. If your LatLon arrays are too
;       large, the programm may lag or even go out of memory.
;       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;       ! 05.11.2010: NOT IMPLEMENTED YET !
;       !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;       Output: the remaped data array, of object grid X and Y dimensions. (if there, time is third dimension)
;       
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    data: in, 
;          the data to map on the object grid. The two first dimenstions are X and Y, the third is handled as time
;    src_datum: in, 
;               the datum of the grid
;    src_lon: in, 
;             the longitudes of the grid
;    src_lat: in, 
;             the latitudes of the grid
;
; :Keywords:
;    MISSING: 
;             value to set to missing values in the final grid. 0 is the default value
;    BACKWARDS: 
;               to use the bacwards method (NOT IMPLEMENTED YET)
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function Grid2D::map_lonlat_data, data, src_datum, src_lon, src_lat, MISSING = missing, BACKWARDS = backwards

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
  
  nx = N_ELEMENTS(data[*,0,0])
  ny = N_ELEMENTS(data[0,*,0])
  nt = N_ELEMENTS(data[0,0,*])
  
  if KEYWORD_SET(BACKWARDS) then begin
   ;TODO: implement this (idea is already here)
  endif else begin
  
    ntot =  N_ELEMENTS(data[*,*,0])
    tot = INDGEN(ntot, /LONG)
    self->transform_LonLat, lon[tot], lat[tot], src_datum, i, j, /NEAREST
    
    data_grid = DBLARR(self.tnt_c.nx, self.tnt_c.ny, n)
    data_grid[*] = missing
    
    pok = WHERE(i ge 0 and j ge 0 and i lt self.tnt_c.nx and j lt self.tnt_c.ny, cnt)
    if cnt ne 0 then begin
      i = i[pok]
      j = j[pok]
      tot = tot[pok]
    endif
    
    if nt eq 1 then begin
      data_grid[i, j] = d[tot]
    endif else begin
      tmp = DBLARR(self.tnt_c.nx, self.tnt_c.ny)
      for i = 0L, nt-1 do begin
        tmp *= 0
        tmp[*] =  missing
        tmp[i, j] = (reform(d[*,*,i]))[tot]
        if N_ELEMENTS(data_grid) eq 0 then data_grid = tmp else data_grid =[[[data_grid]],[[tmp]]]
      endfor
    endelse
  endelse
  
  return, data_grid
  
end

;+
; :Description:
;    Generic routine to transform a data array defined in any other GRID the OBJECT GRID. 
;       Default is to use Neirest Neighbor algorithm.
;       Output is the remaped data array, of the same dimensions of the object grid. and with an eventual 3rd dim (time).
;       
; :Categories:
;         WAVE/OBJ_GIS
;
; :Params:
;    data: in, 
;          the data to map on the grid itself. The two first dimenstions are X and Y, the third is handled as time
;    src_grid: in, 
;              the data grid (Grid2d Object)
;
; :Keywords:
;    MISSING: 
;             value to set to missing values in the arrival grid. 0 is the default value
;    BILINEAR: 
;             if bilinear interpolation have to be used
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function Grid2D::map_gridded_data, data, src_grid, MISSING = missing, BILINEAR = bilinear
     
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if not OBJ_ISA(src_grid, 'Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='Grid2D')
  
  if ~KEYWORD_SET(missing) then missing = 0

  src_grid->getProperty, tnt_c = src_c  
  utils_1d_to_2d, INDGEN(self.tnt_c.nx, /LONG), -INDGEN(self.tnt_c.nx, /LONG) + self.tnt_c.ny - 1, xi, yi
  
  if KEYWORD_SET(BILINEAR) then NEAREST = TRUE
  
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
  j_dst = self.tnt_c.ny - j_dst - 1
  
  ;***********************************
  ; Get the data in the source grid  *
  ;***********************************  
  mx = (N_ELEMENTS(data[*,0,0])-1)
  my = (N_ELEMENTS(data[0,*,0])-1)
  p = where((i_dst lt 0) or (j_dst lt 0) or (i_dst gt mx) or (j_dst gt my), cnt)  ; OUT of range
  n = N_ELEMENTS(data[0,0,*])
  
  if KEYWORD_SET(BILINEAR) then begin
  
    if n eq 1 then begin
      tdata = BILINEAR(data, reform(i_dst, self.tnt_c.nx, self.tnt_c.ny), reform(j_dst, self.tnt_c.nx, self.tnt_c.ny))      
      if cnt ne 0 then tdata[p] = missing
      tdata = reform(tdata, dst_c.nx, dst_c.ny) ;TODO: is this reform necessary ???
    endif else begin
      for i = 0L, n-1 do begin
        tmp =  BILINEAR((reform(data[*,*,i])), reform(i_dst, self.tnt_c.nx, self.tnt_c.ny), reform(j_dst, self.tnt_c.nx, self.tnt_c.ny))
        if cnt ne 0 then tmp[p] = missing
        tmp = reform(tmp, self.tnt_c.nx, self.tnt_c.ny)
        if N_ELEMENTS(tdata) eq 0 then tdata = tmp else data_dst =[[[tdata]],[[tmp]]]
      endfor
    endelse
    
  endif else begin ; Nearest neighbor
  
    if n eq 1 then begin
      tdata = data[i_dst, j_dst]
      if cnt ne 0 then tdata[p] = missing
      tdata = reform(tdata, self.tnt_c.nx, self.tnt_c.ny)
    endif else begin
      for i = 0L, n-1 do begin
        tmp = (reform(data[*,*,i]))[i_dst, j_dst]
        if cnt ne 0 then tmp[p] = missing
        tmp = reform(tmp, self.tnt_c.nx, self.tnt_c.ny)
        if N_ELEMENTS(tdata) eq 0 then tdata = tmp else tdata =[[[tdata]],[[tmp]]]
      endfor
    endelse
    
  endelse
  
  return, tdata
     
end

;+
; :Description:
;    Generic routine to resample the object grid.
;    
; :Categories:
;         WAVE/OBJ_GIS
;
;
; :Keywords:
;    Xsize: 
;           the new X dimension (the original X/Y ratio is conserved) 
;    Ysize: 
;           the new Y dimension (the original X/Y ratio is conserved) (if set, Xsize is ignored)
;    FACTOR: 
;           a factor to multiply to nx and ny (if set, Xsize and Ysize are ignored)
;    PLOT: 
;         to set if the resampled grid is used to plot pruposes (grid containing half of the border pixels)
;
; :Examples:
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
function Grid2D::reGrid, Xsize = Xsize,  Ysize = Ysize, FACTOR = factor,  PLOT = plot
     
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if KEYWORD_SET(Xsize) then begin
    nx = Xsize
    ny = DOUBLE(self.tnt_c.ny) / DOUBLE(self.tnt_c.nx) * nx    
  endif else if KEYWORD_SET(Ysize) then begin
    ny = Ysize
    nx = DOUBLE(self.tnt_c.nx) / DOUBLE(self.tnt_c.ny) * ny 
  end else if KEYWORD_SET(FACTOR) then begin
    nx = self.tnt_c.nx * FACTOR
    ny = self.tnt_c.ny * FACTOR
  end
  
  if KEYWORD_SET(plot) then begin
    x0 = self.tnt_c.x0 - 0.5*self.tnt_c.dx
    y0 = self.tnt_c.y0 + 0.5*self.tnt_c.dy
    x1 = self.tnt_c.x1 + 0.5*self.tnt_c.dx
    y1 = self.tnt_c.y1 - 0.5*self.tnt_c.dy
  endif else begin
    x0 = self.tnt_c.x0
    y0 = self.tnt_c.y0
    x1 = self.tnt_c.x1
    y1 = self.tnt_c.y1
  endelse
  
  return, OBJ_NEW('Grid2D', x0=x0, y0=y0, x1=x1, y1=y1, nx=nx, ny=ny, PROJ=self.tnt_c.proj, META=self.meta + ' (resampled)')
              
end
