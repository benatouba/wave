; docformat = 'rst'
;+
; 
;  w_Map is a central object of the WAVE graphics system: it generates
;  "geolocalized" 2d plots for any kind of gridded data. The role of
;  this object is to create a color image (in pixels) of gridded data 
;  that can be used for other plots (with title, legend, etc.).  
;  The basic flow chart is simple. The two first actions (instancing and
;  mapping) are related to the map itself and the third step is 
;  related to the data plot.
;  
;  1. Instancing: the object is created specifying an image size (in pixels) 
;  and associating a 'w_Grid2D' object to the image. This information 
;  is stored by the object instance and cannot be changed anymore.
;  From now on, each pixel of the image is geolocalized for further 
;  mapping and plotting purposes.
;  
;  2. Mapping: the user may add mapping options to the plot such as country 
;  outlines, any kind of shape file, relief shading, lat-lon grid contour
;  lines... This has to be done only once since it sometimes requires 
;  computing time, and can be undone. Afterwards, this information 
;  is stored for all the plots that will be further on generated.
;  
;  3. Set plot: the two last steps can be repeated in any order for all
;  the future plots on the map. The data to be plotted has to be set, 
;  as well as the plotting params (colors, levels)
;  
;  4. Get Img: the generated plot can be shown on an external window or 
;  recovered for larger plots.
;  
; 
; :Categories:
;         WAVE/OBJ_PLOT 
;
; :Properties:
;      
;      
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2011.
;
;
;-     
 
;+
; :Description:
;    Defines the attributes of the class.
;    
;
; :History:
;     Written by FaM, 2011.
;
;-    
PRO w_Map__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; This is for the colors and data-levels 
  struct = {PLOT_PARAMS                    , $
            type           : ''            , $ ; USER or AUTO generated levels
            nlevels        : 0L            , $ ; number of data levels
            colors         : PTR_NEW()     , $ ; array of nlevels colors
            levels         : PTR_NEW()     , $ ; array of nlevels data levels
            min_val        : 0D            , $ ; min data level
            max_val        : 0D              $ ; max data level           
            }
  
  ; This is the information for one shape file
  struct = {MAP_SHAPE                      , $ 
            shape_file     : ''            , $ ; path to the shape file
            thick          : 0D            , $ ; thickness or the shape line for the plot
            style          : 0D            , $ ; style or the shape line for the plot
            color          : ''            , $ ; color or the shape line for the plot
            n_coord        : 0L            , $ ; number of coordinates in the shape (private)
            coord          : PTR_NEW()     , $ ; coordinates of the shape points (private)
            conn           : PTR_NEW()       $ ; connivence info (private)          
            }
  
  ; This is for the Lon-Lat/UTM contours drawing
  struct = {MAP_PARAMS                     , $
            type           : ''            , $ ; LONLAT or UTM
            xticks         : PTR_new()     , $ ; where to find the ticks on the Xaxis (in relative coordinates from 0 to 1)
            yticks         : PTR_new()     , $ ; where to find the ticks on the Yaxis (in relative coordinates from 0 to 1)
            xlevels        : PTR_new()     , $ ; values of the plotted contours in Xcoordinates
            ylevels        : PTR_new()     , $ ; values of the plotted contours in Ycoordinates
            color          : ''            , $ ; color of the contour lines
            thick          : 0D            , $ ; thickness of the contour lines
            style          : 0D              $ ; style of the contour lines
            }
     
  struct = { w_Map                         , $
             grid          : OBJ_NEW()     , $ ; the grid object (nx = Xsize, ny = Ysize)
             Xsize         : 0L            , $ ; X size of the image in pixels
             Ysize         : 0L            , $ ; Y size of the image in pixels
             img           : PTR_NEW()     , $ ; Byte array ([Xsize,Ysize]) containing the indexes in the colors array
             data          : PTR_NEW()     , $ ; active data array ([Xsize,Ysize]) of any numeric type
             sl            : PTR_NEW()     , $ ; shading layer for topography shading
             relief_factor : 0D            , $ ; strenght of the shading (default: 0.7)
             nshapes       : 0L            , $ ; number of active shape files to plot                  
             shapes        : PTR_NEW()     , $ ; array of nshapes {MAP_SHAPE} structures                               
             map_params    : {MAP_PARAMS}  , $ ; the mapping params for contours
             plot_params   : {PLOT_PARAMS} , $ ; the plotting params          
             is_Shaped     : FALSE         , $ ; is there at least one shape to draw?
             is_Shaded     : FALSE         , $ ; did the user specify a DEM for shading?
             is_Mapped     : FALSE           $ ; did the user specify a contour to draw for mapping?         
             }
    
END

;+
; :Description:
;    Build function. The required parameter is an instance of 'w_grid2d' that 
;    defines the map geolocalisation.
;    
; :Params:
;    grid: in, required, type = 'w_grid2d'
;          the map geolocalisation
; :Keywords:
;    Xsize: in, optional, type = integer
;           the window X dimension size (the original grid X/Y ratio is conserved) 
;    Ysize: in, optional, type = integer, default = 400
;           the window Y dimension size (the original grid X/Y ratio is conserved) (if set, Xsize is ignored)
;    FACTOR: in, optional, type = float
;            a factor to multiply to the grid nx and ny to obtain the window size (if set, Xsize and Ysize are ignored)
;    NO_COUNTRIES: in, optional, type = boolean
;                  default behavior is to add country outlines to the map automatically. Set this keyword
;                  to prevent this.
;
;
; :History:
;     Written by FaM, 2011.
;
;
;-    
Function w_Map::Init, grid, Xsize = Xsize,  Ysize = Ysize, FACTOR = factor, NO_COUNTRIES = no_countries
     
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
  if not OBJ_ISA(grid, 'w_Grid2D')  then Message, WAVE_Std_Message('grid', OBJ='w_Grid2D')
  if ~KEYWORD_SET(Xsize) and ~KEYWORD_SET(Ysize) and ~KEYWORD_SET(FACTOR) then Ysize = 400
  
  self.grid = grid->reGrid(Xsize = Xsize,  Ysize = Ysize, FACTOR = factor) 
  self.grid->getProperty, tnt_C = c
  self.Xsize = c.nx
  self.Ysize = c.ny
  
  dummy = self->set_data()
  dummy = self->set_plot_params(COLORS='white')  
  if ~KEYWORD_SET(NO_COUNTRIES) then dummy = self->set_shape_file(/COUNTRIES)  
  dummy = self->set_map_params()  
  dummy = self->set_shading_params(RELIEF_FACTOR = 0.7)  
                
  RETURN, 1
  
END

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;
;-    
pro w_Map::DestroyPlotParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.plot_params.colors
  ptr_free, self.plot_params.levels
  self.plot_params = {PLOT_PARAMS}
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;
;-  
pro w_Map::DestroyMapParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 

  ptr_free, self.map_params.xticks
  ptr_free, self.map_params.yticks
  ptr_free, self.map_params.xlevels
  ptr_free, self.map_params.ylevels
  
  self.is_Mapped = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;
;-  
pro w_Map::DestroyShapes

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  if PTR_VALID(self.shapes) then begin  
    shapes = *self.shapes
    for i = 0, N_ELEMENTS(shapes) - 1 do begin
      ptr_free, (shapes[i]).coord
      ptr_free, (shapes[i]).conn
    endfor
  endif
  ptr_free, self.shapes
  self.nshapes = 0L
  self.is_Shaped = FALSE
  
end

;+
; :Description:
;    Destroy function. 
;
; :History:
;     Written by FaM, 2011.
;
;-    
pro w_Map::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  OBJ_DESTROY, self.grid
  PTR_FREE, self.img 
  PTR_FREE, self.data 
  PTR_FREE, self.sl    
  
  self->DestroyShapes         
  self->DestroyMapParams       
  self->DestroyPlotParams     
  
END

;+
; :Description:
;    Get access to some params. 
;
; :History:
;     Written by FaM, 2011.
;
;
;-    
PRO w_Map::GetProperty, XSIZE = xsize, YSIZE = ysize, LEVELS = levels, COLORS = colors
    
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
  if ARG_PRESENT(levels) then levels = *self.plot_params.levels
  if ARG_PRESENT(colors) then colors = *self.plot_params.colors
     
end

;+
; :Description:
;    Sets plotting parameters. This can happen at any 
;    time during the plotting process, and can be 
;    updated any time. When no levels are set, the levels
;    are chosen automatically with the current data 
;    array. When no colors are set, the colors are chosen
;    automatically from the active color table.
;
;
; :Keywords:
;    LEVELS: in, type = integer
;    
;    N_LEVELS: in, type = long
;    
;    VAL_MIN: in, type = double 
;    
;    VAL_MAX: in, type = double
;    
;    COLORS: in, type = PTR_NEW()
;    
;    CMIN:
;    
;    CMAX:
;    
;    INVERTCOLORS:
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::set_Plot_Params, LEVELS = levels, N_LEVELS = n_levels, VAL_MIN = val_min, VAL_MAX = val_max , $
                                    COLORS = colors, CMIN=cmin, CMAX=cmax, INVERTCOLORS = invertcolors
         
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    self->DestroyPlotParams
    ok = self->set_Plot_Params()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  self->DestroyPlotParams
   
  is_Levels = N_ELEMENTS(levels) ne 0 
  is_Colors = N_ELEMENTS(colors) ne 0
  
  if is_Levels then self.plot_params.type = 'USER' else self.plot_params.type = 'AUTO'
  
  ; Give a value to nlevels   
  IF N_Elements(n_levels) EQ 0 THEN BEGIN
     IF ~is_Levels and ~is_Colors THEN nlevels = 256 $
     ELSE begin
       if is_colors then nlevels = N_Elements(colors)
       if is_Levels then nlevels = N_Elements(levels)              
     endelse
  ENDIF else nlevels = n_levels
  
  
  if is_Levels then if (nlevels ne N_ELEMENTS(LEVELS)) then $
    message, '$levels and $n_levels are incompatible.'
  if is_colors then if (nlevels ne N_ELEMENTS(colors)) then $
    message, '$colors and $n_levels are incompatible.'
  
  ; Colors
  if is_Colors then _colors = utils_color_convert(COLORS = colors) $
   else _colors = utils_color_convert(NCOLORS = nlevels, CMIN=cmin, CMAX=cmax, INVERTCOLORS = invertcolors)
  
  ; Levels
  if N_ELEMENTS(VAL_MIN) eq 0 then val_min = MIN(*self.data)
  if N_ELEMENTS(VAL_MAX) eq 0 then val_max = MAX(*self.data)  
  if is_Levels then begin 
   _levels = levels 
   val_min = min(levels)
   val_max = max(levels)   
  endif else _levels = ((val_max - val_min) / Float(nlevels)) * Indgen(nlevels) + val_min
   
  ; Fill up
  self.plot_params.nlevels  = nlevels
  self.plot_params.colors   = PTR_NEW(_colors, /NO_COPY)
  self.plot_params.levels   = PTR_NEW(_levels, /NO_COPY)
  self.plot_params.min_val  = val_min
  self.plot_params.max_val  = val_max
  
  return, self->set_img()

end


;+
; :Description:
;    Sets map params
;
; :Categories:
;         WAVE/OBJ_GIS 
;todo: describe everything :)
; :Keywords:
;    TYPE: in, type = string
;    
;    INTERVAL: in, type = double
;    
;    THICK: in, type = double
;    
;    STYLE: in, type = double
;    
;    COLOR: in, type = string
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::set_map_params, TYPE = type, INTERVAL = interval, THICK = thick, STYLE = style, COLOR = color
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->DestroyMapParams
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
    ; This is for the Lon-Lat/UTM contours drawing
;  struct = {MAP_PARAMS                     , $
;            type           : ''            , $
;            xticks         : PTR_new()     , $
;            yticks         : PTR_new()     , $
;            xlevels        : PTR_new()     , $
;            ylevels        : PTR_new()     , $
;            color          : ''            , $
;            interval       : 0D            , $
;            thick          : 0D            , $
;            style          : 0D              $
;            }  
   self->DestroyMapParams
  _type = 'LONLAT'
  _interval = 10.
  _thick = 1.
  _style = 2.
  _color = 'Dark Grey'
  
  if N_ELEMENTS(TYPE) eq 1 then _type = str_equiv(TYPE)
  if N_ELEMENTS(INTERVAL) eq 1 then _interval = INTERVAL
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
 
  self.map_params.type = _type
  self.map_params.thick = _thick
  self.map_params.style = _style
  self.map_params.color = _color
                           
  self.is_Mapped = _type ne ''
  
  if ~self.is_Mapped then begin
   self->DestroyMapParams
   return, 1
  endif
  
  if self.map_params.type eq 'LONLAT' then begin
  
    self.grid->get_Lonlat, lon, lat
    Nlevels = 360 / _interval
    levels = INDGEN(Nlevels) * _interval - 170
    p = where(levels le floor(max(Lon)) and levels ge ceil(min(Lon)), cnt)
    if cnt gt 0 then lonlevels = levels[p]
    p = where(levels le floor(max(Lat)) and levels ge ceil(min(Lat)), cnt)
    if cnt gt 0 then latlevels = levels[p]
    
    ;TODO: ticks         
    self.map_params.xlevels = PTR_NEW(lonlevels, /NO_COPY)
    self.map_params.ylevels = PTR_NEW(latlevels, /NO_COPY) 
       
  endif else Message, 'Currently only LONLAT type is supported'
  
  return, 1

end

;+
; :Description:
;    Set shading params.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;    RELIEF_FACTOR:
;todo: keyword
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::set_shading_params, RELIEF_FACTOR = relief_factor
  
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

;+
; :Description:
;    Set image.
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::set_img
  
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

  img = BYTARR(self.Xsize, self.Ysize)       
  
  for l=0, self.plot_params.nlevels-1 do begin
    if l lt self.plot_params.nlevels-1 then p = where((*self.data) ge (*self.plot_params.levels)[l] and (*self.data) lt (*self.plot_params.levels)[l+1], cnt) $
    else p = where((*self.data) ge (*self.plot_params.levels)[l], cnt)
    if cnt gt 0 then img[p]= l
  endfor
    
  PTR_FREE, self.img
  SELF.img = PTR_NEW(img, /NO_COPY)
  
  return, 1

end

;+
; :Description:
;    Set data. 
;    todo: describe + params/keywords
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Params:
;    data:
;    
;    grid:
;
; :Keywords:
;    BILINEAR:
;    
;    MISSING:
;    
;    VAL_MIN:
;    
;    VAL_MAX:
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::set_data, data, grid, BILINEAR = bilinear, MISSING = missing, VAL_MIN = val_min, VAL_MAX = val_max
                             
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.data
    ok = self->set_data()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if N_PARAMS() eq 0 then begin
   data = BYTARR(self.Xsize, self.Ysize) 
   PTR_FREE, self.data
   self.data = PTR_NEW(data, /NO_COPY)
   return, self->set_img()
  endif  
  
  if ~ arg_okay(data, N_DIM=2, /NUMERIC) then Message, WAVE_Std_Message('data', NDIMS=2)
  
  if N_ELEMENTS(grid) eq 0 then begin
     if arg_okay(img, DIM=[self.Xsize, self.Ysize], /NUMERIC) then _data = data $
      else _data = CONGRID(data, self.Xsize, self.Ysize, /CENTER, INTERP=bilinear)
  endif else begin
    _data = self.grid->map_gridded_data(data, grid, MISSING = missing, BILINEAR = bilinear)
  endelse    
  
  PTR_FREE, self.data
  self.data = PTR_NEW(_data, /NO_COPY)
  
   ; Levels
  if self.plot_params.type eq 'AUTO' then begin
    if N_ELEMENTS(VAL_MIN) eq 0 then val_min = MIN(*self.data)
    if N_ELEMENTS(VAL_MAX) eq 0 then val_max = MAX(*self.data)
    _levels = ((val_max - val_min) / Float(self.plot_params.nlevels)) * Indgen(self.plot_params.nlevels) + val_min
    ptr_free, self.plot_params.levels
    self.plot_params.levels  = PTR_NEW(_levels, /NO_COPY)
    self.plot_params.min_val = val_min
    self.plot_params.max_val = val_max
  endif

  return, self->set_img()

end

function w_Map::set_topography, GRDFILE = grdfile
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.sl
    self.is_Shaded = false
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(grdfile) then begin
    PTR_FREE, self.sl
    self.is_Shaded = false
    return, 1
  end
  
  if N_ELEMENTS(grdfile) eq 0 then grdfile = DIALOG_PICKFILE(TITLE='Please select .grd file to read', /MUST_EXIST)
  if GRDFILE eq '' then begin
    PTR_FREE, self.sl
    self.is_Shaded = false
    return, 1
  end
  
  spli = STRSPLIT(grdfile, '.', /EXTRACT)
  if str_equiv(spli[1]) ne 'GRD' then message, WAVE_Std_Message(/FILE)
  hdr = spli[0] + '.hdr'
  
  self.grid->get_Lonlat, lon, lat ; TODO: change this into GRID kind of things  
  self.grid->getProperty, tnt_c = c
  
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
  if cnt gt 0 then z[p] = 0
  z = FLOAT(reform(z, n_elements(lat[*,0]), n_elements(lat[0,*])))
  
  GIS_xy_derivatives, ret, rotate(z,7), dx = c.dx, dy = c.dy, DFDX=dhdx,DFDY=dhdy
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message('Error by calculating derivatives.')
  
  sl = TEMPORARY(dhdx) - TEMPORARY(dhdy) ; shade layer
  
  
  PTR_FREE, self.sl
  self.sl = PTR_NEW(sl, /NO_COPY)
  self.is_Shaded = TRUE
 
  return, 1

end

;+
; :Description:
;    set shape file
;todo: describe
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;    SHPFILE:
;    
;    SHP_SRC:
;    
;    COUNTRIES:
;    
;    COLOR:
;    
;    THICK:
;    
;    STYLE:
;    
;    REMOVE_ENTITITES:
;    
;    KEEP_ENTITITES:
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::set_shape_file, SHPFILE = shpfile, SHP_SRC = shp_src, COUNTRIES = countries, $
                                    COLOR = color, THICK = thick, STYLE = style, $
                                    REMOVE_ENTITITES = remove_entitites, KEEP_ENTITITES = keep_entitites

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->DestroyShapes
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if KEYWORD_SET(COUNTRIES) then return, self->set_shape_file(SHPFILE = WAVE_resource_dir+'/shapes/world_borders/world_borders.shp', $
            COLOR = color, THICK = thick, STYLE = style, REMOVE_ENTITITES = remove_entitites, KEEP_ENTITITES = keep_entitites)
    
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(shpfile) then begin
   self->DestroyShapes
   return, 1
  endif
  
  if N_ELEMENTS(shpfile) eq 0 then shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )
  if shpfile eq '' then begin
   self->DestroyShapes
   return, 1
  endif
  
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
     
    if KEYWORD_SET(REMOVE_ENTITITES) then begin
      pr = where(REMOVE_ENTITITES eq i, cnt)
      if cnt ne 0 then continue
    endif
    if KEYWORD_SET(KEEP_ENTITITES) then begin
      pr = where(KEEP_ENTITITES eq i, cnt)
      if cnt eq 0 then continue
    endif
     
    ent = shpmodel->GetEntity(i, /ATTRIBUTES)    
    if not ptr_valid(ent.vertices) then continue
    
    x = reform((*ent.vertices)[0,*])
    y = reform((*ent.vertices)[1,*])
    n_vert = n_elements(x)    
    
    if n_vert lt 3 $
    or min(y) gt range[3] $ 
    or max(y) lt range[2] $ 
    or min(x) gt range[1] $ 
    or min(y) gt range[3] then begin
      shpmodel->IDLffShape::DestroyEntity, ent 
      continue
    endif
    
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
        
    shpmodel->IDLffShape::DestroyEntity, ent 

  endfor

  ; clean unused objects
  obj_destroy, shpModel
  
  if N_ELEMENTS(CONN) eq 0 then begin
   message, 'Did not find anything plotable in the shapefile.', /INFORMATIONAL
   return, 1 ;Nothing to do
  endif  
  
  _color = 'black'
  _style = 0.
  _thick = 1.5
  
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  
  sh = {MAP_SHAPE}
  sh.color = _color
  sh.shape_file = shpfile
  sh.style = _style
  sh.thick = _thick  
  sh.conn = PTR_NEW(conn, /NO_COPY)
  sh.coord = PTR_NEW(coord, /NO_COPY)
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

;+
; :Description:
;    Change image to rgb.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
;
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::img_to_rgb

    utils_color_rgb, *self.plot_params.colors, s_r, s_g, s_b
    r = byte(0 > s_r[*self.img] < 255)
    g = byte(0 > s_g[*self.img] < 255)
    b = byte(0 > s_b[*self.img] < 255)
    img = bytarr(3, self.Xsize, self.Ysize)
    img[0,*,*] = r[*,*]
    img[1,*,*] = g[*,*]
    img[2,*,*] = b[*,*]
    
    return, img
    
end    
    
;+
; :Description:
;    Shading function.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::shading

  if self.relief_factor eq 0 then return, self->img_to_rgb()
  nlevels = self.plot_params.nlevels
  
  if nlevels eq 0 or nlevels gt 127 then begin
   MESSAGE, 'Shading: max number of colors is 127', /INFORMATIONAL
   return, self->img_to_rgb()
  endif

  rp = bindgen(256)
  gp = bindgen(256)
  bp = bindgen(256)
  
  utils_color_rgb, *self.plot_params.colors, s_r, s_g, s_b  
  rp[0:nlevels-1] = s_r[*]
  gp[0:nlevels-1] = s_g[*]
  bp[0:nlevels-1] = s_b[*]
  
  ;******************
  ; Prepare shading *
  ;******************  
  sl = *self.sl      
  mean_sl = moment(sl, SDEV=sdev_sl)
  
  p = where(sl gt 0, cnt)  
  if cnt gt 0 then sl[p] = 0.4*sin(0.5*!pi*(-1>(sl[p]/(2*sdev_sl))<1))
  p = 0  
  level = 1.0 - 0.1 * self.relief_factor ; 1.0 for 0% and 0.9 for 100%
  sens  = 0.7 * self.relief_factor       ; 0.0 for 0% and 0.7 for 100%
  
  ;****************
  ; Apply shading *
  ;****************  
  _img = ROTATE(*self.img,7)
  r = rp[_img]
  g = gp[_img]
  b = bp[_img]
  
  r = byte(0 > (level*r*(1+sens*sl) < 255))
  g = byte(0 > (level*g*(1+sens*sl) < 255))
  b = byte(0 > (level*b*(1+sens*sl) < 255))
  sl = 0
  
  img = bytarr(3, self.Xsize, self.Ysize)
  img[0,*,*] = r[*,*]
  img[1,*,*] = g[*,*]
  img[2,*,*] = b[*,*]    
  
  return, reverse(img,3)
  
end

;+
; :Description:
;    Mapping
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
function w_Map::mapping

  if self.map_params.type eq 'LONLAT' then begin
  
    self.grid->get_Lonlat, lon, lat
    
    FSC_Contour, lon, POSITION = [0,0,self.Xsize,self.Ysize], /DEVICE, /OVERPLOT, XTICKLEN = -0.2, $
      COLOR = self.map_params.color, C_LINESTYLE = self.map_params.style, $
      LEVELS = *(self.map_params.xlevels), C_THICK =  self.map_params.thick
      
    FSC_Contour, lat, POSITION = [0,0,self.Xsize,self.Ysize], /DEVICE, /OVERPLOT, XTICKLEN = -0.2, $
      COLOR = self.map_params.color, C_LINESTYLE = self.map_params.style, $
      LEVELS = *(self.map_params.ylevels), C_THICK =  self.map_params.thick
      
  endif
  
  return, 1
  
end

function w_Map::shaping
  
  shapes = *(self.shapes)
  
  for i = 0, self.nshapes-1 do begin
    sh = shapes[i]
    conn = *(sh.conn)
    coord = *(sh.coord)
    
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

;+
; :Description:
;    To display the wind.
;
; :Categories:
;         WAVE/OBJ_GIS 
;todo: describe params/keywords
; :Params:
;    grid:
;    
;    ud:
;    
;    vd:
;    
;    density:
;
; :Keywords:
;    LENGTH:
;    
;    LEGEND:
;    
;    THICK:
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-    
pro w_Map::draw_wind, grid, ud, vd, density, LENGTH=length, LEGEND = legend, THICK=thick

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
    
  if ~KEYWORD_SET(length) then length =0.05  
  if density ne 1 and density ne 3 and density ne 5 and density ne 7 then message, 'Not ready yet.'
  
  grid->getProperty, tnt_c = c   
  nxg = C.nx
  nyg = C.ny
    
  fx = FLOOR(double(nxg)/density) ; possible points
  fy = FLOOR(double(nyg)/density) ; possible points
  s = floor(density/2.) ; where to start (1 for 3, 2 for 5, etc.)
    
  xi = INDGEN(fx, /DOUBLE) * DENSITY + s
  yi = INDGEN(fy, /DOUBLE) * DENSITY + s
  
  x = xi * c.dx + c.x0
  y = yi * c.dy + c.y1
  utils_1d_to_2d, x, y, x, y  
  self.grid->transform_XY, x, y, c.proj, devDLX, devDLY, /NEAREST
  
  utils_1d_to_2d, xi, yi, xi, yi
  ud = ud[xi,yi]
  vd = vd[xi,yi]  
  
  partvelvec, ud, vd, devDLX, devDLY, /OVER, VECCOLORS=FSC_Color('black'), LENGTH=length, THICK=thick, /DEVICE
  
end

;+
; :Description:
;    Show the image.
;
; :Categories:
;         WAVE/OBJ_GIS 
;todo: describe..
; :Params:
;    win:
;
; :Keywords:
;    PIXMAP:
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-   
pro w_Map::show_img, win, PIXMAP = pixmap

  @WAVE.inc
  
   pp = !ORDER ;To restore later
  !ORDER = 0

  if TNT_OS eq 'WINDOWS' then set_plot, 'WIN' else set_plot, 'X'
  DEVICE, RETAIN=2, TRUE_COLOR=24, DECOMPOSED=1
     
  window, XSIZE=self.Xsize, YSIZE=self.Ysize, Title='Map Plot', PIXMAP = pixmap, /FREE
  win = !D.WINDOW
  
  if self.is_Shaded then begin
    img = self->shading()
  endif else begin
    img = self->img_to_rgb()
  endelse  
  
  if self.is_Mapped then begin
   d = bytarr(self.Xsize, self.Ysize)
   d[1] = 1
   FSC_Contour,  d , POSITION = [0,0,self.Xsize,self.Ysize], /DEVICE, /NODATA
  endif
  

  tv, img, true = 1
      
  if self.is_Shaped then ok = self->shaping() 
  if self.is_Mapped then ok = self->mapping() 
  
  !ORDER = pp
  
end

;+
; :Description:
;    To show a color bar. 
;
; :Categories:
;         WAVE/OBJ_GIS 
;todo: describe everything
; :Params:
;    win:
;
; :Keywords:
;    PIXMAP:
;    
;    TITLE:
;    
;    BAR_TAGS:
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-   
pro w_Map::show_color_bar, win, PIXMAP = pixmap, TITLE=title, BAR_TAGS = bar_tags

  @WAVE.inc
  
  pp = !ORDER ;To restore later
  !ORDER = 0

  if TNT_OS eq 'WINDOWS' then set_plot, 'WIN' else set_plot, 'X'
  DEVICE, RETAIN=2, TRUE_COLOR=24, DECOMPOSED=1
  
  xs = self.Ysize * 0.2
  ys = self.Ysize * 0.75
     
  window, XSIZE= xs, YSIZE=ys, Title='Color bar', PIXMAP = pixmap, /FREE
  win = !D.WINDOW
  tv, BYTARR(3, xs, ys)+255, /TRUE
  
  if N_ELEMENTS(BAR_TAGS) eq 0 then bar_TAGS = STRING(*(self.plot_params.levels), FORMAT = '(F5.1)')
  
  if self.plot_params.nlevels lt 40 then begin
    cgDCBar, *(self.plot_params.colors), COLOR = "BLACK", LABELS=bar_tags, Position=[0.20,0.05,0.30,0.95], $
      TITLE=title, CHARSIZE = 2, /VERTICAL;, FONT=-1 ;, CHARTHICK=1
  endif else begin
    WAVE_COLORBAR, *(self.plot_params.colors), COLOR=FSC_Color("BLACK"), Position=[0.20,0.05,0.30,0.95], $
      TITLE=title, CHARSIZE = 2, /VERTICAL, /RIGHT, MINRANGE=self.plot_params.min_val, MAXRANGE=self.plot_params.max_val ;, FONT=-1 ;, CHARTHICK=1
  endelse
  !ORDER = pp
  
end