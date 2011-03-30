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
;-     
 
;+
; :Description:
;    Defines the attributes of the class.
;    
;
; :History:
;     Written by FaM, 2011.
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
            neutral        : 0L            , $ ; neutral color
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
  
  ; This is the information for one polygon to draw
  struct = {MAP_POLYGON                    , $ 
            thick          : 0D            , $ ; thickness or the line for the plot
            style          : 0D            , $ ; style or the line for the plot
            color          : ''            , $ ; color or the line for the plot
            n_coord        : 0L            , $ ; number of coordinates in the polygon (private)
            coord          : PTR_NEW()       $ ; coordinates of the polygon points (private)        
            }
  
  ; This is the information for one point to draw
  struct = {MAP_POINT                      , $ 
            thick          : 0D            , $ ; thickness or the point
            psym           : 0L            , $ ; style or the point
            symsize        : 0D            , $ ; style or the point
            color          : ''            , $ ; color or the point
            text           : ''            , $ ; point annotation
            charsize       : 0D            , $ ; point annotation size
            align          : 0D            , $ ; annotation alignement
            dpText         : [0D,0D]       , $ ; delta pos of the text with respect to the point
            coord          : [0D,0D]         $ ; coordinates of the point       
            }
  
  ; This is for the Lon-Lat/UTM contours drawing
  struct = {MAP_PARAMS                     , $
            type           : ''            , $ ; LONLAT or UTM
            xticks         : PTR_new()     , $ ; where to find the ticks on the Xaxis
            yticks         : PTR_new()     , $ ; where to find the ticks on the Yaxis
            xtickvalues    : PTR_new()     , $ ; value of the ticks on the Xaxis
            ytickvalues    : PTR_new()     , $ ; value of the ticks on the Yaxis
            xlevels        : PTR_new()     , $ ; values of the plotted contours in Xcoordinates
            ylevels        : PTR_new()     , $ ; values of the plotted contours in Ycoordinates
            t_Charsize     : 0D            , $ ; Ticks charsizes
            color          : ''            , $ ; color of the contour lines
            labeled        : 0L            , $ ; if the contours have to labelled
            thick          : 0D            , $ ; thickness of the contour lines
            style          : 0D              $ ; style of the contour lines
            }

  ; This is for the wind vectors 
  struct = {WIND_PARAMS                    , $
            type           : ''            , $ ; currently VECTORS
            velx           : PTR_new()     , $ ; x velocities
            vely           : PTR_new()     , $ ; y velocities
            posx           : PTR_new()     , $ ; coordinates in data device
            posy           : PTR_new()     , $ ; coordinates in data device
            color          : ''            , $ ; color of the arrows
            thick          : 0D            , $ ; thickness of the arrows
            length         : 0D              $ ; lenght of the arrows
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
             npolygons     : 0L            , $ ; number of active polygons to plot                  
             polygons      : PTR_NEW()     , $ ; array of npolygons {MAP_POLYGON} structures                               
             npoints       : 0L            , $ ; number of points to plot                  
             points        : PTR_NEW()     , $ ; array of npointss {MAP_POINT} structures                               
             map_params    : {MAP_PARAMS}  , $ ; the mapping params for contours
             plot_params   : {PLOT_PARAMS} , $ ; the plotting params          
             wind_params   : {WIND_PARAMS} , $ ; the wind params          
             is_Shaped     : FALSE         , $ ; is there at least one shape to draw?
             is_Shaded     : FALSE         , $ ; did the user specify a DEM for shading?
             is_Polygoned  : FALSE         , $ ; did the user specify a polygon to draw?
             is_Pointed    : FALSE         , $ ; did the user specify a point to draw?
             is_Mapped     : FALSE         , $ ; did the user specify a contour to draw for mapping?         
             is_Winded     : FALSE           $ ; did the user specify wind flows ?         
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
  dummy = self->set_shading_params()  
  dummy = self->set_wind(COLOR='black', LENGTH=0.08, THICK=1)  
               
  RETURN, 1
  
END

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
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
;-    
pro w_Map::DestroyWindParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.wind_params.velx
  ptr_free, self.wind_params.vely
  ptr_free, self.wind_params.posx
  ptr_free, self.wind_params.posy
    
  self.wind_params = {WIND_PARAMS}
  self.is_Winded = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::DestroyMapParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 

  ptr_free, self.map_params.xticks
  ptr_free, self.map_params.yticks
  ptr_free, self.map_params.xtickvalues
  ptr_free, self.map_params.ytickvalues
  ptr_free, self.map_params.xlevels
  ptr_free, self.map_params.ylevels

  
  self.map_params = {MAP_PARAMS}
  self.is_Mapped = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
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
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::DestroyPolygons

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  if PTR_VALID(self.polygons) then begin  
    polygons = *self.polygons
    for i = 0, N_ELEMENTS(polygons) - 1 do ptr_free, (polygons[i]).coord
  endif
  
  ptr_free, self.polygons
  self.npolygons = 0L
  self.is_Polygoned = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::DestroyPoints

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.points
  self.npoints = 0L
  self.is_Pointed = FALSE
  
end

;+
; :Description:
;    Destroy function. 
;
; :History:
;     Written by FaM, 2011.
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
  self->DestroyWindParams     
  self->DestroyPolygons     
  self->DestroyPoints   
  
END

;+
; :Description:
;    Get access to some params. 
;
; :History:
;     Written by FaM, 2011.
;-    
PRO w_Map::GetProperty, XSIZE = xsize, YSIZE = ysize, LEVELS = levels, COLORS = colors, TNT_C = tnt_c, MAP_PARAMS = map_params
    
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
  if ARG_PRESENT(tnt_c) then self.grid->getProperty, TNT_C = tnt_c
     
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
;    LEVELS: in, optional, type = numeric
;            the data levels
;    
;    N_LEVELS: in, optional, type = long, default = 256
;              number of data levels (ignored if levels is set)
;             
;    COLORS: in, optional, type = any
;            the colors palette (array of nlevels). If not set, colors are chosen
;            automatically from nlevels and the active color table (see e.g. CTLoad)
;            
;    CMIN: in, optional, type = long
;          minimun index in the color table (ignored if COLORS is set)
;          
;    CMAX: in, optional, type = long
;          maximum index in the color table (ignored if COLORS is set)
;          
;    INVERTCOLORS: in, optional, type = boolean
;                  if the colors in the color table have to be inverted (ignored if COLORS is set)
;                  
;    NEUTRAL_COLOR: in, optional, type = color
;                   the color of the missing data on the plot
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_plot_params, LEVELS = levels, N_LEVELS = n_levels, COLORS = colors, CMIN=cmin, CMAX=cmax, $
                                  INVERTCOLORS = invertcolors, NEUTRAL_COLOR = neutral_color
         
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
     
  is_Levels = N_ELEMENTS(levels) ne 0 
  is_Colors = N_ELEMENTS(colors) ne 0
  
  ; Give a value to nlevels   
  IF N_Elements(n_levels) EQ 0 THEN BEGIN
     IF ~is_Levels and ~is_Colors THEN nlevels = 255 $
     ELSE begin
       if is_colors then nlevels = N_Elements(colors[*,0])
       if is_Levels then nlevels = N_Elements(levels)              
     endelse
  ENDIF else nlevels = n_levels
  
  if is_Levels then nlevels = N_ELEMENTS(levels)
  
  if is_colors then if (nlevels ne N_ELEMENTS(colors[*,0])) then $
    message, '$colors and $n_levels are incompatible.'
  
  ; Colors
  if is_Colors then _colors = utils_color_convert(COLORS = colors) $
   else _colors = utils_color_convert(NCOLORS = nlevels, CMIN=cmin, CMAX=cmax, INVERTCOLORS = invertcolors)
     if KEYWORD_SET(NEUTRAL_COLOR) then _neutral = utils_color_convert(COLORS = NEUTRAL_COLOR) else $
       _neutral = cgColor('white')

  ; Levels
  if is_Levels then begin 
   _levels = levels 
   val_min = min(levels)
   val_max = max(levels)   
  endif else begin
   val_min = self.plot_params.min_val
   val_max = self.plot_params.max_val
   _levels = (double(val_max - val_min) / nlevels) * Indgen(nlevels) + val_min
  endelse
    
  ; Fill up
  self->DestroyPlotParams
  if is_Levels then self.plot_params.type = 'USER' else self.plot_params.type = 'AUTO'
  self.plot_params.nlevels  = nlevels
  self.plot_params.colors   = PTR_NEW(_colors, /NO_COPY)
  self.plot_params.levels   = PTR_NEW(_levels, /NO_COPY)
  self.plot_params.min_val  = val_min
  self.plot_params.max_val  = val_max
  self.plot_params.neutral  = _neutral
  
  return, self->set_img()

end


;+
; :Description:
;    This is to define the contours of lat lons on the map. 
;
; :Keywords:
;    TYPE: in, optional, type = string, default = 'LONLAT'
;          currently, only 'LONLAT' accepted. If set to '', removes the map contours
;    
;    INTERVAL: in, optional, type = float, default =10.
;              interval between contours 
;              
;    THICK: in, optional, type = float, default =1.
;           thickness of the contour lines
;    
;    STYLE: in, optional, type = float, default =2.
;           style of the contour lines
;           
;    COLOR: in, optional, type = string, default ='dark grey'
;           color of the contour lines
;           
;    LABEL: in, optional, type=integer, default=0
;           A 0 means no contour levels are labelled. A 1 means all contour levels are
;           labelled. A 2 means label every 2nd contour level is labelled, and so on
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_map_params, TYPE = type, INTERVAL = interval, THICK = thick, STYLE = style, COLOR = color, LABEL = label, NO_TICK_LABELS = no_tick_labels
  
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
   self->DestroyMapParams
  _type = 'LONLAT'
  _interval = 10.
  _thick = 1.
  _style = 2.
  _color = 'Dark Grey'
  _label = 0
  _tick_labels = TRUE
  
  if N_ELEMENTS(TYPE) eq 1 then _type = str_equiv(TYPE)
  if N_ELEMENTS(INTERVAL) eq 1 then _interval = INTERVAL
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(LABEL) eq 1 then _label = LABEL
  if KEYWORD_SET(NO_TICK_LABELS) eq 1 then _tick_labels = FALSE
  
  self.map_params.type = _type
  self.map_params.thick = _thick
  self.map_params.style = _style
  self.map_params.color = _color
  self.map_params.labeled = _label
                           
  self.is_Mapped = _type ne ''
  
  if ~self.is_Mapped then begin
   self->DestroyMapParams
   return, 1
  endif
  
  if self.map_params.type eq 'LONLAT' then begin
  
    self.grid->get_Lonlat, lon, lat, nx, ny
    Nlevels = 360 / _interval
    levels = INDGEN(Nlevels) * _interval - 170
    p = where(levels le floor(max(Lon)) and levels ge ceil(min(Lon)), cnt)
    if cnt gt 0 then lonlevels = levels[p]
    p = where(levels le floor(max(Lat)) and levels ge ceil(min(Lat)), cnt)
    if cnt gt 0 then latlevels = levels[p]
    
    if _tick_labels then begin
    
      for i=0,N_ELEMENTS(lonlevels)-1 do begin
        p = where(Lon[*,0] le lonlevels[i] ,cnt)
        if cnt gt 1 and cnt lt nx then begin
          if N_ELEMENTS(xticks) eq 0 then xticks =  max(p) else xticks = [xticks, max(p)]
          if N_ELEMENTS(xtickValues) eq 0 then xtickValues =  lonlevels[i] else xtickValues = [xtickValues, lonlevels[i]]
        endif
      endfor
      for i=0,N_ELEMENTS(latlevels)-1 do begin
        p = where(Lat[0,*] le latlevels[i] ,cnt)
        if cnt gt 1 and cnt lt ny then begin
          if N_ELEMENTS(yticks) eq 0 then yticks =  max(p) else yticks = [yticks, max(p)]
          if N_ELEMENTS(ytickValues) eq 0 then ytickValues =  latlevels[i] else ytickValues = [ytickValues, latlevels[i]]
        endif
        
      endfor
      
    endif
    
    self.map_params.xlevels = PTR_NEW(lonlevels, /NO_COPY)
    self.map_params.ylevels = PTR_NEW(latlevels, /NO_COPY)
    self.map_params.xticks = PTR_NEW(xticks, /NO_COPY)
    self.map_params.yticks = PTR_NEW(yticks, /NO_COPY)
    self.map_params.xtickValues = PTR_NEW(xtickValues, /NO_COPY)
    self.map_params.ytickValues = PTR_NEW(ytickValues, /NO_COPY)
    
  endif else Message, 'Currently only LONLAT type is supported'
  
  return, 1

end

;+
; :Description:
;    Set shading params.
;
;
; :Keywords:
;    RELIEF_FACTOR: in, optional, type = float, default = 0.7 
;                   the strenght of shading. no rule for this,
;                   try and see (0.7 or 1.0 usually provide satisfying results)
;
; :History:
;     Written by FaM, 2011.
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
    
  _relief_factor = 0.7  
  if N_ELEMENTS(RELIEF_FACTOR) eq 1 then _relief_factor = RELIEF_FACTOR                           
  
  self.relief_factor = _relief_factor
  
  return, 1

end


;+
; :Description:
;   To set a topography for the shading layer.
;
; :Keywords:
;    GRDFILE: in, required, type = string
;             the .grd file to read (with hdr !!!)
;
;
; :History:
;     Written by DiS, FaM, 2011
;-
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
  if str_equiv(spli[N_ELEMENTS(spli)-1]) ne 'GRD' then message, WAVE_Std_Message(/FILE)
  GEN_str_subst,ret,grdfile,'grd', 'hdr', hdr
  
  self.grid->get_Lonlat, lon, lat, nx, ny ; TODO: Update routine: change this into GRID kind of things  
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
  
  if str_equiv(c.proj.NAME) eq str_equiv('Geographic (WGS-84)') then begin
    ddx = mean(c.dx * 111200 * cos(lat * !pi / 180d ))
    ddy = c.dy * 111200
  endif else begin
    ddx = c.dx
    ddy = c.dy
  endelse
  
  GIS_xy_derivatives, ret, rotate(z,7), dx = ddx, dy = ddy, DFDX=dhdx,DFDY=dhdy
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message('Error by calculating derivatives.')
  
  sl = TEMPORARY(dhdx) - TEMPORARY(dhdy) ; shade layer  
  
  PTR_FREE, self.sl
  self.sl = PTR_NEW(sl, /NO_COPY)
  self.is_Shaded = TRUE
  
  return, 1
  
end

;+
; :Description:
;    Set a shape file to draw on the map.
;    
;
; :Keywords:
;    SHPFILE: in, required
;             the shapefile to read (.shp). If not set, a dialog window will open
;    
;    SHP_SRC: in, optional
;             the shapefile coordinate system (datum or proj) default is WGS-84
;    
;    COUNTRIES: in, optional, type = boolean
;               if set, the two previous keywords are ignored and the standard world boundaries 
;               shape file is read.
;    
;    COLOR: in, optional, type = string
;           the color of the shape lines
;    
;    THICK:in, optional, type = float
;           the thick of the shape lines
;    
;    STYLE:in, optional, type = float
;          the style of the shape lines
;    
;    REMOVE_ENTITITES:in, optional, type = long
;                     an array containing the id of the shape entities to remove from the plot
;                     All other entities are plotted normally.
;                     
;    KEEP_ENTITITES:in, optional, type = long
;                   an array containing the id of the shape entities to keep for the plot. 
;                   All other entities are ignored.
;
; :History:
;     Written by FaM, 2011.
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

  if KEYWORD_SET(COUNTRIES) then begin
   GIS_make_datum, ret, shp_src, NAME = 'WGS-84'
   return, self->set_shape_file(SHPFILE = WAVE_resource_dir+'/shapes/world_borders/world_borders.shp', SHP_SRC=shp_src, $
            COLOR = color, THICK = thick, STYLE = style, REMOVE_ENTITITES = remove_entitites, KEEP_ENTITITES = keep_entitites)
  endif  
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
  
  if ~KEYWORD_SET(shp_src) then begin
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
   self.grid->get_LonLat, glon, glat
   range = [min(glon),max(glon),min(glat),max(glat)]
  end
  if is_proj then begin
   range = [-99999999999d,99999999999d,-99999999999d,99999999999d] ; TODO: Update routine: this
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
    
    self.grid->transform, x, y, x, y, SRC = shp_src
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
  
  coord = coord + 0.5 ; Because Center point of the pixel is not the true coord 

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
   nshapes = self.nshapes
   ptr_free, self.shapes
   temp = [temp, sh]
   self.shapes = PTR_NEW(temp, /NO_COPY)
   self.nshapes = nshapes + 1
  endelse
    
  self.is_Shaped = TRUE
  return, 1
  
end

;+
; :Description:
;    Set a polygon to draw on the map.
;    
;
; :Params:
;  
;    x: in, required
;       the x coordinates of the polygon to draw (at least 3 points)
;    
;    y: in, required
;       the y coordinates of the polygon to draw (at least 3 points)
;       
; :Keywords: 
; 
;    SRC: in, optional
;         the coordinate system (datum or proj) of the coordinates. Default is WGS-84
;    
;    COLOR: in, optional, type = string
;           the color of the polygon lines
;    
;    THICK:in, optional, type = float
;           the thickness of the polygon lines
;    
;    STYLE:in, optional, type = float
;          the style of the the polygon lines
;    
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_polygon, x, y, SRC = src, COLOR = color, THICK = thick, STYLE = style

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->DestroyPolygons
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  ;******************
  ; Check arguments *
  ;******************
  if N_PARAMS() ne 2 then begin
   self->DestroyPolygons
   return, 1
  endif
    
  if ~KEYWORD_SET(src) then GIS_make_datum, ret, src, NAME = 'WGS-84'

  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('src', /ARG)

  if not array_processing(x, y, REP_A0=_x, REP_A1=_y) then Message, WAVE_Std_Message('Y', /ARG)
  n_coord = N_ELEMENTS(_x)
  if n_coord lt 3 then  Message, WAVE_Std_Message('X', NELEMENTS=3)
     
   self.grid->transform, _x, _y, _x, _y, SRC = src
   coord = [1#_x,1#_y]  + 0.5 ; Because Center point of the pixel is not the true coord 

  _color = 'black'
  _style = 0.
  _thick = 1.5  
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  
  poly = {MAP_POLYGON}
  poly.color = _color
  poly.style = _style
  poly.thick = _thick  
  poly.coord = PTR_NEW(coord, /NO_COPY)
  poly.n_coord = n_coord
  
  if self.npolygons eq 0 then begin
   self.npolygons = 1
   self.polygons = PTR_NEW(poly, /NO_COPY)
  endif else begin
   temp = *self.polygons
   npolygons = self.npolygons
   ptr_free, self.polygons
   temp = [temp, poly]
   self.polygons = PTR_NEW(temp, /NO_COPY)
   self.npolygons = npolygons + 1
  endelse
    
  self.is_Polygoned = TRUE
  return, 1
  
end

;+
; :Description:
;    Set a point or an array of points to draw on the map.
;    
;  :Params:
;    
;    x: in, required
;       the x coordinates of the point(s) to draw
;    
;    y: in, required
;       the y coordinates of the point(s) to draw 
;       
; :Keywords:
;    
;    SRC: in, optional
;         the coordinate system (datum or proj) of the coordinates. Default is WGS-84
;    
;    COLOR: in, optional, type = string
;           the color of the points
;    
;    THICK:in, optional, type = float
;           the thickness of the points
;          
;    PSYM:in, optional, type = int, default = 5
;          the style of the the points
;          
;    SYMSIZE:in, optional, type = float
;            the size of the the points
;           
;    TEXT:in, optional, type = float
;          points annotation
;          
;    DELTA_TEXT:in, optional, type = float
;               a delta in relative img coordinates where to put the annotation (2 elements vector)
;               
;    ALIGN:in, optional, type = float
;          the allignment of the annotation
;    
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_point, x, y, SRC = src, COLOR = color, THICK = thick, PSYM = psym, SYMSIZE = symsize, $
                                  TEXT = text, DELTA_TEXT =  Delta_Text, ALIGN = align, CHARSIZE = charsize

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->DestroyPoints
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  ;******************
  ; Check arguments *
  ;******************
  if N_PARAMS() ne 2 then begin
   self->DestroyPoints
   return, 1
  endif
    
  if ~KEYWORD_SET(src) then GIS_make_datum, ret, src, NAME = 'WGS-84'
  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('src', /ARG)

  if not array_processing(x, y, REP_A0=_x, REP_A1=_y) then Message, WAVE_Std_Message('Y', /ARG)
  n_coord = N_ELEMENTS(_x)
  self.grid->transform, _x, _y, _x, _y, SRC = src
  coord = [1#_x,1#_y]  + 0.5 ; Because Center point of the pixel is not the true coord 
  
  if KEYWORD_SET(TEXT) then begin
    if ~ arg_okay(TEXT, TYPE=IDL_STRING) then Message, WAVE_Std_Message('TEXT', /ARG)
    if N_ELEMENTS(TEXT) eq 1 then _TEXT = REPLICATE(TEXT, n_coord) $
     else if N_ELEMENTS(TEXT) eq n_coord then  _TEXT = text $
      else  Message, WAVE_Std_Message('TEXT', /ARG)    
  endif else _TEXT = REPLICATE('', n_coord)
  
  _color = 'black'
  _psym = 5.
  _symsize = 1.
  _thick = 1.  
  _align = 0.
  _dpText = [0.005,0.005]
  _CHARSIZE = 1.
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(PSYM) eq 1 then _psym = PSYM
  if N_ELEMENTS(SYMSIZE) eq 1 then _symsize = SYMSIZE
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  if N_ELEMENTS(DELTA_TEXT) eq 2 then _dpText = DELTA_TEXT
  if N_ELEMENTS(ALIGN) eq 1 then _align = ALIGN
  if KEYWORD_SET(CHARSIZE) then _charsize = CHARSIZE
  
  point = REPLICATE({MAP_POINT}, n_coord)  
  for i = 0, n_coord -1 do begin
    point[i].thick = _thick
    point[i].psym = _psym
    point[i].symsize = _symsize
    point[i].color = _color
    point[i].text = _text[i]
    point[i].align = _align
    point[i].dpText = _dpText
    point[i].coord = [_x[i],_y[i]]
    point[i].charsize = _charsize
    point[i].thick = _thick    
  endfor

  if self.npoints eq 0 then begin
   self.npoints = n_coord
   self.points = PTR_NEW(point, /NO_COPY)
  endif else begin
   temp = *self.points
   npoints = self.npoints
   self->DestroyPoints
   temp = [temp, point]
   self.points = PTR_NEW(temp, /NO_COPY)
   self.npoints = npoints + n_coord
  endelse
     
  self.is_Pointed = TRUE
  return, 1
  
end

;+
; :Description:
;   This function is called internally each time
;   the plot params or the data are set. 
;   You do not have to call it by yourself.   
;  
; :Private:
;
; :History:
;     Written by FaM, 2011.
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

  img = INTARR(self.Xsize, self.Ysize)
  
  dataTypeName = Size(*self.data, /TNAME)
  CASE dataTypeName OF
    'FLOAT': epsilon = (MACHAR()).eps
    'DOUBLE': epsilon = (MACHAR(DOUBLE=1)).eps    
    ELSE: epsilon =0
  ENDCASE
  
  for l=0, self.plot_params.nlevels-1 do begin
    if l lt self.plot_params.nlevels-1 then p = where((*self.data) ge (*self.plot_params.levels)[l] - epsilon and (*self.data) lt (*self.plot_params.levels)[l+1], cnt) $
    else p = where((*self.data) ge (*self.plot_params.levels)[l]- epsilon, cnt)
    if cnt gt 0 then img[p]= l + 1
  endfor
    
  PTR_FREE, self.img
  SELF.img = PTR_NEW(img, /NO_COPY)
  
  return, 1

end

;+
; :Description:
;    Set the data to plot. the data is then loaded and stored,
;    and an image is generated based on the stored plot 
;    parameters.
;
; :Params:
;    data: in, required, type = 2D array
;          the data array to plot
;    
;    grid: in, optional, type = w_grid2d
;          the grid associated to the data (see 'w_grid2d::map_gridded_data'). If not set,
;          data is assumed to be in the same grid as the map and will be resized to the map 
;          grid using congrid (dangerous if you do not know what you are doing, faster if you sure
;          the data is related to the same grid)
;
; :Keywords:
;    BILINEAR: in, optional, type = boolean
;              set this if you want the data to be linearily interpolated
;              onto the map
;    
;    MISSING: in, optional, type = numeric
;             the value to give to missing points in the map (see 'w_grid2d::map_gridded_data')
;    
;    VAL_MIN: in, optional, type = numeric, default=MIN(data)
;             the minimun data value to level (ignored if levels is set using 'set_plot_params')
;    
;    VAL_MAX: in, optional, type = numeric, default=MAX(data)
;             the maximum data value to level (ignored if levels is set using 'set_plot_params')
;
;
;
; :History:
;     Written by FaM, 2011.
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
  if N_ELEMENTS(missing) ne 0 then MISS = true else MISS = false
  
  if N_ELEMENTS(grid) eq 0 then begin
     if arg_okay(img, DIM=[self.Xsize, self.Ysize], /NUMERIC) then _data = data $
      else _data = CONGRID(data, self.Xsize, self.Ysize, /CENTER, INTERP=bilinear)
  endif else begin
    _data = self.grid->map_gridded_data(data, grid, MISSING = missing, BILINEAR = bilinear)
  endelse    
  
  PTR_FREE, self.data
  self.data = PTR_NEW(_data, /NO_COPY)
  
  to_redef = N_ELEMENTS(VAL_MIN) ne 0 or N_ELEMENTS(VAL_MAX) ne 0 or self.plot_params.type eq 'AUTO'
     
  ; Levels
  if to_redef then begin
    pfin = where(finite(*self.data) eq 1, cntfin)  
    if cntfin eq 0 then MESSAGE, '$data has no finite element.'
    if N_ELEMENTS(VAL_MIN) eq 0 then _val_min = MIN((*self.data)[pfin]) else _val_min = VAL_MIN
    if N_ELEMENTS(VAL_MAX) eq 0 then _val_max = MAX((*self.data)[pfin]) else _val_max = VAL_MAX
    if MISS then begin
      dataTypeName = Size(*self.data, /TNAME)
      CASE dataTypeName OF
        'FLOAT': BEGIN
          epsilon = (MACHAR()).eps
          indices = Where( Abs(*self.data - missing) gt epsilon, count)
        END
        'DOUBLE': BEGIN
          epsilon = (MACHAR(DOUBLE=1)).eps
          indices = Where( Abs(*self.data - missing) gt epsilon, count)
        END
        ELSE: BEGIN
          indices = Where(*self.data ne missing, count)
        END
      ENDCASE
      if count ne 0 and N_ELEMENTS(VAL_MIN) eq 0 then _val_min = MIN((*self.data)[indices])
      if count ne 0 and N_ELEMENTS(VAL_MAX) eq 0 then _val_max = MAX((*self.data)[indices])
    endif    
    _levels = (double(_val_max - _val_min) / self.plot_params.nlevels) * Indgen(self.plot_params.nlevels) + _val_min
    ptr_free, self.plot_params.levels
    self.plot_params.levels  = PTR_NEW(_levels, /NO_COPY)
    self.plot_params.min_val = _val_min
    self.plot_params.max_val = _val_max
  endif

  return, self->set_img()

end


;+
; :Description:
;    If you want to add wind vectors to your plot.
;
; :Params:
;    ud: in, required, type = 2d array
;        the wind field in U direction
;    vd: in, required, type = 2d array
;        the wind field in V direction
;    grid: in, required, type = w_grid2d
;          the grid associated to the wind field (same X and Y dimensions as ud and vd)
;
; :Keywords:
;    DENSITY: in, optional, type = long, default = 3
;             the vectors density, in grid points. Supported are (1,3,5,7)
;    LENGTH: in, optional, type = float, default = 0.08
;            The maximum vectorlength relative to the plot data 
;    THICK: in, optional, type = float, default = 1
;           vectors thickness
;    COLOR: in, optional, type = string, default = "black"
;           vectors color
;
;
; :History:
;     Written by FaM, 2011.
;-
function w_Map::set_wind, ud, vd, grid, DENSITY = density , LENGTH=length, THICK=thick, COLOR = color
                             
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->DestroyWindParams
    ok = self->set_wind()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  if ~KEYWORD_SET(length) then length = 0.08
  if ~KEYWORD_SET(thick) then thick = 1
  if ~KEYWORD_SET(density) then density = 3
  if ~KEYWORD_SET(color) then color = 'black'
  type = 'VECTORS'
  
  if N_PARAMS() eq 0 then begin
   self->DestroyWindParams
   self.wind_params.type = type
   self.wind_params.length = length
   self.wind_params.thick = thick
   self.wind_params.color = color
   return, 1
  endif  
  
  if N_PARAMS() ne 3 then Message, WAVE_Std_Message(/NARG)
  
  if not OBJ_ISA(grid, 'w_Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='w_Grid2D')
  if not array_processing(ud, vd) then Message, WAVE_Std_Message(/ARG)  
  if density ne 1 and density ne 3 and density ne 5 and density ne 7 then Message, 'Density must be odd-numbered (1,3,5,7).'
           
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
  self.grid->transform_XY, x, y, c.proj, posX, posY
  posX += 0.5
  posy += 0.5  
  pok = where(posX ge 0 and posX le self.Xsize and posY ge 0 and posY le self.Ysize, cnt)
  if cnt eq 0 then Message, 'Wind grid not compatible.'
  
  utils_1d_to_2d, xi, yi, xi, yi
  velx = ud[xi,yi]
  vely = vd[xi,yi] 
  
  self->DestroyWindParams
  self.wind_params.type = type
  self.wind_params.length = length
  self.wind_params.thick = thick
  self.wind_params.color = color
  self.wind_params.velx = PTR_NEW(velx[pok], /NO_COPY)
  self.wind_params.vely = PTR_NEW(vely[pok], /NO_COPY)
  self.wind_params.posx = PTR_NEW(posx[pok], /NO_COPY)
  self.wind_params.posy = PTR_NEW(posy[pok], /NO_COPY)
  self.is_Winded = TRUE
  
  return, 1

end


;+
; :Description:
;    Change image color palette indexes into rgb image for plot without shading
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::img_to_rgb

    utils_color_rgb, [self.plot_params.neutral, *self.plot_params.colors], s_r, s_g, s_b
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
; 
;    Change image color palette indexes into rgb image for plot with shading
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::shading

  if self.relief_factor eq 0 then return, self->img_to_rgb()
  nlevels = self.plot_params.nlevels + 1
  
  if nlevels eq 0 or nlevels gt 127 then begin
   MESSAGE, 'Shading: max number of colors is 127', /INFORMATIONAL
   return, self->img_to_rgb()
  endif

  rp = bindgen(256)
  gp = bindgen(256)
  bp = bindgen(256)
  
  utils_color_rgb, [self.plot_params.neutral, *self.plot_params.colors], s_r, s_g, s_b  
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
;    Adds the contours to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::draw_map, WINDOW = window
  
  if self.map_params.type eq 'LONLAT' then begin
  
    self.grid->get_Lonlat, lon, lat
    
    cgContour, lon, POSITION = [0,0,self.Xsize,self.Ysize], /DATA,  $
      COLOR = self.map_params.color, C_LINESTYLE = self.map_params.style, /OVERPLOT, LABEL = self.map_params.labeled, $
      LEVELS = *(self.map_params.xlevels), C_THICK =  self.map_params.thick, WINDOW=window
      
    cgContour, lat, POSITION = [0,0,self.Xsize,self.Ysize], /DATA, $
      COLOR = self.map_params.color, C_LINESTYLE = self.map_params.style, /OVERPLOT, LABEL = self.map_params.labeled,$
      LEVELS = *(self.map_params.ylevels), C_THICK =  self.map_params.thick, WINDOW=window
      
  endif
  
  ; Draw a frame
  xf = [0, self.xsize, self.xsize, 0, 0]
  yf = [0, 0, self.ysize, self.ysize, 0]
  cgPlotS, xf, yf, WINDOW = window, /DATA
  
 TICK_LABEL = N_ELEMENTS(*self.map_params.xtickvalues) ne 0
  if TICK_LABEL then begin
  
    xts = *self.map_params.xticks
    yts = *self.map_params.yticks
    xls = *self.map_params.xtickvalues
    yls = *self.map_params.ytickvalues
    
    spacing = 1.
    ;  chardist = !D.Y_CH_SIZE / Float(!D.Y_Size) * $
    ;          ((StrUpCase(!Version.OS_Family) EQ 'WINDOWS') ? (0.9 * spacing) : (1.5 * spacing))    
    ddy = - 0.023 * spacing * self.ysize
    ddx = - 0.008 * spacing * self.xsize
    
    ; Tick labels
    charsize = 0.8    
    format = '(I4)'
    for i=0,N_ELEMENTS(xts)-1 do begin
      label = string(abs(xls[i]),FORMAT=format)
      if xls[i] lt 0 then label += 'W' else label += 'E'
      cgText, xts[i], ddy, GEN_strtrim(label,/ALL), ALI = 0.5, CHARSIZE = charsize, WINDOW=window, /DATA
    endfor
    for i=0,N_ELEMENTS(yts)-1 do begin
      label = string(abs(yls[i]),FORMAT=format)
      if yls[i] lt 0 then label += 'S' else label += 'N'
      cgText, ddx, yts[i]  + ddy/3., GEN_strtrim(label,/ALL), ALI = 1, CHARSIZE = charsize, WINDOW=window, /DATA
    endfor
  end
  
  return, 1
  
end

;+
; :Description:
;    Adds the shapes to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-   
function w_Map::draw_shapes, WINDOW = window  
  
  shapes = *(self.shapes)
    
  for i = 0, self.nshapes-1 do begin
    sh = shapes[i]
    index = 0
    while index lt N_ELEMENTS((*sh.conn)) do begin    
      nbElperConn = (*sh.conn)[index]      
      idx = (*sh.conn)[index+1:index+nbElperConn]      
      index += nbElperConn + 1       
      _coord = (*sh.coord) [*,idx]      
      cgPlots, _coord[0,*], _coord[1,*], /DATA,  Color=cgColor(sh.color), THICK=sh.thick, LINESTYLE=sh.style, NOCLIP=0, WINDOW = window
    endwhile  
  endfor
  
  return, 1
  
end

;+
; :Description:
;    Adds the wind vectors to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-  
function w_Map::draw_wind, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  w_partvelvec, *self.wind_params.velx, $
                *self.wind_params.vely, $
                *self.wind_params.posx, $
                *self.wind_params.posy, $
                VECCOLORS=cgColor(self.wind_params.color), $
                LENGTH = self.wind_params.length, $
                thick = self.wind_params.thick, /OVER, $              
                /DATA,  /NORMAL, WINDOW = window, NOCLIP = 0
  
  return, 1
  
end

;+
; :Description:
;    Adds the polygons to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-  
function w_Map::draw_polygons, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  for i = 0, self.npolygons-1 do begin
     poly = (*self.polygons)[i]
    _coord = *poly.coord
    cgPlots, _coord[0,*], _coord[1,*], /DATA,  Color=cgColor(poly.color), THICK=poly.thick, LINESTYLE=poly.style, NOCLIP=0, WINDOW = window
  endfor
    
  return, 1
  
end

;+
; :Description:
;    Adds the points to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-  
function w_Map::draw_points, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  for i = 0, self.npoints-1 do begin
    p = (*self.points)[i]
    if p.coord[0] lt 0 or p.coord[0] gt self.Xsize then continue
    if p.coord[1] lt 0 or p.coord[1] gt self.Ysize then continue
    cgPlots, p.coord[0], p.coord[1], /DATA,  Color=cgColor(p.color), THICK=p.thick, PSYM=p.psym, SYMSIZE = p.symsize, NOCLIP=0, WINDOW = window
    cgText, p.coord[0]+p.dpText[0]*self.Xsize, p.coord[1]+p.dpText[1]+p.dpText[1]*self.Ysize, p.text, ALIGNMENT=p.align, CHARSIZE=p.charsize, NOCLIP=0, WINDOW = window, /DATA
  endfor
  
  return, 1
  
end

;+
; :Description:
;    Adds the image to an existing plot
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2011.
;-   
pro w_Map::add_img, POSITION = position, WINDOW = window, MULTIMARGIN=multimargin, NOERASE =noerase

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if self.is_Shaded then begin
   cgImage, self->shading(),  /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MULTIMARGIN=multimargin, WINDOW = window, POSITION = position, NOERASE =noerase
  endif else begin
   utils_color_rgb,  [self.plot_params.neutral, *self.plot_params.colors], r,g,b   
   cgImage, *self.img, PALETTE= [[r],[g],[b]], WINDOW = window,  /SAVE, /NORMAL, POSITION = position, /KEEP_ASPECT_RATIO, MULTIMARGIN=multimargin, MINUS_ONE=0, NOERASE =noerase
  endelse
   
  if self.is_Shaped then ok = self->draw_shapes(WINDOW = window) 
  if self.is_Mapped then ok = self->draw_map(WINDOW = window)
  if self.is_Winded then ok = self->draw_wind(WINDOW = window)
  if self.is_Polygoned then ok = self->draw_polygons(WINDOW = window)
  if self.is_Pointed then ok = self->draw_points(WINDOW = window)
  
end

;+
; :Description:
;    To draw a color bar on an existing plot. 
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2011.
;-   
pro w_Map::add_color_bar, TITLE=title, LABELS=labels, WINDOW=window, POSITION=position, CHARSIZE=charsize, $
                          BAR_OPEN=bar_open, BAR_FORMAT=bar_format, _REF_EXTRA=extra

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if n_elements(bar_format) eq 0 then bar_format = '(F5.1)'
  if n_elements(labels) eq 0 then labels = string(*(self.plot_params.levels), FORMAT=bar_format)
  
  if self.plot_params.nlevels lt 40 then begin
    cn = self.plot_params.neutral
    w_cgDCBar, *(self.plot_params.colors), COLOR = "black", LABELS=LABELS, Position=Position, $
      TITLE=title, ADDCMD=window, CHARSIZE=charsize, BAR_OPEN=bar_open, NEUTRAL_COLOR=cn, _EXTRA=extra
  endif else begin
    utils_color_rgb, *(self.plot_params.colors), r,g,b    
    if N_ELEMENTS(r) lt 256 then begin
     r = congrid(r,256) 
     g = congrid(g,256) 
     b = congrid(b,256)       
    end
    cgColorbar, PALETTE= [[r],[g],[b]], Position=Position, _EXTRA=extra, $
        TITLE=title,  MINRANGE=self.plot_params.min_val, CHARSIZE=charsize, $
        MAXRANGE=self.plot_params.max_val > (self.plot_params.min_val+0.1), ADDCMD=window
  endelse
  
end

;+
; :Description:
;    Simple function to have a look at the plot.
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2011.
;-   
pro w_Map::show_img, RESIZABLE = resizable, TITLE = title, PIXMAP = pixmap, MARGIN = margin

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
    
  pp = !ORDER ;To restore later
  !ORDER = 0
  
  DEVICE, RETAIN=2, DECOMPOSED=1  
  
  if NOT KEYWORD_SET(title) then title = 'Map Plot'
  if NOT KEYWORD_SET(margin) then margin = 0.07
  
  xs = self.Xsize * (1.+2.*margin)
  ys = self.Ysize * (1.+2.*margin)
  
  if KEYWORD_SET(RESIZABLE) then begin
    cgWindow, WXSIZE=xs, WYSIZE=ys, WTitle=title
    cgControl, EXECUTE=0
    cgWIN = true
  endif else begin
    cgDisplay, Xs, Ys, /FREE, /PIXMAP
    xwin = !D.WINDOW
  endelse
  
  self->add_img, POSITION = [0.+margin,0.+margin,1.-margin,1.-margin], WINDOW=cgWIN
  
  if KEYWORD_SET(RESIZABLE) then cgControl, EXECUTE=1 else begin 
    img = Transpose(tvrd(/TRUE), [1,2,0])
    WDELETE, xwin
    cgDisplay, Xs, Ys, /FREE, Title=title
    cgImage, img
 endelse
 
 !ORDER = pp
  
end

;+
; :Description:
;    Simple function to have a look at the color bar.
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2011.
;-   
pro w_Map::show_color_bar, RESIZABLE = resizable, VERTICAL = vertical, _REF_EXTRA=extra

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
    
  pp = !ORDER ;To restore later
  !ORDER = 0
  
  DEVICE, RETAIN=2, DECOMPOSED=1    
  title = 'Color bar'
  
  if KEYWORD_SET(VERTICAL) then begin
    xs = self.Ysize * 0.2
    ys = self.Ysize * 0.75  
    _Position=[0.20,0.05,0.30,0.95]
  endif else begin
    xs = self.Xsize * 0.75
    ys = self.Xsize * 0.2  
    _Position=[0.10,0.4,0.90,0.6]
  endelse
  
  if KEYWORD_SET(RESIZABLE) then begin
    cgWindow, WXSIZE=xs, WYSIZE=ys, WTitle=title
    cgControl, EXECUTE=0
    cgWIN = true
  endif else begin
    cgDisplay, /FREE, XSIZE=xs, YSIZE=ys, /PIXMAP, Title=title
    xwin = !D.WINDOW
  endelse
  
  self->add_color_bar, POSITION=_Position, WINDOW=cgWIN, VERTICAL = vertical, _EXTRA=extra
  
  if KEYWORD_SET(RESIZABLE) then cgControl, EXECUTE=1 else begin 
    img = Transpose(tvrd(/TRUE), [1,2,0])
    WDELETE, xwin
    cgDisplay, /FREE, XSIZE=xs, YSIZE=ys, Title=title
    cgImage, img
 endelse
  
end