; docformat = 'rst'
;+
; 
;  w_Map is a central object of the WAVE graphics system: it generates
;  "geolocalized" 2d plots for any kind of gridded data. The role of
;  this object is to create a color image (in pixels) of gridded data 
;  that can be used for other plots (with title, legend, etc.).  
;  
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
;  computing time, and it can be undone. Afterwards, this information 
;  is stored for all the plots that will be generated further on.
;  
;  3. Set plot: the two last steps can be repeated in any order for all
;  the future plots on the map. The data to be plotted has to be set, 
;  as well as the plotting params (colors, levels)
;  
;  4. add Img: the generated plot can be shown on an external window
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
;-     
 
;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-    
pro w_Map::_DestroyPlotParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.plot_params.colors
  ptr_free, self.plot_params.levels
  ptr_free, self.plot_params.dcbar_colors
  self.plot_params = {w_Map_PLOT_PARAMS}
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-    
pro w_Map::_DestroyWindParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.wind_params.velx
  ptr_free, self.wind_params.vely
  ptr_free, self.wind_params.posx
  ptr_free, self.wind_params.posy
    
  self.wind_params = {w_Map_WIND_PARAMS}
  self.is_Winded = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::_DestroyMapParams

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 

  ptr_free, self.map_params.xticks
  ptr_free, self.map_params.yticks
  ptr_free, self.map_params.xtickvalues
  ptr_free, self.map_params.ytickvalues
  ptr_free, self.map_params.xlevels
  ptr_free, self.map_params.ylevels

  
  self.map_params = {w_Map_MAP_PARAMS}
  self.is_Mapped = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::_DestroyShapes

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
pro w_Map::_DestroyPolygons

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
pro w_Map::_DestroyPoints

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.points
  self.npoints = 0L
  self.is_Pointed = FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::_DestroyContours

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  if PTR_VALID(self.contours) then begin  
    contours = *self.contours
    for i = 0, N_ELEMENTS(contours) - 1 do begin
     ptr_free, (contours[i]).data
     ptr_free, (contours[i]).keywords     
    endfor
  endif
  
  ptr_free, self.contours
  self.ncontours= 0L
  self.is_Contoured= FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::_DestroyWindRoses

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  if PTR_VALID(self.windroses) then begin  
    wr = *self.windroses
    for i = 0, N_ELEMENTS(wr) - 1 do begin
     ptr_free, (wr[i]).wind_speed
     ptr_free, (wr[i]).wind_dir
     ptr_free, (wr[i]).keywords     
    endfor
  endif
  
  ptr_free, self.windroses
  self.nWindRoses= 0L
  self.is_WindRosed= FALSE
  
end

;+
; :Description:
;   utilitary routine to properly destroy the pointers.
;
; :History:
;     Written by FaM, 2011.
;-  
pro w_Map::_DestroyMasks

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  if PTR_VALID(self.masks) then begin  
    masks = *self.masks
    for i = 0, N_ELEMENTS(masks) - 1 do begin
     ptr_free, (masks[i]).mask  
    endfor
  endif
  
  ptr_free, self.masks
  self.nmasks= 0L
  self.is_Masked= FALSE
  
end

;;+
;; :Description:
;;    Change image color palette indexes into rgb image for plot without shading
;; 
;; :Private:
;;
;; :History:
;;     Written by FaM, 2011.
;;-    
;function w_Map::_img_to_rgb
;
;  ; Make an indexed image
;  colors = [self.plot_params.neutral, *self.plot_params.colors]
;  img = *self.img
;  if self.is_Masked then begin
;    for m=0, self.nmasks-1 do begin
;      mask = (*self.masks)[m]
;      pm = where(*mask.mask eq 1, cntm)
;      if cntm ne 0 then img[pm] = N_ELEMENTS(colors)
;      colors = [colors,mask.color]
;    endfor
;  endif
;  
;  utils_color_rgb, colors, s_r, s_g, s_b
;  r = byte(0 > s_r[img] < 255)
;  g = byte(0 > s_g[img] < 255)
;  b = byte(0 > s_b[img] < 255)
;  img = bytarr(3, self.Xsize, self.Ysize)
;  img[0,*,*] = r[*,*]
;  img[1,*,*] = g[*,*]
;  img[2,*,*] = b[*,*]
;  
;  return, img
;  
;end    

pro w_Map::_add_mask, img, colors

  for i=0L, N_ELEMENTS(*self.masks)-1 do begin
    m = (*self.masks)[i]
    pok = where(*m.mask eq 0, cnt, COMPLEMENT=pnok, NCOMPLEMENT=cntnok)
    colors = [m.color, colors]    
    if cnt ne 0 then img[pok] = img[pok] + 1
    if cntnok ne 0 then img[pnok] = 0
  endfor
  
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
function w_Map::_shading

  if PTR_VALID(self.img) then begin
    img = COLOR_QUAN(*self.img, 1, s_r, s_g, s_b, COLORS=127)
  endif else if PTR_VALID(self.info) then begin
    inf = *self.info
    img = inf.loc
    colors = inf.colors
    if self.is_Masked then self->_add_mask, img, colors    
    if N_ELEMENTS(colors) gt 127 then Message, 'N_colors to small, sorry youll have to choose other levels to do shading'
    dummy = w_gr_ColorToRGB(colors, s_r, s_g, s_b)  
  endif else begin
    message, 'Shading on what??'
  endelse
     
  rp = bindgen(256)
  gp = bindgen(256)
  bp = bindgen(256)
  
  nc = N_ELEMENTS(s_r)
  rp[0:nc-1] = s_r[*]
  gp[0:nc-1] = s_g[*]
  bp[0:nc-1] = s_b[*]
  
  ;******************
  ; Prepare _shading *
  ;******************
  sl = *self.sl
  if self.shading_params.smooth ne 0 then sl = SMOOTH(sl,self.shading_params.smooth)
  mean_sl = moment(sl, SDEV=sdev_sl)
  
  p = where(sl gt 0, cnt)
  if cnt gt 0 then sl[p] = 0.4*sin(0.5*!pi*(-1>(sl[p]/(2*sdev_sl))<1))
  undefine, p
  level = 1.0 - 0.1 * self.shading_params.relief_factor ; 1.0 for 0% and 0.9 for 100%
  sens  = 0.7 * self.shading_params.relief_factor       ; 0.0 for 0% and 0.7 for 100%
  
  ;****************
  ; Apply _shading *
  ;****************
  img = ROTATE(img,7)
  r = rp[img]
  g = gp[img]
  b = bp[img]
  
  r = byte(0 > (level*r*(1+sens*sl) < 255))
  g = byte(0 > (level*g*(1+sens*sl) < 255))
  b = byte(0 > (level*b*(1+sens*sl) < 255))
  undefine, sl
  
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
function w_Map::_draw_Map, WINDOW = window

  if self.map_params.type eq 'LONLAT' then begin
    self.grid->get_Lonlat, lon, lat
    if N_ELEMENTS(*(self.map_params.xlevels)) ne 0 then begin
      cgContour, lon, COLOR = self.map_params.color, C_LINESTYLE = self.map_params.style, /OVERPLOT, LABEL = self.map_params.labeled, $
        LEVELS = *(self.map_params.xlevels), C_THICK =  self.map_params.thick, WINDOW=window
    endif
    if N_ELEMENTS(*(self.map_params.ylevels)) ne 0 then begin
      cgContour, lat, COLOR = self.map_params.color, C_LINESTYLE = self.map_params.style, /OVERPLOT, LABEL = self.map_params.labeled,$
        LEVELS = *(self.map_params.ylevels), C_THICK =  self.map_params.thick, WINDOW=window
    endif
  endif
  
  ; Draw a frame
  if ~(self.plot_params.contour_img and ~self.is_Shaded) then begin
    xf = [0, self.xsize, self.xsize, 0, 0]
    yf = [0, 0, self.ysize, self.ysize, 0]
    cgPlotS, xf, yf, WINDOW = window, /DATA
  endif
  
  TICK_LABEL = (N_ELEMENTS(*self.map_params.xtickvalues) ne 0) or (N_ELEMENTS(*self.map_params.ytickvalues) ne 0)
  if TICK_LABEL then begin    
    spacing = 1.
    ddy = - 0.023 * spacing * self.ysize
    ddx = - 0.008 * spacing * self.xsize    
    ; Tick labels
    if !D.NAME eq 'PS' then charsize = 0.8 else charsize = double(!D.X_VSIZE) / self.Xsize * 0.7 * self.map_params.label_size_f
    charthick = charsize
    if self.map_params.interval lt 0.1 then format = '(F8.2)' $
     else if self.map_params.interval lt 1. then format = '(F8.1)' $
       else format = '(I4)'
    for i=0,N_ELEMENTS(*self.map_params.xticks)-1 do begin
      label = string(abs((*self.map_params.xtickvalues)[i]),FORMAT=format)
      if (*self.map_params.xtickvalues)[i] lt 0 then label += 'W' else label += 'E'
      cgText, (*self.map_params.xticks)[i], ddy, GEN_strtrim(label,/ALL), ALI = 0.5, WINDOW=window, /DATA, CHARSIZE=charsize, CHARTHICK=charthick
    endfor
    for i=0,N_ELEMENTS(*self.map_params.yticks)-1 do begin
      label = string(abs((*self.map_params.ytickvalues)[i]),FORMAT=format)
      if (*self.map_params.ytickvalues)[i] lt 0 then label += 'S' else label += 'N'
      if (*self.map_params.ytickvalues)[i] eq 0 then label = 'Eq.'
      cgText, ddx, (*self.map_params.yticks)[i]  + ddy/3., GEN_strtrim(label,/ALL), ALI = 1, CHARSIZE = charsize, WINDOW=window, CHARTHICK=charthick, /DATA
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
function w_Map::_draw_shapes, WINDOW = window  
  
  shapes = *(self.shapes)
    
  for i = 0LL, self.nshapes-1 do begin
    sh = shapes[i]
    index = 0
    while index lt N_ELEMENTS((*sh.conn)) do begin    
      nbElperConn = (*sh.conn)[index]      
      idx = (*sh.conn)[index+1:index+nbElperConn]      
      index += nbElperConn + 1       
      _coord = (*sh.coord) [*,idx]     
      if sh.fill then cgColorFill,  _coord[0,*] > 0, _coord[1,*] > 0, /DATA,  Color=sh.color, THICK=sh.thick, LINESTYLE=sh.style, NOCLIP=0, WINDOW = window $
      else cgPlots, _coord[0,*] > 0, _coord[1,*] > 0, /DATA,  Color=sh.color, THICK=sh.thick, LINESTYLE=sh.style, NOCLIP=0, WINDOW = window
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
function w_Map::_draw_wind, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  w_partvelvec, *self.wind_params.velx, $
                *self.wind_params.vely, $
                *self.wind_params.posx, $
                *self.wind_params.posy, $
                VECCOLORS=self.wind_params.color, $
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
function w_Map::_draw_polygons, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  for i = 0, self.npolygons-1 do begin
     poly = (*self.polygons)[i]
    _coord = *poly.coord
    cgPlots, _coord[0,*], _coord[1,*], /DATA,  Color=poly.color, THICK=poly.thick, LINESTYLE=poly.style, NOCLIP=0, WINDOW = window
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
function w_Map::_draw_points, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  for i = 0, self.npoints-1 do begin
    p = (*self.points)[i]
    if p.coord[0] lt 0 or p.coord[0] gt self.Xsize then continue
    if p.coord[1] lt 0 or p.coord[1] gt self.Ysize then continue
    cgPlots, p.coord[0], p.coord[1], /DATA,  Color=p.color, THICK=p.thick, PSYM=SymCat(p.psym), SYMSIZE = p.symsize, NOCLIP=0, WINDOW = window
    cgText, p.coord[0]+p.dpText[0]*self.Xsize, p.coord[1]+p.dpText[1]+p.dpText[1]*self.Ysize, p.text, Color=p.color, ALIGNMENT=p.align, CHARSIZE=p.charsize, NOCLIP=0, WINDOW = window, /DATA
  endfor
  
  return, 1
  
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
function w_Map::_draw_contours, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  for i = 0, self.ncontours-1 do begin
    c = (*self.contours)[i]
    cgContour, *c.data, WINDOW=window, /OVERPLOT, _EXTRA = *c.keywords
  endfor
  
  return, 1
  
end

;+
; :Description:
;    Adds the windroses to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2011.
;-  
function w_Map::_draw_windRoses, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  for i = 0, self.nWindRoses-1 do begin
    wr = (*self.windroses)[i]
    k = *wr.keywords
    c = k.center
    c = CONVERT_COORD(c[0], c[1], /DATA, /DOUBLE, /TO_NORMAL) 
    k.center = [c[0],c[1]]
    w_add_WindRose, *wr.wind_dir, *wr.wind_speed, WINDOW=window, _EXTRA = k
  endfor
  
  return, 1
  
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
;    N_LEVELS: in, optional, type = long, default = 256
;              number of data levels (ignored if levels is set)
;    CMIN: in, optional, type = long
;          minimun index in the color table
;    CMAX: in, optional, type = long
;          maximum index in the color table
;    OOB_TOP: in, optional, type = boolean
;             set this keyword to have a "top arrow" type colorbar
;             this will be done autmatically if there are oob data
;    OOB_BOT: in, optional, type = boolean
;             set this keyword to have a "ot arrow" type colorbar
;             this will be done autmatically if there are oob data
;    COLORS: in, optional
;            an array of N_LEVELS colors to use (currently accepted only for the /DCBAR case)
;    INVERTCOLORS: in, optional, type = boolean
;                  if the colors in the color table have to be inverted (ignored if COLORS is set)
;    NEUTRAL_COLOR: in, optional, type = color
;                   the color to attribute to missing data
;    MIN_VALUE: in, optional, type = numeric, default=MIN(data)
;               the smaller level (for auto generation of levels)
;    MAX_VALUE: in, optional, type = numeric, default=MAX(data)
;               the bigger level (for auto generation of levels)
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_plot_params, $
    LEVELS=levels, $
    N_LEVELS=n_levels, $
    CMIN=cmin, $
    CMAX=cmax, $
    COLORS=colors, $
    INVERTCOLORS=invertcolors, $
    DCBAR=dcbar, $
    NEUTRAL_COLOR=neutral_color, $
    OOB_TOP=oob_top, $
    OOB_BOT=oob_bot, $
    MIN_VALUE=min_value, $
    MAX_VALUE=max_value, $
    CONTOUR=contour
         
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    self->_DestroyPlotParams
    ok = self->set_Plot_Params()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
     
  is_Levels = N_ELEMENTS(LEVELS) ne 0 
  
  ; Type: 
  ; 0 for levels, 
  ; 1 for automatic, 
  ; 3 for automatic with min, 
  ; 4 for automatic with max
  ; 6 for automatic with minmax
  self->_DestroyPlotParams
  if is_Levels then self.plot_params.type = 0 else begin
    self.plot_params.type = 1 ; for automatic
    if N_ELEMENTS(MIN_VALUE) ne 0 then self.plot_params.type += 2
    if N_ELEMENTS(MAX_VALUE) ne 0 then self.plot_params.type += 3    
  endelse
  
  ; Colors
  TVLCT, r, g, b, /GET
  ; Need to reverse the colors?
  IF KEYWORD_SET(INVERTCOLORS) THEN BEGIN
     r = Reverse(r)
     g = Reverse(g)
     b = Reverse(b)
  ENDIF
  self.plot_params.colors = PTR_NEW([[r], [g], [b]])
  
  ; DC bar colors
  if N_ELEMENTS(COLORS) ne 0 then begin
    self.plot_params.dcbar_colors = PTR_NEW(colors)
  endif
   
  self.plot_params.neutral = N_ELEMENTS(NEUTRAL_COLOR) ne 0 ? cgColor(NEUTRAL_COLOR, /DECOMPOSED) : cgColor('white', /DECOMPOSED)
  self.plot_params.cmin = N_ELEMENTS(CMIN) ne 0 ? cmin : 0
  self.plot_params.cmax = N_ELEMENTS(CMAX) ne 0 ? cmax : 255
  if N_ELEMENTS(min_value) eq 1 then self.plot_params.min_val = min_value
  if N_ELEMENTS(max_value) eq 1 then self.plot_params.max_val = max_value

  ; Give a value to nlevels   
  self.plot_params.nlevels = N_Elements(N_LEVELS) ne 0 ? n_levels : self.plot_params.cmax - self.plot_params.cmin
  if is_Levels then begin
   self.plot_params.nlevels = N_ELEMENTS(levels)
   self.plot_params.levels = PTR_NEW(levels)
  endif
  
  self.plot_params.contour_img  = KEYWORD_SET(CONTOUR)
  self.plot_params.dcbar  = KEYWORD_SET(DCBAR)
  self.plot_params.oob_bot  = KEYWORD_SET(OOB_BOT)
  self.plot_params.oob_top  = KEYWORD_SET(OOB_TOP)
  
  return, self->set_img()

end


;+
; :Description:
;    This is to define the lat-lon contouring on the map. 
;
; :Keywords:
;    TYPE: in, optional, type = string, default = 'LONLAT'
;          currently, only 'LONLAT' accepted. If set to '', removes the map contours.
;          TODO: soon, 'UTM' will be implemented.
;    
;    INTERVAL: in, optional, type = float, default = 10
;              interval between contours 
;              
;    THICK: in, optional, type = float, default = 1
;           thickness of the contour lines
;    
;    STYLE: in, optional, type = float, default = 2
;           style of the contour lines
;           
;    COLOR: in, optional, type = color, default ='dark grey'
;           color of the contour lines
;           
;    LABEL: in, optional, type=integer, default=0
;           A 0 means no contour levels are labelled. A 1 means all contour levels are
;           labelled. A 2 means label every 2nd contour level is labelled, and so on
;           
;    LABEL_SIZE_FACTOR: in, optional, type=double, default=1
;                       due to the various possible displays, it makes it soetimes difficult to 
;                       know which size must have the labels. This is a factor to apply to
;                       the automatic size detection.
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_map_params, TYPE = type, INTERVAL = interval, THICK = thick, STYLE = style, COLOR = color, $
                                LABEL = label, NO_TICK_LABELS = no_tick_labels, LABEL_SIZE_FACTOR = label_size_factor
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyMapParams
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  ; This is for the Lon-Lat/UTM contours drawing
  self->_DestroyMapParams
  _type = 'LONLAT'
  _interval = 10.
  _thick = 1.
  _style = 2.
  _color = 'Dark Grey'
  _label = 0
  _tick_labels = TRUE
  _label_size_factor = 1.
  
  if N_ELEMENTS(TYPE) eq 1 then _type = str_equiv(TYPE)
  if N_ELEMENTS(INTERVAL) eq 1 then _interval = INTERVAL
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(LABEL) eq 1 then _label = LABEL
  if N_ELEMENTS(LABEL_SIZE_FACTOR) eq 1 then _label_size_factor = LABEL_SIZE_FACTOR
  if KEYWORD_SET(NO_TICK_LABELS) eq 1 then _tick_labels = FALSE
  
  self.map_params.type = _type
  self.map_params.thick = _thick
  self.map_params.style = _style
  self.map_params.color = cgColor(_color, /DECOMPOSED)
  self.map_params.labeled = _label
  self.map_params.label_size_f = _label_size_factor
  
  self.is_Mapped = _type ne ''
  
  if ~self.is_Mapped then begin
    self->_DestroyMapParams
    return, 1
  endif
  
  if self.map_params.type eq 'LONLAT' then begin
  
    self.grid->get_Lonlat, lon, lat, nx, ny
    ;Decimal factor for small contours    
    if _interval lt 0.1 then dec_factor = 100. $
     else if _interval lt 1. then dec_factor = 10. $
      else dec_factor = 1.
  
    Nlevels = (180 + 360) / _interval
    levels = INDGEN(Nlevels) * (_interval*dec_factor) - 170 * dec_factor
    p = where(levels le floor(max(Lon * dec_factor)) and levels ge ceil(min(Lon * dec_factor)), cnt)
    if cnt gt 0 then lonlevels = levels[p] / dec_factor
    p = where(levels le floor(max(Lat * dec_factor)) and levels ge ceil(min(Lat * dec_factor)), cnt)
    if cnt gt 0 then latlevels = levels[p] / dec_factor
    
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
    self.map_params.interval = _interval
    
  endif else Message, 'Currently only LONLAT type is supported'
  
  return, 1
  
end

;+
; :Description:
;    Set shading params.
;
; :Keywords:
;    RELIEF_FACTOR: in, optional, type = float, default = 0.7 
;                   the strenght of shading. no rule for this,
;                   try and see (0.7 or 1.0 usually provide satisfying results)
;
;    SMOOTH: in, optional, type = long, default = 0 
;            If the topography shading layer has to be smoothed 
;            before shading. This can be usefull to hide artefacts
;            in the DEM, or smooth edges that may occur if the DEM
;            has not a sufficient resolution for the map.
;            See `SMOOTH` for a description of the parameter. Default 
;            is to make no smoothing.  
;            
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_shading_params, RELIEF_FACTOR=relief_factor, SMOOTH=smooth
  
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
  _smooth = 0L  
  if N_ELEMENTS(RELIEF_FACTOR) eq 1 then _relief_factor = RELIEF_FACTOR                           
  if N_ELEMENTS(SMOOTH) eq 1 then _smooth = SMOOTH                           
  
  self.shading_params.relief_factor = _relief_factor
  self.shading_params.smooth = _smooth
  
  return, 1

end


;+
; :Description:
;   To set a topography for the shading layer.
;
; :Keywords:
;    DEFAULT: in, optional, type = boolean
;             set this keyword to use the default topography file.
;             Source: BLUEMARBLE SRTM image, resolution 1 minute of arc
;    GRDFILE: in, optional, type = string
;             the .grd file to read (with hdr !!!)
;    USE_GRID: in, optional, type = boolean
;              If set to 1, this forces to use the exact grid information
;              from the DEM file. Default is to use Lon-Lat neirest 
;              neighbor algorithm if possible (the DEM must be defined 
;              in geographic coordinates) and use exact grid transformation
;              in all other cases. USE_GRID is more precise, but slower.
;              In most of the cases you don't have to care about this keyword.              
;    Z: out, type = float
;       to obtain the topography values for e.g a plot
;
; :History:
;     Written by DiS, FaM, 2011
;-
function w_Map::set_topography, DEFAULT=default, GRDFILE=grdfile, USE_GRID=use_grid, Z=z
  
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
  if N_ELEMENTS(GRDFILE) eq 0 and not KEYWORD_SET(DEFAULT) then begin
    PTR_FREE, self.sl
    self.is_Shaded = false
    return, 1
  end
  
  self.grid->getProperty, tnt_c = c
  self.grid->get_Lonlat, lon, lat, nx, ny
  
  if KEYWORD_SET(DEFAULT) then begin
  
    w = OBJ_NEW('w_BlueMarble', /SRTM)
    z = FLOAT(self.grid->map_gridded_data(w->get_img(), w, /BILINEAR))
    undefine, w
    
  endif else begin
    
    spli = STRSPLIT(grdfile, '.', /EXTRACT)
    if str_equiv(spli[N_ELEMENTS(spli)-1]) ne 'GRD' then message, WAVE_Std_Message(/FILE)
    GEN_str_subst,ret,grdfile,'grd', 'hdr', hdr
        
    if N_ELEMENTS(USE_GRID) eq 0 then begin ; I decide alone
      dem = OBJ_NEW('w_DEM', FILE=grdfile)
      dem->GetProperty, TNT_C=dem_c
      if str_equiv(dem_c.proj.NAME) eq str_equiv('Geographic (WGS-84)') then _ug = FALSE else _ug = TRUE
      OBJ_DESTROY, dem
    endif else _ug = KEYWORD_SET(USE_GRID)
    
    if ~_ug then begin ; Simple NN method
    
      ; Open DEM grid
      !QUIET = 1
      GIS_open_grid, ret, info, id, FILE=hdr, /RONLY, /NO_STC
      !QUIET = 0
      if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message(/FILE)
      
      lat0 = info.coord.y0 & lon0 = info.coord.x0
      dlat = info.coord.dy & dlon = info.coord.dx
      nlon = info.coord.nx & nlat = info.coord.ny
      
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
      p = where(z le -9999, cnt)
      if cnt gt 0 then z[p] = 0
      z = FLOAT(reform(z, n_elements(lat[*,0]), n_elements(lat[0,*])))
      
    endif else begin
    
      dem = OBJ_NEW('w_DEM', FILE=grdfile)
      z = FLOAT(dem->get_Z())
      p = where(z le -9999, cnt)
      if cnt gt 0 then z[p] = 0
      z = self.grid->map_gridded_data(z, dem, MISSING = 0., /CUBIC)
      OBJ_DESTROY, dem
      
    endelse
  endelse
  
  if str_equiv(c.proj.NAME) eq str_equiv('Geographic (WGS-84)') then begin
    ddx = mean(c.dx * 111200 * cos(lat * !pi / 180d ))
    ddy = c.dy * 111200
  endif else begin
    ddx = c.dx
    ddy = c.dy
  endelse
  
  GIS_xy_derivatives, ret, rotate(z,7), dx = ddx, dy = ddy, DFDX=dhdx,DFDY=dhdy
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message('Error when calculating derivatives.')
  
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
;    COLOR: in, optional, type = color
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
;    FILL: in, optional, type = boolean
;          if the shapes have to be filled with color rather than lined
;          
;    OCEANS: in, optional, type = boolean/string
;            set this keyword to make a map with blue oceans shapes
;
;    LAKES: in, optional, type = boolean/string
;            set this keyword to make a map with blue lake shapes
;
;    RIVERS: in, optional, type = boolean/string
;            set this keyword to make a map with blue rivers
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_shape_file, SHPFILE=shpfile, SHP_SRC=shp_src, COUNTRIES=countries, $
                                COLOR=color, THICK=thick, STYLE=style, OCEANS=oceans, LAKES=lakes, RIVERS=rivers,  $
                                REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites, FILL=fill

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyShapes
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if KEYWORD_SET(OCEANS) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   if N_ELEMENTS(color) eq 0 then color = 'PBG4'
   if N_ELEMENTS(fill) eq 0 then fill = 1   
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/oceans/10m_ocean.shp', SHP_SRC=shp_src, $
             THICK=thick, STYLE=style, COLOR=color, FILL=fill)
  endif  
  
  if KEYWORD_SET(LAKES) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   if N_ELEMENTS(color) eq 0 then color = 'PBG4'
   if N_ELEMENTS(fill) eq 0 then fill = 1   
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/lakes/ne_10m_lakes.shp', SHP_SRC=shp_src, $
             THICK=thick, STYLE=style, COLOR=color, FILL=fill)
  endif  
  
  if KEYWORD_SET(RIVERS) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   if N_ELEMENTS(color) eq 0 then color = 'PBG4'
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/rivers/ne_10m_rivers_lake_centerlines.shp', SHP_SRC=shp_src, $
             THICK=thick, STYLE=style, COLOR=color, FILL=fill)
  endif  
  
  if KEYWORD_SET(COUNTRIES) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/world_borders/world_borders.shp', SHP_SRC=shp_src, $
            COLOR=color, THICK=thick, STYLE=style, REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites)
  endif  
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(shpfile) then begin
   self->_DestroyShapes
   return, 1
  endif
  
  if N_ELEMENTS(shpfile) eq 0 then shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )
  if shpfile eq '' then begin
   self->_DestroyShapes
   return, 1
  endif
  
  self.grid->transform_shape, shpfile, x, y, conn, SHP_SRC = shp_src, REMOVE_ENTITITES = remove_entitites, KEEP_ENTITITES = keep_entitites
  n_coord = N_ELEMENTS(x) 
  if n_coord eq 0 then return, 0
  coord = [1#x,1#y]
    
  _color = 'black'
  _style = 0.
  _thick = 1.5
  
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  
  sh = {w_Map_SHAPE}
  sh.color = cgColor(_color, /DECOMPOSED)
  sh.shape_file = shpfile
  sh.style = _style
  sh.thick = _thick  
  sh.conn = PTR_NEW(conn, /NO_COPY)
  sh.coord = PTR_NEW(coord, /NO_COPY)
  sh.n_coord = n_coord
  sh.fill = KEYWORD_SET(FILL)
  
  if self.nshapes eq 0 then begin
   self.nshapes = 1LL
   self.shapes = PTR_NEW(sh, /NO_COPY)
  endif else begin
   temp = *self.shapes
   nshapes = self.nshapes
   ptr_free, self.shapes
   temp = [temp, sh]
   self.shapes = PTR_NEW(temp, /NO_COPY)
   self.nshapes = nshapes + 1LL
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
;    COLOR: in, optional, type = color
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
function w_Map::set_polygon, x, y, SRC=src, COLOR=color, THICK=thick, STYLE=style

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyPolygons
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  ;******************
  ; Check arguments *
  ;******************
  if N_PARAMS() ne 2 then begin
   self->_DestroyPolygons
   return, 1
  endif
    
  if ~KEYWORD_SET(src) then GIS_make_datum, ret, src, NAME = 'WGS-84'

  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if OBJ_VALID(src) and OBJ_ISA(src, 'w_Grid2D') then is_grid = TRUE else is_grid = FALSE 
  if ~is_proj and ~is_dat and ~is_grid then Message, WAVE_Std_Message('src', /ARG)

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
  
  poly = {w_Map_POLYGON}
  poly.color = cgColor(_color, /DECOMPOSED)
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
;          the style of the the points (see symcat for plenty of possibilities)
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
    self->_DestroyPoints
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  ;******************
  ; Check arguments *
  ;******************
  if N_PARAMS() ne 2 then begin
   self->_DestroyPoints
   return, 1
  endif
    
  if N_ELEMENTS(src) eq 0 then GIS_make_datum, ret, src, NAME = 'WGS-84'

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
  _psym = 16
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
  
  point = REPLICATE({w_Map_POINT}, n_coord)  
  for i = 0, n_coord -1 do begin
    point[i].thick = _thick
    point[i].psym = _psym
    point[i].symsize = _symsize
    point[i].color = cgColor(_color, /DECOMPOSED)
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
   self->_DestroyPoints
   temp = [temp, point]
   self.points = PTR_NEW(temp, /NO_COPY)
   self.npoints = npoints + n_coord
  endelse
     
  self.is_Pointed = TRUE
  return, 1
  
end

;+
; :Description:
;   If called without arguments, it generates an info 
;   structure (called internally, you don't have
;   to worry about it).
;   
;   You can give a true color as argument to override this behavior and 
;   set by yourself a true color img (3,nx,ny) to put on the map. If you do
;   so, the data and levels will be ignored on the plot.
;   
; :Params:
;   img: in, optional
;        sets a true color image on the map. If not set, the true color
;        image is generated based on the map data, datalevels and colors.
; 
; :Keywords:
;   interpolate: in, optional, type=boolean, default=0
;         Is set, bilinear interpolation is used to resize the image. Otherwise,
;         nearest neighbor sampling is used instead.
; 
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_img, img, INTERPOLATE=interpolate
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    PTR_FREE, self.img
    PTR_FREE, self.info
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  if N_ELEMENTS(img) ne 0 then begin
    s = SIZE(img, /DIMENSIONS)
    if s[0] ne 3 then Message, '$IMG does not seem to be a TRUECOLOR image. Check it (dims = [3,nx,ny]).'
    PTR_FREE, self.img
    PTR_FREE, self.info
    if s[1] ne self.Xsize or s[2] ne self.Ysize then $
     self.img = PTR_NEW(FSC_Resize_Image(img, self.Xsize, self.Ysize, INTERPOLATE=interpolate)) $
       else self.img = PTR_NEW(img)    
    return, 1
  endif  

  ;Do info 
  ; 0 for levels, 
  ; 1 for automatic, 
  ; 3 for automatic with min, 
  ; 4 for automatic with max
  ; 6 for automatic with minmax
  
  TVLCT, rr, gg, bb, /GET
  TVLCT, *self.plot_params.colors

  t = self.plot_params.type
  if t eq 3 or t eq 6 then min_value = self.plot_params.min_val
  if t eq 4 or t eq 6 then max_value = self.plot_params.max_val
  if t eq 0 then levels = *self.plot_params.levels
  n_levels = self.plot_params.nlevels
  cmin = self.plot_params.cmin
  cmax = self.plot_params.cmax
  neutral_color = self.plot_params.neutral
  dcbar = self.plot_params.dcbar
  if PTR_VALID(self.missing) then missing = *self.missing
  if PTR_VALID(self.plot_params.dcbar_colors) and dcbar then DC_colors = *self.plot_params.dcbar_colors
  if self.plot_params.oob_top eq 1 then oob_top_color = 1
  if self.plot_params.oob_bot eq 1 then oob_bot_color = 1
  
  info = w_gr_DataLevels(*self.data, $
    LEVELS=levels, $
    N_LEVELS=n_levels, $
    NEUTRAL_COLOR=neutral_color, $
    MISSING=missing, $
    COLORS=dc_colors, $
    MIN_VALUE=min_value, $
    MAX_VALUE=max_value, $
    CMIN=cmin, $ 
    CMAX=cmax, $
    OOB_TOP_COLOR=oob_top_color, $ 
    OOB_BOT_COLOR=oob_bot_color, $
    DCBAR=dcbar) 
  
  if self.plot_params.contour_img then begin
    message, 'contour image, no'
;    cgDisplay, self.Xsize, self.Ysize, /FREE, /PIXMAP
;    xwin = !D.WINDOW
;    if FINITE(*self.missing) then begin
;      levels = [*self.missing, *self.plot_params.levels]
;      colors = [self.plot_params.neutral, *self.plot_params.colors]
;    endif else begin
;      levels = *self.plot_params.levels
;      colors = *self.plot_params.colors
;    endelse
;    n_colors = N_ELEMENTS(colors)
;    utils_color_rgb,  colors, r,g,b
;    cgContour, *self.data, /CELL_FILL, LEVELS=levels, C_COLORS = indgen(n_colors), POSITION=[0,0,1,1], XTICKLEN=-1,YTICKLEN=-1, label = 0, PALETTE=[[r],[g],[b]]
;    img_ = TVRD(/TRUE)
;    WDELETE, xwin
;    
;    img = INTARR(self.Xsize, self.Ysize)
;    for i=0, N_ELEMENTS(colors)-1 do begin
;      test = (reform(img_[0,*,*]) eq r[i]) + (reform(img_[1,*,*]) eq g[i]) + (reform(img_[2,*,*]) eq b[i])
;      pok = where(test eq 3, cnt)
;      if cnt ne 0 then img[pok] = i + 1
;    endfor
;    undefine, img_    
  endif else begin 
    PTR_FREE, self.info
    self.info = PTR_NEW(info)
  endelse
  
  TVLCT, rr, gg, bb
    
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
;    OVERPLOT: in, optional
;              if set, the data will just replace the old data on the concerned pixels
;              (ignored if no grid is given as argument)
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_data, data, grid, $
  BILINEAR=bilinear, $
  MISSING=missing, $
  OVERPLOT=overplot
                             
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
   PTR_FREE, self.missing
   self.missing = PTR_NEW()
   return, self->set_img()
  endif  
    
  if ~ arg_okay(data, N_DIM=2, /NUMERIC) then Message, WAVE_Std_Message('data', NDIMS=2)
     
  if N_ELEMENTS(grid) eq 0 then begin
    if arg_okay(data, DIM=[self.Xsize, self.Ysize], /NUMERIC) then _data = data $
    else _data = CONGRID(data, self.Xsize, self.Ysize, /CENTER, INTERP=bilinear)
  endif else begin
    if N_ELEMENTS(missing) ne 0 then _missing = missing
    if KEYWORD_SET(OVERPLOT) then _data = self.grid->map_gridded_data(data, grid, MISSING=_missing, BILINEAR=bilinear, DATA_DST=*self.data) $
     else _data = self.grid->map_gridded_data(data, grid, MISSING=_missing, BILINEAR=bilinear)
  endelse
  
  if N_ELEMENTS(missing) ne 0 then begin
    PTR_FREE, self.missing
    self.missing = PTR_NEW(missing)
  endif else PTR_FREE, self.missing
  
  PTR_FREE, self.data
  self.data = PTR_NEW(_data, /NO_COPY)
 
  return, self->set_img()

end

;+
; :Description:
;    Similar to set_data but for non-gridded data sets
;
; :Params:
;    data: in, required, type = 2D array
;          the data array to plot
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
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_ll_data, data, lon, lat, $
    SRC=src, $
    MISSING=missing
  
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

  if N_PARAMS() eq 0 then return, self->set_data()
 
  _data = self.grid->map_lonlat_data(data, lon, lat, SRC=src, MISSING = missing)
  
  return, self->set_data(_data, MISSING = missing)

end


;+
; :Description:
;    Sets a mask to overplot on the data. Currently, it is just possible to give it 
;    a color, but later it will be possible to do more things.
;    It is possible to set more then one mask.
;
; :Params:
;    mask: in, required, type = 2D array
;          the mask to overplot
;    
;    grid: in, optional, type = w_grid2d
;          the grid associated to the mask (see 'w_grid2d::map_gridded_data'). If not set,
;          mask is assumed to be in the same grid as the map and will be resized to the map 
;          grid using congrid (dangerous if you do not know what you are doing, faster if you sure
;          the mask is related to the same grid)
;
; :Keywords:
;    BILINEAR: in, optional, type = boolean
;              set this if you want the mask to be linearily interpolated
;              onto the map (usually, if it is set in `set_data` it is set
;              here, too)
;    COLOR: in, optional, type = color
;              set this to the color of the mask (string, index in the current table, etc.)
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_mask, mask, grid, BILINEAR=bilinear, COLOR=color
                             
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyMasks
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if N_PARAMS() eq 0 then begin
   self->_DestroyMasks
   return, 1
  endif  
  
  if ~ arg_okay(mask, N_DIM=2, /NUMERIC) then Message, WAVE_Std_Message('mask', NDIMS=2)
        
  if N_ELEMENTS(grid) eq 0 then begin
    if arg_okay(img, DIM=[self.Xsize, self.Ysize], /NUMERIC) then _mask = mask $
    else _mask = CONGRID(mask, self.Xsize, self.Ysize, /CENTER, INTERP=bilinear)
  endif else begin
    _mask = self.grid->map_gridded_data(mask, grid, MISSING=0, BILINEAR=bilinear)
  endelse
  
  _mask = BYTE(0 > _mask < 1)
  _color = 'grey'
  if N_ELEMENTS(color) ne 0 then _color = color
  
  smask = {w_Map_MASK}
  smask.color =  cgColor(_color, /DECOMPOSED)
  smask.mask = PTR_NEW(_mask, /NO_COPY)
    
  if self.nmasks eq 0 then begin
   self.nmasks = 1
   self.masks = PTR_NEW(smask, /NO_COPY)
  endif else begin
   temp = *self.masks
   nmasks = self.nmasks
   ptr_free, self.masks
   temp = [temp, smask]
   self.masks = PTR_NEW(temp, /NO_COPY)
   self.nmasks = nmasks + 1
  endelse
    
  self.is_Masked = TRUE  
  return, 1

end

;+
; :Description:
;    Set additional data to be contoured over the original plot.
;    This can be done as many times as needed.
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
;    
;    MISSING: in, optional, type = numeric
;             the value to give to missing points in the map (see 'w_grid2d::map_gridded_data')
;             
;    _EXTRA: in, optional
;                all the Keywords accepted by cgContour (hence by Contour, too)
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_contour, data, grid, MISSING = missing, _EXTRA = extra
                             
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyContours
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if N_PARAMS() eq 0 then begin
    self->_DestroyContours
    RETURN, 1
  endif  

  if ~ arg_okay(data, N_DIM=2, /NUMERIC) then Message, WAVE_Std_Message('data', NDIMS=2)
    
  if N_ELEMENTS(grid) eq 0 then begin
    if arg_okay(img, DIM=[self.Xsize, self.Ysize], /NUMERIC) then _data = data $
    else _data = CONGRID(data, self.Xsize, self.Ysize, /CENTER, /INTERP)
  endif else begin
    if N_ELEMENTS(missing) ne 0 then _missing = missing
    _data = self.grid->map_gridded_data(data, grid, MISSING = _missing, /BILINEAR)
  endelse
  
  if N_ELEMENTS(missing) eq 0 then begin
    dataTypeName = Size(data, /TNAME)
    CASE dataTypeName OF
      'DOUBLE': missing = !VALUES.D_NAN
      ELSE: missing = !VALUES.F_NAN
    ENDCASE
  endif  

  cont = {w_Map_CONTOUR}
  cont.data = PTR_NEW(_data, /NO_COPY)
  cont.keywords = PTR_NEW(extra)
 
  if self.ncontours eq 0 then begin
   self.ncontours = 1
   self.contours = PTR_NEW(cont, /NO_COPY)
  endif else begin
   temp = *self.contours
   ptr_free, self.contours
   temp = [temp, cont]
   self.contours = PTR_NEW(temp, /NO_COPY)
   self.ncontours = self.ncontours + 1
  endelse
    
  self.is_Contoured = TRUE
  return, 1
   
end

;+
; :Description:
;    Set Wind roses to be added to the plot.
;    This can be done as many times as needed.
;
; :Params:
;    wind_dir: in, required
;              the wind directions (MET convention, from 0 to 360 deg)
;    wind_speed: in, required
;                array of the same dimension as wind_dir.
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
;    _EXTRA: in, optional
;                all the Keywords accepted by `w_add_Windrose`
;
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_WindRose, wind_dir, wind_speed, x, y, SRC=SRC, _EXTRA = extra
                             
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyContours
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  if N_PARAMS() eq 0 then begin
    self->_DestroyWindRoses
    RETURN, 1
  endif  
  
  if N_PARAMS() ne 4 then message, WAVE_Std_Message(/NARG)  
  if ~array_processing(wind_dir, wind_speed) then message, WAVE_Std_Message(/ARG)
  
  if ~KEYWORD_SET(src) then GIS_make_datum, ret, src, NAME = 'WGS-84'
  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  if ~is_proj and ~is_dat then Message, WAVE_Std_Message('src', /ARG)

  if not array_processing(x, y, REP_A0=_x, REP_A1=_y) then Message, WAVE_Std_Message('Y', /ARG)
  self.grid->transform, _x, _y, _x, _y, SRC = src
  coord = [_x,_y]+0.5 ; Because Center point of the pixel is not the true coord

  wr = {w_Map_WindRose}
  wr.wind_dir = PTR_NEW(wind_dir)
  wr.wind_speed = PTR_NEW(wind_speed)
  
  if N_ELEMENTS(extra) ne 0 then begin
   if utils_tag_exist(extra, 'CENTER') then utils_remove_tag, extra, 'CENTER'
   if utils_tag_exist(extra, 'WIN_FACTOR') then utils_remove_tag, extra, 'WIN_FACTOR'
   if ~utils_tag_exist(extra, 'MAX_RADIUS') then extra = CREATE_STRUCT(extra, 'MAX_RADIUS', 0.1)
  endif else begin
    extra = CREATE_STRUCT('MAX_RADIUS', 0.1)
  endelse
  
  extra = CREATE_STRUCT(extra, 'CENTER', coord)
  wr.keywords = PTR_NEW(extra)

  if self.nWindRoses eq 0 then begin
   self.nWindRoses = 1
   self.windroses = PTR_NEW(wr, /NO_COPY)
  endif else begin
   temp = *self.windroses
   ptr_free, self.windroses
   temp = [temp, wr]
   self.windroses = PTR_NEW(temp, /NO_COPY)
   self.nWindRoses = self.nWindRoses + 1
  endelse
    
  self.is_WindRosed = TRUE
  return, 1
   
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
function w_Map::set_wind, ud, vd, grid, DENSITY=density , LENGTH=length, THICK=thick, COLOR=color
                             
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyWindParams
    ok = self->set_wind()
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 
  
  if N_ELEMENTS(length) eq 0 then _length = 0.08 else _length = length
  if N_ELEMENTS(thick) eq 0 then _thick = 1 else _thick = thick
  if N_ELEMENTS(density) eq 0 then _density = 3 else _density = density
  if N_ELEMENTS(color) eq 0 then _color = 'black' else _color = color
  type = 'VECTORS'
  
  if N_PARAMS() eq 0 then begin
   self->_DestroyWindParams
   self.wind_params.type = type
   self.wind_params.length = _length
   self.wind_params.thick = _thick
   self.wind_params.color = cgColor(_color, /DECOMPOSED)
   return, 1
  endif  
  
  if N_PARAMS() ne 3 then Message, WAVE_Std_Message(/NARG)
  
  if not OBJ_ISA(grid, 'w_Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='w_Grid2D')
  if not array_processing(ud, vd) then Message, WAVE_Std_Message(/ARG)  
  if density ne 1 and density ne 3 and density ne 5 and density ne 7 then Message, 'Density must be odd-numbered (1,3,5,7).'
           
  grid->getProperty, tnt_c = c   
  nxg = C.nx
  nyg = C.ny
    
  fx = FLOOR(double(nxg)/_density) ; possible points
  fy = FLOOR(double(nyg)/_density) ; possible points
  s = floor(density/2.) ; where to start (1 for 3, 2 for 5, etc.)
    
  xi = INDGEN(fx, /DOUBLE) * _density + s
  yi = INDGEN(fy, /DOUBLE) * _density + s
  
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
  
  self->_DestroyWindParams
  self.wind_params.type = type
  self.wind_params.length = _length
  self.wind_params.thick = _thick
  self.wind_params.color = cgColor(_color, /DECOMPOSED)
  self.wind_params.velx = PTR_NEW(velx[pok], /NO_COPY)
  self.wind_params.vely = PTR_NEW(vely[pok], /NO_COPY)
  self.wind_params.posx = PTR_NEW(posx[pok], /NO_COPY)
  self.wind_params.posy = PTR_NEW(posy[pok], /NO_COPY)
  self.is_Winded = TRUE
  
  return, 1

end

;+
; :Description:
;    Adds the image to the device
;
; :History:
;     Written by FaM, 2011.
;-   
pro w_Map::add_img, $
    POSITION=position, $
    WINDOW=window, $
    MULTIMARGIN=multimargin, $
    MARGIN=margin, $
    NOERASE=noerase
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if KEYWORD_SET(WINDOW) and ARG_PRESENT(position) then begin ; we have to use a trick to give POSITION as output, too
    tmp = !D.window
    cgDisplay, /FREE, XSIZE=!D.X_SIZE, YSIZE=!D.Y_SIZE, /PIXMAP
    xwin = !D.WINDOW
    if PTR_VALID(self.img) then begin
      cgImage, *self.img, /NORMAL, POSITION=position, MARGIN=margin, MULTIMARGIN=multimargin
    endif else begin
      cgImage, (*(self.info)).loc, /NORMAL, MARGIN=margin, MULTIMARGIN=multimargin, POSITION=position
    endelse
    wdelete, xwin
    wset, tmp
  endif
  
  ; Std image
  if PTR_VALID(self.img) then begin
    if self.is_Shaded then begin
      cgImage, self->_shading(), /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MARGIN=margin, MULTIMARGIN=multimargin, WINDOW=window, POSITION=position, NOERASE=noerase
    endif else begin
      cgImage, *self.img, /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MARGIN=margin, MULTIMARGIN=multimargin, WINDOW=window, POSITION=position, NOERASE=noerase
    endelse
  endif else if PTR_VALID(self.info) then begin
  
    inf = *self.info
    
    do_shade = self.is_Shaded and self.shading_params.relief_factor ne 0
    if (N_ELEMENTS(inf.colors) eq 0 or N_ELEMENTS(inf.colors) gt 128) and do_shade then begin
      MESSAGE, 'w_Map INFO: _shading impossible - number of colors too high - max 128 (including neutral color and nmasks)', /INFORMATIONAL
      do_shade = FALSE
    endif
    
    if do_shade then begin
      cgImage, self->_shading(), /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MARGIN=margin, MULTIMARGIN=multimargin, WINDOW=window, POSITION=position, NOERASE=noerase
    endif else begin
      if self.plot_params.contour_img then begin
        message, 'contour image, no'
      ;      if self.is_Masked then message, 'w_Map INFO: for CONTOUR plots the masks are ignored.'
      ;      ; Make no image but just a contour of it
      ;      if FINITE(*self.missing) then begin
      ;        levels = [*self.missing, *self.plot_params.levels]
      ;        colors = [self.plot_params.neutral, *self.plot_params.colors]
      ;      endif else begin
      ;        levels = *self.plot_params.levels
      ;        colors = *self.plot_params.colors
      ;      endelse
      ;      n_colors = N_ELEMENTS(colors)
      ;      utils_color_rgb,  colors, r,g,b
      ;      cgContour, *self.data, /CELL_FILL, LEVELS=levels, C_COLORS = indgen(n_colors), POSITION=position, XTICKLEN=0,YTICKLEN=0, label = 0, $
      ;        PALETTE=[[r],[g],[b]], /NORMAL, WINDOW=window, XTICKNAME = REPLICATE(' ', 30), YTICKNAME = REPLICATE(' ', 30)
      endif else begin
        img = inf.loc
        colors = inf.colors
        if self.is_Masked then self->_add_mask, img, colors    
        ncolors = N_ELEMENTS(colors)
        if ncolors eq 3 then row=1
        if ncolors le 1 then Message, 'N_colors to small, sorry youll have to choose other levels'
        if ncolors gt 256 then Message, 'N_colors to small, sorry youll have to choose other levels'
        palette = w_gr_ColorToRGB(colors, ROW=row)  
        cgImage, img, PALETTE=palette, WINDOW=window, /SAVE, /NORMAL, POSITION=position, /KEEP_ASPECT_RATIO, MARGIN=margin, MULTIMARGIN=multimargin, MINUS_ONE=0, NOERASE=noerase
      endelse
    endelse
    
  endif else message, 'No image set yet...'
  
  if self.is_Contoured then ok = self->_draw_contours(WINDOW = window)
  if self.is_Shaped then ok = self->_draw_shapes(WINDOW = window)
  if self.is_Mapped then ok = self->_draw_Map(WINDOW = window)
  if self.is_Winded then ok = self->_draw_wind(WINDOW = window)
  if self.is_Polygoned then ok = self->_draw_polygons(WINDOW = window)
  if self.is_Pointed then ok = self->_draw_points(WINDOW = window)
  if self.is_WindRosed then ok = self->_draw_windRoses(WINDOW = window)
  
end

;+
; :Description:
;    To draw a color bar on an existing plot. 
;
;-   
pro w_Map::add_color_bar, TITLE=title, $
    LABELS=labels, $
    WINDOW=window, $
    POSITION=position, $
    CHARSIZE=charsize, $
    FORMAT=format, $
    _EXTRA=extra

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if PTR_VALID(self.info) then begin
    inf = *self.info
    if inf.dcbar then begin
     w_gr_DCBar, inf, TITLE=title, LABELS=labels, ADDCMD=window, POSITION=position, CHARSIZE=charsize, $
                                          FORMAT=format, _EXTRA=extra
    endif else begin
     w_gr_Colorbar, inf, TITLE=title, LABELS=labels, ADDCMD=window, POSITION=position, CHARSIZE=charsize, $
                                          FORMAT=format, _EXTRA=extra
    endelse         
  endif
  
end

;+
; :Description:
;    Simple function to have a look at the plot.
;
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
pro w_Map::show_color_bar, RESIZABLE=resizable, VERTICAL=vertical, _REF_EXTRA=extra

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
    ys = self.Xsize * 0.15  
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
  
  self->add_color_bar, POSITION=_Position, WINDOW=cgWIN, VERTICAL=0, _EXTRA=extra
  
  if KEYWORD_SET(RESIZABLE) then cgControl, EXECUTE=1 else begin 
    img = Transpose(tvrd(/TRUE), [1,2,0])
    WDELETE, xwin
    cgDisplay, /FREE, XSIZE=xs, YSIZE=ys, Title=title
    cgImage, img
 endelse
  
end

;+
; :Description:
;    Get access to some params. 
;
; :History:
;     Written by FaM, 2011.
;-    
PRO w_Map::GetProperty, XSIZE = xsize, YSIZE = ysize, TNT_C = tnt_c
    
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
  if ARG_PRESENT(TNT_C) then self.grid->getProperty, TNT_C = tnt_c
     
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
  PTR_FREE, self.info 
  PTR_FREE, self.img 
  PTR_FREE, self.data 
  PTR_FREE, self.sl    
  PTR_FREE, self.missing    
  
  self->_DestroyShapes         
  self->_DestroyMapParams       
  self->_DestroyPlotParams     
  self->_DestroyWindParams     
  self->_DestroyPolygons     
  self->_DestroyPoints   
  self->_DestroyContours   
  self->_DestroyMasks
  self->_DestroyWindRoses
  
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
;                  default behavior is to add country outlines to the map automatically. 
;                  This can be a bit long. Set this keyword to prevent drawing countries
;                  automatically.
;    BLUE_MARBLE: in, optional, type = boolean/string
;                 set this keyword to make a map using the NASA Land Cover picture (low res, default)
;                 if set to a string, it is the path to an alternative jpg file to use as background
;    HR_BLUE_MARBLE: in, optional, type = boolean/string
;                    set this keyword to make a map using the NASA Land Cover picture (High res)
;    OCEANS: in, optional, type = boolean/string
;            set this keyword to make a map with blue oceans shapes
;
; :History:
;     Written by FaM, 2011.
;-    
Function w_Map::Init, grid, Xsize = Xsize,  Ysize = Ysize, FACTOR = factor, NO_COUNTRIES=no_countries, $
                            BLUE_MARBLE=blue_marble, HR_BLUE_MARBLE=hr_blue_marble, OCEANS=oceans
     
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
  
  ; Defaults
  self.data = PTR_NEW(BYTARR(self.Xsize, self.Ysize), /NO_COPY)  
  TVLCT, rr, gg, bb, /GET
  cgLoadCT, 0, /REVERSE
  dummy = self->set_plot_params(N_LEVELS=10)    
  TVLCT, rr, gg, bb
  dummy = self->set_data()
  dummy = self->set_map_params()  
  dummy = self->set_shading_params()
  dummy = self->set_wind()  
  
  if KEYWORD_SET(OCEANS) then dummy = self->set_shape_file(/OCEANS)  
  
  if ~KEYWORD_SET(NO_COUNTRIES) then dummy = self->set_shape_file(/COUNTRIES)  
  
  if KEYWORD_SET(HR_BLUE_MARBLE) then begin
    w = OBJ_NEW('w_BlueMarble', /HR)
    ok = self->set_img(Transpose(self.grid->map_gridded_data(Transpose(w->get_img(), [1,2,0]), w, /BILINEAR), [2,0,1]))
    undefine, w
  endif
  
  if KEYWORD_SET(BLUE_MARBLE) then begin    
    if arg_okay(BLUE_MARBLE, TYPE=IDL_STRING) then w = OBJ_NEW('w_BlueMarble', FILE=blue_marble) else w = OBJ_NEW('w_BlueMarble')
    ok = self->set_img(Transpose(self.grid->map_gridded_data(Transpose(w->get_img(), [1,2,0]), w, /BILINEAR), [2,0,1]))
    undefine, w
  endif
                 
  RETURN, 1
  
END

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
  struct = {w_Map_PLOT_PARAMS              , $
            type           : 0L            , $ ; USER or AUTO generated levels
            nlevels        : 0L            , $ ; data levels
            colors         : PTR_NEW()     , $ ; table colors ([r,g,b])
            levels         : PTR_NEW()     , $ ; array of nlevels data levels (if user set)
            contour_img    : FALSE         , $ ; the image is generated using contour
            dcbar          : 0B            , $ ; if a dc bar
            dcbar_colors   : PTR_NEW()     , $ ; if a dc bar, maybe colors
            oob_top        : 0B            , $ ; if a top OOB color
            oob_bot        : 0B            , $ ; if a bot OOB color
            cmin           : 0B            , $ ; color index in the table
            cmax           : 0B            , $ ; color index in the table
            neutral        : 0L            , $ ; neutral color
            min_val        : 0D            , $ ; min data level
            max_val        : 0D              $ ; max data level           
            }
  
  ; This is the information for one shape file
  struct = {w_Map_SHAPE                    , $ 
            shape_file     : ''            , $ ; path to the shape file
            thick          : 0D            , $ ; thickness or the shape line for the plot
            style          : 0D            , $ ; style or the shape line for the plot
            color          : 0L            , $ ; color or the shape line for the plot
            n_coord        : 0L            , $ ; number of coordinates in the shape (private)
            fill           : FALSE         , $ ; if the shape has to be filled
            coord          : PTR_NEW()     , $ ; coordinates of the shape points (private)
            conn           : PTR_NEW()       $ ; connivence info (private)          
            }
  
  ; This is the information for one polygon to draw
  struct = {w_Map_POLYGON                  , $ 
            thick          : 0D            , $ ; thickness or the line for the plot
            style          : 0D            , $ ; style or the line for the plot
            color          : 0L            , $ ; color or the line for the plot
            n_coord        : 0L            , $ ; number of coordinates in the polygon (private)
            coord          : PTR_NEW()       $ ; coordinates of the polygon points (private)        
            }
  
  ; This is the information for one point to draw
  struct = {w_Map_POINT                    , $ 
            thick          : 0D            , $ ; thickness of the point
            psym           : 0L            , $ ; style of the point
            symsize        : 0D            , $ ; symsize of the point
            color          : 0L            , $ ; color of the point
            text           : ''            , $ ; point annotation
            charsize       : 0D            , $ ; point annotation size
            align          : 0D            , $ ; annotation alignement
            dpText         : [0D,0D]       , $ ; delta pos of the text with respect to the point
            coord          : [0D,0D]         $ ; coordinates of the point       
            }
  
  ; This is the information for one contour to draw
  struct = {w_Map_CONTOUR                  , $ 
            keywords       : PTR_NEW()     , $ ; Keywords for cgContour 
            data           : PTR_NEW()       $ ; data to contour 
            }
  
  ; This is the information for one wind rose to draw
  struct = {w_Map_WindRose                 , $ 
            keywords       : PTR_NEW()     , $ ; Keywords for w_add_windRose 
            wind_dir       : PTR_NEW()     , $ ; wind_dir
            wind_speed     : PTR_NEW()       $ ; wind_speed
            }
  
  ; This is a mask structure
  struct = {w_Map_MASK                     , $
            mask           : PTR_new()     , $ ; the mask
            color          : 0L              $ 
            } 
  
  ; This is for the Lon-Lat/UTM contours drawing
  struct = {w_Map_MAP_PARAMS               , $
            type           : ''            , $ ; LONLAT or UTM
            xticks         : PTR_new()     , $ ; where to find the ticks on the Xaxis
            yticks         : PTR_new()     , $ ; where to find the ticks on the Yaxis
            xtickvalues    : PTR_new()     , $ ; value of the ticks on the Xaxis
            ytickvalues    : PTR_new()     , $ ; value of the ticks on the Yaxis
            xlevels        : PTR_new()     , $ ; values of the plotted contours in Xcoordinates
            ylevels        : PTR_new()     , $ ; values of the plotted contours in Ycoordinates
            t_Charsize     : 0D            , $ ; Ticks charsizes
            interval       : 0D            , $ ; The interval between ticks
            color          : 0L            , $ ; color of the contour lines
            labeled        : 0L            , $ ; if the contours have to labelled
            label_size_f   : 0D            , $ ; charsize factor
            thick          : 0D            , $ ; thickness of the contour lines
            style          : 0D              $ ; style of the contour lines
            }

  ; This is for the wind vectors 
  struct = {w_Map_WIND_PARAMS              , $
            type           : ''            , $ ; currently VECTORS
            velx           : PTR_new()     , $ ; x velocities
            vely           : PTR_new()     , $ ; y velocities
            posx           : PTR_new()     , $ ; coordinates in data device
            posy           : PTR_new()     , $ ; coordinates in data device
            color          : 0L            , $ ; color of the arrows
            thick          : 0D            , $ ; thickness of the arrows
            length         : 0D              $ ; lenght of the arrows
            }
  
  ; This is for the wind vectors 
  struct = {w_Map_SHADING_PARAMS           , $
            relief_factor : 0D             , $ ; strenght of the shading (default: 0.7)
            smooth        : 0L               $ ; slope layer smoothing width
            }
  
  ; Finaly, this is the object
  struct = { w_Map                                  , $
             grid          : OBJ_NEW()              , $ ; the grid object (nx = Xsize, ny = Ysize)
             Xsize         : 0L                     , $ ; X size of the image in pixels
             Ysize         : 0L                     , $ ; Y size of the image in pixels
             img           : PTR_NEW()              , $ ; image to plot
             info          : PTR_NEW()              , $ ; Output from w_gr_datalevels
             data          : PTR_NEW()              , $ ; active data array ([Xsize,Ysize]) of any numeric type
             missing       : PTR_NEW()              , $ ; missing values in the data array
             sl            : PTR_NEW()              , $ ; shading layer for topography shading
             nshapes       : 0L                     , $ ; number of active shape files to plot                  
             shapes        : PTR_NEW()              , $ ; array of nshapes {w_Map_SHAPE} structures                               
             npolygons     : 0L                     , $ ; number of active polygons to plot                  
             polygons      : PTR_NEW()              , $ ; array of npolygons {w_Map_POLYGON} structures                               
             npoints       : 0L                     , $ ; number of points to plot                  
             points        : PTR_NEW()              , $ ; array of npoints {w_Map_POINT} structures                               
             ncontours     : 0L                     , $ ; number of additional contours to plot                  
             contours      : PTR_NEW()              , $ ; array of ncontours {w_Map_CONTOUR} structures                               
             nWindRoses    : 0L                     , $ ; number of additional wind roses to plot                  
             windroses     : PTR_NEW()              , $ ; array of nWindRoses {w_Map_WindRose} structures                               
             nmasks        : 0L                     , $ ; number of masks                  
             masks         : PTR_NEW()              , $ ; array of namsks {w_Map_MASK} structures                               
             map_params    : {w_Map_MAP_PARAMS}     , $ ; the mapping params for contours
             plot_params   : {w_Map_PLOT_PARAMS}    , $ ; the plotting params          
             shading_params: {w_Map_SHADING_PARAMS} , $ ; the shading params          
             wind_params   : {w_Map_WIND_PARAMS}    , $ ; the wind params          
             is_Shaped     : FALSE                  , $ ; did the user specify a shape to draw?
             is_Shaded     : FALSE                  , $ ; did the user specify a DEM for shading?
             is_Polygoned  : FALSE                  , $ ; did the user specify a polygon to draw?
             is_Pointed    : FALSE                  , $ ; did the user specify a point to draw?
             is_Mapped     : FALSE                  , $ ; did the user specify a contour to draw for mapping?         
             is_Winded     : FALSE                  , $ ; did the user specify wind flows?         
             is_Contoured  : FALSE                  , $ ; did the user specify additional contour plots?      
             is_WindRosed  : FALSE                  , $ ; did the user specify additional windroses?      
             is_Masked     : FALSE                    $ ; did the user specify masks to overplot?      
             }
    
END