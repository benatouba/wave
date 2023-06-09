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
  ptr_free, self.plot_params.oob_bot_color
  ptr_free, self.plot_params.oob_top_color
  ptr_free, self.plot_params.oob_bot_arrow
  ptr_free, self.plot_params.oob_top_arrow
  ptr_free, self.plot_params.sigma
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
  ptr_free, self.wind_params.color
    
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
pro w_Map::_DestroyTexts

    ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2 
  
  ptr_free, self.texts
  self.ntexts = 0L
  self.is_Texted = FALSE
  
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

pro w_Map::_add_mask, img, colors

  for i=0L, N_ELEMENTS(*self.masks)-1 do begin
    m = (*self.masks)[i]
    pok = where(*m.mask eq 0, cnt, COMPLEMENT=pnok, NCOMPLEMENT=cntnok)
    colors = [m.color, colors]    
    if cnt ne 0 then img[pok] = img[pok] + 1
    if cntnok ne 0 then img[pnok] = 0
  endfor
  
end

function w_Map::_add_label, pos_plot, $
    TITLE=title, $
    LABEL_TYPE=label_type, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    WINDOW=window
  
  SetDefaultValue, LABEL_TYPE, 'DEFAULT'
  
  case LABEL_TYPE of
    'DEFAULT': begin
       if N_ELEMENTS(CHARSIZE) ne 0 then fac = CHARSIZE/2. else fac = 1./2.
       x0 = (pos_plot[0] + pos_plot[2])/2.
       y0 = pos_plot[3] + (pos_plot[3]-pos_plot[1]) * 0.04 * fac
       cgText, x0, y0, title, ALIGNMENT=0.5, $
         WINDOW=WINDOW, /NORMAL, CHARSIZE=charsize, CHARTHICK=charthick
    end
    'LEFT': begin
       if N_ELEMENTS(CHARSIZE) ne 0 then fac = CHARSIZE/2. else fac = 1./2.
       x0 = pos_plot[0]
       y0 = pos_plot[3] + (pos_plot[3]-pos_plot[1]) * 0.04 * fac
       cgText, x0, y0, title, ALIGNMENT=0., $
         WINDOW=WINDOW, /NORMAL, CHARSIZE=charsize, CHARTHICK=charthick
    end
    'RIGHT': begin
       if N_ELEMENTS(CHARSIZE) ne 0 then fac = CHARSIZE/2. else fac = 1./2.
       x0 = pos_plot[2]
       y0 = pos_plot[3] + (pos_plot[3]-pos_plot[1]) * 0.04 * fac
       cgText, x0, y0, title, ALIGNMENT=1., $
         WINDOW=WINDOW, /NORMAL, CHARSIZE=charsize, CHARTHICK=charthick
    end
    else: Message, 'LABEL_TYPE not recognized or not implemented yet'
  endcase
    
  return, 1
  
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
function w_Map::_draw_map, WINDOW = window

  format = self.map_params.tick_format
  decimals = 0
  if self.map_params.interval lt 0.01 then decimals = 3 $
    else if self.map_params.interval lt 0.1 then decimals = 2 $
      else if self.map_params.interval lt 1. then decimals = 1
  add_lab = 0
  case (self.map_params.type) of
    'LONLAT': begin
      self.grid->get_Lonlat, xx, yy, nx, ny
      add_lab = 1
    end
    'EN_M': begin
      self.grid->get_XY, xx, yy, nx, ny
    end
    'EN_KM': begin
      self.grid->get_XY, xx, yy, nx, ny
      xx = xx/1000.
      yy = yy/1000.
    end
    else: Message, 'Big problem'
  endcase
  
  if N_ELEMENTS(*(self.map_params.xlevels)) ne 0 then begin
    cgContour, xx, COLOR=self.map_params.color, C_LINESTYLE=self.map_params.style, /OVERPLOT, LABEL=self.map_params.labeled, $
      LEVELS=*(self.map_params.xlevels), C_THICK=self.map_params.thick, WINDOW=window
  endif
  if N_ELEMENTS(*(self.map_params.ylevels)) ne 0 then begin
    cgContour, yy, COLOR=self.map_params.color, C_LINESTYLE=self.map_params.style, /OVERPLOT, LABEL=self.map_params.labeled,$
      LEVELS=*(self.map_params.ylevels), C_THICK=self.map_params.thick, WINDOW=window
  endif

  tick_label = (N_ELEMENTS(*self.map_params.xtickvalues) ne 0) or (N_ELEMENTS(*self.map_params.ytickvalues) ne 0)
  if tick_label then begin 
    
    x_ddy = - 0.023 * self.ysize + self.map_params.xtick_dy * self.ysize
    x_ddx = self.map_params.xtick_dx * self.xsize    
    y_ddx = - 0.008 * self.ysize + self.map_params.ytick_dx * self.xsize 
    y_ddy = self.map_params.ytick_dy * self.ysize + x_ddy/3.
     
    ; Tick labels
    if !D.NAME eq 'PS' then charsize = 0.8 * self.map_params.label_size_f else charsize = double(!D.X_VSIZE) / self.Xsize * 0.7 * self.map_params.label_size_f
    
    charthick = charsize
    incr = self.map_params.tick_interval
    for i=self.map_params.xtick_start, N_ELEMENTS(*self.map_params.xticks)-1, incr do begin
      if add_lab then begin
        label = w_Str(abs((*self.map_params.xtickvalues)[i]), decimals, FORMAT=format)
        if (*self.map_params.xtickvalues)[i] lt 0 then label += 'W' else label += 'E'
      endif else label = w_Str((*self.map_params.xtickvalues)[i], decimals, FORMAT=format)
      cgText, (*self.map_params.xticks)[i] + x_ddx, x_ddy, GEN_strtrim(label,/ALL), ALI = 0.5, WINDOW=window, /DATA, CHARSIZE=charsize, CHARTHICK=charthick
    endfor
    for i=self.map_params.ytick_start,N_ELEMENTS(*self.map_params.yticks)-1, incr do begin
      if add_lab then begin
        label = w_Str(abs((*self.map_params.ytickvalues)[i]), decimals, FORMAT=format)
        if (*self.map_params.ytickvalues)[i] lt 0 then label += 'S' else label += 'N'
        if (*self.map_params.ytickvalues)[i] eq 0 then label = 'Eq.'
      endif else label = w_Str((*self.map_params.ytickvalues)[i], decimals, FORMAT=format)
      cgText, y_ddx, (*self.map_params.yticks)[i] + y_ddy, GEN_strtrim(label,/ALL), ALI = 1, CHARSIZE=charsize, WINDOW=window, CHARTHICK=charthick, /DATA
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
function w_Map::_draw_shapes, WINDOW=window
  _w = KEYWORD_SET(WINDOW)
  
  shapes = *(self.shapes)
  for i = 0LL, self.nshapes-1 do begin
    sh = shapes[i]
    index = 0
    if sh.fill then begin
      is_int = 0
      new_roi = 1
      while index lt N_ELEMENTS((*sh.conn)) do begin
        nbElperConn = (*sh.conn)[index]
        next_is = (*sh.conn)[index+1]
        idx = (*sh.conn)[index+2:index+nbElperConn+1]
        index += nbElperConn + 2
        _coord = (*sh.coord) [*,idx]
        if new_roi then roi = OBJ_NEW('w_ROIGroup')
        new_roi = 0
        roi_ = OBJ_NEW('IDLanROI', _coord[0,*], _coord[1,*])
        roi_->SetProperty, INTERIOR=is_int
        roi->Add,roi_
        if next_is eq 0 then begin
          cgDRAW_ROI, roi, NOCLIP=0, /DATA,  Color=sh.color, ADDCMD=window
          if ~ _w then OBJ_DESTROY, roi
          new_roi = 1
        endif
        cgPlots, _coord[0,*], _coord[1,*], /DATA,  Color=sh.color, THICK=sh.thick, LINESTYLE=sh.style, NOCLIP=0, WINDOW=window
        is_int = next_is
      endwhile
    endif else begin
      while index lt N_ELEMENTS((*sh.conn)) do begin
        nbElperConn = (*sh.conn)[index]
        idx = (*sh.conn)[index+1:index+nbElperConn]
        index += nbElperConn + 1
        _coord = (*sh.coord) [*,idx]
        cgPlots, _coord[0,*], _coord[1,*], /DATA,  Color=sh.color, THICK=sh.thick, LINESTYLE=sh.style, NOCLIP=0, WINDOW=window
      endwhile
    endelse
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
function w_Map::_draw_wind, WINDOW=window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  w_velvec, *self.wind_params.posx, $
    *self.wind_params.posy, $
    *self.wind_params.velx, $
    *self.wind_params.vely, $
    COLORS=*self.wind_params.color, $
    LENGTH=self.wind_params.length, $
    THICK=self.wind_params.thick, $
    STDVEL=self.wind_params.stdvel, $
    /DATA, WINDOW=window, NOCLIP=self.wind_params.noclip
  
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
;     22.09.2016 - Text for points can now be in the same color. - DF
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
    cgPlots, p.coord[0], p.coord[1], /DATA,  Color=p.color, PSYM=p.psym, SYMSIZE = p.symsize, NOCLIP=0, WINDOW = window, THICK=p.thick
    cgText, p.coord[0]+p.dpText[0]*self.Xsize, p.coord[1]+p.dpText[1]+p.dpText[1]*self.Ysize, p.text, ALIGNMENT=p.align, CHARSIZE=p.charsize, NOCLIP=0, WINDOW = window, /DATA, COLOR=p.color
  endfor
  
  return, 1
  
end

;+
; :Description:
;    Adds the texts to the device
; 
; :Private:
;
; :History:
;     Written by FaM, 2013.
;-  
function w_Map::_draw_texts, WINDOW = window

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  for i = 0, self.ntexts-1 do begin
    p = (*self.texts)[i]
    if p.coord[0] lt 0 or p.coord[0] gt self.Xsize then continue
    if p.coord[1] lt 0 or p.coord[1] gt self.Ysize then continue
    cgText, p.coord[0], p.coord[1], p.text, ALIGNMENT=p.align, CHARSIZE=p.charsize, COLOR=p.color, NOCLIP=0, WINDOW=window, /DATA
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
    w_WindRose_addrose, *wr.wind_dir, *wr.wind_speed, WINDOW=window, _EXTRA = k
  endfor
  
  return, 1
  
end

;+
; :Description:
;    Adds the image to the device
; 
; :Private:
;
;-  
function w_Map::_draw_image, POSITION=position, $
    WINDOW=window, $
    MULTIMARGIN=multimargin, $
    MARGIN=margin, $
    NOERASE=noerase

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

 if KEYWORD_SET(WINDOW) then begin ; we have to use a trick to give POSITION as output
    void = cgQuery(/CURRENT, DIMENSIONS=dims)
    cgDisplay, /FREE, XSIZE=dims[0], YSIZE=dims[1], /PIXMAP
    xwin = !D.WINDOW
    if N_ELEMENTS(position) eq 4 then out_pos=position
    if PTR_VALID(self.img) then begin
      cgImage, *self.img, /NORMAL, POSITION=out_pos, MARGIN=margin, MULTIMARGIN=multimargin, /SAVE
    endif else begin
      cgImage, (*(self.info)).loc, /NORMAL, MARGIN=margin, MULTIMARGIN=multimargin, POSITION=out_pos, /SAVE
    endelse
    wdelete, xwin
  endif  
  
  if PTR_VALID(self.img) then begin
    ; User image
    if self.is_Shaded then begin
      cgImage, self->_shading(), /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MARGIN=margin, MULTIMARGIN=multimargin, WINDOW=window, POSITION=position, NOERASE=noerase
    endif else begin
      cgImage, *self.img, /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MARGIN=margin, MULTIMARGIN=multimargin, WINDOW=window, POSITION=position, NOERASE=noerase
    endelse
  endif else begin
    ; data image
    inf = *self.info
    
    do_shade = self.is_Shaded and self.shading_params.relief_factor ne 0
    if do_shade then begin
      img = inf.loc
      colors = inf.colors
      if self.is_Masked then self->_add_mask, img, colors
      if N_ELEMENTS(colors) gt 128 then begin
        Message, 'To many colors to do shading (' + str_equiv(N_ELEMENTS(colors)) +', should be less or equal 128). Keep in mind that N_COLORS = N_LEVELS+1 + N_MASKS + MISSINGCOLOR', /INFORMATIONAL
      do_shade = FALSE
      endif
    endif
    
    if do_shade then begin
      cgImage, self->_shading(), /SAVE, /NORMAL, /KEEP_ASPECT_RATIO, MINUS_ONE=0, MARGIN=margin, MULTIMARGIN=multimargin, WINDOW=window, POSITION=position, NOERASE=noerase
    endif else begin
      if self.plot_params.contour_img then begin
        message, 'contour image, no'
      endif else begin
        img = inf.loc
        colors = inf.colors
        if self.is_Masked then self->_add_mask, img, colors
        ncolors = N_ELEMENTS(colors)
        if ncolors eq 3 then row=1
        if ncolors le 1 then Message, 'Too few colors. Keep in mind that N_COLORS = N_LEVELS+1 + N_MASKS + MISSINGCOLOR'
        if ncolors gt 256 then Message, 'Too many colors ('+str_equiv(ncolors)+'). Keep in mind that N_COLORS = N_LEVELS+1 + N_MASKS + MISSINGCOLOR'
        palette = w_gr_ColorToRGB(colors, ROW=row)
        cgImage, img, PALETTE=palette, WINDOW=window, /SAVE, /NORMAL, POSITION=position, /KEEP_ASPECT_RATIO, MARGIN=margin, MULTIMARGIN=multimargin, MINUS_ONE=0, NOERASE=noerase
      endelse
    endelse
    
  endelse
  
  if N_ELEMENTS(out_pos) ne 0 then position = out_pos
  
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
;    HIST_EQUAL: in, optional, default=0
;                to decide the levels so that each "bin" between levels
;                contains as many elements as the others.
;    SIGMA: in, optional, default=0
;           min and max values are decided as follows: [mean-p1*sigma,mean+p2*sigma]
;           where mean is the mean value and sigma the standard deviation of the
;           data. Set sigma=1 to set p1 and p2 to 1, sigma=2 set p1 and p2 to 2,
;           sigma=[2,1] to set p1 to 2 and p2 to 1, etc. 
;    COLORS: in, optional
;            an array of N_LEVELS colors to use
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
    OOB_TOP_COLOR=oob_top_color, $ 
    OOB_BOT_COLOR=oob_bot_color, $
    OOB_TOP_ARROW=oob_top_arrow, $ 
    OOB_BOT_ARROW=oob_bot_arrow, $
    SIGMA=sigma, $ 
    HIST_EQUAL=hist_equal, $
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
  
  self.plot_params.nlevels = self.plot_params.nlevels
  
  self.plot_params.contour_img  = KEYWORD_SET(CONTOUR)
  self.plot_params.dcbar  = KEYWORD_SET(DCBAR)
  ptr_free, self.plot_params.oob_bot_color 
  ptr_free, self.plot_params.oob_top_color 
  ptr_free, self.plot_params.oob_bot_arrow 
  ptr_free, self.plot_params.oob_top_arrow 
  ptr_free, self.plot_params.sigma 
  if N_ELEMENTS(OOB_BOT_COLOR) ne 0 then self.plot_params.oob_bot_color = PTR_NEW(OOB_BOT_COLOR) 
  if N_ELEMENTS(OOB_TOP_COLOR) ne 0 then self.plot_params.oob_top_color = PTR_NEW(OOB_TOP_COLOR)
  if N_ELEMENTS(OOB_BOT_ARROW) ne 0 then self.plot_params.oob_bot_arrow = PTR_NEW(OOB_BOT_ARROW) 
  if N_ELEMENTS(OOB_TOP_ARROW) ne 0 then self.plot_params.oob_top_arrow = PTR_NEW(OOB_TOP_ARROW)
  if N_ELEMENTS(SIGMA) ne 0 then self.plot_params.sigma = PTR_NEW(SIGMA)
  self.plot_params.hist_equal = KEYWORD_SET(HIST_EQUAL)
  
  return, self->set_img()

end


;+
; :Description:
;    This is to define the lat-lon contouring on the map. 
;
; :Keywords:
;    TYPE: in, optional, type=string, default='LONLAT'
;          either LONLAT, EN_M (eastings northings in m),
;          or EN_KM (eastings northings in km);         
;    INTERVAL: in, optional, type = float, default = 10
;              interval between contours
;    YINTERVAL: in, optional, type = float, default = INTERVAL
;               interval between Y contours
;    NO_TICK_LABELS: in, optional
;                    do not label the ticks
;    TICK_INTERVAL: in, optional, type=long, default=1
;                   Tick annotations every TICK_INTERVAL contours
;    TICK_FORMAT: in, optional, type=string
;                 Tick annotations format (default is automatic)
;    THICK: in, optional, type = float, default = 1
;           thickness of the contour lines
;    STYLE: in, optional, type = float, default = 2
;           style of the contour lines
;    COLOR: in, optional, type = color, default ='dark grey'
;           color of the contour lines
;    LABEL: in, optional, type=integer, default=0
;           A 0 means no contour levels are labelled. A 1 means all contour levels are
;           labelled. A 2 means label every 2nd contour level is labelled, and so on
;    CHARSIZEFACTOR: in, optional, type=double, default=1
;                       due to the various possible displays, it makes it soetimes difficult to 
;                       know which size must have the labels. This is a factor to apply to
;                       the automatic size detection.
;    XTICK_START: in, optional, type=long, default=0
;                 in combination with TICK_INTERVAL, startig index of the xticks
;    XTICK_DX: in, optional, type=float, default=0
;              apply a factor to place the xtick mark correctly on the map side
;    XTICK_DY: in, optional, type=float, default=0
;              apply a factor to place the xtick mark correctly on the map side
;    YTICK_START: in, optional, type=long, default=0
;                 in combination with TICK_INTERVAL, startig index of the xticks
;    YTICK_DX: in, optional, type=float, default=0
;              apply a factor to place the ytick mark correctly on the map side
;    YTICK_DY: in, optional, type=float, default=0
;              apply a factor to place the ytick mark correctly on the map side
;              
;    
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_map_params,  $
    TYPE=type, $
    INTERVAL=interval, $
    YINTERVAL=yinterval, $
    TICK_INTERVAL=tick_interval, $
    TICK_FORMAT=tick_format, $
    THICK=thick, $
    STYLE=style, $
    COLOR=color, $
    LABEL=label, $
    NO_TICK_LABELS=no_tick_labels, $
    CHARSIZEFACTOR=charsizefactor, $
    XTICK_START=xtick_start, $
    XTICK_DX=xtick_dx, $
    XTICK_DY=xtick_dy, $
    YTICK_START=ytick_start, $
    YTICK_DX=ytick_dx, $
    YTICK_DY=ytick_dy
    
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
  _tick_interval = 1
  _tick_format = ''
  _label_size_factor = 1.
  _xtick_start = 0
  _xtick_dx = 0.
  _xtick_dy = 0.
  _ytick_start = 0
  _ytick_dx = 0.
  _ytick_dy = 0.

  if N_ELEMENTS(TYPE) eq 1 then _type = str_equiv(TYPE)
  if N_ELEMENTS(INTERVAL) eq 1 then _interval = INTERVAL
  _yinterval = _interval
  if N_ELEMENTS(YINTERVAL) eq 1 then _yinterval = YINTERVAL
  if N_ELEMENTS(TICK_INTERVAL) eq 1 then _tick_interval = TICK_INTERVAL
  if N_ELEMENTS(TICK_FORMAT) eq 1 then _tick_format = TICK_FORMAT
  if N_ELEMENTS(THICK) eq 1 then _thick = THICK
  if N_ELEMENTS(STYLE) eq 1 then _style = STYLE
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(LABEL) eq 1 then _label = LABEL
  if N_ELEMENTS(CHARSIZEFACTOR) eq 1 then _label_size_factor = CHARSIZEFACTOR
  if KEYWORD_SET(NO_TICK_LABELS) eq 1 then _tick_labels = FALSE
  
  if N_ELEMENTS(XTICK_START) eq 1 then _xtick_start = XTICK_START
  if N_ELEMENTS(YTICK_START) eq 1 then _ytick_start = YTICK_START
  if N_ELEMENTS(XTICK_DX) eq 1 then _xtick_dx = XTICK_DX
  if N_ELEMENTS(XTICK_DY) eq 1 then _xtick_dy = XTICK_DY
  if N_ELEMENTS(YTICK_DX) eq 1 then _ytick_dx = YTICK_DX
  if N_ELEMENTS(YTICK_DY) eq 1 then _ytick_dy = YTICK_DY
  
  self.map_params.type = _type
  self.map_params.thick = _thick
  self.map_params.style = _style
  self.map_params.color = cgColor(_color, /DECOMPOSED)
  self.map_params.labeled = _label
  self.map_params.label_size_f = _label_size_factor
  
  self.map_params.xtick_start = _xtick_start
  self.map_params.ytick_start = _ytick_start
  self.map_params.xtick_dx = _xtick_dx
  self.map_params.xtick_dy = _xtick_dy
  self.map_params.ytick_dx = _ytick_dx
  self.map_params.ytick_dy = _ytick_dy
  
  self.is_Mapped = _type ne ''
  
  if ~self.is_Mapped then begin
    self->_DestroyMapParams
    return, 1
  endif
  
  case (self.map_params.type) of  
    'LONLAT': begin
      self.grid->get_Lonlat, xx, yy, nx, ny
    end
    'EN_M': begin
      self.grid->get_XY, xx, yy, nx, ny
    end
    'EN_KM': begin
      self.grid->get_XY, xx, yy, nx, ny
      xx = xx/1000
      yy = yy/1000      
    end
    else: Message, 'Contour type not supported: ' + self.map_params.type
  endcase

  ;Change XY into interval coordinates
  _xx = xx / _interval
  _yy = yy / _yinterval
  minmax_x = [ceil(min(_xx)), floor(max(_xx))]
  minmax_y = [ceil(min(_yy)), floor(max(_yy))]
  
  ;Back to normal coordinates
  nxx = minmax_x[1] - minmax_x[0] + 1
  nyy = minmax_y[1] - minmax_y[0] + 1
  if nxx ne 0 then xlevels = minmax_x[0] * _interval +  DINDGEN(nxx) * _interval
  if nyy ne 0 then ylevels = minmax_y[0] * _yinterval +  DINDGEN(nyy) * _yinterval

  ;The labels 
  if _tick_labels then begin
    _xx = xx[*,0]
    for i=0,N_ELEMENTS(xlevels)-1 do begin
      p = where(_xx le xlevels[i], cnt)
      if cnt gt 1 and cnt lt nx then begin
        if N_ELEMENTS(xticks) eq 0 then xticks =  max(p) else xticks = [xticks, max(p)]
        if N_ELEMENTS(xtickValues) eq 0 then xtickValues =  xlevels[i] else xtickValues = [xtickValues, xlevels[i]]
      endif
    endfor
    _yy = yy[0,*]
    for i=0,N_ELEMENTS(ylevels)-1 do begin
      p = where(_yy le ylevels[i], cnt)
      if cnt gt 1 and cnt lt ny then begin
        if N_ELEMENTS(yticks) eq 0 then yticks =  max(p) else yticks = [yticks, max(p)]
        if N_ELEMENTS(ytickValues) eq 0 then ytickValues =  ylevels[i] else ytickValues = [ytickValues, ylevels[i]]
      endif
    endfor
  endif
  
  self.map_params.xlevels = PTR_NEW(xlevels, /NO_COPY)
  self.map_params.ylevels = PTR_NEW(ylevels, /NO_COPY)
  self.map_params.xticks = PTR_NEW(xticks, /NO_COPY)
  self.map_params.yticks = PTR_NEW(yticks, /NO_COPY)
  self.map_params.xtickValues = PTR_NEW(xtickValues, /NO_COPY)
  self.map_params.ytickValues = PTR_NEW(ytickValues, /NO_COPY)
  self.map_params.tick_format = _tick_format
  self.map_params.tick_interval = _tick_interval
  self.map_params.interval = min([_tick_interval, _yinterval])
  
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
  
    w = OBJ_NEW('w_geographic', WAVE_RESOURCE_DIR+'/topo/gtopo_1min.nc')
    z = FLOAT(self.grid->map_gridded_data(w->getVarData('h'), w, /BILINEAR))
    undefine, w
    
  endif else begin
    
    spli = STRSPLIT(grdfile, '.', /EXTRACT)
    
    ending = spli[N_ELEMENTS(spli)-1]
    if str_equiv(ending) eq 'TIFF' then ending = 'tif'
    isGeo = 0
    
    case (str_equiv(ending)) of
      'GRD': begin
        GEN_str_subst,ret,grdfile,'grd', 'hdr', hdr
        if N_ELEMENTS(USE_GRID) eq 0 then begin ; I decide alone
          dem = OBJ_NEW('w_DEM', grdfile)
          dem->GetProperty, TNT_C=dem_c
          if str_equiv(dem_c.proj.NAME) eq str_equiv('Geographic (WGS-84)') then _ug = FALSE else _ug = TRUE
          OBJ_DESTROY, dem
        endif else _ug = KEYWORD_SET(USE_GRID)
      end
      'TIF': begin
        _ug = 1
        isGeo = 1      
      end
      else: Message, 'GRDFILE not recognised'
    endcase
    
    if ~_ug then begin ; Simple NN method
    
      ; Open DEM grid
      !QUIET = 1
      GIS_open_grid, ret, info, id, FILE=hdr, /RONLY, /NO_STC
      GIS_close_grid, ret, id
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
      if isGeo then dem = OBJ_NEW('w_GEOTIFF', grdfile, /NO_DELTA) $
         else dem = OBJ_NEW('w_DEM', grdfile, /NO_DELTA)
      z = FLOAT(dem->getVarData())
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
  
  GIS_xy_derivatives, ret, rotate(z,7), dx=ddx, dy=ddy, DFDX=dhdx,DFDY=dhdy
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
;    ENTRULE:in, optional, type=string
;            the name of a compiled FUNCTION to call at each
;            iteration over the shapefile entities. It can be used
;            to e.g filter entities after shapefile specific criterias.
;            the function has to return 1 if the entity must be kept, 0 if not
;            the function must accept two arguments, entity and i (index of the entity)
  
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
                                REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites, FILL=fill,  $
                                ENTRULE=entrule

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
             THICK=thick, STYLE=style, COLOR=color, FILL=fill, REMOVE_ENTITITES=remove_entitites, $
             KEEP_ENTITITES=keep_entitites, ENTRULE=entrule)
  endif  
  
  if KEYWORD_SET(LAKES) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   if N_ELEMENTS(color) eq 0 then color = 'PBG4'
   if N_ELEMENTS(fill) eq 0 then fill = 1   
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/lakes/ne_10m_lakes.shp', SHP_SRC=shp_src, $
             THICK=thick, STYLE=style, COLOR=color, FILL=fill, REMOVE_ENTITITES=remove_entitites, $
             KEEP_ENTITITES=keep_entitites, ENTRULE=entrule)
  endif  
  
  if KEYWORD_SET(RIVERS) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   if N_ELEMENTS(color) eq 0 then color = 'PBG4'
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/rivers/ne_10m_rivers_lake_centerlines.shp', SHP_SRC=shp_src, $
             THICK=thick, STYLE=style, COLOR=color, FILL=fill, REMOVE_ENTITITES=remove_entitites, $
             KEEP_ENTITITES=keep_entitites, ENTRULE=entrule)
   endif  
  
  if KEYWORD_SET(COUNTRIES) then begin
   GIS_make_datum, ret, shp_src, NAME='WGS-84'
   return, self->set_shape_file(SHPFILE=WAVE_resource_dir+'/shapes/world_borders/world_borders.shp', SHP_SRC=shp_src, $
            COLOR=color, THICK=thick, STYLE=style, REMOVE_ENTITITES=remove_entitites, $
             KEEP_ENTITITES=keep_entitites, ENTRULE=entrule, FILL=fill)
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
  
  self.grid->transform_shape, shpfile, x, y, conn, SHP_SRC=shp_src, ENTRULE=entrule, $
    REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites, MARK_INTERIOR=fill
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
    
  if N_ELEMENTS(src) eq 0 then GIS_make_datum, ret, src, NAME = 'WGS-84'

  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  is_grid = FALSE 
  if OBJ_VALID(src) then if OBJ_ISA(src, 'w_Grid2D') then is_grid = TRUE
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
;    Set a prediefined form to draw on the map.
;    Currently, we have implemented::
;       ELLIPSE:
;         [x,y] = center of the ellipse
;         PARAM_1= major semi-axis
;         PARAM_2= minor semi-axis
;         PARAM_3= rotation of the ellips
;         
;       RECTANGLE:
;         x = [Left,Right] corners of the rectangle
;         y = [Bottom,Top] corners of the rectangle
;         
; :Params:
;    x: in, required
;       the x coordinate the form (see form doc for details)
;    y: in, required
;       the y coordinate the form (see form doc for details)
;       
; :Keywords: 
;    FORM: in, required
;          the form ('ELLIPSE' or 'RECTANGLE')
;    SRC: in, optional
;         the coordinate system (datum or proj) of the coordinates. Default is WGS-84
;    COLOR: in, optional, type = color
;           the color of the polygon lines
;    THICK:in, optional, type = float
;           the thickness of the polygon lines
;    STYLE:in, optional, type = float
;          the style of the the polygon lines
;    PARAM_1:in, optional, type = float
;            the parameter of the form
;    PARAM_2:in, optional, type = float
;            the parameter of the form
;    PARAM_3:in, optional, type = float
;            the parameter of the form
;            
; :History:
;     Written by FaM.
;     Changes: 08.12.2016 - Small change in ellipse coordinates to handle 1D array. - DF
;          
;-    
function w_Map::set_form, x, y, $
  SRC=src, $
  COLOR=color, $
  THICK=thick, $
  STYLE=style, $
  FORM=form, $
  PARAM_1=param_1, $
  PARAM_2=param_2, $
  PARAM_3=param_3

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
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
    
  if N_ELEMENTS(src) eq 0 then GIS_make_datum, ret, src, NAME = 'WGS-84'

  if arg_okay(src, STRUCT={TNT_PROJ}) then is_proj = TRUE else is_proj = FALSE 
  if arg_okay(src, STRUCT={TNT_DATUM}) then is_dat = TRUE else is_dat = FALSE 
  is_grid = FALSE 
  if OBJ_VALID(src) then if OBJ_ISA(src, 'w_Grid2D') then is_grid = TRUE
  if ~is_proj and ~is_dat and ~is_grid then Message, WAVE_Std_Message('src', /ARG)
  if N_ELEMENTS(x) eq 0 then Message, WAVE_Std_Message('x', /ARG)
  if N_ELEMENTS(y) eq 0 then Message, WAVE_Std_Message('y', /ARG)
    
  case str_equiv(FORM) of
    'ELLIPSE': begin
       self.grid->transform, x, y, _x, _y, SRC=src       
       if N_ELEMENTS(PARAM_1) eq 0 then Message, 'PARAM_1 (major semi-axis) required'
       if N_ELEMENTS(PARAM_2) eq 0 then Message, 'PARAM_2 (minor semi-axis) required'
       if N_ELEMENTS(PARAM_3) eq 0 then param_3 = 0.       
       npts = 36 * 3
       points = (2 * !PI / (npts-1)) * findgen(npts)
       xx = _x[0] + cos(points) * cos(param_3 * !DTOR) * param_1 - sin(points) * sin(param_3 * !DTOR) * param_2
       yy = _y[0] + cos(points) * sin(param_3 * !DTOR) * param_1 + sin(points) * cos(param_3 * !DTOR) * param_2
       _src = self.grid
    end
    'RECTANGLE': begin
       xx = [x[0], x[1], x[1], x[0], x[0]]
       yy = [y[0], y[0], y[1], y[1], y[0]]
       IF N_ELEMENTS(src) ne 0 then _src=src
    end
    else: Message, 'FORM: ' + str_equiv(FORM) + ' not recognized.'
  endcase
   
  return, self->set_polygon(xx, yy, SRC=_src, COLOR=color, THICK=thick, STYLE=style)
  
end


;+
; :Description:
;    Set a point or an array of points to draw on the map.
;    
;  :Params:
;    x: in, required
;       the x coordinates of the point(s) to draw
;    y: in, required
;       the y coordinates of the point(s) to draw 
;       
; :Keywords:
;    SRC: in, optional
;         the coordinate system (datum or proj) of the coordinates. Default is WGS-84
;    COLOR: in, optional, type = string
;           the color of the points
;    PSYM:in, optional, type = int, default=16
;          the style of the points (see symcat for plenty of possibilities)
;    SYMSIZE:in, optional, type = float
;            the size of the points
;    SYMTHICK: in, optional, type = float
;            the thickness of the points' outline
;    TEXT:in, optional, type = float
;          points annotation
;    DELTA_TEXT:in, optional, type = float
;               a delta in relative img coordinates where to put the annotation (2 elements vector)
;    ALIGN:in, optional, type = float
;          the alignment of the annotation
;    CHARSIZE:in, optional, type = float
;           the annotation size
;    
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_point, x, y, SRC=src, COLOR=color, PSYM=psym, SYMSIZE=symsize, SYMTHICK=symthick, $
                                  TEXT=text, DELTA_TEXT=delta_text, ALIGN=align, CHARSIZE=charsize
                                  
  ; Set up environnement
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
  
  if N_ELEMENTS(TEXT) ne 0 then begin
    if ~ arg_okay(TEXT, TYPE=IDL_STRING) then Message, WAVE_Std_Message('TEXT', /ARG)
    if N_ELEMENTS(TEXT) eq 1 then _TEXT = REPLICATE(TEXT, n_coord) $
     else if N_ELEMENTS(TEXT) eq n_coord then  _TEXT = text $
      else  Message, WAVE_Std_Message('TEXT', /ARG)    
  endif else _TEXT = REPLICATE('', n_coord)
  
  _color = 'black'
  _psym = 16
  _symsize = 1.
  _symthick = 1.
  _align = 0.
  _dpText = [0.005,0.005]
  _CHARSIZE = 1.
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(PSYM) eq 1 then _psym = PSYM
  if N_ELEMENTS(SYMSIZE) eq 1 then _symsize = SYMSIZE
  if N_ELEMENTS(SYMTHICK) eq 1 then _symthick = symthick
  if N_ELEMENTS(DELTA_TEXT) eq 2 then _dpText = DELTA_TEXT
  if N_ELEMENTS(ALIGN) eq 1 then _align = ALIGN
  if N_ELEMENTS(CHARSIZE) eq 1 then _charsize = CHARSIZE
  
  point = REPLICATE({w_Map_POINT}, n_coord)  
  for i = 0, n_coord -1 do begin
    point[i].psym = _psym
    point[i].symsize = _symsize
    point[i].color = cgColor(_color, /DECOMPOSED)
    point[i].text = _text[i]
    point[i].align = _align
    point[i].dpText = _dpText
    point[i].coord = [_x[i],_y[i]]
    point[i].charsize = _charsize
    point[i].thick = _symthick
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
;    Set a text or an array of texts to draw on the map.
;    
;  :Params:
;    x: in, required
;       the x coordinates of the text(s) to draw
;    y: in, required
;       the y coordinates of the text(s) to draw 
;    text: in, required
;          the text(s) to draw 
;       
; :Keywords:
;    SRC: in, optional
;         the coordinate system (datum or proj) of the coordinates. Default is WGS-84
;    COLOR: in, optional, type = string
;           the color of the points
;    ALIGN:in, optional, type = float
;          the alignment of the annotation
;    CHARSIZE:in, optional, type = float
;           the annotation size
;    
; :History:
;     Written by FaM, 2013.
;-    
function w_Map::set_text, x, y, text, SRC=src, COLOR=color, $
                                 ALIGN=align, CHARSIZE=charsize

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self->_DestroyTexts
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF 

  ;******************
  ; Check arguments *
  ;******************
  if N_PARAMS() ne 3 then begin
   self->_DestroyTexts
   return, 1
  endif
    
  if N_ELEMENTS(src) eq 0 then GIS_make_datum, ret, src, NAME = 'WGS-84'

  if not array_processing(x, y, REP_A0=_x, REP_A1=_y) then Message, WAVE_Std_Message(/ARG)
  n_coord = N_ELEMENTS(_x)
  self.grid->transform, _x, _y, _x, _y, SRC = src
  coord = [1#_x,1#_y]  + 0.5 ; Because Center point of the pixel is not the true coord 
  
  if ~ arg_okay(TEXT, TYPE=IDL_STRING) then Message, WAVE_Std_Message('TEXT', /ARG)
  if N_ELEMENTS(TEXT) eq 1 then _TEXT = REPLICATE(TEXT, n_coord) $
     else if N_ELEMENTS(TEXT) eq n_coord then  _TEXT = text $
      else  Message, WAVE_Std_Message('TEXT', /ARG)    
  
  _color = 'black'
  _align = 0.
  _charsize = 1.
  if N_ELEMENTS(COLOR) eq 1 then _color = COLOR
  if N_ELEMENTS(ALIGN) eq 1 then _align = ALIGN
  if KEYWORD_SET(CHARSIZE) then _charsize = CHARSIZE
  
  point = REPLICATE({w_Map_TEXT}, n_coord)  
  for i = 0, n_coord -1 do begin
    point[i].color = cgColor(_color, /DECOMPOSED)
    point[i].text = _text[i]
    point[i].align = _align
    point[i].coord = [_x[i],_y[i]]
    point[i].charsize = _charsize
  endfor

  if self.ntexts eq 0 then begin
   self.ntexts = n_coord
   self.texts = PTR_NEW(point, /NO_COPY)
  endif else begin
   temp = *self.texts
   npoints = self.ntexts
   self->_DestroyTexts
   temp = [temp, point]
   self.texts = PTR_NEW(temp, /NO_COPY)
   self.ntexts = npoints + n_coord
  endelse
     
  self.is_Texted = TRUE
  return, 1
  
end

;+
; :Description:
;    Wrapper to the set_point() method, it draws a filled symbol with a 
;    surrounding one
;
;  :Params:
;    x: in, required
;       the x coordinates of the point(s) to draw
;    y: in, required
;       the y coordinates of the point(s) to draw 
;       
; :Keywords:
;    
;    SRC: in, optional
;         the coordinate system (datum or proj) of the coordinates. Default is WGS-84
;    COLOR: in, optional, type = string
;           the color of the points
;    PSYM:in, optional, type = int, default=16
;          the style of the the points (see symcat for plenty of possibilities)
;    CIRCLE_COLOR: in, optional, type=string, DEFAULT='black'
;           the color of the surrounding circle
;    CIRCLE_THICK:in, optional, type = float
;           the thickness of the surrounding circle
;    PSYM:in, optional, type = int, default=16
;          the style of the the points (see symcat for plenty of possibilities)
;    SYMSIZE:in, optional, type = float
;            the size of the the points
;    TEXT:in, optional, type = float
;          points annotation
;    DELTA_TEXT:in, optional, type = float
;               a delta in relative img coordinates where to put the annotation (2 elements vector)  
;    ALIGN:in, optional, type = float
;          the allignment of the annotation
;
;-
function w_Map::set_filled_point, x, y, SRC=src, COLOR=color, PSYM=psym, SYMSIZE=symsize, CIRCLE_COLOR=circle_color, CIRCLE_THICK=circle_thick, $
                                  TEXT=text, DELTA_TEXT=delta_text, ALIGN=align, CHARSIZE=charsize

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  if N_ELEMENTS(symsize) eq 1 then _symsize = symsize else _symsize = 1.
  if N_ELEMENTS(circle_color) eq 1 then _circle_color = circle_color else _circle_color = 'black'
  if N_ELEMENTS(circle_thick) eq 1 then _circle_thick = circle_thick else _circle_thick = 0.15
  
  ok1 = self->set_point(x, y, SRC=src, COLOR=_circle_color, PSYM=psym, SYMSIZE=_symsize+_circle_thick)
  ok2 = self->set_point(x, y, SRC=src, COLOR=color, PSYM=psym, SYMSIZE=_symsize, $
                                  TEXT=text, DELTA_TEXT=delta_text, ALIGN=align, CHARSIZE=charsize)
  
  return, ok1 and ok2                               
                                                           
end

;+
; :Description:
;   If called without arguments, it generates an info 
;   structure (called internally, you don't have
;   to worry about it).
;   
;   You can give a true color as argument to override this behavior and 
;   set by yourself a true color img (3,nx,ny or nx,ny,3) to put on the map. If you do
;   so, the data and levels will be ignored on the plot.
;   
; :Params:
;   img: in, optional
;        sets a true color image on the map. If not set, the true color
;        image is generated based on the map data, datalevels and colors.
; 
; :Keywords:
;    BLUE_MARBLE: in, optional, type = boolean/string
;                 set this keyword to make a map using the NASA Land Cover picture (low res, default)
;                 if set to a string, it is the path to an alternative jpg file to use as background
;    HR_BLUE_MARBLE: in, optional, type = boolean/string
;                    set this keyword to make a map using the NASA Land Cover picture (High res)
;   INTERPOLATE: in, optional, type=boolean, default=0
;         Is set, bilinear interpolation is used to resize the image. Otherwise,
;         nearest neighbor sampling is used instead.
; 
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_img, img, BLUE_MARBLE=blue_marble, HR_BLUE_MARBLE=hr_blue_marble, INTERPOLATE=interpolate, D_RGB=d_rgb
  
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
  
  SetDefaultValue, D_RGB, 0
  
  if KEYWORD_SET(HR_BLUE_MARBLE) then begin
    w = OBJ_NEW('w_BlueMarble', /HR)
    ok = self->set_img(0 > Transpose(self.grid->map_gridded_data(Transpose(w->get_img(), [1,2,0]), w, /BILINEAR), [2,0,1]) + D_RGB < 255)
    undefine, w
  endif
  
  if KEYWORD_SET(BLUE_MARBLE) then begin    
    if arg_okay(BLUE_MARBLE, TYPE=IDL_STRING) then w = OBJ_NEW('w_BlueMarble', FILE=blue_marble) else w = OBJ_NEW('w_BlueMarble')
    ok = self->set_img(0 > Transpose(self.grid->map_gridded_data(Transpose(w->get_img(), [1,2,0]), w, /BILINEAR), [2,0,1]) + D_RGB < 255)
    undefine, w
  endif
  
  if N_ELEMENTS(img) ne 0 then begin
    _img = img
    s = SIZE(_img, /DIMENSIONS)
    if s[0] ne 3 then begin
      if s[2] ne 3 then Message, '$IMG does not seem to be a TRUECOLOR image. Check it (dims = [3,nx,ny]).'
      _img = Transpose(_img, [2,0,1])
      s = SIZE(_img, /DIMENSIONS)
    endif
    PTR_FREE, self.img
    PTR_FREE, self.info
    if s[1] ne self.Xsize or s[2] ne self.Ysize then $
     self.img = PTR_NEW(cgResizeImage(img, self.Xsize, self.Ysize, INTERPOLATE=interpolate)) $
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
  if PTR_VALID(self.missing) then missing = *self.missing
  if PTR_VALID(self.plot_params.dcbar_colors) then DC_COLORS = *self.plot_params.dcbar_colors
    
  if PTR_VALID(self.plot_params.oob_top_color) then oob_top_color = *self.plot_params.oob_top_color
  if PTR_VALID(self.plot_params.oob_bot_color) then oob_bot_color = *self.plot_params.oob_bot_color
  if PTR_VALID(self.plot_params.oob_top_arrow) then oob_top_arrow = *self.plot_params.oob_top_arrow
  if PTR_VALID(self.plot_params.oob_bot_arrow) then oob_bot_arrow = *self.plot_params.oob_bot_arrow
  if PTR_VALID(self.plot_params.sigma) then sigma = *self.plot_params.sigma
  
  info = w_gr_DataLevels(*self.data, $
    LEVELS=levels, $
    N_LEVELS=self.plot_params.nlevels, $
    NEUTRAL_COLOR=self.plot_params.neutral, $
    MISSING=missing, $
    COLORS=dc_colors, $
    MIN_VALUE=min_value, $
    MAX_VALUE=max_value, $
    SIGMA=sigma, $
    HIST_EQUAL=self.plot_params.hist_equal, $
    CMIN=self.plot_params.cmin, $
    CMAX=self.plot_params.cmax, $
    OOB_TOP_COLOR=oob_top_color, $
    OOB_BOT_COLOR=oob_bot_color, $
    OOB_TOP_ARROW=oob_top_arrow, $
    OOB_BOT_ARROW=oob_bot_arrow, $
    DCBAR=self.plot_params.dcbar)
    
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
;    CUBIC: in, optional, type = boolean
;            set this if you want the data to be cubic interpolated
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
  CUBIC=cubic, $
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
   data = FLTARR(self.Xsize, self.Ysize) * !VALUES.F_NAN
   PTR_FREE, self.data
   self.data = PTR_NEW(data, /NO_COPY)  
   PTR_FREE, self.missing
   self.missing = PTR_NEW()
   return, self->set_img()
  endif  
    
  if ~ arg_okay(data, N_DIM=2, /NUMERIC) then Message, WAVE_Std_Message('data', NDIMS=2)
     
  if N_ELEMENTS(grid) eq 0 then begin
    if arg_okay(data, DIM=[self.Xsize, self.Ysize], /NUMERIC) then _data = data $
    else _data = CONGRID(data, self.Xsize, self.Ysize, /CENTER, INTERP=bilinear, CUBIC=cubic)
  endif else begin
    if N_ELEMENTS(missing) ne 0 then _missing = missing
    if KEYWORD_SET(OVERPLOT) then _data = self.grid->map_gridded_data(data, grid, MISSING=_missing, BILINEAR=bilinear, CUBIC=cubic, DATA_DST=*self.data) $
     else _data = self.grid->map_gridded_data(data, grid, MISSING=_missing, BILINEAR=bilinear, CUBIC=cubic)
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
;    LINEAR: in, optional
;            if linear interpolation has to be used
;    NOBOUNDS: in, optional
;              standard beavior is to replace bounds by MISSING data do avoid
;              stupid extrapolating. Set this keyword to avoid this 
;             
; :History:
;     Written by FaM, 2011.
;-    
function w_Map::set_ll_data, data, lon, lat, $
  SRC=src, $
  MISSING=missing, $
  LINEAR=linear, $
  NOBOUNDS=nobounds
  
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
 
  _data = self.grid->map_lonlat_data(data, lon, lat, SRC=src, MISSING=missing, LINEAR=linear, NOBOUNDS=nobounds)
  
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
  
  _mask = BYTE(0 > CEIL(_mask) < 1)
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
   if tag_exist(extra, 'CENTER') then utils_remove_tag, extra, 'CENTER'
   if tag_exist(extra, 'WIN_FACTOR') then utils_remove_tag, extra, 'WIN_FACTOR'
   if ~tag_exist(extra, 'MAX_RADIUS') then extra = CREATE_STRUCT(extra, 'MAX_RADIUS', 0.1)
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
;    DENSITY: in, optional, type=long, default=3
;             the vectors density, in grid points
;    START_IND: in, optional, type=long, default=0
;               the first grid point to draw in the corner
;    COLOR: in, optional
;            the vector colors. Can be either a scalar, or
;            a vector (nmeric or string) the same size as posx
;    STDVEL: in, optional, default=max velocity
;            the velocity (in velocity units) associated to
;            the standard length (see) the length keyword.
;            set this to be sure to have always the same length
;            between different plots in the same window (or 
;            different windows but with the same X size!)
;    LENGTH: in, optional, default=0.08
;            the length of a vector of STDVEL velocity,
;            in X-normal coordinates 
;    THICK: in, optional
;           thickness of the vectors
;    NOCLIP: in, optional
;            set to 0 to clip the vectors drawing
;
;-
function w_Map::set_wind, ud, vd, grid, $
  DENSITY=density , $
  LENGTH=length, $
  NOCLIP=noclip, $
  STDVEL=stdvel, $
  THICK=thick, $
  COLOR=color,  $
  START_IND=start_ind
                             
  
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
  if N_ELEMENTS(start_ind) eq 0 then _start_ind = 0 else _start_ind = FIX(start_ind)
  if N_ELEMENTS(noclip) eq 0 then _noclip = 0 else _noclip = noclip
  type = 'VECTORS'
  
  if N_PARAMS() eq 0 then begin
   self->_DestroyWindParams
   return, 1
  endif  
  
  if N_PARAMS() ne 3 then Message, WAVE_Std_Message(/NARG)
  
  if not OBJ_ISA(grid, 'w_Grid2D')  then Message, WAVE_Std_Message('src_grid', OBJ='w_Grid2D')
  if not array_processing(ud, vd) then Message, WAVE_Std_Message(/ARG)  
            
  grid->getProperty, tnt_c=c   
  
  xi = findgen(floor(float(C.nx-_start_ind)/_density)) * _density + _start_ind
  yi = findgen(floor(float(C.ny-_start_ind)/_density)) * _density + _start_ind
  x = xi * c.dx + c.x0
  y = yi * c.dy + c.y1
  utils_1d_to_2d, x, y, x, y  
  self.grid->transform_XY, x, y, c.proj, posX, posY
  
  utils_1d_to_2d, xi, yi, xi, yi
  velx = ud[xi,yi]
  vely = vd[xi,yi] 
  
  if N_ELEMENTS(stdvel) eq 0 then _stdvel = max(sqrt(velx^2+vely^2), /NAN) else _stdvel = stdvel
  
  posX += 0.5
  posy += 0.5  
  pok = where(posX ge 0 and posX le self.Xsize and posY ge 0 and posY le self.Ysize and FINITE(velx) and FINITE(vely), cnt)
  if cnt eq 0 then Message, 'Wind grid not compatible.'
  
  self->_DestroyWindParams
  self.wind_params.type = type
  self.wind_params.length = _length
  self.wind_params.thick = _thick
  self.wind_params.stdvel = _stdvel
  self.wind_params.noclip = _noclip
  self.wind_params.color = PTR_NEW(cgColor(_color, /DECOMPOSED))
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
; :Keywords:
;    POSITION: in
;              the position of the image
;    TITLE: in
;           the title of the plot
;    LABEL_TYPE: in
;                the title type:: 
;                  - 'DEFAULT' is centered on the top
;                  - 'LEFT' is ajusted left on the top
;                  - 'IN_LL' is in the plot, lower left corner
;                  - 'IN_LR' is in the plot, lower right corner
;                  - 'IN_UR' is in the plot, upper right corner
;                  - 'IN_UL' is in the plot, upper left corner
;    CHARSIZE: in
;              title charsize
;    CHARTHICK: in
;               title charthick
;    WINDOW: in
;            if th draw in a cgWindow
;    MULTIMARGIN: in
;                 see cgImage
;    MARGIN: in
;            see cgImage
;    NOERASE: in
;             see cgImage
;
;-
pro w_Map::add_img, $
    POSITION=position, $
    TITLE=title, $
    LABEL_TYPE=label_type, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    WINDOW=window, $
    MULTIMARGIN=multimargin, $
    MARGIN=margin, $
    NOERASE=noerase
    
  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if KEYWORD_SET(WINDOW) then begin
    p = cgQuery(COUNT=cnt, /CURRENT) 
    if cnt eq 0 then cgWindow ; just to be sure we have one to draw on
  endif
  
  ; Std image
  if PTR_VALID(self.img) or PTR_VALID(self.info) then ok = self->_draw_image(POSITION=position, $
    WINDOW=window, $
    MULTIMARGIN=multimargin, $
    MARGIN=margin, $
    NOERASE=noerase) else message, 'No image set yet...'  
  
  
  if self.is_Contoured then ok = self->_draw_contours(WINDOW=window)
  if self.is_Shaped then ok = self->_draw_shapes(WINDOW=window)
  if self.is_Mapped then ok = self->_draw_Map(WINDOW=window)
  
  ; Draw a frame
  xf = [0, self.xsize, self.xsize, 0, 0]
  yf = [0, 0, self.ysize, self.ysize, 0]
  cgPlotS, xf, yf, /DATA, WINDOW=window

  if self.is_Winded then ok = self->_draw_wind(WINDOW=window)
  if self.is_Polygoned then ok = self->_draw_polygons(WINDOW=window)
  if self.is_Pointed then ok = self->_draw_points(WINDOW=window)
  if self.is_Texted then ok = self->_draw_texts(WINDOW=window)
  if self.is_WindRosed then ok = self->_draw_windRoses(WINDOW=window)
  if N_ELEMENTS(TITLE) ne 0 then ok = self->_add_label(position, $
    TITLE=title, $
    LABEL_TYPE=label_type, $
    CHARSIZE=charsize, $
    CHARTHICK=charthick, $
    WINDOW=window)
    
end

;+
; :Description:
;    Gets the image produced by w_Map as RGB triplet. No map, 
;    no countours, no country borders, no shapes, no bling bling.
;    
;    This is usefull if you want to play around and make transparent stuffs
;    for example.
;
;-
function w_Map::get_img

   cgDisplay, self.Xsize, self.Ysize, /FREE, /PIXMAP
   xwin = !D.WINDOW
   
   ; Std image
  if PTR_VALID(self.img) or PTR_VALID(self.info) then ok = self->_draw_image() else message, 'No image set yet...'
  disp_img = tvrd(TRUE=3)
  wdelete, xwin
  return, disp_img

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
;    To draw a wind legend on an existing plot. 
;
;-   
pro w_Map::add_wind_legend, UNITS=units, $
    WINDOW=window, $
    POSITION=position, $
    DECIMALS=decimals, $
    TITLEPOSITION=titleposition, $
    CHARSIZE=charsize

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  if ~ self.is_Winded then return ; do nothing
  
  SetDefaultValue, position, [0.8,0.05]
  SetDefaultValue, titleposition, position - [0., 0.03]
  SetDefaultValue, units, 'm s!U-1!N'
  SetDefaultValue, charsize, 1.
  
  w_velvec, position[0], position[1], self.wind_params.stdvel, 0., $
    COLORS=(*self.wind_params.color)[0], $
    LENGTH=self.wind_params.length, $
    THICK=self.wind_params.thick, $
    STDVEL=self.wind_params.stdvel, $
    /NORMAL, WINDOW=window
 
  strnum =  w_str(self.wind_params.stdvel, decimals)
  cgText, titleposition[0], titleposition[1], strnum + ' ' + units, CHARSIZE=charsize, /NORMAL, WINDOW=window
  
end

;+
; :Description:
;    Simple function to have a look at the plot.
;
;-   
pro w_Map::show_img, WINDOW=window, TITLE=title, MARGIN=margin

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
      
  SetDefaultValue, title, 'Map Plot'
  SetDefaultValue, margin, 0.07
  SetDefaultValue, window, 1
  
  xs = self.Xsize * (1.+2.*margin)
  ys = self.Ysize * (1.+2.*margin)
  
  if KEYWORD_SET(window) then begin
    cgWindow, WXSIZE=xs, WYSIZE=ys, WTitle=title
    cgControl, EXECUTE=0
  endif else begin
    cgDisplay, Xs, Ys, /FREE
  endelse
  
  self->add_img, MARGIN=1, WINDOW=window
  cgControl, EXECUTE=1
  
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
pro w_Map::show_color_bar, WINDOW=window, VERTICAL=vertical, _REF_EXTRA=extra

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  
  title = 'Color bar'
  SetDefaultValue, window, 1
  
  if KEYWORD_SET(VERTICAL) then begin
    xs = self.Ysize * 0.2
    ys = self.Ysize * 0.75  
    _Position=[0.20,0.05,0.30,0.95]
    _VERTICAL = 1
    oob_fac = FLOAT(xs)/ys
  endif else begin
    xs = self.Xsize * 0.75
    ys = self.Xsize * 0.15  
    _Position=[0.10,0.4,0.90,0.6]
    _VERTICAL = 0
    oob_fac = FLOAT(ys)/xs
  endelse
  
  if KEYWORD_SET(WINDOW) then begin
    cgWindow, WXSIZE=xs, WYSIZE=ys, WTitle=title
    cgControl, EXECUTE=0
  endif else begin
    cgDisplay, /FREE, XSIZE=xs, YSIZE=ys, Title=title
  endelse

  self->add_color_bar, POSITION=_Position, WINDOW=window, OOB_FACTOR=oob_fac, VERTICAL=_VERTICAL, _EXTRA=extra
  cgControl, EXECUTE=1
    
end

;+
; :Description:
;    Get access to some params. 
;
; :History:
;     Written by FaM, 2011.
;-    
PRO w_Map::GetProperty, XSIZE=xsize, YSIZE=ysize, TNT_C=tnt_c, GRID=grid, $
                        ASPECT=aspect, BARINFO=barinfo
    
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
  if ARG_PRESENT(TNT_C) then self.grid->getProperty, TNT_C=tnt_c
  if ARG_PRESENT(GRID) then grid = self.grid
  if ARG_PRESENT(ASPECT) then aspect = self.Ysize / float(self.Xsize)
  if ARG_PRESENT(BARINFO) then barinfo = *self.info
  
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
;
; :History:
;     Written by FaM, 2011.
;-    
Function w_Map::Init, grid, Xsize = Xsize,  Ysize = Ysize, FACTOR = factor, NO_COUNTRIES=no_countries
                            
     
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
  if N_ELEMENTS(Xsize) eq 0 and N_ELEMENTS(Ysize) eq 0 and N_ELEMENTS(FACTOR) eq 0 then Ysize = 400
  
  self.grid = grid->reGrid(XSIZE=xsize, YSIZE=ysize, FACTOR=factor) 
  self.grid->getProperty, tnt_C = c
  self.Xsize = c.nx
  self.Ysize = c.ny
  
  ; Defaults
  self.data = PTR_NEW(FLTARR(self.Xsize, self.Ysize) * !VALUES.F_NAN, /NO_COPY)  
  TVLCT, rr, gg, bb, /GET
  cgLoadCT, 0, /REVERSE
  dummy = self->set_plot_params(N_LEVELS=10)    
  TVLCT, rr, gg, bb
  dummy = self->set_data()
  dummy = self->set_map_params()  
  dummy = self->set_shading_params()
  dummy = self->set_wind()  
  
  if ~KEYWORD_SET(NO_COUNTRIES) then dummy = self->set_shape_file(/COUNTRIES)  
                 
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
            oob_top_color  : PTR_NEW()     , $ ; if a top OOB color
            oob_bot_color  : PTR_NEW()     , $ ; if a bot OOB color
            oob_top_arrow  : PTR_NEW()     , $ ; if a top OOB arrow
            oob_bot_arrow  : PTR_NEW()     , $ ; if a bot OOB arrow
            hist_equal     : 0B            , $ ; if hist equal levels
            sigma          : PTR_NEW()     , $ ; if sigma auto crop
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
            psym           : 0L            , $ ; style of the point
            symsize        : 0D            , $ ; symsize of the point
            color          : 0L            , $ ; color of the point
            text           : ''            , $ ; point annotation
            charsize       : 0D            , $ ; point annotation size
            align          : 0D            , $ ; annotation alignement
            dpText         : [0D,0D]       , $ ; delta pos of the text with respect to the point
            coord          : [0D,0D]       , $ ; coordinates of the point
            thick          : 0.}
  
  ; This is the information for one text to draw
  struct = {w_Map_TEXT                     , $ 
            text           : ''            , $ ; annotation
            color          : 0L            , $ ; color of the text
            charsize       : 0D            , $ ; annotation size
            align          : 0D            , $ ; annotation alignement
            coord          : [0D,0D]         $ ; coordinates of the text       
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
            xtick_start    : 0L            , $ ; tick marks start
            ytick_start    : 0L            , $ ; tick marks start
            xtick_dx       : 0D            , $ ; tick marks offset factor
            xtick_dy       : 0D            , $ ; tick marks offset factor            
            ytick_dx       : 0D            , $ ; tick marks offset factor           
            ytick_dy       : 0D            , $ ; tick marks offset factor            
            t_Charsize     : 0D            , $ ; Ticks charsizes
            interval       : 0D            , $ ; The interval between levels
            tick_interval  : 0L            , $ ; The interval between tick annotations
            tick_format    : ''            , $ ; The tick annotations format
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
            color          : PTR_new()     , $ ; color of the arrows
            noclip         : 0L            , $ ; clip or not clip?
            stdvel         : 0D            , $ ; standard velocity associated to length 
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
             ntexts        : 0L                     , $ ; number of texts to plot                  
             texts         : PTR_NEW()              , $ ; array of ntexts {w_Map_TEXT} structures                               
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
             is_Masked     : FALSE                  , $ ; did the user specify masks to overplot?      
             is_Texted     : FALSE                    $ ; did the user specify texts to draw?      
             }
    
END