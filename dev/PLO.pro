;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  PLO.pro                                               *
; Version     :  WAVE 0.0                                              *
; Language    :  IDL 7.0 and higher                                    *
; Date        :  04-Jan-2009                                           *
; Last Update :  04-Jan-2009 FaM                                       *
;                                                                      *
; IDL program file for WAVE main program.                              *
;                                                                      *
;***********************************************************************

;-----------------------------------------------------------------------
;+
; NAME:
;       GENERAL INFORMATION
;
;       The PLO library is a bunch of procedures to PLOT and help you to PLOT
;       WRF output data
;-
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;+
; NAME:
;       PLO_init
;
; PURPOSE:
;       PLO_init is called internally at each plot. You don't have to call this pro-
;       cedure. However, you can modify each field to suit your needs.
;
; CATEGORY:
;       WRF
;
; CALLING SEQUENCE:
;       
;
;
; MODIFICATION HISTORY:
;       Written by: Fabien Maussion 2009
;       Modified:   06-Jul-2009 FaM
;                   First occurence 
;-
;-----------------------------------------------------------------------
pro PLO_init, ret, XonY=XonY, HORIZONTAL_BAR = horizontal_bar, NO_BAR = no_bar

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc

  
  ; Admin constants for X window device drawing
  common PLO_ADMIN_X, W_size_fac, Window_pos, MargX, MargY, ImgX, ImgY, BarY, BarX, StartX, StartY, Marg_After_Bar , devCenterX, devCenterY, imgG
  
  ; Admin constants for plots, sizes and colors
  common PLO_ADMIN_G, G_CHARSIZE, G_CHARTHICK, BORDER_THICK, FRAME_THICK, G_PointSize, WIND_DENSITY, $
                        TOPO_FILE_d1, TOPO_FILE_d3, SHAPE_FILE_world, SHAPE_FILE_local, SHAPE_FILE_other
  
  ; Admin keywords for plotting option
  common PLO_ADMIN_O, SHADING, BORDERS, LATLON_NOTES

  ;---------------------------------------
  ; Constants for X window device drawing
  ;--------------------------------------- 
  WAVE_root, root
  
  W_size_fac = 1  ; General window size factor. (Default : 1)
  Window_pos = 1640
  if ~KEYWORD_SET(XonY) then XYfac = 1 else XYfac = XonY ; X on Y ratio, defining the width of the window. Should not be set here but depends on data. 
      
  MargX = 50  * W_size_fac ; The Margin on the left and right side of the plot. (Default : 50)
  MargY = 80 * W_size_fac ; The Margin on the upper and bottom side of the plot. (Default : 80)
  BarY= 150 * W_size_fac   ; Margin left for adding an horizontal color bar. (Default : 150) 
  BarX= 100 * W_size_fac    ; Margin left for adding a vertical color bar. (Default : 100)
  Marg_After_Bar = 200  * W_size_fac ; Margin left if bar tags too long (Default : 50)
 
  ; Position of the down-left corner of the plot in the device window. Should not be modified manually
  if KEYWORD_SET(HORIZONTAL_BAR) then begin
    StartX = MargX 
    StartY = MargY + BarY + 1
  endif else begin
    StartX = MargX 
    StartY = MargY 
  endelse 
  
  ImgY = 700 * W_size_fac            ; Plot width in pixels. (Default : 700) 
  ImgX = ImgY * XYfac                ; Plot height in pixels (do not change)
  
  devCenterX = StartX + DOUBLE(ImgX) / 2 ; Center of the device. Should not be modified 
  devCenterY = StartY + DOUBLE(ImgY) / 2 ; Center of the device. Should not be modified 
  
  ;----------------------------------------------
  ; ; Admin constants for plots, sizes and colors
  ;----------------------------------------------
  
  G_PointSize = 5 * W_size_fac ; Size in pixels of drawn points on the map 
  G_CHARSIZE = 1.6 * W_size_fac  ; Character size. (Default 1.6)  
  G_CHARTHICK= 1.2 * W_size_fac  ; Character thickness. (Default 1.2) 
  BORDER_THICK = 2 * W_size_fac  ; Line thickness when drawing shapes. (Default 2) 
  FRAME_THICK = 2  * W_size_fac  ; Line thickness when drawing Plot frame. (Default 2) 
        
  TOPO_FILE_d1 = ''  ; Id of the default TOPO file for shading (see #GEO_READ_TOPO#) 
  TOPO_FILE_d3 = 'NaM'  ; If setted, it will be used as TOPO file for the smaller domain (SRTM instead of GTOPO30, for example)  
  SHAPE_FILE_world = ''  ; Where to find the default shape.sav file for drawing borders 
  SHAPE_FILE_local = root + '/res/shapes/namco.sav'  ; If setted, it will be used as shape.sav file for drawing borders for the smaller domain
  SHAPE_FILE_other = ''  ; If setted, it will be used as second shape.sav file
    
  ;---------------
  ; PLOT KEYWORDS
  ;---------------   
  SHADING = FALSE ; Enable shading 
  BORDERS = TRUE ; Enable shapes drawing 
  LATLON_NOTES = TRUE ; Enable lat lon mapping numbering
  
  !order = 0
  
  ;********
  ; Ready *
  ;********
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       PLO_time_str_for_plot
;
; PURPOSE:
;       PRIVATE: this function just turns a DATE into string for the plot title
;
; CATEGORY:
;       PLO
;
; MODIFICATION HISTORY:
;       Written by: FM, 2009
;-  
;-----------------------------------------------------------------------
function PLO_time_str_for_plot, date, HOUR = hour, MONTH = month
 
 @WAVE.inc
 
 str = STRING(date.day,FORMAT = "(I02)") + '.' + STRING(date.month,FORMAT = "(I02)") + '.' + STRING(date.year,FORMAT = "(I04)")
 if KEYWORD_SET(HOUR) then begin
  str = str + '_' + STRING(date.hour,FORMAT = "(I02)") + 'H'
  str = str + STRING(date.minute,FORMAT = "(I02)")   
 endif 
 if KEYWORD_SET(MONTH) then str = STRING(date.month,FORMAT = "(I02)") + '.' + STRING(date.year,FORMAT = "(I04)")
 
  
 return, str
 
end

;-----------------------------------------------------------------------
;+
; NAME:
;       PLO_set_X_device
;
; PURPOSE:
;       This function defines a new device ready to plot
;
; CATEGORY:
;       PLO
;
; CALLING SEQUENCE:
;       PLO_set_X_device, pos_graph, pos_bar [, /PIXMAP, XonY = XonY, WINDOW = window, /VERTI_BAR, /NO_BAR]
;
; KEYWORDS:
;       PIXMAP       : if the window has to be hidden before display (most of the cases)
;       XonY         : X on Y ratio from WRF output
;       WINDOW       : Window ID (default : 1)
;       HORIZONTAL_BAR : if the color bar has to be horizontal (default : vertical)
;       NO_BAR : if no bar has to be plotted (default : a bar is plotted)
;       
; OUTPUT:
;       pos_graph : where to put the plot in the device window
;       pos_bar : where to put the color bar in the device window
; 
;-
; MODIFICATION HISTORY:
;       Written by: FM, 2009
;-  
;-----------------------------------------------------------------------
pro PLO_set_X_device, pos_graph, pos_bar, PIXMAP = pixmap, XonY = XonY, WINDOW = window, HORIZONTAL_BAR = horizontal_bar, NO_BAR = no_bar

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  
  ;--------------------------
  ; Check input
  ;--------------------------  
  if ~KEYWORD_SET(WINDOW) then WIN = 1 else WIN = window
  
  PLO_init, ret, XonY = XonY, HORIZONTAL_BAR = horizontal_bar, NO_BAR = no_bar
  
  ; Definition of window sizes, position of plots and color bars... Should be OK like this but can be modified  
  if KEYWORD_SET(HORIZONTAL_BAR) then begin
  
    XSIZE_solo = 2 * MargX + ImgX 
    YSIZE_solo = 2 * MargY + ImgY + BarY
    
    pos_graph = [StartX, StartY , StartX + ImgX, StartY + ImgY]
    
    widthBar = (ImgY - W_size_fac * 150.) / (2.*XSIZE_solo)
    
    pos_bar = [0.5-widthBar, 0.055, 0.5+widthBar, 0.080]
        
  endif else if KEYWORD_SET(NO_BAR) then begin
  
    XSIZE_solo = 2 * MargX + ImgX 
    YSIZE_solo = 2 * MargY + ImgY
    
    pos_graph = [StartX, StartY , StartX + ImgX, StartY + ImgY]
       
    pos_bar = [0, 0, 0, 0]
  
  endif else begin
  
    XSIZE_solo = 2 * MargX + ImgX + BarX + Marg_After_Bar
    YSIZE_solo = 2 * MargY + ImgY
    
    pos_graph = [StartX, StartY , StartX + ImgX, StartY + ImgY]
    
    heightBar = (ImgY - W_size_fac * 70) / (2.*YSIZE_solo)    
        
    widthBar = (BarX/8.0) / (XSIZE_solo)
    posBar = ((BarX+MargX+ 20.* W_size_fac)/2.+ StartX + ImgX) / DOUBLE(XSIZE_solo)
    
    pos_bar = [posBar - widthBar, 0.5-heightBar, posBar + widthBar, 0.5+heightBar]
        
  endelse

  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2  
  window, WIN, XSIZE=XSIZE_solo, YSIZE=YSIZE_solo, YPos=Window_pos, Title='WRF Plots', PIXMAP = pixmap
   
end

;-----------------------------------------------------------------------
;+
; NAME:
;       PLO_show_img
;
; PURPOSE:
;       This function takes the pixmaped windows in a row and put them together
;
; CATEGORY:
;       PLO
;
; CALLING SEQUENCE:
;       PLO_show_img [, WINDOW = window, N_WINDOWS = N_windows, /VERTICAL, /PIXMAP, PNG = png]
;
; KEYWORDS:
;       WINDOW       : Window ID (default : 1). If an array is given, the different windows will be put together
;       OUT_WINDOW   : Id of the output window (default : the same as the first id of the Window Keyword). 
;       PIXMAP       : if the window has to be hidden
;       VERTICAL     : if the plots should be aggregated Verticaly instead of horizontaly
;       PNG          : the path of the PNG file to store the image in it. If setted, the image will not be shown.
;       
; OUTPUT:
;       
;       a beautiful plot 
;-
; MODIFICATION HISTORY:
;       Written by: FM, 2009
;-  
;-----------------------------------------------------------------------
pro PLO_show_img, WINDOW = window, OUT_WINDOW = out_window, VERTICAL = vertical, PIXMAP = pixmap, PNG = png

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  
  ;--------------------------
  ; Check input
  ;--------------------------  
  if ~KEYWORD_SET(WINDOW) then win = 1 else win = window 
  
  N_win = N_ELEMENTS(win)
  
  if N_win gt 1 then begin
    for i = 0, N_win-1 do begin
        temp = win[i]
        WSET, temp
        tempI = Transpose(tvrd(/TRUE), [1,2,0])
        if KEYWORD_SET(VERTICAL) then begin
          if N_ELEMENTS(finalI) eq 0 then finalI = tempI else finalI = [[finalI],[tempI]]
        endif else begin 
          if N_ELEMENTS(finalI) eq 0 then finalI = tempI else finalI = [finalI,tempI]
        endelse
        WDELETE, temp
    endfor
  endif else begin  
    WSET, win
    finalI = Transpose(tvrd(/TRUE), [1,2,0])
    WDELETE, win
  end
  
  if KEYWORD_SET(OUT_WINDOW) then win = OUT_WINDOW else win = win[0]
  
  if KEYWORD_SET(PNG) then PIXMAP = TRUE
  Window, win, XSIZE=N_ELEMENTS(finalI[*,0,0]), YSIZE=N_ELEMENTS(finalI[0,*,0]), YPos=Window_pos, PIXMAP=pixmap
  tv, finalI, TRUE=3
  if KEYWORD_SET(PNG) then WRITE_PNG, png, tvrd(/TRUE)
  if ~KEYWORD_SET(pixmap) then pixmap = FALSE
  
  if (pixmap eq FALSE or KEYWORD_SET(PNG)) and OBJ_VALID(imgG) then OBJ_DESTROY, imgG
  
end

function GEO_READ_TOPO, lat, lon, topo_file

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  WAVE_root, root
  if N_ELEMENTS(TOPO_FILE) eq 0 then begin

    return, -1
  endif
  
  case (TOPO_FILE) of
    'TiP': begin
      file = root + 'res/topo/TiP.grd'
      
      lat0 = 90. ; deg N
      lon0 = 20. ; deg E

      nlat = 18001L ; rows
      nlon = 14401L  ; samples
      
      dlon = 30 ; res in arcsec
      dlat = 30 ; res in arcsec  
    end
    'NaM': begin
      file = root + 'res/topo/NaM.grd'
      
      lat0 = 115201.5D / 3600D ; deg N
      lon0 = 320398.5D / 3600D ; deg E

      nlat = 3601L ; rows
      nlon = 4801L ; samples

      dlon = 3 ; res in arcsec
      dlat = 3 ; res in arcsec  
      
    end
    'KiN': begin
      file = root + 'res/topo/KiN.grd'
      
      lat0 = 90. ; deg N
      lon0 = -180. ; deg E      
      nlat = 6001L ; rows
      nlon = 43201L  ; samples
      
       dlon = 30 ; res in arcsec
       dlat = 30 ; res in arcsec  
      
    end
    else: begin
   
    end
  endcase
  
  ilat = round(3600d*(lat0-lat[*])/dlat)
  ilon = round(3600d*(lon[*]-lon0)/dlon)

  rmin = min(ilat)
  rmax = max(ilat)

  topo = intarr(nlon,rmax-rmin+1)

  openr, lun, file, /GET
  point_lun, lun, 2*rmin*nlon  
  readu, lun, topo
  free_lun, lun

  z = topo[ilon,ilat-rmin]

  p = where(z eq -9999, cnt)
  if cnt gt 0 then z[p] = 0

  z = reform(z, n_elements(lat[*,0]), n_elements(lat[0,*]))

  return, z

end

pro GEO_get_Gradients, lon, lat, dhdx, dhdy, topo_file

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  z = GEO_read_topo(lat, lon, topo_file)  
  z = DOUBLE(z) ; Other wize, the TNT procedure doesn't work.
 
  GIS_xy_derivatives, ret,z, DFDX=DFDX,DFDY=DFDY
  
  dhdx = ROTATE(TRANSPOSE(DFDX),3)  
  dhdy = ROTATE(TRANSPOSE(DFDY),3) 
  
  dhdx = Scale_Vector(dhdx , 0.0,  1.0)
  dhdy = Scale_Vector(dhdy , 0.0,  1.0)
    
end

;-----------------------------------------------------------------------
;+
; NAME:
;       CAP_shade
;
; PURPOSE:
;       This generic procedure produces shaded map layers in CAP.
;
; CATEGORY:
;       Mapping
;
; CALLING SEQUENCE:
;       CAP_shade, ret, grd, dhdx, dhdy, PAL=pal, RELIEF=relief, /PSEUDO, TIFF=tiff, PNG=png, $
;                  IMG=img, OUT_PAL=out_pal
;
; INPUT:
;       grd:
;       dhdx:
;       dhdy:
;
; KEYWORDS:
;       PAL     (I):
;       RELIEF  (I):
;       /PSEUDO (I):
;       TIFF    (I):
;       PNG     (I):
;       IMG     (O):
;       OUT_PAL (O):
;
; OUTPUT:
;       ret: TNT return code
;
; MODIFICATION HISTORY:
;       Written by: D. Scherer 15-Jun-2009
;       Modified:   16-Jun-2009 DiS
;                   Routine implemented from prior version
;-
;-----------------------------------------------------------------------

pro GEO_shade, grd, dhdx, dhdy, PAL=pal, RELIEF=relief, PSEUDO=pseudo, TIFF=tiff, PNG=png, $
               IMG=img, OUT_PAL=out_pal

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2

  ;*****************
  ; Check keywords *
  ;*****************

  ; Check colour palette

  pal_dim = var_info(pal, /N_DIM)

  if pal_dim eq 0 then begin
    pal = bindgen(256)
    gs  = TRUE
    pc  = FALSE
  endif else if pal_dim eq 1 then begin
    if var_info(pal, /N_ELEM) ne 256 then begin
      message, WAVE_Std_Message(/ARG)
      return
    endif
    gs   = TRUE
    pc   = FALSE
  endif else if pal_dim eq 2 then begin
    if var_info(pal, /N_ELEM, DIM=1) ne 3 or var_info(pal, /N_ELEM, DIM=2) ne 256 then begin
      message, WAVE_Std_Message(/ARG)
      return
    endif
    rp = reform(pal[0,*], 256)
    gp = reform(pal[1,*], 256)
    bp = reform(pal[2,*], 256)
    gs = FALSE
    pc = keyword_set(pseudo)
  endif else begin
    message, WAVE_Std_Message(/ARG)
    return
  endelse

  ; Strength of relief shading specified?

  if ~ keyword_set(relief) then rel_factor = 1.0 else rel_factor = (0.>(relief<100.))/100.

  ; TIFF file specified?

  if keyword_set(tiff) then tf = tiff else tf = ''

  ; PNG file specified?

  if keyword_set(png) then pf = png else pf = ''

  ;******************
  ; Check arguments *
  ;******************

  ; Input grid layer

  nx = n_elements(grd[*,0])
  ny = n_elements(grd[0,*])

  ; DEM gradient

  if n_params() eq 1 then begin
    dhdx = fltarr(nx, ny)
    dhdy = fltarr(nx, ny)
  endif else begin
    if n_elements(dhdx[*,0]) ne nx or n_elements(dhdx[0,*]) ne ny or $
       n_elements(dhdy[*,0]) ne nx or n_elements(dhdy[0,*]) ne ny then begin
       message, 'problem'
       return
    endif
  endelse

  ;******************
  ; Prepare shading *
  ;******************

  cl = rotate(grd, 7)

  sl = dhdx - dhdy

  min_sl  = min(sl)
  max_sl  = max(sl)
  mean_sl = moment(sl, SDEV=sdev_sl)

  sl = rotate(sl, 7)

  p = where(sl gt 0)

  sl[p] = 0.4*sin(0.5*!pi*(-1>(sl[p]/(2*sdev_sl))<1))

  p = 0

  level = 1.0 - 0.1*rel_factor ; 1.0 for 0% and 0.9 for 100%
  sens  = 0.7*rel_factor       ; 0.0 for 0% and 0.7 for 100%

  ;****************
  ; Apply shading *
  ;****************

  if ~ gs then begin
    r = rp[cl]
    g = gp[cl]
    b = bp[cl]

    cl = 0

    r = byte(0 > (level*r*(1+sens*sl) < 255))
    g = byte(0 > (level*g*(1+sens*sl) < 255))
    b = byte(0 > (level*b*(1+sens*sl) < 255))

    sl = 0

    r = rotate(r, 7)
    g = rotate(g, 7)
    b = rotate(b, 7)

    out_pal = bytarr(3, 256)

    ; Pseudocolor image?

    if pc then begin
      img = color_quan(r, g, b, rp, gp, bp)

      out_pal[0,*] = rp
      out_pal[1,*] = gp
      out_pal[2,*] = bp

      if tf ne '' then write_tiff, tf+'.tif', img, 1, RED=rp, GREEN=gp, BLUE=bp
      if pf ne '' then write_png,  pf+'.png', rotate(img,7), rp, gp, bp

    endif else begin

      img = bytarr(3, nx, ny)
      img[0,*,*] = r[*,*]
      img[1,*,*] = g[*,*]
      img[2,*,*] = b[*,*]

      for i=0,2 do out_pal[i,*] = bindgen(256)

      if tf ne '' then write_tiff, tf+'.tif', img, 1

      if pf ne '' then write_png, pf+'.png', img, /ORDER

    endelse

  endif else begin

    img = pal[temporary(cl)]

    img = byte(0 > (level*img*(1+sens*temporary(sl)) < 255))

    if pf ne '' then write_png, pf+'.png', img

    img = rotate(img, 7)

    out_pal = pal

    if tf ne '' then write_tiff, tf+'.tif', img, 1

  endelse
  ;********
  ;* DONE *
  ;********
 

end

;-----------------------------------------------------------------------
;+
; NAME:
;       CAP_map_grid
;
; PURPOSE:
;       This procedure produces shaded map layers in CAP.
;
; CATEGORY:
;       Mapping
;
; CALLING SEQUENCE:
;       CAP_map_grd, ret, grd, dhdx, dhdy, PAL=pal, IDX=idx, NAME=name, /NO_LEG, RELIEF=relief, MASK=mask, FADE=fade, /GREY, PNG=png
;
; INPUT:
;       grd:
;       dhdx:
;       dhdy:
;
; KEYWORDS:
;       PAL     (I):
;       IDX     (I):
;       NAME    (I):
;       /NO_LEG (I):
;       RELIEF  (I):
;       MASK    (I):
;       FADE    (I):
;       /GREY   (I):
;       PNG     (I):
;
; OUTPUT:
;       ret: TNT return code
;
; MODIFICATION HISTORY:
;       Written by: D. Scherer 15-Jun-2009
;       Modified:   16-Jun-2009 DiS
;                   Routine implemented from prior version
;                   23-Jun-2009 DiS
;                   Minor improvements
;-
;-----------------------------------------------------------------------

pro GEO_map_grd, grd, dhdx, dhdy, PAL=pal, IDX=idx, NAME=name, NO_LEG=no_leg, RELIEF=relief, MASK=mask, FADE=fade, GREY=grey, PNG=png

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2

  ;*****************
  ; Check keywords *
  ;*****************

  r = bindgen(256)
  g = bindgen(256)
  b = bindgen(256)

  if n_elements(pal) ne 0 then begin
    N_c = n_elements(idx)
    if N_c eq 0 or N_c gt 127 then begin
      MESSAGE, 'nogood' 
      return
    endif

    r[0:N_c-1] = pal[0,idx]
    g[0:N_c-1] = pal[1,idx]
    b[0:N_c-1] = pal[2,idx]

    if n_elements(name) eq 0 then iname = str_equiv(fix(idx)) else iname = strtrim(name, 2) 

  endif else begin

    N_c = 256
  endelse

  ipal = bytarr(3,256)

  ipal[0,*] = r
  ipal[1,*] = g
  ipal[2,*] = b

  do_leg = ~ keyword_set(no_leg) and N_c le 127

  if n_elements(relief) ne 1 then irel = 100 else irel = relief

  if n_elements(mask) eq n_elements(grd) then begin
    do_mask = TRUE
    if n_elements(fade) ne 1 then ifade = 0.25 else ifade = fade
  endif else begin
    do_mask = FALSE
  endelse

  ;************************************
  ; Modify palette for masked mapping *
  ;************************************

  if do_mask then begin
    color_convert, r[0:N_c-1], g[0:N_c-1], b[0:N_c-1], hue, sat, val, /RGB_HSV

    sat[*] *= ifade
    
    if keyword_set(grey) then val[0] = 0.5 else val[*] = 1

    color_convert, hue, sat, val, rf, gf, bf, /HSV_RGB

    c_off = 128

    ipal[0,c_off:c_off+N_c-1] = rf
    ipal[1,c_off:c_off+N_c-1] = gf
    ipal[2,c_off:c_off+N_c-1] = bf

    ; Mask grd

    igrd = byte(idx[grd])

    p = where(mask eq 0, cnt)
    if cnt gt 0 then igrd[p] += c_off

  endif else begin

    igrd = grd

  endelse

  ;********
  ;* DONE *
  ;********

  GEO_shade, igrd, dhdx, dhdy, PAL=ipal, RELIEF=irel, PNG=png

end


pro PLO_shading, s_r, S_g, s_b, wlon, wlat, topo_file, MASK = mask

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  
  siz = SIZE(wlon,/DIMENSIONS)
  grd = bytarr(siz[0],siz[1])
      
  imageTot = Transpose(tvrd(/TRUE), [1,2,0])
  imagePlo = imageTot[StartX : StartX+siz[0]-1, StartY:  StartY+siz[1]-1,*]  
  imagePlo = Reverse(imagePlo, 2)
   
;  Loadct, color_tab, /SILENT
;  TVLCT, r, g, b, /Get
;  
;  colors = [255, color_levels]
;  
;  N_Colors = N_ELEMENTS(colors)
;  pr_idx = bindgen(N_Colors)
;  
;  s_r = bytarr(N_Colors)
;  s_g = bytarr(N_Colors)
;  s_b = bytarr(N_Colors)
;  
;  for i=0, N_Colors-1 do begin
;    s_r[i] = r[colors[i]]
;    s_g[i] = g[colors[i]]
;    s_b[i] = b[colors[i]]
;  endfor

  N_Colors = N_ELEMENTS(s_r)
  pr_idx = bindgen(N_Colors)
  PR_pal = bytarr(3, N_Colors)
  PR_pal[0, *] = s_r
  PR_pal[1, *] = s_g
  PR_pal[2, *] = s_b
  

    
  for l=0, N_Colors-1 do begin
    p = where(imagePlo[*,*,0] eq s_r[l] and imagePlo[*,*,1] eq s_g[l] and imagePlo[*,*,2] eq s_b[l], cnt)
    if cnt gt 0 then grd[p] = l
  endfor
     
  GEO_get_Gradients, wlon, wlat, dhdx, dhdy, topo_file
  
  if KEYWORD_SET(mask) then tomask = REVERSE(congrid(mask, imgX, imgY, /CENTER)  ,2)
  
  WAVE_root, root
  png = root + 'temp/shades'
  GEO_map_grd, grd, 1-dhdx, 1-dhdy, PAL=PR_pal, IDX=pr_idx, PNG=png, MASK = tomask, fade = 0.4
    
  ; Retreive shaded picture  
  shades = Read_PNG(png+'.png')   
  shades = Transpose(shades, [1,2,0])  
   
  tv, shades, StartX, StartY, TRUE=3
      
end


pro PLO_add_LL_map, intval, lon, lat

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
      
  ;--------------------------
  ; Contour the lat and lons
  ;--------------------------
  
  Nlevels = 360 / intval    
  levels = INDGEN(Nlevels) * intval - 170
  
  p = where(levels le floor(max(Lon)) and levels ge ceil(min(Lon)), cnt)
  if cnt gt 0 then lonlevels = levels[p]
  p = where(levels le floor(max(Lat)) and levels ge ceil(min(Lat)), cnt)
  if cnt gt 0 then latlevels = levels[p]
  
  templon = ABS(lon)
  templevs = ABS(lonlevels)
      
  loadct, 0, /SILENT
  contour, templon, LEVELS = templevs[UNIQ(templevs, SORT(templevs))], XStyle=5, YStyle=5, /OVERPLOT, C_linestyle = 2, C_COLORS=60, C_THICK = g_charthick
  contour, lat , LEVELS = latlevels, XStyle=5, YStyle=5, /OVERPLOT, C_linestyle = 2, C_COLORS=60, C_THICK = g_charthick
    
  ;--------------------------
  ; Add the tick anotations
  ;--------------------------
  p = where(levels le floor(max(Lon[*,0])) and levels ge ceil(min(Lon[*,0])), cnt)
  if cnt gt 0 then lonlevels = levels[p]
  p = where(levels le floor(max(Lat[0,*])) and levels ge ceil(min(Lat[0,*])), cnt)
  if cnt gt 0 then latlevels = levels[p]
  
  Nlonlevels = N_ELEMENTS(lonlevels)
  Nlatlevels = N_ELEMENTS(latlevels)
  
  if intval ge 1 then format = '(I4,X)' else format = '(F6.1,X)'
  
  if LATLON_NOTES then begin
    for i=0,Nlonlevels-1 do begin
      p = where(Lon[*,0] le lonlevels[i] ,cnt)
      label = string(lonlevels[i],FORMAT=format)
      if lonlevels[i] lt 0 then label += 'W' else label += 'E'
      if cnt ge 1 then xyouts, max(p) + StartX ,  - G_CHARSIZE * 10 + StartY,GEN_strtrim(label,/ALL), COLOR = 0, ALI = 0.5, /DEVICE, CHARSIZE = G_CHARSIZE, CHARTHICK= g_charthick
    endfor
    for i=0,Nlatlevels-1 do begin
      p = where(Lat[0,*] le latlevels[i] ,cnt)
      label = string(latlevels[i],FORMAT=format)
      if latlevels[i] lt 0 then label += 'S' else label += 'N'
      if cnt ge 1 then xyouts, - G_CHARSIZE * 14 + StartX, max(p) + StartY,GEN_strtrim(label,/ALL), COLOR = 0, ALI = 0.5, /DEVICE, CHARSIZE = G_CHARSIZE, CHARTHICK= g_charthick
    endfor
  end
  
  ;--------------------------
  ; Add the frame
  ;--------------------------
  
  frameX = DBLARR(5)
  frameY = DBLARR(5)
   
  frameX[0] = StartX-1
  frameY[0] = StartY-1
  
  frameX[1] = frameX[0] + ImgX + 1
  frameY[1] = frameY[0]
  
  frameX[2] = frameX[1]
  frameY[2] = frameY[0] + ImgY + 1
  
  frameX[3] = frameX[0]
  frameY[3] = frameY[2]
  
  frameX[4] = frameX[0]
  frameY[4] = frameY[0]  
  
  plots, frameX, frameY, COLOR=0, /NOCLIP, THICK= FRAME_THICK, /DEVICE 
  
end

pro PLO_add_XY_map, intval, x, y

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
      
  ;--------------------------
  ; Contour the lat and lons
  ;--------------------------
  rangex = double([min(x),MAX(x)]) 
  rangey = double([min(y),MAX(y)])
  
  minx = ceil(rangex[0]/intval) * intval
  maxx = FLOOR(rangex[1]/intval) * intval
  miny = ceil(rangey[0]/intval) * intval
  maxy = FLOOR(rangey[1]/intval) * intval
  
  xlevs = minx + INDGEN((maxx-minx)/intval + 1) * intval
  ylevs = miny + INDGEN((maxy-miny)/intval + 1) * intval
  
      
  loadct, 0, /SILENT
  contour, x,  LEVELS = xlevs, XStyle=5, YStyle=5, /OVERPLOT, C_linestyle = 2, C_COLORS=60, C_THICK = g_charthick
  contour, y , LEVELS = ylevs, XStyle=5, YStyle=5, /OVERPLOT, C_linestyle = 2, C_COLORS=60, C_THICK = g_charthick
    
  ;--------------------------
  ; Add the tick anotations
  ;--------------------------
  
  format = '(I8)'

  if LATLON_NOTES then begin
    for i=0,N_ELEMENTS(xlevs)-1 do begin
      p = where(x[*,0] le xlevs[i] ,cnt)
      label = string(xlevs[i],FORMAT=format)
      if cnt ge 1 then xyouts, max(p) + StartX ,  - G_CHARSIZE * 10 + StartY,GEN_strtrim(label,/ALL), COLOR = 0, ALI = 0.5, /DEVICE, CHARSIZE = G_CHARSIZE, CHARTHICK= g_charthick
    endfor
    for i=0,N_ELEMENTS(ylevs)-1 do begin
      p = where(y[0,*] le ylevs[i] ,cnt)
      label = string(ylevs[i],FORMAT=format)
      if cnt ge 1 then xyouts, - G_CHARSIZE * 7 + StartX, max(p) + StartY,GEN_strtrim(label,/ALL), COLOR = 0, ALI = 0.5, /DEVICE, CHARSIZE = G_CHARSIZE, CHARTHICK= g_charthick, ORIENTATION=90
    endfor
  end
  
  ;--------------------------
  ; Add the frame
  ;--------------------------
  
  frameX = DBLARR(5)
  frameY = DBLARR(5)
   
  frameX[0] = StartX-1
  frameY[0] = StartY-1
  
  frameX[1] = frameX[0] + ImgX + 1
  frameY[1] = frameY[0]
  
  frameX[2] = frameX[1]
  frameY[2] = frameY[0] + ImgY + 1
  
  frameX[3] = frameX[0]
  frameY[3] = frameY[2]
  
  frameX[4] = frameX[0]
  frameY[4] = frameY[0]  
  
  plots, frameX, frameY, COLOR=0, /NOCLIP, THICK= FRAME_THICK, /DEVICE 
  
end

pro PLO_draw_wind, grid, ud, vd, density, LENGTH=length, LEGEND = legend

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
    
  if ~KEYWORD_SET(length) then length = 1.8
  
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
  imgG->transform_XY, x, y, c.proj, devDLX, devDLY, /NEAREST
  
  utils_1d_to_2d, xi, yi, xi, yi
  ud = ud[xi,yi]
  vd = vd[xi,yi]  
  
  p = where(devDLX lt imgx and devDLX gt 0 and devDLY lt imgY and devDLY gt 0, cx)

;  utils_1d_to_2d, px, py, pxi, pyi
  
  partvelvec, ud[p], vd[p], devDLX[p] + StartX, devDLY[p] + StartY, /oVER, VECCOLORS=0, LENGTH=0.1, /DEVICE ;,COLOR=color
  
;  s = SIZE(ud)
;  for l = 0, s[2]-1 do for k = 0, s[3]-1 do velovect, reform(ud[l,k],1,1), reform(vd[l,k],1,1), devDLX[l,k] + StartX, devDLY[l,k] + StartY, COLOR=0, LENGTH=length, /OVER, /DEVICE, THICK = 1.2 * W_size_fac
  
  
;  px = where(devDLX lt imgx and devDLX gt 0, cx)
;  py = where(devDLY lt imgY and devDLY gt 0, cy)
;  utils_1d_to_2d, px, py, pxi, pyi
;  
;  velovect, ud[pxi,pyi], vd[pxi,pyi], devDLX[px] + StartX, devDLY[py] + StartY, COLOR=0, LENGTH=length, /OVER, /DEVICE, THICK = 1.2 * W_size_fac
;  
;  if KEYWORD_SET(LEGEND) then begin
;   vels = sqrt(ud[pxi,pyi]^2 + vd[pxi,pyi]^2)
;   m = max(vels)
;   cx = [devDLX[px[0]], devDLX[px[1]]] + 10.  * W_size_fac + StartX
;   cy = [devDLy[py[0]], devDLX[py[1]]] - 50.  * W_size_fac + StartY
;   velovect, [[m,100.],[100.,100.]], [[0.,100.],[100.,100.]], cX, cY , COLOR=0, LENGTH=length, /OVER, /DEVICE, MISSING=100., THICK = 1.2 * W_size_fac
;   XYOUTS, cx[1] + 15. * W_size_fac * length, cy[0] - 4. * W_size_fac, 'Max velocity: ' + STRING(m, FORMAT='(F6.2)') + ' m/s', /DEVICE, color = 0, CHARSIZE=G_CHARSIZE, CHARTHICK=g_charthick
;  endif  
  
  
end

pro PLO_trace_borders, shape_file, range = range

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  
  ;--------------------------
  ; Plot
  ;--------------------------
  WAVE_root, root
  restore, FILE = SHAPE_FILE ;latlons, conn, shp_src created
  
  xRan = [startX - 1 , startX + ImgX]
  yRan = [startY - 1 , startY + ImgY]
    
  ;TODO: Save time
  
  index = 0
  
  while index lt N_ELEMENTS(conn) do begin
  
    nbElperConn = conn[index]
    
    idx = conn[index+1:index+nbElperConn]
    
    index += nbElperConn + 1
    
    tlatlons = latlons[*,idx]
    
    lats = tlatlons[1,*]
    lons = tlatlons[0,*]
    
    if N_ELEMENTS(lons) lt 3 then continue
    if KEYWORD_SET(range) then if min(lats) gt range[3] then continue  
    if KEYWORD_SET(range) then if max(lats) lt range[2] then continue  
    if KEYWORD_SET(range) then if min(lons) gt range[1] then continue  
    if KEYWORD_SET(range) then if max(lons) lt range[0] then continue  
    
    imgG->transform, lons, lats, devX, devY, SRC = shp_src
      
    devX += StartX
    devY += StartY
    
    p = where(devX lt xRan[0], cnt)
    if cnt gt 0 then devX[p] = xRan[0]
    
    p = where(devX gt xRan[1], cnt)
    if cnt gt 0 then devX[p] = xRan[1]
    
    p = where(devY lt yRan[0], cnt)
    if cnt gt 0 then devY[p] = yRan[0]
    
    p = where(devY gt yRan[1], cnt)
    if cnt gt 0 then devY[p] = yRan[1]
    
    plots, devX , devY , /DEVICE,  Color=0, THICK=BORDER_THICK
    
  endwhile
  
end


FUNCTION PLO_CIRCLE, xcenter, ycenter, radius
  points = (2 * !PI / 99.0) * FINDGEN(100)
  x = xcenter + radius * COS(points )
  y = ycenter + radius * SIN(points )
  RETURN, TRANSPOSE([[x],[y]])
END


pro PLO_draw_Point, x, y, TEXT = text, LEFT = left, SRC = src, FONTSIZE = fontsize, top = top, quad = quad, circle = circle, EMPTY = empty

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  
  if ~KEYWORD_SET(SRC) then GIS_make_datum, ret, src, NAME='WGS-84'
  
  imgG->transform, x, y, pxs, pys, SRC = src
  
  xran = [0, imgX]  
  yran = [0, imgY]  
  
  ;TODO: remove this for loop
  for i = 0, N_ELEMENTS(pxs)-1 do begin
  
    px = pxs[i]
    py = pys[i]    
    
    if (px lt xRan[0] or px gt xRan[1] or py lt yRan[0] or py gt yRan[1]) then continue
    
    x = [px+G_PointSize + 1, px, px-G_PointSize - 1, px, px+G_PointSize + 1]
    y = [py, py+G_PointSize + 1, py, py-G_PointSize - 1, py]
    
    if KEYWORD_SET(quad) then begin
        x = [px-G_PointSize, px+G_PointSize, px+G_PointSize, px-G_PointSize, px-G_PointSize]
        y = [py+G_PointSize, py+G_PointSize, py-G_PointSize, py-G_PointSize, py+G_PointSize]
    endif
    if KEYWORD_SET(circle) then begin 
     x = PLO_CIRCLE(px, py, G_PointSize)
     y = x[1,*]
     x = x[0,*]
    endif
          
    LOADCT, 0, /SILENT 
    if NOt KEYWORD_SET(empty) then Polyfill, x + StartX, y + startY, /DEVICE,  Color=0
    plots, x + StartX, y + startY, /DEVICE,  Color=0, THICK=2
    
    if KEYWORD_SET(text) then begin
      if KEYWORD_SET(LEFT) then XYOUTS, px + StartX - 5, py - 21 + StartY, text[i], ALIGNMENT=1 , CHARSIZE= G_CHARSIZE, CHARTHICK = G_CHARTHICK, /DEVICE, COLOR = 0 $
      else XYOUTS, px + StartX + 5, py - 21 + StartY, text[i], ALIGNMENT=0 , CHARSIZE= G_CHARSIZE, CHARTHICK = G_CHARTHICK, /DEVICE, COLOR = 0
    end
    if KEYWORD_SET(top) then begin
      if KEYWORD_SET(LEFT) then XYOUTS, px + StartX - 5, py + 16 + StartY, top[i], ALIGNMENT=1 , CHARSIZE= G_CHARSIZE, CHARTHICK = G_CHARTHICK, /DEVICE, COLOR = 0 $
      else XYOUTS, px + StartX + 5, py + 16 + StartY, top[i], ALIGNMENT=0 , CHARSIZE= G_CHARSIZE, CHARTHICK = G_CHARTHICK, /DEVICE, COLOR = 0
    end
       
  endfor
  
end

pro PLO_draw_GRID, grid, latlons = latlons, ijs = ijs, quad = quad, circle = circle, EMPTY = empty

  grid->get_XY, x, y, nx, ny, p
  
  utils_1d_to_2d, INDGEN(nx, /LONG), INDGEN(ny, /LONG), is, js
  if KEYWORD_SET(ijs) and KEYWORD_SET(latlons) then begin
   text = '(' + STRING(is, FORMAT=('(I3)')) + ',' + STRING(js, FORMAT=('(I3)')) + ')' 
   grid->get_LonLAt, lon, lat 
   top = STRING(lon, FORMAT=('(F8.4)')) + ',' + STRING(lat, FORMAT=('(F8.4)'))
   PLO_draw_Point, x, y, src = p, TEXT=text, top = top, quad = quad, circle = circle, EMPTY = empty
  endif else if KEYWORD_SET(ijs) then begin
   text = '(' + STRING(is, FORMAT=('(I3)')) + ',' + STRING(js, FORMAT=('(I3)')) + ')' 
   PLO_draw_Point, x, y, src = p, TEXT=text, quad = quad, circle = circle, EMPTY = empty
  endif else if KEYWORD_SET(latlons) then begin
   grid->get_LonLAt, lon, lat   
   text = STRING(lon, FORMAT=('(F8.4)')) + ',' + STRING(lat, FORMAT=('(F8.4)'))
   PLO_draw_Point, x, y, src = p, TEXT=text, quad = quad, circle = circle, EMPTY = empty
  endif else begin
   PLO_draw_Point, x, y, src = p, quad = quad, circle = circle, EMPTY = empty
  endelse

end

pro PLO_draw_GRID_CONTOUR, grid

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  
  xRan = [startX - 1 , startX + ImgX]
  yRan = [startY - 1 , startY + ImgY]

  grid->get_XY, x, y, nx, ny, p
  grid->getProperty, tnt_c = c
  
  xi = [reform(x[*,ny-1]),reform(rotate(x[nx-1,*],7)),reform(x[*,0]),reform(x[0,*])]
  yi = [reform(y[*,ny-1]),reform(rotate(y[nx-1,*],7)),reform(y[*,0]),reform(y[0,*])]
  
  imgG->transform, xi, yi, devX, devY, SRC = c.proj
  
  devX += StartX
  devY += StartY
  
  p = where(devX lt xRan[0], cnt)
  if cnt gt 0 then devX[p] = xRan[0]
  
  p = where(devX gt xRan[1], cnt)
  if cnt gt 0 then devX[p] = xRan[1]
  
  p = where(devY lt yRan[0], cnt)
  if cnt gt 0 then devY[p] = yRan[0]
  
  p = where(devY gt yRan[1], cnt)
  if cnt gt 0 then devY[p] = yRan[1]
  
  plots, devX , devY , /DEVICE,  Color=0, THICK=BORDER_THICK
  
end

pro PLO_draw_letter, letter, HORIZONTAL = horizontal

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O

  if ~KEYWORD_SET(HORIZONTAL) then XYOUTS, 30, devCenterY - 5, letter, ALIGNMENT=0 , CHARSIZE=G_CHARSIZE, CHARTHICK=G_CHARTHICK, /DEVICE, COLOR = 0 $
  else XYOUTS, devCenterX, 20, letter, ALIGNMENT=0.5 , CHARSIZE=G_CHARSIZE+2, CHARTHICK=G_CHARTHICK, /DEVICE, COLOR = 0
  
end

pro PLO_draw_dom, domFile, thickness, ADD_num = ADD_num

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  
  domFile->get_Lonlat, wlon2, wlat2, nx, ny
  imgG->get_Lonlat, lon, lat
  
  
  lons = DBLARR(5)
  lats = DBLARR(5)
  
  lons[0] = wlon2[nx-1,0]
  lats[0] = wlat2[nx-1,0]  
  lons[1] = wlon2[nx-1,ny-1]
  lats[1] = wlat2[nx-1,ny-1]  
  lons[2] = wlon2[0,ny-1]
  lats[2] = wlat2[0,ny-1]  
  lons[3] = wlon2[0,0]
  lats[3] = wlat2[0,0]  
  lons[4] = wlon2[nx-1,0]
  lats[4] = wlat2[nx-1,0]
  
  loadct, 0, /SILENT   
  p = utils_POS_NEAREST_NEIGHBORHOOD(lon,lat,lons,lats)  
  ind = ARRAY_INDICES(lat, p)
  clon = ind[0,*] + StartX
  clat = ind[1,*] + startY
          
  LOADCT, 0, /SILENT 
  plots, clon, clat, /DEVICE,  Color=0, THICK=thickness
  
  domFile->GetProperty, dom = num
  num = 'd0' + str_equiv(num)
 
  if KEYWORD_SET(add_num) then XYOUTS, clon[3]  + 4, clat[3] + 4, num, ALIGNMENT=0 , CHARSIZE= add_num *0.8 * W_size_fac, /DEVICE, COLOR = 0
    
end

;********************************************************************************************************************************************
;********************************************************************************************************************************************
;
;                               The real plot routines begin here !
;
;********************************************************************************************************************************************
;********************************************************************************************************************************************

pro PLO_array, toPlot, grid, data_levels, colors, titlebar = TITLEBAR, bar_tags = bar_tags, color_table = color_table, GRAPHTITLE = graphtitle, MASK = mask, $
                            PNG = png, PIXMAP=pixmap, WINDOW = window, SUBTITLE = subtitle, NO_BAR = no_bar, HORIZONTAL_BAR = horizontal_bar, TOPO_FINE = TOPO_FINE, $
                            LatLon_interval = LatLon_interval,  XY_interval = XY_interval, SHAPE_loc = SHAPE_loc, INTERP=interp, $
                            nodemi = nodemi

  ;--------------------------
  ; Set up environment
  ;--------------------------
  compile_opt idl2
  @WAVE.inc
  common PLO_ADMIN_X
  common PLO_ADMIN_G
  common PLO_ADMIN_O
  WAVE_root, root
  
  ;--------------------------
  ; Set the window
  ;--------------------------
  nx = N_ELEMENTS(toplot[*,0])
  ny = N_ELEMENTS(toplot[0,*])
  
  PLO_set_X_device, pos_graph, pos_bar, PIXMAP = TRUE, XonY = DOUBLE(Nx)/DOUBLE(ny), WINDOW = window, HORIZONTAL_BAR = horizontal_bar, NO_BAR = no_bar
  
  if KEYWORD_SET(nodemi) then imgG = grid->reGrid(YSize = DOUBLE(ImgY)) else imgG = grid->reGrid(YSize = DOUBLE(ImgY), /PLOT)
     
  ;-------------------------------------
  ; Plot dummy data for title and more 
  ;-------------------------------------   
  loadct, 0, /SILENT
  contour, congrid(toplot, imgX, imgY, /CENTER), XStyle=5, YStyle=5, /Follow , /FILL , LEVELS = data_levels, C_COLORS = color_levels, FONT = -1, $
    TITLE =graphtitle,  /DEVICE, BACKGROUND=255, COLOR=0, POS=pos_graph, CHARSIZE = G_CHARSIZE, CHARTHICK=G_CHARTHICK, SUBTITLE = SUBTITLE
    
  ;----------------------------
  ; See what colors do we have
  ;----------------------------
  if ~KEYWORD_SET(BAR_TAGS) then BAR_TAGS = STRING(data_levels, FORMAT = '(F6.1)')
  if ~KEYWORD_SET(titlebar) then titlebar = 'Data'
  
  sizC = SIZE(colors)
  
  if sizC[0] eq 1 then begin
    
    if ~KEYWORD_SET(color_table) then begin
      print, 'No color table set?'
      color_table = 0
    endif
    
    Loadct , color_table, /SILENT
    TVLCT, r, g, b, /Get
    
    N_Colors = sizC[1]
    color_levs = [255,colors]
    
    s_r = bytarr(N_Colors + 1) + 255B
    s_g = bytarr(N_Colors + 1) + 255B
    s_b = bytarr(N_Colors + 1) + 255B
    
    for i=0, N_Colors do begin
      s_r[i] = r[color_levs[i]]
      s_g[i] = g[color_levs[i]]
      s_b[i] = b[color_levs[i]]
    endfor
    
  endif else if sizC[0] eq 2 then begin
   
   color_table = 0

    color_levs = colors
    N_Colors = sizC[1]
    
    s_r = bytarr(N_Colors + 1) + 255B
    s_g = bytarr(N_Colors + 1) + 255B
    s_b = bytarr(N_Colors + 1) + 255B
    ;TODO: this is very baaaad:
    for i=0L, N_Colors-1 do begin
      s_r[i+1] = color_levs[i,0]
      s_g[i+1] = color_levs[i,1]
      s_b[i+1] = color_levs[i,2]
    endfor
    
  endif else message, 'Colors not good'
  
  ;--------------------------
  ; Over plot array
  ;--------------------------
  t = congrid(toplot, imgX, imgY, /CENTER, interp = interp) 
  re = LONG(t) * 0L - 1L
  img = bytarr(imgX,imgY,3) 
       
  for l=0, N_Colors-1 do begin
    if l lt N_Colors-1 then p = where(t ge data_levels[l] and t lt data_levels[l+1], cnt) $
    else p = where(t ge data_levels[l], cnt)
    if cnt gt 0 then re[p]= l
  endfor
  
  re+=1  
   
  r = byte(0 > s_r[re] < 255)
  g = byte(0 > s_g[re] < 255)
  b = byte(0 > s_b[re] < 255)
  img = [[[r]],[[g]],[[b]]] 
  
  tv, img, startX, startY , /DEVICE, TRUE=3   
  
  ;---------
  ; Mapping
  ;---------  
  
  if KEYWORD_SET(TOPO_FINE) then topo_file = TOPO_FILE_d3 else topo_file = TOPO_FILE_d1    
  
  if SHADING then begin
    imgG->GET_LonLat, lon, lat
    imgG->GETProperty, tnt_c = tnt_c
    GIS_make_datum, ret, datumwgs84 , NAME='WGS-84'
    GIS_coord_trafo,  ret, lon, lat, lonwgs, latwgs, SRC = tnt_c.proj.Datum, DST = datumwgs84
    lonwgs = REFORM(lonwgs, N_ELEMENTS(lon[*,0]), N_ELEMENTS(lon[0,*]))
    latwgs = REFORM(latwgs, N_ELEMENTS(lon[*,0]), N_ELEMENTS(lon[0,*]))
    if topo_file ne '' then PLO_shading, s_r, s_g, s_b, lonwgs, latwgs, topo_file, MASK = mask
  endif
  
  if KEYWORD_SET(LatLon_interval) then begin
    imgG->GET_LonLat, lon, lat
    PLO_add_LL_map, LatLon_interval, lon, lat
  endif
  if KEYWORD_SET(XY_interval) then begin
      imgG->GETProperty, tnt_c = tnt_c
      x = tnt_c.x0 + INDGEN(tnt_c.nx, /LONG) * tnt_c.dx
      y = tnt_c.y1 + INDGEN(tnt_c.ny, /LONG) * tnt_c.dy 
      utils_1d_to_2d, x, y , x, y     
      PLO_add_XY_map, XY_interval, x, y
  endif
  if BORDERS and SHAPE_FILE_world ne '' then PLO_trace_borders, SHAPE_FILE_world
  if BORDERS and KEYWORD_SET(SHAPE_loc) and SHAPE_FILE_local ne '' then PLO_trace_borders, SHAPE_FILE_local
  if BORDERS and SHAPE_FILE_other ne '' then PLO_trace_borders, SHAPE_FILE_other
         
  ;--------------------------
  ; ADD COLORBAR
  ;--------------------------  
  if ~KEYWORD_SET(horizontal_bar) then VERTIBAR = TRUE else VERTIBAR = FALSE  
  Loadct , color_table, /SILENT
  if sizC[0] eq 2 then barc = COLOR24(colors) else barc = colors
  if ~KEYWORD_SET(NO_BAR) then DCBar, barc, COLOR="BLACK", LABELS=bar_tags, Position=pos_bar, TITLE=titlebar, CHARSIZE = G_CHARSIZE, $
              MYCHARDIFAC = 0.8, VERTICAL = VERTIBAR, CHARTHICK=g_charthick ;, /ARROW
  
  ;--------------------------
  ; Resolve all
  ;--------------------------
  PLO_show_img, PIXMAP=pixmap, WINDOW = window, PNG = png
    
end