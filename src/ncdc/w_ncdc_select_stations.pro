;+
; 
; Selects all available NCDC stations within a grid or Lon-Lat corners.
;
; :Categories:
;    NCDC
;
; :History:
;    Written by FaM, 2012
;
;-


;+
; :Description:
;    Helper routine to reduce an history structure to the selected stations
;
; :Params:
;    h: the history structure
;    p: the selected indexes
;    cnt: the number or indexes
;    
; :Returns:
;    the cropped structure
;
; :History:
;     Written by FaM, 2012.
;
;-
function w_ncdc_select_stations_crop_struct, h, p, cnt

  out={n_stations:cnt, $
    usaf:h.usaf[p] ,$
    wban:h.wban[p] ,$
    name:h.name[p] ,$
    lon:h.lon[p]   ,$
    lat:h.lat[p]   ,$
    elev:h.elev[p]  $
    }
  return, out
end


;+
; :Description:
;    Selects all available NCDC stations within a grid or Lon-Lat corners.
;
; :Params:
;    stations: out
;              a structure containing the station information
;
; :Keywords:
;    GRID: in, optional
;          the stations within the grid will be selected
;    LLBOX: in, optional
;           the stations within the LATLON Box will be selected
;           [DL Lon, DL Lat, UR Lon, UR Right]       
;    DO_PLOT: in, optional
;             set this keyword to make a plot of the stations found             
;
; :History:
;     Written by FaM, 2012.
;
;-
pro w_ncdc_select_stations, stations, GRID=grid, LLBOX=llbox, DO_PLOT=do_plot

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ;ON_ERROR, 2
  
  do_p = KEYWORD_SET(DO_PLOT)

  n_stat = 0L
  
  RESTORE, WAVE_RESOURCE_DIR + '/ncdc/ncdc_history.sav'
  GIS_make_datum, ret, wgs, NAME='WGS-84'  
  _h = TEMPORARY(ncdc_history)
  
  IF N_ELEMENTS(GRID) ne 0 then begin
  
    if not OBJ_ISA(grid, 'w_Grid2D') then Message, WAVE_Std_Message('grid', OBJ='w_Grid2D')
    grid->GetProperty, nx=nx, ny=ny
    
    grid->transform_LonLat, _h.lon, _h.lat  > (-89.99), wgs, i_dst, j_dst ; for the AMUNDSEN-SCOTT station!
    dummy = where((i_dst lt (-0.5)) or (j_dst lt (-0.5)) or (i_dst gt (nx+0.5)) or (j_dst gt (ny+0.5)), COMPLEMENT=p_in, NCOMPLEMENT=cnt_in)
    if cnt_in eq 0 then begin
      MESSAGE, 'No NCDC station found!', /INFORMATIONAL
      return
    endif
    
    if do_p then map = OBJ_NEW('w_Map', grid, YSIZE=600, /BLUE_MARBLE)
    
  endif else if N_ELEMENTS(LLBOX) eq 4 then begin
  
    dummy = where((_h.lon lt llbox[0]) or (_h.lat lt llbox[1]) or (_h.lon gt llbox[2]) or (_h.lat gt llbox[3]), COMPLEMENT=p_in, NCOMPLEMENT=cnt_in)
    
    if cnt_in eq 0 then begin
      MESSAGE, 'No NCDC station found!', /INFORMATIONAL
      return
    endif
    
    if do_p then begin
      GIS_make_proj, ret, proj, PARAM='1, WGS-84'
      grid = OBJ_NEW('w_Grid2D', x0 = llbox[0], x1 = llbox[2], y0 = llbox[3], y1 = llbox[1], dx=0.01, dy=0.01, PROJ=proj)
      map = OBJ_NEW('w_Map', grid, YSIZE=600)
      undefine, grid
    endif
  endif else MESSAGE, WAVE_Std_Message(/ARG)
  
  stations = w_ncdc_select_stations_crop_struct(_h, p_in, cnt_in)
  
  if do_p and stations.n_stations ne 0 then begin
    h = stations.elev
    lon = stations.lon
    lat = stations.lat
    TVLCT, rr, gg, bb, /GET
    cgLoadCT, 34
    info = w_gr_DataLevels(h, N_LEVELS=256)
    d = map->set_plot_params(N_LEVELS=256)    
    d = map->set_data(FLTARR(2,2)-999.) 
    for i=0, N_ELEMENTS(h)-1 do d = map->set_point(lon[i], lat[i], SRC=wgs, COLOR=info.colors[info.loc[i]])
    w_standard_2d_plot, map, title='ALL available NCDC stations', /RESIZABLE, $
                                /NO_BAR, XFACTOR=1.2
    w_gr_Colorbar, info, FORMAT='(I4)', TITLE='Stat. elevation (m)', /FIT, /ADDCMD
    undefine, map
    TVLCT, rr, gg, bb, /GET    
  endif
  
end
