;+
; :Description:
;   Draws a cross-section contour plot for wind-speed and potential temperature
;
; :Params:
;    ws: in, required
;        a 2d array (lat-press or lon-pres) of wind-speed
;    theta: in, required
;           a 2d array (lat-press or lon-pres) of potential temperature
;    x_m: in, required
;         the x coordinates in the model system, e.g m (although any regular
;         grid will do the trick)
;    x_deg: in, required
;           the x coordinates in latitude (probably irregular)
;    pl: in, required
;        the pressure levels
;
; :Keywords:
;    WS_LEVELS: in, optional, default:should be ok
;               wind speed levels (positive, I will take care of the negative)
;    THETA_LEVELS: in, optional, default:should be ok
;                 theta levels (positive, I will take care of the negative)
;    MASK: in, optional, default:finite
;          topography mask where no data is available. You might want to overwrite 
;          the default if you are plotting different months on different plots and
;          want the topography to remain the same
;    TITLE: in, optional
;           plot title
;    POSITION: in, optional
;              plot position
;    YTICKMARKS: in, optional, default:should be ok
;                where to write the pressure tickmarks
;    XTICKMARKS: in, optional, default:should be ok
;                where to write the lat tickmarks
;    YTITLE: in, optional, default:should be ok
;            ytitle
;    CHARSIZE: in, optional, default:should be ok
;              charsize
;    ADDCMD: in, optional, default:should be ok
;            add the commands to a cgWindow
;
; :Author: Fabien Maussion 2012
;          Last modification: FaM, May 8, 2014
;
;-
pro w_crossection_wstheta, ws, theta, x_m, x_deg, pl, $
  WS_LEVELS=ws_levels, $
  THETA_LEVELS=theta_levels, $
  MASK=mask, $
  TITLE=title, $
  POSITION=position, $
  YTICKMARKS=ytickmarks, $
  XTICKMARKS=xtickmarks, $
  YTITLE=ytitle, $
  CHARSIZE=charsize, $
  ADDCMD=addcmd

  ; Set up environment
  compile_opt idl2
  @WAVE.inc
  
  ; Security checks
  if ~ array_processing(ws, theta) then Message, 'WS and THETA not compatible'
  sw = size(ws)
  if sw[0] ne 2 then message, 'WS and THETA should be 2-dimensional'
  if (sw[1] ne n_elements(x_m)) or (sw[1] ne n_elements(x_deg)) then $
    message, 'X-coordinates x_m or x_deg not valid'
  if (sw[2] ne n_elements(pl)) then message, 'Y-coordinates pl not valid'
  tvlct, rr, gg, bb, /GET ; to restore later
  
  ; Default values
  SetDefaultValue, ws_levels, findgen(8)*5 + 15
  SetDefaultValue, theta_levels, 290. + FINDGEN(8)*20
  SetDefaultValue, mask, ~finite(ws)
  SetDefaultValue, xtickmarks, [10., 20, 30, 40, 50]
  SetDefaultValue, ytickmarks, [100,200,300,400,500,700,1000]
  SetDefaultValue, ytitle, 'Pressure (hPa)'
     
  ; Tick values

  ; Where are the corresponind lats in cartesian space?
  xtv = x_m[VALUE_LOCATE(x_deg, xtickmarks)] 
  nxt = N_Elements(xtv)
  ; Pressure levels have to be inverted
  ytv = ytickmarks[reverse(sort(ytickmarks))]
  nyt = N_Elements(ytv)
  ; Xtick names TODO: make it more generic!
  xtnames = STRING(xtickmarks, FORMAT='(I02)') + 'N'
  
  
  ; Grey shaded contours for windspeed (absolute)  
  cgLoadCT, 0
  info = w_gr_DataLevels(ABS(ws), /INVERTCOLORS, CMIN=0, CMAX=210, $
    LEVELS=ws_levels, NEUTRAL_COLOR='WHITE')    
  cgContour, ABS(ws), x_m, pl, YRANGE=[1000, 75], /YLOG, /CELL_FILL, $
    LEVELS=info.levels, PALETTE=info.palette[1:*,*], ADDCMD=addcmd, YTICKS=nyt-1, $
    YTICKV=ytv, XSTYLE=5, YTITLE=ytitle, TITLE=title, CHARSIZE=charsize, $
     POSITION=position, /NOERASE
    
  ; Continuous lines for positive countours, dashed lines for negative
  cgContour, ws, x_m, pl, LEVELS=info.levels, /OVERPLOT, ADDCMD=addcmd, CHARSIZE=charsize

  cgContour, ws, x_m, pl, LEVELS=-info.levels[SORT(-info.levels)], /OVERPLOT, $
     ADDCMD=addcmd, C_LINESTYLE=5, CHARSIZE=charsize
  
  ; Color lines for theta
  cgLoadCT, 34
  info = w_gr_DataLevels(LEVELS=theta_levels, /OOB_TOP_COLOR)
  tvlct, info.PALETTE
  cgContour, theta, x_m, pl, LEVELS=info.levels, /OVERPLOT, ADDCMD=addcmd, $
    C_COLORS=indgen(n_elements(info.levels)), C_LINESTYLE=5, C_THICK=2, CHARSIZE=charsize
    
  ; Draw a brown polygon where no data
  y_vertices = pl[TOTAL(mask, 2)]
  y_vertices[N_ELEMENTS(y_vertices)-1] = 1000 
  cgPolygon, x_m, y_vertices, /DATA, COLOR='tan', /FILL, ADDCMD=addcmd
  cgPlotS, x_m, y_vertices, /DATA, COLOR='black', ADDCMD=addcmd
  
  ; Redraw the axis because of the polygon
  cgAxis, 0, XRANGE=utils_minmax(x_m), WINDOW=addcmd, XSTYLE=1, XTICKV=xtv, $
    XTICKS=nxt-1, XTICKNAME = xtnames, $
    XTITLE='', CHARSIZE=charsize
  cgAxis, /XAXIS, XRANGE=utils_minmax(x_m), WINDOW=addcmd, XSTYLE=1, XTICKV=xtv, $
    XTICKS=nxt-1, XTICKNAME = REPLICATE(' ',N_ELEMENTS(xtv)), CHARSIZE=charsize
  
  ; Restore colors
  tvlct, rr, gg, bb

end