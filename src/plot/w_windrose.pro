;+
; 
;    Makes a Wind rose distribution plot. If a second variable is provided, it also computes 
;    and color-displays the distribution of this new variable within the direction bins. 
;                           
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
; :Version:
;       WAVE V 0.1
;
; :History:
;     Written by FaM, 2011.
;     Modified by FaM, 2012
;
;-

;+
; :Description:
;   Private function to compute nice circles or arcs of circles.
;    
; :Private:
;
;-
function w_windrose_circle, xcenter, ycenter, radius, NPTS=npts, ANGLE_RANGE=angle_range, ELL_FACTOR=ell_factor
  
  IF N_Elements(npts) EQ 0 THEN npts = 100
  if N_ELEMENTS(angle_range) eq 0 then begin 
    delta_ang = 2 * !PI
    start_ang = 0.
  endif else begin
    delta_ang = max(angle_range) - min(angle_range)
    start_ang = min(angle_range) - !PI/2.
  endelse
  
  rx = 1.
  ry = 1.
  
  if N_ELEMENTS(ell_factor) ne 0 then begin
    if ell_factor gt 1. then begin
     rx = 1./ell_factor
     ry = 1.
    endif else begin
     rx = 1.
     ry = 1.*ell_factor
    endelse
  endif
  
  points = (delta_ang / (npts-1)) * findgen(npts) + start_ang
  x = xcenter + radius * Cos(points) * rx
  y = ycenter - radius * Sin(points) * ry
  
  RETURN, Transpose([[x],[y]])
  
END

pro w_windrose_addlegend, leginfo, TITLE=title, UNIT=unit, WINDOW=window, ADDCMD=addcmd, CHARSIZE=charsize, POSITION=position, FORMAT=format

  if N_ELEMENTS(FORMAT) eq 0 then FORMAT = '(G0)'
  
  w_gr_Colorbar, leginfo, POSITION=position, WINDOW=window, ADDCMD=addcmd, $
    CHARSIZE=charsize, /VERTICAL, FORMAT=format
  IF N_ELEMENTS(TITLE) eq 0 then bar_title='Wind Speed' else bar_title=title
  cgText, (position[0]+position[2])/2., position[3]+0.065, bar_title, ALIGNMENT=0.35, COLOR=cgColor('BLACK'), $
    WINDOW=window, ADDCMD=addcmd, /NORMAL, CHARSIZE=charsize
  IF N_ELEMENTS(UNIT) eq 0 then bar_title='(m.s!u-1!n)' else bar_title=unit
  cgText, (position[0]+position[2])/2., position[3]+0.035, bar_title, ALIGNMENT=0.35, COLOR=cgColor('BLACK'), $
    WINDOW=window, ADDCMD=addcmd, /NORMAL, CHARSIZE=charsize
    
end


;+
; :Description:
;    Makes a Wind rose distribution plot. If a second variable is provided, it also computes 
;    and color-displays the distribution of this new variable within the direction bins. 
;
; :Private:
;
; :Params:
;    wind_dir: in, required
;              the wind directions (MET convention, from 0 to 360 deg)
;    wind_speed: in, required
;                array of the same dimension as wind_dir.
;
; :Keywords:
;    ADD_VAR: in, optional, Default=wind_speed
;             use ADD_VAR to specify a variable to vizualize as colored
;             distribution within each direction bin. Default behavior
;             is to set this keyword automaticaly to `wind_speed`. Set 
;             `NO_WS` to prevent this behavior and plot only the direction
;             distribution.
;    N_BINS: in, optional, Default=16
;            the number of direction bins. Must be divisible by 4
;    TITLE: in, optional, Default=''
;           The plot title
;    TICKS_ANGLE: in, optional, default=1
;                 counter clockwize angle of the circle ticknames, in 25 deg intervals.
;                 Ranges from 0 to 7.
;                 TICKS_ANGLE=1 and TICKS_ANGLE=7 should suit to most of the situations.
;    LEVELS: in, optional
;               default behavior is to compute levels of the second var (see 'ADD_VAR')
;               automatically. To force a constant scale , you may want to set this 
;               keyword to an array of N+1 levels providing the desired intervals,
;                e.g. [0,4,5,6,7,8,12]
;    OOB_TOP_COLOR: in, optional
;                   Set this keyword (/OOB_TOP_COLOR) to draw an OOB arrow.
;                   Set this keyword to a string for an OOB color of your choice
;    OOB_BOT_COLOR: in, optional
;                   Set this keyword (/OOB_BOT_COLOR) to draw an OOB arrow.
;                   Set this keyword to a string for an OOB color of your choice
;    DEF_CALM: in, optional, default=0.
;              use DEF_CALM to specify the wind speed threshold below which winds are 
;              considered as "calm" and the values removed from the wind rose. If the 
;              percentage o calm winds exceeds 0%, this value is indicated on the
;              bottom of the plot (see `CALM_LEGEND`)
;    MAX_RADIUS: in, optional, default = 0.4
;                in normal coordinates, the radius of the largest circle
;    CENTER: in, optional
;            in normal coordinates, the position of the center of the plot
;    MAX_PERC: in, optional
;              Fix the circle axes to a given scale fixed by this max value.
;    NO_WS: in, optional, default=0
;           ignore the second variable and make a simple plot of the wind direction distribution
;    GS: in, optional
;        set this keyword to creat gray scale plots
;    FORMAT: in, optional
;            color bar labels stirng format (e.g: '(F4.2)')
;    CHARSIZE: in, optional
;              character size
;    CALM_LEGEND: in, optional
;                 default behavior is to indicate the percentage of calm winds only
;                 if this percentage exceeds 0%. Set this keyword to 1 to force the
;                 indication of this information and to 0 to prevent plotting the
;                 legend
;    LEGINFO: out
;             for the colorbar
;    CALM_PERC: out
;               for the calm winds legend
;                           
;
;-
pro w_WindRose_addrose, wind_dir, wind_speed,  $
    ADD_VAR=add_var, $
    N_BINS=n_bins, $
    TITLE=title, $
    GS=gs, $
    TICKS_ANGLE=ticks_angle, $
    LEVELS=levels, $
    OOB_BOT_COLOR=oob_bot_color, $
    OOB_TOP_COLOR=oob_top_color, $
    DEF_CALM=def_calm, $
    MAX_PERC=max_perc, $
    NO_WS=no_ws, $
    MAX_RADIUS=max_radius,  $
    CENTER=center, $
    CHARSIZE=charsize, $
    WINDOW=window,  $
    ADDCMD=addcmd,  $
    LEGINFO=leginfo, $ ; OUT
    CALM_PERC=calm_perc ; OUT
                
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
;  ON_ERROR, 2
    
  if N_ELEMENTS(N_BINS) eq 0 then nbins = 16L else nbins = LONG(n_bins)
  if N_ELEMENTS(TICKS_ANGLE) eq 0 then ticks_angle = 1
  if N_ELEMENTS(ADD_VAR) eq 0 then _var2 = wind_speed else _var2 = add_var
  if N_ELEMENTS(DEF_CALM) eq 0 then _def_calm = 0. else _def_calm = DEF_CALM
  
  wid = cgQuery(/CURRENT, COUNT=cnt)
  if cnt eq 0 then begin 
    _wf = FLOAT(!D.X_Size)/!D.Y_Size
  endif else begin
    dims = SIZE(cgSnapshot(WID=wid), /DIMENSIONS)
    if N_ELEMENTS(dims) eq 3 then _wf = FLOAT(dims[1])/dims[2] else _wf = FLOAT(dims[0])/dims[1]
  endelse
    
  if _wf gt 1. then begin
    rx = 1./_wf
    ry = 1.
  endif else begin
    rx = 1.
    ry = 1.*_wf
  endelse
  if N_ELEMENTS(MAX_RADIUS) eq 0 then max_radius = 0.4
  if N_ELEMENTS(CHARSIZE) eq 0 then _charsize = 1. else _charsize = CHARSIZE
  if N_ELEMENTS(CENTER) eq 0 then begin
    xcenter = 0.5
    ycenter = 0.5
  endif else begin
    xcenter = CENTER[0]
    ycenter = CENTER[1]
  endelse
  
  do_var2 = (N_ELEMENTS(_var2) ne 0) and (~KEYWORD_SET(NO_WS))
  
  if ~array_processing(wind_dir, wind_speed, _var2, REP_A0=_wd, REP_A1=_ws) then message, WAVE_Std_Message(/ARG)

  ; Extract valid data
  p = where(finite(_wd) and finite(_ws) and finite(_var2), cnt)
  if cnt eq 0 then MESSAGE, WAVE_Std_Message(/ARG)
  _wd = _wd[p]
  _ws = _ws[p]
  _var2 = _var2[p]
  p = where(_wd gt -999 and _ws gt -999 and _var2 gt -999, cnt)
  if cnt eq 0 then MESSAGE, WAVE_Std_Message(/ARG)
  _wd = _wd[p]
  _ws = _ws[p]
  _var2 = _var2[p]
 
  if nbins/4. ne nbins/4 then message, WAVE_Std_Message('N_BINS', /ARG)
    
  ; Find the calms
  ntot = N_ELEMENTS(_wd)
  p = where(_ws gt _def_calm, nel)
  if nel eq 0 then message, 'Only calm winds?'
  _wd = _wd[p]
  _var2 = _var2[p]
  nc = ntot-nel
  calm_perc = float(nc)/ntot * 100.
  
  ; Bins
  bsize = 360. / (nbins)
  bmin = 0. - bsize/2.
  ; Ensure that all values will fall into the bins
  _wd =  (FLOAT(_wd) MOD (360))
  ptoshift = where(_wd gt (360. - bsize/2. - (MACHAR()).eps), cnttoshift)
  if cnttoshift ne 0 then _wd[ptoshift] = _wd[ptoshift] - 360. + (MACHAR()).eps ; Razor edges

  
  ;Distribution
  h = HISTOGRAM(_wd, MIN=bmin, BINSIZE=bsize, NBINS=nbins, LOCATIONS=locs, REVERSE_INDICES=r)
  ;Check
  if TOTAL(h) ne nel then message, 'Angle Razor edge'
  
  ;Locations in Radians for plotting later
  locs_rad = [locs, 2*locs[nbins-1] - locs[nbins-2]] * 2. * !PI / 360.
  perc = (float(h) / ntot) * 100.
  
  ;***********************
  ; Axis scale and circles
  ;***********************
  percstep = 5
  if KEYWORD_SET(MAX_PERC) then _max = max_perc else _max = MAX(perc)
  maxscale = CEIL(_max / percstep) * percstep
  npercsteps = maxscale / (percstep)
  if npercsteps gt 5 then step_OK = FALSE else step_OK = TRUE
  while ~step_OK do begin
    percstep += 5
    maxscale = CEIL(_max / percstep) * percstep
    npercsteps = maxscale / percstep
    if npercsteps gt 5 then step_OK = FALSE else step_OK = TRUE
  endwhile
  percsteps = [0., (INDGEN(npercsteps)+1)*percstep]
  
  ;*****************
  ; Var2 scale
  ;*****************
  if do_var2 then begin
    if N_ELEMENTS(LEVELS) eq 0 then begin
      wstep = 1
      wmaxscale = CEIL((MAX(_var2)-MIN(_var2)) / wstep) * wstep
      nwsteps = wmaxscale / (wstep)
      if nwsteps gt 8 then step_OK = FALSE else step_OK = TRUE
      while ~step_OK do begin
        wstep += 1
        wmaxscale = CEIL((MAX(_var2)-MIN(_var2)) / wstep) * wstep
        nwsteps = wmaxscale / (wstep)
        if nwsteps gt 6 then step_OK = FALSE else step_OK = TRUE
      endwhile
      wsteps = FLOOR(MIN(_var2)) + [0., (INDGEN(nwsteps)+1)*wstep]
    endif else begin
      wsteps = LEVELS
    endelse
    
  endif
  
  ;Draw
  step_radius = max_radius/npercsteps
  npts = 361 ; Circles number of points
 
  ;************
  ; Camemberts
  ;************
  
  ; Check for all data to set top/bot arrows 
  _dinfo = w_gr_DataLevels(_var2, LEVELS=wsteps, OOB_BOT_COLOR=oob_bot_color, OOB_TOP_COLOR=oob_top_color) 
  if _dinfo.is_ootop eq 1 and N_ELEMENTS(OOB_TOP_COLOR) eq 0 then oob_top_color = 1
  if _dinfo.is_oobot eq 1 and N_ELEMENTS(OOB_BOT_COLOR) eq 0 then oob_bot_color = 1
  
  ;First, do the colors if needed, other things will be over plotted anyway
  if do_var2 then begin
    TVLCT, rr, gg, bb, /GET
    if KEYWORD_SET(GS) then CGLOADCT, 0 else CGLOADCT, 34
    for i=0, nbins-1 do begin
      radius = perc[i] * max_radius /  maxscale
      if (R[i] gt R[i+1]-1) then continue
      ws=_var2[R[R[i] : R[i+1]-1]]       
      leginfo = w_gr_DataLevels(ws, LEVELS=wsteps, OOB_BOT_COLOR=oob_bot_color, OOB_TOP_COLOR=oob_top_color)     
      wh = leginfo.histo
      percw = (float(wh) /  TOTAL(wh))
      nl = N_ELEMENTS(leginfo.colors)
      for j=0, nl-1 do begin
        wradius = TOTAL(percw[0:nl-1-j]) * radius
        wcir = w_windrose_circle(xcenter, ycenter, wradius, nPts=npts, ANGLE_RANGE=[locs_rad[i],locs_rad[i+1]], ELL_FACTOR=_wf)
        rrr = KEYWORD_SET(WINDOW) or KEYWORD_SET(addcmd)
        cgColorFill, [reform(wcir[0,*]), xcenter], [reform(wcir[1,*]), ycenter], WINDOW=rrr or KEYWORD_SET(addcmd), COLOR=leginfo.colors[nl-1-j], /NORMAL
        cgPlotS, wcir, WINDOW=window, ADDCMD=addcmd, /NORMAL
      endfor
    endfor
    TVLCT, rr, gg, bb
  endif
  
  ; Cake peaces
  for i=0, nbins-1 do begin
    radius = perc[i] * max_radius /  maxscale
    cir = w_windrose_circle(xcenter, ycenter, radius, nPts=npts, ANGLE_RANGE=[locs_rad[i],locs_rad[i+1]], ELL_FACTOR=_wf)
    cgPlotS, [reform(cir[0,*]),xcenter,cir[0,1]], [reform(cir[1,*]), ycenter,cir[1,1]], WINDOW=window, ADDCMD=addcmd, /NORMAL
  endfor

  ; Axes Circles
  for i=1, npercsteps do cgPlotS, w_windrose_circle(xcenter, ycenter, step_radius*i, nPts=361, ELL_FACTOR=_wf), WINDOW=window, ADDCMD=addcmd, LINESTYLE = 1, COLOR=cgColor('dark grey'), /NORMAL
  
  ; Axes
  x_radius = max_radius*rx
  y_radius = max_radius*ry
  as = _charsize * 1.
  delta_x = as * rx * 0.005
  delta_y = as * ry * 0.02
  cgPlotS, [xcenter-x_radius, xcenter+x_radius], [ycenter,ycenter], WINDOW=window, ADDCMD=addcmd, LINESTYLE = 1, COLOR=cgColor('dark grey'), /NORMAL
  cgPlotS, [xcenter, xcenter], [ycenter-y_radius,ycenter+y_radius], WINDOW=window, ADDCMD=addcmd, LINESTYLE = 1, COLOR=cgColor('dark grey'), /NORMAL
  cgText, xcenter+delta_x, ycenter+y_radius-delta_y, 'N', COLOR=cgColor('dark grey'), WINDOW=window, ADDCMD=addcmd, /NORMAL, CHARSIZE=as
  cgText, xcenter+x_radius-delta_x, ycenter-delta_y, 'E', COLOR=cgColor('dark grey'), WINDOW=window, ADDCMD=addcmd, ALIGNMENT=1., /NORMAL, CHARSIZE=as
  cgText, xcenter+delta_x, ycenter-y_radius+delta_x, 'S', COLOR=cgColor('dark grey'), WINDOW=window, ADDCMD=addcmd, /NORMAL, CHARSIZE=as
  cgText, xcenter-x_radius+delta_x, ycenter-delta_y, 'W', COLOR=cgColor('dark grey'), WINDOW=window, ADDCMD=addcmd, /NORMAL, CHARSIZE=as
  
  ; Ticks
  case (ticks_angle) of
    0: angle_ticks = 4. * !PI / 8.
    1: angle_ticks = 2. * !PI / 8.
    2: angle_ticks = 0
    3: angle_ticks = - 2. * !PI / 8.
    4: angle_ticks = - 4. * !PI / 8.
    5: angle_ticks = - 6. * !PI / 8.
    6: angle_ticks = - 8. * !PI / 8.
    7: angle_ticks = - 10. * !PI / 8.
    else: message, WAVE_Std_Message('ticks_angle', /RANGE)
  endcase
  for i=1, npercsteps do begin
    xt = xcenter + Cos(angle_ticks) * rx * step_radius*i
    yt = ycenter + Sin(angle_ticks) * ry * step_radius*i
    cgText,xt, yt, str_equiv(STRING(percsteps[i], FORMAT='(I3)')+'%') ,WINDOW=window, ADDCMD=addcmd, COLOR=cgColor('dark grey'), /NORMAL, CHARSIZE=_charsize
  endfor
  
  ; White center circle
  cir = w_windrose_circle(xcenter, ycenter, step_radius/10., nPts=361, ELL_FACTOR=_wf)
  rrr = KEYWORD_SET(WINDOW) or KEYWORD_SET(addcmd)   
  cgColorFill, cir[0,*], cir[1,*], WINDOW=rrr, COLOR=cgColor('white'), /NORMAL
  cgPlotS, cir, WINDOW=window, ADDCMD=addcmd, /NORMAL
    
  ; Title
  if N_ELEMENTS(TITLE) eq 0 then title = ''
  cgText,xcenter, ycenter + max_radius + 0.05, title, WINDOW=window, ADDCMD=addcmd, ALIGNMENT = 0.5, CHARSIZE=1.3*_charsize, /NORMAL
  
  if ARG_PRESENT(LEVELS) and ~ KEYWORD_SET(NO_WS) then levels = wsteps
    
end

;+
; :Description:
;    Makes a Wind rose distribution plot. If a second variable is provided, it also computes 
;    and color-displays the distribution of this new variable within the direction bins. 
;
; :Params:
;    wind_dir: in, required
;              the wind directions (MET convention, from 0 to 360 deg)
;    wind_speed: in, required
;                array of the same dimension as wind_dir.
;
; :Keywords:
;    ADD_VAR: in, optional, Default=wind_speed
;             use ADD_VAR to specify a variable to vizualize as colored
;             distribution within each direction bin. Default behavior
;             is to set this keyword automaticaly to `wind_speed`. Set 
;             `NO_WS` to prevent this behavior and plot only the direction
;             distribution.
;    N_BINS: in, optional, Default=16
;            the number of direction bins. Must be divisible by 4
;    TITLE: in, optional, Default=''
;           The plot title
;    TICKS_ANGLE: in, optional, default=1
;                 counter clockwize angle of the circle ticknames, in 25 deg intervals.
;                 Ranges from 0 to 7.
;                 TICKS_ANGLE=1 and TICKS_ANGLE=7 should suit to most of the situations.
;    LEVELS: in, optional
;               default behavior is to compute levels of the second var (see 'ADD_VAR')
;               automatically. To force a constant scale , you may want to set this 
;               keyword to an array of N+1 levels providing the desired intervals,
;                e.g. [0,4,5,6,7,8,12]
;    OOB_TOP_COLOR: in, optional
;                   Set this keyword (/OOB_TOP_COLOR) to draw an OOB arrow.
;                   Set this keyword to a string for an OOB color of your choice
;    OOB_BOT_COLOR: in, optional
;                   Set this keyword (/OOB_BOT_COLOR) to draw an OOB arrow.
;                   Set this keyword to a string for an OOB color of your choice
;    VAR_TITLE: in, optional, type=string
;               the title of the added variable legend
;    VAR_UNITS: in, optional, type=string
;               the units of the added variable
;    DEF_CALM: in, optional, default=0.
;              use DEF_CALM to specify the wind speed threshold below which winds are 
;              considered as "calm" and the values removed from the wind rose. If the 
;              percentage o calm winds exceeds 0%, this value is indicated on the
;              bottom of the plot (see `CALM_LEGEND`)
;    MAX_PERC: in, optional
;              Fix the circle axes to a given scale fixed by this max value.
;    NO_LEGEND: in, optional, default=0
;               do not draw legend for wind speed
;    NO_WS: in, optional, default=0
;           ignore the second variable and make a simple plot of the wind direction distribution
;    PNG: in, optional
;         path of the output img file
;    IM_RESIZE: in, optional
;               Size of the png. for png only
;    EPS: in, optional
;         path of the output img file
;    STD_PNG: in, optional
;              path of the output img file   
;    GS: in, optional
;        set this keyword to creat gray scale plots
;    FORMAT: in, optional
;            color bar labels stirng format (e.g: '(F4.2)')
;    CHARSIZE: in, optional
;              character size
;    CALM_LEGEND: in, optional
;                 default behavior is to indicate the percentage of calm winds only
;                 if this percentage exceeds 0%. Set this keyword to 1 to force the
;                 indication of this information and to 0 to prevent plotting the
;                 legend
;
;-
pro w_WindRose, wind_dir, wind_speed, $
    ADD_VAR=add_var, $
    N_BINS=n_bins, $
    TITLE=title, $
    TICKS_ANGLE=ticks_angle, $
    LEVELS=levels, $
    OOB_BOT_COLOR=oob_bot_color, $
    OOB_TOP_COLOR=oob_top_color, $
    VAR_TITLE=var_title, $
    VAR_UNITS=var_units, $
    DEF_CALM=def_calm, $
    MAX_PERC=max_perc, $
    NO_LEGEND=no_legend, $
    NO_WS=no_ws, $
    PNG=png, $
    EPS=eps, $
    STD_PNG=std_png, $
    IM_RESIZE=im_resize, $
    GS=gs, $
    FORMAT=format, $
    CHARSIZE=charsize, $
    CALM_LEGEND=calm_legend

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  ; Window
  xsiz = 650
  ysiz = 600
  cgDisplay, XSIZE=xsiz, YSIZE=ysiz, /PIXMAP, /FREE, /WINDOW
  wd = !D.WINDOW
  
  window = 0
  if N_ELEMENTS(png) ne 0 then begin
    PS_Start, FILENAME=png, /DECOMPOSED, /LAND
  endif else if N_ELEMENTS(EPS) ne 0 then begin
    PS_Start, FILENAME=eps, /DECOMPOSED, /LAND
  endif else if N_ELEMENTS(std_png) ne 0 then begin
     
  endif else window = 1
  
  if window then begin
    cgWindow, WXSIZE=xsiz, WYSIZE=ysiz, Title='w_WindRose'
    cgControl, EXECUTE=0
  endif
  
  w_WindRose_addrose, wind_dir, wind_speed, ADD_VAR=add_var, $
    N_BINS=n_bins, $
    TITLE=title, $
    LEVELS=levels, $
    MAX_PERC=max_perc, $
    DEF_CALM=def_calm, $
    NO_WS=no_ws, $
    ADDCMD=window, $
    MAX_RADIUS=max_radius, $
    CENTER=[0.45,0.5], $
    CHARSIZE=charsize, $
    TICKS_ANGLE=ticks_angle, $
    GS=gs, $
    LEGINFO=leginfo, $
    CALM_PERC=calm_perc, $
    OOB_BOT_COLOR=oob_bot_color, $
    OOB_TOP_COLOR=oob_top_color
  
  if N_ELEMENTS(ADD_VAR) eq 0 then _var2 = wind_speed else _var2 = add_var
  do_var2 = (N_ELEMENTS(_var2) ne 0) and (~KEYWORD_SET(NO_WS))
  do_legend = ~ KEYWORD_SET(NO_LEGEND)
  
  ; Var2 legend
  if do_var2 and do_legend then begin
    pbar = [0.87, 0.01, 0.9, 0.20]
    w_windrose_addlegend, leginfo, TITLE=VAR_TITLE, UNIT=VAR_UNITS, ADDCMD=window, CHARSIZE=charsize, FORMAT=format, POSITION=pbar
  endif
  
  ; Calm percentage
  if N_ELEMENTS(CALM_LEGEND) eq 0 then calm_legend = -1  
  do_legend = calm_legend eq 1
  if ~do_legend then do_legend = calm_perc gt 0 and calm_legend ne 0
  if do_legend ne 0 then begin
   text = 'Calm winds: ' + STRING(calm_perc, FORMAT='(F5.2)') + '%'
   cgText, 0.02,0.02, text, COLOR=cgColor('dark grey'), ADDCMD=window, /NORMAL, CHARSIZE=_charsize
  endif
  
  ; Stop
  if WINDOW then begin
    cgControl, EXECUTE=1
    if KEYWORD_SET(PNG) then cgControl, CREATE_PNG=png, IM_RESIZE=75, /IM_RASTER
    if KEYWORD_SET(EPS) then cgControl, CREATE_PS=eps, /PS_ENCAPSULATED, /PS_METRIC
    if KEYWORD_SET(STD_PNG) then cgControl, CREATE_PNG=std_png, IM_RASTER=0
  endif
  
  if N_ELEMENTS(png) ne 0 then begin
    PS_End, /PNG, RESIZE=im_resize
  endif else if N_ELEMENTS(EPS) ne 0 then begin
    PS_End
  endif else if N_ELEMENTS(std_png) ne 0 then begin
    write_png, std_png, tvrd(TRUE=1)
  endif
  
  WDELETE, wd

end