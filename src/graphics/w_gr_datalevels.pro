;+
; 
; The purpose of this function is to provide a generic tool to define
; data levels and associate the right color to the right value for a plot 
; (like contour, image, or coloured scatter plot for example).
; A nice by-product of this routine is the possibility to visualise 
; your data on the display and have access to some easy statistics.
; 
; The output of `w_gr_data_levels` is a structure containing all necessary 
; information to generate a colored plot and/or a colorbar. It should be used
; in combination with w_gr_image, w_gr_contour, w_gr_dcbar, w_gr_colorbar.
; 
; Here its most important tags::
;   
;     info.levels: array of the data levels that the routine chose for you or
;                  that you specified by yourself
;     info.colors: array of the colors that the routine chose for you
;                  colors are LONGS, that means that you should work
;                  in decompes mode (one should always work in decomposed mode)
;                  If you want the colors in RGB, do rgb = w_gr_ColorToRGB(info.colors)
;     info.loc: integer array of the same dimesnions as DATA. Each element of
;               info.loc corresponds to an index in info.colors 
;               (info.colors[info.loc[0]] is simply the color assigned to the
;               first element in your data)
;     ...
;       
; 
; There are basically two ways to assign a color to a data value:
; (1) the data is of integer type and one color can be attributed to each value (/DCBar case); 
; (2) the data value falls into an interval of two levels (default)
; 
; In the default case, a value can fall into one of the intervals [lev(i),lev(i+1)[ 
; (last bound excluded) or fall into the two Out Of Bounds (OOB) categories:
; - OOB Bottom: value is smaller than the lowest level
; - OOB Top: value is larger then the highest level
; When displayed using a colorbar, these colors will have to be represented with 
; arrows.
; 
; The last data category to handle is "Missing data". If this category is present
; in the dataset, one has to assign one color to it (default is grey).
; 
; Things become complicated when you need to display this data as an image, for
; example. The standard way to display data is to use cgImage, which uses the IDL
; TV command, which uses the current color table, which contains max 256 colors.
; In the easiest case where your dataset has no OOB values and no missing data,
; the 256 colors can be taken from the colortable. In all other cases, the 
; number of available colors is reduced. The main purpose of `w_gr_data_levels`
; is to handle those cases automatically, and to help you to assign the right
; color to the right value.
; 
; :Categories:
;    Graphics
;    
; :Examples:
; 
;    Overplot points with a WAVE colortable:: 
;      data = cgDemoData(1)
;      time = cgScaleVector(Findgen(101), 0, 6)  
;      cgPlot, time, data, POSITION=[0.1,0.1,0.7,0.85]
;      w_loadct, 'humidity'
;      info = w_gr_DataLevels(data)
;      colors = info.colors[info.loc]
;      for i=0, N_ELEMENTS(data)-1 do cgPlotS, time[i], data[i], PSYM=-16, color=colors[i], SYMSIZE=2
;      w_gr_Colorbar, info, /FIT, TITLE='Humidity'
;      
;    Have a look at your data:: 
;      w_LoadCT, 'wiki-schwarzwald-cont'
;      info = w_gr_DataLevels(cgDemoData(7), /SHOW)
;         
;    
; :History:
;     Written by FaM, 2012.
;    
;-

;+
; :Private:
; 
; :Description:
;    Makes a nice graphical overview of the content of a info structure
;
; :Params:
;    info: in, required
;          the output of `w_gr_DataLevels`
;
;-
pro w_gr_DataLevels_show, info

  @WAVE.inc
  compile_opt idl2

  if info.is_Data then begin
  
    ;Console out
    print, '% Data info'
    print, ' Data type: ' + info.dataTypeName
    print, ' Data n elements: ' + str_equiv(info.dataNel)
    if info.is_Missing then begin
      print, ' Missing value: ' + str_equiv(info.MISSING_VALUE)
      print, ' Missing elements: ' + str_equiv(info.nmissing)
    endif else print, ' No missing values'
    print, ' Data min: ', info.DATA_MIN
    print, ' Data max: ', info.DATA_MAX
    
    ;Histogram
    if info.dcbar then begin
      labels = cgNumber_Formatter(info.levels)
      if info.is_Missing then labels = ['Miss', labels]
    endif else begin
      labels = cgNumber_Formatter(info.levels[0:N_ELEMENTS(info.levels)-2]) + ' to ' + cgNumber_Formatter(info.levels[1:*])
      if info.is_ooBotColor then labels = ['< '+ cgNumber_Formatter(info.levels[0]), labels]
      if info.is_ooTopColor then labels = [labels, '> '+cgNumber_Formatter(MAX(info.levels))]
      if info.is_Missing then labels = ['Missing', labels]
    endelse
    if N_ELEMENTS(labels) le 60 then begin
      pwhite = where(info.colors eq cgColor('White', /DECOMPOSED), nwhite)
      if nwhite ne 0 then background=cgColor('light yellow', /DECOMPOSED)
      cgWindow, WXSIZE=700, WYSIZE=500
      cgBarPlot, info.histo, /WINDOW, COLORS=info.colors, XTITLE='', BACKGROUND=background, $
        BARNAMES=labels, /ROTATE, POSITION=[0.4,0.1,0.9,0.87], $
        TITLE='Levels distribution (NLEVELS= '+str_equiv(N_ELEMENTS(info.levels))+')'
    endif else begin
      TVLCT, rr, gg, bb, /GET
      TVLCT, w_gr_ColorToRGB(info.colors)
      cgWindow, WXSIZE=700, WYSIZE=500
      cgHistoplot, info.loc, PSYM=10, /WINDOW, BINSIZE=1, MINinput=0, /FILLPOLYGON, $
        POLYCOLOR=INDGEN(N_ELEMENTS(info.colors)), MAXinput=N_ELEMENTS(info.colors)-1, $
        datacolorname = 'black'
      TVLCT, rr, gg, bb
    endelse
    
    ;Nice plot of the data
    cgWindow, WXSIZE=700, WYSIZE=500
    if info.dataNdims eq 2 then begin ; Image case
      w_gr_image, info, /WINDOW, $
        MINUS_ONE=0, /AXIS, TITLE='Data image', MARGIN=0.2
    endif else begin
      d = CEIL(SQRT(info.dataNel))
      img = LONARR(d,d)
      img[0:info.dataNel-1] = info.loc
      cgImage, img, PALETTE=w_gr_ColorToRGB(info.colors), /WINDOW, $
        MINUS_ONE=0, /AXIS, TITLE='Data (reformed)', /NOERASE, MARGIN=0.2
      if N_ELEMENTS(img) gt info.dataNel then begin ; put a cross where no data
        inds = ARRAY_INDICES(img, INDGEN(N_ELEMENTS(img)-info.dataNel) + info.dataNel)
        cgPlotS, inds[0,*]+0.5, inds[1,*]+0.5, PSYM=34, /DATA, /ADDCMD, SYMSIZE=1.5
      endif
    endelse
    if info.dcbar then w_gr_DCBar, info, /FIT, /ADDCMD else w_gr_Colorbar, info, /FIT, /ADDCMD
  endif else begin
    cgWindow, WXSIZE=300, WYSIZE=600
    if info.dcbar then w_gr_DCBar, info, /ADDCMD, POSITION=[0.02,0.02,0.2,0.98] else w_gr_Colorbar, info, /ADDCMD, POSITION=[0.02,0.02,0.2,0.98]
  endelse
  
end

;+
; :Private:
; 
; :Description:
;    Makes a nice graphical overview of the content of a info structure
;
; :Params:
;    info: in, required
;          the output of `w_gr_DataLevels`
;
;-
function w_gr_DataLevels_dataloc, info, data

  @WAVE.inc
  compile_opt idl2
  
  if N_ELEMENTS(INFO.levels) lt 2 or info.nValid eq 0 then begin
    loc = LONARR(SIZE(data, /DIMENSIONS)) - 1
  endif else begin 
    loc = LONARR(SIZE(data, /DIMENSIONS)) - 1
    loc[info.pValid] = VALUE_LOCATE(info.levels, data[info.pValid])  
  endelse
  
  ; Some temporary checks
  p_ooBot = where(info.valid and loc lt 0, cnt_ooBot)
  if cnt_ooBot ne 0 and ~ info.is_ooBot then begin
   Message, 'Internal error by OO_Bot. VALUE_LOCATE ist schuld. Sollte nicht zu schlimm sein, aber zeig es mal Fabi, falls sowas passiert.', /INFORMATIONAL
  endif
  p_ooTop = where(loc ge (N_ELEMENTS(info.levels)-1), cnt_ooTop)  
  if cnt_ooTop ne 0 and ~ info.is_ooTop then begin
    ; check for the "on the bound" case 
    ponthbound = where(ABS(data-Max(info.levels)) le info.epsilon, cnton)
    if cnton ne cnt_ooTop then begin
      Message, 'Internal error by OO_Top. VALUE_LOCATE ist schuld. Sollte nicht zu schlimm sein, aber zeig es mal Fabi, falls sowas passiert.', /INFORMATIONAL
    endif
    if ~ info.dcbar then loc[p_ooTop] = N_ELEMENTS(info.levels)-2
  endif
    
  if info.is_ooBotColor then loc += 1
  if info.is_Missing then begin 
    loc+=1
    loc[info.pmissing] = 0
  endif 
  
  ;statistics
  h = HISTOGRAM(loc, BINSIZE=1, MIN=0, MAX=N_ELEMENTS(info.colors)-1)
  
  return, CREATE_STRUCT('loc', loc, 'histo', h, info)

end

;+
; :Description:
;    The purpose of this function is to provide a generic tool to define
;    data levels and associate the right color to the right value for a plot 
;
; :Params:
;    data: in, optional
;          the data to assign colors to
;
; :Keywords:
;    LEVELS: in, optional
;            an array of level values            
;    N_LEVELS: in, optional, default=max possible
;              the number of levels to define (ignored if LEVELS is set)     
;    TABLE_SIZE: in, optional, default=!D.TABLE_SIZE
;                the size of the color table that is currently loaded in IDL    
;    COLORS: in, optional
;            an array of N_LEVELS colors to use (for the /DCBAR case)
;            or N_LEVELS+1 colors to use (for the normal case)          
;    NEUTRAL_COLOR: in, optional, default='grey'
;                   the color to assign to missing values
;    MISSING: in, optional, default=NaN
;             the missing value
;    MIN_VALUE: in, optional, default=min(data)
;               the minimal level to consider when generating levels (ignored if LEVELS is set)
;    MAX_VALUE: in, optional, default=max(data)
;               the maximal level to consider when generating levels (ignored if LEVELS is set)
;    EPSILON: in, optional, default=machar().eps
;             the epsilon value when checking for missing or OOB data
;    HIST_EQUAL: in, optional, default=0
;                to decide the levels so that each "bin" between levels
;                contains as many elements as the others.
;    SIGMA: in, optional, default=0
;           min and max values are decided as follows: [mean-p1*sigma,mean+p2*sigma]
;           where mean is the mean value and sigma the standard deviation of the
;           data. Set sigma=1 to set p1 and p2 to 1, sigma=2 set p1 and p2 to 2,
;           sigma=[2,1] to set p1 to 2 and p2 to 1, etc. 
;    CMIN: in, optional, default=0
;          the index where to start in the color table 
;          (usefull if the first colors are too dark for example)
;    CMAX: in, optional, default=TABLE_SIZE-1
;          the index where to end in the color table 
;          (usefull if the last colors are too dark for example)
;    CENTER_LEV: in, optional
;                set to the index of the level which has to be at the center of the 
;                colortable (e.g. if your levels are [-100, -50, 0, 50, 100, 200, 300]
;                set CENTER_LEV=2). 
;                This is quite useful when you have diverging color tables
;                and more levels on one side than on the other
;    CENTER_RANGE: in, optional
;                  set to an integer (e.g. 20) to define the color strength around the
;                  center level, which is otherwise white on both sides
;    INVERTCOLORS: in, optional, type=boolean, default=0
;                  If this keyword is set, the color table vectors are reversed.
;    OOB_TOP_ARROW: in, optional, Default=1
;                   Set this keyword to 0 to stop drawing an OOB arrow.
;    OOB_TOP_COLOR: in, optional, Default=0
;                   Set this keyword (/OOB_TOP_COLOR) to force an OOB arrow color 
;                   (this color will be taken from the current color table).
;                   Set this keyword to a string for an OOB color of your choice
;    OOB_BOT_ARROW: in, optional, Default=1
;                   Set this keyword to 0 to stop drawing an OOB arrow.
;    OOB_BOT_COLOR: in, optional, Default=0
;                   Set this keyword (/OOB_BOT_COLOR) to force an OOB arrow color.
;                   (this color will be taken from the current color table).
;                   Set this keyword to a string for an OOB color of your choice
;    DCBAR: in, optional, type=boolean, default=0
;           Set this keyword to make a DCBar (categorical level values)
;    DOUBLE: in, optional, type=boolean, default=0
;            Set this keyword to transform the data to double before computing the levels
;    SHOW: in, optional, type=boolean, default=0
;          Set this keyword to visualize your data
;
; :Returns:
;    A structure containing all necessary information to do your plot
;-
function w_gr_DataLevels, data, $
    LEVELS=levels, $
    N_LEVELS=n_levels, $
    TABLE_SIZE=table_size, $
    COLORS=colors, $
    NEUTRAL_COLOR=neutral_color, $
    MISSING=missing, $
    MIN_VALUE=min_value, $
    MAX_VALUE=max_value, $
    EPSILON=epsilon, $
    SIGMA=sigma, $
    HIST_EQUAL=hist_equal, $
    CMIN=cmin, $ 
    CMAX=cmax, $
    CENTER_LEV=center_lev, $
    CENTER_RANGE=center_range, $
    INVERTCOLORS=invertcolors, $
    OOB_TOP_COLOR=oob_top_color, $ 
    OOB_BOT_COLOR=oob_bot_color, $
    OOB_TOP_ARROW=oob_top_arrow, $ 
    OOB_BOT_ARROW=oob_bot_arrow, $
    DCBAR=dcbar, $
    DOUBLE=double, $
    SHOW=show
    
  @WAVE.inc
  compile_opt idl2
  
  ON_ERROR, 2
    
  if N_ELEMENTS(data) ne 0 then is_data = TRUE else is_data = FALSE  
  
  user_Levels = N_ELEMENTS(LEVELS) ne 0 
  user_Colors = N_ELEMENTS(COLORS) ne 0  
  user_dcbar = BYTE(KEYWORD_SET(DCBAR))
  SetDefaultValue, oob_top_arrow, KEYWORD_SET(oob_top_color)
  SetDefaultValue, oob_bot_arrow, KEYWORD_SET(oob_bot_color)
  oob_top_str = arg_okay(oob_top_color, TYPE=IDL_STRING)
  oob_bot_str = arg_okay(oob_bot_color, TYPE=IDL_STRING)
  is_hist = FALSE
  
  ;Give a value to maxncolors
  if N_ELEMENTS(table_size) ne 0 then begin
    maxncolors = long(table_size)
  endif else begin
    maxncolors = !D.TABLE_SIZE
  endelse
  
  if oob_top_str and ~ oob_top_arrow then oob_top_arrow = 1
  if oob_bot_str and ~ oob_bot_arrow then oob_bot_arrow = 1
    
  if is_data then begin
    if ~arg_okay(data, /NUMERIC) then Message, WAVE_Std_Message('data', /ARG)
    _data = data ; Working copy
  endif else begin
    ; Check for strange keywords combination
    if ~user_Levels and ~ ((N_ELEMENTS(min_value) ne 0) and (N_ELEMENTS(max_value) ne 0)) then $
    Message, 'No $DATA, no $LEVELS, no $MIN and $MAX, what do you want me to do exactly?'  
    ; the user wants me to define colors/levels tables
    ; be sure the programm runs to the end by setting dummy values
    ; useless info will be discarded from the output at the end
    _data = [0.,1.] 
  endelse
  
  if KEYWORD_SET(DOUBLE) then _data = DOUBLE(_data)
    
  ; Get some info about the data
  s = SIZE(_data)
  dataNel = N_ELEMENTS(_data)
  dataNdims = s[0]
  dataType = s[N_ELEMENTS(s)-2]
  dataTypeName = Size(_data, /TNAME)
  
  ;Check for missing
  if N_ELEMENTS(MISSING) eq 0 then begin
    case dataTypeName of
      'FLOAT' : _missing = !VALUES.F_NAN
      'DOUBLE': _missing = !VALUES.D_NAN
      'BYTE': _missing = 0B
      'LONG': _missing = -9999L
      'INT': _missing = -9999
      else: Message, 'Data type too exotic for me'
    endcase
  endif else begin
    if ~ arg_okay(missing, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('MISSING', /ARG)
    _missing = missing
  endelse
  
  ;Check for epsilon
  if N_ELEMENTS(EPSILON) eq 0 then begin
    case dataTypeName of
      'FLOAT' : _epsilon = (MACHAR()).eps
      'DOUBLE': _epsilon = (MACHAR(/DOUBLE)).eps
      else: _epsilon = 0
    endcase
  endif else begin
    if ~ arg_okay(epsilon, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('EPSILON', /ARG)
    _epsilon = epsilon
  endelse

  ; First check for finite elements
  pFin = where(finite(_data), cntFin, COMPLEMENT=pNoFin, NCOMPLEMENT=cntNoFin)
    
  ; Then add the test for missing values if needed
  if finite(_missing) then begin
    if cntNoFin ne 0 then _data[pNoFin] = _missing ; just in case
    pValid = where(Abs(_data - _missing) gt _epsilon, cntValid, $
                       COMPLEMENT=pNoValid, NCOMPLEMENT=cntNoValid)
  endif else begin
    pValid = TEMPORARY(pFin)
    cntValid = TEMPORARY(cntFin)
    pNoValid = TEMPORARY(pNoFin)
    cntNoValid = TEMPORARY(cntNoFin)   
  endelse
  is_Missing = cntNoValid ne 0
  is_Valid = cntValid ne 0
  
  ; Mask
  valid = BYTARR(SIZE(_data, /DIMENSIONS))
  if is_Valid then valid[pValid] = 1B
  
  ; Ok. If sigma there then do it now
  if N_ELEMENTS(sigma) ne 0 then begin
    if N_ELEMENTS(MAX_VALUE) ne 0 or N_ELEMENTS(MIN_VALUE) ne 0 $
      then Message, 'Ambiguous combination of SIGMA with min/max values'
    if user_Levels then Message, 'Ambiguous keyword combination of LEVELS and SIGMA'
    if ~ is_data or cntValid eq 0 then Message, 'Sigma only works with real (valid) data!'
    mean_d = mean(_data[pValid])
    sig_d = stddev(_data[pValid])
    case (N_ELEMENTS(sigma)) of
      1: begin
        ps1 = sigma
        ps2 = sigma
      end
      2: begin
        ps1 = sigma[0]
        ps2 = sigma[1]
      end
      else: Message, WAVE_Std_Message('SIGMA', NELEMENTS=2)
    endcase
    min_value = mean_d - ps1 * sig_d
    max_value = mean_d + ps2 * sig_d
  endif
  
  user_Max = N_ELEMENTS(MAX_VALUE) ne 0
  user_Min = N_ELEMENTS(MIN_VALUE) ne 0
  user_MinMax = user_Max and user_Min
  if (user_Levels and user_Max) or (user_Levels and user_Min) then $
   Message, 'Ambiguous keyword combination of MIN/MAX with LEVELS.'
     
  ; Min and max
  if is_Valid then dataMin = min(_data[pValid]) else dataMin = _missing
  if is_Valid then dataMax = max(_data[pValid]) else dataMax = _missing
    
  ; Now go for the levels and check the user decisions
  if user_Levels then begin
    if ~ arg_okay(levels, /NUMERIC, /ARRAY) then Message, WAVE_Std_Message('LEVELS', /ARG)
    if is_data then begin
      ; Make the levels the same type as data
      case dataTypeName of
        'FLOAT' : _levels = FLOAT(levels)
        'DOUBLE': _levels = DOUBLE(levels)
        'BYTE': _levels = BYTE(levels)
        'LONG': _levels = LONG(levels)
        'INT': _levels = FIX(levels)
        else: Message, 'Data type too exotic for me'
      endcase
    endif else _levels = levels
  endif
  
  ; Define level max and min
  if user_Levels then begin 
    _min_level = min(_levels)
    _max_level = max(_levels)
    if _min_level eq _max_level then Message, '$LEVELS is not valid: min and max are the same'
  endif else begin
    if user_Min then begin
      if ~ arg_okay(min_value, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('MIN_VALUE', /ARG)
      _min_level = min_value
    endif else begin
      if FINITE(datamin) then _min_level = dataMin else _min_level = 0.
    endelse
    if user_Max then begin
      if ~ arg_okay(max_value, /NUMERIC, /SCALAR) then Message, WAVE_Std_Message('MAX_VALUE', /ARG)
      _max_level = max_value
    endif else begin
      if FINITE(dataMax) then _max_level = dataMax else  _max_level = 0.
    endelse    
    if user_MinMax and (_min_level eq _max_level) then Message, '$MIN_VALUE and $MAX_VALUE are not valid: min and max are the same'
  endelse
      
  ; Now check for out of bounds data
  if cntValid ne 0 then begin
    p_ooTop = where(valid and (_data gt _max_level), cnt_ooTop)
    p_ooBot = where(valid and (_data lt _min_level), cnt_ooBot)
  endif else begin
    p_ooTop = -1 & cnt_ooTop = 0
    p_ooBot = -1 & cnt_ooBot = 0
  endelse
  is_ooTop = is_data and (cnt_ooTop ne 0)
  is_ooBot = is_data and (cnt_ooBot ne 0) 
  
  if (is_ooBot or is_ooTop) and user_dcbar then $
  message, 'There are out of bound values. $DCBAR is not compatible with such values, treat them as missing data.'
 
  if is_ooBot then oob_bot_arrow = 1 ; Force it
  if is_ooTop then oob_top_arrow = 1 ; Force it   
    
  same_minmax = _min_level eq _max_level
     
  ; Give a value to _n_levels   
  if n_elements(n_levels) eq 0 then begin
    ; With top OOB, ncolors eq nlevels else ncolors eq nlevels-1
    if user_Colors then begin 
      if user_dcbar then begin
        _n_levels = N_Elements(colors[*,0,0])
        if user_Levels then if N_ELEMENTS(_levels) ne _n_levels then Message, 'If COLORS and LEVELS are set for a DCBAR, they should have the same number as elements.'
      endif else begin
        _n_levels = N_Elements(colors[*,0,0]) - 1
        if user_Levels then if N_ELEMENTS(_levels) ne _n_levels then Message, 'If COLORS and LEVELS are set, COLORS should have one more element than LEVELS.'
      endelse  
    endif
    if user_Levels then _n_levels = N_Elements(_levels)
    if N_ELEMENTS(_n_levels) eq 0 then begin    
      ; if min and max are the same, one level one color
      if same_minmax then _n_levels = 2 else begin
        if user_dcbar then _n_levels = maxncolors else _n_levels = maxncolors - 1 ; default value
        if ~ oob_bot_arrow then _n_levels += 1
        if ~ oob_top_arrow then _n_levels += 1
        if is_Missing then _n_levels-=1
      endelse
    endif
  endif else begin
    if ~ arg_okay(n_levels, /INTEGER, /SCALAR) then Message, WAVE_Std_Message('N_LEVELS', /ARG)
    _n_levels = n_levels
  endelse   
  
  if _n_levels eq 1 then user_dcbar = 1B
  
  if _n_levels lt 1 then Message, 'Less then 1 level? Something got really wrong.'
   
  ; A few more checks for the crasiest cases
  if user_Levels then if _n_levels ne N_ELEMENTS(_levels) then $
   Message, '$LEVELS and $N_LEVELS are incompatible.'
   
   ; Compute the levels
   if ~ user_Levels then begin
     if KEYWORD_SET(HIST_EQUAL) then begin
       if user_Levels then Message, 'Ambiguous keyword combination of LEVELS and HIST_EQUAL'
       if ~ is_data or cntValid eq 0 then Message, 'HIST_EQUAL only works with real (valid) data!'
       factor = 5000d
       case dataTypeName of
         'FLOAT' : binsize = (_max_level-_min_level) / factor
         'DOUBLE': binsize = (_max_level-_min_level) / factor
         'BYTE': binsize = 1
         'LONG': binsize = 1
         'INT': binsize = 1
         else: Message, 'Data type too exotic for me'
       endcase
       hist = histogram(_data[pValid], MIN=_min_level, MAX=_max_level, $
          BINSIZE=binsize, LOCATIONS=locs)
       hist = total(hist, /CUMULATIVE) / TOTAL(hist)
       hist_levels = cgScaleVector(FINDGEN(_n_levels) / (_n_levels-1), min(hist), max(hist))
       r =  0 > VALUE_LOCATE(hist, hist_levels) < (N_ELEMENTS(hist)-1)
       r[0] = 0 ; allways
       ;Unfortunately, check for uniqueness
       uu = UNIQ(r)
       if N_ELEMENTS(uu) ne N_ELEMENTS(r) then begin
         for i=1, N_ELEMENTS(r)-2 do begin
           if r[i] eq r[i-1] then begin
             if r[i] + 1 ne r[i+1] then r[i] = r[i] + 1
           endif
           if r[i] eq r[i+1] then begin
             if r[i] - 1 ne r[i-1] then r[i] = r[i] - 1
           endif
         endfor
       endif
       uu = UNIQ(r)
       if N_ELEMENTS(uu) ne N_ELEMENTS(r) then Message, 'sorry, histequal not working with this data / n_level combination...'
       _levels = locs[r] 
       is_hist = TRUE
     endif else begin   
       case dataTypeName of
         'FLOAT':  _levels = (float(_max_level + same_minmax - _min_level) / (_n_levels-1)) * Indgen(_n_levels) + _min_level
         'DOUBLE':  _levels = (double(_max_level + same_minmax - _min_level) / (_n_levels-1)) * Indgen(_n_levels) + _min_level
          else: begin
            _levels = ROUND((FLOAT(_max_level + same_minmax - _min_level) / (_n_levels-1)) * Indgen(_n_levels) + _min_level)
            _levels = _levels[SORT(_levels)]
            if N_ELEMENTS(UNIQ(_levels)) ne N_ELEMENTS(_levels) then $
             _levels = (float(_max_level + same_minmax - _min_level) / (_n_levels-1)) * Indgen(_n_levels) + _min_level
          end
       endcase
     endelse
   endif
 
  ; decide n_colors to compute automatically  
  if user_dcbar then begin
    _n_colors = _n_levels     
  endif else begin
    _n_colors = _n_levels + 1
    if ~ oob_bot_arrow or oob_bot_str then _n_colors -= 1
    if ~ oob_top_arrow or oob_top_str then _n_colors -= 1
  endelse
  
  ; Colors
  if user_Colors then begin
    if size(colors, /n_dimensions) eq 2 then begin ;Palette
      _colors = cgColor24(colors)
    endif else begin
      _colors = cgColor(colors)
    endelse
  endif else begin ;Auto colors
    if N_ELEMENTS(CMIN) ne 0 then begin
      if ~ arg_okay(cmin, /INTEGER, /SCALAR) then Message, WAVE_Std_Message('CMIN', /ARG)
      _cmin = BYTE(cmin)
    endif else _cmin = 0B
    if N_ELEMENTS(CMAX) ne 0 then begin
      if ~ arg_okay(cmax, /INTEGER, /SCALAR) then Message, WAVE_Std_Message('CMAX', /ARG)
      _cmax = BYTE(cmax)
    endif else _cmax = BYTE(maxncolors-1)
    if _n_levels eq 1 then begin
      _colors = _cmin
    endif else begin
      if N_ELEMENTS(CENTER_LEV) ne 0 then begin
        ; could it be possible to define where in the middle?
        if is_ooBot then midd = center_lev + 1 else midd = center_lev
        center_c = FIX((_cmax-_cmin)/2.)
        if N_ELEMENTS(CENTER_RANGE) eq 0 then _cr = 0 else _cr = center_range
        _cc1 = cgScaleVector(Indgen(midd, /BYTE), _cmin, center_c-_cr, /PRESERVE_TYPE)
        _cc2 = cgScaleVector(Indgen(_n_colors-midd, /BYTE), center_c+_cr, _cmax, /PRESERVE_TYPE)
        _colors = [_cc1,_cc2]
      endif else begin
        _colors = cgScaleVector(Indgen(_n_colors, /BYTE), _cmin, _cmax, /PRESERVE_TYPE)
      endelse
    endelse
    if KEYWORD_SET(INVERTCOLORS) then _colors  = Reverse(_colors)
    if N_ELEMENTS(_colors) eq 3 then begin
      _colors = (cgColor([0B,_colors], /DECOMPOSED))[1:*]
    endif else begin
      _colors = cgColor(_colors, /DECOMPOSED)
    endelse
  endelse

  ; OOB colors
  if oob_bot_str then begin
    _colors = [cgColor(oob_bot_color), _colors]
  endif else begin
    if ~ is_ooBot and oob_bot_arrow and ~ KEYWORD_SET(oob_bot_color) then _colors[0] = cgColor('white')
  endelse
  if oob_top_str then begin
    _colors = [_colors, cgColor(oob_top_color)]
  endif else begin
    if ~ is_ooTop and oob_top_arrow and ~ KEYWORD_SET(oob_top_color) then _colors[N_ELEMENTS(_colors)-1] = cgColor('white')
  endelse
      
  ; Neutral Color
  if N_ELEMENTS(NEUTRAL_COLOR) ne 0 then begin
    if ~ arg_okay(neutral_color, /SCALAR) then Message, WAVE_Std_Message('NEUTRAL_COLOR', /ARG)
   _neutral_color = cgColor(neutral_color, /DECOMPOSED)
  endif else _neutral_color = cgColor('grey', /DECOMPOSED)
  
  if is_Missing then _colors = [_neutral_color, _colors]
  
  if N_ELEMENTS(_colors) gt 256 then Message, 'More than 256 colors. Impossible'
  
  ; some common sense
  _levels = _levels[SORT(_levels)]
  if N_ELEMENTS(UNIQ(_levels)) ne N_ELEMENTS(_levels) then Message, 'Levels are not unique?'
  
  is_ooTopColor = oob_top_arrow
  is_ooBotColor = oob_bot_arrow
  
  if  N_ELEMENTS(_colors) eq 3 then row = 1
  _palette = w_gr_ColorToRGB(_colors, ROW=row)
  
    
  if ~ is_data then begin 
    ; we give back the colors and levels and that's all
    out = { $
           is_data : FALSE, $
           levels : _levels, $ 
           colors : _colors, $ 
           palette : _palette, $ 
           is_hist : FALSE, $
           dcbar : user_dcbar, $   
           is_ooTopColor : is_ooTopColor, $
           is_ooBotColor : is_ooBotColor, $
           is_Missing : is_Missing $                  
          }    
   if KEYWORD_SET(SHOW) then w_gr_DataLevels_show, out
   return, out
  endif
    
  ; We have data
   out = { $
    is_data : TRUE, $
    levels : _levels, $
    colors : _colors, $ 
    palette : _palette, $ 
    dcbar : user_dcbar, $
    is_ooTopColor : is_ooTopColor, $
    is_ooBotColor : is_ooBotColor, $
    is_Missing : is_Missing, $
    is_ooTop : is_ooTop, $
    is_ooBot : is_ooBot, $
    is_hist : is_hist, $
    missing_value : _missing , $
    epsilon : _epsilon , $
    dataTypeName : dataTypeName , $
    dataNel : dataNel , $
    dataNdims : dataNdims , $
    data_min : dataMin , $
    data_max : dataMax , $
    pmissing : pNoValid, $
    nmissing : cntNoValid, $
    valid : valid, $
    pValid : pValid, $
    nValid : cntValid $
    }
    
   out = w_gr_DataLevels_dataloc(out, _data)
  
  if KEYWORD_SET(SHOW) then w_gr_DataLevels_show, out
  
  return, out
  
end