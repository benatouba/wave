; docformat = 'rst'

;+
;
;This bundle of procedures is a tool set available to the WAVE user.
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  22-Nov-2010 FaM
;-

;+
; :Description:
;      This procedure simply takes xs and ys in form of an array to make a 2d grid.
;      
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    ax: in, required, type=array, default=none
;        The 1 dimensional array of x-coordinates (e.g. longitudes).
;    ay: in, required, type=array, default=none
;        The 1 dimensional array of y-coordinates (e.g. latitudes).
;    x: out, type=array, default=none
;       The 2 dimensional array of x-coordinates (e.g. longitudes).
;    y: out, type=array, default=none
;       The 2 dimensional array of y-coordinates (e.g. latitudes).
;       
; :Examples:
;        This is how the output may look like::
;              IDL> ax = [91.,92.,93.]
;              IDL> ay = [31.,32.]
;              IDL> utils_1d_to_2d, ax, ay, x, y
;              IDL> print, x
;                   91.000000       92.000000       93.000000
;                   91.000000       92.000000       93.000000
;              IDL> print, y
;                   31.000000       31.000000       31.000000
;                   32.000000       32.000000       32.000000
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
pro utils_1d_to_2d, ax, ay, x, y

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN
  ENDIF  

  if n_params() ne 4 then Message, WAVE_Std_Message(/NARG)
  
  if not arg_okay(ax, /NUMERIC) then Message, WAVE_Std_Message('ax', /NUMERIC)
  if not arg_okay(ay, /NUMERIC) then Message, WAVE_Std_Message('ay', /NUMERIC)
    
  if SIZE(ax, /N_DIMENSIONS) ne 1 then Message, WAVE_Std_Message('ax', DIMARRAY=1)
  if SIZE(ay, /N_DIMENSIONS) ne 1 then Message, WAVE_Std_Message('ay', DIMARRAY=1)
  
  nx = n_elements(ax)
  ny = n_elements(ay)
  
  if nx lt 2 then Message, '$ax has not enough elements.'
  if ny lt 2 then Message, '$ay has not enough elements.'
  
  y = (LONARR(nx) + 1) # ay ; "the georef-eq" 2-dimensional array
  x = ax # (LONARR(ny) + 1) ; "the georef-eq" 2-dimensional array
  
end



;+
; :Description:
;    TODO: Describe procedure.
;
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    data: in, required, type= , default=none
;    levels:  type= , default=none
;    clevels:  type= , default=none
;
; :Keywords:
;    NLEVS: in, optional, type= , default=none
;    RANGE: in, optional, type= , default=none
;    MODIF_DATA: in, optional, type= , default=none
;    COLOR_RANGE: in, optional, type= , default=none
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
pro utils_data_levels, data, levels, clevels, NLEVS = nlevs, RANGE = range, MODIF_DATA = modif_DATA, COLOR_RANGE = color_range

  if ~ KEYWORD_SET(RANGE) then range = [FLOOR(min(data)), FLOOR(max(data))]  
  if ~ KEYWORD_SET(COLOR_RANGE) then COLOR_RANGE = [1, 254]  
  if ~ KEYWORD_SET(nlevs) then nlevs = 10
  
  newdata = data
  if range[0] gt min(data) then begin
    p = where(data lt range[0])    
    newdata[p] = range[0]
  endif   

  levels = indgen(nlevs) * (range[1] - range[0])/double(nlevs-1) + range[0]
  clevels = indgen(nlevs) * (COLOR_RANGE[1] - COLOR_RANGE[0])/double(nlevs-1) + COLOR_RANGE[0]
  
  modif_DATA = range[0] > data < range[1] 

end



;+
; :Description:
;    This function simply deaccumulates a field (like RAINNC).
;    It substracts the step i from the step i+1 on the last 
;    dimension of the array, supposed to be time.
;
; :Categories:
;    WAVE/UTILS
;    
; :Params:
;    accumulated: in, required, default=none
;                 the accumulated variable (dim 1 or 3)
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
function utils_ACC_TO_STEP, accumulated

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
    
  siz = SIZE(accumulated, /DIMENSIONS)
  
  if N_ELEMENTS(siz) eq 3 then begin
      n = siz[2]
      tempsubs = accumulated * 0.
      tempsubs[*,*,1:n-1] = accumulated[*,*,0:n-2]
      return, accumulated - tempsubs   
  endif else if N_ELEMENTS(siz) eq 1 then begin
      n = siz[0]
      tempsubs = accumulated * 0
      tempsubs[1:n-1] = accumulated[0:n-2]
      return, accumulated - tempsubs 
  endif else begin
      Message, WAVE_Std_Message('accumulated', /NDIMS)
      return, -1
  end
  
end


;+
; :Description:
;    Reads a shp-file and returns a polygon model of all valid vertices. 
;    TODO: Descriptions for params/keywords
;    
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    latlons: in, required, type=  , default=none                              
;    conn: in, required, type=  , default=none  
;
; :Keywords:
;    SHPFILE: in, optional, type=  , default=DIALOG_PICKFILE()  
;    SAVFILE: in, optional, type=  , default=none
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
pro utils_read_shp, latlons, conn, SHPFILE = shpfile, SAVFILE = savfile

  ; Set Up environnement
  compile_opt idl2
  @WAVE.inc

  undefine, latlons, conn

  if not KEYWORD_SET(shpfile) then begin
    shpfile = DIALOG_PICKFILE(TITLE='Please select shape file file to read', /MUST_EXIST, FILTER = '*.shp' )
    if shpfile eq '' then return
  endif

  ; read shp file and create polygon object from entities
  shpmodel=OBJ_NEW('IDLffShape',shpfile)

  ;Get the number of entities so we can parse through them
  shpModel->GetProperty, N_ENTITIES=N_ent
  
  n_latlons = 0L

  for i=0L, N_ent-1 do begin

    ent = shpmodel->GetEntity(i, /ATTRIBUTES)
    
    if not ptr_valid(ent.vertices) then continue
    if n_elements((*ent.vertices)[0,*]) lt 3 then continue
;    if n_elements((*ent.vertices)[0,*]) lt 50 then continue
;    if ABS((*ent.attributes).ATTRIBUTE_1 -950582848.989) gt 1 then continue
;    if (*ent.attributes).ATTRIBUTE_0 ne 53 then continue ;that was for namco shore
;    if (*ent.attributes).ATTRIBUTE_4 ne 'Berlin' then continue

;    print, ' Entity nb ' + STRING(i, FORMAT='(I2)') + '. att 0: ' + STRING((*ent.attributes).ATTRIBUTE_0, FORMAT='(I2)')+ '. att 1: ' + STRING((*ent.attributes).ATTRIBUTE_1, FORMAT='(I2)') + '. N elements :' + STRING(ent.n_vertices, FORMAT='(I5)')

    if n_elements(latlons) eq 0 then latlons = *ent.vertices else latlons = [[latlons],[*ent.vertices]]

    parts = *ent.parts
;    if ent.n_parts gt 1 then begin  
;      print, 'yes' 
;    end
    for k=0L, ent.n_parts-1 do begin

      if k eq ent.n_parts-1 then n_vert = ent.n_vertices - parts[k]  else n_vert = parts[k+1]-parts[k]

      polyconn = (lindgen(n_vert)) + n_latlons

      if n_elements(conn) eq 0 then begin
        conn = n_vert
        conn = [conn,polyconn]
      endif else begin
        conn = [conn,n_vert]
        conn = [conn,polyconn]
      endelse    
     
      n_latlons += n_vert
      
    endfor
    
    
    
  endfor

  ; clean unused objects
  obj_destroy, shpModel

;  this was for the basin
;  map = MAP_PROJ_INIT(101, CENTER_LONGITUDE = 93.0, CENTER_LATITUDE = 0 , ZONE =46)
;  latlons = MAP_PROJ_INVERSE(latlons[0,*], latlons[1,*], map_structure=map)

;  if KEYWORD_SET(SAVFILE) then save, latlons, conn, FILE=SAVFILE
  
end


;+
; :Description:
;    This procedure retrieves the georeferenced equivalent positions from a lat-lon array, using the nearest neighborhood algorithm. 
;    Therefore the output is an one dimensional array of size N_ELEMENTS(flon) or [4,N_ELEMENTS(flon)]. 
;    
;      TODO: Descriptions for Params/Keywords
;
; :Categories:
;    WAVE/UTILS
;    
; :Params:
;    ilon: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the longitudes corresponding to the original array.
;    ilat: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the latitudes corresponding to the original array.
;    flon: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the longitudes corresponding to desired fitted data.
;    flat: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the latitudes corresponding to desired fitted data.
;
; :Keywords:
;    DISTANCES: in, optional, type=string, default=none
;    CLASSICAL: in, optional, type=string, default=none
;    TRIANGULATION: in, optional, type=string, default=none
;    FOURPOINTS: in, optional, type=string, default=none
;    
;    
; :Examples:
;     Calling Sequence::
;              Result = GEO_POS_NEAREST_NEIGHBORHOOD(ilon, ilat, flon, flat)
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
function utils_POS_NEAREST_NEIGHBORHOOD, ilon, ilat, flon, flat, DISTANCES = distances, $
                  CLASSICAL = classical, TRIANGULATION = triangulation, FOURPOINTS = fourpoints

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  ;********
  ; Check *
  ;********
  
  if n_params() ne 4 then MESSAGE, WAVE_Std_Message(/NARG)
  
  if not arg_okay(flon, /NUMERIC) or not arg_okay(flat, /NUMERIC) or not arg_okay(ilon, /NUMERIC) or not arg_okay(ilat, /NUMERIC) then $
    MESSAGE,'lat and/or lons not in numerical format'
  
  if ~array_processing(ilon, ilat) then MESSAGE, WAVE_Std_Message('ilon ilat',/ARG)
  if ~array_processing(flon, flat) then MESSAGE, WAVE_Std_Message('flon flat',/ARG)
  
  doDist = ARG_PRESENT(DISTANCES)
  
  ;********************
  ; Define output dim *
  ;********************  
  n = N_ELEMENTS(flon)
  IF KEYWORD_SET(FOURPOINTS) then begin
    if n eq 1 then begin
      out = LONARR(4) 
      if doDist then distances = MAKE_ARRAY(4, /NOZERO, TYPE=SIZE(flon, /TYPE))
    endif else begin
     out = lonarr(4,n) 
     if doDist then distances = MAKE_ARRAY(4, n, /NOZERO, TYPE=SIZE(flon, /TYPE))
    endelse  
  endif else begin
    if n eq 1 then begin
      out = 0L 
      if doDist then distances =  0D
    endif else begin
     out = lonarr(n)
     if doDist then distances = MAKE_ARRAY(n, /NOZERO, TYPE=SIZE(flon, /TYPE))
     endelse
  endelse 

  ;********************
  ; Go threw Keywords *
  ;********************  
  algo = ''
  if KEYWORD_SET(CLASSICAL) then algo = 'CLASSICAL' $
    else if KEYWORD_SET(TRIANGULATION) then algo = 'TRIANGLE' $
      else if n eq 1 then algo = 'CLASSICAL' else algo = 'TRIANGLE'        

  
  if KEYWORD_SET(FOURPOINTS) then algo = 'CLASSICAL_F'
  
  ;**********
  ; Lets go *
  ;**********
  
  CASE algo OF
  
    'CLASSICAL': begin
    
      for i = 0l, n - 1 do begin      
        quad = (flon[i] - ilon)^2 + (flat[i]- ilat)^2
        minquad = min(quad, p)    
        out[i] = p[0]
         if doDist then DISTANCES[i] = minquad        
      endfor
      
    end
    
    'CLASSICAL_F': begin
    
      for i = 0l, n - 1 do begin
        quad = (flon[i] - ilon)^2 + (flat[i]- ilat)^2
        s = sort(quad)
        out[*,i] = s[0:3]
        if doDist then distances[*,i] = quad[s[0:3]]
      endfor
      
    end
    
    'TRIANGLE': begin
    
      n1 = n_elements(ilon)
      x1 = ilon[*] & y1 = ilat[*]
      x2 = flon[*] & y2 = flat[*]    
      
      triangulate, x1, y1, c ; Compute Delaunay triangulation     
       
      out = GRIDDATA(x1,y1,LINDGEN(n1), XOUT=x2, YOUT=y2, /NEAREST_N, TRIANGLES =c)
      if doDist then distances = (ilon[out] - x2)^2 + (ilat[out] - y2)^2         
      
    end
    
;    'ROW': begin
;    
;      n1 = n_elements(tilon)
;      x1 = tilon[*] & y1 = tilat[*]
;      x2 = tflon[*] & y2 = tflat[*]
;      
;      d=(rebin(transpose(x1),n,n1,/SAMPLE)-rebin(x2,n,n1,/SAMPLE))^2 + $
;        (rebin(transpose(y1),n,n1,/SAMPLE)-rebin(y2,n,n1,/SAMPLE))^2
;        
;      m = MIN(d, DIMENSION=2, s)
;      out[*] = s[*]/n & distances[*] = d[s]
;      
;    end
;    
;    'ROW_F': begin
;    
;      n1 = n_elements(tilon)
;      x1 = tilon[*] & y1 = tilat[*]
;      x2 = tflon[*] & y2 = tflat[*]
;      
;      d=(rebin(transpose(x1),n,n1,/SAMPLE)-rebin(x2,n,n1,/SAMPLE))^2 + $
;        (rebin(transpose(y1),n,n1,/SAMPLE)-rebin(y2,n,n1,/SAMPLE))^2
;        
;      for j=0L, 3 do begin
;        m = MIN(d, DIMENSION=2, s)
;        out[j,*] = s[*]/n & distances[j,*] = d[s]
;        d[s] = max(d) * 2. ;dummy large distance
;      endfor
;      
;    end
;    
;    'TRIANGLE_F': begin
;    
;      n1 = n_elements(tilon)
;;      x1 = tilon[*] & y1 = tilat[*]
;      x2 = TEMPORARY(tflon[*]) 
;      y2 = TEMPORARY(tflat[*])     
;      
;      triangulate, tilon[*], tilat[*], c ; Compute Delaunay triangulation     
;      subsets = LINDGEN(SIZE(tilon, /DIMENSIONS))
;      tout = GRIDDATA(tilon[*],tilat[*], subsets[*], XOUT=x2, YOUT=y2, /NEAREST_N, TRIANGLES =c)
;      
;      inds = ARRAY_INDICES(tilon, tout)
;      subindX = [[REFORM(inds[0,*]) - 3],[REFORM(inds[0,*]) + 3]]
;      subindY = [[REFORM(inds[1,*]) - 3],[REFORM(inds[1,*]) + 3]]
;      
;      indmax = N_ELEMENTS(tilon[*,0])-1
;      p = where(subindX gt indmax, cnt)
;      if cnt ne 0 then subindX[p] = indmax
;      p = where(subindX lt 0, cnt)
;      if cnt ne 0 then subindX[p] = 0  
;      
;      indmax = N_ELEMENTS(tilon[0,*]) -1
;      p = where(subindY gt indmax, cnt)
;      if cnt ne 0 then subindY[p] = indmax            
;      p = where(subindY lt 0, cnt)
;      if cnt ne 0 then subindY[p] = 0      
;      
;      for i = 0l, n - 1 do begin
;                     
;        sublons = tilon[subindX[i,0]:subindX[i,1],subindY[i,0]:subindY[i,1]]
;        sublats = tilat[subindX[i,0]:subindX[i,1],subindY[i,0]:subindY[i,1]]
;        subsub = subsets[subindX[i,0]:subindX[i,1],subindY[i,0]:subindY[i,1]]
;        
;        quad = (tflon[i] - sublons)^2 + (tflat[i]- sublats)^2
;        
;        for j=0L, 3 do begin
;          minquad = min(quad, p)
;          if N_ELEMENTS(p) gt 1 then p = p[0] ; it happens.....          
;          out[j,i] = subsub[p] & DISTANCES[j,i] = minquad
;          quad[p] = max(quad) * 2. ;dummy large distance
;        endfor
;        
;      endfor
;     
;    end
        
  ENDCASE
          
    ;********
    ;* DONE *
    ;********
    if doDist then DISTANCES=sqrt(DISTANCES)
    if doDist then DISTANCES=reform(DISTANCES, N_ELEMENTS(flon[*,0]), N_ELEMENTS(flon[0,*]))
    
    return, reform(LONG(out), N_ELEMENTS(flon[*,0]), N_ELEMENTS(flon[0,*]))
  
end



;+
; :Description:
;    This procedure follows the use of `utils_POS_NEAREST_NEIGHBORHOOD` to get the data.
;    
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    pos: in, required, type= ,default=none
;         The positions obtained with a previous call of `utils_POS_NEAREST_NEIGHBORHOOD`.
;    data: in, type= ,default=none
;          The 1 or 2 dimensional array of the data to fit.
;    
; :Examples:
;        Calling Sequence::
;                 Result = utils_COMPUTE_NEAREST_NEIGHBORHOOD(pos, data)
;     
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
function utils_COMPUTE_NEAREST_NEIGHBORHOOD, pos, data

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  if n_params() lt 2 then message, WAVE_Std_Message(/NARG)  
  if not arg_okay(pos, /NUMERIC) then  message, 'pos not in num format'
  
  out = MAKE_ARRAY(N_ELEMENTS(pos[*,0]), N_ELEMENTS(pos[0,*]), /NOZERO, TYPE=SIZE(data, /TYPE))  
    
  out[*] = data[pos]
    
  ;********
  ;* DONE *
  ;********

  return, out
  
end

;+
; :Description:
;    This procedure reads all TRMM 3B42 netcdf files from a directory, 
;    sorts them by date and aggregates the precipitation field to a 
;    single NCDF file.
;    
;    Various options are given to the user, one of them being the possibility
;    to "shift" the time serie. Originaly, TRMM 3hourly files at e.g. 09H UTC
;    represent the rainfall rate from 07H30 to 10H30. The user can choose 
;    wether to keep the time as it is with the keyword 'NOSHIFT'(standard method 
;    also used by the NASA to aggregate 3 Hourly files to daily files) () or
;    to "shift" the time serie: the prcp at 09H UTC is then the summ of half 
;    the prcp in the 06H file and half the prcp in the 09H file.
;    
;       
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    directory: in, required, type=string , default=none
;               all the 3B42 files in this directory will be recursively parsed.
; :Keywords:
;    START_TIME: in, optional, type=time, default=first available time in directory
;                starting time in the aggregated output file.
;    END_TIME: in, optional, type=time, default=last available time in directory
;             ending time in the aggregated output file.
;    OUTFILE: in, optional, type=string, default= directory+'/3B42_agg.'+date+'.nc'
;             path tto the output file. TO BE WAVE compliant, the file MUST BEGIN with "3B42_agg"
;    NOSHIFT: in, optional
;             if set, aggregat the files using their original timestamp.
;    SUBSET_IJ: in, optional
;              set to aggregate only a subset of the file (see 'TRMM_nc')
;    SUBSET_LL: in, optional
;              set to aggregate only a subset of the file (see 'TRMM_nc')
;    LL_DATUM: in, optional
;               the datum for 'SUBSET_LL'  (see 'TRMM_nc')
; 
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2010
;       Modified:   15-Dec-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
pro utils_TRMM_aggregate_3B42, directory, START_TIME = start_time, END_TIME = end_time, OUTFILE = outfile, $
    NOSHIFT = noshift, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum
    
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = WAVE_Error_Message(!Error_State.Msg) + ' Will not aggregate...'
      if arg_okay(outfile, TYPE = IDL_STRING) then if FILE_TEST(outfile) then FILE_DELETE, outfile
      close, 1
      RETURN
    ENDIF
  
  ; ---------------
  ; Check the input
  ; ---------------
  
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select directory to parse', /DIRECTORY)
  if not FILE_TEST(directory, /DIRECTORY) then message, 'Directory is not a directory'
  
  fileList = FILE_SEARCH(directory, '3B42.*', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT, count = cfiles)
  fileList = fileList[SORT(fileList)]
  if cfiles eq 0 then Message, '$directory is not set properly.'
  
  step = MAKE_TIME_STEP(hour=3)
  
  ; ---------------------
  ; Create the time serie
  ; ---------------------
  ;Parse names for available times wrfout_d01_2008-10-26_12:00:00
  fname = FILE_BASENAME(fileLIST)
  for i=0, cfiles-1 do begin
    tsp = STRSPLIT(fname[i], '.',/EXTRACT)
    tsd = tsp[1]
    y = LONG(STRMID(tsd,0,2))
    if y lt 80 then y +=2000 else y+=1900
    m = LONG(STRMID(tsd,2,2))
    d = LONG(STRMID(tsd,4,2))
    h = LONG(tsp[2])
    if N_ELEMENTS(time) eq 0 then time = QMS_TIME(YEAR=y, MONTH=m, DAY=d, HOUR=h) $
    else  time = [time, QMS_TIME(YEAR=y, MONTH=m, DAY=d, HOUR=h)]
  endfor
  time = time[sort(time)]    
  if ~check_TS(time) then message, '  The files do not form a continous time serie.'
  
  if ~KEYWORD_SET(NOSHIFT) then time = time[1:(N_ELEMENTS(time)-1)]
  nt = N_ELEMENTS(time)
  p0 = 0
  p1 = nt-1
  if KEYWORD_SET(START_TIME) then begin
    if ~check_WTIME(START_TIME, OUT_QMS=t0) then Message, WAVE_Std_Message('START_TIME', /ARG)
    p0 = where(time eq t0, cnt)
    if cnt ne 1 then Message, 'Your start time (' + TIME_to_STR(t0) + $
      ') do not match in my TS: (' + TIME_to_STR(time[0]) + '->'+ TIME_to_STR(time[nt-1])+').'
  endif
  if KEYWORD_SET(END_TIME) then begin
    if ~check_WTIME(END_TIME, OUT_QMS=t1) then Message, WAVE_Std_Message('END_TIME', /ARG)
    p1 = where(time eq t1, cnt)
    if cnt ne 1 then Message, 'Your end time (' + TIME_to_STR(t1) + $
      ') do not match in my TS: (' + TIME_to_STR(time[0]) + '->'+ TIME_to_STR(time[nt-1])+').'
  endif
  
  time = time[p0:p1]
  nt = N_ELEMENTS(time)
  t0 = time[0]
  t1 = time[nt-1]
  
  ; -------------------
  ; Create the log file
  ; -------------------
  str = TIME_to_STR(t0) ; 22.10.2008 03:00:00
  str = strmid(str,6,4) + '_' + strmid(str,3,2) + '_' + strmid(str,0,2)
  
  if ~KEYWORD_SET(OUTFILE) then outfile = DIRECTORY + '/3B42_agg.' + str + '.nc'
  if STRMID(FILE_BASENAME(OUTFILE),0,8) ne '3B42_agg' then Message, 'The output file MUST have the suffix 3B42_agg'
  
  OPENW, 1, DIRECTORY + '/trmm_agg_'+ str + '.log'
  
  printf, 1, 'TRMM 3B42 aggregation'
  printf, 1, ''
  printf, 1, 'Number of files: ' + str_equiv(cfiles)  
  printf, 1, 'Start date : ' + TIME_to_STR(time[0])
  printf, 1, 'End   date : ' + TIME_to_STR(time[nt-1])
  
  ; Open the file you just created and copy the information in it to another file.
  printf, 1, ''
  text = 'Destination file : ' + outfile
  printf, 1, text
  
  ; ---------------------------
  ; Read the Netcdf template file
  ; ---------------------------
  template = OBJ_NEW('TRMM_nc', FILE=fileLIST[0], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)
  TEMPLATE->get_ncdf_coordinates, lon, lat, nx, ny
  TEMPLATE->GetProperty, SUBSET = subset
  OBJ_DESTROY, TEMPLATE
  ; ---------------------------
  ; Create the Netcdf out file
  ; ---------------------------
  tid = NCDF_CREATE(outfile, /CLOBBER)
  NCDF_CONTROL, tid, /FILL
  ;Define dimensions
  dimTimeid  = NCDF_DIMDEF(tid, 'time', nt)
  dimLonid  = NCDF_DIMDEF(tid, 'lon', nx)
  dimLatid  = NCDF_DIMDEF(tid, 'lat', ny)
  
  ; Define tvariables
  Timeid = NCDF_VarDef(tid, 'time', dimTimeid, /LONG)
  ; Add attributes
  NCDF_AttPut, tid, Timeid, 'zone', 'UTC', /CHAR
  d = TIME_to_STR(time[0])
  str = 'hours since ' + STRMID(d,6,4) + '-' + STRMID(d,3,2) + '-' + STRMID(d,0,2) + ' ' + STRMID(d,11,2)
  NCDF_AttPut, tid, Timeid, 'units', str, /CHAR
  
  Lonid = NCDF_VarDef(tid, 'lon', dimLonid, /FLOAT)
  NCDF_AttPut, tid, Lonid, 'units', 'degrees_east', /CHAR
  Latid = NCDF_VarDef(tid, 'lat', dimLatid, /FLOAT)
  NCDF_AttPut, tid, Latid, 'units', 'degrees_north', /CHAR
  
  pcpid = NCDF_VarDef(tid, 'precipitation', [dimLonid, dimLatid, dimTimeid], /FLOAT)
  NCDF_AttPut, tid, pcpid, 'units', 'mm 3hrs-1', /CHAR
  
  ; Add global attributes to the file.
  NCDF_AttPut, tid, 'creation_date', TIME_to_STR(QMS_TIME()), /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'conventions', 'COARDS', /GLOBAL, /CHAR
  
  if KEYWORD_SET(NOSHIFT) then tinfo = 'The value at each UTC step is the accumulated prcp over the three PREVIOUS hours (mm 3hrs-1).' $
  else TINFO = 'The value at each UTC step is the accumulated prcp over the three SURROUNDING hours (mm 3hrs-1).'
  NCDF_AttPut, tid, 'time_info', tinfo, /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'source', 'TRMM 3B42 three-hourly product.', /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'title', 'TRMM 3B42 agg file', /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'subset', 'subset = ['+ str_equiv(subset[0]) +', '+ str_equiv(subset[1]) +', '+ str_equiv(subset[2]) +', '+ str_equiv(subset[3]) +']', /GLOBAL, /CHAR
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
  
  printf, 1,  'Destination file defined.'
  printf, 1, 'Now starting to fill... '
    
  hours = INDGEN(nt, /LONG) * 3L
  NCDF_VARPUT, tid, Timeid, hours
  NCDF_VARPUT, tid, Lonid, REFORM(lon[*,0])
  NCDF_VARPUT, tid, Latid, REFORM(lat[0,*])
  if ~KEYWORD_SET(NOSHIFT) then NCDF_VARPUT, tid, pcpid, FLTARR(nx,ny,nt)
  
  for i = 0, cfiles-1 do begin
  
    t_obj = OBJ_NEW('TRMM_nc', FILE=fileLIST[i], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)

    pcp = (t_obj->get_prcp(t) > 0) * 3
    
    if KEYWORD_SET(NOSHIFT) then begin
      idx = where(time eq t, cnt)
      if cnt eq 1 then NCDF_VARPUT, tid, pcpid, pcp, OFFSET = [0,0,idx]
    endif else begin
      idx = where(time eq t, cnt)
      if cnt eq 1 then begin
        NCDF_VARGET, tid, pcpid, pcpold, OFFSET = [0,0,idx], count = [nx,ny,1]
        NCDF_VARPUT, tid, pcpid, pcpold + pcp/2., OFFSET = [0,0,idx]
      endif      
      idx = where(time eq t + 3LL*H_QMS, cnt)
      if cnt eq 1 then begin
        NCDF_VARGET, tid, pcpid, pcpold, OFFSET = [0,0,idx], count = [nx,ny,1]
        NCDF_VARPUT, tid, pcpid, pcpold + pcp/2., OFFSET = [0,0,idx]
      endif      
    endelse
    OBJ_DESTROY, t_obj
    printf, 1,  '  ' + fileLIST[i] + ' processed.'
    
  endfor
  
  printf, 1,  'DONE!'
  CLOSE, 1
  NCDF_CLOSE, tid
  
end

;+
; :Description:
;    
;    !!! Not implemented yet !!!
;    see 'utils_TRMM_aggregate_3B42' 
;    
;-
pro utils_TRMM_aggregate_3B43
    

  
end


;+
; :Description:
;    Reads specific tags from the 'CoreMetadata.0' attribute of MODIS/EOS files.
;
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    pvlstring: in, type= , default=none
;    objstring: in, type= , default=none
;    n_char: in, type= , default=none
;
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
function utils_EOD_get_metadata, pvlstring, objstring, n_char

  ; Extract a beginning date object
  objstring = strupcase(objstring)
  val_str = '"'
  obj_start = strpos(pvlstring,objstring)
  if obj_start ne -1  then obj_start = strpos(pvlstring,val_str,obj_start)+1
  info = strmid(pvlstring,obj_start,n_char)
  return, info
    
end


;+
; :Description:
;    This procedure reads a time from a ncdf file that uses the COARDS convention.    
;    Returns TRUE if time is found, FALSE in all other cases
;
; :Params:
;    cdfid: in, required, type = long
;           the active file cdfid
;    time: out, optional, type = time
;          the time serie (if found)
;    time0: out, optional, type = time
;           the first time in the time serie (if found)
;    time1: out, optional, type = time
;           the last time in the time serie (if found)
;    nt: out, optional, type = long
;        the number of elements in the time serie

; :Keywords:
;     VarName: in, optional, type = String
;              if you know the name of a variable which is coards
;         
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin}
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          10-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-
function UTILS_NC_COARDS_TIME, cdfid, time, time0, time1, nt, VARNAME = varname

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN, FALSE
  ENDIF  
  
  s_list = ['time','times','xtime']
  
  inq = NCDF_INQUIRE(Cdfid)
  vok = -1
  
  if KEYWORD_SET(VarName) then begin    
      
    vinf = NCDF_VARINQ(Cdfid, VarName)
    vname = vinf.name
    vtype = vinf.DATATYPE
    vok = NCDF_VARID(Cdfid, VarName) 
    
  endif else begin
  
    for varid = 0, inq.NVARS - 1 do begin
      vinf = NCDF_VARINQ(Cdfid, varid)
      vname = vinf.name
      vtype = vinf.DATATYPE
      p = where(str_equiv(s_list) eq str_equiv(vname), cnt)
      if cnt ne 0 then begin
        test = 1
      endif
      
      if cnt ne 0 and vtype ne 'CHAR' and  vtype ne 'BYTE' then begin
        vok = varid
        break
      endif
    endfor
    
  endelse
        
  if vok lt 0 then return, FALSE
  
  
  ; Read time0 from att
  NCDF_ATTGET, Cdfid, vok , 'units', ts
  ts = STRSPLIT(STRING(ts), ' ', /EXTRACT)
  psince = WHERE(str_equiv(ts) eq str_equiv('since'), csince)
  if csince ne 1 then begin
    ; Read time0 from att
    NCDF_ATTGET, Cdfid, vok , 'description', ts
    ts = STRSPLIT(STRING(ts), ' ', /EXTRACT)
    psince = WHERE(str_equiv(ts) eq str_equiv('since'), csince)    
  endif
  if csince ne 1 then return, FALSE
  
  unit = ts[psince - 1]
  d = ts[psince + 1]
  t = ts[psince + 2]  
  if N_ELEMENTS(ts) gt psince + 3 then Message, 'Time contains a zone (which is currently not supported) or is not of suitable format'
   
  d = STRSPLIT(d,'-', /EXTRACT)   
  if N_ELEMENTS(d) ne 3 then return, FALSE  
  y = LONG(d[0])
  mo = LONG(d[1])
  d = LONG(d[2])
  
  t = STRSPLIT(t,':', /EXTRACT)   
  if N_ELEMENTS(t) eq 1 then begin
   h = LONG(t[0])
  endif else if N_ELEMENTS(t) eq 2 then begin
   h = LONG(t[0])
   mi = LONG(t[1])
  endif else if N_ELEMENTS(t) eq 3 then begin
   h = LONG(t[0])
   mi = LONG(t[1])
   s = LONG(t[2])
   milli = LONG64( (DOUBLE(t[2]) - FLOOR(double(t[2]))) * 1000D )
  endif else return, FALSE  
      
  time0 = (MAKE_ABS_DATE(YEAR=y, MONTH=mo, DAY=d, HOUR = h, MINUTE=mi, SECOND=s, MILLISECOND=milli)).qms
  
  NCDF_VARGET, Cdfid, vok, u
  
  fac = 0LL
  case (str_equiv(UNIT)) of
    'SEC': fac = S_QMS
    'SECS': fac = S_QMS
    'S': fac = S_QMS
    'SS': fac = S_QMS
    'SECOND': fac = S_QMS
    'SECONDS': fac = S_QMS
    'MINUTE':  fac = M_QMS
    'MINUTES': fac = M_QMS
    'MIN': fac = M_QMS
    'MINS': fac = M_QMS
    'HOUR': fac = H_QMS
    'HOURS': fac = H_QMS
    'HR': fac = H_QMS
    'HRS': fac = H_QMS
    'H': fac = H_QMS
    'HS': fac = H_QMS
    'DAY': fac = D_QMS
    'DAYS': fac = D_QMS
    'D': fac = D_QMS
    'DS': fac = D_QMS
    else: return, FALSE
  endcase

  time = time0 + fac * LONG64(u)
  nt = N_ELEMENTS(time)
  time0 = time[0]
  time1 = time[nt-1]
  
  return, TRUE

end

;+
; :Description:
;    This procedure reads a time from a WRF ncdf file
;    Returns TRUE if time is found, FALSE in all other cases
;
; :Params:
;    cdfid: in, required, type = long
;           the active file cdfid
;    time: out, optional, type = time
;          the time serie (if found)
;    time0: out, optional, type = time
;           the first time in the time serie (if found)
;    time1: out, optional, type = time
;           the last time in the time serie (if found)
;    nt: out, optional, type = long
;        the number of elements in the time serie
;        
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin}
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          10-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-
function utils_wrf_time, cdfid, time, time0, time1, nt

  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    RETURN, FALSE
  ENDIF  
  
  NCDF_VARGET, cdfid, 'Times', stimes
  ntimes = N_ELEMENTS(stimes[0,*])
  stimes = STRING(stimes[*,0:ntimes-1])    
  
  if stimes[0] eq '0000-00-00_00:00:00' then begin ;Check if geogrid
    NCDF_ATTGET, cdfid, 'TITLE', title, /GLOBAL
    isHere = STRPOS(str_equiv(title), 'GEOGRID')
    if isHere ne -1 then time = QMS_TIME(year = 2000, month = 01, day = 01) else Message, 'Really dont know whate this is'
  endif else begin  
  ;String format : '2008-10-26_12:00:00; length 19
  time = QMS_TIME(YEAR=STRMID(stimes,0,4), MONTH=STRMID(stimes,5,2),DAY=STRMID(stimes,8,2), $
      HOUR=STRMID(stimes,11,2),MINUTE=STRMID(stimes,14,2),SECOND=STRMID(stimes,17,2))
  endelse
  
  nt = N_ELEMENTS(time)
  time0 = time[0]
  time1 = time[nt-1]
  
  return, TRUE

end
;-----------------------------------------------------------------------
;+
; NAME:
;       utils_ncdf_LonLat
;
; PURPOSE:
;       
;       
; CATEGORY:
;       WAVE utils
;
; CALLING SEQUENCE:
;       result = utils_ncdf_LonLat(cdfid, lon, lat)
;
; INPUT:
;       cdfid: the active file cdfid
;
; OUTPUT:
;       lon 
;       lat  
;
; MODIFICATION HISTORY:
;       Written by: FM, 2010
;-
;-----------------------------------------------------------------------
;+
; :Description:
;   This procedure reads lons and lats from a ncdf file, trying several known variable names. 
;   Returns TRUE if time is found, FALSE in all other cases
;   
; :Params:
;    cdfid: in, required, type = long
;           the active file cdfid
;    lon_id: out, optional, type = long
;            the netcdf variable ID of the longitudes (if found)
;    lat_id: out, optional, type = long
;            the netcdf variable ID of the latitudes (if found)
;        
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin}
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          10-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-
function utils_nc_LonLat, cdfid, lon_id, lat_id

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN, FALSE
  ENDIF  
  
  lon_list = ['lon','longitude','lon','longitudes','lons','xlong','xlong_m']
  lat_list = ['lat','latitude' ,'lat','latitudes' ,'lats','xlat' ,'xlat_m']
  
  inq = NCDF_INQUIRE(Cdfid)
  
  lon_id = -1
  lat_id = -1
  for varid = 0, inq.NVARS - 1 do begin
    vname = (NCDF_VARINQ(Cdfid, varid)).name
    p = where(str_equiv(lon_list) eq str_equiv(vname), cnt)
    if cnt ne 0 then lon_id = varid
    p = where(str_equiv(lat_list) eq str_equiv(vname), cnt)
    if cnt ne 0 then lat_id = varid
    if lat_id ge 0 and lon_id ge 0 then break
  endfor
  
  if lat_id lt 0 or lon_id lt 0 then return, FALSE
  
  return, TRUE

end

;+
; :Description:
;    This procedure is a low level WRF pre-processing tool to copy a WRF geo_em file
;    and change the land_cat dimension to 33.
;    The newly created file will be striclty identical exept for the land_cat dimension value,
;    the LANDUSEF variable and the num_land_cat global attribute. 
;
; :Params:
;    file: in, optional, type = string
;          the file to copy. It will not be modified. If not present, a dialog window will open  
; :Keywords:
;    NEW_LUF: in, optional, type = float array
;             The new lu fraction (dimensions: nx, ny, 33). If not set, the original fraction is kept
;             and the classes 25 to 33 are all set to 0. 
;             
;             If set, the mew dominant Category (LU_INDEX) will be computed, as well as the new LANDMASK.
;             TODO: Carefull: SOILCTOP, SOILCBOT, SCT_DOM, SCB_DOM are NOT actualized, but this should be ok...
;             
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 16 Dec 2010 
;       Modified:   16-Dec-2010 FaM
;                   First aparition
;
;-
pro UTILS_usgs_24_to_33, file, NEW_LUF = new_luf

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
;  ON_ERROR, 2

  if N_ELEMENTS(file) eq 0 then file = DIALOG_PICKFILE(TITLE='Please select the geo file to copy.')
  outfile = file + '.33'
  
  ; Check new_luf
  tochange = KEYWORD_SET(new_luf)
  
  if tochange then begin
  
    if ~arg_okay(NEW_LUF, /NUMERIC, N_DIM=3) then message, WAVE_Std_Message('new_luf',/ARG)
    dims = SIZE(new_luf, /DIMENSIONS)
    luf = FLOAT(NEW_LUF)
    dummy = MAX(luf, pd, DIMENSION=3)
    inds = ARRAY_INDICES(luf, pd)
    inds = reform(inds[2, *], dims[0], dims[1])
    dominant = inds + 1
    
    ; if 50-50,  then water
    water = luf[*,*,15]
    pwat = where(water ge 0.5, cnt)
    if cnt ne 0 then dominant[pwat] = 16
   
    ; If water lt 49, then second max
    pnowat = where(dominant eq 16 and water le 0.5, cnt)  
    water[pnowat] = 0.
    myluf = luf
    myluf[*,*,15] = water  
    dummy = MAX(myluf, pd, DIMENSION=3)
    inds = ARRAY_INDICES(myluf, pd)
    inds = reform(inds[2, *], dims[0], dims[1])
    dominant[pnowat] = inds[pnowat] + 1
    undefine, myluf
    
    ; calculate Landmask
    lm = dominant * 0 + 1 
    p = where(dominant eq 16, cnt)
    if cnt ne 0 then lm[p] = 0
    
  end

  sid = Ncdf_open(File, /NOWRITE)        
  inq = NCDF_INQUIRE(sid)
  tid = NCDF_CREATE(outfile, /CLOBBER)
  NCDF_CONTROL, tid, /FILL
    
  for i =0, inq.ndims-1 do begin
  
    NCDF_DIMINQ, sid, i, sName, sSize  
    if str_equiv(sName) eq 'LAND_CAT' then ssize = 33
    
    if str_equiv(sName) eq 'TIME' then dummy = NCDF_DIMDEF(tid, sName, /UNLIMITED) $
    else dummy = NCDF_DIMDEF(tid, sName, ssize)

  endfor ; Dimensions OK
    
  for i =0, inq.Ngatts-1 do begin  
  
    sName = NCDF_ATTNAME(sid, i , /GLOBAL)        
    sAtt_info = NCDF_attINQ(sid, sName, /GLOBAL)
    NCDF_ATTGET, sid, sName, sValue, /GLOBAL
        
    ; Set the appropriate netCDF data type keyword.
    CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1      
    ENDCASE
    
    if str_equiv(sName) eq str_equiv('num_land_cat') then sValue = 33

    ; Add the attribute to the file.
    NCDF_AttPut, tid, sName, svalue, /GLOBAL, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    
  endfor ; Att OK
    
  for svid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,svid)
      
    ; Check the data type to see that it conforms to netCDF protocol.
    CASE StrUpCase(s_var_info.datatype) OF
      'BYTE': tbyte = 1
      'CHAR': tchar = 1
      'DOUBLE': tdouble = 1
      'FLOAT': tfloat = 1
      'LONG': tlong = 1
      'SHORT': tshort = 1
    ENDCASE
        
    ; If the dimension names are present, use them to get the dimension IDs, which are needed to define the variable.
    dimsIds = s_var_info.dim
    ; Define the variable.
    TvID = NCDF_VarDef(tid, s_var_info.name, dimsIds, $
      BYTE=tbyte, $
      CHAR=tchar, $
      DOUBLE=tdouble, $
      FLOAT=tfloat, $
      LONG=tlong, $
      SHORT=tshort)
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    
    if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
    
    ; Copy the variable attributes
    for sattid = 0, s_var_info.NATTS - 1 do begin
    
      sName = NCDF_ATTNAME(sid, svid, sattid)
      sAtt_info = NCDF_attINQ(sid, svid, sName)
      NCDF_ATTGET, sid, svid, sName, sValue
      
      ; Set the appropriate netCDF data type keyword.
      CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
      ENDCASE
      
      if StrUpCase(sAtt_info.DATATYPE) eq 'CHAR' then begin
          isHere = STRPOS(str_equiv(sValue),str_equiv('24-category'))
          if isHere ne -1 then GEN_str_subst, ret, STRING(sValue),'24','33', sValue
          if isHere ne -1 and tochange then GEN_str_subst, ret, STRING(sValue),'USGS','user-defined', sValue          
      endif
      
      ; Add the attribute to the file.
      NCDF_AttPut, tid, TvID, sName, sValue,  $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort     
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    endfor
      
  endfor
  
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
 
  for vid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,vid)
    ndims = s_var_info.ndims
   
    NCDF_VARGET, sid, vid, var
    
    if str_equiv(s_var_info.name) eq str_equiv('LANDUSEF') then begin
      if tochange then var = LUF $
      else begin 
        dim = SIZE(var, /DIMENSIONS)
        var = [[[var]],[[FLTARR(dim[0],dim[1],9)]]]
      endelse
    endif
        
    if str_equiv(s_var_info.name) eq str_equiv('LANDMASK') and tochange then var = FLOAT(lm)
    if str_equiv(s_var_info.name) eq str_equiv('LU_INDEX') and tochange then var = FLOAT(dominant)
    
    NCDF_VARPUT, tid, vid, var

  endfor  
  
  NCDF_CLOSE, sid ; Close source file
  NCDF_CLOSE, tid ; Close file

end


;+
; :Description:
;    This function makes a simple mean aggregation of a 2d array to a small array
;    uisung a given ratio.
;
; :Params:
;    array: in, required, type = float
;           the 2d array to aggregate
;    ratio: in, required, type = long
;           the ratio (it must be a divider of both X and Y dimensions)
; 
; :Examples:
;   A very easy usage:: 
;     IDL> array = [[0,0,1,1],[2,2,1,1],[2,2,2,2],[3,3,3,3]]
;     IDL> print, array
;            0       0       1       1
;            2       2       1       1
;            2       2       2       2
;            3       3       3       3
;     IDL> print, UTILS_aggregate_Grid_data(array, 2)
;            1.0000000       1.0000000
;            2.5000000       2.5000000
; 
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          22-Dec-2010 FaM
;          First apparition
;
;-
function UTILS_aggregate_Grid_data, array, ratio ; TODO: add grid update

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  if ~arg_okay(array, /NUMERIC, /ARRAY, N_DIM=2) then message, WAVE_Std_Message('array',/ARG)
  if ~arg_okay(ratio, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('ratio',/ARG)
     
  siz = SIZE(array, /DIMENSIONS)
  nxin = siz[0]
  nyin = siz[1]
  
  if nxin / DOUBLE(ratio) ne (nxin / ratio) or nyin / DOUBLE(ratio) ne (nyin / ratio) then message, 'Ratio is not a divider of array.'

  nxout = nxin / ratio
  nyout = nyin / ratio
  
  agg = DBLARR(nxout,nyout)
  tempx = DBLARR(nxout,nyin)
    
  for i = 0, nxin-1 do tempx[i/ratio,*] += array[i,*]            
  for j = 0, nyin-1 do agg[*,j/ratio] += tempx[*,j]      

  return, agg / DOUBLE(ratio*ratio)
  
end

function UTILS_COLOR_CONVERT, colors = colors, ncolors = ncolors, cmin = cmin, cmax = cmax, r = r, g = g, b = b, INVERTCOLORS = INVERTCOLORS

  if N_ELEMENTS(cmin) eq 0 then cmin = 0
  if N_ELEMENTS(cmax) eq 0 then cmax = 255
  
  ; Check parameters and keywords.
  IF N_Elements(colors) EQ 0 THEN BEGIN ; NO colors given
    IF N_Elements(ncolors) EQ 0 THEN begin
     _colors = 'white' 
      NCOLORS = 1
    endif else if NCOLORS gt 1 then _colors = Scale_Vector(Indgen(ncolors), cmin, cmax) else _COLORS = cmin
    
  ENDIF ELSE begin ; COLORS given
    if arg_okay(colors, N_DIM=2) then begin
      dims = SIZE(colors, /DIMENSIONS)
      if dims[0] ne 3 then message, WAVE_Std_Message('colors', /ARG)
      _colors = COLOR24(colors)
    endif else _colors = colors
  endelse
  
  ncolors = N_Elements(_colors)
  
  ; I would prefer to draw in 24-bit color if I can, since this way I can
  ; avoid loading colors into the color table. I'll have to see where I am to
  ; see if I can do this in 24-bit color.
  CASE !D.Name OF
    'X': BEGIN
      Device, Get_Visual_Depth=theDepth
      IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
    END
    'WIN': BEGIN
      Device, Get_Visual_Depth=theDepth
      IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
    END
    'Z': BEGIN
      Device, Get_Pixel_Depth=theDepth
      IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
    END
    'PS': BEGIN
      IF Float(!Version.Release) GE 7.1 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      IF supportsTrueColor THEN BEGIN
        Device, DECOMPOSED=1
        theState = 1
      ENDIF
    END
    ELSE: BEGIN
      supportsTrueColor = 0
    END
  ENDCASE
  
  ; Set up the colors for drawing. All 24-bit if it supports true color.
  ; Otherwise load colors for 8-bit support.
  IF supportsTrueColor THEN BEGIN
    CASE Size(_colors, /TNAME) OF
      'STRING': BEGIN
        _colors = FSC_Color(_colors, DECOMPOSED=1, FILE=file)
      END
      'INT': BEGIN
        TVLCT, r, g, b, /GET
        temp = LonArr(ncolors)
        FOR j=0,ncolors-1 DO BEGIN
          temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
        ENDFOR
        _colors = Temporary(temp)
      END
      'ULONG':
      'LONG': BEGIN
      
        ; If the state is NOT using decomposed color, these are probably
        ; color index numbers, rather than long integers to be decomposed.
        IF theState EQ 0 THEN BEGIN
          TVLCT, r, g, b, /GET
          temp = LonArr(ncolors)
          FOR j=0,ncolors-1 DO BEGIN
            temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
          ENDFOR
          _colors = Temporary(temp)
        ENDIF
        
        ; If the maximum value of these long integers in not over 255, then
        ; we can be pretty sure these are color index numbers. At least I'm
        ; going to treat them that way for now and see what kind of trouble I
        ; get in.
        IF Max(_colors) LE 255 THEN BEGIN
          TVLCT, r, g, b, /GET
          temp = LonArr(ncolors)
          FOR j=0,ncolors-1 DO BEGIN
            temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
          ENDFOR
          _colors = Temporary(temp)
        ENDIF
      END
      'BYTE': BEGIN
        TVLCT, r, g, b, /GET
        temp = LonArr(ncolors)
        FOR j=0,ncolors-1 DO BEGIN
          temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
        ENDFOR
        _colors = Temporary(temp)
      END
      ELSE: BEGIN
        TVLCT, r, g, b, /GET
        temp = LonArr(ncolors)
        FOR j=0,ncolors-1 DO BEGIN
          temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
        ENDFOR
        _colors = Temporary(temp)
      END
    ENDCASE
  ENDIF ELSE BEGIN
  
   ; I'd rather not go here
    MESSAGE, 'NO True color?'
  
;    CASE Size(_colors, /TNAME) OF
;      'STRING': BEGIN
;        _colors = FSC_Color(_colors, DECOMPOSED=0, FILE=file)
;      END
;      'LONG': BEGIN
;      
;        ; If the maximum value of these long integers in not over 255, then
;        ; we can be pretty sure these are color index numbers. At least I'm
;        ; going to treat them that way for now and see what kind of trouble I
;        ; get into.
;        IF Max(_colors) GT 255 THEN BEGIN
;          r = _colors AND '0000FF'xL
;          g = ISHFT(_colors AND '00FF00'xL, -8)
;          b = ISHFT(_colors AND 'FF0000'xL, -16)
;          TVLCT, r, g, b, cmin
;          _colors = Indgen(ncolors) + cmin
;        ENDIF
;      END
;      ELSE:
;    ENDCASE
;    
  ENDELSE
  if KEYWORD_SET(INVERTCOLORS) then _colors  = ROTATE(_colors,2)
  utils_color_rgb, _colors, r, g, b  
  return, _colors  
  
end

pro utils_color_rgb, color, r, g, b
  
  UNDEFINE, r, g, b
  r = BYTE(color) * 0B
  g = r
  b = r
  for i = 0, N_ELEMENTS(color)-1 do begin
  
    bi = ROTATE(BitGet(LONG(color[i])), 2)
  
    tr = 0L
    tg = 0L
    tb = 0L
    for j = 0, 7 do tr += bi[j] * 2 ^ j
    for j = 8, 15 do tg += bi[j] * 2 ^ (j-8)
    for j = 16, 23 do tb += bi[j] * 2 ^ (j-16)
     
    r[i] = tr
    g[i] = tg
    b[i] = tb
     
  endfor

end
