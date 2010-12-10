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
;    NOREDIM: in, optional, type=string, default=none
;    CLASSICAL: in, optional, type=string, default=none
;    TRIANGULATION: in, optional, type=string, default=none
;    FOURPOINTS: in, optional, type=string, default=none
;    DELINPUT: in, optional, type=string, default=none
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
function utils_POS_NEAREST_NEIGHBORHOOD, ilon, ilat, flon, flat, DISTANCES = distances, NOREDIM = NOREDIM, $
                  CLASSICAL = classical, TRIANGULATION = triangulation, FOURPOINTS = fourpoints, DELINPUT = delinput

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  ;********
  ; Check *
  ;********
  
  if n_params() ne 4 then MESSAGE, WAVE_Std_Message(/NARG)
  
  if not arg_okay(flon, /NUMERIC) or not arg_okay(flat, /NUMERIC) or not arg_okay(ilon, /NUMERIC) or not arg_okay(ilat, /NUMERIC) then $
    MESSAGE,'lat and/or lons not in numerical format'
  
  if SIZE(flon, /N_DIMENSIONS) gt 2 or SIZE(flat, /N_DIMENSIONS) gt 2 or SIZE(ilon, /N_DIMENSIONS) gt 2 or SIZE(ilat, /N_DIMENSIONS) gt 2 $
     or SIZE(flon, /N_DIMENSIONS) ne SIZE(flat, /N_DIMENSIONS) or SIZE(ilon, /N_DIMENSIONS) ne SIZE(ilat, /N_DIMENSIONS) then $
       MESSAGE,'lat and/or lons not in the right dimension, or dimensions not agree'

  
  ;****************************************
  ; Make input and output Matrix same dim *
  ;****************************************
  
  if KEYWORD_SET(DELINPUT) then begin
    tilat = TEMPORARY(ilat)
    tilon = TEMPORARY(ilon)
  endif else begin
    if SIZE(ilon, /N_DIMENSIONS) eq 1 and ~KEYWORD_SET(NOREDIM) then begin
      inx = n_elements(ilon)
      iny = n_elements(ilat)
      tilat = MATRIX_MULTIPLY(dblarr(inx) +1d, DOUBLE(ilat)) ; "the georef-eq" 2-dimensional array
      tilon = MATRIX_MULTIPLY(DOUBLE(ilon), dblarr(iny)+1d) ; "the georef-eq" 2-dimensional array
    endif else if SIZE(ilon, /N_DIMENSIONS) eq 2 or KEYWORD_SET(NOREDIM) then begin
      tilat = ilat & tilon = ilon
    endif else MESSAGE,'Input lat and lons not of supported dimension'
  endelse
  
  if KEYWORD_SET(DELINPUT) then begin
    tflat = TEMPORARY(flat)
    tflon = TEMPORARY(flon)
  endif else begin
      tflat = flat &  tflon = flon
  endelse
  
  ;********************
  ; Define output dim *
  ;********************  
  n = N_ELEMENTS(tflon)
  IF KEYWORD_SET(FOURPOINTS) then begin
    if n eq 1 then begin
      out = LONARR(4) & distances = DBLARR(4)
    endif else out = lonarr(4,n) &  DISTANCES=DBLARR(4,n)
  endif else begin
    if n eq 1 then begin
      out = 0L & distances = 0D
    endif else out = lonarr(n) &  DISTANCES=DBLARR(n)
  endelse 

  ;********************
  ; Go threw Keywords *
  ;********************  
  algo = ''
  if KEYWORD_SET(CLASSICAL) then algo = 'CLASSICAL' $
    else if KEYWORD_SET(TRIANGULATION) then algo = 'TRIANGLE' $
      else begin
        if n eq 1 then algo = 'CLASSICAL' else algo = 'TRIANGLE'        
      endelse
  
  if KEYWORD_SET(FOURPOINTS) then algo += '_F'
  
  ;**********
  ; Lets go *
  ;**********
    tilon = double(tilon) & tilat = double(tilat)
    tflon = double(tflon) & tflat = double(tflat)

  
  CASE algo OF
  
    'CLASSICAL': begin
    
      for i = 0l, n - 1 do begin      
        quad = (tflon[i] - tilon)^2 + (tflat[i]- tilat)^2
        minquad = min(quad, p)
        if N_ELEMENTS(p) gt 1 then p = p[0] ; it happens.....       
        out[i] = p 
        DISTANCES[i] = minquad        
      endfor
      
    end
    
    'CLASSICAL_F': begin
    
      for i = 0l, n - 1 do begin
        quad = (tflon[i] - tilon)^2 + (tflat[i]- tilat)^2
        s = sort(quad)
        out[*,i] = s[0:3]
        DISTANCES[*,i] = quad[s[0:3]]
      endfor
      
    end
    
    'TRIANGLE': begin
    
      n1 = n_elements(tilon)
      x1 = tilon[*] & y1 = tilat[*]
      x2 = tflon[*] & y2 = tflat[*]    
      
      triangulate, x1, y1, c ; Compute Delaunay triangulation     
       
      out = GRIDDATA(x1,y1,LINDGEN(n1), XOUT=x2, YOUT=y2, /NEAREST_N, TRIANGLES =c)
      if arg_present(DISTANCES) then distances = (tilon[out] - x2)^2 + (tilat[out] - y2)^2         
      
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
    if arg_present(DISTANCES) then DISTANCES=sqrt(DISTANCES)
    
    return, out
  
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
;    output_size: out, type= ,default=none
;          The data fitted with nearest neighborhood.
;    
; TODO: Define types etc.!
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
function utils_COMPUTE_NEAREST_NEIGHBORHOOD, pos, data, output_size

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  if n_params() lt 2 then message, WAVE_Std_Message(/NARG)
  
  if not arg_okay(pos, /NUMERIC) then  message, 'pos not in num format'
  
  if SIZE(pos, /N_DIMENSIONS) gt 1 then  message, 'pos not a one dimensional array'
  
  if N_ELEMENTS(output_size) eq 0 then output_size = N_ELEMENTS(pos)
  
  if N_ELEMENTS(output_size) eq 2 then out = dblarr(output_size[0],output_size[1]) else out = dblarr(output_size[0])
  if N_ELEMENTS(pos) ne N_ELEMENTS(out) then message, 'pos and output_size do not match?'
  
    
  out[*] = data[pos]
    
  ;********
  ;* DONE *
  ;********

  return, out
  
end

;+
; :Description:
;    This procedure reads all TRMM netcdf files from a directory, sorts them by date
;       and aggregates the precipitation field. 
;       
;    !!!! Carefull: not tested since a long time
;       
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    fname: in, required, type=string , default=none
;           any TRMM file in the directory to aggregate
; :Keywords:
;    SUBSET: in, optional, default=none
;    VERBOSE: in, optional, default=none
;    NOSHIFT: in, optional, default=none
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
pro utils_TRMM_aggregate, file, outfile, SUBSET = subset, VERBOSE = VERBOSE, NOSHIFT = noshift

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
 
  if N_ELEMENTS(file) eq 0 then file = DIALOG_PICKFILE(TITLE='Please select TRMM ncdf file to aggregate', /MUST_EXIST)  
  path = FILE_DIRNAME(file)
  pattern = '*.nc'
  filelist = file_search(path, pattern, COUNT=cnt)
  filelist = filelist[SORT(filelist)]
  
  if KEYWORD_SET(subset) then mysubs = subset
  
  t0 = OBJ_NEW('TRMM_nc', FILE=file, SUBSET_IJ=mysubs)
  t0->GetProperty, type = type
  t0->Get_LonLat, lon, lat  
  OBJ_DESTROY, t0
  
  undefine, mysubs
  if KEYWORD_SET(subset) then mysubs = subset
  
  if KEYWORD_SET(VERBOSE) then print, 'Selected file is of type: ' + type + '. All other files will be ignored.'
  
  for f = 0, N_ELEMENTS(filelist)-1 do begin
    t0 = OBJ_NEW('TRMM_nc', FILE=filelist[f], SUBSET_IJ=mysubs)
    if not OBJ_VALID(t0) then begin
      if KEYWORD_SET(VERBOSE) then print, FILE_BASENAME(filelist[f]) + ' not recognized. Ignored.'
      continue
    end
    t0->GetProperty, type = type0
    if str_equiv(type0) ne str_equiv(type) then begin
      if KEYWORD_SET(VERBOSE) then print, FILE_BASENAME(filelist[f]) + ' not of correct type. Ignored.'
      OBJ_DESTROY, t0
      continue
    end
    time0 = t0->get_time()    
    if N_ELEMENTS(time) eq 0 then time = time0 else time = [time, time0]
    pcp0 = 0 > t0->get_prcp() ; Remove missing vals
    if N_ELEMENTS(pcp) eq 0 then pcp = pcp0 else pcp = [[[pcp]], [[pcp0]]]
       
    if KEYWORD_SET(VERBOSE) then print, TIME_to_STR(time0) + ' read.'
    OBJ_DESTROY, t0
          
  endfor
  if KEYWORD_SET(VERBOSE) then print, ''
  iqms = time.qms
  s = sort(iqms)
  iqms = iqms[s]
  pcp = pcp[*,*,s]
  
  check = check_TS(iqms, timestep)
  if check ne TRUE then MESSAGE, 'Time serie not complete!'
  
  ni = N_ELEMENTS(iqms)
 
  if type eq '3B42_h' then begin
    if  KEYWORD_SET(NOSHIFT) then begin
     fqms = iqms
     fpcp = pcp*3.
    endif else begin
      if KEYWORD_SET(VERBOSE) then print, '3B42 files need a 1H30 shift. This is done here.'
      fqms0 = iqms[1]
      fqms1 = iqms[ni-1]
      fqms = INDGEN(ni-1,/L64) * 3LL * H_QMS + fqms0
      if fqms1 ne fqms[N_ELEMENTS(fqms)-1] then message, 'done something wrong'      
      fpcp = (pcp[*,*,0:ni-2] + pcp[*,*,1:ni-1]) * 3. / 2.
      if N_ELEMENTS(fpcp[0,0,*]) ne N_ELEMENTS(fqms) then message, 'done something wrong here too'
    endelse

    if KEYWORD_SET(VERBOSE) then print, 'Start time : ' + TIME_to_STR(fqms0)
    if KEYWORD_SET(VERBOSE) then print, 'End time : ' + TIME_to_STR(fqms1)
    
    nf = N_ELEMENTS(fqms)
        
    if N_ELEMENTS(fname) eq 0 then begin
      if KEYWORD_SET(NOSHIFT) then fname = path + STRMID(type,0,4) + '_agg_noshift_' + STRMID(TIME_to_STR(fqms[0]),0,10) + '.nc' $
       else  fname = path + STRMID(type,0,4) + '_agg_' + STRMID(TIME_to_STR(fqms[0]),0,10) + '.nc' 
    endif
    if KEYWORD_SET(VERBOSE) then print, 'Start to fill the file : ' + fname
    tid = NCDF_CREATE(fname, /CLOBBER)
    NCDF_CONTROL, tid, /FILL
    
    ;Define dimensions
    dimTimeid  = NCDF_DIMDEF(tid, 'time', nf)
    dimLonid  = NCDF_DIMDEF(tid, 'lon', N_ELEMENTS(lon[*,0]))
    dimLatid  = NCDF_DIMDEF(tid, 'lat', N_ELEMENTS(lat[0,*]))
    
    ; Define tvariables
    Timeid = NCDF_VarDef(tid, 'time', dimTimeid, /LONG)
      ; Add attributes
      NCDF_AttPut, tid, Timeid, 'zone', 'UTC', /CHAR
      d = TIME_to_STR(fqms[0])
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
    NCDF_AttPut, tid, 'time_info', 'The value at each UTC step is the accumulated prcp over the three PREVIOUS hours (mm 3hrs-1).', /GLOBAL, /CHAR
    NCDF_AttPut, tid, 'source', 'TRMM 3B42 three-hourly product.', /GLOBAL, /CHAR
    NCDF_AttPut, tid, 'title', 'TRMM 3B42 agg file', /GLOBAL, /CHAR
    if ~ KEYWORD_SET(SUBSET) then subset = [0,0,0,0]
    NCDF_AttPut, tid, 'subset', 'subset = ['+ str_equiv(subset[0]) +', '+ str_equiv(subset[1]) +', '+ str_equiv(subset[2]) +', '+ str_equiv(subset[3]) +']', /GLOBAL, /CHAR
    
    NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
    hours = INDGEN(nf, /LONG) * 3L    
    NCDF_VARPUT, tid, Timeid, hours
    NCDF_VARPUT, tid, Lonid, REFORM(lon[*,0])
    NCDF_VARPUT, tid, Latid, REFORM(lat[0,*])
    NCDF_VARPUT, tid, pcpid, fpcp
    
    if KEYWORD_SET(VERBOSE) then print, '... done.'

    NCDF_CLOSE, tid
    
  endif else message, 'Currently only 3B42 product can be aggregated ... ' 
  
  ;TODO: propose other features

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
; :Examples:
;        
;     
; TODO: complete description
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
function utils_nc_COARDS_time, cdfid, time, time0, time1, nt

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
   milli = LONG64((DOUBLE(t[2]) - FLOOR(t[2])) * 1000D)
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
