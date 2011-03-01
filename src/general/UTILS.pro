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
;
; :History:
;       Written by FaM, 2010
;       
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
;
; :History:
;       Written by FaM, 2010
;-
function utils_acc_to_step, accumulated

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  ON_ERROR, 2
    
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
;    This procedure retrieves the georeferenced equivalent positions from a lat-lon array,
;    using the nearest neighbor algorithm. 
;    
;    Therefore the output is an one dimensional array of size N_ELEMENTS(flon) or [4,N_ELEMENTS(flon)]. 
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
;          The 1 or 2 dimensional array of the longitudes corresponding to desired grid.
;    flat: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the latitudes corresponding to desired grid.
;
; :Keywords:
;    DISTANCES: out, optional
;               set this keyword to a named variable to obtain the distances between 
;               each neirest neighbor (same dim as output)
;    CLASSICAL: in, optional
;               set this keyword to force using the naive approach of the algorithm
;    TRIANGULATION: in, optional
;                   set this keyword to force using the delaunay approach of the algorithm (default)
;    FOURPOINTS: in, optional
;                set this keyword to get the 4-neirest points (currently using the naive approach, very slow)
;                
; :Returns:
;    an array of the same dimensions as flon containing the indexes in of the clothest 
;    point in the input grid. 
;     
;
; :History:
;       Written by FaM, 2010
;-
function utils_nearest_neighbor, ilon, ilat, flon, flat, DISTANCES = distances, $
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
; 
;    This procedure follows the use of `utils_nearest_neighbor`.
;    
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    pos: in, required
;         The positions obtained with a previous call of `utils_nearest_neighbor`.
;    data: in, required
;          The 1 or 2 dimensional array of the data to fit.
;    
; :Returns:
;     the fitted data   
;
; :History:
;       Written by FaM, 2010
;-
function utils_compute_nearest_neighbor, pos, data

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
; 
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
;              set to aggregate only a subset of the file (see 'w_TRMM')
;    SUBSET_LL: in, optional
;              set to aggregate only a subset of the file (see 'w_TRMM')
;    LL_DATUM: in, optional
;               the datum for 'SUBSET_LL'  (see 'w_TRMM')
; 
; :History:
;       Written by FaM, 2010
;-
pro utils_trmm_aggregate_3B42, directory, START_TIME = start_time, END_TIME = end_time, OUTFILE = outfile, $
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
  if ~check_TimeSerie(time) then message, '  The files do not form a continous time serie.'
  
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
  template = OBJ_NEW('w_TRMM', FILE=fileLIST[0], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)
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
  
    t_obj = OBJ_NEW('w_TRMM', FILE=fileLIST[i], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)

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
; TODO: Implement procedure   
;-
pro utils_trmm_aggregate_3B43
    

  
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
; :History:
;       Written by FaM, 2010
;-
function utils_eod_get_metadata, pvlstring, objstring, n_char

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
; 
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
;              to specify the name of the variable which is coards compliant
;         
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :History:
;     Written by FaM, 2010.
;-
function utils_nc_coards_time, cdfid, time, time0, time1, nt, VARNAME = varname

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
       
  is_OLD = FALSE
  if y lt 1900 then begin 
    is_OLD = TRUE
    jd0 = JULDAY(mo,d,y,h,mi,s)
    time0 = QMS_TIME(YEAR=1900, MONTH=01, DAY=01)
    jd1 = TIME_to_JD(time0)
  endif else time0 = QMS_TIME(YEAR=y, MONTH=mo, DAY=d, HOUR = h, MINUTE=mi, SECOND=s, MILLISECOND=milli)
  
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
  
  if is_OLD then begin 
   deltaU = (jd1 - jd0) * D_QMS / fac
   if MIN(u) lt deltaU then deltaU = 0 
  endif else deltaU = 0 
  
  time = time0 + fac * LONG64(u - deltaU)
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
; :History:
;     Written by FaM, 2010.
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
; :History:
;     Written by FaM, 2010.
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
; 
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
; TODO: Update routine: Careful - SOILCTOP, SOILCBOT, SCT_DOM, SCB_DOM are NOT actualized, but this should be ok...
;             
; :History:
;       Written by FaM, 16 Dec 2010 
;-
pro utils_usgs_24_to_33, file, NEW_LUF = new_luf

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
;    using a given ratio.
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
;     IDL> print, utils_aggregate_Grid_data(array, 2)
;            1.0000000       1.0000000
;            2.5000000       2.5000000
;
; :History:
;     Written by FaM, 2010.
;-
function utils_aggregate_Grid_data, array, ratio ; TODO: Update routine: add grid update

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

;+
; :Description:
;    This function converts any colors vector (rgb, strings, indexes in a 
;    color table...) to 24-bit long integers.
;    It is strongly inspired from the Coyote Graphics routines.
;    
; :Returns:
;    A vector of 24-bit long integers.
;
; :Keywords:
;    COLORS: in, optional
;            if it is a 1-dimensional vector, the colors to convert (strings in cgcolor, 
;            indexes in the current color table, etc.)
;            if it is a two dimensional vector, it must be 3xN dimensional whereas the three 
;            first columns are the RGB values. 
;            it not set, the colors will be deduced from the other keywords
;    NCOLORS: in, optional
;             the number of colors in the output vector (only if COLORS is not set)
;    CMIN: in, optional
;          The lowest color index of the colors to be loaded in
;          the color vector (only if COLORS is not set)
;    CMAX: in, optional
;          The highest color index of the colors to be loaded in
;          the color vector (only if COLORS is not set)
;    INVERTCOLORS: in, optional 
;                  Setting this keyword inverts the colors in the color vector (only if COLORS is not set)
;    R: out, optional
;       the equivalent Rgb triplet
;    G: out, optional
;       the equivalent rGb triplet
;    B: out, optional
;       the equivalent rgB triplet
;
; :History:
;     Written by FaM, 2011.
;-
function utils_color_convert, COLORS = colors, NCOLORS = ncolors, CMIN = cmin, CMAX = cmax, INVERTCOLORS = INVERTCOLORS, R = r, G = g, B = b

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2

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
      if dims[1] ne 3 then message, WAVE_Std_Message('colors', /ARG)
      _colors = COLOR24(colors)
    endif else _colors = colors
  endelse
  
  ncolors = N_Elements(_colors)
  
  ; I would prefer to draw in 24-bit color
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
  IF supportsTrueColor THEN BEGIN
    CASE Size(_colors, /TNAME) OF
      'STRING': BEGIN
        _colors = cgColor(_colors, DECOMPOSED=1, FILE=file)
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
  ENDELSE
  
  if KEYWORD_SET(INVERTCOLORS) then _colors  = ROTATE(_colors, 2)
  
  utils_color_rgb, _colors, r, g, b  
  return, _colors  
  
end

;+
; :Description:
;    Converts a 24 bit color vector into a rgb triple.
;
; :Params:
;    color: in, required, type = LONG
;           the colors to converts
;    r: out
;    g: out
;    b: out
;    
; :History:
;     Written by FaM, 2011.
;-
pro utils_color_rgb, color, r, g, b
   
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
   
  if ~ arg_okay(color, TYPE=IDL_LONG) then message, WAVE_Std_Message('color', /ARG)
    
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

;+
; :Description:
;    To generate an ASCII template from an ascci file.
;
; :Keywords:
;    FILE: in, optional
;          the ascii file to template (if not set, a dialog window will open)
;
; :History:
;     Written by FaM, 2010.
;     
;-
pro utils_make_template, FILE = file

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  if ~KEYWORD_SET(file) then file = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file to template', /FIX_FILTER, /MUST_EXIST)
  
  template = ASCII_TEMPLATE(file)

  GEN_str_subst, ret, file, '.dat', '.tpl', tpl_file_path
  SAVE, template, filename=tpl_file_path
  ok = DIALOG_MESSAGE('File: ' + tpl_file_path + ' saved')
  
end

;+
; :Description:
;    Compares two {TNT_ELLIPSOID} structuress
;
; :Params:
;    ell1: in, required
;           the first ell
;    ell2: in, required
;           the second ell
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the ell are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_ellipsoid, ell1, ell2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(ell1, STRUCT={TNT_ELLIPSOID}) then return, 0
  if not arg_okay(ell2, STRUCT={TNT_ELLIPSOID}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(ell1.name) ne  str_equiv(ell2.name)  then return, 0 
  if ABS(ell1.a - ell2.a) gt epsilon then return, 0 
  if ABS(ell1.b - ell2.b) gt epsilon then return, 0 
            
  return, 1
    
end

;+
; :Description:
;    Compares two {TNT_DATUM} structuress
;
; :Params:
;    dat1: in, required
;           the first dat
;    dat2: in, required
;           the second dat
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the dat are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_datum, dat1, dat2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(dat1, STRUCT={TNT_DATUM}) then return, 0
  if not arg_okay(dat2, STRUCT={TNT_DATUM}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(dat1.name) ne  str_equiv(dat2.name)  then return, 0 
  if ~ utils_compare_ellipsoid(dat1.ellipsoid, dat1.ellipsoid, epsilon = epsilon) then return, 0 
  if ABS(dat1.dx - dat2.dx) gt epsilon then return, 0 
  if ABS(dat1.dy - dat2.dy) gt epsilon then return, 0 
  if ABS(dat1.dz - dat2.dz) gt epsilon then return, 0 
            
  return, 1
    
end

;+
; :Description:
;    Compares two {TNT_PROJ} structuress
;
; :Params:
;    proj1: in, required
;           the first proj
;    proj2: in, required
;           the second proj
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the proj are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_proj, proj1, proj2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(proj1, STRUCT={TNT_PROJ}) then return, 0
  if not arg_okay(proj2, STRUCT={TNT_PROJ}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(proj1.name) ne  str_equiv(proj2.name)  then return, 0 
  if ~ utils_compare_datum(proj1.datum, proj2.datum, epsilon = epsilon) then return, 0 
  if proj1.idx ne proj2.idx then return, 0 
  if ABS(proj1.a - proj2.a) gt epsilon then return, 0 
  if ABS(proj1.b - proj2.b) gt epsilon then return, 0 
  if ABS(proj1.azi - proj2.azi) gt epsilon then return, 0 
  if total(ABS(proj1.lat - proj2.lat)) gt 3*epsilon then return, 0 
  if total(ABS(proj1.lon - proj2.lon)) gt 3*epsilon then return, 0 
  if total(ABS(proj1.xy - proj2.xy)) gt 2*epsilon then return, 0 
  if ABS(proj1.h - proj2.h) gt epsilon then return, 0 
  if total(ABS(proj1.sp - proj2.sp)) gt 2*epsilon then return, 0 
  if proj1.zone ne proj2.zone then return, 0 
  if proj1.flag ne proj2.flag then return, 0 
  if ABS(proj1.k0 - proj2.k0) gt epsilon then return, 0 
  if ABS(proj1.rot - proj2.rot) gt epsilon then return, 0 
  if total(ABS(proj1.shp - proj2.shp)) gt 2*epsilon then return, 0 
  if ABS(proj1.ang - proj2.ang) gt epsilon then return, 0   
  if total(ABS(proj1.som - proj2.som)) gt 2*epsilon then return, 0 
  if total(ABS(proj1.sat - proj2.sat)) gt 2*epsilon then return, 0 
  if str_equiv(proj1.envi) ne  str_equiv(proj2.envi)  then return, 0 
          
  return, 1
    
end

;+
; :Description:
;    Compares two w_grid_2d objects
;
; :Params:
;    grid1: in, required
;           the first grid
;    grid2: in, required
;           the second grid
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the grids are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_grid, grid1, grid2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
    
  if ~ OBJ_VALID(grid1) then return, 0
  if ~ OBJ_ISA(grid1, 'w_Grid2D') then  return, 0
  if ~ OBJ_VALID(grid2) then return, 0
  if ~ OBJ_ISA(grid2, 'w_Grid2D') then  return, 0
  
  grid1->GetProperty, TNT_C=c1
  grid2->GetProperty, TNT_C=c2
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if ABS(c1.nx - c2.nx) gt 0 then return, 0
  if ABS(c1.ny - c2.ny) gt 0 then return, 0
  if ABS(c1.dx - c2.dx) gt epsilon then return, 0
  if ABS(c1.dy - c2.dy) gt epsilon then return, 0
  if ABS(c1.x0 - c2.x0) gt epsilon then return, 0
  if ABS(c1.y0 - c2.y0) gt epsilon then return, 0
  if ABS(c1.y1 - c2.y1) gt epsilon then return, 0
  if ABS(c1.x1 - c2.x1) gt epsilon then return, 0
  if ~ utils_compare_proj(c1.proj, c2.proj, epsilon = epsilon)  then return, 0 
        
  return, 1
    
end


;+
; :Description:
;    This functions combines one or more grids to make the "biggest possible"
;    grid with it.
;
; :Params:
;    grids: in, required
;           an array of grid objects
;
; :Returns:
;    a w_grid_2d object, mosaic of the input grids.
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_MOSAIC_grid, grids


  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
;  ON_ERROR,2
  
  if N_ELEMENTS(grids) eq 0 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~ OBJ_VALID(grids[0]) then MESSAGE, WAVE_Std_Message('grids', /ARG)
  if ~ OBJ_ISA(grids[0], 'w_Grid2D') then MESSAGE, WAVE_Std_Message('grids', /ARG)
  
  (grids[0])->getProperty, tnt_c = c  , meta = meta
  x0 = c.x0
  y0 = c.y0
  x1 = c.x1
  y1 = c.y1
  dx = c.dx
  dy = c.dy
  proj = c.proj
  
  for i = 1, N_ELEMENTS(grids)-1 do begin
    if ~ OBJ_VALID(grids[i]) then MESSAGE, WAVE_Std_Message('grids', /ARG)
    if ~ OBJ_ISA(grids[i], 'w_Grid2D') then MESSAGE, WAVE_Std_Message('grids', /ARG)
    (grids[i])->getProperty, tnt_c = c  
    if ~ utils_compare_proj(proj, c.proj) then MESSAGE, 'Projections do not match.'
    if ABS(dx - c.dx) gt (MACHAR()).eps then MESSAGE, 'Dxs do not match.'
    if ABS(dy - c.dy) gt (MACHAR()).eps then MESSAGE, 'Dys do not match.'
    x0 = min([x0,c.x0]) 
    y0 = max([y0,c.y0])
    x1 = max([x1,c.x1])
    y1 = min([y1,c.y1])    
  endfor
  
  return, OBJ_NEW('w_Grid2D', x0=x0, y0=y0, x1=x1, y1=y1, dx=dx, dy=dy, PROJ=proj, META=meta + ' mosaic')
      
end


;+
; :Description:
;    Transform ater vapor mixing ratio e.g from WRF output ([kg/kg])
;    to relative humidity
;
; :Params:
;    qv: in, required
;        Water vapor mixing ratio in [kg/kg].
;    p: in, required
;        Full pressure (perturbation + base state pressure) with the same dimension structure as qv. Units must be [Pa]. 
;    t: in, required
;        Temperature in [K] with the same dimension structure as qv
;
;  :Returns:
;    Relative humidity [%]
;
; :History:
;     Written by FaM, 2010.
;-
function utils_qv_to_rh, qv, p, t
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not array_processing(qv, p, t) then message, WAVE_Std_Message(/ARG)

  SVP1=0.6112D
  SVP2=17.67D
  SVP3=29.65D
  SVPT0=273.15D
  
  ;      DOUBLE PRECISION QVS,ES,PRESSURE,TEMPERATURE
  ;      DOUBLE PRECISION EP_2,R_D,R_V
  R_D=287.D
  R_V=461.6D
  EP_2=R_D/R_V
  
  EP_3=0.622D
  
  PRESSURE = P
  TEMPERATURE = T
  
  ES = 10.D*SVP1*EXP(SVP2* (TEMPERATURE-SVPT0)/(TEMPERATURE-SVP3))
  
  QVS = EP_3 * ES / (0.01D * PRESSURE - (1.D - EP_3)*ES)
  
  RH = 100.D*((QV/QVS < 1.0D) > 0.0D)
  
  RETURN, rh
  
END
