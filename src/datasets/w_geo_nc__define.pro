; docformat = 'rst'
;+
; 
; w_GEO_nc is the basis class for all kinds of NCDF files that 
; contain geolocalisation data. It will try to detect the geolocalistion
; info from both dimensions and variables, as well as the
; time dimension if present. 
; 
; This is achieved by checking if the NetCDF file follows the COARDS conventions,
; and by verifiying some dimension names against a list of known suitable names.
; 
; TODO: Update routine: currently this list is hard coded, it would be better using configuration files
; 
; The X, Y and time coordinates are then available through the 'get_ncdf_coordinates'
; and 'get_time' methods. Also, the 'QuickPlotVar' method is extended to include 
; the geolocaliation info.
; 
; The major feature of this class is to encapsulate the 
; 'NCDF::get_Var()' method into a higher level tool which allows automatic subseting. 
; 
; The subset has to be previously defined with the 'define_subset' method. 
; Subseting is realized when reading the data from the NCDF files, 
; which spares a lot of memory. All geolocalised variables will be subsetted the same 
; way, so that the user does not have to worry about this once the object is initialized.
; 
; Usually, the 'define_subset' method will be redefined by the child classes in a "handier" way.
;  
; :Properties: 
;    XID: in, optional, type = integer
;         ncdf ID of the X dimension
;    YID: in, optional, type = integer
;         ncdf ID of the Y dimension
;    TID: in, optional, type = integer
;         ncdf ID of the T dimension (-1 if not found)
;    time: in, optional, type = ptr
;          the time in qms (-1 if not found)  
;    t0: in, optional, type = LL64
;          first time in qms         (-1 if not found)   
;    t1: in, optional, type = LL64
;          end time in qms         (-1 if not found)   
;    nt: in, optional, type = interger
;          number of elements in time
;    cropped: in, optional, type = string
;          either 'FALSE' or 'TRUE', or set by child classes. if not 'FALSE', the data will be cropped at reading
;    subset: in, optional, type = integer array 
;              Four elements array::              
;                first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.
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
;     Written by FaM, 2010.
;-


;+
; :Description:
; 
;   Defines the attributes of the class. Attributes::
;   
;        w_GEO_nc                      
;            INHERITS w_NCDF               
;            XID : 0L                    
;            YID : 0L                    
;            TID : 0L                    
;            time: ptr_new()           
;            t0  : 0LL                   
;            t1  : 0LL                   
;            nt  : 0L                    
;            cropped :   ''             
;            subset : [0L,0L,0L,0L]        
;            
;            
; :Categories:
;         WAVE/OBJ_GIS   
;
; :History:
;     Written by FaM, 2010.
;-
PRO w_GEO_nc__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {w_GEO_nc                    , $
            INHERITS w_NCDF             , $
            XID : 0L                    , $
            YID : 0L                    , $
            ZID : 0L                    , $
            TID : 0L                    , $
            time: ptr_new()             , $ 
            t0  : 0LL                   , $
            t1  : 0LL                   , $
            nt  : 0L                    , $
            cropped :   ''              , $
            subset : [0L,0L,0L,0L]        $ 
            }
    
END

;+
; :Description:
;       Build function. 
;
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;    FILE: in, optional, type = string
;          path to the ncdf file
;    SUBSET: in, optional, type = integer array 
;              Four elements array::              
;                first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.
;                
; :Returns:              
;     1 if the object has been created succesfully.
;     
; :History:
;      Written by FaM, 2010.
;-
Function w_GEO_nc::Init, FILE = file, SUBSET = subset
           
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    if self.cdfid gt 0 then ncdf_close, self.cdfid
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select GEO NCDF file to read', /MUST_EXIST)  
  if file eq '' then MESSAGE, WAVE_Std_Message(/FILE)
  IF NOT self->w_NCDF::Init(file = file) THEN RETURN, 0    
  
  ;*****************************
  ; Check  dimensions validity *
  ;*****************************
  xp = ['west_east','lon','longitude','longitudes','lons','xlong','xlong_m', 'dimlon','x','lon_3','long', 'phony_dim_0']
  yp = ['south_north','lat','latitude' ,'latitudes' ,'lats','xlat' ,'xlat_m', 'dimlat','y','lat_3', 'phony_dim_1']
  zp = ['levelist','level', 'pressure', 'press', 'zlevel', 'z']
  tp = ['time','times','xtime']
  
  foundX = -1
  foundY = -1
  foundZ = -1
  foundT = -1
  
  for i=0, self.Ndims-1 do begin
    dimn = (*self.dimNames)[i]
    p = where(str_equiv(xp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundX = i
    p = where(str_equiv(yp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundY = i
    p = where(str_equiv(zp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundZ = i
    p = where(str_equiv(tp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundT = i
    if foundX ge 0 and foundY ge 0 and foundZ ge 0 and foundT ge 0 then break
  endfor
  
  if foundX lt 0 or foundY lt 0 then Message, 'X and Y dimensions could not be found.'
  self.XID = foundX
  self.YID = foundY
  self.ZID = foundZ
  self.TID = foundT
  
  self.t0 = -1LL
  self.t1 = -1LL 
  time = -1LL
  if foundT ge 0 then begin
    if utils_nc_COARDS_time(self.cdfid, time, time0, time1, nt) then begin
      self.t0 = time0
      self.t1 = time1
    endif else if utils_wrf_time(self.cdfid, time, time0, time1, nt) then begin
      self.t0 = time0
      self.t1 = time1
    endif else Message, 'Time dimension found but time could not be parsed.', /INFORMATIONAL
  endif  
  self.time = PTR_NEW(time, /NO_COPY) 
  self.nT = N_ELEMENTS(*self.time)
  
  ;***********************
  ; Check  crop variable *
  ;***********************
  if ~self->w_GEO_nc::define_subset(SUBSET = subset) then RETURN, 0 
  
  RETURN, 1
  
END

;+
; :Description:
;    Destroy function. 
;    
; :Categories:
;    WAVE/OBJ_GIS   
;    
; :History:
;      Written by FaM, 2010.
;-
pro w_GEO_nc::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  self->w_NCDF::Cleanup

  PTR_FREE, self.time
  
END

;+
; :Description:
;    Get access to some params. 
;
; :Categories:
;    WAVE/OBJ_GIS   
;
; :Keywords: 
;    XID: out, optional, type = integer
;         ncdf ID of the X dimension
;    YID: out, optional, type = integer
;         ncdf ID of the Y dimension
;    ZID: out, optional, type = integer
;         ncdf ID of the Z dimension (-1 if not found)
;    TID: out, optional, type = integer
;         ncdf ID of the T dimension
;    time: out, optional, type = ptr
;          the time in qms (-1 if not found)  
;    t0: out, optional, type = LL64
;          first time in qms         (-1 if not found)   
;    t1: out, optional, type = LL64
;          end time in qms         (-1 if not found)   
;    nt: out, optional, type = integer
;          number of elements in time
;    cropped: out, optional, type = string
;             'FALSE' or 'TRUE' or more ...
;    subset: out, optional, type = integer array 
;            Four elements array: see file description 
;    _Ref_Extra: 
;        see 'NCDF:GetProperty' 
;        
; :History:
;      Written by FaM, 2010.
;-
PRO w_GEO_nc::GetProperty  , $
            XID = XID    , $
            YID = YID    , $
            ZID = ZID    , $
            TID = TID    , $
            time = time  , $ 
            t0 = t0      , $
            t1 =  t1           , $
            nt =  nt           , $
            cropped = cropped  , $
            subset = subset    ,$
            _Ref_Extra=extra
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  IF Arg_Present(XID) NE 0 THEN XID = self.XID
  IF Arg_Present(YID) NE 0 THEN YID = self.YID
  IF Arg_Present(ZID) NE 0 THEN ZID = self.ZID
  IF Arg_Present(TID) NE 0 THEN TID = self.TID
  IF Arg_Present(time) NE 0 THEN time = *self.time
  IF Arg_Present(t0) NE 0 THEN t0 = self.t0
  IF Arg_Present(t1) NE 0 THEN t1 = self.t1
  IF Arg_Present(nt) NE 0 THEN nt = self.nt
  IF Arg_Present(cropped) NE 0 THEN cropped = self.cropped
  IF Arg_Present(subset) NE 0 THEN subset = self.subset
  
  self->w_NCDF::GetProperty, _Extra=extra
    
end

;+
; :Description:
;   Retrieve time info.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Params:
;    time: out, type = QMS
;          the time in qms (-1 if not found)  
;    nt: out, type = integer
;        number of elements in time
;    t0: out, type = LL64
;        first time in qms         (-1 if not found)   
;    t1: out, type = LL64
;        end time in qms         (-1 if not found) 
;        
; :History:
;      Written by FaM, 2010.
;-
pro w_GEO_nc::get_time, time, nt, t0, t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  time = *self.time
  nt = self.nt
  t0 = self.t0
  t1 = self.t1
  
end

;+
; :Description:
;    This function reads a variable from the netcdf file and makes a
;    subset of it if it has been previously set with 'define_subset'.
;    
;    If the time dimension has been found, there is the possibility to
;    restrict the retrieved variable time serie to a given period 
;    (keyword 'T0' and 'T1')
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Params:
;    Varid: in, required, type = string/integer
;           the variable ID (string or integer) to retrieve
;    time:  out, type = qms
;           the variable times
;    nt: out, type = long
;        the variable number of times
;
; :Keywords:
;   T0: in, optional, type = qms/{ABS_DATE}
;       if set, it defines the first time of the variable timeserie
;   T1: in, optional, type = qms/{ABS_DATE}
;       if set, it defines the last time of the variable timeserie
;   HOUROFDAY: in, optional, type = long
;              to get strides of the time serie at a specific hour of the day
;   MINUTEOFHOUR: in, optional, type = long, default=0
;                 together with HOUROFDAY, it defines a stride to get 
;                 (important for half hourly values for example)
;   MONTHOFYEAR: in, optional, type=long
;                to get strides of the time serie at a specific month
;   DAYOFMONTH: in, optional, type = long, default=1
;                together with MONTHOFYEAR, it defines a stride to get 
;                (get every 15th of each month for example (rare))
;   STATIC: in, optional, boolean
;           set this keyword to automaticaly retrieve only the first
;           element of the variable in the time dimension
;   ZLEVELS: in, optional, type = long
;            set this keyword to an array of one or two elements, containing the range
;            of the indexes to keep from the original NCDF file in the Z dimension.
;            It is better to use the `ZDIM_ID` keyword to specify
;            in which dimension these indexes must be kept, otherwise `get_Var` will try
;            to do its best from what it knows 
;            (in most cases -4D arrays-, it should work fine).
;   ZDIM_ID: in, optional, type = long/string
;            the dimension name (or ID) to crop (see `ZLEVELS`)   
;   description: out, type = string
;                If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions (if the variable is cropped, the dimensions are updated too)
;   dimnames: out, type = string
;             the dimensions names (if the variable is cropped, the dimension names are updated too)
; 
; :Returns:
;         The variable
;
; :History:
;      Written by FaM, 2010.
;-
function w_GEO_nc::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to w_GEO_nc_VARDEF or w_GEO_nc_VARID, or the name of the variable. 
                            time,  $
                            nt,  $
                            T0=t0, $
                            T1=t1, $
                            MONTHOFYEAR=monthofyear, $
                            DAYOFMONTH=dayofmonth, $
                            HOUROFDAY=hourofday, $
                            MINUTEOFHOUR=minuteofhour, $
                            STATIC=static , $
                            ZLEVELS=zlevels, $
                            ZDIM_ID=zdim_id, $
                            units = units, $
                            description = description, $
                            varname = varname , $ 
                            dims = dims, $ 
                            dimnames = dimnames, $
                            caching = caching 
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  undefine, count, offset
  value = -1
  
  if ~self->w_NCDF::get_Var_Info (Varid, $
    out_id = vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
  
  if KEYWORD_SET(STATIC) then begin
   T0 = self.t0
   T1 = self.t0   
  endif
    
  ndims = N_ELEMENTS(dims)
  count = LONARR(ndims) - 1
  offset = LONARR(ndims) - 1
  stride = LONARR(ndims) + 1
  
  if self.cropped ne 'FALSE' and self.cropped ne '' then begin
    p = where(dimnames eq (*self.dimNames)[self.XID], cnt)
    if cnt ne 0 then begin
      offset[p[0]] = self.subset[0]
      count[p[0]] = self.subset[1]
    endif
    p = where(dimnames eq (*self.dimNames)[self.YID], cnt)
    if cnt ne 0 then begin
      offset[p[0]] = self.subset[2]
      count[p[0]] = self.subset[3]
    endif
  endif else begin ; still fill the offests with standard values for later  
    p = where(dimnames eq (*self.dimNames)[self.XID], cnt)
    if cnt ne 0 then begin
      offset[p[0]] = 0
      count[p[0]] = dims[p[0]]
    endif
    p = where(dimnames eq (*self.dimNames)[self.YID], cnt)
    if cnt ne 0 then begin
      offset[p[0]] = 0
      count[p[0]] = dims[p[0]]
    endif
  endelse
  
  time = *self.time
  if self.TID ge 0 and time[0] gt -1 then begin ; We found the time dimension in the file
    
    p = where(dimnames eq (*self.dimNames)[self.TID], cnt)    
    if cnt ne 0 then begin ; the variable has a time dimension
      p0 = 0
      p1 = self.nt-1
      if check_WTIME(t0, OUT_QMS= it0) and self.nt gt 1 then begin
        v = 0 > VALUE_LOCATE(time, it0) < (self.nt-1)
        p0 = v[0]
      endif
      if check_WTIME(t1, OUT_QMS= it1) and self.nt gt 1  then begin
        v = 0 > VALUE_LOCATE(time, it1) < (self.nt-1)
        p1 = v[0]
      endif
      ; Ok, now set the offset and count accordingly
      time = time[p0:p1]
      offset[p[0]] = p0
      count[p[0]] = p1 - p0 + 1
      
      ; Check for stride
      if N_ELEMENTS(HOUROFDAY) ne 0 then begin
        if N_ELEMENTS(HOUROFDAY) ne 1 then message, 'HOUROFDAY should be a scalar integer'
        SetDefaultValue, minuteofhour, 0
        ok = CHECK_WTIME(time, OUT_ABSDATE=absd)
        phour = where(absd.hour eq hourofday and absd.minute eq minuteofhour, cnthour)
        if cnthour eq 0 then message, 'Hour of day not found in the time serie'
        if cnthour lt 2 then message, 'Youre asking me strange things: not even 2 elements with hour of day?'
        mphour = min(phour)      
        time = time[phour]  
        phour = phour-mphour
        diffs = phour[1:*] - phour[0:cnthour-2]
        dh = phour[1]-phour[0]
        if total(ABS(diffs - dh)) ne 0 then Message, 'Timeserie not regular!'
        offset[p[0]] = offset[p[0]] + mphour
        count[p[0]] = cnthour
        stride[p[0]] = dh           
      endif
      
      if N_ELEMENTS(MONTHOFYEAR) ne 0 then begin
        if N_ELEMENTS(MONTHOFYEAR) ne 1 then message, 'MONTHOFYEAR should be a scalar integer'
        SetDefaultValue, dayofmonth, 1
        ok = CHECK_WTIME(time, OUT_ABSDATE=absd)
        pm = where(absd.month eq monthofyear and absd.day eq dayofmonth, cntm)
        if cntm eq 0 then message, 'Specified month not found in the time serie'
        if cntm lt 2 then message, 'Youre asking me strange things: not even 2 elements with the specified month?'
        mpm = min(pm)      
        time = time[pm]  
        pm = pm-mpm
        diffs = pm[1:*] - pm[0:cntm-2]
        dh = pm[1]-pm[0]
        if total(ABS(diffs - dh)) ne 0 then Message, 'Timeserie not regular!'
        offset[p[0]] = offset[p[0]] + mpm
        count[p[0]] = cntm
        stride[p[0]] = dh
      endif
      
    endif else begin ; the variable has no time dimension
      time = self.t0
    endelse
    
  endif  
  nt = N_ELEMENTS(time)
  
  ; Z cropping
  if N_ELEMENTS(ZLEVELS) ne 0 then begin
     
    if N_ELEMENTS(ZDIM_ID) ne 0 then begin
      if arg_okay(zdim_id, TYPE=IDL_STRING, /SCALAR) then begin
        _zid = zdim_id
      endif else if arg_okay(zdim_id, /NUMERIC, /SCALAR) then begin
        if zdim_id ge self.Ndims or zdim_id lt 0 then Message, '$ZDIM_ID not valid'
        _zid = (*self.dimNames)[zdim_id]
      endif else Message, '$ZDIM_ID not valid'
      
      pz = where(dimnames eq _zid, cnt)
      if cnt ne 1 then Message, '$ZDIM_ID not valid'
      if offset[pz] ne -1 then Message, '$ZDIM_ID not valid' ; I allready cropped this variable      
    endif else begin
      pz =  where(offset lt 0, cnt)
      if cnt ne 1 then Message, 'I could not determine which Z dimension to crop.'
    endelse
    
    nz = dims[pz[0]]
    
    if N_ELEMENTS(zlevels) eq 1 then _zlevels = [zlevels,zlevels] $
     else if N_ELEMENTS(zlevels) eq 2 then _zlevels = zlevels $
      else Message, WAVE_Std_Message('zlevels', /NELEMENTS)
    
    if min(_zlevels) lt 0 or max(_zlevels) ge nz then Message, WAVE_Std_Message('zlevels', /RANGE)
    
    offset[pz[0]] = min(_zlevels)
    count[pz[0]] = max(_zlevels) - min(_zlevels) + 1 

  endif
      
  ; Now fill every dimension that have not been cropped with std values
  pnok = where(offset lt 0, cnt)
  if cnt ne 0 then for i=0, cnt-1 do offset[pnok[i]] = 0  
  pnok = where(count lt 0, cnt)
  if cnt ne 0 then for i=0, cnt-1 do count[pnok[i]] = dims[pnok[i]]
    
  value = self->w_NCDF::get_Var(vid, COUNT=count, OFFSET=offset, STRIDE=stride, $
                                     units = units, $
                                     description = description, $
                                     varname = varname , $
                                     dims = dims, $
                                     dimnames = dimnames, $
                                     caching = caching)
  
  return, value
  
end


;+
; :Description:
;    This function reads a variable from the netcdf file but only
;    at a specific location.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Params:
;    Varid: in, required, type = string/integer
;           the variable ID (string or integer) to retrieve
;    i: in, required, type = long
;       the X index (within the subset) where to get the variable
;    j: in, required, type = long
;       the Y index (within the subset) where to get the variable
;    time:  out, type = qms
;           the variable times
;    nt: out, type = long
;        the variable number of times
;
; :Keywords:
;   K: in, optional, type = long
;      if a third dimension is available AND is not TIME, index of the level to retrieve.
;   T0: in, optional, type = qms/{ABS_DATE}
;       if set, it defines the first time of the variable timeserie
;   T1: in, optional, type = qms/{ABS_DATE}
;       if set, it defines the last time of the variable timeserie
;   description: out, type = string
;               If available, the description of the variable
;   units: out, type = string
;          If available, the units of the variable
;   varname: out, type = string
;            the name of the variable
;   dims: out, type = long
;         the variable dimensions
;   dimnames: out, type = string
;             the dimensions names
; 
; :Returns:
;         The variable
; 
; :History:
;      Written by FaM, 2010.
;-
function w_GEO_nc::get_TimeSerie, Varid, $ ; The netCDF variable ID, returned from a previous call to w_GEO_nc_VARDEF or w_GEO_nc_VARID, or the name of the variable. 
                          i, j, $
                          time,  $
                          nt,  $
                          t0 = t0, $
                          t1 = t1, $
                          k = k , $
                          units = units, $
                          description = description, $
                          varname = varname , $ ; 
                          dims = dims, $ ;
                          dimnames = dimnames ;
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  undefine, count, offset
  value = -1

  if ~self->w_NCDF::get_Var_Info (Varid, out_id = vid, $
    dims = dims, $
    dimnames = dimnames) then Message, '$Varid is not a correct variable ID'
  
  if ~arg_okay(i, /NUMERIC, N_ELEM=1) then  Message, WAVE_Std_Message('i', /ARG)
  if ~arg_okay(j, /NUMERIC, N_ELEM=1) then  Message, WAVE_Std_Message('j', /ARG)
  
  i =  FLOOR(i)
  j =  FLOOR(j)
  
  ndims = N_ELEMENTS(dims)
  count = LONARR(ndims) - 1
  offset = LONARR(ndims) - 1

  pi = where(dimnames eq (*self.dimNames)[self.XID], cnt)
  if cnt ne 0 then begin
    off = self.subset[0] + i
    if off ge dims[pi[0]] or off lt 0 then Message, WAVE_Std_Message('i', /RANGE)
    offset[pi[0]] = off
    count[pi[0]] = 1
  endif
  pj = where(dimnames eq (*self.dimNames)[self.YID], cnt)
  if cnt ne 0 then begin
    off = self.subset[2] + j
    if off ge dims[pj[0]] or off lt 0 then Message, WAVE_Std_Message('j', /RANGE)
    offset[pj[0]] = off
    count[pj[0]] = 1
  endif
  
  time = *self.time
  pt = -1
  if self.TID ge 0 and time[0] gt -1 then begin ; We found the time dimension in the file
  
    pt = where(dimnames eq (*self.dimNames)[self.TID], cnt)    
    if cnt ne 0 then begin ; the variable has a time dimension
      p0 = 0
      p1 = self.nt-1
      if check_WTIME(t0, OUT_QMS= it0) and p0 ne p1 then begin
        v = 0 > VALUE_LOCATE(time, it0) < (self.nt-1)
        p0 = v[0]
      endif
      if check_WTIME(t1, OUT_QMS= it1) and p0 ne p1 then begin
        v = 0 > VALUE_LOCATE(time, it1) < (self.nt-1)
        p1 = v[0]
      endif
      ; Ok, now set the offset and count accordingly
      time = time[p0:p1]
      offset[pt[0]] = p0
      count[pt[0]] = p1 - p0 + 1
                  
    endif else begin ; the variable has no time dimension
      time = self.t0
    endelse
    
  endif  
  nt = N_ELEMENTS(time)
  
  ; Now check for third dimension
  IF N_ELEMENTS(k) ne 0 then begin
    if N_ELEMENTS(k) gt 1 then Message, WAVE_Std_Message('k', /RANGE)
    case (ndims) of
      3: begin
        if pt[0] eq -1 then begin
          pk = where(offset lt 0, cnt)
          if k ge dims[pk[0]] or k lt 0 then Message, WAVE_Std_Message('k', /RANGE)
          offset[pk[0]] = k
          count[pk[0]] = 1
        endif ; else ignore it too
      end
      4: begin
        if pt[0] ne -1 then begin
          pk = where(offset lt 0, cnt)
          if k ge dims[pk[0]] or k lt 0 then Message, WAVE_Std_Message('k', /RANGE)
          offset[pk[0]] = k
          count[pk[0]] = 1
        endif ; else ignore it too        
      end
      else: ; Just ignore it
    endcase
    
  endif
  
      
  ; Now fill every dimension that have not been cropped with std values
  pnok = where(offset lt 0, cnt)
  if cnt ne 0 then for r=0, cnt-1 do offset[pnok[r]] = 0  
  pnok = where(count lt 0, cnt)
  if cnt ne 0 then for r=0, cnt-1 do count[pnok[r]] = dims[pnok[r]]
    
  value = self->w_NCDF::get_Var(vid, COUNT=count, OFFSET=offset, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames)
  
  return, value
  
end

;+
; :Description:
;    Describe the procedure.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Params:
;    varid: in, required, type = string/integer
;           the variable ID (string or integer) to retrieve
;           
;    i: in, required, type = long
;       the X index where to get the variable 
;       
;    j: in, required, type = long
;       the Y index where to get the variable 
;
; :Keywords:
;    t0: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the first time of the variable timeserie
;    t1: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the last time of the variable timeserie
;    K: in, optional
;       if 3D variable, the index in Z dimension where to get the TS
;
; :History:
;     Written by FaM, 2012.
;-      
pro w_GEO_nc::plot_TimeSerie, Varid, $ ; The netCDF variable ID, returned from a previous call to w_GEO_nc_VARDEF or w_GEO_nc_VARID, or the name of the variable. 
                              i, j, $
                              t0=t0, $
                              t1=t1, $
                              k=k

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if N_PARAMS() lt 3 then Message, WAVE_Std_Message(/NARG)
  
  if ~self->W_NCDF::get_Var_Info(Varid) then MESSAGE, 'Variable not found'  
  
  var = self->w_GEO_nc::get_TimeSerie( Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                         i, $
                         j, $
                         time, $
                         nt, $
                         t0 = t0, $
                         t1 = t1, $
                         units = units, $
                         description = description, $
                         varname = varname , $ ; 
                         dims = dims, $ ;
                         dimnames = dimnames )
                             
  w_TimeLinePlot, var, time, varname, COLOR1='red', TITLE='Geo NC TS plot: ' + description, YTITLE=units, THICKNESS=2
  
  cgtext, 0.7915, 0.26, 'Grid point: [' + str_equiv(STRING(i, FORMAT = '(I3)')) + ',' + str_equiv(STRING(j, FORMAT = '(I3)')) + ']', $
          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW

  self->w_GEO_nc::get_ncdf_coordinates, lon, lat
  
  cgtext, 0.7915 + 0.01, 0.2, 'Lon: ' + str_equiv(STRING(lon[i,j], FORMAT='(F7.2)')), $
          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW  
  cgtext, 0.7915 + 0.01, 0.15, 'Lat: ' + str_equiv(STRING(lat[i,j], FORMAT='(F7.2)')), $
          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW  
    
end

;+
; :Description:
;   Checks if geolocalisation info is found in the NCDF file.
;               
;-
function w_GEO_nc::has_ncdf_coordinates
  
  return, utils_nc_LonLat(self.cdfid)
  
end

;+
; :Description:
;   Retrieve geolocalisation info from the NCDF file.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Params:
;    lon: out
;         the longitudes  
;    lat: out
;         the latitudes
;    nx: out, type = long
;        number of elements in x dimension
;    ny: out, type = LL64
;        number of elements in y dimension
;
; :Keywords:
;    
;    NO_REFORM: in, optional
;               defaut behaviour is to return a 2D array of lats and lons. If they are one dimensional
;               in the original file, they will be transformed to the 2d equivalent arrays (see 'utils_1d_to_2d').
;               Set this keyword to prevent this action.
;               
; :History:
;      Written by FaM, 2010.
;-
pro w_GEO_nc::get_ncdf_coordinates, lon, lat, nx, ny, NO_REFORM = no_reform
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF  
  
  ok = utils_nc_LonLat(self.cdfid, lon_id, lat_id)
  if ~ ok then  Message, 'X and Y variables could not be found.'
  
  lon = self->w_GEO_nc::get_Var(lon_id, dimnames = londims)  
  lat = self->w_GEO_nc::get_Var(lat_id, dimnames = latdims)  
  
  nlondims = N_ELEMENTS(londims)
  nlatdims = N_ELEMENTS(latdims)
  
  ok = FALSE
  if nlondims eq 1 and nlatdims eq 1 then begin
    nx = N_ELEMENTS(lon)
    ny = N_ELEMENTS(lat)  
    if ~KEYWORD_SET(NO_REFORM) then utils_1d_to_2d, lon, lat, lon, lat
    ok = TRUE
  endif 
  
  if nlondims eq 2 and nlatdims eq 2 then begin
    nx = N_ELEMENTS(lon[*,0])
    ny = N_ELEMENTS(lon[0,*])  
    ok = TRUE
  endif
  
  if nlondims eq 3 and nlatdims eq 3 then begin
    if self.TID ge 0 then begin ; We found the time dimension in the file
      p = where(londims eq (*self.dimNames)[self.TID], cnt)
      ok = TRUE
      case (p[0]) of
        0: begin
          lon = lon[0,*,*]
        end
        1: begin
          lon = lon[*,0,*]
        end
        2: begin
          lon = lon[*,*,0]
        end
        else: begin
          ok = FALSE
        end
      endcase
      
      p = where(latdims eq (*self.dimNames)[self.TID], cnt)
      case (p[0]) of
        0: begin
          lat = lat[0,*,*]
        end
        1: begin
          lat = lat[*,0,*]
        end
        2: begin
          lat = lat[*,*,0]
        end
        else: begin
          ok = FALSE
        end
      endcase
      nx = N_ELEMENTS(lon[*,0])
      ny = N_ELEMENTS(lon[0,*])
    endif else ok = FALSE ; We did not found the time dimension in the file
    
  endif
  
  if not ok then Message, 'Still do not know how to get the info.'
  
  lon = REFORM(lon)
  lat = REFORM(lat)
   
end

;+
; :Description:
;    Plots a desired variable for quick visualisation purposes.
;    It adds geolocalisation info, too.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid : in, required, type = integer/ string
;            variable index or name of the desired variable
;
; :Keywords:
;   UPSIDEDOWN: in, optional
;                to rotate the variable before plotting it
;    WID: out
;         the widget id     
;    
;    _EXTRA: in, optional
;            All keywords accepted by the get_var() function
;    
;         
; :History:
;     Written by FaM, 2010.
;-
pro w_GEO_nc::QuickPlotVar, Varid, UPSIDEDOWN = UPSIDEDOWN, WID = wid, _EXTRA = extra
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if ~self->get_Var_Info(Varid) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.' 
  var = self->get_Var(Varid, time, _EXTRA = extra, varname = varname, dimnames = dimnames, units = units, description=description)


  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  if KEYWORD_SET(UPSIDEDOWN) then var = ROTATE(var, 7)
  
  self->w_GEO_nc::get_ncdf_coordinates, lon, lat, nx, ny  
  
  if self.TID ge 0 and time[0] gt -1 then begin ; We found the time dimension in the file  
    p = where(dimnames eq (*self.dimNames)[self.TID], cnt)    
    if cnt ne 0 then tsrt = TIME_to_STR(time)
  endif   
  
  case (N_ELEMENTS(DIMNAMES)) of
    2: begin
       w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='w_GEO_nc view: ' + self.fname, $
        dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, WID = wid 
    end
    3: begin
       w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='w_GEO_nc view: ' + self.fname, $
         dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, dim3tags = tsrt, WID = wid
    end
    4: begin
       w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='w_GEO_nc view: ' + self.fname, $
            dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, dim4tags = tsrt, WID = wid
    end
    else: MESSAGE, 'Variable is not of suitable dimension.'
  endcase      

end

;+
; :Description:
; 
;    This function defines a new automatic subset for the NCDF file.
;    
;    It is called during the object instancing but can be called also once the instance is active.
;    It subsets the original data to a region of interest and actualises geolocalisation accordingly.
;    Future calls to 'get_Var' will return the subseted data. 
;       
;    To reset to the original geoloc just call this method without arguments.
;    Output is 1 if the w_GEO_nc object is updated successfully, 0 if not.
;    
;    This method should also be called internaly from redefined methods from child objects.
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;       SUBSET:  in, optional, type = integer array 
;               Four elements array::              
;                first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.                       
; :Returns:
;     1 if the subset was defined succesfully.
;          
; :History:
;      Written by FaM, 2010.
;-
function w_GEO_nc::define_subset, SUBSET = subset

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    self.subset = [0L,0L,0L,0L]
    self.cropped = 'FALSE'
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont do the subset. Returning... ')
    RETURN, 0
  ENDIF
  
  self.cropped = ''
  self.subset = [0,0,0,0]
  
  if ~ KEYWORD_SET(SUBSET) then begin
    self.subset  = [0,0,0,0]
    self.cropped = 'FALSE'
  endif else begin
    if ~ arg_okay(SUBSET, /ARRAY, /NUMERIC, N_ELEM=4) then Message, WAVE_Std_Message('SUBSET', /ARG)
    do_init = SUBSET[1] ne 0
    if ~ do_init then do_init = SUBSET[1] ne 0
    if ~ do_init then do_init = SUBSET[2] ne 0
    if ~ do_init then do_init = SUBSET[3] ne 0
    if do_init then begin    
      if SUBSET[0] lt 0 or SUBSET[0] gt ((*self.dimSizes)[self.XID]) then MESSAGE, WAVE_Std_Message('SUBSET[0]', /RANGE)
      if SUBSET[1] lt 1 or SUBSET[1] gt ((*self.dimSizes)[self.XID] - SUBSET[0]) then MESSAGE, WAVE_Std_Message('SUBSET[1]', /RANGE)
      if SUBSET[2] lt 0 or SUBSET[2] gt ((*self.dimSizes)[self.YID]) then MESSAGE, WAVE_Std_Message('SUBSET[2]', /RANGE)
      if SUBSET[3] lt 1 or SUBSET[3] gt ((*self.dimSizes)[self.YID] - SUBSET[2]) then MESSAGE, WAVE_Std_Message('SUBSET[3]', /RANGE)
      self.subset  = SUBSET
      self.cropped = 'TRUE'
    endif else begin
      self.subset  = [0,0,0,0]
      self.cropped = 'FALSE'
    endelse
  endelse
  
  return, 1 ;OK
  
end
