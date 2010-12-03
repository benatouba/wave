;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  GEO_nc__Define.pro                                    *
; Version     :  WAVE 0.1                                              *
; Language    :  IDL 7.0 and higher                                    *
; Date        :  2010                                                  *
; Last Update :  24-Nov-2010 FaM                                       *
;                                                                      *
; IDL class file for the WAVE library.                                 *
;                                                                      *
;***********************************************************************

;-----------------------------------------------------------------------
;+
; NAME:
;       GENERAL INFORMATION
;
;       GEO_nc is the basis class for all kinds of NCDF files that 
;       contin geolocalisation data. It will try to detect the geolocalistion
;       info from both dimensions and variables, as well as detetecting
;       time and Z dimensions. 
;       This is done based on the COARDS conventions and extended based on
;       the datasets already implemented in WAVE.
;        
;       The major feature of this class is to be able to call the 
;       #NCDF::get_Var()# method with subsets that have to be previously
;       defined. For example, WRF and TRMM classes inherits from this
;       superclass.
;              
;       =================================================================
;       Superclass:
;       ----------------------
;       NCDF
;       
;       =================================================================
;       Attributes:
;       ----------------------
;          XID:  ID of the X dimension 
;          YID:  ID of the Y dimension 
;          TID:  ID of the time dimension  (-1 if not found)
;          time: ptr_new() the time in qms (null if not found)   
;          t0  : first time in qms         (-1 if not found)   
;          t1  : last time in qms          (-1 if not found)  
;          nt  : number of time            (-1 if not found)
;          isCropped : '' 
;          Xsubset : [0L,0L] 
;                          first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                          second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;          Ysubset : [0L,0L] 
;                          first  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                          second el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.                   
;    
;       =================================================================
;       Object initialisation:
;       ----------------------
;       KEYWORDS:
;         FILE: the path to the NCDF file. If not set, a dialog window will open
;              
;       
;       =================================================================
;       Methods:
;       ----------------------
;       The following methods can be used directly. Non ducumented methods 
;       are not for external use.
;       
;       
;       =================================================================
;       
;-
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc__Define
;
; PURPOSE:
;       Object structure definition
;
; CATEGORY:
;       WAVE grid objects
;       
; MODIFICATION HISTORY:
;       Written by: Fabien Maussion 2010
;       Modified:   04-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO GEO_nc__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {GEO_nc                      , $
            INHERITS NCDF               , $
            XID : 0L                    , $
            YID : 0L                    , $
            TID : 0L                    , $
            time: ptr_new()             , $ 
            t0  : 0LL                   , $
            t1  : 0LL                   , $
            nt  : 0L                    , $
            cropped :   ''              , $
            subset : [0L,0L,0L,0L]        $ 
            }
    
END

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc::Init
;
; PURPOSE:
;       Build function. 
;
; CATEGORY:
;       WAVE grid objects
;
; KEYWORDS:
;       FILE: the path to the NCDF file. If not set, a dialog window will open
;
; OUTPUT:
;       1 if the GEO_nc object is updated successfully, 0 if not
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
Function GEO_nc::Init, FILE = file
           
           
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
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select NCDF file to read', /MUST_EXIST)  
  IF NOT self->NCDF::Init(file = file) THEN RETURN, 0    
  
  ;*****************************
  ; Check  dimensions validity *
  ;*****************************
  xp = ['west_east','lon','longitude','lon','longitudes','lons','xlong','xlong_m', 'dimlon','x']
  yp = ['south_north','lat','latitude' ,'lat','latitudes' ,'lats','xlat' ,'xlat_m', 'dimlat','y']
  tp = ['time','times','xtime']
  
  foundX = -1
  foundY = -1
  foundT = -1
  
  for i=0, self.Ndims-1 do begin
    dimn = self.dimNames[i]
    p = where(str_equiv(xp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundX = i
    p = where(str_equiv(yp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundY = i
    p = where(str_equiv(tp) eq str_equiv(dimn), cnt)
    if cnt ne 0 then foundT = i
    if foundX ge 0 and foundY ge 0 and foundT ge 0 then break
  endfor
  
  if foundX lt 0 or foundY lt 0 then Message, 'X and Y dimensions could not be found.'
  self.XID = foundX
  self.YID = foundY
  self.TID = foundT
  
  ;******************************
  ; Check  coordinate variables *
  ;******************************
  if ~geo_nc_LonLat(slef.cdfid) then  Message, 'X and Y variables could not be found.'
  
  self.t0 = -1LL
  self.t1 = -1LL  
  if foundT gt 0 then begin
    if ~ geo_nc_COARDS_time(self.cdfid, time, time0, time1, nt) then Message, 'Time dimension found but time could not be parsed.', /INFORMATIONAL
    self.t0 = time0
    self.t1 = time1
    self.time = PTR_NEW(time, /NO_COPY)    
  endif
  
  ;***********************
  ; Check  crop variable *
  ;***********************
  if ~self->define_subset(SUBSET = subset) then RETURN, 0 
  
  RETURN, 1
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc::Cleanup
;
; PURPOSE:
;       Destroy function. 
;
; CATEGORY:
;       WAVE grid objects
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;-
;-----------------------------------------------------------------------
pro GEO_nc::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  NCDF_CLOSE, self.cdfid
  PTR_FREE, self.varNames
  PTR_FREE, self.dimNames
  PTR_FREE, self.dimSizes
  PTR_FREE, self.time
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc::GetProperty
;
; PURPOSE:
;       Get access to some params. 
;
; CATEGORY:
;       WAVE grid objects
; 
; KEYWORDS:
;       Output:
;       path : complete path of the active GEO_nc file
;       HDFid : id of the HDF file as given by  GEO_nc 
;       fname : name of the active GEO_nc file
;       directory : directory of the active GEO_nc file
;       Ndims : the number of dimensions
;       Nvars : The number of variables defined for this GEO_nc file. 
;       Ngatts : The number of global attributes defined for this GEO_nc file. 
;       varNames:  An array of (nVars) strings containing the variable names. 
;       dimNames:  An array of (nDims) strings containing the dimension names. 
;       dimSizes:  An array of (nDims) longs containing the dimension sizes. 
;       RecDim : The ID of the unlimited dimension, if there is one, for this NetCDF file.
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO GEO_nc::GetProperty, $
    path = path, $
    cdfid = cdfid, $
    fname = fname, $
    directory = directory, $
    Ndims = Ndims, $
    Nvars = Nvars, $
    Ngatts = Ngatts, $
    varNames = varNames, $
    dimNames = dimNames, $
    dimSizes = dimSizes, $
    RecDim = RecDim
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  IF Arg_Present(path) NE 0 THEN path = self.path
  IF Arg_Present(fname) NE 0 THEN fname = self.fname
  IF Arg_Present(directory) NE 0 THEN directory = self.directory
  IF Arg_Present(cdfid) NE 0 THEN cdfid = self.cdfid
  IF Arg_Present(Ndims) NE 0 THEN Ndims = self.Ndims
  IF Arg_Present(Nvars) NE 0 THEN Nvars = self.Nvars
  IF Arg_Present(Ngatts) NE 0 THEN Ngatts = self.Ngatts
  IF Arg_Present(varNames) NE 0 THEN varNames = self.varNames
  IF Arg_Present(dimNames) NE 0 THEN dimNames = self.dimNames
  IF Arg_Present(dimSizes) NE 0 THEN dimSizes = self.dimSizes
  IF Arg_Present(RecDim) NE 0 THEN RecDim = self.RecDim
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc::get_Var
;
; PURPOSE:
;       extracts the desired variable from the GEO_nc file
;
; CATEGORY:
;       WAVE grid objects
;       
; INPUT:
;       varid : the netCDF variable ID, returned from a previous call to GEO_nc_VARDEF or GEO_nc_VARID, or the name of the variable. 
;       
; OUTPUT:
;       the variable
;       
; KEYWORDS:
;        COUNT: An optional vector containing the counts to be used in reading Value. COUNT is a 1-based vector with an element for each dimension of the data to be written.The default matches the size of the variable so that all data is written out. 
;        OFFSET: An optional vector containing the starting position for the read. The default start position is [0, 0, ...]. 
;        STRIDE: An optional vector containing the strides, or sampling intervals, between accessed values of the netCDF variable. The default stride vector is that for a contiguous read, [1, 1, ...]. 
;        varinfo: (O) structure that contains information about the variable. This  has the form: { NAME:"", DATATYPE:"", NDIMS:0L, NATTS:0L, DIM:LONARR(NDIMS) } 
;        description: (O) If available, the description of the variable
;        units: (O) If available, the units of the variable
;        varname: (O) the name of the variable
;        dims : (O) the variable dimensions
;        dimnames : (O) the dimensions names
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
function GEO_nc::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to GEO_nc_VARDEF or GEO_nc_VARID, or the name of the variable. 
                        COUNT=COUNT, $ ; An optional vector containing the counts to be used in reading Value. COUNT is a 1-based vector with an element for each dimension of the data to be written.The default matches the size of the variable so that all data is written out. 
                        OFFSET=OFFSET, $ ; An optional vector containing the starting position for the read. The default start position is [0, 0, ...]. 
                        STRIDE=STRIDE, $ ; An optional vector containing the strides, or sampling intervals, between accessed values of the netCDF variable. The default stride vector is that for a contiguous read, [1, 1, ...]. 
                        varinfo = varinfo , $ ; 
                        units = units, $
                        description = description, $
                        varname = varname , $ ; 
                        dims = dims, $ ;
                        dimnames = dimnames ;
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if arg_okay(VarId, TYPE=IDL_STRING, /SCALAR) then begin
    p = WHERE(str_equiv(*self.varNames) eq str_equiv(varid), cnt)
    if cnt ne 0 then vid = p[0] else MESSAGE, 'VarId is not a correct variable name.'
  endif else if arg_okay(VarId, /INTEGER, /SCALAR) then begin
    vid = varid  
  endif else MESSAGE, WAVE_Std_Message('VarId', /ARG)
  
  ncdf_VARGET, self.Cdfid, vid, Value, COUNT=count, OFFSET=offset, STRIDE=stride
  
  varinfo = ncdf_VARINQ(self.Cdfid, vid)
  
  if ARG_PRESENT(varname) then varname = varinfo.Name
  
  if ARG_PRESENT(description) then begin 
    description = ''
    for i = 0, varinfo.natts -1 do begin
      AttName = ncdf_ATTNAME(self.Cdfid, vid, i)
      if str_equiv(AttName) eq str_equiv('description') $
        or str_equiv(AttName) eq str_equiv('long_name') $
          then ncdf_ATTGET, self.Cdfid, vid, AttName, description
    endfor
    description = STRING(description)
  endif
  
  if ARG_PRESENT(units) then begin 
    units = ''
    for i = 0, varinfo.natts -1 do begin
      AttName = ncdf_ATTNAME(self.Cdfid, vid, i)
      if str_equiv(AttName) eq str_equiv('units') $
        or str_equiv(AttName) eq str_equiv('unit') $
          then ncdf_ATTGET, self.Cdfid, vid, AttName, units
    endfor
    units = STRING(units)
  endif  
  
  if ARG_PRESENT(dims) then begin
    dims = LONARR(varinfo.Ndims)
    for i = 0, varinfo.Ndims - 1 do begin
      ncdf_DIMINQ, self.Cdfid, varinfo.Dim[i], dimName, dimSize
      dims[i] = dimSize
    endfor
  endif
  
  if ARG_PRESENT(dimnames) then begin
    dimnames = STRARR(varinfo.Ndims)
    for i = 0, varinfo.Ndims - 1 do begin
      ncdf_DIMINQ, self.Cdfid, varinfo.Dim[i], dimName, dimSize
      dimnames[i] = dimName
    endfor
  endif
  
  return, value
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc::quickPlotVar
;
; PURPOSE:
;       flat plot of a desired variable for quick visualisation purposes
;
; CATEGORY:
;       WAVE grid objects
; 
; INPUT:
;       varid : variable index or name of the desired variable
;       
; OUTPUT:
;       a plot
;       
; KEYWORDS:
;        /UPSIDEDOWN: to rotate the variable before plotting it
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
pro GEO_nc::quickPlotVar, Varid, UPSIDEDOWN = UPSIDEDOWN

  var = self->get_Var(Varid, varname = varname, dimnames = dimnames, units = units, DESCRIPTION=DESCRIPTION)
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  if KEYWORD_SET(UPSIDEDOWN) then var = ROTATE(var, 7)
  
  QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='GEO_nc view: ' + self.fname, dimnames = dimnames, CBARTITLE=units

end

;-----------------------------------------------------------------------
;+
; NAME:
;       GEO_nc::define_subset
;
; PURPOSE:
;       It is called during the object instancing but can be called also once the object is created.
;       It subsets the original data to a region of interest and actualises Geolocalisation accordingly.
;       Future calls to #TRMM_nc::get_var# will return the subseted data. 
;       
;       To reset to the original geoloc just call this method without arguments.
;
; CATEGORY:
;       WAVE grid objects
;
; KEYWORDS:
;       SUBSET_LL : (I)   set it to the desired subset corners to automatically subset the data.
;                         Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                         lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : (I/O) indexes in the ORIGINAL ncdf grid array in the form [x_dl,y_dl,x_ur,y_ur]. 
;                         Unless you know what you do, it should not be set manually.
;                         One can retrive it from this method by setting it to a named variable                         
;       LL_DATUM  : (I)   datum in which the Lat and Lons are defined. Default: WGS-84
;
; OUTPUT:
;       1 if the MODIS_Grid object is updated successfully, 0 if not
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
function GEO_nc::define_subset, SUBSET = subset

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
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
    if SUBSET[0] lt 0 or SUBSET[0] gt (self.dimSizes[self.XID] - 1) then MESSAGE, WAVE_Std_Message('SUBSET[0]', /RANGE)
    if SUBSET[1] lt 0 or SUBSET[1] gt (self.dimSizes[self.XID] - 1) then MESSAGE, WAVE_Std_Message('SUBSET[1]', /RANGE)
    if SUBSET[2] lt 0 or SUBSET[2] gt (self.dimSizes[self.YID] - 1) then MESSAGE, WAVE_Std_Message('SUBSET[2]', /RANGE)
    if SUBSET[3] lt 0 or SUBSET[3] gt (self.dimSizes[self.YID] - 1) then MESSAGE, WAVE_Std_Message('SUBSET[3]', /RANGE)
    self.subset  = SUBSET
    self.cropped = 'TRUE'           
  endelse

  return, 1 ;OK
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       utils_ncdf_COARDS_time
;
; PURPOSE:
;       This procedure reads a time from a ncdf file that uses the COARDS convention.
;       Returns TRUE if time is found, FALSE in all other cases
;       
; CATEGORY:
;       WAVE utils
;
; CALLING SEQUENCE:
;       result = utils_ncdf_COARDS_time(cdfid, time, time0, time1, nt)
;
; INPUT:
;       cdfid: the active file cdfid
;
; OUTPUT:
;       time : the qms time array
;       time0 : the first qms
;       time1 : the last qms
;       nt : number of elements
;
; MODIFICATION HISTORY:
;       Written by: FM, 2010
;-
;-----------------------------------------------------------------------
function geo_nc_COARDS_time, cdfid, time, time0, time1, nt

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

;-----------------------------------------------------------------------
;+
; NAME:
;       utils_ncdf_LonLat
;
; PURPOSE:
;       This procedure reads lons and lats from a ncdf file, trying several known variable names. 
;       Returns TRUE if time is found, FALSE in all other cases
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
function geo_nc_LonLat, cdfid, lon, lat

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
  
  vlonok = -1
  vlatok = -1
  for varid = 0, inq.NVARS - 1 do begin
    vname = (NCDF_VARINQ(Cdfid, varid)).name
    p = where(str_equiv(lon_list) eq str_equiv(vname), cnt)
    if cnt ne 0 then vlonok = varid
    p = where(str_equiv(lat_list) eq str_equiv(vname), cnt)
    if cnt ne 0 then vlatok = varid
    if vlatok ge 0 and vlonok ge 0 then break
  endfor
  
  if vlatok lt 0 or vlonok lt 0 then return, FALSE
  
  ; Read Lon
  NCDF_VARGET, Cdfid, vlonok, lon
  ; Read Lat
  NCDF_VARGET, Cdfid, vlatok, lat
  
  return, TRUE

end
