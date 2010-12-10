; docformat = 'rst'
;+
; 
; GEO_nc is the basis class for all kinds of NCDF files that 
; contain geolocalisation data. It will try to detect the geolocalistion
; info from both dimensions and variables, as well as detetecting
; time and Z dimensions. 
; 
; This is achieved by checking if the NetCDF file follow the COARDS conventions,
; and by verifiying some dimension names against a list of known suitable names.
; TODO: currently this list is hard coded, it should be done using configuration files
; 
; The major feature of this class is to be able to call the 
; 'NCDF::get_Var()' method using subseting. The subset have to be previously
;  defined with 'define_subset'. 
;  
; :Properties: 
;    XID: in, optional, type = integer
;         ncdf ID of the X dimension
;    YID: in, optional, type = integer
;         ncdf ID of the Y dimension
;    time: in, optional, type = ptr
;          the time in qms (-1 if not found)  
;    t0: in, optional, type = LL64
;          first time in qms         (-1 if not found)   
;    t1: in, optional, type = LL64
;          end time in qms         (-1 if not found)   
;    nt: in, optional, type = interger
;          number of elements in time
;    subset: in, optional, type = integer array 
;              Four elements array::              
;                first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.
;   cropped: if it is cropped or not (private)
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
;     Last modification:  09-Dec-2010 FaM
;-


;+
; :Description:
; 
;   Defines the attributes of the class Grid2D. Attributes::
;   
;     GEO_nc                      
;            INHERITS NCDF               
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
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin}
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-
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

;+
; :Description:
;       Build function. 
;
; :Categories:
;            WAVE/OBJ_GIS   
;
; :Keywords:
;    nx: in, optional, type = integer
;        number of elements in x-direction
;    ny: in, optional, type = integer
;        number of elements in y-direction
;    x0: in, optional, type = float
;        most western (most left) x-coordinate
;    y0: in, optional, type = float
;        most northern (most upper) y-coordinate
;    x1: in, optional, type = float
;        most eastern (most right) x-coordinate
;    y1: in, optional, type = float
;        most southern (most lower) y-coordinate
;    dx: in, optional, type = float
;        resolution in x-direction
;    dy: in, optional, type = float
;        resolution in y-direction
;    tnt_c: in, optional, type = {TNT_COORD}
;        TNT_GIS {TNT_COORD} struct. If set, all previous keywords are ignored.
;    proj: in, optional, type = {TNT_PROJ}
;        TNT_GIS {TNT_PROJ} struct. MUST be SET.
;    meta: in, optional, type = string
;         a string containing any kind of info
;       
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;      Written by FaM, 2010.
;       
;       Modified::
;          22-Nov-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;-
Function GEO_nc::Init, FILE = file, SUBSET = subset
           
           
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
    dimn = (*self.dimNames)[i]
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
  if ~utils_nc_LonLat(self.cdfid) then  Message, 'X and Y variables could not be found.'
  
  self.t0 = -1LL
  self.t1 = -1LL 
  self.time = PTR_NEW(-1LL, /NO_COPY) 
  if foundT ge 0 then begin
    if utils_nc_COARDS_time(self.cdfid, time, time0, time1, nt) then begin
      self.t0 = time0
      self.t1 = time1
      self.time = PTR_NEW(time, /NO_COPY)    
    endif else if utils_wrf_time(self.cdfid, time, time0, time1, nt) then begin
      self.t0 = time0
      self.t1 = time1
      self.time = PTR_NEW(time, /NO_COPY)   
    endif ; else Message, 'Time dimension found but time could not be parsed.', /INFORMATIONAL
  endif  
  
  self.nT = N_ELEMENTS(*self.time)
  
  ;***********************
  ; Check  crop variable *
  ;***********************
  if ~self->GEO_nc::define_subset(SUBSET = subset) then RETURN, 0 
  
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
            XID = XID        , $
            YID = YID    , $
            TID = TID             , $
            time = time       , $ 
            t0 = t0               , $
            t1 =  t1              , $
            nt =  nt           , $
            cropped = cropped          , $
            subset = subset       ,$
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
  IF Arg_Present(TID) NE 0 THEN TID = self.TID
  IF Arg_Present(time) NE 0 THEN time = *self.time
  IF Arg_Present(t0) NE 0 THEN t0 = self.t0
  IF Arg_Present(t1) NE 0 THEN t1 = self.t1
  IF Arg_Present(nt) NE 0 THEN nt = self.nt
  IF Arg_Present(cropped) NE 0 THEN cropped = self.cropped
  IF Arg_Present(subset) NE 0 THEN subset = self.subset
  
  self->NCDF::GetProperty, _Extra=extra
    
end

pro GEO_nc::get_time, time, nt, t0, t1

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
;    This function reads a variable from the netcdf file and make a
;    subset of it if desired.    
;
; :Params:
;    Varid: in, required
;           the variable ID (string or integer) to check
;
; :Keywords:
;  TODO:  doc
;   t0: in, type = qms / {ABS_DATE}
;       if set, it defines the first available time 
;   times: out, type = qms  
;          if available, the time of the variable 
;   varinfo: out
;            structure that contains information about the variable. This  has the form: { NAME:"", DATATYPE:"", NDIMS:0L, NATTS:0L, DIM:LONARR(NDIMS) }
;   description: out
;               If available, the description of the variable
;   units: out
;          If available, the units of the variable
;   varname: out
;            the name of the variable
;   dims: out
;         the variable dimensions
;   dimnames: out
;             the dimensions names
; 
; :Returns:
;         1 if the variable id is valid, 0 if not
; 
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin}
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          First apparition for upgrade to WAVE 0.1
;
;-
function GEO_nc::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to GEO_nc_VARDEF or GEO_nc_VARID, or the name of the variable. 
                          time,  $
                          nt,  $
                          t0 = t0, $
                          t1 = t1, $
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
  
  undefine, count, offset
  value = -1
  
  if ~self->NCDF::get_Var_Info (Varid, $
    out_id = vid, $
    varinfo = varinfo , $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames) then Message, '$Varid is not a correct variable ID'
    
  ndims = N_ELEMENTS(dims)
  count = LONARR(ndims) - 1
  offset = LONARR(ndims) - 1
  
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
    
  endif
  
  time = *self.time
  if self.TID ge 0 and time[0] gt -1 then begin ; We found the time dimension in the file
  
    p = where(dimnames eq (*self.dimNames)[self.TID], cnt)    
    if cnt ne 0 then begin ; the variable has a time dimension
      p0 = 0
      p1 = self.nt-1
      if check_WTIME(t0, OUT_QMS= it0) then begin
        v = 0 > VALUE_LOCATE(time, it0) < (self.nt-1)
        p0 = v[0]
      endif
      if check_WTIME(t1, OUT_QMS= it1) then begin
        v = 0 > VALUE_LOCATE(time, it1) < (self.nt-1)
        p1 = v[0]
      endif
      ; Ok, now set the offset and count accordingly
      time = time[p0:p1]
      offset[p[0]] = p0
      count[p[0]] = p1 - p0 + 1
                  
    endif else begin ; the variable has no time dimension
      time = self.t0
    endelse
    
  endif  
  nt = N_ELEMENTS(time)
      
  ; Now fill every dimension that have not been cropped with std values
  pnok = where(offset lt 0, cnt)
  if cnt ne 0 then for i=0, cnt-1 do offset[pnok[i]] = 0  
  pnok = where(count lt 0, cnt)
  if cnt ne 0 then for i=0, cnt-1 do count[pnok[i]] = dims[pnok[i]]
    
  value = self->NCDF::get_Var(vid, COUNT=count, OFFSET=offset)
  
  return, value
  
end


pro GEO_nc::get_ncdf_coordinates, lon, lat, nx, ny
  
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
  
  lon = self->GEO_nc::get_Var(lon_id, dimnames = londims)  
  lat = self->GEO_nc::get_Var(lat_id, dimnames = latdims)  
  
  nlondims = N_ELEMENTS(londims)
  nlatdims = N_ELEMENTS(latdims)
  
  ok = FALSE
  if nlondims eq 1 and nlatdims eq 1 then begin
    utils_1d_to_2d, lon, lat, lon, lat
    ok = TRUE
  endif
  
  if nlondims eq 2 and nlatdims eq 2 then begin
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
      
    endif else ok = FALSE ; We did not fiund the time dimension in the file  
 
  endif
  
  if not ok then Message, 'Still do not know how to get the info.'
  
  lon = REFORM(lon)
  lat = REFORM(lat)
  
  nx = N_ELEMENTS(lon[*,0])
  ny = N_ELEMENTS(lon[0,*])
   
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
pro GEO_nc::quickPlotVar, Varid, t0 = t0, t1 = t1, UPSIDEDOWN = UPSIDEDOWN

  var = self->get_Var(Varid, t0 = t0, t1 = t1, time = time, varname = varname, dimnames = dimnames, units = units, DESCRIPTION=DESCRIPTION)
  if N_ELEMENTS(var) eq 1 and var[0] eq -1 then return

  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  if KEYWORD_SET(UPSIDEDOWN) then var = ROTATE(var, 7)
  
  self->get_ncdf_coordinates, lon, lat, nx, ny  
  
  if self.TID ge 0 and time[0] gt -1 then begin ; We found the time dimension in the file  
    p = where(dimnames eq (*self.dimNames)[self.TID], cnt)    
    if cnt ne 0 then tsrt = TIME_to_STR(time)
  endif   
  
  QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='GEO_nc view: ' + self.fname, $
        dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, dim3tags = tsrt

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
    if SUBSET[0] lt 0 or SUBSET[0] gt ((*self.dimSizes)[self.XID] - 1) then MESSAGE, WAVE_Std_Message('SUBSET[0]', /RANGE)
    if SUBSET[1] lt 0 or SUBSET[1] gt ((*self.dimSizes)[self.XID] - 1 - SUBSET[0]) then MESSAGE, WAVE_Std_Message('SUBSET[1]', /RANGE)
    if SUBSET[2] lt 1 or SUBSET[2] gt ((*self.dimSizes)[self.YID] - 1) then MESSAGE, WAVE_Std_Message('SUBSET[2]', /RANGE)
    if SUBSET[3] lt 1 or SUBSET[3] gt ((*self.dimSizes)[self.YID] - 1 - SUBSET[2]) then MESSAGE, WAVE_Std_Message('SUBSET[3]', /RANGE)
    self.subset  = SUBSET
    self.cropped = 'TRUE'           
  endelse

  return, 1 ;OK
  
end
