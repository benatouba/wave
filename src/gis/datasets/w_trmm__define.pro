; docformat = 'rst'
;+
;
;  w_TRMM is the basis class for TRMM datasets in NetCDF format.
;  It reads the geolocalisation from the file and provides subseting 
;  tools to the user, as well as all the 'w_Grid2D' transformation tools.
;              
;  Accepted products so far::
;  
;       3B43 (Monthly)
;       3B42 (Hourly)
;       3B42_daily (Daily) (!Carefull with geoloc: use exclusively get_Prcp method!)
;       3B42_agg : 3B42 products aggregated with the WAVE 'utils_TRMM_aggregate_3B42' procedure
;       3B43_agg : 3B43 products aggregated with the WAVE 'utils_TRMM_aggregate_3B43' procedure
;      
; :Properties:
;         type: type = string              
;               type of data granule: '3B42_d', '3B42_h', '3B43', '3B42_agg', '3B43_agg'
;              
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          09-Dec-2010 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-

;+
; :Description:
;    Defines the attributes of the class w_Grid2D. Attributes::
;
;       w_TRMM
;            INHERITS w_Grid2D           
;            INHERITS w_GEO_nc         
;            type : ''
;
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :History:
;     Written by FaM, 2010.
;-
PRO w_TRMM__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { w_TRMM                  ,  $
            INHERITS w_Grid2D           ,  $
            INHERITS w_GEO_nc           ,  $
            type:               ''       $ ; type of data granule: '3B42_d', '3B42_h', '3B43', '3B42_a', '3B43_a'
            }
    
END

;+
; :Description:
; 
;    Build function.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;       FILE      : in, optional, type = string
;                   the path to the TRMM file. If not set, a dialog window will open
;       SUBSET_LL : in, optional, type = float vector 
;                   set it to the desired subset corners to automatically subset the data.
;                   Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : in, type = long vector
;                    Four elements array::              
;                      first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                      second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                      third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                      fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.
;                   
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the 'define_subset' method.
;                   
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84
;                   datum in which the Lat and Lons from 'SUBSET_LL' are defined
;
; :Returns:
;    1 if the object is created successfully. 
;
; :History:
;     Written by FaM, 2010.
;-
Function w_TRMM::Init, FILE = file, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum
           
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
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select TRMM ncdf file to read', /MUST_EXIST)  
  IF NOT self->w_GEO_nc::Init(file = file) THEN RETURN, 0    
 
  ;*****************
  ; Check filename *
  ;*****************  
  fname = self.fname
  ; type of data granule: '3B42_d', '3B42_h', '3B43', '3B42_a', '3B43_a'
  type = ''
  isHere = STRPOS(fname, '3B43')
  if isHere ge 0 then type = '3B43'
  isHere = STRPOS(fname, '3B42')
  if isHere ge 0 then type = '3B42_h'
  isHere = STRPOS(fname, '3B42_daily')
  if isHere ge 0 then type = '3B42_d'  
  isHere = STRPOS(fname, '3B42_agg')
  if isHere ge 0 then type = '3B42_a'  
  isHere = STRPOS(fname, '3B43_agg')
  if isHere ge 0 then type = '3B43_a'  
  if type eq '' then message, WAVE_Std_Message('Inut file not recognized as a known TRMM product.')
  self.type = type
  
  ;*********
  ; Geoloc *
  ;*********
  self.cropped = ''
  if NOT self->w_TRMM::define_subset(SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum) THEN RETURN, 0
      
  ;****************
  ; Read metadata *
  ;****************    
  tok = utils_nc_COARDS_time(self.cdfid, time, time0, time1, nt)
  
  ; Read time from fname : 3B42.081001.0.6A.nc
  if tok eq FALSE and type eq '3B42_h' then begin
    tsp = STRSPLIT(fname, '.',/EXTRACT)
    tsd = tsp[1]
    y = LONG(STRMID(tsd,0,2))
    if y lt 90 then y+=2000 else y +=1900
    m = LONG(STRMID(tsd,2,2))
    d = LONG(STRMID(tsd,4,2))
    h = LONG(tsp[2])
    time0 = QMS_TIME(YEAR=y, MONTH=m, DAY=d, HOUR=h)
    time1 = time0
    time = time0
    nt = 1
    tok = TRUE
  endif  
    
  self.t0 = time0
  self.t1 = time1
  ptr_free, self.time
  self.time = PTR_NEW(time, /NO_COPY)
  self.nt = nt
    
  ; Ok
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
pro w_TRMM::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
 
  self->w_Grid2D::Cleanup
  self->w_GEO_nc::Cleanup
      
END

;+
; :Description:
;    Get access to some params. 
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;    type: out, optional, type = string
;          type of data granule: '3B42_d', '3B42_h', '3B43', '3B42_agg', '3B43_agg'
;    _Ref_Extra: 
;        see 'w_GEO_nc:GetProperty' and 'w_Grid2D::GetProperty'
; :History:
;     Written by FaM, 2010.
;-
PRO w_TRMM::GetProperty,  $
                       type = type     ,  $ ; type of active file
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
      
  IF Arg_Present(type) NE 0 THEN type = self.type
  
  self->w_Grid2D::GetProperty, _Extra=extra
  self->w_GEO_nc::GetProperty, _Extra=extra
  
end

;+
; :Description:
;    see 'w_GEO_nc:get_Var'
;    
;    This function has been redefined because of the TRMM daily files that have
;    stupidly an other format.
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
;   _Ref_Extra: 
;        see 'w_GEO_nc:get_Var'
; 
; :Returns:
;         The variable
;         
; :History:
;      Written by FaM, 2010.
;-
function w_TRMM::get_Var, varid, time, nt, _Ref_Extra = extra                     
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if self.type eq '3B42_d' then Message, 'The function get_var does not work properly with 3B42 daily files because their format is stupid. Use get_Prcp instead.', /INFORMATIONAL
    
  return, self->w_GEO_nc::get_Var(varid, time, nt, _Extra=extra)

end


;+
; :Description:
; 
;    Extracts the precipitation variable from the TRMM file. The data obtained from this method
;    is consistent with the grid geolocalisation and should not be modified externally
;    without using the suitable methods (see 'w_Grid2D').
;    
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :Params:
;    time:  out, type = qms
;           the variable times
;    nt: out, type = long
;        the variable number of times
;        
; :Keywords:
;    units: out, optional, type = string
;           the units of prcp
;    T0: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the first time of the variable timeserie
;    T1: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the last time of the variable timeserie
; 
;  :Returns:
;     precipitation
;     
; :History:
;     Written by FaM, 2010.
;-
function w_TRMM::get_prcp, time, nt, units = units, t0 = t0, t1 = t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN, 0
  ENDIF 
  
 if self.type eq '3B42_d' then begin
    NCDF_VARGET, self.cdfid, 'hrf', pcp
    pcp = [pcp[720:*,*] , pcp[0:719,*]]
    units = 'mm d-1'
    time = *self.time
    nt = 1
    if self.cropped ne 'FALSE' then begin
      urx = self.subset[0] + self.subset[1] - 1
      ury = self.subset[2] + self.subset[3] - 1
      pcp = pcp[self.subset[0]:urx,self.subset[2]:ury]
    endif
  endif else if self.type eq '3B42_h' then begin
    pcp = self->w_GEO_nc::get_Var('precipitation', time, nt, t0 = t0, t1 = t1) 
    units = 'mm hr-1'
  endif else if self.type eq '3B43' then begin
    pcp = self->w_GEO_nc::get_Var('pcp', time, nt, t0 = t0, t1 = t1) 
    units = 'mm hr-1'
  endif else begin ; agg files
    pcp = self->w_GEO_nc::get_Var('precipitation', time, nt, t0 = t0, t1 = t1) 
    NCDF_ATTGET, self.cdfid, 'precipitation', 'units', units
    units = STRING(units)
  endelse 

  return, pcp
    
end


function w_TRMM::get_Prcp_TimeSerie, x, y, $
                              time, nt, $
                              t0 = t0, t1 = t1, $
                              src = src, $
                              units = units, $
                              point_i = point_i, $
                              point_j = point_j, $
                              point_lon = point_lon, $
                              point_lat = point_lat
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  pcp = -1
  
  if N_PARAMS() lt 2 then Message, WAVE_Std_Message(/NARG)
  
  ; no go threw the possibilites:
  if N_ELEMENTS(src) EQ 0 then mysrc = self else mysrc = src
  
  ; This is to obtain the indexes in the grid
  self->transform,  x, y, point_i, point_j, SRC = mysrc, /NEAREST
  
  ; This is to obtain lat and lons of the selected grid point
  self->transform, point_i, point_j, dummy1, dummy2, src=self, $
    LON_DST=point_lon, LAT_DST=point_lat
    
  if self.type eq '3B42_d' then begin
    NCDF_VARGET, self.cdfid, 'hrf', pcp
    pcp = [pcp[720:*,*] , pcp[0:719,*]]
    units = 'mm d-1'
    time = *self.time
    nt = 1
    if self.cropped ne 'FALSE' then pcp = pcp[self.subset[0]+point_i,self.subset[2]+point_j]
  endif else if self.type eq '3B42_h' then begin
    pcp = self->w_GEO_nc::get_TimeSerie('precipitation', point_i, point_j, time, nt, t0 = t0, t1 = t1)
    units = 'mm hr-1'
  endif else if self.type eq '3B43' then begin
    pcp = self->w_GEO_nc::get_TimeSerie('pcp', point_i, point_j, time, nt, t0 = t0, t1 = t1)
    units = 'mm hr-1'
  endif else begin ; agg files
    pcp = self->w_GEO_nc::get_TimeSerie('precipitation', point_i, point_j, time, nt, t0 = t0, t1 = t1)
    NCDF_ATTGET, self.cdfid, 'precipitation', 'units', units
    units = STRING(units)
  endelse
  
  return, pcp
  
end


;+
; :Description:
; 
;    Plots the prcp variable for quick visualisation purposes.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;  :Keywords:
;    WID: out
;         the widget id
;         
; :History:
;     Written by FaM, 2010.
;-
pro w_TRMM::QuickPlotPrcp, WID = wid

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN
  ENDIF 

  pcp = self->get_prcp(times, units = units)
  
  if self.type eq '3B42_d' then self->Get_LonLat, lon, lat else self->get_ncdf_coordinates, lon, lat
  
  if N_ELEMENTS(times) eq 1 then w_QuickPlot, pcp, COLORTABLE=2, TITLE= self.meta + ' ' + TIME_to_STR(times), $
    WINDOW_TITLE = 'NCDF view: ' + self.fname, COORDX=lon, COORDY=lat, CBARTITLE='Prcp ' + units, DIMNAMES=['lon','lat'], WID = wid $
      else w_QuickPlot, pcp, COLORTABLE=2, TITLE= self.meta, dim3tags=TIME_to_STR(times), $
          WINDOW_TITLE = 'NCDF view: ' + self.fname, COORDX=lon, COORDY=lat, CBARTITLE='Prcp ' + units, DIMNAMES=['lon','lat','time'], WID = wid

end

;+
; :Description:
; 
;    This function defines a new automatic subset for the TRMM file. It encapsulates
;    the 'w_GEO_nc::define_dubset' method by replacing the SUBSET keyword with 'SUBSET_IJ'
;    and adding the 'SUBSET_LL' keyword for Lon-Lat corners subset.
;    
;    It is called during the object instancing but can be called also once the instance is active.
;    It subsets the original data to a region of interest and actualises geolocalisation accordingly.
;    Future calls to 'get_Var' will return the subseted data. 
;       
;    To reset to the original geoloc just call this method without arguments.
;    Output is 1 if the w_TRMM object is updated successfully, 0 if not.
;    
;    
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;       SUBSET_LL : in, optional, type = float vector 
;                   set it to the desired subset corners to automatically subset the data.
;                   Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.);                   
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84
;                   datum in which the Lat and Lons from 'SUBSET_LL' are defined
;       SUBSET_IJ:  in, out, optional, type = integer array 
;                   Four elements array::              
;                     first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                     second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                     third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                     fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.                       
; :Returns:
;     1 if the subset was defined succesfully.
;     
; :History:
;      Written by FaM, 2010.
;-
function w_TRMM::define_subset, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF
  
  do_init = self.cropped eq '' ;first init
  firstcall = do_init
  if ~ do_init then begin
    if (N_ELEMENTS(SUBSET_LL) eq 4) and (self.cropped eq 'FALSE') then do_init = TRUE  $ ; Is not cropped yet
      else if (N_ELEMENTS(SUBSET_IJ) eq 4) and (self.cropped eq 'FALSE') then do_init = TRUE  $ ; Is not cropped yet
        else do_init = self.cropped ne 'FALSE'   ; Is cropped but we dont want to
  endif

  if ~ do_init and KEYWORD_SET(SUBSET_IJ) then begin ;Look if the subset changed
    do_init = self.subset[0] ne SUBSET_IJ[0]
    if ~ do_init then do_init = self.subset[1] ne SUBSET_IJ[1]
    if ~ do_init then do_init = self.subset[2] ne SUBSET_IJ[2]
    if ~ do_init then do_init = self.subset[3] ne SUBSET_IJ[3]
  endif
  
  if ~ do_init then return, 1 ;nothing to do
  
  ;*****************************************
  ; First, define the ORIGINAL grid geoloc *
  ;*****************************************
  ok = self->w_GEO_nc::define_subset()
  ok = utils_nc_LonLat(self.cdfid, lon_id, lat_id)  
  lon = self->w_GEO_nc::get_Var(lon_id)  
  lat = self->w_GEO_nc::get_Var(lat_id)
  nx = N_ELEMENTS(lon)
  ny = N_ELEMENTS(lat)
  
  if self.type eq '3B42_d' then lon = [lon[720:*] - 360., lon[0:719]] ; Fucking conventions
  
  ;Projection
  GIS_make_proj, ret, proj, PARAM='1, WGS-84'
  
  case (self.type) of
    '3B42_h' : meta = 'TRMM 3B42 3hrly file.'
    '3B42_a' : meta = 'TRMM 3B42 3hrly agg. file.'
    '3B42_d' : meta = 'TRMM 3B42 daily file.'
    '3B43'   : meta = 'TRMM 3B43 monthly file.'
    '3B43_a' : meta = 'TRMM 3B43 monthly agg file.'
  endcase
  
  if FIRSTCALL then begin
    IF NOT self->w_Grid2D::Init( nx = nx                , $
                                 ny = ny                , $
                                 dx = 0.25D             , $
                                 dy = 0.25D             , $
                                 x0 = lon[0]            , $
                                 y0 = lat[ny-1]         , $
                                 proj = proj            , $
                                 meta = meta  ) THEN RETURN, 0
  endif else begin
    IF NOT self->w_Grid2D::ReInit(  nx = nx                , $
                                  ny = ny                , $
                                  dx = 0.25D             , $
                                  dy = 0.25D             , $
                                  x0 = lon[0]            , $
                                  y0 = lat[ny-1]         , $
                                  proj = proj            , $
                                  meta = meta  ) THEN RETURN, 0
  endelse
  
  ;*********
  ; SUBSET *
  ;*********  
  self.cropped = 'FALSE'
  ok = FALSE
  
  if N_ELEMENTS(SUBSET_LL) eq 4 then begin
    if KEYWORD_SET(ll_datum) then begin
      if not arg_okay(ll_datum, STRUCT={TNT_DATUM}) then Message, WAVE_Std_Message('ll_datum', STRUCT={TNT_DATUM})
      dat = ll_datum
    endif else GIS_make_datum, ret, dat, NAME='WGS-84'
    
    self->transform_LonLat, SUBSET_LL[0], SUBSET_LL[1], dat, idl, jdl, /NEAREST
    self->transform_LonLat, SUBSET_LL[2], SUBSET_LL[3], dat, iur, jur, /NEAREST
    
    ;**********
    ; Errors? *
    ;**********
    if idl lt 0 then begin
      idl = 0
      MESSAGE, 'Down left corner out of X range. Setting to 0.', /INFORMATIONAL
    endif
    if idl gt (self.tnt_c.nx - 1) then begin
      idl = (self.tnt_c.nx - 1)
      MESSAGE, 'Down left corner out of X range. Setting to (nx - 1).', /INFORMATIONAL
    endif
    if jdl lt 0 then begin
      jdl = 0
      MESSAGE, 'Down left corner out of Y range. Setting to 0.', /INFORMATIONAL
    endif
    if jdl gt (self.tnt_c.ny - 1) then begin
      jdl = (self.tnt_c.ny - 1)
      MESSAGE, 'Down left corner out of Y range. Setting to (ny - 1).', /INFORMATIONAL
    endif
    if iur lt 0 then begin
      iur = 0
      MESSAGE, 'Upper right corner out of X range. Setting to 0.', /INFORMATIONAL
    endif
    if iur gt (self.tnt_c.nx - 1) then begin
      iur = (self.tnt_c.nx - 1)
      MESSAGE, 'Upper right corner out of X range. Setting to (nx - 1).', /INFORMATIONAL
    endif
    if jur lt 0 then begin
      jur = 0
      MESSAGE, 'Upper right corner out of Y range. Setting to 0.', /INFORMATIONAL
    endif
    if jur gt (self.tnt_c.ny - 1) then begin
      jur = (self.tnt_c.ny - 1)
      MESSAGE, 'Upper right corner out of Y range. Setting to (ny - 1).', /INFORMATIONAL
    endif
    
    cx = iur - idl + 1
    cy = jur - jdl + 1
    if (cx lt 1) or (cy lt 1) then MESSAGE, 'Subset_LL corners are not compatible.'  ; Fatal error
    ok = self->w_GEO_nc::define_subset(SUBSET=[idl,cx,jdl,cy])
  endif else if N_ELEMENTS(SUBSET_ij) eq 4 then begin
    ok = self->w_GEO_nc::define_subset(SUBSET=SUBSET_ij)
  endif else begin
   if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = [0L,0L,0L,0L]
   return, self->w_GEO_nc::define_subset()
  endelse
  
  if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = self.subset  
  if not ok then return, 0   
  
  if TOTAL(self.subset) ne 0 then begin
  
    urx = self.subset[0] + self.subset[1] - 1
    ury = self.subset[2] + self.subset[3] - 1
    lon = lon[self.subset[0]:urx]
    lat = lat[self.subset[2]:ury]
    nx = N_ELEMENTS(lon)
    ny = N_ELEMENTS(lat)
    
    IF NOT self->w_Grid2D::reInit( nx = nx                , $
      ny = ny                , $
      dx = 0.25D             , $
      dy = 0.25D             , $
      x0 = lon[0]            , $
      y0 = lat[ny-1]         , $
      proj = proj            , $
      meta = meta ) THEN begin
        dummy = self->w_GEO_nc::define_subset() ; NO subset
        RETURN, 0
      endif
    
  endif
  
  return, 1 ;OK
  
end
