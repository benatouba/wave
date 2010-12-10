;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  TRMM_nc__Define.pro                                   *
; Version     :  WAVE 0.1                                              *
; Language    :  IDL 7.0 and higher                                    *
; Date        :  2010                                                  *
; Last Update :  05-Nov-2010 FaM                                       *
;                                                                      *
; IDL class file for the WAVE library.                                 *
;                                                                      *
;***********************************************************************

;-----------------------------------------------------------------------
;+
; NAME:
;       GENERAL INFORMATION
;
;       TRMM_nc is the basis class for TRMM datasets in NetCDF format.
;       It reads the geolocalisation from the file and provides subseting 
;       tools to the user, as well as all the #GRID2d# transformation tools.
;              
;       Accepted products so far:
;       3B43 (Monthly)
;       3B42 (Hourly)
;       3B42_daily (Daily)
;       3B42_agg : 3B42 products aggregated with the WAVE #utils_TRMM_aggregate# procedure
;       3B43_agg : 3B43 products aggregated with the WAVE #utils_TRMM_aggregate# procedure
;      
;       =================================================================
;       Superclass:
;       ----------------------
;       NCDF
;       Grid2D
;       
;       =================================================================
;       Attributes:
;       ----------------------
;          +NCDF attributes  
;          +Grid2D attributes  
;            subset:  [0l,0l,0l,0l]    ,  $ ; if not equal to 0, it holds the indexes in the ORIGINAL ncdf grid array where to subset
;            type:               ''    ,  $ ; type of data granule: '3B42_d', '3B42_h', '3B43', '3B42_agg', '3B43_agg'
;            cropped:            ''    ,  $ ; set to true or false at initialisation.
;            t0:         {ABS_DATE}    ,  $ ; first available time
;            t1:         {ABS_DATE}    ,  $ ; last available time
;            time:        PTR_NEW()    ,  $ ; available times
;            nt:                 0L       $ ; n times
;          
;                    
;       =================================================================
;       Object initialisation:
;       ----------------------
;       KEYWORDS:
;         FILE: the path to the MODIS file. If not set, a dialog window will open
;         SUBSET_LL : Subset corners in Lat Lons
;         SUBSET_IJ : subset indexes in the NCDF file (to define using SUBSET_LL)
;         LL_DATUM  : datum in which the Lat and Lons are defined. Default: WGS-84
;              
;       
;       =================================================================
;       Methods:
;       ----------------------
;       The following methods can be used directly. Non ducumented methods 
;       are not for external use.
;       + NCDF methods
;       + GRID2D methods
;       Overrided methods:
;       obj->Get_Var()  : get a specific variable along with some information, coherent with the grid geoloc
;       New methods:
;       obj->define_subset() : to subset TRMM data to a rgion of interest accordingly to its intern geoloc.
;       obj->quickPlotPrcp    : plots a "flat" image of TRMM pcp 
;              
;       =================================================================
;       
;-
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc__Define
;
; PURPOSE:
;       Object structure definition
;
; CATEGORY:
;       WAVE grid objects
;       
; MODIFICATION HISTORY:
;       Written by: Fabien Maussion 2010
;       Modified:   05-Nov-2010 FaM
;                   Upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO TRMM_nc__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { TRMM_nc                  ,  $
            INHERITS Grid2D           ,  $
            INHERITS GEO_nc           ,  $
            type:               ''       $ ; type of data granule: '3B42_d', '3B42_h', '3B43', '3B42_a', '3B43_a'
            }
    
END

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc::Init
;
; PURPOSE:
;       Build function. I proposes subsetting tools, but subsetting is also
;       possible once the object is created using the #define_subset# method.
;
; CATEGORY:
;       WAVE grid objects
;
; KEYWORDS:
;       FILE      : the path to the TRMM file. If not set, a dialog window will open
;       SUBSET_LL : set it to the desired subset corners to automatically subset the data.
;                   Format : [ul_lon, ul_lat, dr_lon, dr_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : indexes in the ORIGINAL ncdf grid array in the form [x_dl,nx,y_dl,ny]. 
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the #define_subset# method.
;       LL_DATUM  : datum in which the Lat and Lons are defined. Default: WGS-84
;
; OUTPUT:
;       1 if the TRMM_nc object is created successfully, 0 if not
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
Function TRMM_nc::Init, FILE = file, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum
           
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
  IF NOT self->GEO_nc::Init(file = file) THEN RETURN, 0    
 
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
  if NOT self->TRMM_nc::define_subset(SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum) THEN RETURN, 0
  
  
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
  self.time = PTR_NEW(time, /NO_COPY)
  self.nt = nt
    
  ; Ok
  RETURN, 1
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc::Cleanup
;
; PURPOSE:
;       Destroy. 
;
; CATEGORY:
;       WAVE grid objects
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;-
;-----------------------------------------------------------------------
pro TRMM_nc::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  Ptr_Free, self.time
  NCDF_CLOSE, self.cdfid
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc::GetProperty
;
; PURPOSE:
;       Get access to some params. 
;
; CATEGORY:
;       WAVE grid objects
; 
; KEYWORDS:
;      Output:
;       type
;       t0 
;       t1     
;       time
;       nt
;       _Ref_Extra : see #NCDF:GetProperty# and #Grid2D::GetProperty#
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO TRMM_nc::GetProperty,  $
    type = type     ,  $ ; type of active file
    t0 = t0  ,  $ ; first available time
    t1 = t1   ,  $ ; last available time
    time = time   ,  $ ; available times
    nt = nt  ,  $ ; n times
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
  IF Arg_Present(t0) NE 0 THEN t0 = self.t0
  IF Arg_Present(t1) NE 0 THEN t1 = self.t1
  IF Arg_Present(time) NE 0 THEN time = *self.time
  IF Arg_Present(nt) NE 0 THEN nt = self.nt
  
  self->GRID2D::GetProperty, _Extra=extra
  self->NCDF::GetProperty, _Extra=extra
  
end

function TRMM_nc::get_Var, varid, time, nt, _Ref_Extra = extra                     
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if self.type eq '3B42_d' then Message, 'The function get_var does not work with daily files because their format is stupid. Use get_Prcp instead.', /INFORMATIONAL
    
  return, self->GEO_nc::get_Var(varid, time, nt, _Extra=extra)

end  

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc::get_prcp
;
; PURPOSE:
;             
;       Extracts the precipitation variable from the NCDF file. The data obtained from this method
;       is consistent with the grid geolocalisation and should not be modified externally
;       without using the suitable methods (see #Grid2D#).
;
; CATEGORY:
;       WAVE grid objects
;       
; INPUT:
;       none
;       
; OUTPUT:
;       precipitation
;       
; PARAMS:
;        times (O) times
;        nt    (O) n times
; 
; KEYWORDS:
;        units: (O) the units of prcp
;        t0:    (I) if set, the start time of the desired time serie
;        t1:    (I) if set, the end time of the desired time serie
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
function TRMM_nc::get_prcp, time, nt, units = units, t0 = t0, t1 = t1

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
    pcp = self->GEO_nc::get_Var('precipitation', time, nt, t0 = t0, t1 = t1) 
    units = 'mm hr-1'
  endif else if self.type eq '3B43' then begin
    pcp = self->GEO_nc::get_Var('pcp', time, nt, t0 = t0, t1 = t1) 
    units = 'mm hr-1'
  endif else begin ; agg files
    pcp = self->GEO_nc::get_Var('precipitation', time, nt, t0 = t0, t1 = t1) 
    NCDF_ATTGET, self.cdfid, 'precipitation', 'units', units
    units = STRING(units)
  endelse 

  return, pcp
    
end

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc::QuickPlotPrcp
;
; PURPOSE:
;       flat plot of prcp variable for quick visualisation purposes
;
; CATEGORY:
;       WAVE grid objects
; 
; INPUT:
;       
;       
; OUTPUT:
;       a plot
;       
; KEYWORDS:
;        /NO_CALIB: the default behaviour id to check if calibration data is contained 
;                   in the HDF variable attributes and apply it to the variable. Set this
;                   keyword to avoid making an automatic calibration
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
pro TRMM_nc::QuickPlotPrcp

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
  
  if N_ELEMENTS(times) eq 1 then QuickPLot, pcp, COLORTABLE=2, TITLE= self.meta, $
    WINDOW_TITLE = 'NCDF view: ' + self.fname, COORDX=lon, COORDY=lat, CBARTITLE='Prcp ' + units, DIMNAMES=['lon','lat'] $
      else QuickPLot, pcp, COLORTABLE=2, TITLE= self.meta, dim3tags=TIME_to_STR(times), $
          WINDOW_TITLE = 'NCDF view: ' + self.fname, COORDX=lon, COORDY=lat, CBARTITLE='Prcp ' + units, DIMNAMES=['lon','lat','time']

end

;-----------------------------------------------------------------------
;+
; NAME:
;       TRMM_nc::define_subset
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
;       SUBSET_IJ : (I/O) indexes in the ORIGINAL ncdf grid array in the form [x_dl,nx,y_dl,ny]. 
;                         Unless you know what you do, it should not be set manually.
;                         One can retrive it from this method by setting SUBSET_IJ to a named variable                         
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
function TRMM_nc::define_subset, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum

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
  ok = self->GEO_nc::define_subset()
  ok = utils_nc_LonLat(self.cdfid, lon_id, lat_id)  
  lon = self->GEO_nc::get_Var(lon_id)  
  lat = self->GEO_nc::get_Var(lat_id)
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
    IF NOT self->grid2D::Init(   nx = nx                , $
                                 ny = ny                , $
                                 dx = 0.25D             , $
                                 dy = 0.25D             , $
                                 x0 = lon[0]            , $
                                 y0 = lat[ny-1]         , $
                                 proj = proj            , $
                                 meta = meta  ) THEN RETURN, 0
  endif else begin
    IF NOT self->grid2D::ReInit(  nx = nx                , $
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
    ok = self->GEO_nc::define_subset(SUBSET=[idl,cx,jdl,cy])
  endif else if N_ELEMENTS(SUBSET_ij) eq 4 then begin
    ok = self->GEO_nc::define_subset(SUBSET=SUBSET_ij)
  endif else begin
   if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = [0L,0L,0L,0L]
   return, self->GEO_nc::define_subset()
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
    
    IF NOT self->grid2D::reInit( nx = nx                , $
      ny = ny                , $
      dx = 0.25D             , $
      dy = 0.25D             , $
      x0 = lon[0]            , $
      y0 = lat[ny-1]         , $
      proj = proj            , $
      meta = meta ) THEN begin
        dummy = self->GEO_nc::define_subset() ; NO subset
        RETURN, 0
      endif
    
  endif
  
  return, 1 ;OK
  
end
