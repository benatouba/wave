;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  ARCLake_mask__Define.pro                                   *
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
;       ARCLake_mask is the basis class for TRMM datasets in NetCDF format.
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
;       ARCLake_mask__Define
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
PRO ARCLake_mask__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { ARCLake_mask             ,  $
            INHERITS Grid2D           ,  $
            INHERITS NCDF             ,  $
            subset:  [0l,0l,0l,0l]    ,  $ ; if not equal to 0, it holds the indexes in the ORIGINAL ncdf grid array where to subset (format: [x_dl,y_dl,x_ur,y_ur])
            cropped:            ''       $ ; set to true or false at initialisation.
            }
    
END

;-----------------------------------------------------------------------
;+
; NAME:
;       ARCLake_mask::Init
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
;       SUBSET_IJ : indexes in the ORIGINAL ncdf grid array in the form [x_dl,y_dl,x_ur,y_ur]. 
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the #define_subset# method.
;       LL_DATUM  : datum in which the Lat and Lons are defined. Default: WGS-84
;
; OUTPUT:
;       1 if the ARCLake_mask object is created successfully, 0 if not
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
Function ARCLake_mask::Init, FILE = file, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum
           
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
  IF NOT self->NCDF::Init(file = file) THEN RETURN, 0    
  
  ;*********
  ; Geoloc *
  ;*********
  self.cropped = ''
  if NOT self->define_subset(SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum) THEN RETURN, 0
      
  ; Ok
  RETURN, 1
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       ARCLake_mask::Cleanup
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
pro ARCLake_mask::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  NCDF_CLOSE, self.cdfid
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       ARCLake_mask::get_Mask
;
; PURPOSE:
;             
;       Extracts the mask variable from the NCDF file. The data obtained from this method
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
;       mask
;       
; PARAMS:
; 
; KEYWORDS:
; 
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
function ARCLake_mask::get_Mask

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN, 0
  ENDIF 
  
 NCDF_VARGET, self.cdfid, 'lakeid', mask
 mask = ROTATE(mask,7)
    
 if TOTAL(self.subset) ne 0 then mask = mask[self.subset[0]:self.subset[2],self.subset[1]:self.subset[3]] 
  
  return, mask
    
end

;-----------------------------------------------------------------------
;+
; NAME:
;       ARCLake_mask::QuickPlotPrcp
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
pro ARCLake_mask::QuickPlotMask

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN
  ENDIF 

  Mask = self->get_Mask()  
  self->Get_LonLat, lon, lat
  
  QuickPLot, Mask, COLORTABLE=2, TITLE= self.meta, $
    WINDOW_TITLE = 'NCDF view: ' + self.fname, COORDX=lon, COORDY=lat, CBARTITLE='Lake index', DIMNAMES=['lon','lat']
     
end

;-----------------------------------------------------------------------
;+
; NAME:
;       ARCLake_mask::define_subset
;
; PURPOSE:
;       It is called during the object instancing but can be called also once the object is created.
;       It subsets the original data to a region of interest and actualises Geolocalisation accordingly.
;       Future calls to #ARCLake_mask::get_var# will return the subseted data. 
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
function ARCLake_mask::define_subset, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum

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
  lok = geo_nc_LonLat(self.cdfid, lon, lat)
  if lok eq FALSE then message, 'Could not read lats or lons from this file...'
  lat = ROTATE(lat,2)
    
  nx = N_ELEMENTS(lon)
  ny = N_ELEMENTS(lat)
  
  ;Projection
  GIS_make_proj, ret, proj, PARAM='1, WGS-84'
  
  meta = 'ARCLake 0.05deg mask.'

  
  if FIRSTCALL then begin
    IF NOT self->grid2D::Init(   nx = nx                , $
                                 ny = ny                , $
                                 dx = 0.05D             , $
                                 dy = 0.05D             , $
                                 x0 = lon[0]            , $
                                 y0 = lat[ny-1]         , $
                                 proj = proj            , $
                                 meta = meta  ) THEN RETURN, 0
  endif else begin
    IF NOT self->grid2D::ReInit(  nx = nx                , $
                                  ny = ny                , $
                                  dx = 0.05D             , $
                                  dy = 0.05D             , $
                                  x0 = lon[0]            , $
                                  y0 = lat[ny-1]         , $
                                  proj = proj            , $
                                  meta = meta  ) THEN RETURN, 0
  endelse
  
  ;*********
  ; SUBSET *
  ;*********  
  self.cropped = 'FALSE'
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
    self.subset = [idl,jdl,iur,jur]
    
  endif else if N_ELEMENTS(SUBSET_ij) eq 4 then begin
    if (SUBSET_ij[0] lt 0) or (SUBSET_ij[1] lt 0) or (SUBSET_ij[2] gt self.tnt_c.nx-1) or (SUBSET_ij[3] gt self.tnt_c.ny-1) $
      then MESSAGE, 'Subset_IJ corners are not compatible.'  ; Fatal error
    self.subset = SUBSET_ij
  endif else self.subset = [0,0,0,0] ; NO subset
  
  if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = self.subset
  
  if TOTAL(self.subset) ne 0 then begin
  
    lon = lon[self.subset[0]:self.subset[2]]
    lat = lat[self.subset[1]:self.subset[3]]
    nx = N_ELEMENTS(lon)
    ny = N_ELEMENTS(lat)
    
    IF NOT self->grid2D::reInit( nx = nx                , $
      ny = ny                , $
      dx = 0.05D             , $
      dy = 0.05D             , $
      x0 = lon[0]            , $
      y0 = lat[ny-1]         , $
      proj = proj            , $
      meta = meta ) THEN RETURN, 0
    
    self.cropped = 'TRUE'
    
  endif
  
  return, 1 ;OK
  
end
