;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  MODIS_Grid__Define.pro                                *
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
;       MODIS_Grid is the basis class for MODIS level 2 & 3 files. It reads
;       the geolocalisation from the HDF-EOS file and provides subseting 
;       tools to the user, as well as all the #GRID2d# transformation tools.
;       
;       Tested products so far:
;       MOD10A1
;       MOD11A1
;       MOD13A1       
;       But there should be no limitiations in using it for other L2 L3 products.
;              
;      
;       =================================================================
;       Superclass:
;       ----------------------
;       HDF_EOS
;       Grid2D
;       
;       =================================================================
;       Attributes:
;       ----------------------
;          +HDF_EOS attributes  
;          +Grid2D attributes  
;          t0:              {ABS_DATE}      first available time
;          t1  :            {ABS_DATE}      last available time
;          subset:          [0l,0l,0l,0l]   if not equal to 0, it holds the indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). It should not be set manually but using the #define_subset# method.
;          cropped:         ''              Set to "TRUE" or "FALSE" at the first initialisation and updated with #define_subset#
;          
;       =================================================================
;       Object initialisation:
;       ----------------------
;       KEYWORDS:
;         FILE: the path to the MODIS file. If not set, a dialog window will open
;         SUBSET_LL : Subset corners in Lat Lons
;         SUBSET_IJ : subset indexes in the HDF file (to define using SUBSET_LL)
;         LL_DATUM  : datum in which the Lat and Lons are defined. Default: WGS-84
;              
;       
;       =================================================================
;       Methods:
;       ----------------------
;       The following methods can be used directly. Non ducumented methods 
;       are not for external use.
;       + HDF-EOS methods
;       + GRID2D methods
;       Overrided methods:
;       obj->Get_Var()  : get a specific variable along with some information, coherent with the grid geoloc
;       obj->quickPlotVar    : plots a "flat" image of a given variable, coherent with the grid geoloc and adding geoloc infos to th image
;       New methods:
;       obj->define_subset() : to subset MODIS data to a rgion of interest accordingly to its intern geoloc.
;       
;       =================================================================
;       
;-
;-----------------------------------------------------------------------

;-----------------------------------------------------------------------
;+
; NAME:
;       MODIS_Grid__Define
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
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO MODIS_Grid__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {MODIS_Grid                         ,  $
            INHERITS HDF_EOS                   ,  $ ; For HDF file methods (get_Var, dump, etc.)
            INHERITS Grid2D                    ,  $ ; For geolocalisation methods 
            t0:              {ABS_DATE}        ,  $ ; first available time
            t1:              {ABS_DATE}        ,  $ ; last available time
            subset:          [0l,0l,0l,0l]     ,  $ ; if not equal to 0, it holds the indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). It should not be set manually but using the #define_subset# method.
            cropped:         ''                   $ ; Set to "TRUE" or "FALSE" at the first initialisation and updated with #define_subset#
            }
    
END

;-----------------------------------------------------------------------
;+
; NAME:
;       MODIS_Grid::Init
;
; PURPOSE:
;       Build function. I proposes subsetting tools, but subsetting is also
;       possible once the object is created using the #define_subset# method.
;
; CATEGORY:
;       WAVE grid objects
;
; KEYWORDS:
;       FILE      : the path to the MODIS file. If not set, a dialog window will open
;       SUBSET_LL : set it to the desired subset corners to automatically subset the data.
;                   Format : [ul_lon, ul_lat, dr_lon, dr_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). 
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the #define_subset# method.
;       LL_DATUM  : datum in which the Lat and Lons are defined. Default: WGS-84
;
; OUTPUT:
;       1 if the MODIS_Grid object is created successfully, 0 if not
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
Function MODIS_Grid::Init, FILE = file, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum


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
  if not KEYWORD_SET(file) then begin
    file = DIALOG_PICKFILE(TITLE='Please select MODIS_Grid file to read', /MUST_EXIST)
    IF file EQ "" THEN MESSAGE, WAVE_Std_Message(/FILE)
  endif
  
  ;*****************
  ; Check validity *
  ;*****************
  if not EOS_Query(file, info) then message, WAVE_Std_Message(/FILE)
  IF NOT self->HDF_EOS::Init(file = file) THEN RETURN, 0
     
  ;*********
  ; Geoloc *
  ;*********
  self.cropped = ''
  if NOT self->define_subset(SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum) THEN RETURN, 0
  
  ;******************************
  ; get date/time from metadata *
  ;******************************
  attindex = hdf_sd_attrfind(self.HDFid, 'CoreMetadata.0')
   if (attindex ge 0) then hdf_sd_attrinfo, self.hdfid, attindex, name=name, type=type, data=data $
     else message, 'CoreMetadata.0 not present in the HDF global attributes.'
  
  ; get range beginning date/time
  d = utils_EOD_get_metadata(data,'RangeBeginningDate',10)
  st_date = strmid(d,8,2)+'.'+strmid(d,5,2)+'.'+strmid(d,0,4)
  st_time = utils_EOD_get_metadata(data,'RangeBeginningTime',8)
  self.t0 = MAKE_ABS_DATE(DATE_STR=st_date, TIME_STR=st_time)
  
  ; get range ending date/time
  d = utils_EOD_get_metadata(data,'RangeEndingDate',10)
  st_date = strmid(d,8,2)+'.'+strmid(d,5,2)+'.'+strmid(d,0,4)
  st_time = utils_EOD_get_metadata(data,'RangeBeginningTime',8)
  self.t1 = MAKE_ABS_DATE(DATE_STR=st_date, TIME_STR=st_time)
  
  ;bonus
  self.meta = info.GRID_NAMES[0]
  
  ; ok
  RETURN, 1
  
END

;-----------------------------------------------------------------------
;+
; NAME:
;       MODIS_Grid::GetProperty
;
; PURPOSE:
;       Get access to some params. 
;
; CATEGORY:
;       WAVE grid objects
; 
; KEYWORDS:
;       Output:
;       t0 
;       t1     
;       _Ref_Extra : see #HDF_EOS:GetProperty# and #Grid2D::GetProperty#
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
PRO MODIS_Grid::GetProperty, $  
                   t0 =  t0, $
                   t1 =  t1, $    
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
  
  IF Arg_Present(t0) NE 0 THEN t0 = self.t0    
  IF Arg_Present(t1) NE 0 THEN t1 = self.t1    
  self->HDF_EOS::GetProperty, _Extra=extra
  self->Grid2D::GetProperty, _Extra=extra
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       MODIS_Grid::get_Var
;
; PURPOSE:
;       Overriding the #HDF::get_VAR# method.
;       
;       Extracts the desired variable from the HDF file. The data obtained from this method
;       is consistent with the grid geolocalisation and should not be modified externally
;       without using the suitable methods (see #Grid2D#).
;
; CATEGORY:
;       WAVE grid objects
;       
; INPUT:
;       varid : HDF SD index (int) or name (string) of the desired variable
;       
; OUTPUT:
;       the variable
;       
; KEYWORDS:
;        description: (O) If available, the description of the variable
;        units: (O) If available, the units of the variable
;        varname: (O)the name of the variable
;        dims : (O)the variable dimensions
;        /NO_CALIB: the default behaviour id to check if calibration data is contained 
;                   in the HDF variable attributes and apply it to the variable. Set this
;                   keyword to avoid making this automatic calibration
;
; MODIFICATION HISTORY:
;       Written by: FaM, 2010
;       Modified:   05-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
function MODIS_Grid::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to HDF_VARDEF or HDF_VARID, or the name of the variable.                       
                       description = description , $ ; If available, the description of the variable
                       units = units, $ ; If available, the units of the variable
                       varname = varname , $  ; the name of the variable
                       dims = dims, $ ; the variable dimensions
                       NO_CALIB = no_calib ; the variable dimensions
                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
  
  if self.cropped eq 'FALSE' then begin
    data = self->HDF::get_Var(Varid, $
      description = description , units = units, varname = varname , dims = dims, NO_CALIB = no_calib )
  endif else begin
     data = self->HDF::get_Var(Varid, $
                               start = [self.subset[0],self.subset[1]], $
                               count = [self.subset[2],self.subset[3]], $
                               description = description , units = units, varname = varname , dims = dims, NO_CALIB = no_calib )  
  endelse
  
  return, ROTATE(data, 7) ; Because MODIS data is upside down
  
end

;-----------------------------------------------------------------------
;+
; NAME:
;       MODIS_Grid::quickPlotVar
;
; PURPOSE:
;       flat plot of a desired variable for quick visualisation purposes
;
; CATEGORY:
;       WAVE grid objects
; 
; INPUT:
;       varid : HDF SD index or name of the desired variable
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
pro MODIS_Grid::quickPlotVar, Varid, NO_CALIB = NO_CALIB, LON_LAT = lon_lat

  var = self->get_Var(Varid, varname = varname, units = units, DESCRIPTION=DESCRIPTION, NO_CALIB = NO_CALIB)
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
    
  if KEYWORD_SET(lon_lat) then begin
  
    self->Get_LonLat, lon, lat
    if N_ELEMENTS(lon) ne N_ELEMENTS(var) then message, '???'
    QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='MODIS view: ' + self.fname, DIMNAMES=['lon','lat'] ,CBARTITLE=units, $
      COORDX=lon, COORDY=lat
      
  endif else  QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='MODIS view: ' + self.fname, CBARTITLE=units

end

;-----------------------------------------------------------------------
;+
; NAME:
;       MODIS_Grid::define_subset
;
; PURPOSE:
;       It is called during the object instancing but can be called also once the object is created.
;       It subsets the original data to a region of interest and actualises Geolocalisation accordingly.
;       Future calls to #MODIS_Grid::get_var# will return the subseted data. 
;       
;       To reset to the original geoloc just call this method without arguments.
;
; CATEGORY:
;       WAVE grid objects
;
; KEYWORDS:
;       SUBSET_LL : (I)   set it to the desired subset corners to automatically subset the data.
;                         Format : [ul_lon, ul_lat, dr_lon, dr_lat]. (it is assumed that
;                         lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : (I/O) indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). 
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
function MODIS_Grid::define_subset, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum

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
  
  ;***************
  ; OK. Let's go *
  ;***************

  ; Get info from the HDF-EOS file
  eosgdfid = EOS_GD_OPEN(self.path, /READ)
  if self.NUM_GRIDS ne 1 then message, 'The EOS file has not the expected number of grids (only one is espected)'
  dummy = EOS_Query(self.path, info)
  gridID = EOS_GD_ATTACH(eosgdfid, info.GRID_NAMES)
  
  res = EOS_GD_PROJINFO(gridID, projcode, zonecode, spherecode, projparm)
  if res eq -1 then message, WAVE_Std_Message(/FILE)  
  if PROJCODE ne 16 then message, 'EOS Projection code currently not supported. (' + str_equiv(PROJCODE) + ')
  res = EOS_GD_GRIDINFO(gridID, xdimsize, ydimsize, upleft, lowright)
  if res eq -1 then message, WAVE_Std_Message(/FILE)  
  status = EOS_GD_CLOSE(eosgdfid)
  
  ; EOS closed. Now make the GIS structures
  x0 = upleft[0]
  y0 = upleft[1]
  x1 = lowright[0]
  y1 = lowright[1]
  proj_str = str_equiv(projcode) +  ', ' + STRING(PROJPARM[0], FORMAT='(F11.2)') + ', 0.0, 0.0, 0.0, WGS-84, EOS Sinusoidal'
  GIS_make_proj, ret, proj, PARAM = proj_str
  
  ;*****************************************
  ; First, define the ORIGINAL grid geoloc *
  ;*****************************************
  if FIRSTCALL then begin
    IF NOT self->grid2D::Init(   nx = xdimsize          , $
                                 ny = ydimsize          , $
                                 x0 = x0                , $
                                 y0 = y0                , $
                                 x1 = x1                , $
                                 y1 = y1                , $
                                 proj = proj) THEN RETURN, 0
  endif else begin
   IF NOT self->grid2D::ReInit(  nx = xdimsize          , $
                                 ny = ydimsize          , $
                                 x0 = x0                , $
                                 y0 = y0                , $
                                 x1 = x1                , $
                                 y1 = y1                , $
                                 proj = proj) THEN RETURN, 0
  endelse
     
  ;*********
  ; Subset *
  ;*********  
  self.cropped = 'FALSE'
  if N_ELEMENTS(SUBSET_LL) eq 4 then begin
    if KEYWORD_SET(ll_datum) then begin
      if not arg_okay(ll_datum, STRUCT={TNT_DATUM}) then Message, WAVE_Std_Message('ll_datum', STRUCT={TNT_DATUM})
      dat = ll_datum
    endif else GIS_make_datum, ret, dat, NAME='WGS-84'
    self->transform_LonLat, SUBSET_LL[0], SUBSET_LL[1], dat, iul, jul, /NEAREST
    jul = self.tnt_c.ny - jul  - 1 ;up and down again ;)
    self->transform_LonLat, SUBSET_LL[2], SUBSET_LL[3], dat, idr, jdr, /NEAREST
    jdr = self.tnt_c.ny - jdr  - 1 ;up and down again ;)
    
    ;**********
    ; Errors? *
    ;**********
    if iul lt 0 then begin
      iul = 0
      MESSAGE, 'Upper left corner out of X range. Setting to 0.', /INFORMATIONAL
    endif
    if iul gt (self.tnt_c.nx - 1) then begin
      iul = (self.tnt_c.nx - 1)
      MESSAGE, 'Upper left corner out of X range. Setting to (nx - 1).', /INFORMATIONAL
    endif
    if jul lt 0 then begin
      jul = 0
      MESSAGE, 'Upper left corner out of Y range. Setting to 0.', /INFORMATIONAL
    endif
    if jul gt (self.tnt_c.ny - 1) then begin
      jul = (self.tnt_c.ny - 1)
      MESSAGE, 'Upper left corner out of Y range. Setting to (ny - 1).', /INFORMATIONAL
    endif
    if idr lt 0 then begin
      idr = 0
      MESSAGE, 'Down right corner out of X range. Setting to 0.', /INFORMATIONAL
    endif
    if idr gt (self.tnt_c.nx - 1) then begin
      idr = (self.tnt_c.nx - 1)
      MESSAGE, 'Down right corner out of X range. Setting to (nx - 1).', /INFORMATIONAL
    endif
    if jdr lt 0 then begin
      jdr = 0
      MESSAGE, 'Down right corner out of Y range. Setting to 0.', /INFORMATIONAL
    endif
    if jdr gt (self.tnt_c.ny - 1) then begin
      jdr = (self.tnt_c.ny - 1)
      MESSAGE, 'Down right corner out of Y range. Setting to (ny - 1).', /INFORMATIONAL
    endif             
    cx = idr - iul + 1
    cy = jdr - jul + 1    
    if (cx lt 1) or (cy lt 1) then MESSAGE, 'Subset_LL corners are not compatible.'  ; Fatal error 
    self.subset = [iul,jul,cx,cy]
  endif else if N_ELEMENTS(SUBSET_ij) eq 4 then begin
    if (SUBSET_ij[0] lt 0) or (SUBSET_ij[1] lt 0) or (SUBSET_ij[2] lt 1) or (SUBSET_ij[3] lt 1) $
       then MESSAGE, 'Subset_IJ corners are not compatible.'  ; Fatal error     
    self.subset = SUBSET_ij
  endif else self.subset = [0,0,0,0] ; NO subset

  if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = self.subset
  
  if TOTAL(self.subset) ne 0 then begin
    
    x0 = x0 + self.tnt_c.dx*self.subset[0]
    y0 = y0 - self.tnt_c.dy*self.subset[1]

    
    IF NOT self->grid2D::reInit(  nx = self.subset[2]    , $
                                  ny = self.subset[3]    , $
                                  dx = self.tnt_c.DX     , $
                                  dy = self.tnt_c.DY     , $
                                  x0 = x0                , $
                                  y0 = y0                , $
                                  proj = self.tnt_c.proj) THEN RETURN, 0
    self.cropped = 'TRUE'
    
  endif
  
  return, 1 ;OK

end
