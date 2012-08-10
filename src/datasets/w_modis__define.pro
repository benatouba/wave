; docformat = 'rst'
;+
;      
;       w_MODIS is the basis class for MODIS level 2 & 3 files. It reads
;       the geolocalisation from the HDF-EOS file and provides subseting 
;       tools to the user, as well as all the #w_Grid2D# transformation tools.
;       
;       Tested products so far::
;       MOD10A1
;       MOD11A1
;       MOD13A1       
;       But there should be no limitiations in using it for other L2 L3 products.
;       
;       The following methods can be used directly. Non ducumented methods 
;       are not for external use.
;       + HDF-EOS methods
;       + w_Grid2D methods
;       Overrided methods:
;       obj->Get_Var()  : get a specific variable along with some information, coherent with the grid geoloc
;       obj->QuickPlotVar    : plots a "flat" image of a given variable, coherent with the grid geoloc and adding geoloc infos to th image
;       New methods:
;       obj->define_subset() : to subset MODIS data to a rgion of interest accordingly to its intern geoloc.
;               
;      :Properties:
; 
;          +w_HDF_EOS attributes  
;          +w_Grid2D attributes  
;          t0: in, type = {ABS_DATE}      
;              first available time
;          t1: in, type = {ABS_DATE}      
;                last available time
;          subset: in, type = [0l,0l,0l,0l]   
;                 if not equal to 0, it holds the indexes in the ORIGINAL hdf grid array
;                 in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). It should not be set 
;                 manually but using the #define_subset# method.
;          cropped:         ''              
;                  Set to "TRUE" or "FALSE" at the first initialisation and updated with #define_subset#
;          
;          FILE: in, optional, type = string
;                the path to the MODIS file. If not set, a dialog window will open
;         SUBSET_LL: in, optional, type = float vector 
;                    Subset corners in Lat Lons
;         SUBSET_IJ: in, type = long vector
;                    Subset indexes in the HDF file (to define using SUBSET_LL)
;         LL_DATUM: in, type = {TNT_DATUM}, default = WGS-84 
;                   datum in which the Lat and Lons are defined. Default: WGS-84
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
;    Object structure definition. Attributes::
;   
;     w_MODIS                      
;            INHERITS w_HDF_EOS                   
;            INHERITS w_Grid2D                   
;            t0 : {ABS_DATE}        
;            t1 : {ABS_DATE}        
;            subset : [0l,0l,0l,0l]    
;            cropped : ''                  
;
; :Categories:
;         WAVE/OBJ_GIS 
;          
; :History:
;     Written by FaM, 2010.
;-
PRO w_MODIS__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {w_MODIS                         ,  $
            INHERITS w_HDF_EOS              ,  $ ; For HDF file methods (get_Var, dump, etc.)
            INHERITS w_Grid2D               ,  $ ; For geolocalisation methods 
            t0:              0LL            ,  $ ; first available time
            t1:              0LL            ,  $ ; last available time
            subset:          [0l,0l,0l,0l]  ,  $ ; if not equal to 0, it holds the indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). It should not be set manually but using the #define_subset# method.
            cropped:         ''                $ ; Set to "TRUE" or "FALSE" at the first initialisation and updated with #define_subset#
            }
    
END

;+
; :Description:
;    Build function. I proposes subsetting tools, but subsetting is also
;    possible once the object is created using the #define_subset# method.
;    
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;       FILE: in, optional, type = string 
;             the path to the MODIS file. If not set, a dialog window will open
;       SUBSET_LL: in, optional, type = float vector 
;                  set it to the desired subset corners to automatically subset the data.
;                  Format : [ul_lon, ul_lat, dr_lon, dr_lat]. (it is assumed that
;                  lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ: in, type = long vector
;                  indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny] (y=0 is at the top). 
;                  Unless you know what you do, it should not be set manually but 
;                  retrieved using the #define_subset# method.
;       LL_DATUM: in, type = {TNT_DATUM}, default = WGS-84
;                 datum in which the Lat and Lons are defined. Default: WGS-84
;                 
;   :Returns:
;                 1 if the object is created successfully.        
;           
; :History:
;     Written by FaM, 2010.
;-
Function w_MODIS::Init, FILE = file, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum


  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    if self.hdfID gt 0 then HDF_SD_END, self.hdfID
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF
  
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(file) then begin
    file = DIALOG_PICKFILE(TITLE='Please select w_MODIS file to read', /MUST_EXIST)
    IF file EQ "" THEN MESSAGE, WAVE_Std_Message(/FILE)
  endif
  
  ;*****************
  ; Check validity *
  ;*****************
  if not EOS_Query(file, info) then message, WAVE_Std_Message(/FILE)
  IF NOT self->w_HDF_EOS::Init(file = file) THEN RETURN, 0
     
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
  self.t0 = QMS_TIME(DATE_STR=st_date, TIME_STR=st_time)
  
  ; get range ending date/time
  d = utils_EOD_get_metadata(data,'RangeEndingDate',10)
  st_date = strmid(d,8,2)+'.'+strmid(d,5,2)+'.'+strmid(d,0,4)
  st_time = utils_EOD_get_metadata(data,'RangeBeginningTime',8)
  self.t1 = QMS_TIME(DATE_STR=st_date, TIME_STR=st_time)
  
  ;bonus
  self.meta = info.GRID_NAMES[0]
  
  ; ok
  RETURN, 1
  
END

;+
; :Description:
;    Destroy function. 
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :History:
;     Written by FaM, 2010.
;-      
pro w_MODIS::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_Grid2D::Cleanup
  self->w_HDF_EOS::Cleanup
  
END

;+
; :Description:
;    Get access to some params. 
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;    t0: out, type = {ABS_DATE}      
;        first available time
;    t1: out, type = {ABS_DATE}      
;        last available time
;    _Ref_Extra: 
;         see #w_HDF_EOS:GetProperty# and #w_Grid2D::GetProperty#
;
; :History:
;     Written by FaM, 2010.
;-
PRO w_MODIS::GetProperty, $  
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
  self->w_HDF_EOS::GetProperty, _Extra=extra
  self->w_Grid2D::GetProperty, _Extra=extra
  
end

pro w_MODIS::Get_Time, t0, t1
   
   self->GetProperty, t0 = t0, t1 = t1                
                   
end

;+
; :Description:
;    Overriding the #HDF::get_VAR# method.
;       
;       Extracts the desired variable from the HDF file. The data obtained from this method
;       is consistent with the grid geolocalisation and should not be modified externally
;       without using the suitable methods (see #w_Grid2D#).
;       
;       
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :Params:
;    Varid: in, required, type = string/integer
;           HDF SD index (int) or name (string) of the desired variable
;
; :Keywords:
;    description: out, optional, type = string 
;                 If available, the description of the variable
;    units: out, optional, type = string 
;           If available, the units of the variable
;    varname: out, optional, type = string 
;             the name of the variable
;    dims : out, optional, type = integer
;           the variable dimensions
;    NO_CALIB: optional
;              the default behaviour id to check if calibration data is contained 
;              in the HDF variable attributes and apply it to the variable. Set this
;              keyword to avoid making this automatic calibration
;  
; :Returns:
;           the variable
;
; :History:
;     Written by FaM, 2010.
;-
function w_MODIS::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to HDF_VARDEF or HDF_VARID, or the name of the variable.                       
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
    data = self->w_HDF::get_Var(Varid, $
      description = description , units = units, varname = varname , dims = dims, NO_CALIB = no_calib )
  endif else begin
     data = self->w_HDF::get_Var(Varid, $
                               start = [self.subset[0],self.subset[1]], $
                               count = [self.subset[2],self.subset[3]], $
                               description = description , units = units, varname = varname , dims = dims, NO_CALIB = no_calib )  
  endelse
  
  return, ROTATE(data, 7) ; Because MODIS data is upside down
  
end

;+
; :Description:
;    Flat plot of a desired variable for quick visualisation purposes
;    
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :Params:
;    Varid: in, required, type = string/integer
;           HDF SD index (int) or name (string) of the desired variable
;
; :Keywords:
;    NO_CALIB:
;             the default behaviour id to check if calibration data is contained 
;             in the HDF variable attributes and apply it to the variable. Set this
;             keyword to avoid making an automatic calibration
;    LON_LAT:
;             if set, the geolocalisation will be shown too (long process by
;             large uncropped files)
;    WID: out
;         the widget id
;              
; :History:
;     Written by FaM, 2010.
;-
pro w_MODIS::QuickPlotVar, Varid, NO_CALIB = NO_CALIB, LON_LAT = lon_lat, WID = wid

  var = self->get_Var(Varid, varname = varname, units = units, DESCRIPTION=DESCRIPTION, NO_CALIB = NO_CALIB)
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
    
  if KEYWORD_SET(lon_lat) then begin
  
    self->Get_LonLat, lon, lat
    if N_ELEMENTS(lon) ne N_ELEMENTS(var) then message, '???'
    w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='MODIS view: ' + self.fname, DIMNAMES=['lon','lat'] ,CBARTITLE=units, $
      COORDX=lon, COORDY=lat, WID = wid
      
  endif else  w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='MODIS view: ' + self.fname, CBARTITLE=units, WID = wid

end

;+
; :Description:
;    It is called during the object instancing but can be called also once the object is created.
;       It subsets the original data to a region of interest and actualises Geolocalisation accordingly.
;       Future calls to #w_MODIS::get_var# will return the subseted data. 
;       
;       To reset to the original geoloc just call this method without arguments.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;       SUBSET_LL : in, type = float vector
;                set it to the desired subset corners to automatically subset the data. Form = [ul_lon, ul_lat, dr_lon, dr_lat]
;                (it is assumed that lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : in, out, type = long vector  
;                   indexes in the ORIGINAL hdf grid array in the form [x_ul,y_ul,nx,ny](y=0 is at the top). 
;                   Unless you know what you do, it should not be set manually.
;                   One can retrive it from this method by setting it to a named variable                         
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84 
;                   datum in which the Lat and Lons are defined. Default: WGS-84
;
; :History:
;     Written by FaM, 2010.
;-
function w_MODIS::define_subset, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
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
  if self.NUM_GRIDS gt 1 then message, 'The EOS file has not the expected number of grids (only one is espected)'
  dummy = EOS_Query(self.path, info)
  gridID = EOS_GD_ATTACH(eosgdfid, info.GRID_NAMES)
  
  res = EOS_GD_PROJINFO(gridID, projcode, zonecode, spherecode, projparm)
  if res eq -1 then message, WAVE_Std_Message(/FILE)  
  
  case projcode of
    16: begin
      proj_str = str_equiv(projcode) +  ', ' + STRING(PROJPARM[0], FORMAT='(F11.3)') + ', 0.0, 0.0, 0.0, WGS-84, EOS Sinusoidal'
      GIS_make_proj, ret, proj, PARAM = proj_str
    end
    0: begin
      GIS_make_proj, ret, proj, NAME='Geographic (WGS-84)'
    end
    else: message, 'EOS Projection code currently not supported. (' + str_equiv(projcode) + ')
  endcase

  res = EOS_GD_GRIDINFO(gridID, xdimsize, ydimsize, upleft, lowright)
  if res eq -1 then message, WAVE_Std_Message(/FILE)   
  status = EOS_GD_DETACH(gridID)   
  status = EOS_GD_CLOSE(eosgdfid)
  
  ; EOS closed. Now make the GIS structures
  x0 = upleft[0]
  y0 = upleft[1]
  x1 = lowright[0]
  y1 = lowright[1]
  ; The UpperLeftPointMtrs is in projection coordinates, and identifies the very 
  ; upper left corner of the upper left pixel of the image data
  ; The LowerRightMtrs identifies the very lower right corner of the lower right pixel of the image data. 
  ; These projection coordinates are the only metadata that accurately reflect the extreme corners of the gridded image 
  dx = (x1 - x0) / double(xdimsize)/2d
  dy = (y1 - y0) / double(ydimsize)/2d
  x0 += dx
  x1 -= dx
  y0 += dy
  y1 -= dy  
  
  
  ;*****************************************
  ; First, define the ORIGINAL grid geoloc *
  ;*****************************************
  if FIRSTCALL then begin
    IF NOT self->w_Grid2D::Init( nx = xdimsize          , $
                                 ny = ydimsize          , $
                                 x0 = x0                , $
                                 y0 = y0                , $
                                 x1 = x1                , $
                                 y1 = y1                , $
                                 proj = proj) THEN RETURN, 0
  endif else begin
   IF NOT self->w_Grid2D::ReInit(  nx = xdimsize          , $
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
      if ~ !QUIET then MESSAGE, 'Upper left corner out of X range. Setting to 0.', /INFORMATIONAL
    endif
    if iul gt (self.tnt_c.nx - 1) then begin
      iul = (self.tnt_c.nx - 1)
      if ~ !QUIET then MESSAGE, 'Upper left corner out of X range. Setting to (nx - 1).', /INFORMATIONAL
    endif
    if jul lt 0 then begin
      jul = 0
      if ~ !QUIET then MESSAGE, 'Upper left corner out of Y range. Setting to 0.', /INFORMATIONAL
    endif
    if jul gt (self.tnt_c.ny - 1) then begin
      jul = (self.tnt_c.ny - 1)
      if ~ !QUIET then MESSAGE, 'Upper left corner out of Y range. Setting to (ny - 1).', /INFORMATIONAL
    endif
    if idr lt 0 then begin
      idr = 0
      if ~ !QUIET then MESSAGE, 'Down right corner out of X range. Setting to 0.', /INFORMATIONAL
    endif
    if idr gt (self.tnt_c.nx - 1) then begin
      idr = (self.tnt_c.nx - 1)
      if ~ !QUIET then MESSAGE, 'Down right corner out of X range. Setting to (nx - 1).', /INFORMATIONAL
    endif
    if jdr lt 0 then begin
      jdr = 0
      if ~ !QUIET then MESSAGE, 'Down right corner out of Y range. Setting to 0.', /INFORMATIONAL
    endif
    if jdr gt (self.tnt_c.ny - 1) then begin
      jdr = (self.tnt_c.ny - 1)
      if ~ !QUIET then MESSAGE, 'Down right corner out of Y range. Setting to (ny - 1).', /INFORMATIONAL
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

    
    IF NOT self->w_Grid2D::reInit(  nx = self.subset[2]    , $
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

pro w_MODIS::write_envi_hdr, FILE = file

    if N_ELEMENTS(file) eq 0 then begin
      file = Dialog_Pickfile(/Write, File= self.fname + '.hdr')
    endif
    
    self->GetProperty, TNT_C = c
    
    OPENW, lu, file, /GET_LUN
    printf, lu, 'ENVI'
    printf, lu, 'samples = ' + str_equiv(c.nx)
    printf, lu, 'lines   = ' + str_equiv(c.ny)
    printf, lu, 'bands   = 1'
    printf, lu, 'data type = 1'
    printf, lu, 'header offset = 0'
    printf, lu, 'byte order = 0'
    str = '{Sinusoidal, 1.0000, 1.0000, ' + STRING(c.x0 - c.dx/2., FORMAT='(F13.4)') +', ' + STRING(c.y0 + c.dy/2., FORMAT='(F13.4)')
    str += ', ' + STRING(c.dx, FORMAT='(F9.5)') + ', ' + STRING(c.dy, FORMAT='(F9.5)') + ', , units=Meters}'
    printf, lu, 'map info = ' + str

    
    printf, lu, 'projection info = {16, 6371007.2, 0.000000, 0.0, 0.0, Sinusoidal, units=Meters}'
    printf, lu, 'coordinate system string = {PROJCS["Sinusoidal",GEOGCS["GCS_ELLIPSE_BASED_1",DATUM["D_ELLIPSE_BASED_1",SPHEROID["S_ELLIPSE_BASED_1",6371007.181,0.0]],PRIMEM["Greenwich",0.0],UNIT["Degree",0.0174532925199433]],PROJECTION["Sinusoidal"],PARAMETER["False_Easting",0.0],PARAMETER["False_Northing",0.0],PARAMETER["Central_Meridian",0.0],UNIT["Meter",1.0]]}'
    
    close, lu ; close file
    
    add = utils_replace_string(file, '.hdr', '.img')
    img = BYTARR(c.nx, c.ny)
    save,  img, FILENAME=add


end
