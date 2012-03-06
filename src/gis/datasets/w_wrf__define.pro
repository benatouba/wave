; docformat = 'rst'
;+
;
;  w_WRF is the basis class for WRF datasets.
;  
;  
; :Properties:
;      
;      
; :Author: Fabien Maussion::
;            FG Klimatologie
;            TU Berlin
;
; :History:
;     Written by FaM, 2010.
;
;       Modified::
;          06-Apr-2011 FaM
;          Documentation for upgrade to WAVE 0.1
;
;-      



;+
; :Description:
;    Object structure definition. Attributes::
;       w_WRF                   
;            INHERITS w_Grid2D           
;            INHERITS w_GEO_nc           
;            type              : ''   
;                                type of active file: AGG, WRF, GEO, MET or INP
;            version           : ''  
;                                WRF version
;            hstep             : {TIME_STEP} 
;                                Time step of the file
;            dom               : 0L 
;                                id of the considered nested domain
;            west_east         : 0L 
;                                original X dimension (unstaggered)
;            south_north       : 0L 
;                                original Y dimension (unstaggered)
;            bottom_top        : 0L 
;                                original Z dimension (unstaggered)
;            dx                : 0D 
;                                grid spacing in m
;            dy                : 0D 
;                                grid spacing in m
;            i_parent_start    : 0L 
;                                i index of the start point in parent domain
;            j_parent_start    : 0L 
;                                j index of the start point in parent domain
;            parent_grid_ratio : 0L 
;                                ratio to parent
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :History:
;     Written by FaM, 2010.
;-      
PRO w_WRF__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { w_WRF                    ,  $
            INHERITS w_Grid2D         ,  $
            INHERITS w_GEO_nc         ,  $
            type:               ''    ,  $ ; type of active file: PRO, AGG, WRF, GEO, MET or INP
            ndiagvar:               0L,  $ ; Number of diagnostic variables
            diagVars:        PTR_NEW()   $ ; Diagnostic Variables (derived from WRF output)
            }
    
END

;+
; :Description:
;    Defines a subset of the WRF data. This has two major consequences. First,
;    the geolocalisation of the WRF object is changed (w_grid2D) accordingly. 
;    Second, the `w_WRF::get_var` is automatically returning data corresponding
;    to the chosen subset.
;    
;    Call this function without arguments to reset the WRF object to its original
;    form.
;    
;    Internally, the w_Grid2D::set_ROI method is called but the indexes of the
;    spatially heterogen ROI are then used to create the 
;    
;    Currently one can define a subset with 6 different methods::
;      1. SHAPE: use a .shp file as imput, the ROI is generated using the 
;                IDLanROI object to transform the vertices into a mask
;                (don't forget to set the SRC keyword)
;      2. POLYGON: similar to SHAPE but the polygon is directly specified
;                 (don't forget to set the SRC keyword)
;      3. MASK: simply define the ROI using a mask (of dimensions corresponding to the grid)
;      4. CROPBORDER: remove a certain number of pixels from the borders of the grid
;      5. GRID: the ROI is defined by the position of another grid within the object grid
;      6. CORNERS: the ROI is defined by setting the LL and UR corners of the desired subset
;                 (don't forget to set the SRC keyword)
;
;
; :Keywords:
;    SHAPE: in, type = string
;           the shapefile to read (.shp), coordinate system defined by `SRC`
;    POLYGON: in, type = array
;             not implemented yet, coordinate system defined by `SRC`
;    MASK: in, type = array
;          a mask of the desired ROI (dimensions must match the grid)
;    CROPBORDER: in, type = long
;                number of pixels to remove from each border
;    GRID: in, type = w_Grid2D
;          a grig object
;    CORNERS: in, type = array 
;            LL and UR corners of the desired subset ([XLL,YLL,XDR,YDR]), coordinate system defined by `SRC`
;    NO_ERASE: in, type = boolean
;              set this keyword to update the ROI in place of replacing it
;    SRC: in, optional
;         the polygon or shape coordinate system (datum or proj) default is WGS-84
;    REMOVE_ENTITITES:in, optional, type = long
;                     an array containing the id of the shape entities to remove from the shape
;                     All other entities are plotted normally.
;    KEEP_ENTITITES:in, optional, type = long
;                   an array containing the id of the shape entities to keep for the shape. 
;                   All other entities are ignored.
;    ROI_MASK_RULE: Set this keyword to an integer specifying the rule used to determine whether
;                   a given pixel should be set within the mask when computing a mask with
;                   polygons or shapes. Valid values include::
;                       * 0 = Boundary only. All pixels falling on a region's boundary are set.
;                       * 1 = Interior only. All pixels falling within the region's boundary, but not on the boundary, are set.
;                       * 2 = Boundary + Interior. All pixels falling on or within a region's boundary are set. This is the default.
;
; :Returns:
;   1 if the subset has been set correctly, 0 if not
;
; :History:
;     Written by FaM, 2011.
;
;-
function w_WRF::define_subset,  SUBSET_LL  = subset_ll,  $ ; Place holder for backwards compatibility
                                LL_DATUM   = ll_datum ,  $ ; Place holder for backwards compatibility
                                SHAPE=shape,  $
                                POLYGON=polygon, MASK=mask,  $
                                CROPBORDER=cropborder,  $
                                GRID=grid,    $                          
                                CORNERS=corners, $
                                NO_ERASE=no_erase, $ 
                                SRC=src, $
                                REMOVE_ENTITITES=remove_entitites, $ 
                                KEEP_ENTITITES=keep_entitites, $
                                ROI_MASK_RULE=roi_mask_rule

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, self->define_subset()
  ENDIF
  
  ;*******************
  ; check what to do *
  ;*******************
  firstcall = self.cropped eq '' ;first init
  if N_ELEMENTS(SUBSET_LL) ne 0 then begin
    CORNERS = SUBSET_LL
    if N_ELEMENTS(ll_datum) ne 0 then src=ll_datum else GIS_make_datum, ret, src, NAME='WGS-84'
    message, 'SUBSET_LL Keyword is depreciated. Use CORNERS with SRC instead', /INFORMATIONAL
  endif
        
  ;************
  ; GRID info *
  ;************      
  if self.type eq 'PRO' then begin
    x = self->get_Var('west_east')
    y = self->get_Var('south_north')         
    nx = N_ELEMENTS(x)        
    ny = N_ELEMENTS(y)              
    dx = x[1]-x[0] ;TODO CHANGE this!!
    dy = y[1]-y[0] ;TODO CHANGE this!!
    x0 = min(x)             
    y0 = max(y) 
  endif else begin
    center_lat = self->get_Gatt('CEN_LAT')
    center_lon = self->get_Gatt('CEN_LON')
    ; Get easting and northings from dom center
    GIS_coord_trafo, ret, center_lon, center_lat, e, n, SRC=self.tnt_c.proj.datum, DST= self.tnt_c.proj
    nx = self->get_Gatt('WEST-EAST_GRID_DIMENSION')-1
    ny = self->get_Gatt('SOUTH-NORTH_GRID_DIMENSION')-1
    dx = self->get_Gatt('DX')
    dy = self->get_Gatt('DY')
    x0 =  - (nx-1) / 2. * dx + e ; UL corner
    y0 =    (ny-1) / 2. * dy + n ; UL corner
  endelse
  
  if FIRSTCALL then begin
    IF NOT self->w_Grid2D::Init(  nx = nx                , $
                                  ny = ny                , $
                                  dx = dx                , $
                                  dy = dy                , $
                                  x0 = x0                , $
                                  y0 = y0                , $
                                  proj = self.tnt_c.proj) THEN RETURN, 0
    ;Temporary test
    self->Get_LonLat, gislon, gislat
    self->get_ncdf_coordinates, lon, lat
    if max(abs(gislon-lon)) gt 1e-4 then Message, 'My lons different from the file lons? Diff: ' + str_equiv(max(abs(gislon-lon))), /INFORMATIONAL
    if max(abs(gislat-lat)) gt 1e-4 then Message, 'My lats different from the file lats? Diff: ' + str_equiv(max(abs(gislat-lat))), /INFORMATIONAL
    
  endif else begin
   IF NOT self->w_Grid2D::ReInit(  nx = nx                , $
                                   ny = ny                , $
                                   dx = dx                , $
                                   dy = dy                , $
                                   x0 = x0                , $
                                   y0 = y0                , $
                                   proj = self.tnt_c.proj) THEN RETURN, 0  
  endelse
  
  
  if not self->set_ROI(SHAPE=shape,  $
    POLYGON=polygon, MASK=mask,  $
    CROPBORDER=cropborder,  $
    GRID=grid,    $
    CORNERS=corners, $
    NO_ERASE=no_erase, $
    SRC=src, $
    REMOVE_ENTITITES=remove_entitites, $
    KEEP_ENTITITES=keep_entitites, $
    ROI_MASK_RULE=roi_mask_rule) THEN RETURN, 0
  
  dummy = self->w_GEO_nc::define_subset()
  if self->is_ROI() then begin
    self->get_ROI, SUBSET=subset
    if ~self->w_GEO_nc::define_subset(SUBSET=SUBSET) then return, 0 
    new_grid = self->reGrid(/TO_ROI)
    IF NOT self->w_Grid2D::ReInit(grid=new_grid) THEN RETURN, 0  
    dummy = self->set_ROI()    
    undefine, new_grid
  endif 
    
  return, 1
    
end
   
;+
; :Description:
;    Build function.
;
; :Keywords:
;       FILE      : in, optional, type = string
;                   the path to the WRF file. If not set, a dialog window will open
;       _REF_EXTRA: in, optional
;                   all keywords accepted by w_WRF::define_subset
;
; :Returns:
; 
;    1 if the object is created successfully. 
;    
; :History:
;     Written by FaM, 2010.
;-
Function w_WRF::Init, FILE=file, _REF_EXTRA=extra  
           
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
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select WRF ncdf file to read', /MUST_EXIST)
  if file eq '' then MESSAGE, WAVE_Std_Message(/FILE)
  IF NOT self->w_GEO_nc::Init(file = file) THEN RETURN, 0    
 
  ;****************
  ; Read metadata *
  ;****************    
  cdfid = self.cdfid
  
  ;*****************************************
  ; Determine the type of WRF file we have *
  ;*****************************************
  title = self->get_Gatt('TITLE')
  ftype = ''
  isHere = STRPOS(str_equiv(title), 'WRF')
  if isHere ne -1 then ftype = 'WRF'
  isHere = STRPOS(str_equiv(title), 'GEOGRID')
  if isHere ne -1 then ftype = 'GEO'
  isHere = STRPOS(str_equiv(title), 'METGRID')
  if isHere ne -1 then ftype = 'MET'
  isHere = STRPOS(str_equiv(title), 'REAL')
  if isHere ne -1 then ftype = 'REAL'
  isHere = STRPOS(str_equiv(self.fname), 'AGG')
  if isHere ne -1 then ftype = 'AGG'    
  isHere = self->get_Gatt_Info('PROJ_ENVI_STRING')
  if isHere eq 1 then ftype = 'PRO'  
  if ftype eq '' then message, 'Input file not recognized as a known WRF product.'
  self.type = ftype
  meta = str_equiv(title)
  
  ;*******
  ; Time *
  ;*******
  ; Done by w_GEO_nc
  if self.nt gt 1 then if ~check_TimeSerie(*self.time) then Message, 'Time serie in the file is not regular.'
  
  ; Projection
  if self.type eq 'PRO' then begin  
    ; Make the projection
    GIS_make_proj, ret, proj, PARAM=STRING(self->get_Gatt('PROJ_ENVI_STRING'))
    self.tnt_c.proj = proj      
  endif else begin
    center_lat = self->get_Gatt('CEN_LAT')
    center_lon = self->get_Gatt('CEN_LON')
    moad_cen_lat = self->get_Gatt('MOAD_CEN_LAT')
    stand_lon = self->get_Gatt('STAND_LON')
    truelat1 = self->get_Gatt('TRUELAT1')
    truelat2 = self->get_Gatt('TRUELAT2')
    proj_id = self->get_Gatt('MAP_PROJ')
    GIS_make_ellipsoid, ret, ell, NAME='WRF Sphere', RA=6370000.0, RB=6370000.0
    switch proj_id of
      1: begin
        ; 4 - Lambert Conformal Conic
        ;   a, b, lat0, lon0, x0, y0, sp1, sp2, [datum], name
        envi_proj = 4
        proj_param = str_equiv(envi_proj) + ', ' + $                      ;proj_id
          STRING(ell.a, FORMAT='(F16.8)') + ', ' + $            ;a
          STRING(ell.b, FORMAT='(F16.8)') + ', ' + $            ;b
          STRING(moad_cen_lat, FORMAT='(F16.8)') + ', ' + $     ;lat0
          STRING(stand_lon, FORMAT='(F16.8)') + ', ' + $        ;lon0
          '0.0' + ', ' + $                                      ;x0
          '0.0' + ', ' + $                                      ;y0
          STRING(truelat1, FORMAT='(F16.8)') + ', ' + $         ;sp1
          STRING(truelat2, FORMAT='(F16.8)') + ', ' + $         ;sp2
          'WGS-84' + ', ' + $                                   ;datum
          'WRF Lambert Conformal'                               ;name
        break
      end
      2: begin
        ; 31- Polar Stereographic
        ;   a, b, lat0, lon0, x0, y0, [datum], name
        envi_proj = 31
        proj_param = str_equiv(envi_proj) + ', ' + $                      ;proj_id
          STRING(ell.a, FORMAT='(F16.8)')+ ', ' + $             ;a
          STRING(ell.b, FORMAT='(F16.8)') + ', ' + $            ;b
          STRING(truelat1, FORMAT='(F16.8)') + ', ' + $         ;lat0
          STRING(stand_lon, FORMAT='(F16.8)') + ', ' + $        ;lon0
          '0.0' + ', ' + $                                      ;x0
          '0.0' + ', ' + $                                      ;y0
          'WGS-84' + ', ' + $                                   ;datum
          'WRF Polar Stereographic'                             ;name
        break
      end
      else: Message, 'Projection currently not supported.'
      
    endswitch
    
    ; Make the projection
    GIS_make_proj, ret, proj, PARAM=proj_param
    self.tnt_c.proj = proj
    
  endelse
    
  
  ;***********************
  ; Diagnostic variables *
  ;***********************    
  self->get_Varlist, /DIAGNOSTIC
    
  ;*********
  ; define *
  ;*********
  self.cropped = ''
  if NOT self->define_subset(_EXTRA=extra) THEN RETURN, 0
    
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
pro w_WRF::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_Grid2D::Cleanup
  self->w_GEO_nc::Cleanup
  
  for i=0, self.ndiagvar -  1 do begin 
   tvar = (*self.diagVars)[i]
   PTR_FREE, tvar.dims
   PTR_FREE, tvar.dimnames   
  endfor
  PTR_FREE, self.diagVars
  
END

;+
; :Description:
;    Get access to some params. 
;    
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;    
;    _Ref_Extra: out
;                all parent classed property
;    
; :History:
;     Written by FaM, 2010.
;-      
PRO w_WRF::GetProperty,  $
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
  
  self->w_Grid2D::GetProperty, _Extra=extra
  self->w_GEO_nc::GetProperty, _Extra=extra
  
end

;+
; :Description:
;    This function checks if a variable ID is valid and returns 1 if it is. Additionally,
;    it tries to obtain a maximum of information about the desired variable.
;    
;    This function have been enhanced for `w_WRF` to include additional diagnostic variables.
;
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    Varid: in, required, type = string/integer
;           the variable ID (string or integer) to check
;
; :Keywords:
;   out_id: out, type = long
;           the netcdf variable ID (long)
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
;         1 if the variable id is valid, 0 if not
;
;       :History:
;     Written by FaM, 2010.
;-
function w_WRF::get_Var_Info, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                              out_id = out_id, $
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
    RETURN, FALSE
  ENDIF
  
  if ~self->w_NCDF::get_Var_Info(Varid, $
                                 out_id = out_id, $
                                 units = units, $
                                 description = description, $
                                 varname = varname , $ ; 
                                 dims = dims, $ ;
                                 dimnames = dimnames) then begin
     
     ;Post processed variables
     post = (*self.diagVars).name           
     p = where(post eq str_equiv(Varid), cnt)
     if cnt eq 0 then return, FALSE  
            
     dvar = (*self.diagVars)[p[0]]
     out_id = dvar.name                   
     varname =  dvar.name                     
     units = dvar.unit                   
     description = dvar.description               
     dims = *dvar.dims 
     dimnames = *dvar.dimnames                                  
 
  endif    
  
  return, TRUE
  
end


;+
; :Description:
;    Get some informations on available variables in the NCDF file. 
;    Redefined for WRF to print the new "diagnostic" variables 
;    
; :Categories:
;         WAVE/OBJ_GIS   
;         
; :Params:
;    varid: out, type = long
;           NCDF var indexes
;    varnames: out, type = string
;              variables name
;    varndims: out, type = long
;              variables number of dimensions
;    varunits: out, type = string
;              variables units
;    vardescriptions: out, type = string
;                     variables description (or long name)
;    vartypes: out, type = string
;              variables type 
;
; :Keywords:
;    PRINTVARS: in, optional
;               to print the infos in the console
;               
;    DIAGNOSTIC: in, optional
;                to obtain the list of the wrf specific computed variables only
;                
;    ALL: in, optional
;         to obtain the list of the all the variables 
;         (ncdf variables and wrf specific computed variables) 
;               
; :History:
;     Written by FaM, 2010.
;-
pro w_WRF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS=printvars, DIAGNOSTIC=diagnostic, ALL=all

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
    
  if KEYWORD_SET(DIAGNOSTIC) then begin ; Computed Diagnostic variables 
  
    if ~ PTR_VALID(self.diagVars) then begin  ; first call
      
      dvars = {name:'',unit:'',ndims:0L,description:'',type:'',dims:PTR_NEW(),dimnames:PTR_NEW()}
      
      ;PRCP
      d1 = self->w_NCDF::get_Var_Info('RAINNC', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('RAINC')
      d3 = self->w_NCDF::get_Var_Info('SR')
      if (d1 and d2) then begin
        var = {name:'PRCP',unit:'mm',ndims:N_elements(dims),description:'Total precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'PRCP_NC',unit:'mm',ndims:N_elements(dims),description:'Grid scale precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'PRCP_C',unit:'mm',ndims:N_elements(dims),description:'Cumulus precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        if d3 then begin
          var = {name:'PRCP_FR',unit:'mm',ndims:N_elements(dims),description:'Frozen precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
        endif
      endif
            
      ;snowfall
      d1 = self->w_NCDF::get_Var_Info('SNOWNC', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SNOWFALL',unit:'mm',ndims:N_elements(dims),description:'Grid scale snow and ice (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      ;graupel
      d1 = self->w_NCDF::get_Var_Info('GRAUPELNC', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'GRAUPEL',unit:'mm',ndims:N_elements(dims),description:'Grid scale graupel (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      ;hail
      d1 = self->w_NCDF::get_Var_Info('HAILNC', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'HAIL',unit:'mm',ndims:N_elements(dims),description:'Grid scale hail (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;POTEVP
      d1 = self->w_NCDF::get_Var_Info('POTEVP', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'POTEVAP',unit:'w m-2',ndims:N_elements(dims),description:'Potential evaporation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;pressure
      d1 = self->w_NCDF::get_Var_Info('P', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('PB') 
      if (d1 and d2) then begin
        var = {name:'PRESSURE',unit:'hPa',ndims:N_elements(dims),description:'Full model pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]         
      endif           
      d1 = self->w_NCDF::get_Var_Info('PRES', DIMNAMES=dnames,DIMS=dims) ;met_em
      if (d1) then begin
        var = {name:'PRESSURE',unit:'hPa',ndims:N_elements(dims),description:'Full model pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]         
      endif 
      
      ; geopotential, z
      d1 = self->w_NCDF::get_Var_Info('PH', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('PHB')  
      if (d1 and d2) then begin
        dnames = utils_replace_string(dnames, '_stag', '')
        var = {name:'GEOPOTENTIAL',unit:'m2 s-2',ndims:N_elements(dims),description:'Full model geopotential on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]      
        var = {name:'Z',unit:'m',ndims:N_elements(dims),description:'Full model height on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]                 
      endif          
      d1 = self->w_NCDF::get_Var_Info('GHT', DIMNAMES=dnames,DIMS=dims) ;met_em
      if (d1) then begin
        var = {name:'GEOPOTENTIAL',unit:'m2 s-2',ndims:N_elements(dims),description:'Full model geopotential on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]         
        var = {name:'Z',unit:'m',ndims:N_elements(dims),description:'Full model height on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]    
      endif      
        
      ;TK and TC
      d1 = self->w_NCDF::get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)      
      d2 = self->w_NCDF::get_Var_Info('P')
      d3 = self->w_NCDF::get_Var_Info('PB')   
      if (d1 and d2 and d3) then begin
        var = {name:'TK',unit:'K',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'TC',unit:'C',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]   
        var = {name:'T2PBL',unit:'K',ndims:N_elements(dims)-1,description:'2 m temperature (extrapolated from eta-levels)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
        var = {name:'T2PBLC',unit:'C',ndims:N_elements(dims)-1,description:'2 m temperature (extrapolated from eta-levels)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]     
      endif 
  
      ;THETA
      if (d1) then begin
        var = {name:'THETA',unit:'K',ndims:N_elements(dims),description:'Potential Temperature (theta)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]    
      endif 
            
      d1 = self->w_NCDF::get_Var_Info('TT', DIMNAMES=dnames,DIMS=dims) ;Met em
      if (d1) then begin
        var = {name:'TK',unit:'K',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'TC',unit:'C',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
      endif
      
      ;T2C
      d1 = self->w_NCDF::get_Var_Info('T2', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'T2C',unit:'C',ndims:N_elements(dims),description:'2 m Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;RH
      d1 = self->w_NCDF::get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('QVAPOR')
      d3 = self->w_NCDF::get_Var_Info('P')
      d4 = self->w_NCDF::get_Var_Info('PB')
      if (d1 and d2 and d3 and d4)then begin
        var = {name:'RH',unit:'%',ndims:N_elements(dims),description:'Relative Humidity',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif

      ;RH2
      d1 = self->w_NCDF::get_Var_Info('T2', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('Q2')
      d3 = self->w_NCDF::get_Var_Info('PSFC')
      if (d1 and d2 and d3) then begin
        var = {name:'RH2',unit:'%',ndims:N_elements(dims),description:'2 m Relative Humidity',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;SWUP
      d1 = self->w_NCDF::get_Var_Info('SWDOWN', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('ALBEDO')      
      if (d1 and d2) then begin
        var = {name:'SWUP',unit:'w m-2',ndims:N_elements(dims),description:'upward short wave flux at ground surface',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;LWUP
      d1 = self->w_NCDF::get_Var_Info('TSK', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('EMISS')      
      if (d1 and d2) then begin
        var = {name:'LWUP',unit:'w m-2',ndims:N_elements(dims),description:'upward long wave flux at ground surface',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;LWDOWN
      d1 = self->w_NCDF::get_Var_Info('GLW', DIMNAMES=dnames,DIMS=dims)  
      if (d1) then begin
        var = {name:'LWDOWN',unit:'w m-2',ndims:N_elements(dims),description:'downward long wave flux at ground surface',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;NETRAD
      d1 = self->w_NCDF::get_Var_Info('SWDOWN', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('ALBEDO')        
      d3 = self->w_NCDF::get_Var_Info('TSK')        
      d4 = self->w_NCDF::get_Var_Info('EMISS')        
      d5 = self->w_NCDF::get_Var_Info('GLW')        
      d6 = self->w_NCDF::get_Var_Info('SWDOWN')        
      if (d1 and d2 and d3 and d4 and d5 and d6) then begin
        var = {name:'NETRAD',unit:'w m-2',ndims:N_elements(dims),description:'net radiation at ground surface (+ = downward)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;TER      
      d1 = self->w_NCDF::get_Var_Info('HGT', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'TER',unit:'m',ndims:2L,description:'Model Terrain Height',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      d1 = self->w_NCDF::get_Var_Info('HGT_M', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'TER',unit:'m',ndims:2L,description:'Model Terrain Height',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;lucat      
      d1 = self->w_NCDF::get_Var_Info('LU_INDEX', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'LUCAT',unit:'-',ndims:2L,description:'Model Landuse Category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;soiltop      
      d1 = self->w_NCDF::get_Var_Info('SCT_DOM', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILTOP',unit:'-',ndims:2L,description:'Model dominant soil category (top)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      d1 = self->w_NCDF::get_Var_Info('ISLTYP', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILTOP',unit:'-',ndims:2L,description:'Model dominant soil category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;soilbot      
      d1 = self->w_NCDF::get_Var_Info('SCB_DOM', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILBOT',unit:'-',ndims:2L,description:'Model dominant soil category (bottom)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      d1 = self->w_NCDF::get_Var_Info('ISLTYP', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILBOT',unit:'-',ndims:2L,description:'Model dominant soil category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;SLP
      d1 = self->w_NCDF::get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('P')
      d3 = self->w_NCDF::get_Var_Info('PB')
      d4 = self->w_NCDF::get_Var_Info('QVAPOR')      
      d5 = self->w_NCDF::get_Var_Info('PH')
      d6 = self->w_NCDF::get_Var_Info('PHB')
      if (d1 and d2 and d3 and d4 and d5 and d6) then begin
        var = {name:'SLP',unit:'hPa',ndims:N_elements(dims)-1,description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
      endif
      
      d1 = self->w_NCDF::get_Var_Info('PMSL', DIMNAMES=dnames,DIMS=dims) ;MET EM
      if d1 then begin
        var = {name:'SLP',unit:'hPa',ndims:N_elements(dims)-1,description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;SLP_B
      d1 = self->w_NCDF::get_Var_Info('PSFC', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('T2')
      if self.type eq 'MET' then d3 = self->w_NCDF::get_Var_Info('HGT_M') $
       else  d3 = self->w_NCDF::get_Var_Info('HGT')
      if (d1 and d2 and d3) then begin
        var = {name:'SLP_B',unit:'hPa',ndims:N_elements(dims),description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif      
      
      ;WS and WD
      d1 = self->w_NCDF::get_Var_Info('U10', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('V10') 
      if (d1 and d2) then begin
        var = {name:'WS10',unit:'m s-1',ndims:N_elements(dims),description:'10 m wind speed',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'WD10',unit:'degrees',ndims:N_elements(dims),description:'10 m wind direction',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
      endif      
           
      self.ndiagvar = N_ELEMENTS(dvars) - 1
      if self.ndiagvar ne 0 then begin      
        dvars = dvars[1:*]
        ; Check if they are already available in the orig ncdf (maybe an agg file)
        self->w_NCDF::get_Varlist, ncvarid, ncvarnames
        for i=0, N_ELEMENTS(dvars)-1 do begin
          dummy = where(str_equiv(ncvarnames) eq str_equiv((dvars[i]).name), nmatch)
          if nmatch ne 0L then begin
            if N_ELEMENTS(match) eq 0 then match = i else match = [match,i]
          endif
        endfor   
        if N_ELEMENTS(match) ne 0 then utils_array_remove, match, dvars     
      endif

      dvarnames = str_equiv(dvars.name)
      u = UNIQ(dvarnames, SORT(dvarnames))
      dvars = dvars[u]
      dvars.name = dvarnames[u]
      self.diagVars = PTR_NEW(dvars)    
      self.ndiagvar = N_ELEMENTS(dvars)  
    endif else begin
     dvars = *self.diagVars
    endelse
    
    undefine, varid, varnames, varndims, varunits, vardescriptions, vartypes

    varid = -1L * (LINDGEN(N_ELEMENTS(dvars))+1L)
    varnames = dvars.name
    varndims = dvars.ndims
    varunits = dvars.unit
    vardescriptions = dvars.description
    vartypes = dvars.type

    if KEYWORD_SET(PRINTVARS) then print, 'WAVE diagnostic variables:'
    if KEYWORD_SET(PRINTVARS) then for i = 0, self.ndiagvar-1 DO print, 'Id: ' + str_equiv(varid[i]) + $
       '. Name: ' + varnames[i] + '. Unit: ' + varunits[i] + '. Ndims: ' + str_equiv(varndims[i])+ $
         '. Type: ' + VARTYPES[i] + '. Description: ' + vardescriptions[i]
    
  endif else if KEYWORD_SET(ALL) then begin
  
    self->w_NCDF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS = printvars
    self->get_Varlist, dvarid, dvarnames, dvarndims, dvarunits, dvardescriptions, dvartypes, /DIAGNOSTIC, PRINTVARS = printvars
     
    if self.ndiagvar ne 0 then begin
      varid = [varid,dvarid]
      varnames = [varnames,dvarnames]
      varndims = [varndims,dvarndims]
      varunits = [varunits,dvarunits]
      vardescriptions = [vardescriptions,dvardescriptions]
      vartypes = [vartypes,dvartypes]
    endif
    
  endif else self->w_NCDF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS = printvars
  
  
end

;+
; :Description:
;    This function reads a variable from the file but only
;    at a specific location. The output is a vector of 
;    nt elements, where nt is the number of times in the 
;    time serie.
;
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :Params:
;    varid: in, required, type = string/integer
;           the variable ID (string or integer) to retrieve
;    x: in, required, type = long
;       the X coordinate of the point where to get the variable (if SRC is not specified, it is an index within the Grid)
;    y: in, required, type = long
;       the Y coordinate of the point where to get the variable (if SRC is not specified, it is an index within the Grid)
;    time:  out, type = qms
;           the variable times
;    nt: out, type = long
;        the variable number of times
;
; :Keywords:
;    t0: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the first time of the variable timeserie
;    t1: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the last time of the variable timeserie
;    src: in, optional
;         the coordinate system (w_Grid2D or {TNT_PROJ} or {TNT_DATUM}) in which x and y are defined
;    point_i: out, optional
;             the i index in the grid where the nearest point was found
;    point_j: out, optional
;             the j index in the grid where the nearest point was found
;    point_lon: out, optional
;              the longitude of the nearest grid point
;    point_lat: out, optional
;              the latitude of the nearest grid point
;    dist_x: out, optional
;            the easting difference between the (x,y) point and the nearest grid point 
;    dist_y: out, optional
;            the northing difference between the (x,y) point and the nearest grid point 
;    units: out, type = string
;           If available, the units of the variable
;    description: out, type = string
;                 If available, the description of the variable
;    varname: out, type = string
;             the name of the variable
;    dims: out, type = long
;          the variable dimensions
;    dimnames: out, type = string
;              the dimensions names
;              
; :History:
;     Written by FaM, 2010.
;-      
function w_WRF::get_TimeSerie,varid, x, y, $
                              time, nt, $
                              T0=t0, T1=t1, $
                              SRC=src, $
                              K=K, $
                              POINT_I=point_i, $
                              POINT_J=point_j, $
                              POINT_LON=point_lon, $
                              POINT_LAT=point_lat, $
                              DIST_X = dist_x, $
                              DIST_Y = dist_y, $
                              UNITS=units, $
                              DESCRIPTION=description, $
                              VARNAME=varname , $ ; 
                              DIMS=dims, $ ;
                              DIMNAMES=dimnames 
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  if N_PARAMS() lt 3 then Message, WAVE_Std_Message(/NARG)  
  if ~arg_okay(VarId, /SCALAR) then MEssage, WAVE_Std_Message('VarId', /SCALAR)
 
  ;Some check
  not_implemented = ['TK','TC','THETA','SLP','SLP_B','PRESSURE','GEOPOTENTIAL', 'Z', 'T2PBL', 'T2PBLC']
  pni = where(not_implemented eq str_equiv(Varid), cntni)
  if cntni gt 0 then Message, '$' + str_equiv(VarId) + ' is currently not available for w_WRF::get_TimeSerie.'
        
  ;Check if the variable is available
  if ~self->get_Var_Info(Varid, out_id = vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
    
  _acc_to_step = KEYWORD_SET(ACC_TO_STEP)
    
  ; GIS
  if N_ELEMENTS(src) EQ 0 then mysrc = self else mysrc = src  
  ; This is to obtain the indexes in the grid
  self->transform,  x, y, point_i, point_j, SRC = mysrc, /NEAREST, E_DST=_x, N_DST=_y  
  ; This is to obtain lat and lons of the selected grid point
  self->transform, point_i, point_j, dummy, dummy, src=self, $
    LON_DST=point_lon, LAT_DST=point_lat, E_DST=point_x, N_DST=point_y    
  dist_x = _x - point_x
  dist_y = _y - point_y   
    
  undefine, value
 
  ; Check for the known diagnostic variable names
  case str_equiv(vid) of
  
    'PRCP': begin
      value = self->w_GEO_nc::get_TimeSerie('RAINNC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
                          dims = dims, $ ;
                          dimnames = dimnames) + self->w_GEO_nc::get_TimeSerie('RAINNC', point_i, point_j, K = K, t0 = t0, t1 = t1)
     end
    
    'PRCP_STEP': begin
      d1 = self->w_NCDF::get_Var_Info('RAINNC_step')
      d2 = self->w_NCDF::get_Var_Info('RAINC_step')
      if d1 and d2 then begin
        value = self->w_GEO_nc::get_TimeSerie('RAINNC_step', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
          dims = dims, $ ;
          dimnames = dimnames) + self->w_GEO_nc::get_TimeSerie('RAINNC_step', point_i, point_j, K = K, t0 = t0, t1 = t1)
        _acc_to_step = FALSE
      endif else begin
        d1 = self->w_NCDF::get_Var_Info('RAINNC')
        d2 = self->w_NCDF::get_Var_Info('RAINC')
        if d1 and d2 then begin
          value = self->w_GEO_nc::get_TimeSerie('RAINNC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
            dims = dims, $ ;
            dimnames = dimnames) + self->w_GEO_nc::get_TimeSerie('RAINNC', point_i, point_j, K = K, t0 = t0, t1 = t1)
          _acc_to_step = TRUE
        endif else Message, 'Precipitation variables not available'
      endelse
    end
    
    
    'T2C': begin
      value = self->w_GEO_nc::get_TimeSerie('T2', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
                          dims = dims, $ ;
                          dimnames = dimnames) - 273.15
     end
    
    'RH': begin
      T = self->w_GEO_nc::get_TimeSerie('T', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)        
      P = self->w_GEO_nc::get_TimeSerie('P', point_i, point_j, K = K, t0 = t0, t1 = t1)
      PB = self->w_GEO_nc::get_TimeSerie('PB', point_i, point_j, K = K, t0 = t0, t1 = t1)
      QVAPOR = self->w_GEO_nc::get_TimeSerie('QVAPOR', point_i, point_j, K = K, t0 = t0, t1 = t1) > 0.
      T = T + 300.
      P  = P + PB
      tk = utils_wrf_tk(P,T)
      value = utils_wrf_rh(QVAPOR, P, tk)
    end
    
    'RH2': begin
      T2 = self->w_GEO_nc::get_TimeSerie('T2', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)        
      PSFC = self->w_GEO_nc::get_TimeSerie('PSFC', point_i, point_j, K = K, t0 = t0, t1 = t1)
      Q2 = self->w_GEO_nc::get_TimeSerie('Q2', point_i, point_j, K = K, t0 = t0, t1 = t1) > 0.
      value = utils_wrf_rh(Q2, PSFC, T2)
    end

    'TER': begin
      if self->w_NCDF::get_Var_Info('HGT') then _id = 'HGT' $
       else _id = 'HGT_M'
      value = self->w_GEO_nc::get_TimeSerie(_id, point_i, point_j, time, nt, t0 = self.t0, t1 = self.t0, $
        dims = dims, $ ;
        dimnames = dimnames)
    end
    
    'LUCAT': begin
      value = self->w_GEO_nc::get_TimeSerie('LU_INDEX', point_i, point_j, time, nt, t0 = self.t0, t1 = self.t0, $
        dims = dims, $ ;
        dimnames = dimnames)
    end
        
    'WS10': begin
      u10 = self->w_GEO_nc::get_TimeSerie('U10', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      v10 = self->w_GEO_nc::get_TimeSerie('V10', point_i, point_j, K = K, t0 = t0, t1 = t1)      
      MET_u_v_to_ws_wd, ret, u10, v10, WS = value
    end

    'WD10': begin
      u10 = self->w_GEO_nc::get_TimeSerie('U10', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      v10 = self->w_GEO_nc::get_TimeSerie('V10', point_i, point_j, K = K, t0 = t0, t1 = t1)
      MET_u_v_to_ws_wd, ret, u10, v10, WD=value
    end
    
    else:
    
  endcase
    
  if N_ELEMENTS(value) eq 0 then begin ;This is probably a standard variable
       
    value = self->w_GEO_nc::get_TimeSerie(varid, point_i, point_j, time, nt, t0 = t0, t1 = t1, $
                                                           K = K , $
                                                           units = units, $
                                                           description = description, $
                                                           varname = varname , $ ;
                                                           dims = dims, $ ;
                                                           dimnames = dimnames )
  endif
       
  if _ACC_TO_STEP then begin
   value = utils_ACC_TO_STEP(value)
   description += ' (de-accumulated)' 
  endif 
  
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
;    x: in, required, type = long
;       the X index where to get the variable (if SRC is not specified, it is an index within the Grid)
;       
;    y: in, required, type = long
;       the Y index where to get the variable (if SRC is not specified, it is an index within the Grid)
;
; :Keywords:
;    t0: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the first time of the variable timeserie
;    t1: in, optional, type = qms/{ABS_DATE}
;        if set, it defines the last time of the variable timeserie
;    src: in, optional
;         the coordinate system (w_Grid2D or {TNT_PROJ} or {TNT_DATUM}) in which x and y are defined
;    K: in, optional
;       if 3D variable, the index in Z dimension where to get the TS
;
; :History:
;     Written by FaM, 2010.
;-      
pro w_WRF::plot_TimeSerie, varid, x, y, $
                           t0 = t0, t1 = t1, $
                           src = src, K=k

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
  
  if ~self->get_Var_Info(Varid) then MESSAGE, 'Variable not found'  
  
  var = self->get_TimeSerie( Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                         x, $
                         y, $
                         times, nt, $
                         src = src, $
                         K = K, $
                         t0 = t0, $
                         t1 = t1, $
                         point_i = wrf_ind_i, $
                         point_j = wrf_ind_j, $
                         point_lon = wrf_lon, $
                         point_lat = wrf_lat, $
                         units = units, $
                         description = description, $
                         varname = varname , $ ; 
                         dims = dims, $ ;
                         dimnames = dimnames )
                           
  ;TODO: Update routine: if var dim 2 then more than one curve 
  
  w_TimeLinePlot, var, times, varname, COLOR1='red', TITLE='WRF TS plot: ' + description, YTITLE=units, THICKNESS=2
  
  cgtext, 0.7915, 0.26, 'Grid point: [' + str_equiv(STRING(wrf_ind_i, FORMAT = '(I3)')) + ',' + str_equiv(STRING(wrf_ind_j, FORMAT = '(I3)')) + ']', $
          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW
  
  cgtext, 0.7915 + 0.01, 0.2, 'WRF lon: ' + str_equiv(STRING(wrf_lon, FORMAT='(F7.2)')), $
          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW  
  cgtext, 0.7915 + 0.01, 0.15, 'WRF lat: ' + str_equiv(STRING(wrf_lat, FORMAT='(F7.2)')), $
          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW  
    
end


;+
; :Description:
;    This function reads a variable from the netcdf file and makes a
;    subset of it if it has been previously set with 'define_subset'.
;    
;    There is the possibility to restrict the retrieved variable to 
;    a given time period (keywords `T0` and `T1`)
;    
;    Additionaly to the "standard" variables available in the WRF file,
;    a few diagnostic variables are computed automatically.
;    Here is a list of the diagnostic variables (only if needed WRF 
;    variables are present. Check the available variables with
;    `w_WRF::get_Varlist, /DIAGNOSTIC, /PRINT`) ::
;              
;             prcp: total precipitation (step-wize) [mm]
;             snowfall: Grid scale snow and ice (step-wize) [mm]
;             prcp_c: Cumulus precipitation (step-wize) [mm]
;             prcp_nc: Grid scale precipitation (step-wize) [mm]
;             prcp_fr: Frozen precipitation (step-wize) [mm]
;             graupel: Grid scale graupel (step-wize) [mm]
;             hail: Grid scale hail (step-wize) (step-wize) [mm]             
;             potevap:  Potential evaporation (step-wize) [w m-2]             
;             rh: Relative Humidity [%]
;             rh2: 2m Relative Humidity [%]
;             slp: Sea level pressure [hPa] (computed with full vertical levels - slow. See `utils_wrf_slp` 
;                  (If the vertical dimension is not present in the file, slp_b is computed automatically instead)
;             slp_b: Sea level pressure [hPa] (computed with surface values - fast. see `MET_barometric` for more info)
;             ter: Model terrain height [m] (static: no time dimension)
;             lucat: Model landuse category [] (static: no time dimension)
;             soiltop: Model soil category top [] (static: no time dimension)
;             soilbot: Model soil category bot [] (static: no time dimension)
;             tc: Temperature [C]
;             t2c: 2m Temperature [C]
;             t2pbl: 2 m temperature (extrapolated from eta-levels) [K]
;             t2pblc: 2 m temperature (extrapolated from eta-levels) [C]
;             theta: Potential temperature [K]
;             tk: Temperature [K]
;             ws10: wind speed at 10m [m.s-1] TODO: rotated to earth coordinates
;             wd10: wind direction [degrees] TODO: rotated to earth coordinates
;             geopotential: Full model geopotential [m2 s-2] (unstaggered)
;             pressure: Full model pressure [hPa]
;             z: Full model height (geopotential / 9.81) [m]
;             
;             TODO: umet10, vmet10, umet, vmet, components of wind rotated to earth coordinates
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
;   unstagger: in, optional
;              if set, the variable will be automatically unstaggered
;   eta_levels: in, optional, type = float
;            set this keyword to an array of one or two elements, containing the range
;            of the indexes to keep from the original NCDF file in the Z dimension (eta-levels).
;   zlevels: in, optional, type = float
;            same as eta-levels
;   pressure_levels: in, optional, type = float
;                    set this keyword to an array of pressure levels (hPa) to interpolate to.
;                    the output array will then have the dimensions [nx,ny,nl,nt], where nl is the 
;                    number of elements in pressure_levels
;   height_levels: in, optional, type = float
;                  set this keyword to an array of height levels (m) to interpolate to.
;                  the output array will then have the dimensions [nx,ny,nl,nt], where nl is the 
;                  number of elements in height_levels
;   above_ground_levels: in, optional, type = float
;                        set this keyword to an array of height levels (m) ABOVE model height to interpolate to.
;                        the output array will then have the dimensions [nx,ny,nl,nt], where nl is the 
;                        number of elements in height_levels
;   acc_to_step: in, optional
;                if set, the variable is returned "step-wize" (as a difference to previous step) and not accumulated
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
function w_WRF::get_Var, Varid, $ 
                            time,  $
                            nt,  $
                            T0=t0, $
                            T1=t1, $
                            UNSTAGGER=unstagger, $
                            ETA_LEVELS=eta_levels, $
                            ZLEVELS=zlevels, $
                            PRESSURE_LEVELS=pressure_levels, $
                            HEIGHT_LEVELS=height_levels, $
                            ABOVE_GROUND_LEVELS=above_ground_levels, $
                            ACC_TO_STEP=acc_to_step , $
                            UNITS=units, $
                            DESCRIPTION=description, $
                            VARNAME=varname , $ 
                            DIMS=dims, $ 
                            DIMNAMES=dimnames 

                        
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  undefine, count, offset
  
  if self.type eq 'PRO' and N_ELEMENTS(Varid) eq 0 then varid = self.Nvars-1
  
  ;Some check
  if str_equiv(Varid) eq 'SLP' and ~self->get_Var_Info('SLP') then varid = 'SLP_B'
     
  ; Z level handling
  if N_ELEMENTS(ETA_LEVELS) ne 0 then ZLEVELS=eta_levels
  _do_eta = N_ELEMENTS(ZLEVELS) ne 0
  _do_pres = N_ELEMENTS(PRESSURE_LEVELS) ne 0
  _do_h = N_ELEMENTS(HEIGHT_LEVELS) ne 0
  _do_ag = N_ELEMENTS(ABOVE_GROUND_LEVELS) ne 0
  if total([_do_eta,_do_pres,_do_h,_do_ag]) gt 1 then Message, 'Some keywords are incompatible (Z-dimension).'
  
  
  ;Check if the variable is available
  if ~self->get_Var_Info(Varid, out_id = vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
    
  _acc_to_step = KEYWORD_SET(ACC_TO_STEP)
  _unstagger = KEYWORD_SET(UNSTAGGER)
  
  ; Check for the known diagnostic variable names
  case str_equiv(vid) of
      
    'PRCP': begin
      value = self->get_Var('RAINNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) + self->get_Var('RAINC', t0 = t0, t1 = t1)
      _acc_to_step = TRUE
    end
    
    'SNOWFALL': begin
      value = self->get_Var('SNOWNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      _acc_to_step = TRUE
    end
    
    'PRCP_FR': begin
      value = self->get_Var('PRCP', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      sr = self->get_Var('SR', t0 = t0, t1 = t1)
      ptm = where(sr gt (MACHAR()).EPS and value gt (MACHAR()).EPS, cntp)
      if cntp ne 0 then value[ptm] = sr[ptm] * value[ptm] 
      _acc_to_step = FALSE
    end
    
    'PRCP_NC': begin
      value = self->get_Var('RAINNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      _acc_to_step = TRUE      
    end
    
    'PRCP_C': begin
      value = self->get_Var('RAINC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      _acc_to_step = TRUE
    end
    
    'GRAUPEL': begin
      value = self->get_Var('GRAUPELNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      _acc_to_step = TRUE
    end
    
    'HAIL': begin
      value = self->get_Var('HAILNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      _acc_to_step = TRUE
    end
    
    'POTEVAP': begin
      value = self->get_Var('POTEVP', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      _acc_to_step = TRUE
    end
    
    'LWDOWN': begin
      value = self->get_Var('GLW', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
    end
    
    'LWUP': begin
      value = self->get_Var('TSK', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      e = self->get_Var('EMISS', time, nt, t0 = t0, t1 = t1)
      value = (5.6704e-8) * e * value^4
    end
    
    'SWUP': begin
      value = self->get_Var('SWDOWN', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      value *= self->get_Var('ALBEDO', time, nt, t0 = t0, t1 = t1)
    end
    
    'NETRAD': begin
      value = self->get_Var('SWDOWN', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      swup = self->get_Var('SWUP', time, nt, t0 = t0, t1 = t1)
      lwup = self->get_Var('LWUP', time, nt, t0 = t0, t1 = t1)
      lwdown = self->get_Var('LWDOWN', time, nt, t0 = t0, t1 = t1)
      value += lwdown - swup - lwup
    end
    
    'TK': begin
      if self->get_Var_Info('T') then begin
        T = self->get_Var('T', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        P = self->get_Var('P', T0=t0, T1=t1, ZLEVELS=zlevels)
        PB = self->get_Var('PB', T0=t0, T1=t1, ZLEVELS=zlevels)
        T = T + 300.
        P = P + PB
        value = utils_wrf_tk(P,T)    ; calculate TK
      endif else begin
        value = self->get_Var('TT', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, ZLEVELS=zlevels, $
          dimnames = dimnames)
      endelse
    end
    
    'TC': begin
      value = self->get_Var('TK', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
        dims = dims, $
        dimnames = dimnames) - 273.15
    end
        
    'THETA': begin
      value = self->get_Var('T', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
        dims = dims, $
        dimnames = dimnames) + 300.
    end
    
    'T2C': begin
      value = self->get_Var('T2', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) - 273.15
    end
    
    'T2PBL': begin
      value = self->get_Var('TK', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, ABOVE_GROUND_LEVELS=2., $
        dimnames = dimnames)      
     end
    
    'T2PBLC': begin
      value = self->get_Var('T2PBL', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) - 273.15      
     end
    
    'RH': begin
      T = self->get_Var('T', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
        dims = dims, $
        dimnames = dimnames)
      P = self->get_Var('P', T0=t0, T1=t1, ZLEVELS=zlevels)
      PB = self->get_Var('PB', T0=t0, T1=t1, ZLEVELS=zlevels)
      QVAPOR = self->get_Var('QVAPOR', T0=t0, T1=t1, ZLEVELS=zlevels) > 0.
      T = T + 300.
      P  = P + PB
      tk = utils_wrf_tk(P,T)
      value = utils_wrf_rh(QVAPOR, P, tk)
    end
    
    'RH2': begin
      T2 = self->get_Var('T2', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
        dims = dims, $
        dimnames = dimnames)
      PSFC = self->get_Var('PSFC', T0=t0, T1=t1, ZLEVELS=zlevels)
      Q2 = self->get_Var('Q2', T0=t0, T1=t1, ZLEVELS=zlevels) > 0.
      value = utils_wrf_rh(Q2, PSFC, T2)
    end
    
    'TER': begin
      if self->w_NCDF::get_Var_Info('HGT') then _id = 'HGT' $
       else _id = 'HGT_M'
      value = self->get_Var(_id, time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, ZLEVELS=zlevels, $
        dimnames = dimnames)
    end
    
    'SOILTOP': begin
      if self->w_NCDF::get_Var_Info('SCT_DOM') then _id = 'SCT_DOM' $
       else _id = 'ISLTYP'
      value = self->get_Var(_id, time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, ZLEVELS=zlevels, $
        dimnames = dimnames)
    end
    
    'SOILBOT': begin
      if self->w_NCDF::get_Var_Info('SCB_DOM') then _id = 'SCB_DOM' $
       else _id = 'ISLTYP'
      value = self->get_Var(_id, time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, ZLEVELS=zlevels, $
        dimnames = dimnames)
    end
    
    'LUCAT': begin
      value = self->get_Var('LU_INDEX', time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, $
        dimnames = dimnames)
    end
    
    'SLP': begin
      if self->get_Var_Info('T') then begin
        T = self->get_Var('T', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, $
          dimnames = dimnames)
        P = self->get_Var('P', T0=t0, T1=t1)
        PB = self->get_Var('PB', T0=t0, T1=t1)
        QVAPOR = self->get_Var('QVAPOR', T0=t0, T1=t1) > 0.
        PH = self->get_Var('PH', T0=t0, T1=t1, /UNSTAGGER)
        PHB = self->get_Var('PHB', T0=t0, T1=t1, /UNSTAGGER)
        T = T + 300.
        P = P + PB
        z = ( PH + PHB ) / 9.81
        tk = utils_wrf_tk(P,T)    ; calculate TK
        mdims = SIZE(tk, /DIMENSIONS)
        value = FLTARR(mdims[0],mdims[1],nt)
        for t=0, nt-1 do value[*,*,t] = utils_wrf_slp(z[*,*,*,t], tk[*,*,*,t], P[*,*,*,t], QVAPOR[*,*,*,t])  ; calculate slp
        if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
      endif else begin
        value = self->get_Var('PMSL', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, $
          dimnames = dimnames) * 0.01 ; in hPa
      endelse
    end
    
    'SLP_B': begin
      ps = self->get_Var('PSFC', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames) * 0.01 ; in hPa
      T2 = self->get_Var('T2') - 273.15 ; in degC
      zs = self->get_Var('TER') ; in m
      mdims = SIZE(t2, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for k=0,Nt-1 do value[*,*,k] = MET_barometric(ps[*,*,k], zs, T2[*,*,k], 0.)
      if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
    end
    
    'WS10': begin
      u10 = self->get_Var('U10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10')      
      MET_u_v_to_ws_wd, ret, u10, v10, WS=value
    end

    'WD10': begin
      u10 = self->get_Var('U10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10')      
      MET_u_v_to_ws_wd, ret, u10, v10, WD=value
    end
    
    'PRESSURE': begin
      if self->get_Var_Info('P') then begin
        value = self->get_Var('P', time, nt, T0=t0, T1=t1,  $
          dims = dims, ZLEVELS=zlevels, $
          dimnames = dimnames)
        value += self->get_Var('PB', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels)
      endif else begin
        value = self->get_Var('PRES', time, nt, T0=t0, T1=t1,  $
          dims = dims, ZLEVELS=zlevels, $
          dimnames = dimnames)
      endelse
      value *= 0.01
    end
    
    'GEOPOTENTIAL': begin
      if self->get_Var_Info('PH') then begin
        value = self->get_Var('PH', time, nt, T0=t0, T1=t1,  $
          dims = dims, $
          dimnames = dimnames, ZLEVELS=zlevels)
        value += self->get_Var('PHB', time, nt, T0=t0, T1=t1, ZLEVELS=zlevels)
        _unstagger = TRUE
      endif else begin
        value = self->get_Var('GHT', time, nt, T0=t0, T1=t1,  $
          dims = dims, $
          dimnames = dimnames, ZLEVELS=zlevels) * 9.81
      endelse
    end
    
    'Z': begin
        value = self->get_Var('GEOPOTENTIAL', time, nt, T0=t0, T1=t1,  $
          dims = dims, $
          dimnames = dimnames, ZLEVELS=zlevels) / 9.81
    end
    
    else:
    
  endcase
    
  if N_ELEMENTS(value) eq 0 then begin ;This is probably a standard variable
       
    value = self->w_GEO_nc::get_Var(vid, time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
      units = units, $
      description = description, $
      varname = varname , $
      dims = dims, $
      dimnames = dimnames)
      
  endif
  
  dims = SIZE(value, /DIMENSIONS)
  ndims = N_ELEMENTS(dims)
  
  if _unstagger then begin  
    found = -1
    for i=0, ndims-1 do begin
      isHere = STRPOS(str_equiv(dimnames[i]), str_equiv('_stag'))
      p = WHERE(isHere ne -1, cnt)
      if cnt eq 0 then continue
      dimnames[i] = STRMID(dimnames[i], 0, isHere)
      found = i
    endfor
    if found eq -1 then Message, 'Staggered dimension not found. You sure you want to unstagger?' 
    value = utils_wrf_unstagger(value, found)     
    dimnames[found] = utils_replace_string(dimnames[found], '_stag', '')
  endif
  
  if _acc_to_step then begin
   if KEYWORD_SET(t0) or KEYWORD_SET(t1) then $
    message, 'Warning. You are using /ACC_TO_STEP with user set T0 -> T1 intervals, hope you know what you are doing', /INFORMATIONAL
    if nt eq 1 then Message, 'You asked to de-accumulate only one time-step. Not possible.'
   value = utils_ACC_TO_STEP(value)
   description += ' (de-accumulated)' 
  endif 
 
  if _do_pres or _do_h or _do_ag then begin
    ptime = where(strmatch(str_equiv(dimnames), 'TIME'), cnttime)
    pstag = where(strmatch(str_equiv(dimnames), '*_STAG'), cntstag)

    pdimtochange = where(strmatch(str_equiv(dimnames), 'BOTTOM_TOP'), cntdimtochange)
    if cntdimtochange eq 0 then $ ; Try the metgrid case
      pdimtochange = where(strmatch(str_equiv(dimnames), 'NUM_METGRID_LEVELS'), cntdimtochange)      
    if cntstag ne 0 then Message, 'It does not seem I can perform the vertical levels interpolation on the selected variable (unstagger first).'
    if (ptime eq 2) or (ndims lt 3) or (cntdimtochange ne 1)then Message, 'It does not seem I can perform the vertical levels interpolation on the selected variable.'
    
    if _do_pres then begin
      p = self->get_Var('pressure', T0=t0, T1=t1)
      value = reform(utils_wrf_intrp3d(value, p, pressure_levels))
      nlocs = N_ELEMENTS(pressure_levels)
      if nlocs eq 1 then utils_array_remove, pdimtochange, dimnames $
      else dimnames[pdimtochange] = 'pressure_levels'
    endif
    
    if _do_h then begin
      h = self->get_Var('z', T0=t0, T1=t1)
      value = reform(utils_wrf_intrp3d(value, h, height_levels))
      nlocs = N_ELEMENTS(height_levels)
      if nlocs eq 1 then utils_array_remove, pdimtochange, dimnames $
      else dimnames[pdimtochange] = 'height_levels'
    endif
    
    if _do_ag then begin
      _dims = dims & _dims[2:*] = 1
      ter =  rebin(reform(self->get_Var('ter'),_dims), dims) ; make it same dim as z
      h = self->get_Var('z', T0=t0, T1=t1) - TEMPORARY(ter)
      value = reform(utils_wrf_intrp3d(value, h, above_ground_levels, /EXTRAPOLATE))
      nlocs = N_ELEMENTS(above_ground_levels)
      if nlocs eq 1 then utils_array_remove, pdimtochange, dimnames $
      else dimnames[pdimtochange] = 'height_above_ground'
    endif
     
  endif
  
  dims = SIZE(value, /DIMENSIONS)
  if N_ELEMENTS(dims) ne N_ELEMENTS(DIMNAMES) then begin
   MESSAGE, 'Internal Warning: contact FaM.', /INFORMATIONAL
  endif
  return, value
  
end
