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
;            LL and UR corners of the desired subset ([XLL,YLL,XUR,YUR]), coordinate system defined by `SRC`
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
;                       * 2 = Boundary + Interior. All pixels falling on or within a region's boundary are set.
;                       * 3 = Pixel center point is used to test the appartenance to the ROI. This is the default!
;    MARGIN: in
;            set to a positive integer value to add a margin to the subset
;            (MARGIN=1 will put one grid point on each side of the subset, so two
;             more columns per dimension in total)
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
                                MARGIN=margin, $
                                ROI_MASK_RULE=roi_mask_rule

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, self->w_WRF::define_subset()
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
    x = self->w_NCDF::get_Var('west_east')
    y = self->w_NCDF::get_Var('south_north')         
    nx = N_ELEMENTS(x)        
    ny = N_ELEMENTS(y)              
    dx = x[1]-x[0] ;TODO CHANGE this!!
    dy = y[1]-y[0] ;TODO CHANGE this!!
    x0 = min(x)             
    y0 = max(y) 
  endif else begin
    center_lat = self->w_NCDF::get_Gatt('CEN_LAT')
    center_lon = self->w_NCDF::get_Gatt('CEN_LON')
    ; Get easting and northings from dom center
    GIS_coord_trafo, ret, center_lon, center_lat, e, n, SRC=self.tnt_c.proj.datum, DST= self.tnt_c.proj
    nx = self->w_NCDF::get_Gatt('WEST-EAST_GRID_DIMENSION')-1
    ny = self->w_NCDF::get_Gatt('SOUTH-NORTH_GRID_DIMENSION')-1
    dx = self->w_NCDF::get_Gatt('DX')
    dy = self->w_NCDF::get_Gatt('DY')
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
      
    ;Temporary tests to be sure we are good at WRF georeference
    self->w_GRID2d::Get_LonLat, gislon, gislat
    self->w_GEO_nc::get_ncdf_coordinates, lon, lat
    
    ;If we are at the poles we should mask the pole, errors are frequent there
    pp = where(lat gt 86., cntp)
    if cntp ne 0 then begin
     lon[pp]=0 & lat[pp]=0 & gislon[pp]=0 & gislat[pp]=0
     inds = ARRAY_INDICES(lon, pp)
     lon[*,inds[1,*]]=0 & gislon[*,inds[1,*]]=0
    endif
    ;If there is a nest, WRF jhas the bad habit to change its georeference
    if self->get_var_info('NEST_POS') then begin
      np = (self->get_Var('NEST_POS'))[*,*,0]
      pnest = where(np ne 0, cntp)
      if cntp ne 0 then begin
       lon[pnest]=0 & lat[pnest]=0 & gislon[pnest]=0 & gislat[pnest]=0
      endif
    endif
    if (max(abs(gislon-lon)) gt 1e-4) or (max(abs(gislat-lat)) gt 1e-4) then begin
      w_QuickPlot, abs(gislon-lon)
      Message, 'My lons different from the file lons? Diff: ' + str_equiv(max(abs(gislon-lon))), /INFORMATIONAL
      Message, 'My lats different from the file lats? Diff: ' + str_equiv(max(abs(gislat-lat))), /INFORMATIONAL
    endif
  endif else begin
    IF NOT self->w_Grid2D::ReInit(  nx = nx                , $
                                    ny = ny                , $
                                    dx = dx                , $
                                    dy = dy                , $
                                    x0 = x0                , $
                                    y0 = y0                , $
                                    proj = self.tnt_c.proj) THEN RETURN, 0
  endelse
  
  
  if not self->w_Grid2D::set_ROI(SHAPE=shape,  $
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
  if self->w_Grid2D::is_ROI() then begin
    self->w_Grid2D::get_ROI, SUBSET=subset, MARGIN=margin
    if ~self->w_GEO_nc::define_subset(SUBSET=SUBSET) then return, 0 
    new_grid = self->w_Grid2D::reGrid(/TO_ROI, MARGIN=margin)
    IF NOT self->w_Grid2D::ReInit(grid=new_grid) THEN RETURN, 0  
    dummy = self->w_Grid2D::set_ROI()    
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
;       _EXTRA: in, optional
;                   all keywords accepted by w_WRF::define_subset
;
; :Returns:
; 
;    1 if the object is created successfully. 
;    
; :History:
;     Written by FaM, 2010.
;-
Function w_WRF::Init, FILE=file, _EXTRA=extra  
           
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
  title = self->w_NCDF::get_Gatt('TITLE')
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
  isHere = self->w_NCDF::get_Gatt_Info('PROJ_ENVI_STRING')
  if isHere eq 1 then ftype = 'PRO'  
  if ftype eq '' then message, 'Input file not recognized as a known WRF product.'
  self.type = ftype
  meta = str_equiv(title)
  
  ; Projection
  if self.type eq 'PRO' then begin  
    ; Make the projection
    GIS_make_proj, ret, proj, PARAM=STRING(self->get_Gatt('PROJ_ENVI_STRING'))
    self.tnt_c.proj = proj      
  endif else begin
    center_lat = self->w_NCDF::get_Gatt('CEN_LAT')
    center_lon = self->w_NCDF::get_Gatt('CEN_LON')
    moad_cen_lat = self->w_NCDF::get_Gatt('MOAD_CEN_LAT')
    stand_lon = self->w_NCDF::get_Gatt('STAND_LON')
    truelat1 = self->w_NCDF::get_Gatt('TRUELAT1')
    truelat2 = self->w_NCDF::get_Gatt('TRUELAT2')
    proj_id = self->w_NCDF::get_Gatt('MAP_PROJ')
    GIS_make_ellipsoid, ret, ell, NAME='WRF Sphere', RA=6370000.0, RB=6370000.0
    case proj_id of
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
      end
      3: begin
        ; 20- Mercator
        ;   a, b, lat0, lon0, x0, y0, [datum], name
        envi_proj = 20
        proj_param = str_equiv(envi_proj) + ', ' + $            ;proj_id
          STRING(ell.a, FORMAT='(F16.8)') + ', ' + $            ;a
          STRING(ell.b, FORMAT='(F16.8)') + ', ' + $            ;b
          STRING(truelat1, FORMAT='(F16.8)') + ', ' + $         ;lat0
          STRING(center_lon, FORMAT='(F16.8)') + ', ' + $          ;lon0
          '0.0' + ', ' + $                                      ;x0
          '0.0' + ', ' + $                                      ;y0
          'WGS-84' + ', ' + $                                   ;datum
          'WRF Mercator'
      end
      else: Message, 'Projection currently not supported.'
      
    endcase
    
    ; Make the projection
    GIS_make_proj, ret, proj, PARAM=proj_param
    self.tnt_c.proj = proj
    
  endelse
    
  
  ;***********************
  ; Diagnostic variables *
  ;***********************    
  self->w_WRF::get_Varlist, /DIAGNOSTIC
    
  ;*********
  ; define *
  ;*********
  self.cropped = ''
  if NOT self->w_WRF::define_subset(_EXTRA=extra) THEN RETURN, 0
    
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
  
  if ~ PTR_VALID(self.diagVars) then return
  
  for i=0, N_ELEMENTS(*self.diagVars) - 1 do begin 
   tvar = (*self.diagVars)[i]
   PTR_FREE, tvar.dims
   PTR_FREE, tvar.dimnames   
  endfor
  PTR_FREE, self.diagVars
  
end

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
  
  if self.type eq 'PRO' and N_ELEMENTS(Varid) eq 0 then varid = self.Nvars-1
  
  if ~ self->w_NCDF::get_Var_Info(Varid, $
                                 out_id = out_id, $
                                 units = units, $
                                 description = description, $
                                 varname = varname , $ ; 
                                 dims = dims, $ ;
                                 dimnames = dimnames) then begin
     
     ;Post processed variables
     if ~ PTR_VALID(self.diagVars) then return, FALSE 
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
      d1 = self->get_Var_Info('RAINNC', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('RAINC')
      d3 = self->get_Var_Info('SR')
      if (d1 and d2) then begin
        var = {name:'PRCP',unit:'mm h-1',ndims:N_elements(dims),description:'Total precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'PRCP_NC',unit:'mm h-1',ndims:N_elements(dims),description:'Grid scale precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'PRCP_C',unit:'mm h-1',ndims:N_elements(dims),description:'Cumulus precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        if d3 then begin
          var = {name:'PRCP_FR',unit:'mm h-1',ndims:N_elements(dims),description:'Frozen precipitation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
        endif
      endif
            
      ;snowfall
      d1 = self->get_Var_Info('SNOWNC', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SNOWFALL',unit:'mm h-1',ndims:N_elements(dims),description:'Grid scale snow and ice (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      ;graupel
      d1 = self->get_Var_Info('GRAUPELNC', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'GRAUPEL',unit:'mm h-1',ndims:N_elements(dims),description:'Grid scale graupel (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      ;hail
      d1 = self->get_Var_Info('HAILNC', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'HAIL',unit:'mm h-1',ndims:N_elements(dims),description:'Grid scale hail (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;POTEVP
      d1 = self->get_Var_Info('POTEVP', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'POTEVAP',unit:'w m-2',ndims:N_elements(dims),description:'Potential evaporation (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;Mixing ratios
      ; qvapor: Liquid water mixing ratio [kg.kg-1]
      d1 = self->get_Var_Info('QVAPOR', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'COL_QVAPOR',unit:'kg kg-1',ndims:N_elements(dims),description:'Total column water vapor mixing ratio',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]        
      endif
      
      ; qliquid: Liquid water mixing ratio [kg.kg-1]
      d1 = self->get_Var_Info('QRAIN', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('QCLOUD', DIMNAMES=dnames,DIMS=dims)
      if (d1 or d2) then begin
        var = {name:'QLIQUID',unit:'kg kg-1',ndims:N_elements(dims),description:'Liquid water mixing ratio',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'COL_QLIQUID',unit:'kg kg-1',ndims:N_elements(dims),description:'Total column liquid water mixing ratio',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]        
      endif
      
      ; qsolid: Solid water mixing ratio [kg.kg-1]
      d1 = self->get_Var_Info('QSNOW', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('QGRAUP', DIMNAMES=dnames,DIMS=dims)
      d3 = self->get_Var_Info('QICE', DIMNAMES=dnames,DIMS=dims)
      if (d1 or d2 or d3) then begin
        var = {name:'QSOLID',unit:'kg kg-1',ndims:N_elements(dims),description:'Solid water mixing ratio',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'COL_QSOLID',unit:'kg kg-1',ndims:N_elements(dims),description:'Total column solid water mixing ratio',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
      endif
      
      ;pressure
      d1 = self->get_Var_Info('P', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('PB') 
      if (d1 and d2) then begin
        var = {name:'PRESSURE',unit:'hPa',ndims:N_elements(dims),description:'Full model pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]         
      endif           
      d1 = self->get_Var_Info('PRES', DIMNAMES=dnames,DIMS=dims) ;met_em
      if (d1) then begin
        var = {name:'PRESSURE',unit:'hPa',ndims:N_elements(dims),description:'Full model pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]         
      endif 
      
      ; geopotential, z
      d1 = self->get_Var_Info('PH', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('PHB')  
      if (d1 and d2) then begin
        dnames = utils_replace_string(dnames, '_stag', '')
        var = {name:'GEOPOTENTIAL',unit:'m2 s-2',ndims:N_elements(dims),description:'Full model geopotential on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'Z',unit:'m',ndims:N_elements(dims),description:'Full model height on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        d1 = self->get_Var_Info('HGT')
        if (d1) then begin
          var = {name:'ZAG',unit:'m',ndims:N_elements(dims),description:'Full model height above ground on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
        endif
      endif     
      d1 = self->get_Var_Info('GHT', DIMNAMES=dnames,DIMS=dims) ;met_em
      if (d1) then begin
        var = {name:'GEOPOTENTIAL',unit:'m2 s-2',ndims:N_elements(dims),description:'Full model geopotential on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'Z',unit:'m',ndims:N_elements(dims),description:'Full model height on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        d1 = self->get_Var_Info('HGT_M', DIMNAMES=dnames,DIMS=dims)
        if (d1) then begin
          var = {name:'ZAG',unit:'m',ndims:N_elements(dims),description:'Full model height above ground on mass points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
        endif
      endif    
        
      ;TK and TC
      d1 = self->get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)      
      d2 = self->get_Var_Info('P')
      d3 = self->get_Var_Info('PB')   
      if (d1 and d2 and d3) then begin
        var = {name:'TK',unit:'K',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'TC',unit:'C',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'T2ETA',unit:'K',ndims:N_elements(dims)-1,description:'2 m temperature (extrapolated from the first two eta-levels)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
        var = {name:'T2ETAC',unit:'C',ndims:N_elements(dims)-1,description:'2 m temperature (extrapolated from the first two eta-levels)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
        d4 = self->get_Var_Info('PBLH')
        if (d4) then begin
          var = {name:'T2PBL',unit:'K',ndims:N_elements(dims)-1,description:'2 m temperature (linear fit from pbl eta-levels)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        var = {name:'T2PBLC',unit:'C',ndims:N_elements(dims)-1,description:'2 m temperature (linear fit from pbl eta-levels)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
      endif
      
      ;TD
      d1 = self->get_Var_Info('QVAPOR', DIMNAMES=dnames,DIMS=dims)      
      d2 = self->get_Var_Info('P')
      d3 = self->get_Var_Info('PB')   
      if (d1 and d2 and d3) then begin
        var = {name:'TD',unit:'C',ndims:N_elements(dims),description:'Dewpoint Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var] 
      endif 
      
      ;ET
      d1 = self->get_Var_Info('ACLHF', DIMNAMES=dnames,DIMS=dims)      
      if (d1) then begin
        var = {name:'ET',unit:'mm h-1',ndims:N_elements(dims),description:'Evapotranspiration (step-wize)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var] 
      endif 
      
      ;TD2
      d1 = self->get_Var_Info('PSFC', DIMNAMES=dnames,DIMS=dims)      
      d2 = self->get_Var_Info('Q2')
      if (d1 and d2) then begin
        var = {name:'TD2',unit:'C',ndims:N_elements(dims),description:'2m Dewpoint Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var] 
      endif 
      
      
      ;THETA
      if (d1) then begin
        var = {name:'THETA',unit:'K',ndims:N_elements(dims),description:'Potential Temperature (theta)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]    
      endif 
            
      d1 = self->get_Var_Info('TT', DIMNAMES=dnames,DIMS=dims) ;Met em
      if (d1) then begin
        var = {name:'TK',unit:'K',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'TC',unit:'C',ndims:N_elements(dims),description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
      endif
      
      ;T2C
      d1 = self->get_Var_Info('T2', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'T2C',unit:'C',ndims:N_elements(dims),description:'2 m Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;RH
      d1 = self->get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('QVAPOR')
      d3 = self->get_Var_Info('P')
      d4 = self->get_Var_Info('PB')
      if (d1 and d2 and d3 and d4)then begin
        var = {name:'RH',unit:'%',ndims:N_elements(dims),description:'Relative Humidity',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif

      ;RH2
      d1 = self->get_Var_Info('T2', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('Q2')
      d3 = self->get_Var_Info('PSFC')
      if (d1 and d2 and d3) then begin
        var = {name:'RH2',unit:'%',ndims:N_elements(dims),description:'2 m Relative Humidity',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;SWUP
      d1 = self->get_Var_Info('SWDOWN', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('ALBEDO')      
      if (d1 and d2) then begin
        var = {name:'SWUP',unit:'w m-2',ndims:N_elements(dims),description:'upward short wave flux at ground surface',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;LWUP
      d1 = self->get_Var_Info('TSK', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('EMISS')      
      if (d1 and d2) then begin
        var = {name:'LWUP',unit:'w m-2',ndims:N_elements(dims),description:'upward long wave flux at ground surface',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;LWDOWN
      d1 = self->get_Var_Info('GLW', DIMNAMES=dnames,DIMS=dims)  
      if (d1) then begin
        var = {name:'LWDOWN',unit:'w m-2',ndims:N_elements(dims),description:'downward long wave flux at ground surface',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;NETRAD
      d1 = self->get_Var_Info('SWDOWN', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('ALBEDO')        
      d3 = self->get_Var_Info('TSK')        
      d4 = self->get_Var_Info('EMISS')        
      d5 = self->get_Var_Info('GLW')        
      d6 = self->get_Var_Info('SWDOWN')        
      if (d1 and d2 and d3 and d4 and d5 and d6) then begin
        var = {name:'NETRAD',unit:'w m-2',ndims:N_elements(dims),description:'net radiation at ground surface (+ = downward)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;TER      
      d1 = self->get_Var_Info('HGT', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'TER',unit:'m',ndims:2L,description:'Model Terrain Height',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      d1 = self->get_Var_Info('HGT_M', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'TER',unit:'m',ndims:2L,description:'Model Terrain Height',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;lucat      
      d1 = self->get_Var_Info('LU_INDEX', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'LUCAT',unit:'-',ndims:2L,description:'Model Landuse Category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;soiltop      
      d1 = self->get_Var_Info('SCT_DOM', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILTOP',unit:'-',ndims:2L,description:'Model dominant soil category (top)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      d1 = self->get_Var_Info('ISLTYP', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILTOP',unit:'-',ndims:2L,description:'Model dominant soil category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;soilbot      
      d1 = self->get_Var_Info('SCB_DOM', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILBOT',unit:'-',ndims:2L,description:'Model dominant soil category (bottom)',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      d1 = self->get_Var_Info('ISLTYP', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'SOILBOT',unit:'-',ndims:2L,description:'Model dominant soil category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;SLP
      d1 = self->get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('P')
      d3 = self->get_Var_Info('PB')
      d4 = self->get_Var_Info('QVAPOR')      
      d5 = self->get_Var_Info('PH')
      d6 = self->get_Var_Info('PHB')
      if (d1 and d2 and d3 and d4 and d5 and d6) then begin
        var = {name:'SLP',unit:'hPa',ndims:N_elements(dims)-1,description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
      endif
      
      d1 = self->get_Var_Info('CLDFRA', DIMNAMES=dnames,DIMS=dims)
      if d1 then begin
        var = {name:'SCLD',unit:'-',ndims:N_elements(dims)-1,description:'Total Column Clouds',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
        var = {name:'SCLDFRA',unit:'-',ndims:N_elements(dims)-1,description:'Surface cloud fraction',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
      endif
      
      d1 = self->get_Var_Info('PMSL', DIMNAMES=dnames,DIMS=dims) ;MET EM
      if d1 then begin
        var = {name:'SLP',unit:'hPa',ndims:N_elements(dims)-1,description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;SLP_B
      d1 = self->get_Var_Info('PSFC', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('T2')
      if self.type eq 'MET' then d3 = self->get_Var_Info('HGT_M') $
       else  d3 = self->get_Var_Info('HGT')
      if (d1 and d2 and d3) then begin
        var = {name:'SLP_B',unit:'hPa',ndims:N_elements(dims),description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif      
      
      ;WS and WD
      d1 = self->get_Var_Info('U10', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('V10') 
      if (d1 and d2) then begin
        var = {name:'WS10',unit:'m s-1',ndims:N_elements(dims),description:'10 m wind speed',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'WD10',unit:'degrees',ndims:N_elements(dims),description:'10 m wind direction',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
        d1 = self->get_Var_Info('COSALPHA')
        d2 = self->get_Var_Info('SINALPHA') 
        if d1 and d2 then begin
          var = {name:'UMET10',unit:'m s-1',ndims:N_elements(dims),description:'U component of 10m wind rotated to earth coordinates',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'VMET10',unit:'m s-1',ndims:N_elements(dims),description:'V component of 10m wind rotated to earth coordinates',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'WDMET10',unit:'degrees',ndims:N_elements(dims),description:'10m wind direction rotated to earth coordinates',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]          
        endif         
      endif      
      d1 = self->get_Var_Info('U', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('V') 
      if (d1 and d2) then begin
        dnames = utils_replace_string(dnames, '_stag', '')
        var = {name:'WS',unit:'m s-1',ndims:N_elements(dims),description:'Horizontal wind speed on mass grid points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'WD',unit:'degrees',ndims:N_elements(dims),description:'Horizontal wind direction on mass grid points',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
      endif      
      
      ; Atmospheric moisture fluxes
      d1 = self->get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('QVAPOR')
      d3 = self->get_Var_Info('P')
      d4 = self->get_Var_Info('PB')      
      if (d1 and d2 and d3 and d4) then begin
        du = self->get_Var_Info('U') 
        dv = self->get_Var_Info('V') 
        dw = self->get_Var_Info('W')
        if du then begin
          var = {name:'U_VAPORFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Zonal water vapor flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'U_INTVAPORFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated zonal water vapor flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if dv then begin
          var = {name:'V_VAPORFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Meridional water vapor flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'V_INTVAPORFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated meridional water vapor flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if dw then begin
          var = {name:'W_VAPORFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Vertical water vapor flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'W_INTVAPORFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated vertical water vapor flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if du and dv then begin
          var = {name:'INTVAPORFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated absolute water vapor flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif          
      endif 
      
      d1 = self->get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('QCLOUD') or self->get_Var_Info('QRAIN')
      d3 = self->get_Var_Info('P')
      d4 = self->get_Var_Info('PB')      
      if (d1 and d2 and d3 and d4) then begin
        du = self->get_Var_Info('U') 
        dv = self->get_Var_Info('V') 
        dw = self->get_Var_Info('W')
        if du then begin
          var = {name:'U_LIQUIDFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Zonal liquid water flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'U_INTLIQUIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated zonal liquid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if dv then begin
          var = {name:'V_LIQUIDFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Meridional liquid water flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'V_INTLIQUIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated meridional liquid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if dw then begin
          var = {name:'W_LIQUIDFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Vertical liquid water flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'W_INTLIQUIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated vertical liquid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if du and dv then begin
          var = {name:'INTLIQUIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated absolute liquid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif          
      endif 
      
      d1 = self->get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->get_Var_Info('QSNOW') or self->get_Var_Info('QICE') or self->get_Var_Info('QGRAUP')
      d3 = self->get_Var_Info('P')
      d4 = self->get_Var_Info('PB')      
      if (d1 and d2 and d3 and d4) then begin
        du = self->get_Var_Info('U') 
        dv = self->get_Var_Info('V') 
        dw = self->get_Var_Info('W')
        if du then begin
          var = {name:'U_SOLIDFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Zonal solid water flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'U_INTSOLIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated zonal solid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if dv then begin
          var = {name:'V_SOLIDFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Meridional solid water flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'V_INTSOLIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated meridional solid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if dw then begin
          var = {name:'W_SOLIDFLUX',unit:'kg m-2 s-1',ndims:N_elements(dims),description:'Vertical solid water flux',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
          var = {name:'W_INTSOLIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated vertical solid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif
        if du and dv then begin
          var = {name:'INTSOLIDFLUX',unit:'kg m-1 s-1',ndims:N_elements(dims)-1,description:'Column integrated absolute solid water flux',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
          dvars = [dvars,var]
        endif          
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
  not_implemented = ['TK','TC','THETA','SLP','SLP_B','PRESSURE','GEOPOTENTIAL', 'Z', 'T2PBL', 'T2PBLC', 'T2ETA', 'T2ETAC', $
                     'QLIQUID', 'QSOLID', 'RH', 'TD', 'TD2', 'SOILTOP', 'SOILBOT', 'SCLD', 'COL_QVAPOR', 'COL_QLIQUID', 'COL_QSOLID', $
                     'SCLDFRA', 'UMET10', 'VMET10','WDMET10','WS','WD','GEOPOTENTIAL','ZAG','U_WVFLUX','V_WVFLUX', $
                     'U_INTWVFLUX', 'V_INTWVFLUX','INTWVFLUX']
                     
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
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
        
   'SNOWFALL': begin
      value = self->w_GEO_nc::get_TimeSerie('SNOWNC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
        
    'PRCP_FR': begin
      value = self->w_GEO_nc::get_TimeSerie('PRCP', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      sr = self->w_GEO_nc::get_TimeSerie('SR', point_i, point_j, K = K, t0 = t0, t1 = t1)
      ptm = where(sr gt (machar()).eps and value gt (machar()).eps, cntp, COMPLEMENT=pc, NCOMPLEMENT=cntc)
      if cntp ne 0 then value[ptm] = sr[ptm] * value[ptm]
      if cntc ne 0 then value[pc] = 0.
      _acc_to_step = FALSE
    end
    
    'PRCP_NC': begin
      value = self->w_GEO_nc::get_TimeSerie('RAINNC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'PRCP_C': begin
      value = self->w_GEO_nc::get_TimeSerie('RAINC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'GRAUPEL': begin
      value = self->w_GEO_nc::get_TimeSerie('GRAUPELNC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'HAIL': begin
      value = self->w_GEO_nc::get_TimeSerie('HAILNC', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'POTEVAP': begin
      value = self->w_GEO_nc::get_TimeSerie('POTEVP', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      _acc_to_step = TRUE
    end
    
    'LWDOWN': begin
      value = self->w_GEO_nc::get_TimeSerie('GLW', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
    end
    
    'LWUP': begin
      value = self->w_GEO_nc::get_TimeSerie('TSK', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      e = self->w_GEO_nc::get_TimeSerie('EMISS', point_i, point_j, K = K, t0 = t0, t1 = t1)
      value = (5.6704e-8) * e * value^4
    end
    
    'SWUP': begin
      value = self->w_GEO_nc::get_TimeSerie('SWDOWN', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      value *= self->w_GEO_nc::get_TimeSerie('ALBEDO', point_i, point_j, K = K, t0 = t0, t1 = t1)
    end
    
    'NETRAD': begin
      value = self->w_GEO_nc::get_TimeSerie('SWDOWN', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames)
      swup = self->w_GEO_nc::get_TimeSerie('SWUP', point_i, point_j, K = K, t0 = t0, t1 = t1)
      lwup = self->w_GEO_nc::get_TimeSerie('LWUP', point_i, point_j, K = K, t0 = t0, t1 = t1)
      lwdown = self->w_GEO_nc::get_TimeSerie('LWDOWN', point_i, point_j, K = K, t0 = t0, t1 = t1)      
      value += lwdown - swup - lwup
    end
    
    'ET': begin
      value = self->w_GEO_nc::get_TimeSerie('ACLHF', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
        dims = dims, $ ;
        dimnames = dimnames) / 2.5e6       ; Heat of vaporization
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE    
    end
    
    'T2C': begin
      value = self->w_GEO_nc::get_TimeSerie('T2', point_i, point_j, time, nt, t0 = t0, t1 = t1, K = K , $
                          dims = dims, $ ;
                          dimnames = dimnames) - 273.15
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
      if self->get_Var_Info('HGT') then _id = 'HGT' $
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
;    OBJECT: out, optional, type=objref
;         The object reference to the underlying plot object.
;         
; :History:
;     Written by FaM, 2010.
;-      
pro w_WRF::plot_TimeSerie, varid, x, y, $
                           t0=t0, t1=t1, $
                           src=src, K=k, $
                           OBJECT=object

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
  
  title = 'WRF TS plot: ' + VarName
  title = title + '!C ' + description
  title = title + '!C ' + 'Grid point: [' + str_equiv(STRING(wrf_ind_i, FORMAT = '(I3)')) + ',' + str_equiv(STRING(wrf_ind_j, FORMAT = '(I3)')) + ']'
  title = title + '!C ' + 'WRF lon,lat: ' + str_equiv(STRING(wrf_lon, FORMAT='(F7.2)')) + ', ' + str_equiv(STRING(wrf_lat, FORMAT='(F7.2)'))
    
  w_gr_tzplot, times, var, TITLE=title, YTITLE=units, THICK=2, COLOR='red', position=[0.1,0.15,0.94,0.82], CHARSIZE=1., OBJECT=object
  
;  w_TimeLinePlot, var, times, varname, COLOR1='red', TITLE='WRF TS plot: ' + description, YTITLE=units, THICKNESS=2
;  
;  cgtext, 0.7915, 0.26, 'Grid point: [' + str_equiv(STRING(wrf_ind_i, FORMAT = '(I3)')) + ',' + str_equiv(STRING(wrf_ind_j, FORMAT = '(I3)')) + ']', $
;          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW
;  
;  cgtext, 0.7915 + 0.01, 0.2, 'WRF lon: ' + str_equiv(STRING(wrf_lon, FORMAT='(F7.2)')), $
;          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW  
;  cgtext, 0.7915 + 0.01, 0.15, 'WRF lat: ' + str_equiv(STRING(wrf_lat, FORMAT='(F7.2)')), $
;          CHARSIZE=1, CHARTHICK = 1., COLOR = cgColor('BLUE'), /NORMAL, /WINDOW  
    
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
;             prcp: total precipitation (step-wize) [mm hr-1]
;             snowfall: Grid scale snow and ice (step-wize) [mm hr-1]
;             prcp_c: Cumulus precipitation (step-wize) [mm hr-1]
;             prcp_nc: Grid scale precipitation (step-wize) [m hr-1m]
;             prcp_fr: Frozen precipitation (step-wize) [mm hr-1]
;             graupel: Grid scale graupel (step-wize) [mm hr-1]
;             hail: Grid scale hail (step-wize) (step-wize) [mm hr-1]             
;             potevap:  Potential evaporation (step-wize) [w m-2]             
;             rh: Relative Humidity [%]
;             rh2: 2m Relative Humidity [%]
;             td: Dewpoint temperature [C]
;             td2: 2m Dewpoint temperature [C]
;             slp: Sea level pressure [hPa] (computed with full vertical levels - slow. See `utils_wrf_slp` 
;                  (If the vertical dimension is not present in the file, slp_b is computed automatically instead)
;             slp_b: Sea level pressure [hPa] (computed with surface values - fast. see `MET_barometric` for more info)
;             ter: Model terrain height [m] (static: no time dimension)
;             lucat: Model landuse category [] (static: no time dimension)
;             soiltop: Model soil category top [] (static: no time dimension)
;             soilbot: Model soil category bot [] (static: no time dimension)
;             tc: Temperature [C]
;             t2c: 2m Temperature [C]
;             t2eta: 2 m temperature (extrapolated from the first two eta-levels) [K]
;             t2etac: 2 m temperature (extrapolated from the first two eta-levels) [C]
;             t2pbl: 2 m temperature (linear fit from pbl eta-levels) [K]
;             t2pblc: 2 m temperature (linear fit from pbl eta-levels) [C]
;             theta: Potential temperature [K]
;             tk: Temperature [K]
;             ws10: Wind speed at 10m [m.s-1]
;             wd10: Wind direction [degrees]
;             ws: Horizontal wind speed at mass grid points [m.s-1]
;             wd: Horizontal wind direction at mass grid points [degrees]
;             geopotential: Full model geopotential [m2 s-2] (unstaggered)
;             pressure: Full model pressure [hPa]
;             z: Full model height (geopotential / 9.81) [m]
;             zag: Full model height above ground (geopotential / 9.81) [m]
;             umet10: U component of 10m wind rotated to earth coordinates [m.s-1]
;             vmet10: V component of 10m wind rotated to earth coordinates  [m.s-1]
;             wdmet10: 10m wind direction rotated to earth coordinates  [m.s-1]
;             qliquid: Liquid water mixing ratio [kg.kg-1]
;             qsolid: Solid water mixing ratio [kg.kg-1]
;             et: Evapotranspiration (step-wize) [mm.h-1]
;             fluxes: many water fluxes []
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
;   hour_of_day: in, optional, type = long
;                the hour of day to get the data from
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
                            HOUROFDAY=hourofday, $
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
  _acc_to_step = KEYWORD_SET(ACC_TO_STEP)
  _unstagger = KEYWORD_SET(UNSTAGGER)
   
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
    

  ; Check for the known diagnostic variable names
  case str_equiv(vid) of
      
    'PRCP': begin
      value = self->get_Var('RAINNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) + self->get_Var('RAINC', t0 = t0, t1 = t1)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'SNOWFALL': begin
      value = self->get_Var('SNOWNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'PRCP_FR': begin
      value = self->get_Var('PRCP', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      sr = self->get_Var('SR', t0 = t0, t1 = t1)
      ptm = where(sr gt (machar()).eps and value gt (machar()).eps, cntp, COMPLEMENT=pc, NCOMPLEMENT=cntc)
      if cntp ne 0 then value[ptm] = sr[ptm] * value[ptm]
      if cntc ne 0 then value[pc] = 0.
      _acc_to_step = FALSE
    end
    
    'PRCP_NC': begin
      value = self->get_Var('RAINNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE      
    end
    
    'PRCP_C': begin
      value = self->get_Var('RAINC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'GRAUPEL': begin
      value = self->get_Var('GRAUPELNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE
    end
    
    'HAIL': begin
      value = self->get_Var('HAILNC', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
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
    
    'QLIQUID': begin
      if self->get_Var_Info('QRAIN') then begin
        tmp = self->get_Var('QRAIN', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        ; Set some tolerance level to avoid underflows
        pu = where(tmp lt (machar()).eps, cntu)
        if cntu ne 0 then tmp[pu] = 0.
        if N_ELEMENTS(value) eq 0 then value = tmp else value += tmp
      endif
      if self->get_Var_Info('QCLOUD') then begin
        tmp = self->get_Var('QCLOUD', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        ; Set some tolerance level to avoid underflows
        pu = where(tmp lt (machar()).eps, cntu)
        if cntu ne 0 then tmp[pu] = 0.
        if N_ELEMENTS(value) eq 0 then value = tmp else value += tmp
      endif
    end
    
    'QSOLID': begin
      if self->get_Var_Info('QSNOW') then begin
        tmp = self->get_Var('QSNOW', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        ; Set some tolerance level to avoid underflows
        pu = where(tmp lt (machar()).eps, cntu)
        if cntu ne 0 then tmp[pu] = 0.
        if N_ELEMENTS(value) eq 0 then value = tmp else value += tmp
      endif
      if self->get_Var_Info('QGRAUP') then begin
        tmp = self->get_Var('QGRAUP', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        ; Set some tolerance level to avoid underflows
        pu = where(tmp lt (machar()).eps, cntu)
        if cntu ne 0 then tmp[pu] = 0.
        if N_ELEMENTS(value) eq 0 then value = tmp else value += tmp
      endif
      if self->get_Var_Info('QICE') then begin
        tmp = self->get_Var('QICE', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        ; Set some tolerance level to avoid underflows
        pu = where(tmp lt (machar()).eps, cntu)
        if cntu ne 0 then tmp[pu] = 0.
        if N_ELEMENTS(value) eq 0 then value = tmp else value += tmp
      endif      
    end  
    
    'TK': begin
      if self->get_Var_Info('T') then begin
        T = self->get_Var('T', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
          dims = dims, $
          dimnames = dimnames)
        P = self->get_Var('P', T0=t0, T1=t1, ZLEVELS=zlevels)
        PB = self->get_Var('PB', T0=t0, T1=t1, ZLEVELS=zlevels)
        T += 300.
        P += PB
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
        
    'ET': begin
      value = self->get_Var('ACLHF', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) / 2.5e6       ; Heat of vaporization
      ts = ((*self.time)[1] - (*self.time)[0]) / H_QMS
      if ts ne 1 then value = value / ts
      _acc_to_step = TRUE      
    end
        
    'TD': begin
        QVAPOR = self->get_Var('QVAPOR', time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels,  $
                    dims = dims, $
                    dimnames = dimnames)
        P = self->get_Var('P', T0=t0, T1=t1, ZLEVELS=zlevels)
        PB = self->get_Var('PB', T0=t0, T1=t1, ZLEVELS=zlevels)
        P += PB
        value = utils_wrf_td(P,QVAPOR)    ; calculate TD
    end
        
    'TD2': begin
        PSFC = self->get_Var('PSFC', time, nt, t0 = t0, t1 = t1,  $
                    dims = dims, $
                    dimnames = dimnames)
        Q2 = self->get_Var('Q2', T0=t0, T1=t1)
        value = utils_wrf_td(PSFC,Q2)    ; calculate TD2
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
    
    'T2ETA': begin
      value = self->get_Var('TK', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, ABOVE_GROUND_LEVELS=2., $
        dimnames = dimnames)      
     end
    
    'T2ETAC': begin
      value = self->get_Var('T2ETA', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) - 273.15      
     end
    
    'T2PBL': begin
      tk = self->get_Var('TK', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      z = self->get_Var('ZAG', t0 = t0, t1 = t1)
      pblh = self->get_Var('PBLH', t0 = t0, t1 = t1)
      pdimtochange = where(strmatch(str_equiv(dimnames), 'BOTTOM_TOP'), cntdimtochange)
      if cntdimtochange ne 1 then Message, 'aaarg'
      utils_array_remove, pdimtochange, dimnames
      value = REFORM(tk[*,*,0,*]) * 0
      for t=0, nt-1 do begin
        tmptk = REFORM(tk[*,*,*,t], dims[0]*dims[1], dims[2])
        tmpz = REFORM(z[*,*,*,t], dims[0]*dims[1], dims[2])
        tmppblh = REFORM(pblh[*,*,t], dims[0]*dims[1])
        tmpval = FLTARR(dims[0], dims[1])
        tmpcorr = FLTARR(dims[0], dims[1])
        for e=0, N_ELEMENTS(value[*,*,0,0])-1 do begin
          ph = where(tmpz[e,*] le (tmppblh[e])[0], cnth)
          if cnth le 1 then ph = [0,1]
          tmptmptk = (tmptk[e,ph])[*]
          if max(ABS(tmptmptk-mean(tmptmptk))) lt 0.00001 then begin
            a = 0.
            lr_corr = 1.
          endif else begin
            a = REGRESS((tmpz[e,ph])[*], tmptmptk, CONST=b)
          endelse
          tmpval[e] = a * 2 + b
        endfor
        value[*,*,t] = tmpval
      endfor
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
      if self->get_Var_Info('HGT') then _id = 'HGT' $
       else _id = 'HGT_M'
      value = self->get_Var(_id, time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, ZLEVELS=zlevels, $
        dimnames = dimnames)
    end
    
    'SOILTOP': begin
      if self->get_Var_Info('SCT_DOM') then _id = 'SCT_DOM' $
       else _id = 'ISLTYP'
      value = self->get_Var(_id, time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, ZLEVELS=zlevels, $
        dimnames = dimnames)
    end
    
    'SOILBOT': begin
      if self->get_Var_Info('SCB_DOM') then _id = 'SCB_DOM' $
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
    
    'SCLD': begin
      cld = self->get_Var('CLDFRA', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      mdims = SIZE(cld, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for t=0, nt-1 do value[*,*,t] = TOTAL(cld[*,*,*,t], 3)   ; Total
      if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
    end
    
    'COL_QVAPOR': begin
      q = self->get_Var('QVAPOR', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      ; Set some tolerance level to avoid underflows
      pu = where(q lt (machar()).eps, cntu)
      if cntu ne 0 then q[pu] = 0.
      mdims = SIZE(q, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for t=0, nt-1 do value[*,*,t] = TOTAL(q[*,*,*,t], 3)   ; Total
      ; Set some tolerance level to avoid underflows
      pu = where(value lt (machar()).eps, cntu)
      if cntu ne 0 then value[pu] = 0.
      if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
    end
    
    'COL_QLIQUID': begin
      q = self->get_Var('QLIQUID', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      mdims = SIZE(q, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for t=0, nt-1 do value[*,*,t] = TOTAL(q[*,*,*,t], 3)   ; Total
      ; Set some tolerance level to avoid underflows
      pu = where(value lt (machar()).eps, cntu)
      if cntu ne 0 then value[pu] = 0.
      if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
    end
    
    'COL_QSOLID': begin
      q = self->get_Var('QSOLID', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      mdims = SIZE(q, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for t=0, nt-1 do value[*,*,t] = TOTAL(q[*,*,*,t], 3)   ; Total
      ; Set some tolerance level to avoid underflows
      pu = where(value lt (machar()).eps, cntu)
      if cntu ne 0 then value[pu] = 0.
      if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
    end
    
    'SCLDFRA': begin
      cld = self->get_Var('SCLD', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) < 1
      self->Get_XY, xx, yy, nx, ny
      cld = reform(cld, [nx * ny, nt])
      value = cld * 0.
      radius = 50001.^2
      for i = 0L, N_ELEMENTS(xx)-1 do begin
        ; at my point
        x = xx[i]
        y = yy[i]
        _cld = cld
        mask = ((xx-x)^2 + (yy-y)^2) lt radius
        _cld[where(mask eq 0.), *] = 0.
        value[i, *] = TOTAL(_cld,1) / TOTAL(mask)
      endfor
      value = reform(value, [nx, ny, nt])
    end
    
    'SLP_B': begin
      ps = self->get_Var('PSFC', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames) * 0.01 ; in hPa
      T2 = self->get_Var('T2', T0=t0, T1=t1) - 273.15 ; in degC
      zs = self->get_Var('TER', T0=t0, T1=t1) ; in m
      mdims = SIZE(t2, /DIMENSIONS)
      value = FLTARR(mdims[0],mdims[1],nt)
      for k=0,Nt-1 do value[*,*,k] = MET_barometric(ps[*,*,k], zs, T2[*,*,k], 0.)
      if nt eq 1 then dimnames = [dimnames[0],dimnames[1]] else dimnames = [dimnames[0],dimnames[1],dimnames[3]]
    end
    
    'UMET10': begin
      u10 = self->get_Var('U10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10', T0=t0, T1=t1)      
      cosalpha = self->get_Var('COSALPHA', T0=t0, T1=t1)
      sinalpha = self->get_Var('SINALPHA', T0=t0, T1=t1)
      if SIZE(cosalpha, /N_DIMENSIONS) ne size(u10, /N_DIMENSIONS) then begin ; has been cropped
        _dims = dims & _dims[2:*] = 1
        cosalpha =  rebin(reform(cosalpha,_dims), dims) ; make it same dim
        sinalpha =  rebin(reform(sinalpha,_dims), dims) ; make it same dim        
      endif
      value = u10*cosalpha + v10*sinalpha
    end

    'VMET10': begin
      u10 = self->get_Var('U10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10', T0=t0, T1=t1)      
      cosalpha = self->get_Var('COSALPHA', T0=t0, T1=t1)
      sinalpha = self->get_Var('SINALPHA', T0=t0, T1=t1)
      if SIZE(cosalpha, /N_DIMENSIONS) ne size(u10, /N_DIMENSIONS) then begin ; has been cropped
        _dims = dims & _dims[2:*] = 1
        cosalpha =  rebin(reform(cosalpha,_dims), dims) ; make it same dim
        sinalpha =  rebin(reform(sinalpha,_dims), dims) ; make it same dim        
      endif
      value = v10*cosalpha - u10*sinalpha
    end

    'WS10': begin
      u10 = self->get_Var('U10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10', T0=t0, T1=t1)      
      value = SQRT(u10^2 + v10^2)
    end
    
    'WD10': begin
      u10 = self->get_Var('U10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10', T0=t0, T1=t1)      
      MET_u_v_to_ws_wd, ret, u10, v10, WD=value
    end
    
    'WDMET10': begin
      u10 = self->get_Var('Umet10', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('Vmet10', T0=t0, T1=t1)      
      MET_u_v_to_ws_wd, ret, u10, v10, WD=value
    end
    
    'WS': begin
      u = self->get_Var('U', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames, /UNSTAGGER)
      v = self->get_Var('V', T0=t0, T1=t1, /UNSTAGGER)      
      value = SQRT(u^2 + v^2)
    end

    'WD': begin
      u = self->get_Var('U', time, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames, /UNSTAGGER)
      v = self->get_Var('V', T0=t0, T1=t1, /UNSTAGGER)      
      MET_u_v_to_ws_wd, ret, u, v, WD=value
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
        ; This is the only variable one unstaggers
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
    
    'ZAG': begin
        value = self->get_Var('Z', time, nt, T0=t0, T1=t1,  $
          dims = dims, $
          dimnames = dimnames, ZLEVELS=zlevels)          
        _dims = dims & _dims[2:*] = 1
        ter =  rebin(reform(self->get_Var('ter'),_dims), dims) ; make it same dim as z
        value = value - TEMPORARY(ter)
    end
    
    'U_VAPORFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'V_VAPORFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'W_VAPORFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'U_LIQUIDFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'V_LIQUIDFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'W_LIQUIDFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'U_SOLIDFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
          dims=dims, $
          dimnames=dimnames)
    end
    
    'V_SOLIDFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'W_SOLIDFLUX': begin
      if N_ELEMENTS(zlevels) eq 1 then Message, 'ZLEVELS and moisture fluxes not compatible'
      value = w_wrf_moisture_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'U_INTVAPORFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'V_INTVAPORFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'W_INTVAPORFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'U_INTLIQUIDFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'V_INTLIQUIDFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'W_INTLIQUIDFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'U_INTSOLIDFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'V_INTSOLIDFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'W_INTSOLIDFLUX': begin
      value = w_wrf_integrate_flux(vid, self, time, nt, t0=t0, t1=t1, $
        dims=dims, $
        dimnames=dimnames)
    end
    
    'INTVAPORFLUX': begin
      u = self->get_Var('U_INTVAPORFLUX', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, $
          dimnames = dimnames)
      v = self->get_Var('V_INTVAPORFLUX', time, nt, t0 = t0, t1 = t1)
      value = SQRT(u^2 + v^2)
    end

    'INTLIQUIDFLUX': begin
      u = self->get_Var('U_INTLIQUIDFLUX', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, $
          dimnames = dimnames)
      v = self->get_Var('V_INTLIQUIDFLUX', time, nt, t0 = t0, t1 = t1)
      value = SQRT(u^2 + v^2)
    end

    'INTSOLIDFLUX': begin
      u = self->get_Var('U_INTSOLIDFLUX', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, $
          dimnames = dimnames)
      v = self->get_Var('V_INTSOLIDFLUX', time, nt, t0 = t0, t1 = t1)
      value = SQRT(u^2 + v^2)
    end

    else:
    
  endcase
    
  if N_ELEMENTS(value) eq 0 then begin ;This is probably a standard variable
       
    value = self->w_GEO_nc::get_Var(vid, time, nt, t0 = t0, t1 = t1, ZLEVELS=zlevels, $
      HOUROFDAY=hourofday,  $
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
      h = self->get_Var('z', T0=t0, T1=t1, ZLEVELS=zlevels)
      value = reform(utils_wrf_intrp3d(value, h, height_levels))
      nlocs = N_ELEMENTS(height_levels)
      if nlocs eq 1 then utils_array_remove, pdimtochange, dimnames $
      else dimnames[pdimtochange] = 'height_levels'
    endif
    
    if _do_ag then begin
      h = self->get_Var('zag', T0=t0, T1=t1)
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
