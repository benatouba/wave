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
            type:               ''    ,  $ ; type of active file: AGG, WRF, GEO, MET or INP
            version:            ''    ,  $ ; WRF version
            hstep:         {TIME_STEP},  $ ; Time step of the file
            ndiagvar:               0L,  $ ; Number of diagnostic variables
            diagVars:        PTR_NEW(),  $ ; Diagnostic Variables (derived from WRF output)
            dom:                    0L,  $ ; id of the considered nested domain
            west_east:              0L,  $ ; original X dimension (unstaggered)
            south_north:            0L,  $ ; original Y dimension (unstaggered)
            bottom_top:             0L,  $ ; original Z dimension (unstaggered)
            dx:                     0D,  $ ; grid spacing in m
            dy:                     0D,  $ ; grid spacing in m
            i_parent_start:         0L,  $ ; i index of the start point in parent domain
            j_parent_start:         0L,  $ ; j index of the start point in parent domain
            parent_grid_ratio:      0L   $ ; ratio to parent
            }
    
END

;+
; :Description:
;    Redefine the WRF subset.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;       SUBSET_LL : in, optional, type = float vector 
;                   set it to the desired subset corners to automatically subset the data.
;                   Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : in, type = long vector
;                   Four elements array::              
;                   first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                   second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                   third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                   fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the 'define_subset' method.
;                   
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84
;                   datum in which the Lat and Lons from 'SUBSET_LL' are defined
;                   
;       CROPCHILD: in, optional
;                  set this keyword to crop the grid to the area covered by its child domain (if any)
;                  
;       CROPBORDER: in, optional
;                  set this keyword to crop the grid of CROPBORDER elements on each side
;    
; :History:
;     Written by FaM, 2010.
;-      
function w_WRF::define_subset, SUBSET_LL  = subset_ll,  $
                               SUBSET_IJ  = subset_ij,  $
                               LL_DATUM   = ll_datum ,  $
                               CROPCHILD  = cropchild   ,  $
                               CROPBORDER = cropborder 

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
  do_init = self.cropped eq '' ;first init
  firstcall = do_init
   
  if ~ do_init then begin
    if KEYWORD_SET(CROPBORDER) then do_init = self.cropped ne 'BORDER'  $ ; Is not cropped yet
    else if KEYWORD_SET(CROPCHILD) then do_init = self.cropped ne 'CROPCHILD'   $ ; Is not cropped yet    
    else if KEYWORD_SET(SUBSET_LL) then do_init = TRUE  $  ; Is not cropped yet    
    else if KEYWORD_SET(SUBSET_IJ) then do_init = self.cropped ne 'SUBSET'  $  ; Is not cropped yet    
    else do_init = self.cropped ne 'FALSE'   ; Is cropped but we dont want to
  endif
  
  if ~ do_init and KEYWORD_SET(SUBSET_IJ) then begin
    if ~ arg_okay(SUBSET_ij, /ARRAY, /NUMERIC, N_ELEM=4) then Message, WAVE_Std_Message('SUBSET_ij', /ARG)
    do_init = self.subset[0] ne SUBSET_IJ[0]
    if ~ do_init then do_init = self.subset[1] ne SUBSET_IJ[1]
    if ~ do_init then do_init = self.subset[2] ne SUBSET_IJ[2]
    if ~ do_init then do_init = self.subset[3] ne SUBSET_IJ[3]    
  endif
  
  if ~ do_init and KEYWORD_SET(CROPBORDER) then begin
    if ~ arg_okay(CROPBORDER, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('CROPBORDER', /ARG) 
    do_init = self.subset[0] ne CROPBORDER
    if ~ do_init then do_init = self.subset[1] ne self.west_east - 2 * CROPBORDER
    if ~ do_init then do_init = self.subset[2] ne CROPBORDER
    if ~ do_init then do_init = self.subset[3] ne self.south_north - 2 * CROPBORDER
  endif
      
  if ~ do_init then return, 1 ;nothing to do
  
  if KEYWORD_SET(CROPBORDER) then self.cropped = 'BORDER' $
    else if KEYWORD_SET(CROPCHILD) then self.cropped = 'CROPCHILD' $
      else if KEYWORD_SET(Subset_LL) then self.cropped = 'SUBSET_LL' $
        else if KEYWORD_SET(Subset_IJ) then self.cropped = 'SUBSET_IJ' $
          else self.cropped = 'FALSE'  
        
  ;************
  ; GRID info *
  ;************      
  center_lat = self->get_Gatt('CEN_LAT')
  center_lon = self->get_Gatt('CEN_LON')
  
  ; Get easting and northings from dom center
  GIS_coord_trafo, ret, center_lon, center_lat, e, n, SRC=self.tnt_c.proj.datum, DST= self.tnt_c.proj    
  nx = self.west_east
  ny = self.south_north
  x0 =  - (nx-1) / 2. * self.dx + e ; UL corner
  y0 =    (ny-1) / 2. * self.dy + n ; UL corner
  if FIRSTCALL then begin
    IF NOT self->w_Grid2D::Init(  nx = nx                , $
                                ny = ny                , $
                                dx = self.DX           , $
                                dy = self.dy           , $
                                x0 = x0                , $
                                y0 = y0                , $
                                proj = self.tnt_c.proj) THEN RETURN, 0  
  endif else begin
   IF NOT self->w_Grid2D::ReInit(  nx = nx                , $
                                 ny = ny                , $
                                 dx = self.DX           , $
                                 dy = self.DY           , $
                                 x0 = x0                , $
                                 y0 = y0                , $
                                 proj = self.tnt_c.proj) THEN RETURN, 0  
  endelse
  
  ; Indexes in the original ncdf file
  isubs =[0l,0l,0l,0l]
           
  case self.cropped of
    'BORDER': begin
      if ~ arg_okay(CROPBORDER, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('CROPBORDER', /ARG) 
      if cropborder lt 0 or cropborder ge nx / 2 then message, 'Cropsize not ok'
      if cropborder lt 0 or cropborder ge ny / 2 then message, 'Cropsize not ok'
      isubs = [cropborder, nx-2*cropborder, cropborder, ny-2*cropborder]
    end
    'CROPCHILD': begin
      if self->get_Var_Info('NEST_POS') then begin
        nest_pos = self->w_NCDF::get_Var('NEST_POS')
        nest_pos = nest_pos[*,*,0]
        npos = where(nest_pos ne 0, cnt)
        if cnt ne 0 then begin
          inds = ARRAY_INDICES(NEST_POS, npos)
          mx = min(inds[0,*]) - 1
          my = min(inds[1,*]) - 1
          isubs = [mx,max(inds[0,*])-mx+2,my,max(inds[1,*])-my+2]
        endif else begin
          ; we have to check if there is the child file here
          file1 = self.path
          GEN_str_subst, ret, file1,'d0'+str_equiv(self.dom),'d0'+str_equiv(self.dom+1),file2
          if ~NCDF_IsValidFile(file2) then Message, 'could not find NEST_POS or the child domains file for cropping.'
          cdfid = NCDF_OPEN(file2, /NOWRITE)
          if self.type eq 'GEO' or self.type eq 'MET' then attid = 'i_parent_start' else attid = 'I_PARENT_START'
          NCDF_ATTGET, Cdfid , attid, ipar, /GLOBAL
          if self.type eq 'GEO' or self.type eq 'MET' then attid = 'j_parent_start' else attid = 'J_PARENT_START'
          NCDF_ATTGET, Cdfid , attid, jpar, /GLOBAL
          if self.type eq 'GEO' or self.type eq 'MET' then attid = 'parent_grid_ratio' else attid = 'PARENT_GRID_RATIO'
          NCDF_ATTGET, Cdfid , attid, ratio, /GLOBAL
          NCDF_ATTGET, Cdfid , 'WEST-EAST_GRID_DIMENSION', ni, /GLOBAL
          NCDF_ATTGET, Cdfid , 'SOUTH-NORTH_GRID_DIMENSION', nj, /GLOBAL
          NCDF_close, cdfid
          neli = LONG(DOUBLE(ni)/DOUBLE(ratio))
          nelj = LONG(DOUBLE(nj)/DOUBLE(ratio))
          isubs = [iPar-1, neli,jPar-1,nelj]
        endelse        
      endif else begin
        ; we have to check if there is the child file here
        file1 = self.path
        GEN_str_subst, ret, file1,'d0'+str_equiv(self.dom),'d0'+str_equiv(self.dom+1),file2
        if ~NCDF_IsValidFile(file2) then Message, 'could not find NEST_POS or the child domains file for cropping.'
        cdfid = NCDF_OPEN(file2, /NOWRITE)
        if self.type eq 'GEO' or self.type eq 'MET' then attid = 'i_parent_start' else attid = 'I_PARENT_START'
        NCDF_ATTGET, Cdfid , attid, ipar, /GLOBAL
        if self.type eq 'GEO' or self.type eq 'MET' then attid = 'j_parent_start' else attid = 'J_PARENT_START'
        NCDF_ATTGET, Cdfid , attid, jpar, /GLOBAL
        if self.type eq 'GEO' or self.type eq 'MET' then attid = 'parent_grid_ratio' else attid = 'PARENT_GRID_RATIO'
        NCDF_ATTGET, Cdfid , attid, ratio, /GLOBAL
        NCDF_ATTGET, Cdfid , 'WEST-EAST_GRID_DIMENSION', ni, /GLOBAL
        NCDF_ATTGET, Cdfid , 'SOUTH-NORTH_GRID_DIMENSION', nj, /GLOBAL
        NCDF_close, cdfid
        neli = LONG(DOUBLE(ni)/DOUBLE(ratio))
        nelj = LONG(DOUBLE(nj)/DOUBLE(ratio))
        isubs = [iPar-1, neli,jPar-1,nelj]
      endelse    
    end
    'SUBSET_IJ': begin
       isubs = SUBSET_IJ 
       self.cropped = 'SUBSET'
    end
    'SUBSET_LL': begin
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
      isubs = [idl,cx,jdl,cy]
      self.cropped = 'SUBSET'
    end
    else: begin
      ; nothing to do
    end
  endcase
  
  before = self.cropped
  if ~self->w_GEO_nc::define_subset(SUBSET=isubs) then return, 0 
  self.cropped = before 
  if self.cropped ne 'FALSE' then begin
    x0 = x0 + self.dx*isubs[0]
    y0 = y0 - self.dy*(self.south_north - (isubs[3]+isubs[2]))
    nx = isubs[1]
    ny = isubs[3]
    IF NOT self->w_Grid2D::ReInit( nx = nx                , $
                                 ny = ny                , $
                                 dx = self.Dx           , $
                                 dy = self.dy           , $
                                 x0 = x0                , $
                                 y0 = y0                , $
                                 proj = self.tnt_c.proj) THEN RETURN, 0  
   
  endif
      
  if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = self.subset  
    
  return, 1
    
end
   
;+
; :Description:
;    Build function.
;
; :Keywords:
;       FILE      : in, optional, type = string
;                   the path to the WRF file. If not set, a dialog window will open
;       SUBSET_LL : in, optional, type = float vector 
;                   set it to the desired subset corners to automatically subset the data.
;                   Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : in, type = long vector
;                   Four elements array::              
;                   first  el: start index in the ncdf variable in X dimension. Default is 0 (no subset)
;                   second el: count of the variable in X dimension. default matches the size of the variable so that all data is written out. 
;                   third  el: start index in the ncdf variable in Y dimension. Default is 0 (no subset)
;                   fourth el: count of the variable in Y dimension. default matches the size of the variable so that all data is written out.
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the 'define_subset' method.
;                   
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84
;                   datum in which the Lat and Lons from 'SUBSET_LL' are defined
;                   
;       CROPCHILD: in, optional
;                  set this keyword to crop the grid to the area covered by its child domain (if any)
;                  
;       CROPBORDER: in, optional
;                  set this keyword to crop the grid of CROPBORDER elements on each side
;
; :Returns:
; 
;    1 if the object is created successfully. 
;    
; :History:
;     Written by FaM, 2010.
;-
Function w_WRF::Init, FILE       = file     ,  $
                      SUBSET_LL  = subset_ll,  $
                      SUBSET_IJ  = subset_ij,  $
                      LL_DATUM   = ll_datum ,  $
                      CROPCHILD  = cropchild   ,  $
                      CROPBORDER = cropborder
           
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
  if ftype eq '' then message, 'Input file not recognized as a known WRF product.'
  self.type = ftype
  self.version = str_equiv(title)
  meta = str_equiv(title)
  
  ;*******
  ; Time *
  ;*******
  ; Done by w_GEO_nc
  if self.nt gt 1 then begin
   if ~check_TimeSerie(*self.time, hstep) then Message, 'Time serie in the file is not regular.'
    self.hstep = hstep
  endif else if self.nt eq 1 then begin
    self.hstep = MAKE_TIME_STEP()
  endif else MESSAGE, 'Problem by reading time.'
   
  ; NESTING
  self.i_parent_start = self->get_Gatt('i_parent_start')
  self.j_parent_start = self->get_Gatt('j_parent_start')
  self.parent_grid_ratio = self->get_Gatt('parent_grid_ratio')
  self.dom = self->get_Gatt('grid_id')
  self.dx = self->get_Gatt('DX')
  self.dy = self->get_Gatt('DY')
  self.west_east = self->get_Gatt('WEST-EAST_GRID_DIMENSION')-1 ;stag
  self.south_north  = self->get_Gatt('SOUTH-NORTH_GRID_DIMENSION')-1 ;stag
  self.bottom_top = (self->get_Gatt('bottom-top_grid_dimension')-1) > 1
    
  ; Projection
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
  
  ;***********************
  ; Diagnostic variables *
  ;***********************    
  self->get_Varlist, /DIAGNOSTIC
    
  ;*********
  ; define *
  ;*********
  self.cropped = ''
  if NOT self->define_subset(SUBSET_LL = subset_ll,  $
                             SUBSET_IJ  = subset_ij,  $
                             LL_DATUM   = ll_datum ,  $
                             CROPCHILD  = cropchild   ,  $
                             CROPBORDER = cropborder) THEN RETURN, 0
    
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

  NCDF_CLOSE, self.cdfid
  PTR_FREE, self.varNames
  PTR_FREE, self.dimNames
  PTR_FREE, self.dimSizes
  PTR_FREE, self.gattNames
  
  PTR_FREE, self.time
  
  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  
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
;    cropped: out
;             type of the cropping    
;    type: out, type = string
;          type of active file: AGG, WRF, GEO, MET or INP
;    version: out, type = string
;             WRF version
;    hstep: out, type = {TIME_STEP} 
;           Time step of the file
;    bottom_top: out, type = long
;                original Z dimension (unstaggered)
;    dom: out, type = long
;         id of the considered nested domain
;    i_parent_start: out, type = long
;                    i index of the start point in parent domain
;    j_parent_start: out, type = long
;                    j index of the start point in parent domain
;    parent_grid_ratio: out, type = long
;                       ratio to parent
;    
;    _Ref_Extra: out
;                all parent classed property
;    
; :History:
;     Written by FaM, 2010.
;-      
PRO w_WRF::GetProperty,  $
    cropped = cropped         ,  $ ;
    type=type    ,  $ ; type of active file: AGG, WRF, GEO, MET or INP
    version=version    ,  $ ; WRF version
    hstep=hstep,  $ ; Time step of the file
    bottom_top = bottom_top,  $ ; original Z dimension (unstaggered)           
    dom = dom,  $ ; id of the considered nested domain
    i_parent_start = i_parent_start,  $ ; i index of the start point in parent domain
    j_parent_start = j_parent_start,  $ ; j index of the start point in parent domain
    parent_grid_ratio = parent_grid_ratio,  $ ; ratio to parent
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
      
  IF Arg_Present(cropped) NE 0 THEN cropped = self.cropped
  IF Arg_Present(type) NE 0 THEN type = self.type
  IF Arg_Present(version) NE 0 THEN version = self.version
  IF Arg_Present(dom) NE 0 THEN dom = self.dom
  IF Arg_Present(i_parent_start) NE 0 THEN i_parent_start = self.i_parent_start
  IF Arg_Present(hstep) NE 0 THEN hstep = self.hstep
  IF Arg_Present(bottom_top) NE 0 THEN bottom_top = self.bottom_top
  IF Arg_Present(j_parent_start) NE 0 THEN j_parent_start = self.j_parent_start
  IF Arg_Present(parent_grid_ratio) NE 0 THEN parent_grid_ratio = self.parent_grid_ratio
  
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
; :History:
;     Written by FaM, 2010.
;-
pro w_WRF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS = printvars, DIAGNOSTIC = diagnostic, ALL = all

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
  
    if self.ndiagvar eq 0 then begin    
      
      dvars = {name:'',unit:'',ndims:0L,description:'',type:'',dims:PTR_NEW(),dimnames:PTR_NEW()}
      
      ;PRCP
      d1 = self->w_NCDF::get_Var_Info('rainnc', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('rainc')
      if (d1 and d2) then begin
        var = {name:'PRCP',unit:'mm',ndims:3L,description:'Accumulated total precipitation',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;PRCP_STEP
      d1 = self->w_NCDF::get_Var_Info('rainnc', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('rainc')
      if (d1 and d2) then begin
        var = {name:'PRCP_STEP',unit:'mm',ndims:3L,description:'Step total precipitation',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif else begin
        d1 = self->w_NCDF::get_Var_Info('rainnc', DIMNAMES=dnames,DIMS=dims)
        d2 = self->w_NCDF::get_Var_Info('rainc')
        if (d1 and d2) then begin
          var = {name:'PRCP_STEP',unit:'mm',ndims:3L,description:'Step total precipitation',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
          dvars = [dvars,var]
        endif
      endelse
      
      ;TK and TC
      d1= self->w_NCDF::get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('P')
      d3 = self->w_NCDF::get_Var_Info('PB')
      if (d1 and d2 and d3) then begin
        var = {name:'TK',unit:'K',ndims:3L,description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'TC',unit:'C',ndims:3L,description:'Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
      endif    
      
      ;TH and THETA
      if (d1) then begin
        var = {name:'TH',unit:'K',ndims:3L,description:'Potential Temperature (theta)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'THETA',unit:'K',ndims:3L,description:'Potential Temperature (theta)',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]    
      endif 

      ;T2C
      d1 = self->w_NCDF::get_Var_Info('T2', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'T2C',unit:'C',ndims:2L,description:'2 m Temperature',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;RH
      d1 = self->w_NCDF::get_Var_Info('T', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('P')
      d3 = self->w_NCDF::get_Var_Info('PB')
      d4 = self->w_NCDF::get_Var_Info('QVAPOR')
      if (d1 and d2 and d3 and d4) then begin
        var = {name:'RH',unit:'%',ndims:3L,description:'Relative Humidity',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;RH2
      d1 = self->w_NCDF::get_Var_Info('T2', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('Q2')
      d3 = self->w_NCDF::get_Var_Info('PSFC')
      if (d1 and d2 and d3) then begin
        var = {name:'RH2',unit:'%',ndims:2L,description:'2 m Relative Humidity',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif
      
      ;TER      
      if self.type eq 'MET' then d1 = self->w_NCDF::get_Var_Info('HGT_M', DIMNAMES=dnames,DIMS=dims) $
       else  d1 = self->w_NCDF::get_Var_Info('HGT', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'TER',unit:'m',ndims:2L,description:'Model Terrain Height',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
        dvars = [dvars,var]
      endif
      
      ;LUCAT      
      d1 = self->w_NCDF::get_Var_Info('LU_INDEX', DIMNAMES=dnames,DIMS=dims)
      if (d1) then begin
        var = {name:'LUCAT',unit:'-',ndims:2L,description:'Model Landuse Category',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1]]), dimnames:PTR_NEW([dnames[0],dnames[1]])}
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
        var = {name:'SLP',unit:'hPa',ndims:2L,description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW([dims[0],dims[1],dims[3]]), dimnames:PTR_NEW([dnames[0],dnames[1],dnames[3]])}
        dvars = [dvars,var]
      endif
      
      ;SLP_B
      d1 = self->w_NCDF::get_Var_Info('PSFC', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('T2')
      if self.type eq 'MET' then d3 = self->w_NCDF::get_Var_Info('HGT_M') $
       else  d3 = self->w_NCDF::get_Var_Info('HGT')
      if (d1 and d2 and d3) then begin
        var = {name:'SLP_B',unit:'hPa',ndims:2L,description:'Sea level pressure',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
      endif      
      
      ;WS and WD
      d1 = self->w_NCDF::get_Var_Info('U10', DIMNAMES=dnames,DIMS=dims)
      d2 = self->w_NCDF::get_Var_Info('V10') 
      if (d1 and d2) then begin
        var = {name:'WS10',unit:'m.s-1',ndims:2L,description:'10 m wind speed',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]
        var = {name:'WD10',unit:'degrees',ndims:2L,description:'10 m wind direction',type:'FLOAT', dims:PTR_NEW(dims), dimnames:PTR_NEW(dnames)}
        dvars = [dvars,var]        
      endif      
      
      dvars = dvars[1:*]   
      self.ndiagvar = N_ELEMENTS(dvars)
      self.diagVars = PTR_NEW(dvars)      
      
    endif else dvars = *self.diagVars
    
    varid = LONARR(self.ndiagvar) - 1
    varnames = dvars.name
    varndims = dvars.ndims
    varunits = dvars.unit
    vardescriptions = dvars.description
    vartypes = dvars.type
    
    if KEYWORD_SET(PRINTVARS) then print, 'WAVE diagnostic variables:'
    if KEYWORD_SET(PRINTVARS) then for i = 0, self.ndiagvar-1 DO print, 'Id: ' + str_equiv(varid[i]) + $
       '. Name: ' + varnames[i] + '. Unit: ' + varunits[i] + '. Ndims: ' + str_equiv(varndims[i])+ $
         '. Type: ' + VARTYPES[i] + '. Description: ' + str_equiv(vardescriptions[i])
    
  endif else if KEYWORD_SET(ALL) then begin
  
    self->w_NCDF::get_Varlist, varid, varnames, varndims, varunits, vardescriptions, vartypes, PRINTVARS = printvars
    self->get_Varlist, dvarid, dvarnames, dvarndims, dvarunits, dvardescriptions, dvartypes, /DIAGNOSTIC, PRINTVARS = printvars
    varid = [varid,dvarid]
    varnames = [varnames,dvarnames]
    varndims = [varndims,dvarndims]
    varunits = [varunits,dvarunits]
    vardescriptions = [vardescriptions,dvardescriptions]
    vartypes = [vartypes,dvartypes]
    
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
  not_implemented = ['TK','TC','THETA','SLP','SLP_B']
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
      if self.type eq 'MET' then _id = 'HGT_M' else _id = 'HGT'
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
;    a few diagnostic variables are computed automatically if requested.
;    Here is a list of the diagnostic variables (only if requested WRF 
;    output variables are present. Check the available variables with
;    `w_WRF::get_Varlist, /DIAGNOSTIC, /PRINT`) ::
;              
;             prcp: total precipitation (accumulated, unless `/ACC_TO_STEP` is set)
;             prcp_step: total precipitation (step-wise)
;             rh: Relative Humidity [%]
;             rh2: 2m Relative Humidity [%]
;             slp: Sea level pressure [hPa] (computed with full vertical levels - slow. See `utils_wrf_slp` 
;                  (If the vertical dimension is not present in the file, slp_b is computed automatically instead)
;             slp_b: Sea level pressure [hPa] (computed with surface values - fast. see `MET_barometric` for more info)
;             ter: Model terrain height [m] (static: no time dimension)
;             lucat: Model landuse category [] (static: no time dimension)
;             tc: Temperature [C]
;             t2c: 2m Temperature [C]
;             theta: Potential temperature [K]
;             tk: Temperature [K]
;             ws10: wind speed at 10m [m.s-1]
;             wd10: wind direction [degrees]
; 
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
                            UNSTAGGER=unstagger , $
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
  
  ;Some check
  if str_equiv(Varid) eq 'SLP' and ~self->get_Var_Info('SLP') then varid = 'SLP_B'
  
  ;Check if the variable is available
  if ~self->get_Var_Info(Varid, out_id = vid, $
    units = units, $
    description = description, $
    varname = varname , $
    dims = dims, $
    dimnames = dimnames) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.'
    
  _acc_to_step = KEYWORD_SET(ACC_TO_STEP)
  
  ; Check for the known diagnostic variable names
  case str_equiv(vid) of
  
    'PRCP': begin
      value = self->get_Var('RAINNC', time, nt, t0 = t0, t1 = t1, dims=dims, dimnames = dimnames) + self->get_Var('RAINC', t0 = t0, t1 = t1)
    end
    
    'PRCP_STEP': begin
      d1 = self->w_NCDF::get_Var_Info('RAINNC_step')
      d2 = self->w_NCDF::get_Var_Info('RAINC_step')
      if d1 and d2 then begin
        value = self->get_Var('RAINNC_step', time, nt, t0 = t0, t1 = t1,  $
          dims = dims, $
          dimnames = dimnames) + self->get_Var('RAINC_step', t0 = t0, t1 = t1)
        _acc_to_step = FALSE
      endif else begin
        d1 = self->w_NCDF::get_Var_Info('RAINNC')
        d2 = self->w_NCDF::get_Var_Info('RAINC')
        if d1 and d2 then begin
          value = self->get_Var('RAINNC', time, nt, t0 = t0, t1 = t1,  $
            dims = dims, $
            dimnames = dimnames) + self->get_Var('RAINC', t0 = t0, t1 = t1)
          _acc_to_step = TRUE
        endif else Message, 'Precipitation variables not available'
      endelse
    end
    
    'TK': begin
      T = self->get_Var('T', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      P = self->get_Var('P', T0=t0, T1=t1)
      PB = self->get_Var('PB', T0=t0, T1=t1)
      T = T + 300.
      P = P + PB
      value = utils_wrf_tk(P,T)    ; calculate TK
    end
    
    'TC': begin
      value = self->get_Var('TK', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) - 273.15
    end
        
    'THETA': begin
      value = self->get_Var('T', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) + 300.
    end
    
    'T2C': begin
      value = self->get_Var('T2', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames) - 273.15
    end
    
    'RH': begin
      T = self->get_Var('T', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      P = self->get_Var('P', T0=t0, T1=t1)
      PB = self->get_Var('PB', T0=t0, T1=t1)
      QVAPOR = self->get_Var('QVAPOR', T0=t0, T1=t1) > 0.
      T = T + 300.
      P  = P + PB
      tk = utils_wrf_tk(P,T)
      value = utils_wrf_rh(QVAPOR, P, tk)
    end
    
    'RH2': begin
      T2 = self->get_Var('T2', time, nt, t0 = t0, t1 = t1,  $
        dims = dims, $
        dimnames = dimnames)
      PSFC = self->get_Var('PSFC', T0=t0, T1=t1)
      Q2 = self->get_Var('Q2', T0=t0, T1=t1) > 0.
      value = utils_wrf_rh(Q2, PSFC, T2)
    end
    
    'TER': begin
      if self.type eq 'MET' then _id = 'HGT_M' else _id = 'HGT'
      value = self->get_Var(_id, time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, $
        dimnames = dimnames)
    end
    
    'LUCAT': begin
      value = self->get_Var('LU_INDEX', time, nt, t0 = self.t0, t1 = self.t0,  $
        dims = dims, $
        dimnames = dimnames)
    end
    
    'SLP': begin
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
      u10 = self->get_Var('U10', times, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10')      
      MET_u_v_to_ws_wd, ret, u10, v10, WS = value
    end

    'WD10': begin
      u10 = self->get_Var('U10', times, nt, T0=t0, T1=t1,  $
        dims = dims, $
        dimnames = dimnames)
      v10 = self->get_Var('V10')      
      MET_u_v_to_ws_wd, ret, u10, v10, WD= value
    end
    
    else:
    
  endcase
    
  if N_ELEMENTS(value) eq 0 then begin ;This is probably a standard variable
       
    value = self->w_GEO_nc::get_Var(vid, time, nt, t0 = t0, t1 = t1,  $
      units = units, $
      description = description, $
      varname = varname , $
      dims = dims, $
      dimnames = dimnames)
      
  endif
  
  if KEYWORD_SET(UNSTAGGER) then begin  
    ndims = N_ELEMENTS(dims)
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
    message, 'Warning. You are using /ACC_TO_STEP with T0 -> T1 intervals, hope you know what you are doing', /INFORMATIONAL
    if nt eq 1 then Message, 'You asked to de-accumulate only one time-step. Not possible.'
   value = utils_ACC_TO_STEP(value)
   description += ' (de-accumulated)' 
  endif 
  
  dims = SIZE(value, /DIMENSIONS)
  if N_ELEMENTS(dims) ne N_ELEMENTS(DIMNAMES) then begin
   MESSAGE, 'Internal Warning: contact FaM.', /INFORMATIONAL
  endif
  return, value
  
end

;+
; :Description:
;    DEPRECATED. Use `w_WRF::get_Var("prcp")` instead.
;    
;    
; :History:
;     Written by FaM, 2010.
;-      
function w_WRF::get_prcp, times, nt, t0 = t0, t1 = t1, STEP_WIZE = step_wize, NONCONVECTIVE = NONCONVECTIVE, CONVECTIVE = CONVECTIVE

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN, 0
  ENDIF 
  
  Message, 'INFO: w_WRF::get_prcp is deprecated. You should use: result = w_WRF::get_Var("prcp") instead.', /INFORMATIONAL
  
  if KEYWORD_SET(CONVECTIVE) then pcp = self->get_Var('RAINC', times, nt, t0 = t0, t1 = t1) $
    else if KEYWORD_SET(NONCONVECTIVE) then pcp = self->get_Var('RAINNC',times, nt, t0 = t0, t1 = t1) $
      else pcp = self->get_Var('RAINNC', times, nt, t0 = t0, t1 = t1) + self->get_Var('RAINC', t0 = t0, t1 = t1)    
  
  if KEYWORD_SET(STEP_WIZE) then pcp = utils_ACC_TO_STEP(pcp)
  
  return, pcp
    
end
