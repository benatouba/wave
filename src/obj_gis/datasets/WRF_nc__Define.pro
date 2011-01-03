PRO WRF_nc__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { WRF_nc                   ,  $
            INHERITS Grid2D           ,  $
            INHERITS GEO_nc           ,  $
            type:               ''    ,  $ ; type of active file: AGG, WRF, GEO, MET or INP
            version:            ''    ,  $ ; WRF version
            hstep:         {TIME_STEP},  $ ; Time step of the file
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

function WRF_nc::define_subset, SUBSET_LL  = subset_ll,  $
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
    self.subset = 'ERROR'
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
    IF NOT self->grid2D::Init(  nx = nx                , $
                                ny = ny                , $
                                dx = self.DX           , $
                                dy = self.dy           , $
                                x0 = x0                , $
                                y0 = y0                , $
                                proj = self.tnt_c.proj) THEN RETURN, 0  
  endif else begin
   IF NOT self->grid2D::ReInit(  nx = nx                , $
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
      isubs = [cropborder, nx-2*cropborder, cropborder, ny-2*cropborder]
    end
    'CROPCHILD': begin
      if self->get_Var_Info('NEST_POS') then begin
        nest_pos = self->NCDF::get_Var('NEST_POS')
        nest_pos = nest_pos[*,*,0]
        npos = where(nest_pos ne 0, cnt)
        if cnt eq 0 then Message, 'You sure the domain has a nest?'
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
  if ~self->GEO_nc::define_subset(SUBSET=isubs) then return, 0 
  self.cropped = before 
  if self.cropped ne 'FALSE' then begin
    x0 = x0 + self.dx*isubs[0]
    y0 = y0 - self.dy*(self.south_north - (isubs[3]+isubs[2]))
    nx = isubs[1]
    ny = isubs[3]
    IF NOT self->grid2D::ReInit( nx = nx                , $
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


Function WRF_nc::Init, FILE       = file     ,  $
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
  IF NOT self->GEO_nc::Init(file = file) THEN RETURN, 0    

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
  ; Done by geo_nc
  if self.nt gt 1 then begin
   if ~check_TS(*self.time, hstep) then Message, 'Time serie in the file is not regular.'
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
    else: begin
        print, 'Projection currently not supported'
        stop
    end
  endswitch
    
  ; Make the projection
  GIS_make_proj, ret, proj, PARAM=proj_param
  self.tnt_c.proj = proj
    
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

pro WRF_nc::Cleanup

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
  
END

PRO WRF_nc::GetProperty,  $
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
  
  self->GRID2D::GetProperty, _Extra=extra
  self->GEO_nc::GetProperty, _Extra=extra
  
end

function WRF_nc::get_TS, varid, x, y, $
                              time, nt, $
                              t0 = t0, t1 = t1, $
                              src = src, $
                              point_i = point_i, $
                              point_j = point_j, $
                              point_lon = point_lon, $
                              point_lat = point_lat , $
                          varinfo = varinfo , $ ; 
                          units = units, $
                          description = description, $
                          varname = varname , $ ; 
                          dims = dims, $ ;
                          dimnames = dimnames 
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ON_ERROR, 2
  
  if N_PARAMS() lt 3 then Message, WAVE_Std_Message(/NARG)
  
  if ~self->get_Var_Info(Varid) then MESSAGE, 'Variable not found'  
  
  ; no go threw the possibilites:
  if N_ELEMENTS(src) EQ 0 then mysrc = self else mysrc = src
  
  ; This is to obtain the indexes in the grid
  self->transform,  x, y, point_i, point_j, SRC = mysrc, /NEAREST
  
  ; This is to obtain lat and lons of the selected grid point
  self->transform, point_i, point_j, dummy1, dummy2, src=self, $
    LON_DST=point_lon, LAT_DST=point_lat
  
  return, self->GEO_nc::get_TS(varid, point_i, point_j, time, nt, t0 = t0, t1 = t1, $
                          varinfo = varinfo , $ ; 
                          units = units, $
                          description = description, $
                          varname = varname , $ ; 
                          dims = dims, $ ;
                          dimnames = dimnames )
  
end


pro WRF_nc::plot_TS, varid, x, y, $
                              t0 = t0, t1 = t1, $
                              src = src, PNG = png

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
  
  var = self->get_ts( Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                         x, $
                         y, $
                         times, nt, $
                         src = src, $
                         t0 = t0, $
                         t1 = t1, $
                         point_i = wrf_ind_i, $
                         point_j = wrf_ind_j, $
                         point_lon = wrf_lon, $
                         point_lat = wrf_lat, $
                          varinfo = varinfo , $ ; 
                          units = units, $
                          description = description, $
                          varname = varname , $ ; 
                          dims = dims, $ ;
                          dimnames = dimnames )
   
  
  WTimeLine_plot, var, MAKE_ABS_DATE(QMS=times), varname, COLOR1='red', TITLE='WRF TS plot: ' + description, YTITLE=units, THICKNESS=2, /PIXMAP
  
  XYOUTS, 900, 500, 'Grid point: [' + str_equiv(STRING(wrf_ind_i, FORMAT = '(I3)')) + ',' + str_equiv(STRING(wrf_ind_j, FORMAT = '(I3)')) + ']', $
          CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color('BLUE'), /DEVICE 
  
  XYOUTS, 900, 460, 'WRF lon: ' + str_equiv(wrf_lon), $
          CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color('BLUE'), /DEVICE 
  XYOUTS, 900, 435, 'WRF lat: ' + str_equiv(wrf_lat), $
          CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color('BLUE'), /DEVICE          
  
  PLO_show_img, PIXMAP=pixmap, WINDOW = window, PNG = png
  
end

function WRF_nc::get_prcp, times, nt, t0 = t0, t1 = t1, STEP_WIZE = step_wize, NONCONVECTIVE = NONCONVECTIVE, CONVECTIVE = CONVECTIVE

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN, 0
  ENDIF 
  
  if KEYWORD_SET(CONVECTIVE) then pcp = self->get_Var('RAINC', times, nt, t0 = t0, t1 = t1) $
    else if KEYWORD_SET(NONCONVECTIVE) then pcp = self->get_Var('RAINNC',times, nt, t0 = t0, t1 = t1) $
      else pcp = self->get_Var('RAINNC', times, nt, t0 = t0, t1 = t1) + self->get_Var('RAINC')    
  
  if KEYWORD_SET(STEP_WIZE) then pcp = utils_ACC_TO_STEP(pcp)
  
  return, pcp
    
end

;function WRF_nc::get_T, time0 = time0, time1 = time1, times, nt, units = units
;
;  ; SET UP ENVIRONNEMENT
;  @WAVE.inc
;  COMPILE_OPT IDL2
;    
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    ok = WAVE_Error_Message()
;    RETURN, 0
;  ENDIF 
;  
;  p = self->get_Var('P')
;  pb = self->get_Var('PB')
;  T = self->get_Var('T') + 300.
;  
;  P1000MB=100000D
;  R_D=287D
;  CP=7*R_D/2.
; 
;  PI = (P / P1000MB) ^ (R_D/CP)
;  TK = PI*T
;  
;  times = *self.time
;  
;  n = self.nt
;  p1 = 0
;  p2 = n - 1
;  
;  units = 'K'
;  
;  if arg_okay(time0, STRUCT={ABS_DATE}) then begin 
;     v = 0 > VALUE_LOCATE(times.qms, time0.qms) < (n-1)
;     p1 = v[0]
;  endif 
;  if arg_okay(time1, STRUCT={ABS_DATE}) then begin 
;     v = 0 > VALUE_LOCATE(times.qms, time1.qms) < (n-1)
;     p2 = v[0] 
;  endif
;  
;  nt = p2 - p1 + 1 
;  times = times[p1:p2]
;  TK = TK[*,*,p1:p2]
;  
;  
;  return, tk
;    
;end