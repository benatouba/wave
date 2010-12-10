PRO WRF_nc__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { WRF_nc                   ,  $
            INHERITS Grid2D           ,  $
            INHERITS NCDF             ,  $
            subset:  [0l,0l,0l,0l]    ,  $ ; if not equal to 0, it holds the indexes in the ORIGINAL ncdf grid array where to subset
            cropped:            ''    ,  $ ; 
            type:               ''    ,  $ ; type of active file: AGG, WRF, GEO, MET or INP
            version:            ''    ,  $ ; WRF version
            time0:      {ABS_DATE}    ,  $ ; first available time
            time1:      {ABS_DATE}    ,  $ ; last available time
            time:        PTR_NEW()    ,  $ ; available times
            nt:                 0L    ,  $ ; n times
            dt:                0LL    ,  $ ; time step in ms
            dom:                    0L,  $ ; id of the considered nested domain
            i_parent_start:         0L,  $ ; i index of the start point in parent domain
            j_parent_start:         0L,  $ ; j index of the start point in parent domain
            parent_grid_ratio:      0L   $ ; ratio to parent
            }
    
END

function WRF_nc::define_grid, SUBSET = subset   ,  $ ;[xo,yo,x1,y1]
                              cropD2 = cropD2   ,  $
                              cropB  = cropB  

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  ;*******************
  ; check what to do *
  ;*******************
  do_init = self.cropped eq '' ;first init
  firstcall = do_init
  if ~ do_init then begin
    if KEYWORD_SET(cropb) then do_init = self.cropped ne 'BORDER'  $ ; Is not cropped yet
    else if KEYWORD_SET(cropD2) then do_init = self.cropped ne 'DOM2'   $ ; Is not cropped yet    
    else if KEYWORD_SET(SUBSET) then do_init = self.cropped ne 'SUBSET'  $  ; Is not cropped yet    
    else do_init = self.cropped ne 'FALSE'   ; Is cropped but we dont want to
  endif
  
  if ~ do_init and KEYWORD_SET(SUBSET) then begin
    do_init = self.subset[0] ne SUBSET[0]
    if ~ do_init then do_init = self.subset[1] ne SUBSET[1]
    if ~ do_init then do_init = self.subset[2] ne SUBSET[2]
    if ~ do_init then do_init = self.subset[3] ne SUBSET[3]    
  endif
    
  if ~ do_init then return, 1 ;nothing to do
  
  if KEYWORD_SET(CROPB) then self.cropped = 'BORDER' $
    else if KEYWORD_SET(CROPD2) then self.cropped = 'DOM2' $
        else if KEYWORD_SET(Subset) then self.cropped = 'SUBSET' $
          else self.cropped = 'FALSE'  
        
  ;************
  ; GRID info *
  ;************      
  NCDF_ATTGET, self.Cdfid , 'DX', dx, /GLOBAL
  NCDF_ATTGET, self.Cdfid , 'DY', dy, /GLOBAL  
  NCDF_ATTGET, self.Cdfid , 'WEST-EAST_GRID_DIMENSION', nx, /GLOBAL
  NCDF_ATTGET, self.Cdfid , 'SOUTH-NORTH_GRID_DIMENSION', ny, /GLOBAL
  NCDF_ATTGET, self.Cdfid , 'CEN_LAT', center_lat, /GLOBAL
  NCDF_ATTGET, self.Cdfid , 'CEN_LON', center_lon, /GLOBAL
  
  dx = DOUBLE(dx)
  dy = DOUBLE(dy)
  nx = LONG(nx) -1 
  ny = LONG(ny) -1 
  
  ; Get easting and northings from dom center
  GIS_coord_trafo, ret, center_lon, center_lat, e, n, SRC=self.tnt_c.proj.datum, DST= self.tnt_c.proj    
  x0 =  - (nx-1) / 2. * dx + e ; UL corner
  y0 =    (ny-1) / 2. * dy + n ; UL corner
  
  ; Indexes in the original var
  isubs =[0l,0l,nx-1,ny-1]
          
  case self.cropped of
    'BORDER': begin
      cropSize = 5
      if cropSize lt 0 or cropSize ge nx / 2 then message, 'Cropsize not ok'
      isubs = [cropSize, cropSize, nx-1-cropSize,ny-1-cropSize]
    end
    'DOM2': begin
      file1 = self.path
      if self.dom eq 1 then GEN_str_subst, ret, file1,'d01','d02',file2 $
        else if self.dom eq 2 then GEN_str_subst, ret, file1,'d02','d03',file2 $
           else message, 'Dom is not 1 or 2, i cannot crop it'
    
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
      neli = LONG(DOUBLE(ni)/DOUBLE(ratio)) - 1
      nelj = LONG(DOUBLE(nj)/DOUBLE(ratio)) - 1
      
      isubs = [iPar-1,jPar-1,iPar-1+neli,jPar-1+nelj]    
    end
    'SUBSET': begin
       isubs = subset
    end
    else: begin
      ; nothing to do
    end
  endcase
  
  self.subset = isubs
  if self.cropped ne 'FALSE' then begin
    x0 = x0 + dx*isubs[0]
    y0 = y0 - dy*(ny - isubs[3] - 1)  
    nx = isubs[2] - isubs[0] + 1
    ny = isubs[3] - isubs[1] + 1
  endif
  
  if FIRSTCALL then begin
    IF NOT self->grid2D::Init(  nx = nx                , $
                                ny = ny                , $
                                dx = DX                , $
                                dy = DY                , $
                                x0 = x0                , $
                                y0 = y0                , $
                                proj = self.tnt_c.proj) THEN RETURN, 0  
  endif else begin
   IF NOT self->grid2D::ReInit(  nx = nx                , $
                                 ny = ny                , $
                                 dx = DX                , $
                                 dy = DY                , $
                                 x0 = x0                , $
                                 y0 = y0                , $
                                 proj = self.tnt_c.proj) THEN RETURN, 0  
  endelse
  
  return, 1
    
end


Function WRF_nc::Init        ,  $
           FILE = file       ,  $
           SUBSET = subset   ,  $
           cropD2 = cropD2   ,  $
           cropB  = cropB    
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
        
  ;******************
  ; Check arguments *
  ;******************
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select WRF file to read' ,/MUST_EXIST)
  IF NOT self->NCDF::Init(file = file) THEN RETURN, 0
  
  ;****************
  ; Read metadata *
  ;****************    
  cdfid = self.cdfid
  
  ;*****************************************
  ; Determine the type of WRF file we have *
  ;*****************************************
  NCDF_ATTGET, Cdfid , 'TITLE', title, /GLOBAL
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
  if ftype eq '' then message, WAVE_Std_Message('Init file not recognized as a known WRF product.') ;MET_EM and GEO files come later on....
  self.type = ftype
  self.version = str_equiv(title)
  meta = str_equiv(title)
  
  ;*********
  ;  TIMES *
  ;********* 
  if self.type eq 'GEO' then begin
    ntimes = 1
    times = MAKE_ABS_DATE(year = 2000, month = 01, day = 01)
  endif else begin
    NCDF_VARGET, cdfid, 'Times', stimes
    ntimes = N_ELEMENTS(stimes[0,*])
    stimes = STRING(stimes[*,0:ntimes-1])    
    ;String format : '2008-10-26_12:00:00; length 19
    times = MAKE_ABS_DATE(YEAR=STRMID(stimes,0,4), MONTH=STRMID(stimes,5,2),DAY=STRMID(stimes,8,2), $
      HOUR=STRMID(stimes,11,2),MINUTE=STRMID(stimes,14,2),SECOND=STRMID(stimes,17,2))
  endelse
  
  tstep = 0LL
  if NTIMES gt 1 then tstep = times[1].qms - times[0].qms
  self.nt = ntimes
  self.dt = tstep
  self.time0 = times[0]
  self.time1 = times[ntimes-1]
  self.time = PTR_NEW(times)
  
  ; NESTING
  if self.type eq 'GEO' or self.type eq 'MET' then attid = 'i_parent_start' else attid = 'I_PARENT_START'
  NCDF_ATTGET, Cdfid , attid, ipar, /GLOBAL
  if self.type eq 'GEO' or self.type eq 'MET' then attid = 'j_parent_start' else attid = 'J_PARENT_START'
  NCDF_ATTGET, Cdfid , attid, jpar, /GLOBAL
  if self.type eq 'GEO' or self.type eq 'MET' then attid = 'parent_grid_ratio' else attid = 'PARENT_GRID_RATIO'
  NCDF_ATTGET, Cdfid , attid, ratio, /GLOBAL
  if self.type eq 'GEO' or self.type eq 'MET' then attid = 'grid_id' else attid = 'GRID_ID'
  NCDF_ATTGET, Cdfid , attid, dom, /GLOBAL
  self.dom = dom
  self.i_parent_start = ipar
  self.j_parent_start = jpar
  self.parent_grid_ratio = ratio
  
  ; Projection
  NCDF_ATTGET, Cdfid , 'CEN_LAT', center_lat, /GLOBAL
  NCDF_ATTGET, Cdfid , 'CEN_LON', center_lon, /GLOBAL
  NCDF_ATTGET, Cdfid , 'MOAD_CEN_LAT', moad_cen_lat, /GLOBAL
  NCDF_ATTGET, Cdfid , 'STAND_LON', stand_lon, /GLOBAL
  NCDF_ATTGET, Cdfid , 'TRUELAT1', truelat1, /GLOBAL
  NCDF_ATTGET, Cdfid , 'TRUELAT2', truelat2, /GLOBAL
  NCDF_ATTGET, Cdfid , 'MAP_PROJ', proj_id, /GLOBAL 
  
  GIS_make_ellipsoid, ret, ell, NAME='WRF Sphere', RA=6370000.0, RB=6370000.0
  switch proj_id of
    1: begin
      ; 4 - Lambert Conformal Conic
      ;   a, b, lat0, lon0, x0, y0, sp1, sp2, [datum], name
      envi_proj = 4
      proj_param = str_equiv(envi_proj) + ', ' + $                      ;proj_id
                  STRING(ell.a, FORMAT='(F16.8)') + ', ' + $              ;a
                  STRING(ell.b, FORMAT='(F16.8)') + ', ' + $            ;b
                  STRING(moad_cen_lat, FORMAT='(F16.8)') + ', ' + $     ;lat0
                  STRING(stand_lon, FORMAT='(F16.8)') + ', ' + $     ;lon0
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
  if NOT self->define_grid(SUBSET = subset,  cropD2 = cropD2, cropB  = cropB) THEN RETURN, 0
    
  RETURN, 1
  
END

pro WRF_nc::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  Ptr_Free, self.time
  NCDF_CLOSE, self.cdfid

  
END

PRO WRF_nc::GetProperty,  $
    subset = subset ,  $ ; if not equal to 0, it holds the indexes in the ORIGINAL ncdf grid array where to subset
    cropped = cropped    ,  $ ;
    type = type      ,  $ ; type of active file: AGG, WRF, GEO, MET or INP
    version = version  ,  $ ; WRF version
    time0 = time0  ,  $ ; first available time
    time1 = time1   ,  $ ; last available time
    time = time   ,  $ ; available times
    nt = nt  ,  $ ; n times
    dt = dt  ,  $ ; time step in ms
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
      
  IF Arg_Present(subset) NE 0 THEN subset = self.subset
  IF Arg_Present(cropped) NE 0 THEN cropped = self.cropped
  IF Arg_Present(type) NE 0 THEN type = self.type
  IF Arg_Present(version) NE 0 THEN version = self.version
  IF Arg_Present(time0) NE 0 THEN time0 = self.time0
  IF Arg_Present(time1) NE 0 THEN time1 = self.time1
  IF Arg_Present(time) NE 0 THEN time = *self.time
  IF Arg_Present(nt) NE 0 THEN nt = self.nt
  IF Arg_Present(dt) NE 0 THEN dt = self.dt
  IF Arg_Present(dom) NE 0 THEN dom = self.dom
  IF Arg_Present(i_parent_start) NE 0 THEN i_parent_start = self.i_parent_start
  IF Arg_Present(j_parent_start) NE 0 THEN j_parent_start = self.j_parent_start
  IF Arg_Present(parent_grid_ratio) NE 0 THEN dom = self.parent_grid_ratio
  
  self->GRID2D::GetProperty, _Extra=extra
  self->NCDF::GetProperty, _Extra=extra
  
end

pro WRF_nc::get_time, time, nt, t0, t1

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  time = *self.time
  nt = self.nt
  t0 = self.time0
  t1 = self.time1
  
end

function WRF_nc::get_Var, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                          t0 = t0, $
                          t1 = t1, $
                          times = times, $
                          nt = nt, $
                          CONTI = conti, $
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
  
  mynames = true
  mydims = true
  var = self->NCDF::get_Var( Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                             varinfo = varinfo , $ ; 
                             units = units, $
                             description = description, $
                             varname = varname , $ ; 
                             dims = mydims, $ ;
                             dimnames = mynames)
                             
  dimnames = mynames
  dims = mydims
  ndims = N_ELEMENTS(mynames)
  p = where(str_equiv(mynames) eq str_equiv('time'), cnt)
  
  times = *self.time
  n = self.nt
  p1 = 0
  p2 = self.nT - 1
  
  if arg_okay(t0, STRUCT={ABS_DATE}) then begin 
     v = 0 > VALUE_LOCATE(times.qms, t0.qms) < (n-1)
     p1 = v[0]
  endif 
  if arg_okay(t1, STRUCT={ABS_DATE}) then begin 
     v = 0 > VALUE_LOCATE(times.qms, t1.qms) < (n-1)
     p2 = v[0] 
  endif
  
  nt = p2 - p1 + 1 
  times = times[p1:p2]
  
  s = self.subset
 
  ;TODO: support vertical level selection
  case cnt of
    0: begin
      
      case (NDIMS) of
        2:  begin 
          var = var[s[0]:s[2],s[1]:s[3]] 
          mydims = [s[2]-s[0]+1,  s[3]-s[1]+1]
        end
        3:  begin
          var = var[s[0]:s[2],s[1]:s[3],*]
          mydims = [s[2]-s[0]+1, s[3]-s[1]+1,mydims[2]]
        end
        else: ; do nothing
      endcase
    end
    1: begin
      case (NDIMS) of
        2:  begin
          var = var[*,p1:p2]
          mydims = [mydims[0], nt]     
        end
        3:  begin
          var = var[s[0]:s[2],s[1]:s[3],p1:p2]
          mydims = [s[2]-s[0]+1, s[3]-s[1]+1,nt]
        end
        4: begin
          var = var[s[0]:s[2],s[1]:s[3],*,p1:p2]  
          mydims = [s[2]-s[0]+1, s[3]-s[1]+1, mydims[2], nt]
        end
        else: ; do nothing
      endcase
    end
    else: message, 'Did not understand the dimension of the variable';????
  endcase
    
  if KEYWORD_SET(dims) then dims = mydims  
  if KEYWORD_SET(CONTI) then return, var[*,*,0,0] else return, var
    
end


function WRF_nc::get_ts, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                         x, $
                         y, $
                         src, $
                         t0 = t0, $
                         t1 = t1, $
                         times = times, $
                         nt = nt, $
                         wrf_ind_i = wrf_ind_i, $
                         wrf_ind_j = wrf_ind_j, $
                         wrf_lon = wrf_lon, $
                         wrf_lat = wrf_lat, $
                         wrf_x = wrf_x, $
                         wrf_y = wrf_y, $
                         CONTI = conti, $
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
  
  var = self->get_Var( Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                          t0 = t0, $
                          t1 = t1, $
                          times = times, $
                          nt = nt, $
                          CONTI = conti, $
                          varinfo = varinfo , $ ; 
                          units = units, $
                          description = description, $
                          varname = varname , $ ; 
                          dims = dims, $ ;
                          dimnames = dimnames)
                          
  ; no go threw the possibilites:
  if N_ELEMENTS(src) EQ 0 then mysrc = self else mysrc = src
  
  if N_ELEMENTS(dims) gt 3 then begin
   var = reform(var[*,*,0,*])
   message, 'Var is a 4D variable, I will take the id 0.', /INFORMATIONAL
  end
  ; This is to obtain the indexes in WRF    
  self->transform,  x, y, wrf_ind_i, wrf_ind_j, SRC = mysrc, /NEAREST 
  
  ; This is to obtain lat and lons of the WRF grid points
  self->transform, wrf_ind_i, wrf_ind_j, dummy1, dummy2, src=self, $
               LON_DST=wrf_lon, LAT_DST=wrf_lat, E_DST=wrf_x, N_DST=wrf_y
 
  ts = reform(var[wrf_ind_i,wrf_ind_j,*])
  
  return, ts
  
end

pro WRF_nc::plot_ts, Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                         x, $
                         y, $
                         src, $
                         PNG = png, $
                         t0 = t0, $
                         t1 = t1, $
                         times = times, $
                         nt = nt, $
                         wrf_ind_i = wrf_ind_i, $
                         wrf_ind_j = wrf_ind_j, $
                         wrf_lon = wrf_lon, $
                         wrf_lat = wrf_lat, $
                         wrf_x = wrf_x, $
                         wrf_y = wrf_y, $
                         CONTI = conti, $
                         varinfo = varinfo , $ ; 
                         units = units, $
                         description = description, $
                         varname = varname , $ ; 
                         dims = dims, $ ;
                         dimnames = dimnames

; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  var = self->get_ts( Varid, $ ; The netCDF variable ID, returned from a previous call to NCDF_VARDEF or NCDF_VARID, or the name of the variable. 
                         x, $
                         y, $
                         src, $
                         t0 = t0, $
                         t1 = t1, $
                         times = times, $
                         nt = nt, $
                         wrf_ind_i = wrf_ind_i, $
                         wrf_ind_j = wrf_ind_j, $
                         wrf_lon = wrf_lon, $
                         wrf_lat = wrf_lat, $
                         wrf_x = wrf_x, $
                         wrf_y = wrf_y, $
                         CONTI = conti, $
                         varinfo = varinfo , $ ; 
                         units = units, $
                         description = description, $
                         varname = varname , $ ; 
                         dims = dims, $ ;
                         dimnames = dimnames)
   
  
  WTimeLine_plot, var, times, varname, COLOR1='red', TITLE='WRF TS plot: ' + description, YTITLE=units, THICKNESS=2, /PIXMAP
  
  XYOUTS, 900, 500, 'Grid point: [' + str_equiv(STRING(wrf_ind_i, FORMAT = '(I3)')) + ',' + str_equiv(STRING(wrf_ind_j, FORMAT = '(I3)')) + ']', $
          CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color('BLUE'), /DEVICE 
  
  XYOUTS, 900, 460, 'WRF lon: ' + str_equiv(wrf_lon), $
          CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color('BLUE'), /DEVICE 
  XYOUTS, 900, 435, 'WRF lat: ' + str_equiv(wrf_lat), $
          CHARSIZE=2, CHARTHICK = 1.3, COLOR = FSC_Color('BLUE'), /DEVICE          
  
  PLO_show_img, PIXMAP=pixmap, WINDOW = window, PNG = png
  
end

function WRF_nc::type
  
  return, self.type
  
end

pro WRF_nc::quickPlotVar, Varid

  var = self->get_Var(Varid, varname = varname, dimnames = dimnames, units = units, DESCRIPTION=DESCRIPTION)
  if N_ELEMENTS(var) eq 1 and var[0] eq -1 then return
  
  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  
  nd = N_ELEMENTS(dimnames)
  
  self->Get_LonLat, lon, lat
  
  if nd eq 3 then QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='NCDF view: ' + self.fname, dimnames = dimnames, CBARTITLE=units, $
                    COORDX=lon, COORDY=lat, dim3tags=TIME_to_STR(*self.time) $
  else if nd eq 4 then QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='NCDF view: ' + self.fname, dimnames = dimnames, CBARTITLE=units, $
                    COORDX=lon, COORDY=lat, dim4tags=TIME_to_STR(*self.time) $
  else message, 'Dim of var not supported'
  
end

function WRF_nc::get_prcp, time0 = time0, time1 = time1, times, nt, units = units, STEP_WIZE = step_wize, NC = nc, C = C

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN, 0
  ENDIF 
  
  rainnc = self->get_Var('RAINNC')
  rainc = self->get_Var('RAINC')
  
  if KEYWORD_SET(C) then pcp = rainc else if KEYWORD_SET(NC) then pcp = rainnc else pcp = rainnc + rainc    
  
  times = *self.time
  
  n = self.nt
  p1 = 0
  p2 = n - 1
  
  if arg_okay(time0, STRUCT={ABS_DATE}) then begin 
     v = 0 > VALUE_LOCATE(times.qms, time0.qms) < (n-1)
     p1 = v[0]
  endif 
  if arg_okay(time1, STRUCT={ABS_DATE}) then begin 
     v = 0 > VALUE_LOCATE(times.qms, time1.qms) < (n-1)
     p2 = v[0] 
  endif
  
  nt = p2 - p1 + 1 
  times = times[p1:p2]
  pcp = pcp[*,*,p1:p2]
  
  if KEYWORD_SET(STEP_WIZE) then pcp = utils_ACC_TO_STEP(pcp)
  
  return, pcp
    
end

function WRF_nc::get_T, time0 = time0, time1 = time1, times, nt, units = units

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message()
    RETURN, 0
  ENDIF 
  
  p = self->get_Var('P')
  pb = self->get_Var('PB')
  T = self->get_Var('T') + 300.
  
  P1000MB=100000D
  R_D=287D
  CP=7*R_D/2.
 
  PI = (P / P1000MB) ^ (R_D/CP)
  TK = PI*T
  
  times = *self.time
  
  n = self.nt
  p1 = 0
  p2 = n - 1
  
  units = 'K'
  
  if arg_okay(time0, STRUCT={ABS_DATE}) then begin 
     v = 0 > VALUE_LOCATE(times.qms, time0.qms) < (n-1)
     p1 = v[0]
  endif 
  if arg_okay(time1, STRUCT={ABS_DATE}) then begin 
     v = 0 > VALUE_LOCATE(times.qms, time1.qms) < (n-1)
     p2 = v[0] 
  endif
  
  nt = p2 - p1 + 1 
  times = times[p1:p2]
  TK = TK[*,*,p1:p2]
  
  
  return, tk
    
end