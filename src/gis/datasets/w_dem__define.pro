; docformat = 'rst'
;+
;
;  w_DEM is a basis class to read DEM in ENVI's grd format.
;  It reads the projection info frm the .hdr file and the 
;  elevation from the binary .grd files, so both must be put in the 
;  same directory for reading.
;   
;  
;  It reads the geolocalisation from the file and provides subseting 
;  tools to the user, as well as all the 'w_Grid2D' transformation tools.
;
; :History:
;     Written by FaM, 2011.
;
;-
PRO w_DEM__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {w_DEM                      ,  $
            INHERITS w_Grid2D          ,  $
            file:                ''    ,  $ ; .grd file
            hdr:                 ''    ,  $ ; .hdr file
            id:                  0L    ,  $ ; grid ID
            cropped :            ''    ,  $ ; is cropped? 'BORDER' 'SUBSET_LL' 'SUBSET_IJ' 'FALSE'
            subset : [0L,0L,0L,0L]     ,  $ ; [x0,y0,x1,y1]
            z:      PTR_NEW()             $ ; the elevation
            }
    
end

;+
; :Description:
;    Creates an instance of the DEM object
;
; :Keywords:
;       FILE: in, optional
;             the path to the file to open. If not set, a dialog window is opened
;       SUBSET_LL : in, optional, type = float vector 
;                   set to the desired subset corners to automatically subset the data.
;                   Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : in, type = long vector
;                   Four elements array:[x0,y0,x1,y1]
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the 'define_subset' method.
;                   
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84
;                   datum in which the Lat and Lons from 'SUBSET_LL' are defined
;                   
;       CROPBORDER: in, optional
;                  set this keyword to crop the grid of CROPBORDER elements on each side
;
; :Returns:
;
; :History:
;     Written by FaM, 2011.
;
;
;-
Function w_DEM::Init, FILE = file, SUBSET_LL  = subset_ll,  $
                                   SUBSET_IJ  = subset_ij,  $
                                   LL_DATUM   = ll_datum ,  $
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
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select .grd file to read', /MUST_EXIST)
  spli = STRSPLIT(file, '.', /EXTRACT)
  if str_equiv(spli[1]) ne 'GRD' then message, WAVE_Std_Message(/FILE)
  hdr = spli[0] + '.hdr'
  
  self.file = file
  self.hdr = hdr
  
  ;*********
  ; define *
  ;*********
  self.cropped = ''
  if NOT self->define_subset(SUBSET_LL = subset_ll,  $
                             SUBSET_IJ  = subset_ij,  $
                             LL_DATUM   = ll_datum ,  $
                             CROPBORDER = cropborder) THEN RETURN, 0

  RETURN, 1
  
END

;+
; :Description:
;    Redefine the DEM subset.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;       SUBSET_LL : in, optional, type = float vector 
;                   set to the desired subset corners to automatically subset the data.
;                   Format : [dl_lon, dl_lat, ur_lon, ur_lat]. (it is assumed that
;                   lons and lats are in the WGS-84 Datum if LL_DATUM is not set.)
;       SUBSET_IJ : in, type = long vector
;                   Four elements array:[x0,y0,x1,y1]
;                   Unless you know what you do, it should not be set manually but 
;                   retrieved using the 'define_subset' method.
;                   
;       LL_DATUM  : in, type = {TNT_DATUM}, default = WGS-84
;                   datum in which the Lat and Lons from 'SUBSET_LL' are defined
;                   
;       CROPBORDER: in, optional
;                  set this keyword to crop the grid of CROPBORDER elements on each side
;    
; :History:
;     Written by FaM, 2011.
;-      
function w_DEM::define_subset, SUBSET_LL  = subset_ll,  $
                               SUBSET_IJ  = subset_ij,  $
                               LL_DATUM   = ll_datum ,  $
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
    if KEYWORD_SET(CROPBORDER) then do_init = TRUE      $ ; Is not cropped yet
    else if KEYWORD_SET(SUBSET_LL) then do_init = TRUE  $  ; Is not cropped yet    
    else if KEYWORD_SET(SUBSET_IJ) then do_init = TRUE  $  ; Is not cropped yet    
    else do_init = self.cropped ne 'FALSE'   ; Is cropped but we dont want to
  endif
  
  if ~ do_init and KEYWORD_SET(SUBSET_IJ) then begin
    if ~ arg_okay(SUBSET_ij, /ARRAY, /NUMERIC, N_ELEM=4) then Message, WAVE_Std_Message('SUBSET_ij', /ARG)
    do_init = self.subset[0] ne SUBSET_IJ[0]
    if ~ do_init then do_init = self.subset[1] ne SUBSET_IJ[1]
    if ~ do_init then do_init = self.subset[2] ne SUBSET_IJ[2]
    if ~ do_init then do_init = self.subset[3] ne SUBSET_IJ[3]    
  endif
  
  if ~ do_init then return, 1 ;nothing to do
  
  PTR_FREE, self.z
  
  if KEYWORD_SET(CROPBORDER) then self.cropped = 'BORDER' $
    else if KEYWORD_SET(Subset_LL) then self.cropped = 'SUBSET_LL' $
      else if KEYWORD_SET(Subset_IJ) then self.cropped = 'SUBSET_IJ' $
        else self.cropped = 'FALSE'  
        
  ;************
  ; GRID info *
  ;************      
  ; Open DEM grid
  _quiet = !QUIET
  !QUIET = 1
  GIS_open_grid, ret, info, id, FILE=self.file, /RONLY, /NO_STC
  !QUIET = _quiet
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message(/FILE)
  
  self.id = id

  ; Check and correct TNT coordinate structure
  coord = info.coord
  coord.system_z = 'DEM'
  if ~ GIS_check_coord(coord, /CORR, ERROR=error) then begin
    GIS_close_grid, ret, id
    ret = TNT_return(DTM, TNT_S_ERROR, error)
    message, WAVE_Std_Message(/FILE)
  endif

  info.coord = coord
  map_info = utils_replace_string(utils_replace_string(coord.map_info, '{', ''), '}', '')
  proj_name = (STRSPLIT(map_info, ',', /EXTRACT))[0]
  if PROJ_NAME eq 'Geographic Lat/Lon' then PROJ_NAME = 'Geographic (WGS-84)'
  GIS_make_proj, ret, proj, NAME=proj_name
  
  ;*****************************************
  ; Convert map projection and subset data *
  ;*****************************************
  if FIRSTCALL then begin
    IF NOT self->w_Grid2D::Init(nx = coord.nx             , $
                                ny = coord.ny             , $
                                dx = coord.dx             , $
                                dy = coord.dy             , $
                                x0 = coord.x0             , $
                                y0 = coord.y0             , $
                                proj = proj               ) THEN RETURN, 0  
  endif else begin
   IF NOT self->w_Grid2D::ReInit(nx = coord.nx             , $
                                 ny = coord.ny             , $
                                 dx = coord.dx             , $
                                 dy = coord.dy             , $
                                 x0 = coord.x0             , $
                                 y0 = coord.y0             , $
                                 proj = proj               ) THEN RETURN, 0  
  endelse 
  
  ; Indexes
  isubs =[0l,0l,0l,0l]
           
  case self.cropped of
    'BORDER': begin
      if ~ arg_okay(CROPBORDER, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('CROPBORDER', /ARG)
      if cropborder lt 0 or cropborder ge coord.nx / 2 then message, 'Cropsize not ok'
      if cropborder lt 0 or cropborder ge coord.ny / 2 then message, 'Cropsize not ok'
      isubs = [cropborder, cropborder, coord.nx-1-cropborder, coord.ny-1-cropborder]
    end
    'SUBSET_IJ': begin
      isubs = SUBSET_IJ
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
      isubs = [idl,jdl,iur,jur]
    end
    else: begin
    ; nothing to do
    end
  endcase
  
  self.subset = isubs
    
  if self.cropped ne 'FALSE' then begin
  
    x0 = coord.x0 + coord.dx*isubs[0]
    y0 = coord.y0 - coord.dy*(coord.ny - (isubs[3]))
    nx = isubs[2] - isubs[0] + 1
    ny = isubs[3] - isubs[1] + 1
    
    IF NOT self->w_Grid2D::ReInit(nx = nx             , $
                                  ny = ny             , $
                                  dx = coord.dx       , $
                                  dy = coord.dy       , $
                                  x0 = x0             , $
                                  y0 = y0             , $
                                  proj = proj               ) THEN RETURN, 0  
     
  endif
      
  if ARG_PRESENT(SUBSET_ij) then SUBSET_ij = self.subset  
    
  return, 1
   
end

;+
; :Description:
;    Destroy function. 
;    
; :Categories:
;    WAVE/OBJ_GIS   
;
; :History:
;      Written by FaM, 2011.
;-
pro w_DEM::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  Ptr_Free, self.z
 
END

;+
; :Description:
;    Get access to some params
;
; :Keywords:
;    file: the grd file path
;    hdr: the hdr file path
;    _Ref_Extra: all other keywords accepted by `w_Grid2d->getProperty`
;
; :Returns:
;
; :History:
;     Written by FaM, 2011.
;
;
;-
PRO w_DEM::GetProperty            ,  $
            file = file           ,  $
            hdr = hdr             ,  $ 
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
  
  IF Arg_Present(file) NE 0 THEN file = self.file             
  IF Arg_Present(hdr) NE 0 THEN type = self.hdr             
  
  self->w_Grid2D::GetProperty, _Extra=extra
  
end

;+
; :Description:
;   Read the DEM elevation. The first call of the function
;   may be a bit slow since the DEM is read from the binary file.
;
; :Returns:
;    the DEM elevetation
;
; :History:
;     Written by FaM, 2011.
;
;-
function w_DEM::get_Z

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~PTR_VALID(self.z) then begin
    ;****************
    ; Read DEM data *
    ;****************
    GIS_read_grid, ret, self.id, z
    if TNT_err_code(ret) ne TNT_E_NONE then message, WAVE_Std_Message(/FILE)
    GIS_close_grid, ret, self.id
    if TNT_err_code(ret) ne TNT_E_NONE then message, WAVE_Std_Message(/FILE)
    z = ROTATE(z,7)
    if self.cropped ne 'FALSE' then z = z[self.subset[0]:self.subset[2],self.subset[1]:self.subset[3]]
    self.z = PTR_NEW(z, /NO_COPY)
  endif
  
  return, *self.z
    
end


;+
; :Description:
;   Get a quick look at the data 
;
; :Keywords:
;    WID: widget window ID
;    NO_LL: if no LAT LON info have to be shown (faster)
;
; :History:
;     Written by FaM, 2011.
;
;-
pro w_DEM::QuickPlotZ, WID = wid, NO_LL=NO_LL

  z = self->get_z()
  if ~KEYWORD_SET(NO_LL) then self->Get_LonLat, lon, lat
    
  w_QuickPlot, z, COLORTABLE=13, TITLE= 'Height (m)', WINDOW_TITLE='view: ' + self.file, dimnames = 'Z', CBARTITLE='meters', $
              COORDX=lon, COORDY=lat, WID = wid

end
