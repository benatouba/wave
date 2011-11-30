; docformat = 'rst'

;+
; :Description:
;    Defines the attributes of the class w_Grid2D. Attributes::
;
;       w_FNL
;            INHERITS w_Grid2D           
;            INHERITS w_GEO_nc         
;            type : ''
;
; :Categories:
;         WAVE/OBJ_GIS 
;         
; :History:
;     Written by FaM, 2010.
;-
PRO w_FNL__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { w_FNL                       ,  $
            INHERITS w_Grid2D            ,  $
            INHERITS w_GEO_nc               $
            }
    
END

;+
; :Description:
; 
;    Build function.
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;       FILE      : in, optional, type = string
;                   the path to the FNL file. If not set, a dialog window will open
;
; :Returns:
;    1 if the object is created successfully. 
;
; :History:
;     Written by FaM, 2011.
;-
Function w_FNL::Init, FILE = file
           
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
  IF NOT self->w_GEO_nc::Init(file = file) THEN RETURN, 0    
   
  ;*********
  ; Geoloc *
  ;*********
  ok = self->w_GEO_nc::define_subset()
  ok = utils_nc_LonLat(self.cdfid, lon_id, lat_id)  
  lon = self->w_GEO_nc::get_Var(lon_id)  
  lat = self->w_GEO_nc::get_Var(lat_id)
  nx = N_ELEMENTS(lon)
  ny = N_ELEMENTS(lat)  
  lat = REVERSE(lat)  
  
  ;Projection
  GIS_make_proj, ret, proj, PARAM='1, WGS-84'  
  meta = 'FNL data'
  IF NOT self->w_Grid2D::Init( nx = nx             , $
                               ny = ny             , $
                               dx = 1D             , $
                               dy = 1D             , $
                               x0 = lon[0]         , $
                               y0 = lat[ny-1]      , $
                               proj = proj         , $
                               meta = meta  ) THEN RETURN, 0

  ;****************
  ; Read metadata *
  ;****************    
  
  ; Read time from fname : fnl_20100301_12_00_c.nc
  tsp = self.fname
  y = LONG(STRMID(tsp,4,4))
  m = LONG(STRMID(tsp,8,2))
  d = LONG(STRMID(tsp,10,2))
  h = LONG(STRMID(tsp,13,2))
  time0 = QMS_TIME(YEAR=y, MONTH=m, DAY=d, HOUR=h)
  time1 = time0
  time = time0
  nt = 1
  
  self.t0 = time0
  self.t1 = time1
  ptr_free, self.time
  self.time = PTR_NEW(time, /NO_COPY)
  self.nt = nt
  
  ; Ok
  RETURN, 1
  
END

;+
; :Description:
;    Destroy function. 
;    
; :Categories:
;    WAVE/OBJ_GIS   
;
; :History:
;      Written by FaM, 2010.
;-
pro w_FNL::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
 
  self->w_Grid2D::Cleanup
  self->w_GEO_nc::Cleanup
      
END

;+
; :Description:
;    Get access to some params. 
;
; :Categories:
;         WAVE/OBJ_GIS 
;
; :Keywords:
;    _Ref_Extra: 
;        see 'w_GEO_nc:GetProperty' and 'w_Grid2D::GetProperty'
;        
; :History:
;     Written by FaM, 2010.
;-
PRO w_FNL::GetProperty,  _Ref_Extra=extra
    
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
;
;    This function has been redefined because the FNL files 
;    have a stupid [0, 360] format
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
;   _Ref_Extra: 
;        see 'w_GEO_nc:get_Var'
; 
; :Returns:
;         The variable
;         
; :History:
;      Written by FaM, 2010.
;-
function w_FNL::get_Var, varid, time, nt, _Ref_Extra = extra                     
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, -1
  ENDIF
     
  var = self->w_GEO_nc::get_Var(varid, time, nt, _Extra=extra, DIMS=dims)
  
  nd = N_ELEMENTS(dims)
  if nd eq 1 then return, var
  if nd eq 2 then return, ROTATE(TEMPORARY(var), 7)  
  if nd eq 3 then nz = dims[2] 
  if N_ELEMENTS(nz) eq 0 then Message, 'Dimensions???'

  for i=0, nz-1 do var[*,*,i] = ROTATE(var[*,*,i], 7)    
  return, var
  
end

pro w_FNL::QuickPlotVar, Varid, WID = wid, _EXTRA = extra
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if ~self->get_Var_Info(Varid) then Message, '$' + str_equiv(VarId) + ' is not a correct variable ID.' 
  var = self->get_Var(Varid, time, _EXTRA = extra, varname = varname, dimnames = dimnames, units = units, description=description)


  if DESCRIPTION ne '' then varname = varname + ' - ' + DESCRIPTION 
  
  self->Get_LonLat, lon, lat, nx, ny  
  
  if self.TID ge 0 and time[0] gt -1 then begin ; We found the time dimension in the file  
    p = where(dimnames eq (*self.dimNames)[self.TID], cnt)    
    if cnt ne 0 then tsrt = TIME_to_STR(time)
  endif   
  
  case (N_ELEMENTS(DIMNAMES)) of
    2: begin
       w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='w_GEO_nc view: ' + self.fname, $
        dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, WID = wid 
    end
    3: begin
       w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='w_GEO_nc view: ' + self.fname, $
         dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, dim3tags = tsrt, WID = wid
    end
    4: begin
       w_QuickPlot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='w_GEO_nc view: ' + self.fname, $
            dimnames = dimnames, CBARTITLE=units, COORDX=lon, COORDY = lat, dim4tags = tsrt, WID = wid
    end
    else: MESSAGE, 'Variable is not of suitable dimension.'
  endcase      

end
