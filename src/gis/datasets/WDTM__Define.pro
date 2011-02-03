PRO WDTM__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {WDTM                      ,  $
            INHERITS Grid2D           ,  $
            file:               ''    ,  $
            hdr:                ''    ,  $ 
            z:      PTR_NEW()            $ ; the height
            }
    
end

Function WDTM::Init, FILE = file
           
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
  
  ; Open DEM grid
  !QUIET = 1
  GIS_open_grid, ret, info, id, FILE=hdr, /RONLY, /NO_STC
  !QUIET = 0
  if TNT_err_code(ret) ne TNT_E_NONE then  message, WAVE_Std_Message(/FILE)

  ; Check and correct TNT coordinate structure
  coord = info.coord
  coord.system_z = 'DEM'
  if ~ GIS_check_coord(coord, /CORR, ERROR=error) then begin
    GIS_close_grid, ret, id
    ret = TNT_return(DTM, TNT_S_ERROR, error)
    message, WAVE_Std_Message(/FILE)
  endif

  info.coord = coord

  ;****************
  ; Read DEM data *
  ;****************
  GIS_read_grid, ret, id, z
  if TNT_err_code(ret) ne TNT_E_NONE then message, WAVE_Std_Message(/FILE)
  GIS_close_grid, ret, id
  if TNT_err_code(ret) ne TNT_E_NONE then message, WAVE_Std_Message(/FILE)

  self.z =  PTR_NEW(ROTATE(z,7))


  ;*****************************************
  ; Convert map projection and subset data *
  ;*****************************************

  GIS_make_proj, ret, proj, PARAM='1, WGS-84'  
  IF NOT self->grid2D::Init( nx = coord.nx             , $
                             ny = coord.ny             , $
                             dx = coord.dx             , $
                             dy = coord.dy             , $
                             x0 = coord.x0             , $
                             y0 = coord.y0             , $
                             proj = proj               , $
                             meta = meta               , $                           
                             _Extra = extra) THEN RETURN, 0  
  
  meta = ''

  RETURN, 1
  
END

pro WDTM::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  Ptr_Free, self.z
 
END

PRO WDTM::GetProperty             ,  $
            file = file           ,  $
            hdr = hdr             ,  $ 
            z = z                 ,  $ 
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
  IF Arg_Present(z) NE 0 THEN z = *self.z      
  
  self->GRID2D::GetProperty, _Extra=extra
  
end

function WDTM::get_Z

;, x, y, src, BILINEAR = bilinear

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  z = *self.z
      
;  if ARG_PRESENT(x) and ARG_PRESENT(y) then begin
;    if ~ARG_PRESENT(src) then src = self.tnt_c
;    if KEYWORD_SET(BILINEAR) then NEAREST = false else nearest = true
;    self->transform,  x, y, i, j, SRC = src, NEAREST = NEAREST
;    if KEYWORD_SET(BILINEAR) then z = REFORM(BILINEAR(z, i, j)) else z = REFORM(z[i,j])
;  
;  endif 
    
  return, z
    
end

pro WDTM::wQuickPlotZ

  z = self->get_z()
  self->Get_LonLat, lon, lat
    
  wQuickPlot, z, COLORTABLE=13, TITLE= 'Height (m)', WINDOW_TITLE='view: ' + self.file, dimnames = 'Z', CBARTITLE='meters', $
              COORDX=lon, COORDY=lat

end
