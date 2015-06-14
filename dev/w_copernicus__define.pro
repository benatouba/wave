; docformat = 'rst'
;+
;
;  Inherits w_geographic to read the bullshit files:
;  http://land.copernicus.vgt.vito.be/geonetwork/srv/eng/main.home?any=a5078170-a0a7-11e0-8264-0800200c9a66
;  
;  it works ONLY for africa files.
;  
;  Maybe it is a half pixel wrong, no way to find out.
;  
;-

function w_copernicus::init, file, _EXTRA=extra

  ; Set up environnement
  @WAVE.inc
  compile_opt IDL2

  catch, theError
  if theError ne 0 then begin
    catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    return, 0
  endif


  ; Check arguments
  geo = obj_new('w_GEO_nc', FILE=file)
  if ~ obj_valid(geo) then return, 0
  self.obj = geo

  ; Original grid geoloc
  ok = geo->define_subset()
  
  ; is upside down
  self.order = 1

  ;Projection
  SetDefaultValue, datum, 'WGS-84'
  GIS_make_proj, ret, proj, PARAM='1, ' + datum
  
  ; I assume the corners are not in the center
  dx = (60d - double(string(geo->get_Gatt('LONG')))) / (geo->get_Dim('phony_dim_1'))
  dy = (40d + double(string(geo->get_Gatt('LAT')))) / (geo->get_Dim('phony_dim_0'))
  
  grid = obj_new('w_Grid2D', nx=geo->get_Dim('phony_dim_1'), $
    ny=geo->get_Dim('phony_dim_0'), $
    dx=dx, $
    dy=dy, $
    x0=double(string(geo->get_Gatt('LONG')))+dx/2, $
    y0=double(string(geo->get_Gatt('LAT')))-dy/2, $
    proj=proj, $
    meta=meta)

  ; Give the grid to the real worker here
  ok = self->w_GISdata::init(grid, _EXTRA=extra)
  if n_elements(FILEGRID) eq 0 then undefine, grid ; I instanciated this object
  if ~ ok then return, 0

  return, 1

end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_copernicus__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_copernicus                        ,  $
            INHERITS w_geographic                  $
          }
    
end