; docformat = 'rst'
;+
;
;  w_BlueMarble is a basis class to read image files from the NASA Blue Marble Serie
;  (http://earthobservatory.nasa.gov/Features/BlueMarble/). 
;  
;  The default world map (Low Res, Topo global) and a 2km SRTM dataset 
;  is available in the WAVE repository.
;  For further High Resolution images, go to:  
;  http://earthobservatory.nasa.gov/Features/BlueMarble/BlueMarble_monthlies.php
;  
;
; :History:
;     Written by FaM, 2011.
;
;-
PRO w_BlueMarble__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {w_BlueMarble               ,  $
            INHERITS w_Grid2D          ,  $
            path :               ''    ,  $ ; path to the file or directory
            type :               ''    ,  $ ; JPG or PNG or DIR
            cropped :            ''    ,  $ ; is cropped? 'TRUE' 'FALSE'
            subset : [0L,0L,0L,0L]     ,  $ ; [x0,y0,x1,y1]
            img:      PTR_NEW()           $ ; the img
            }
    
end

;+
; :Description:
;    Creates an instance of the object
;
; :Keywords:
;   FILE: in, optional
;         the path to another NASA jpg file to use (must be jpg)
;   SRTM: in, optional
;         if set, the default topo jpeg file from the marble is read instead of
;         the land map
;
; :Returns:
;   one if the object was created successfully
;
; :History:
;     Written by FaM, 2011.
;
;
;-
Function w_BlueMarble::Init, FILE=file, SRTM=srtm
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
      
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF 
  
  if N_ELEMENTS(FILE) eq 0 then _file = WAVE_RESOURCE_DIR+'/files/bluemarble/world.topo.200407.3x5400x2700.jpg' $
   else _file = file
  if KEYWORD_SET(SRTM) then _file = WAVE_RESOURCE_DIR+'/files/bluemarble/srtm_ramp2.world.21600x10800.jpg'
 
  if N_ELEMENTS(_file) eq 0 then _file = DIALOG_PICKFILE(TITLE='Please select bluemarble file or directory to read')
  
  type = ''
  if QUERY_JPEG(_file, Info) then type = 'JPG'
  if type eq '' and QUERY_PNG(_file, Info) then type = 'PNG'
  if FILE_TEST(_file, /DIRECTORY) then type = 'DIR'
  
  if type eq 'DIR' then Message, 'Large files currently not supported.'
  if type eq '' then Message, WAVE_Std_Message(_file, /FILE)
  
  self.type = type
  self.path = _file  

  nx = info.dimensions[0]
  ny = info.dimensions[1]
  
  x0 = -180d
  y0 = 90d
  x1 = 180d
  y1 = -90d  
  dx = double(x1-x0) / nx
  dy = double(y0-y1) / ny  
  x0 += dx/2d
  y0 -= dy/2d
  
  ;Projection
  GIS_make_proj, ret, proj, PARAM='1, WGS-84'

  RETURN, self->w_Grid2D::Init(nx = nx             , $
                               ny = ny             , $
                               dx = dx             , $
                               dy = dy             , $
                               x0 = x0             , $
                               y0 = y0             , $
                               proj = proj           ) 
  
END


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
pro w_BlueMarble::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_Grid2D::Cleanup
  Ptr_Free, self.img
 
END

;+
; :Description:
;    Get access to some params
;
; :Keywords:
;    _Ref_Extra: all other keywords accepted by `w_Grid2d->getProperty`
;
; :Returns:
;
; :History:
;     Written by FaM, 2011.
;
;
;-
PRO w_BlueMarble::GetProperty            ,  $
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
function w_BlueMarble::get_img

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if ~PTR_VALID(self.img) then begin
    if self.type eq 'JPG' then  READ_JPEG, self.path, img
    if self.type eq 'PNG' then  READ_PNG, self.path, img
    self.img = PTR_NEW(img, /NO_COPY)
  endif

  return, *self.img
    
end


