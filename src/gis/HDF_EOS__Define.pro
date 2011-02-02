;+
; 
;       HDF_EOS is the basis class for HDF_EOS files. It reads
;       HDF_EOS files and provides some tools for rapid visualisation and
;       to analyse the content of a HDF_EOS file.
;       
;       It should be the superclass from all HDF_EOS related objects 
;       
;       !!!!!!!!!!!!
;       ! Carefull !
;       !!!!!!!!!!!!     
;       Because no real need exists right now for specific EOS file handling,
;       this class is really reduced to the minimum. Still, EOS objects
;       like MODIS should implement it in case we need it in the future
;       
;       =================================================================
; :Properties:
; todo: describe properties
;                 
;          NUM_GRIDS: in, type = long                
;          NUM_POINTS: in, type = long                
;          NUM_SWATHS: in, type = long      
;          FILE: in, optional, type = string
;                the path to the HDF_EOS file. If not set, a dialog window will open
;       
;-

;-----------------------------------------------------------------------
;+
; NAME:
;       HDF_EOS__Define
;
; PURPOSE:
;       Object structure definition
;
; CATEGORY:
;       WAVE grid objects
;       
; MODIFICATION HISTORY:
;       Written by: Fabien Maussion 2010
;       Modified:   04-Nov-2010 FaM
;                   Written for upgrade to WAVE 0.1
;-
;-----------------------------------------------------------------------
;+
; :Description:
;    Defines the attributes of the class Grid2D. Attributes::
;    HDF_EOS                       
;            INHERITS HDF                  
;            NUM_GRIDS  : 0L                  
;            NUM_POINTS : 0L                   
;            NUM_SWATHS : 0L  
;             
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  09-Dec-2010 FaM
;-
PRO HDF_EOS__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = {HDF_EOS                       ,  $
            INHERITS HDF                  ,  $
            NUM_GRIDS  : 0L               ,  $        
            NUM_POINTS : 0L               ,  $          
            NUM_SWATHS : 0L                  $
            }
    
END

;+
; :Description:
;    Build function. Output: 1 if the HDF object is updated successfully, 0 if not.
;
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;    FILE: in, optional, type = string
;          the path to the HDF_EOS file. If not set, a dialog window will open
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  09-Dec-2010 FaM
;-
Function HDF_EOS::Init, FILE = file
           
           
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
  if not KEYWORD_SET(file) then begin
    file = DIALOG_PICKFILE(TITLE='Please select HDF_EOS file to read', /MUST_EXIST)
    IF file EQ "" THEN MESSAGE, WAVE_Std_Message(/FILE)
  endif
  
  ;*****************
  ; Check validity *
  ;***************** 
  if not EOS_Query(file, info) then message, WAVE_Std_Message(/FILE)
  
  IF NOT self->HDF::Init(file = file) THEN RETURN, 0  
  
  self.NUM_GRIDS = info.NUM_GRIDS
  self.NUM_POINTS =info.NUM_POINTS
  self.NUM_SWATHS = info.NUM_SWATHS
 
  RETURN, 1
  
END

;+
; :Description:
;    Get access to some params. 
;: FaM, 2010
;       Modified:   04-Nov-2010 FaM
;                 
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords: 
;    NUM_GRIDS: out, optional                     
;    NUM_POINTS: out, optional                    
;    NUM_SWATHS: out, optional           
;    _Ref_Extra: out, optional   
;                see #HDF:GetProperty#
;                
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  09-Dec-2010 FaM
;-
PRO HDF_EOS::GetProperty, $
                  NUM_GRIDS     =      NUM_GRIDS, $          
                  NUM_POINTS    =      NUM_POINTS, $   
                  NUM_SWATHS    =      NUM_SWATHS, $
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
  
  IF Arg_Present(NUM_GRIDS) NE 0 THEN NUM_GRIDS = self.NUM_GRIDS
  IF Arg_Present(NUM_POINTS) NE 0 THEN NUM_POINTS = self.NUM_POINTS
  IF Arg_Present(NUM_SWATHS) NE 0 THEN NUM_SWATHS = self.NUM_SWATHS
  
  self->HDF::GetProperty, _Extra=extra
  
end

;+
; :Description:
;    To write all infos contained in the HDF EOS file to an ASCII file.
;
; :Categories:
;         WAVE/OBJ_GIS   
;
; :Keywords:
;    FILE: in, optional, type = string
;          An optional string containing the path to the output ASCII file. If not set, a dialog window will open
;    NO_GATTS: in, optional, type = string
;              Global attributes wont be written in the ASCII file 
;    NO_VARIABLES: in, optional, type = string
;                  variable wont be written in the ASCII file 
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  09-Dec-2010 FaM
;-
PRO HDF_EOS::dump, FILE = file, NO_GATTS = no_gatts, NO_VARIABLES = no_variables

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(file) eq 0 then begin
    isPoint = STRPOS(self.fname, '.')
    if isPoint ne -1 then file = Dialog_Pickfile(/Write, File= (STRSPLIT(self.fname,'.', /EXTRACT))[0] + '_dump.txt') $
    else file = Dialog_Pickfile(/Write, File= self.fname + '_dump.txt')
  endif
  
  if file eq '' then return
  
  ; Create dump file
  OPENW, lu, file, /GET_LUN
  printf, lu, '{'
  printf, lu, ''    
  text = 'hdf EOS: ' + self.fname
  printf, lu, text
  text = 'directory : ' + self.directory
  printf, lu, text
  
  ok = EOS_Query(self.path, info)
  
  IF self.NUM_GRIDS gt 0 THEN BEGIN
    printf, lu, ' '
    printf, lu, '-------------'
    printf, lu, '* EOS GRIDS *'
    printf, lu, '-------------'
    printf, lu, ' '
    ;Go threw the grids.
    for i = 0, self.NUM_GRIDS-1 do begin  
      text = '' + info.grid_names[i] + ' '
      printf, lu, text      
    endfor ; Att OK
  ENDIF
 
  IF self.NUM_POINTS gt 0 THEN BEGIN
    printf, lu, ' '
    printf, lu, '--------------'
    printf, lu, '* EOS POINTS *'
    printf, lu, '--------------'
    printf, lu, ' '
    ;Go threw the grids.
    for i = 0, self.NUM_POINTS-1 do begin  
      text = '' + info.point_names[i] + ' '
      printf, lu, text      
    endfor ; Att OK
  ENDIF
 
  IF self.NUM_SWATHS gt 0 THEN BEGIN
    printf, lu, ' '
    printf, lu, '--------------'
    printf, lu, '* EOS SWATHS * '
    printf, lu, '--------------'
    printf, lu, ' '
    ;Go threw the grids.
    for i = 0, self.NUM_GRIDS-1 do begin  
      text = '' + info.swath_names[i] + ' '
      printf, lu, text      
    endfor ; Att OK
  ENDIF
 
  self->HDF::dump, NO_GATTS = no_gatts, NO_VARIABLES = no_variables, LUN=lu
 
  printf, lu, ''
  printf, lu, '}'
    
  close, lu ; close file
  
end