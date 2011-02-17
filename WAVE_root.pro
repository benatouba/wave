; docformat = 'rst'
;+
; 
; Simple routine to catch the root directory of the WAVE library                                          
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
;     Written by FaM, 2010.
;-

;+
; :Description:
; 
; Simple routine to catch the root directory of the WAVE library                                          
;
; :Params:
;    root: out, type = string
;          The path to the WAVE library
; 
; :History:
;       Written by FaM, 2009
;-
pro WAVE_root, root

  ; Get path
  info = routine_info('WAVE_root', /SOURCE)
  root = file_dirname(info.path, /MARK)

end