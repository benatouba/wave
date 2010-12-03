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
;     Last modification:  22-Nov-2010 FaM
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
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;           
; :History:
;       Written by Fam, 2009
;       Modified:   22-Nov-2010 FaM
;                   Documentation for upgrade to WAVE 0.1
;-
pro WAVE_root, root

  ; Get path
  info = routine_info('WAVE_root', /SOURCE)
  root = file_dirname(info.path, /MARK)

end