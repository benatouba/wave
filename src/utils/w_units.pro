;+
; :Description:
;   Converts a CF compliant string to a string that plot routines
;   will understand. 
;   
;   Example::
;       IDL> print, w_units('kg m^-2')
;       kg m!U-2!N
;
; :Params:
;    str: in
;         the string to convert
;
; :Returns:
;    a string to pass to a graphic routine
;
;-
function w_units, str
  
  ostr = STRSPLIT(str, ' ', /EXTRACT)
  
  foreach o, ostr, i do begin
    if strpos(o, '^') ne -1 then begin
      o = utils_replace_string(o, '^', '!U') + '!N'
      ostr[i]  =o
    endif
  endforeach

  return, strjoin(ostr, ' ')

end