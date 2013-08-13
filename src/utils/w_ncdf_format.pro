;+
; :Description:
;    Checks the format of a NCDF file without opening it 
;    (CLASSIC, 64BIT or NETCDF4)
;    
; :Returns:
;    A string::
;      - 'FORMAT_CLASSIC'
;      - 'FORMAT_64BIT'
;      - 'FORMAT_NETCDF4'
;      - 'Unknown'
;
; :Params:
;    filename: in, string
;              the path to the file to check
;
; :Author: Exelis Anonymous Developer
;-
function w_ncdf_format, filename

  ON_ERROR, 2

  openr, unit, filename, /get_lun
  data = bytarr(4)
  readu, unit, data
  free_lun, unit
  
  case string(data) of
    string([67b, 68b, 70b, 1b]): return, 'FORMAT_CLASSIC'
    string([67b, 68b, 70b, 2b]): return, 'FORMAT_64BIT'
    string([137b, 72b, 68b, 70b]): return, 'FORMAT_NETCDF4'
    else: return, 'Unknown'
  endcase
  
end
