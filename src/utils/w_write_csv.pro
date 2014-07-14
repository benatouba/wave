;+
; :Description:
;   simple wrapper for the write_csv routine. Now accepts one and only one
;   data argument, which has to be an ordered hash. Keys are the headers, and
;   data['key'] must be a list or an array.
;
; :Params:
;    fileName: in, required
;              the path to the output file
;    data: in, required
;          an ordered hash (see description)
;
; :Keywords:
;    TABLE_HEADER: in, optional
;                  if you want to put some lines at the top of the file
;    IGNORETAG: in, optional
;               keys to ignore in the input hash
;    IGNORETAG: in, optional
;               keys to ignore in the input hash
;    MAXCHAR: in, optional
;             limit the number of characters printed in one column
;    FLOATFORMAT: in, optional
;                 format all floats or double to the desired format (e.g. (F5.2))
;                 this overrides /AUTOFORMAT for the floats columns in data but
;                 you can still set /AUTO for the other columns (this has the
;                 further advantage of removing blanks)
;    AUTOFORMAT: in, optional
;                a call to `w_str` is made for all values
;               
; :Author: Fabien Maussion 2014
;          Last modification: FaM, Apr 29, 2014
;
;-
pro w_write_csv, fileName, data, $
  TABLE_HEADER=table_header, $
  IGNORETAG=ignoretag, $
  MAXCHAR=maxchar, $
  FLOATFORMAT=floatformat, $
  AUTOFORMAT=autoformat
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ obj_valid(data) && ~obj_isa(data, 'ORDEREDHASH') then $
    Message, '$data must be an ORDEREDHASH'
    
  auto = keyword_set(AUTOFORMAT)
  ff = n_elements(FLOATFORMAT) ne 0
  ll = n_elements(MAXCHAR) ne 0
   
  ignore = n_elements(ignoretag) ne 0
  str={}
  
  header = list()
  foreach d, data, key do begin
    if ignore then begin
      pk = where(str_equiv(ignoretag) eq str_equiv(key), cntk)
      if cntk ne 0 then continue
    endif
    if n_elements(d) eq 0 then continue
    if n_elements(d) eq 1 && obj_valid(d) && obj_isa(d, 'list') then d = d->toArray()
    if ff && (size(d, /TNAME) eq 'FLOAT' or size(d, /TNAME) eq 'DOUBLE') then begin
      d = string(d, FORMAT=floatformat)
    endif
    if auto then d = w_Str(d)
    if ll then d = strmid(d, 0, maxchar)
    str = create_struct(str, key, d)
    header.add, key
  endforeach
  
  WRITE_CSV, fileName, str, HEADER=header->toArray(), TABLE_HEADER=table_header
  
end