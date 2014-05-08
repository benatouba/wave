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
;    TABLE_HEADER: if you want to put some lines at the top of the file
;
; :Author: Fabien Maussion 2014
;          Last modification: FaM, Apr 29, 2014
;
;-
pro w_write_csv, fileName, data, TABLE_HEADER=table_header, IGNORETAG=ignoretag
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  on_error, 2
  
  if ~ obj_valid(data) && ~obj_isa(data, 'ORDEREDHASH') then $
    Message, '$data must be an ORDEREDHASH'
    
  ignore = n_elements(ignoretag) ne 0
  str={}
  
  header = list()
  foreach d, data, key do begin
    if ignore then begin
      pk = where(str_equiv(ignoretag) eq str_equiv(key), cntk)
      if cntk ne 0 then continue
    endif
    if obj_valid(d) && obj_isa(d, 'list') then d = d->toArray()
    str = create_struct(str, key, d)
    header.add, key
  endforeach
  
  WRITE_CSV, fileName, str, HEADER=header->toArray(), TABLE_HEADER=table_header
  
end