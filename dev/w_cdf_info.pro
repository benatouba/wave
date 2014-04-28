;+
; :Description:
;    Prints info about a CDF file in the console
;
; :Params:
;    file: in, required
;          the path to the file
;
; :Author: FaM, April 2014
;-
pro w_cdf_info, file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2

  if ~ FILE_TEST(file) then Print, 'File not valid: ' + file

  id = CDF_OPEN(file, /READONLY)

  inq = CDF_INQUIRE(id)
  
  print, ' ' 
  print, 'W_CDF_INFO   ' 
  print, 'Dirname:    ' + FILE_DIRNAME(file)
  print, 'Filename:   ' + FILE_BASENAME(file)
  print, ' '
  
  print, 'NDIMS: ' + w_str(inq.NDIMS)
  print, 'NVARS: ' + w_str(inq.NVARS)
  print, 'NZVARS: ' + w_str(inq.NZVARS)
  print, 'NATTS: ' + w_str(inq.NATTS)
  print, 'DIM: ' + w_str(inq.DIM)
  
  for i=0, inq.nvars-1 do begin
    Message, 'ups not nvars ne 0 implemented yet'
  endfor
  
  for i=0, inq.natts-1 do begin
    Message, 'ups not natts ne 0 implemented yet'
  endfor
  
  print, 'ZVARS:'
  for i=0, inq.nzvars-1 do begin
    vinq = CDF_VARINQ(id, i, /ZVARIABLE)
    print, '  ZVAR   ' + w_str(i)
    print, '    NAME:   ' + vinq.name
    print, '    DIMVAR: '   + STRJOIN(w_str(vinq.DIMVAR), ',')
    print, '    DIM:    ' + STRJOIN(w_str(vinq.DIM), ',')
  endfor
  
  CDF_CLOSE, id

end