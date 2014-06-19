; $Id: //depot/Release/ENVI51_IDL83/idl/idldir/lib/read_csv.pro#1 $
;
; Copyright (c) 2008-2013, Exelis Visual Information Solutions, Inc. All
;       rights reserved. Unauthorized reproduction is prohibited.

;----------------------------------------------------------------------------
function w_read_csv_fieldnames, fieldCount

  compile_opt idl2, hidden

  digits_str = strtrim(string(strlen(strtrim(string(fieldCount),2))),2)
  fstr = '(i' + digits_str + '.' + digits_str + ')'
  fieldNames   = 'field' + string(lindgen(fieldCount)+1, FORMAT=fstr)

  return, fieldNames
end

; :History:
;   Written, CT, VIS, Oct 2008
;   MP, VIS, Oct 2009: Added keywords N_TABLE_HEADER and TABLE_HEADER
;   CT, VIS, Aug 2013: Use code provided by Marcos Montes to better distinguish
;                      between strings and numbers.
;                      
;  FM: added the NOCONVERT keyword
;
;-
function w_read_csv_parse, Filename, $
  COUNT=count, $
  HEADER=header, $
  MISSING_VALUE=missingValue, $
  NUM_RECORDS=numRecordsIn, $
  RECORD_START=recordStart, $
  NOCONVERT=noconvert, $
  N_TABLE_HEADER=nTableHeader, $
  TABLE_HEADER=tableHeader

  compile_opt idl2, hidden

  ON_ERROR, 2         ;Return on error

  CATCH, err
  if (err ne 0) then begin
    CATCH, /CANCEL
    if (N_ELEMENTS(lun) gt 0) then $
      FREE_LUN, lun
    if (MAX(PTR_VALID(pData)) eq 1) then $
      PTR_FREE, pData
    MESSAGE, !ERROR_STATE.msg
  endif

  header = ''

  if (N_PARAMS() eq 0) then $
    MESSAGE, 'Incorrect number of arguments.'
  
  ; Empty file
  if (FILE_TEST(filename, /ZERO_LENGTH)) then $
    return, 0

  ;Set appropriate dataStart, where dataStart includes column header.
  dataStart = keyword_set(nTableHeader) ? LONG64(nTableHeader) : 0
  
  OPENR, lun, filename, /GET_LUN

  str = ''
  tableHeader = ''
  for i=0L, dataStart-1 do begin
    READF, lun, str
    pos = stregex(str, '"') 
    if pos ne 0 then begin ; string not enclosed in quotes
      pos = stregex(str, ',+'); check for extra commas
      if pos ne -1 then str = strmid(str, 0, pos) 
    endif else begin
      ; string enclosed in commas
      pos = stregex(str, '",+') ; check for extra commas
      if pos ne -1 then str = strmid(str, 1, pos-1) else str = strjoin(strsplit(str, '"', /EXTRACT))
    endelse
    tableHeader = (i eq 0) ? str : [tableHeader, str]
  endfor
  
  READF, lun, str

  ; Skip any blank (whitespace only) lines after the header.
  ; The next good line will be stored in str.
  while (STRLEN(STRTRIM(str,2)) eq 0) do begin
    READF, lun, str
  endwhile
  
  FREE_LUN, lun
  
  ; We need to count the number of fields.
  ; First remove escaped quote characters, which look like "".
  str = STRJOIN(STRTOK(str, '""', /REGEX, /EXTRACT))
  ; Now remove quoted strings, which might contain bogus commas.
  str = STRJOIN(STRTOK(str,'"[^"]*"', /REGEX, /EXTRACT))
  ; Finally, count the number of commas.
  fieldCount = N_Elements(STRTOK(str, ',', /PRESERVE_NULL))

  fieldNames = w_read_csv_fieldnames(fieldCount)

  template = { $
    version:         1.0, $
    dataStart:       dataStart, $  ; specified as a keyword below
    delimiter:       BYTE(','), $  ; comma-separated
    missingValue:    0, $
    commentSymbol:   '', $
    fieldCount:      fieldCount, $
    fieldTypes:      REPLICATE(7L, fieldCount), $
    fieldNames:      fieldNames, $
    fieldLocations:  LONARR(fieldCount), $  ; ignored for csv
    fieldGroups:     LINDGEN(fieldCount) $  ; ungrouped
  }

  if (N_Elements(numRecordsIn)) then $
    numRecords = numRecordsIn[0] + 1

  data = READ_ASCII(filename, /CSV, $
    COUNT=count, $
    DATA_START=dataStart, $
    NUM_RECORDS=numRecords, $
    RECORD_START=recordStart, $
    TEMPLATE=template)

  if (N_TAGS(data) eq 0) then $
    MESSAGE, 'File "' + filename + '" is not a valid CSV file.', /NONAME

  ; Eliminate empty columns
  columnLen = LONARR(fieldCount)
  firstNonEmptyRow = count - 1
  lastNonEmptyRow = 0L

  for i=0L,fieldCount-1 do begin
    data.(i) = STRTRIM(data.(i), 2)
    lengths = STRLEN(data.(i))
    good = WHERE(lengths gt 0, ngood)
    if (ngood gt 0) then begin
      firstNonEmptyRow = firstNonEmptyRow < MIN(good)
      lastNonEmptyRow = lastNonEmptyRow > MAX(good)
      columnLen[i] = MAX(lengths)
    endif
  endfor

  nColumns = LONG(TOTAL(columnLen gt 0))
  
  ; All of the fields were empty.
  if (nColumns eq 0) then begin
    return, 0
  endif
  
  count = lastNonEmptyRow - firstNonEmptyRow + 1

  ; Convert each field to a pointer, for easier manipulation.
  j = 0L
  pData = PTRARR(nColumns)
  for i=0L,fieldCount-1 do begin
    if (columnLen[i] eq 0) then continue
    columnLen[j] = columnLen[i]
    pData[j] = PTR_NEW((data.(i))[firstNonEmptyRow:lastNonEmptyRow])
    j++
  endfor
  
  data = 0
  columnLen = columnLen[0:nColumns-1]

  ; Attempt to determine the data types for each field.
  types = BYTARR(nColumns)
  if (count gt 1) then begin
  
    for j=0,nColumns-1 do begin
    
      subdata = (*pData[j])[1:(100 < (count-1))]
      subdata = subdata[WHERE(subdata ne '', /NULL)]
      if (~ISA(subdata)) then continue
      subdata = STRUPCASE(subdata)
      
      ON_IOERROR, skip1

      number = '^[+-]?[0-9]*[.]?[0-9]*[DdEd]?[+-]?[0-9]*$'
      if (MIN(STREGEX(subdata, number, /BOOLEAN)) eq 0) then continue

      tmpDouble = DOUBLE(subdata)
      isDecimal = MAX(STREGEX(subdata, '[.DdEe]', /BOOLEAN))

      if (isDecimal) then begin
        types[j] = 5 ; double
      endif else begin
        tmpLong64 = LONG64(subdata)
        ; Make sure our integers are not too huge
        if (ARRAY_EQUAL(tmpLong64, tmpDouble)) then begin
          ; Now see if our integers can fit into either a long or a long64
          tmpLong = LONG(subdata)
          types[j] = ARRAY_EQUAL(tmpLong, tmpLong64) ? 3 : 14
        endif else begin
          types[j] = 5 ; double
        endelse
      endelse
      
skip1:
      ON_IOERROR, null
      
    endfor
    
    void = CHECK_MATH()

    ; Attempt to determine if the first line is a header line.
    isFirstLineText = 0
    for j=0,nColumns-1 do begin
      if (types[j] ne 0) then begin
        ON_IOERROR, skip2
        ; If we fail to convert the first item to the type for that column,
        ; then assume that it is a "string" column header.
        result = FIX((*pData[j])[0], TYPE=types[j])
        continue
skip2:
        ON_IOERROR, null
        isFirstLineText = 1
        break
      endif
    endfor
    
    nheader = isFirstLineText ? 1 : 0
    
    fieldNames = w_read_csv_fieldnames(nColumns)
    
    if (nheader gt 0) then begin
      count -= nheader
      header = STRARR(nColumns, nheader)
      for j=0,nColumns-1 do begin
        header[j,*] = (*pData[j])[0:nheader-1]
      endfor
    endif else begin
      ; If NUM_RECORDS was specified, we needed to read one extra record,
      ; in case there was a header. Since there was no header, get rid
      ; of the extra record.
      if (N_Elements(numRecordsIn)) then count--
    endelse
    
    hasMissingValue = N_Elements(missingValue) eq 1 && $
      missingValue[0] ne 0
    
    ; Do the actual type conversion.
    
    if keyword_set(noconvert) then types[*] = 7 ; force types to string
    
    for j=0,nColumns-1 do begin
    
      *pData[j] = (*pData[j])[nheader:nheader+count-1]
      
      if (types[j] ne 0) then begin
      
        if (hasMissingValue) then begin
          iMiss = WHERE(*pData[j] eq '', nmiss)
        endif
        
        ON_IOERROR, skip3
        ; Do the actual type conversion.
        *pData[j] = FIX(*pData[j], TYPE=types[j])
        
        if (hasMissingValue && nmiss gt 0) then begin
          (*pData[j])[iMiss] = missingValue[0]
        endif
skip3:
        ON_IOERROR, null
      endif
    endfor
    

  endif   ; count gt 1
  
  
  ; Create the final anonymous structure.
  data = READ_ASCII_CREATE_STRUCT(fieldNames, pData)
  
  PTR_FREE, pData
  
  return, data
end


; :Keywords:
;    COUNT
;      Set this keyword equal to a named variable that will contain the
;      number of records read.
;
;    HEADER
;      Set this keyword equal to a named variable that will contain the
;      column headers as a vector of strings. If no header exists,
;      an empty scalar string is returned.
;
;    MISSING_VALUE
;      Set this keyword equal to a value used to replace any missing
;      floating-point or integer data. The default value is 0.
;
;    NUM_RECORDS
;      Set this keyword equal to the number of records to read.
;      The default is to read all records in the file.
;
;    RECORD_START
;      Set this keyword equal to the index of the first record to read.
;      The default is the first record of the file (record 0).
;
;    N_TABLE_HEADER
;       Set this keyword to the number of lines to skip at the beginning of the file,
;       not including the HEADER line. These extra lines may be retrieved by using the TABLE_HEADER keyword.
;
;    TABLE_HEADER
;       Set this keyword to a named variable in which to return an array of strings
;       containing the extra table headers at the beginning of the file, as specified by N_TABLE_HEADER.

;+
; :Description:
;   Wrapper for the IDL builtin read_csv returning an ordered hash instead of a
;   structure.
;
; :Params:
;    Filename: in, required
;              A string containing the name of a CSV file to be read.
;
; :Keywords:
;    COUNT: out
;           the number of records read
;    HEADER: out
;            the column headers as a vector of strings
;    MISSING_VALUE: in, optional, default=0
;                    replace any missing floating-point or integer data
;    NUM_RECORDS: in, optional
;                 integer specifying the number of records to read
;    RECORD_START: in, optional
;                   integer specifying the index of the first record to read
;    N_TABLE_HEADER: in, optional
;                    number of lines of the CSV file to skip before beginning
;                    to read records from the file
;    NOCONVERT: in, optional
;               avoid automatic conversion of strings to numeric types (only
;               string arrays are returned)
;    TABLE_HEADER: out
;                   the file's table header information as a vector of strings
;    TOLIST: in, optional
;            if set, the columns are returned as lists instead of arrays
;
; :Author: Fabien Maussion 2014
;          Last modification: FaM, Jun 19, 2014
;
;-
function w_read_csv, Filename, $
  COUNT=count, $
  HEADER=header, $
  MISSING_VALUE=missing_value, $
  NUM_RECORDS=num_records, $
  RECORD_START=record_start, $
  N_TABLE_HEADER=n_table_header, $
  NOCONVERT=noconvert, $
  TABLE_HEADER=table_header, $
  TOLIST=tolist

  ; Set up environnement
  @WAVE.inc
  compile_opt IDL2
  on_error, 2

  str = w_read_csv_parse(Filename, $
    COUNT=count, $
    HEADER=header, $
    MISSING_VALUE=missing_value, $
    NUM_RECORDS=num_records, $
    RECORD_START=record_start, $
    N_TABLE_HEADER=n_table_header, $
    NOCONVERT=noconvert, $
    TABLE_HEADER=table_header)

  if n_elements(header) ne n_tags(str) then _h = tag_names(str) else _h = header

  out = orderedhash()
  for i=0, n_elements(_h)-1 do begin
    if keyword_set(TOLIST) then out[_h[i]] = list(str.(i), /EXTRACT) else out[_h[i]] = str.(i)
  endfor

  return, out

end