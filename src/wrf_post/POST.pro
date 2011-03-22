; docformat = 'rst'
;+
; 
; This bundle of functions and procedures gives tools to the WAVE user
; to process and aggregate standard WRF output files into e.g. monthly files
; using a standard format understood by the WAVE library.
; 
; Currently, only the surface data aggregation is provided
; 
; The POST library is in development. Here is a non-
; exhaustive TODO list::
;   - Adapt the output files to the NCEP/NCAR reanalysis format
;   - Extend the diagnostic tools: unstaggering, eta to pressure levels, etc.
;   - Extend functionalities to daily means, etc.
;  
;  :Author:
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
;    This function returns the list of WRF output files recursively
;    encountered in a given directory.
;    
; :Private:
;
; :Params:
;    domain: in, required, type = integer
;            the domain id to look for 
;    directory: in, optional, type = string
;            the path to the directory to parse (if omitted, a dialog window opens).
;
; :Keywords:
;    CNT: out, optional, type =  numeric
;         the number of files found.
;         
; :History:
;       Written by FaM, 2010 
;-
function POST_list_files, domain, directory, CNT = cnt

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select directory to parse', /DIRECTORY)  
  if not FILE_TEST(directory, /DIRECTORY) then message, 'Directory is not a directory'
  
  if ~arg_okay(domain, /NUMERIC) then message, 'Domain is not set'
    
  fileList = FILE_SEARCH(directory, 'wrfout*', /EXPAND_ENVIRONMENT)
   
  isHere = STRPOS(filelist, 'd0' + str_equiv(domain))
  p = WHERE(isHere ne -1, cnt)
  if cnt ne 0 then filelist = filelist[p] else fileList = ''
    
  return, fileList[SORT(fileList)]    

end

;+
; :Description:
;       
; This function parses a standard .csv file which contains a list 
; of the WRF output variables that should be aggregated to the final
; netcdf file. Normally, you should not call this procedure but
; you can try it on your own *.csv file to test if the format
; is respected. The format is strictly restricted to 
; comma separated fields, commentar lines starting with ";"
; 
; An example of file can be found in : .../WAVE/resources/files/vars_surface_default_V32.csv
; 
; The function returns an anonymous structure of the form::
;      
;      {
;      NAME            STRING    The netcdf ID of the variables to keep in the aggregated file 
;      STATIC          BYTE      if the variable is static (non time dependent)
;      ACC             BYTE      if the variable is an accumulated field to "de-accumulate"
;      DIFF            BYTE      if the variable is a field from which the "difference to previous step" is to compute (e.g. snow height)
;      D3TOD2          BYTE      if the variable is a 3D field from which one would like to keep only the first layer (e.g. T) 
;      UNSTAG          BYTE      if the variable is on the staggered filed and need to be unstaggered (e.g. U, V) - !not used currently!
;      N_VARS          LONG      number of variables to keep
;      }
;       
; :Categories:
;    WRF/Post
;  
; :Params:
;    file:  in, required, type=str
;           The path to the file to parse.
;    vartokeep:  out, required, type=struct
;                 A structure containing the list of variables to parse as well as
;                 the action to perform on it (see the function description for 
;                 more info)
;    dimtokeep:  out, required, type=str vector
;                 The dimensions id to keep in the aggregated file
; :Returns:
;     1 if everything went fine
;           
; :Examples:
;   
; The default file is read as following::
;   
;   IDL> file = WAVE_resource_dir + '/files/post/vars_surface_default_V32.csv'
;   IDL> ok = POST_parse_Vartokeep(file, vartokeep, dimtokeep)
;   IDL> help, vartokeep, /str
;   ** Structure <2656108>, 7 tags, length=808, data length=802, refs=1:
;      NAME            STRING    Array[38]
;      STATIC          BYTE      Array[38]
;      ACC             BYTE      Array[38]
;      DIFF            BYTE      Array[38]
;      D3TOD2          BYTE      Array[38]
;      UNSTAG          BYTE      Array[38]
;      FOUND           BYTE      Array[38]
;      N_VARS          LONG                38
;   IDL> print, DIMTOKEEP
;   Time DateStrLen west_east south_north
;    
; :History:
;       Written by FaM, 2010.
;-
function POST_parse_Vartokeep, file, vartokeep, dimtokeep

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF

  if not FILE_TEST(file, /READ) then message, WAVE_Std_Message(/FILE)
  
  OPENR, fid, file, /GET_LUN  
  
  line = ''
  
  vars = ''
  static = FALSE
  acc = FALSE
  diff = FALSE
  d3tod2 = FALSE
  Unstag = FALSE
  
  c = 0L
  
  ; While there is text left, read
  WHILE ~ EOF(fid) DO BEGIN
    READF, fid, line
    line = GEN_strpack(line)
    if STRMID(line,0,1) eq ';' then continue
    spl = STRSPLIT(line , ',', /PRESERVE_NULL, /EXTRACT, count = cnt)
    if cnt lt 1 then continue
    ;Check if dimensions are here
    if str_equiv(spl[0]) eq str_equiv('DIMENSIONS') then begin
      spl = STRSPLIT(line , ',', /EXTRACT, count = cnt)
      dimtokeep = spl[1:*]     
    end
    if cnt ne 8 then continue
    if spl[1] ne '+' then continue
    
    vars = [vars,spl[0]]    
    if spl[2] eq '+' then static = [static,TRUE] else static = [static,FALSE] 
    if spl[3] eq '+' then acc = [acc,TRUE] else acc = [acc,FALSE] 
    if spl[4] eq '+' then diff = [diff,TRUE] else diff = [diff,FALSE] 
    if spl[5] eq '+' then d3tod2 = [d3tod2,TRUE] else d3tod2 = [d3tod2,FALSE] 
    if spl[6] eq '+' then Unstag = [Unstag,TRUE] else Unstag = [Unstag,FALSE] 
    
    c += 1
              
  ENDWHILE
  
  if c eq 0 then MESSAGE, WAVE_Std_Message(/FILE)
  if N_ELEMENTS(dimtokeep) lt 3 then message, 'Dimensions could probably not be read properly'
  
  vars = vars[1:*]
  static = static[1:*]
  acc = acc[1:*]
  diff = diff[1:*]
  d3tod2 = d3tod2[1:*]
  Unstag = Unstag[1:*]
  found = Unstag * 0B
  
  ; Close the files and deallocate the units:
  CLOSE, fid
  FREE_LUN, fid
  
  vartokeep = {name   : vars, $
           static : static, $
           acc    : acc, $
           diff   : diff, $
           D3toD2 : d3tod2, $
           Unstag : Unstag, $
           found  : found, $
           n_vars : c      $
           }
 
  return, 1
  
end

;+
; :Description:
;    Simple function to convert an absolute date in WRF compliant string
;
; :Params:
;    absDate:  in, required, type={ABS_DATE}/qms
;              The date in qms or abs_date format
;
; :Categories:
;    WRF/Post
; 
; :Private:
;
; :Returns:
;     a string of the same format as standard WRF files
;
; :History:
;       Written by FaM, 2010.
;-
function POST_absDate_to_wrfstr, absDate
  
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  n = N_ELEMENTS(absdate)    

  if arg_okay(absDate, /NUMERIC) then mytime = MAKE_ABS_DATE(qms = absDate)   $ 
   else if arg_okay(absDate, STRUCT={ABS_DATE}) then mytime = absDate  $
     else message, WAVE_Std_Message('ABSDATE', /ARG)
    
  for i = 0, n-1 do begin
  
   if N_ELEMENTS(sout) eq 0 then sout = '' else sout = [sout , '']
    
    if mytime[i].day lt 10 then str = '0' + STRTRIM(mytime[i].day,1) + '-' else str = STRTRIM(mytime[i].day,1)+ '-'
    if mytime[i].month lt 10 then str =  str + '0' + STRTRIM(mytime[i].month,1) + '-' else str = str + STRTRIM(mytime[i].month,1)+ '-'
  
    str = str + STRTRIM(mytime[i].year,1) + '_'
  
    if mytime[i].hour lt 10 then str =  str + '0' + STRTRIM(mytime[i].hour,1) + ':' else str = str + STRTRIM(mytime[i].hour,1)+ ':'
    if mytime[i].minute lt 10 then str =  str + '0' + STRTRIM(mytime[i].minute,1) + ':' else str = str + STRTRIM(mytime[i].minute,1)+ ':'
    if mytime[i].second lt 10 then str =  str + '0' + STRTRIM(mytime[i].second,1) else str = str + STRTRIM(mytime[i].second,1)
  
    sout[i] = str
    
  endfor
  
  return, sout
  
end


;+
; :Description:
;    Simple function to convert an absolute date in WRF filename string
;
; :Params:
;    absDate:  in, required, type={ABS_DATE}/qms
;              The date in qms or abs_date format
;
; :Categories:
;    WRF/Post
; 
; :Private:
;
; :Returns:
;     a string of the same format as standard WRF files (2003-09-01_12_00_00)
; 
; :History:
;       Written by FaM, 2010.
;-
function POST_absDate_to_wrffname, absDate
  
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  ;2003-09-01_12_00_00
  
  n = N_ELEMENTS(absdate)    

  if arg_okay(absDate, /NUMERIC) then mytime = MAKE_ABS_DATE(qms = absDate)   $ 
   else if arg_okay(absDate, STRUCT={ABS_DATE}) then mytime = absDate  $
     else message, WAVE_Std_Message('ABSDATE', /ARG)
    
  for i = 0, n-1 do begin
  
   if N_ELEMENTS(sout) eq 0 then sout = '' else sout = [sout , '']
    
    str = STRTRIM(mytime[i].year,1) + '-'
    if mytime[i].month lt 10 then str +=  '0' + STRTRIM(mytime[i].month,1) + '-' else str += STRTRIM(mytime[i].month,1)+ '-'
    if mytime[i].day lt 10 then str += '0' + STRTRIM(mytime[i].day,1) + '_' else str += STRTRIM(mytime[i].day,1)+ '_'  
    
  
    if mytime[i].hour lt 10 then str =  str + '0' + STRTRIM(mytime[i].hour,1) + '_' else str = str + STRTRIM(mytime[i].hour,1)+ '_'
    if mytime[i].minute lt 10 then str =  str + '0' + STRTRIM(mytime[i].minute,1) + '_' else str = str + STRTRIM(mytime[i].minute,1)+ '_'
    if mytime[i].second lt 10 then str =  str + '0' + STRTRIM(mytime[i].second,1) else str = str + STRTRIM(mytime[i].second,1)
  
    sout[i] = str
    
  endfor
  
  return, sout
  
end

;+;+
; :Description:
;    Simple function to convert an absolute date in WRF directory name string
;
; :Params:
;    absDate:  in, required, type={ABS_DATE}/qms
;              The date in qms or abs_date format
;
; :Categories:
;    WRF/Post
; 
; :Private:
;
; :Returns:
;     a string of the format for WRF cropped directories (2003.09.01)
; 
; :History:
;       Written by FaM, 2011.
;-
function POST_absDate_to_wrfdir, absDate
  
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  ;2003.09.01
  
  n = N_ELEMENTS(absdate)    

  if arg_okay(absDate, /NUMERIC) then mytime = MAKE_ABS_DATE(qms = absDate)   $ 
   else if arg_okay(absDate, STRUCT={ABS_DATE}) then mytime = absDate  $
     else message, WAVE_Std_Message('ABSDATE', /ARG)
    
  for i = 0, n-1 do begin
  
   if N_ELEMENTS(sout) eq 0 then sout = '' else sout = [sout , '']
    
    str = STRTRIM(mytime[i].year,1) + '.'
    if mytime[i].month lt 10 then str +=  '0' + STRTRIM(mytime[i].month,1) + '.' else str += STRTRIM(mytime[i].month,1)+ '.'
    if mytime[i].day lt 10 then str += '0' + STRTRIM(mytime[i].day,1) else str += STRTRIM(mytime[i].day,1)
  
    sout[i] = str
    
  endfor
  
  return, sout
  
end

; :Description:
;    This procedure is a low level WRF post-processing tool to copy a WRF output file
;    and remove the useless spin-up steps.
;    The newly created file will be striclty identical but with a given number 
;    of indexes removed from the Time dimension. Typically, the size of a 36H run file
;    with 12H spin-up is reduced of 30%.
;
; :Params:
;    file: in, optional, type = string
;          the file to crop. It will not be modified   
;    index: in, optional, type = integer
;          the first index (starting at 0) to keep in the output file.
;          default is 4 for wrf dommain 1 and 12 for other domains.
;    step: in, optional, type = {TIME_STEP}
;             The timestep. Default is 3 hours for the first domain, 1 hour for the
;              others.
;          
; :Keywords:
;    outdirectory: in, optional, type = string
;           the output directory. If the file already exists, it will be overwritten.
;           default is to ask the user where to put the file
;
; :History:
;       Written by FaM, 2010.
;-
pro POST_crop_file, file, index, step, OUTDIRECTORY = outdirectory

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2

 if N_ELEMENTS(file) eq 0  then file = DIALOG_PICKFILE(TITLE='Please select the file to crop.')
 if N_ELEMENTS(OUTdirectory) eq 0 then OUTdirectory = DIALOG_PICKFILE(TITLE='Please select directory where to place the output', /DIRECTORY)
 if OUTdirectory eq '' then return
 if ~FILE_TEST(OUTdirectory, /DIRECTORY) then FILE_MKDIR, OUTdirectory
  
  sid = Ncdf_open(File, /NOWRITE)    
  
  NCDF_ATTGET, sid , 'GRID_ID', dom, /GLOBAL
  
  if ~arg_okay(index, /NUMERIC) then if dom eq 1 then index = 4 else index = 12      
  if ~arg_okay(step, STRUCT={TIME_STEP}) then if dom eq 1 then step = MAKE_TIME_STEP(hour=3) else step = MAKE_TIME_STEP(hour=1)
  
  ; ------------------
  ; Rename the file
  ; ------------------    
  ;Parse names for available times wrfout_d01_2003-09-01_12_00_00
  fname = FILE_BASENAME(File)
  month = LONG(STRMID(fname,16,2))
  year = LONG(STRMID(fname,11,4))
  day = LONG(STRMID(fname,19,2))
  hour = LONG(STRMID(fname,22,2))
  ds = QMS_TIME(MONTH=month, year=year, day = day, hour = hour)
  t0 = POST_absDAte_to_wrffname(REL_TIME(ds, MILLISECOND = step.dms * index))
        
  outfile = OUTDIRECTORY + '/' + STRMID(fname,0,11) + t0 + '_24h.nc'
    
  inq = NCDF_INQUIRE(sid)
  if FILE_TEST(outfile) then FILE_DELETE, outfile
  tid = NCDF_CREATE(outfile, /CLOBBER, /NET)
  NCDF_CONTROL, tid, /FILL
    
  idTime = -1

  for i =0, inq.ndims-1 do begin
  
    NCDF_DIMINQ, sid, i, sName, sSize    
    if str_equiv(sName) eq 'TIME' then tdimid = NCDF_DIMDEF(tid, sName, /UNLIMITED) $
    else tdimid = NCDF_DIMDEF(tid, sName, ssize)

  endfor ; Dimensions OK
    
  for i =0, inq.Ngatts-1 do begin  
  
    sName = NCDF_ATTNAME(sid, i , /GLOBAL)        
    sAtt_info = NCDF_attINQ(sid, sName, /GLOBAL)
    NCDF_ATTGET, sid, sName, sValue, /GLOBAL
        
    ; Set the appropriate netCDF data type keyword.
    CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1      
    ENDCASE

    ; Add the attribute to the file.
    NCDF_AttPut, tid, sName, svalue, /GLOBAL, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
  endfor ; Att OK
  
  ; Add my own
  NCDF_AttPut, tid, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'ORIGINAL_FILE', FILE, /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'WAVE_COMENT', 'File generated with the WAVE POST_crop_file procedure to automatically remove spin-up.', /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'REMOVED_INDEXES', str_equiv(index), /GLOBAL, /CHAR
  
  for svid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,svid)
      
    ; Check the data type to see that it conforms to netCDF protocol.
    CASE StrUpCase(s_var_info.datatype) OF
      'BYTE': tbyte = 1
      'CHAR': tchar = 1
      'DOUBLE': tdouble = 1
      'FLOAT': tfloat = 1
      'LONG': tlong = 1
      'SHORT': tshort = 1
    ENDCASE
    
    ; If the dimension names are present, use them to get the dimension IDs, which are needed to define the variable.
    dimsIds = s_var_info.dim
    ; Define the variable.
    TvID = NCDF_VarDef(tid, s_var_info.name, dimsIds, $
      BYTE=tbyte, $
      CHAR=tchar, $
      DOUBLE=tdouble, $
      FLOAT=tfloat, $
      LONG=tlong, $
      SHORT=tshort)
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    
    if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
    
    ; Copy the variable attributes
    for sattid = 0, s_var_info.NATTS - 1 do begin
    
      sName = NCDF_ATTNAME(sid, svid, sattid)
      sAtt_info = NCDF_attINQ(sid, svid, sName)
      NCDF_ATTGET, sid, svid, sName, sValue
      
      ; Set the appropriate netCDF data type keyword.
      CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
      ENDCASE
      
      ; Add the attribute to the file.
      NCDF_AttPut, tid, TvID, sName, sValue,  $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort     
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    endfor
      
  endfor
  
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
 
  for vid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,vid)
    ndims = s_var_info.ndims
    
    if ndims eq 1 then NCDF_VARGET, sid, vid, var, OFFSET=index $
     else if ndims eq 2 then NCDF_VARGET, sid, vid, var, OFFSET=[0,index] $
       else if ndims eq 3 then NCDF_VARGET, sid, vid, var, OFFSET=[0,0,index]$
          else if ndims eq 4 then NCDF_VARGET, sid, vid, var, OFFSET=[0,0,0,index]
    
    NCDF_VARPUT, tid, vid, var

  endfor  
  
  NCDF_CLOSE, sid ; Close source file
  NCDF_CLOSE, tid ; Close file
  
end

pro POST_cpy_crop_directory, input_dir = input_dir, output_dir = output_dir

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if N_ELEMENTS(input_dir) eq 0 then input_dir = DIALOG_PICKFILE(TITLE='Please select directory to copy-crop', /DIRECTORY)  
  if N_ELEMENTS(output_dir) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output directory', /DIRECTORY)  
  
  if not FILE_TEST(input_dir, /DIRECTORY) then message, 'input_dir is not a directory'
  if not FILE_TEST(output_dir, /DIRECTORY) then FILE_MKDIR, output_dir
  
  input_dir =  FILE_DIRNAME(input_dir, /MARK_DIRECTORY) +  FILE_BASENAME(input_dir)
  output_dir =  FILE_DIRNAME(output_dir, /MARK_DIRECTORY) +  FILE_BASENAME(output_dir)
      
  fileList = FILE_SEARCH(input_dir, 'wrfout*', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT)
  GEN_str_subst, ret, fileList, input_dir, '', arbo
  arbo = FILE_DIRNAME(arbo)
  arboU = arbo[SORT(arbo)]
  OarboU = arboU[UNIQ(arboU)]
  
  OPENW, unit, output_dir + '/wrf_cpy_crop.log', /GET_LUN
  
  printf, unit, '' 
  printf, unit, 'WRF output copy cropping' 
  printf, unit, ''
  printf, unit, 'Start : ' + TIME_to_STR(QMS_TIME())
  
  printf, unit, ''
  printf, unit, 'Number of files to copy: ' + str_equiv(N_ELEMENTS(fileList))
  
  if N_ELEMENTS(fileList) eq 0 then begin
    printf, unit, ' '
    printf, unit, 'Nothing to do.'
    printf, unit, ' '
    printf, unit, ' '
    printf, unit, '------------'
    printf, unit, '* SUCCESS * '
    printf, unit, '------------'
    printf, unit, ' '    
    printf, unit, 'End   : ' + TIME_to_STR(QMS_TIME())
    printf, unit, ' '    
    close, unit ; close log file
    return
  endif
  
  printf, unit, ''
  printf, unit, 'Make directories:'
  
  for i=0, N_ELEMENTS(arboU) - 1 do begin
    sid = Ncdf_open(fileList[i], /NOWRITE)      
    NCDF_ATTGET, sid , 'GRID_ID', dom, /GLOBAL  
    if dom eq 1 then index = 4 else index = 12      
    if dom eq 1 then step = MAKE_TIME_STEP(hour=3) else step = MAKE_TIME_STEP(hour=1)
    NCDF_CLOSE, sid
    fname = FILE_BASENAME(fileList[i])
    month = LONG(STRMID(fname,16,2))
    year = LONG(STRMID(fname,11,4))
    day = LONG(STRMID(fname,19,2))
    hour = LONG(STRMID(fname,22,2))
    ds = QMS_TIME(MONTH=month, year=year, day = day, hour = hour)    
    new_dir = FILE_DIRNAME(arboU[i]) + '/' + POST_absDAte_to_wrfdir(REL_TIME(ds, MILLISECOND = step.dms * index))
    new_dir = FILE_DIRNAME(output_dir + '/' + new_dir, /MARK_DIRECTORY) +  FILE_BASENAME(output_dir + '/' + new_dir)
    if dom eq 1 then begin
      FILE_MKDIR, new_dir
      printf, unit, ': ' + arboU[i]
      printf, unit, '  ---> ' + new_dir
    endif
    arboU[i] = new_dir
  endfor 
   
  printf, unit, ''
  printf, unit, 'Ok. Lets start to copy everything but the WRF files.'
  printf, unit, ''
  printf, unit, '...'
  printf, unit, ''
  flush, unit  
  FarboU = arboU[UNIQ(arboU)]
  for i=0, N_ELEMENTS(OarboU) - 1 do begin
    files_tocpy = FILE_SEARCH(input_dir + '/' + OarboU[i],'*' ,/EXPAND_ENVIRONMENT, count = cnttocpy)
    if cnttocpy ne 0 then begin
      for j = 0, cnttocpy - 1 do begin
        filetocpy = files_tocpy[j]
        if str_equiv(STRMID(FILE_BASENAME(filetocpy),0,6)) ne str_equiv('wrfout') then begin
          ntr = N_ELEMENTS(byte(input_dir + '/' + OarboU[i]))
          dest = FarboU[i] + '/' + STRMID(filetocpy, ntr, N_ELEMENTS(byte(filetocpy)) - ntr)
          dest =  FILE_DIRNAME(dest, /MARK_DIRECTORY) +  FILE_BASENAME(dest)
          FILE_COPY, filetocpy, dest, /RECURSIVE, /OVERWRITE
          printf, unit, ': ' + filetocpy
          printf, unit, '  ---> ' + dest
        endif
      endfor
    endif
  endfor
  
  printf, unit, ''
  printf, unit, 'Ok. Lets start copy-cropping stuff: '
  printf, unit, ''
  flush, unit  
  
  indstart = 0
  inpb = 0
  tried = 0 
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    printf, unit, '  Oups, error : ' + !ERROR_STATE.MSG + ' . Trying to restart here.'
    message, /RESET        
    indstart = inpb
    tried += 1
    if tried gt 100 then begin
      printf, unit, '  Tried everything. Stop the massacre...'
      flush, unit  
      return
    endif
  ENDIF
    
  for i=indstart, N_ELEMENTS(fileList) - 1 do begin
    inpb = i
    printf, unit, '  Start : ' + fileList[i] + ' ... '
    flush, unit
    syst = QMS_TIME()
    POST_crop_file, fileList[i], OUTDIRECTORY = arboU[i] + '/'
    tott = MAKE_TIME_STEP(DMS=(QMS_TIME()-syst))
    printf, unit, '  ... Done: ' + str_equiv(tott.hour) + ' hrs, ' + str_equiv(tott.minute) + ' mns, ' + str_equiv(tott.second) + ' secs.'
  endfor
  
  printf, unit, ' '
  printf, unit, ' '
  printf, unit, '------------'
  printf, unit, '* SUCCESS * ' 
  printf, unit, '------------'
  printf, unit, ' ' 

  printf, unit, 'End   : ' + TIME_to_STR(QMS_TIME())
  printf, unit, ' '
 
  close, unit ; close log file  
  FREE_LUN, unit
  
end

;+
; :Description:
;    This procedure fills a target WRF aggregation file, given 
;    a list of file to parse and a list of variable to copy.
;
; :Categories:
;    WRF/Post
; 
; :Private:
; 
; :Params:
;    unit: in, required
;         the log file unit
;    tid: in, required
;         the netCDF target file ID
;    filelist: in, required
;              the list of files paths to parse
;    vartokeep: in, required
;              the list of variable to copy
;    ts: in, required
;        the timeserie of the final aggregate file
;    spin_index: in, required
;                where to start the copy in the original files
;    e_index: in, required
;                where to end the copy in the original files
;                
; :History:
;       Written by FaM, 2010.
;-
pro POST_fill_ncdf, unit, tid, filelist, vartokeep, ts, spin_index, e_index 
  
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
;  ON_ERROR, 2
  
  nvars = vartokeep.n_vars  
  nfiles = N_ELEMENTS(filelist)
  nt = N_ELEMENTS(ts)
  cert = LONARR(nt) ; To check if everything is filled
  
  for f=0, nfiles-1 do begin
  
    fid = NCDF_OPEN(filelist[f], /NOWRITE)
    ginq = NCDF_INQUIRE(fid)
    ;Go threw the dimensions. 
    for i =0, ginq.ndims-1 do begin
      NCDF_DIMINQ, fid, i, sName, sSize
      if N_ELEMENTS(dimSizes) eq 0 then dimSizes = sSize else dimSizes=[dimSizes,sSize]
    endfor ; Dimensions OK
    
    NCDF_VARGET, fid, 'Times', stimes
    ntimes = N_ELEMENTS(stimes[0,*])
    stimes = STRING(stimes[*,0:ntimes-1])
    ;String format : '2008-10-26_12:00:00; length 19
    times = QMS_TIME(YEAR=STRMID(stimes,0,4), MONTH=STRMID(stimes,5,2),DAY=STRMID(stimes,8,2), $
      HOUR=STRMID(stimes,11,2),MINUTE=STRMID(stimes,14,2),SECOND=STRMID(stimes,17,2))
      
    if spin_index lt 0 or spin_index ge ntimes then message, WAVE_Std_Message('$SPINUP_INDEX', /RANGE)
    if e_index lt spin_index or e_index ge ntimes then message, WAVE_Std_Message('$END_INDEX', /RANGE)
       
    ps = where(ts eq times[spin_index], cnt)
    if cnt eq 0 then begin
     if (times[spin_index] lt ts[0]) then begin
       ps = 0
       my_spin_index =  where(times eq ts[ps], cnt)  
       if cnt eq 0 then continue  
     endif else continue
    endif else my_spin_index = spin_index
       
    pe = where(ts eq times[e_index], cnt)    
    if cnt eq 0 then begin
     if (times[ntimes-1] gt ts[nt-1]) then begin
       pe = nt-1
       my_e_index =  where(times eq ts[pe], cnt)  
       if cnt eq 0 then continue  
     endif else continue
    endif else my_e_index = e_index
    
    for v=0, nvars - 1 do begin
    
      vid = vartokeep.name[v]
      if ~vartokeep.found[v] then continue ; was not in the file
      if vartokeep.Unstag[v] then continue ; not implemented
      
      s_var_info = NCDF_VARINQ(fid,vid)
      ndims = s_var_info.ndims
      dims = s_var_info.dim
      
      if vartokeep.acc[v] then begin
        if ndims eq 3 then NCDF_VARGET, fid, vid, var, OFFSET=[0,0,(my_spin_index-1)], COUNT=[dimSizes[dims[0]],dimSizes[dims[1]], my_e_index + 2 - my_spin_index] $
              else message, 'Impossible'
         var = utils_ACC_TO_STEP(var)
         vid += '_STEP'
         NCDF_VARPUT, tid, vid, var[*,*,1:*], OFFSET=[0,0,0,ps]
      endif else if vartokeep.diff[v] then begin
        if ndims eq 3 then NCDF_VARGET, fid, vid, var, OFFSET=[0,0,(my_spin_index-1)], COUNT=[dimSizes[dims[0]],dimSizes[dims[1]], my_e_index + 2 - my_spin_index] $
              else message, 'Impossible'         
         NCDF_VARPUT, tid, vid, var[*,*,1:*], OFFSET=[0,0,0,ps] ; Normal variable
         var = utils_ACC_TO_STEP(var)
         vid += '_DIFF'
         NCDF_VARPUT, tid, vid, var[*,*,1:*], OFFSET=[0,0,0,ps] ; Diff variable
      endif else if vartokeep.static[v] then begin
          if f eq 0 then begin ; We need to do it just one time...
          if ndims eq 3 then NCDF_VARGET, fid, vid, var, OFFSET=[0,0,my_spin_index], COUNT=[dimSizes[dims[0]],dimSizes[dims[1]], 1] $
            else message, 'Impossible'
          NCDF_VARPUT, tid, vid, var
          printf, unit, '   Static field ' + vid + ' filled.'
          flush, unit  
          endif
      endif else if vartokeep.D3toD2[v] then begin
          if ndims eq 4 then NCDF_VARGET, fid, vid, var, OFFSET=[0,0,0,my_spin_index], COUNT=[dimSizes[dims[0]],dimSizes[dims[1]], 1, my_e_index - my_spin_index + 1] $
            else message, 'Impossible'
          NCDF_VARPUT, tid, vid, reform(var), OFFSET=[0,0,ps]
      endif else begin
        if ndims eq 1 then begin
          NCDF_VARGET, fid, vid, var, OFFSET=[my_spin_index], COUNT=[my_e_index - my_spin_index + 1]
          NCDF_VARPUT, tid, vid, var, OFFSET=[ps]
        endif else if ndims eq 2 then begin
          NCDF_VARGET, fid, vid, var, OFFSET=[0,my_spin_index], COUNT=[dimSizes[dims[0]], my_e_index - my_spin_index + 1]
          NCDF_VARPUT, tid, vid, var, OFFSET=[0,ps]
        endif else if ndims eq 3 then begin
          NCDF_VARGET, fid, vid, var, OFFSET=[0,0,my_spin_index], COUNT=[dimSizes[dims[0]],dimSizes[dims[1]], my_e_index - my_spin_index + 1]
          NCDF_VARPUT, tid, vid, var, OFFSET=[0,0,ps]
        endif else if ndims eq 4 then begin
          NCDF_VARGET, fid, vid, var, OFFSET=[0,0,0,my_spin_index], COUNT=[dimSizes[dims[0]],dimSizes[dims[1]],dimSizes[dims[2]], my_e_index - my_spin_index + 1]
          NCDF_VARPUT, tid, vid, var, OFFSET=[0,0,0,ps]
        endif else message, 'Impossible'        
      endelse  
      cert[ps:pe] += 1 ; allright     
      
    endfor
    
    printf, unit, FILE_BASENAME(filelist[f]) + ' processed.'
    flush, unit
    NCDF_CLOSE, fid
  endfor
  
  dummy = where(VARTOKEEP.found eq TRUE, pnvars)
  pcert = WHERE(cert ne pnvars, cnt)
  if cnt ne 0 then begin
   
   message, 'The desired time serie is not complete: files are missing in the parsed directory.', /INFORMATIONAL
   printf, unit, ''
   printf, unit, '!!! Caution !!!'
   printf, unit, 'Times missing in the Time Serie: '
   for i=0, cnt-1 do printf, unit, '  ', TIME_to_STR(ts[pcert[i]])
   printf, unit, ''
   
  endif
  printf, unit, ''
  printf, unit, 'Retrieve the accumulated variables... '
  
  ; Go threw the de-accumulated variables
  da = WHERE(vartokeep.acc, ca)
  for i = 0L, ca-1 do begin
    vid = vartokeep.name[da[i]]
    NCDF_VARGET, tid, vid + '_STEP', var 
    ndims = SIZE(var, /N_DIMENSIONS)
    varsiz = SIZE(var, /DIMENSIONS)  
    var = TOTAL(var, 3, /CUMULATIVE) ; we have to deaccumulate
    NCDF_VARPUT, tid, vid, var
    printf, unit, '  ' + vid + ' processed. '
    flush, unit
  endfor

  printf, unit, ''
  printf, unit, 'Full.'
  printf, unit, ''

end

;+
; :Description:
;    This is the most important aggregation function. It will parse all files
;    recursively in a directory and copy the data into an aggregate output,
;    which time period and variables are choosen by the user.
;    
;    The desired output period (month, season, year...) is indicated by the user trough
;    the keywords 'START_TIME' and 'END_TIME'. The variables that have to be aggregated
;    are given using standard file formats (see 'POST_parse_Vartokeep'). Default file
;    is the "surface" file in the ./res/files/post directory.
;    
;    Be carefull to choose the directory(s) circumspectly. The chosen directories must contain:
; 
;       1. WRF output files that have a standard file name (the routine looks for the "wrfout" and "d0" patterns)
;     
;       2. At least enough files to fill up the chosen output period. Default behavior is to throw an error 
;        if the final time serie is not complete
;        
;       3. Not too many files: the routine will open all files to check if their time matches the output
;        period and this might be long.
;    
;    The output files have a preformatted name: wrf_agg_YYYY_MM_DD_d0x.nc, where YYYY_MM_DD is the first available
;    day in the time serie (start_time=01.06.2010 01:00:00 and domain 1 will create the file: wrf_agg_2009_06_01_d01.nc)
;   
;    
; :Params:
;    domain: in, required, type = integer
;            The domain id. No default.
;    directory: in, required, type = string
;               The directory to parse recursively. If omitted, a dialog window is opened.
; :Keywords:
;    OUTDIRECTORY: in, required, type = string
;              The output directory where to put the aggregated file. See the descripition for 
;              the output file format.
;    START_TIME: in, optional, type = {ABS_DATE}
;                The output start time. The default is to check the first alphabetical file name,
;                and add the spin index multiplied by the timestep (e.g: wrfout_d01_2009-05-31_12_00_00 
;                would lead to start_time = 01.06.2010 03:00:00. wrfout_d02_2009-05-31_12_00_00 to a 
;                start_time = 01.06.2010 01:00:00)
;    END_TIME: in, optional, type = {ABS_DATE}
;                The output end time. The default is to check the last alphabetical file name,
;                and add 36H (e.g: wrfout_d01_2009-06-29_12_00_00 would lead to 
;                end_time = 01.07.2010 00:00:00.)
;    SPINUP_INDEX: in, optional, type = integer
;                  The index where to start to get the data in each file to aggregate. Default is
;                  5 for the first domain, 13 for the other domains. 
;    END_INDEX: in, optional, type = integer
;               The index where to stop to get the data in each file to aggregate. Default is
;               12 for the first domain, 36 for the other domains. 
;    TIMESTEP: in, optional, type = {TIME_STEP}
;              The output time serie timestep. Default is 3 hours for the first domain, 1 hour for the
;              others.
;    VARTOKEEP_FILE: in, optional, type = string
;              The path to the variable file to parse (see 'POST_parse_Vartokeep').
;              
; :History:
;       Written by FaM, 2010 
;-
pro POST_aggregate_directory, domain, directory, START_TIME = start_time, END_TIME = end_time, TIMESTEP = timestep, SPINUP_INDEX = spinup_index, END_INDEX = end_index, OUTDIRECTORY = OUTdirectory, VARTOKEEP_FILE = vartokeep_file

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    if N_ELEMENTS(unit) ne 0 then begin
;      printf, unit, ' '
;      printf, unit, ' '
;      printf, unit, '* ERROR : ' + !Error_State.Msg
;      printf, unit, ' '
;      printf, unit, ' '
;      close, unit
;      free_lun, Unit
;    endif
;    ok = WAVE_Error_Message(!Error_State.Msg)
;    RETURN
;  ENDIF 
  
  ; ---------------
  ; Check the input
  ; ---------------
  
  fileLIST = POST_list_files(domain, directory, CNT = cnt)  
  if cnt eq 0 then Message, 'Either $domain or $directory are not set properly.'
  
  if ~KEYWORD_SET(SPINUP_INDEX) then begin
   if domain eq 1 then spin_index = 5 else spin_index = 13
  endif else spin_index = SPINUP_INDEX
  
  if N_ELEMENTS(TIMESTEP) eq 0 then begin
    if domain eq 1 then step = MAKE_TIME_STEP(hour=3) else step = MAKE_TIME_STEP(hour=1)
  endif else begin 
    if ~arg_okay(TIMESTEP, STRUCT={TIME_STEP}) then Message, WAVE_Std_Message('timestep', STRUCT = {TIME_STEP}) 
    step = TIMESTEP
  endelse
  
  if N_ELEMENTS(OUTdirectory) eq 0 then OUTdirectory = DIALOG_PICKFILE(TITLE='Please select directory where to place the output', /DIRECTORY)
  if OUTdirectory eq '' then return
  if ~FILE_TEST(OUTdirectory, /DIRECTORY) then FILE_MKDIR, OUTdirectory
  
  if N_ELEMENTS(VARTOKEEP_FILE) eq 0 then VARTOKEEP_FILE = WAVE_resource_dir + '/files/post/vars_surface_default_V32.csv'
  ok = POST_parse_Vartokeep(VARTOKEEP_FILE, vartokeep, dimtokeep)
  if ~ok then return
  
  ; ---------------------
  ; Create the time serie
  ; ---------------------
    
  ;Parse names for available times wrfout_d01_2008-10-26_12:00:00
  fnames = FILE_BASENAME(fileLIST)
  month = LONG(STRMID(fnames,16,2))
  year = LONG(STRMID(fnames,11,4))
  day = LONG(STRMID(fnames,19,2))
  hour = LONG(STRMID(fnames,22,2))
  ds = QMS_TIME(MONTH=month, year=year, day = day, hour = hour)
  dummy = min(ds, pmin, SUBSCRIPT_MAX=pmax)
  
  myt0 = REL_TIME(ds[pmin], MILLISECOND = step.dms * spin_index)
  if ~KEYWORD_SET(END_INDEX) then begin
   if domain eq 1 then e_index = 12 else e_index = 36
  endif else e_index = END_INDEX
  myt1 = REL_TIME(ds[pmax], MILLISECOND = step.dms * e_index)

  if ~KEYWORD_SET(START_TIME) then t0 = myt0 else begin
    if ~check_WTIME(START_TIME, OUT_QMS=t0) then Message, WAVE_Std_Message('START_TIME', /ARG)
    if myt0 ne t0 then Message, 'My start time (' + TIME_to_STR(myt0) +') and your start time (' + TIME_to_STR(t0) + $
                                    ') do not match. Taking yours.', /INFORMATIONAL
  endelse
  
  if ~KEYWORD_SET(END_TIME) then t1 = myt1 else begin
    if ~check_WTIME(END_TIME, OUT_QMS=t1) then Message, WAVE_Std_Message('END_TIME', /ARG)
    if myt1 ne t1 then Message, 'My end time (' + TIME_to_STR(myt1) +') and your end time (' + TIME_to_STR(t1) + $
                                    ') do not match. Taking yours.', /INFORMATIONAL
  endelse
  
  ; Ok. Lets go
  ts = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=step, /QMSTIME)
  nt = N_ELEMENTS(ts)
  
  ; For log purposes
  syst = QMS_TIME()  
  
  ; -------------------
  ; Create the log file
  ; -------------------
  str = TIME_to_STR(t0) ; 22.10.2008 03:00:00
  str = strmid(str,6,4) + '_' + strmid(str,3,2) + '_' + strmid(str,0,2) + '_d0' + str_equiv(domain)
  OPENW, unit, OUTdirectory + '/wrf_cpy_'+ str + '.log', /GET_LUN
  
  text = 'WRF output aggregation'
  printf, unit, text 
  text = 'Start date : ' + TIME_to_STR(ts[0])
  printf, unit, text
  text = 'End   date : ' + TIME_to_STR(ts[nt-1])  
  printf, unit, text 
  
  ; Open the file you just created and copy the information in it to another file.
  sourceFile = fileLIST[0]
  destFile = OUTdirectory + '/wrf_agg_' + str + '.nc'
  
  printf, unit, ''
  text = 'Destination file : ' + destFile 
  printf, unit, text   
  
  ; ---------------------------
  ; Create the Netcdf out file
  ; ---------------------------
  sid = Ncdf_open(sourceFile, /NOWRITE)
  inq = NCDF_INQUIRE(sid)
  if FILE_TEST(destFile) then FILE_DELETE, destFile, /VERBOSE
  tid = NCDF_CREATE(destFile, /CLOBBER, /NET)
  NCDF_CONTROL, tid, /FILL ;We gonna define all this
  
  printf, unit, ''
  printf, unit, '---------------'
  printf, unit, '* Dimensions * ' 
  printf, unit, '---------------'
  printf, unit, ' '  

  ;Go threw the dimensions.
  t_dim_names = ''
  t_dim_ids = 0
  t_s_dim_ids = 0
  for i =0, inq.ndims-1 do begin  
    NCDF_DIMINQ, sid, i, sName, sSize     
    if N_ELEMENTS(s_dim_names) eq 0 then s_dim_names = sName else s_dim_names=[s_dim_names,sName]       
    p = where(str_equiv(dimtokeep) eq str_equiv(sName), cnt)    
    if cnt eq 0 then begin ;Nothing to be done
      printf, unit, '  -:', sName, ': ', STR_equiv(sSize), ' (ignored)'
    endif else begin
      if str_equiv(sName) eq 'TIME' then ssize = nt ; No more "infinite dimension"
      if (str_equiv(sName) eq 'TIME') and i ne 0 then MESSAGE, 'Time HAS to be the dimension 0.'      
      tdimid  = NCDF_DIMDEF(tid, sName, ssize)         
      t_dim_ids = [t_dim_ids, tdimid]
      t_dim_names = [t_dim_names, sName]
      t_s_dim_ids = [t_s_dim_ids, i]
      printf, unit, '  +:', sName, ': ', STR_equiv(sSize)
    endelse
  endfor ; Dimensions OK
  t_dim_names = t_dim_names[1:*]
  t_dim_ids = t_dim_ids[1:*]
  t_s_dim_ids = t_s_dim_ids[1:*]
  
  printf, unit, ' '
  printf, unit, ' '
  printf, unit, '----------------------'
  printf, unit, '* Global attributes * ' 
  printf, unit, '----------------------'
  printf, unit, ' '
  
  ;Go threw the global Attribute. No need to change a lot but add some info  
  removAbleAtts = ['SIMULATION_START_DATE','JULYR','JULDAY', 'CREATION_DATE', 'ORIG_FILE', 'WAVE_COMENT', 'REMOVED_INDEXES']  
  
  for i =0, inq.Ngatts-1 do begin  
  
    sName = NCDF_ATTNAME(sid, i , /GLOBAL)        
    sAtt_info = NCDF_attINQ(sid, sName, /GLOBAL)
    NCDF_ATTGET, sid, sName, sValue, /GLOBAL
    
    ; go threw the atts we want to remove
    isHere = WHERE(removAbleAtts eq str_equiv(sName), cnt)
    if cnt ne 0 then begin
      printf, unit, '  -:' + StrlowCase(sName) + ': ' + str_equiv(sValue), ' (ignored)'
      continue
    endif
    ; go threw the atts we want to change
    if str_equiv(sName) eq 'START_DATE' then sValue = POST_absDAte_to_wrfstr(ts[0])
    
    ; Set the appropriate netCDF data type keyword.
    CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1      
    ENDCASE

    ; Add the attribute to the file.
    NCDF_AttPut, tid, sName, svalue, /GLOBAL, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort

    printf, unit, '  +:' + StrlowCase(sName) + ': ' + str_equiv(sValue)
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
  endfor ; Att OK
    
  ; Add my own
  NCDF_AttPut, tid, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), /GLOBAL, /CHAR
  printf, unit, '  +:' + 'CREATION_DATE' + ': ' + TIME_to_STR(QMS_TIME()) + ' (added)'
  NCDF_AttPut, tid, 'WAVE_COMENT', 'File generated by aggregation of WRF single runs using the WAVE POST_aggregate_directory routine.', /GLOBAL, /CHAR
  printf, unit, '  +:' + 'WAVE_COMENT' + ': ' + 'File generated by aggregation of WRF single runs using the WAVE POST_aggregate_directory routine.' + ' (added)'
  NCDF_AttPut, tid, 'SPINUP_INDEX', spin_index, /GLOBAL, /LONG
  printf, unit, '  +:' + 'SPINUP_INDEX' + ': ' + str_equiv(spin_index) + ' (added)'
  NCDF_AttPut, tid, 'END_INDEX', e_index, /GLOBAL, /LONG
  printf, unit, '  +:' + 'END_INDEX' + ': ' + str_equiv(e_index) + ' (added)'
  
  printf, unit, ' '
  printf, unit, ' '
  printf, unit, '--------------'
  printf, unit, '* Variables * ' 
  printf, unit, '--------------'
  printf, unit, ' '
 
  for svid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,svid)
    dimsIds = s_var_info.dim
    
    p = where(str_equiv(vartokeep.name) eq str_equiv(s_var_info.name), cnt)
    if cnt eq 0 then begin
      text = '  -:' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(s_var_info.name) + '('
      for i =0, N_ELEMENTS(dimsIds) - 1 do begin
        text += STRLOWCASE(str_equiv(s_dim_names[dimsIds[i]]))
        if i ne N_ELEMENTS(dimsIds) - 1 then  text += ','
      endfor
      text += ')  ; (ignored)'
      printf, unit, text
      continue ;Nothing to be done
    endif
    VARTOKEEP.found[p] = TRUE
    
    ; Check the data type
    CASE StrUpCase(s_var_info.datatype) OF
      'BYTE': tbyte = 1
      'CHAR': tchar = 1
      'DOUBLE': tdouble = 1
      'FLOAT': tfloat = 1
      'LONG': tlong = 1
      'SHORT': tshort = 1
    ENDCASE
    
    ; If the dimension names are present, use them to get the dimension IDs, which are needed to define the variable.
    addInfo = ''
    if  vartokeep.static[p] then begin ; We want to turn the static field on
      s = WHERE(DIMSIDS ne 0, c)
      if c eq N_ELEMENTS(dimsIds) then Message, 'Hmmm, Time dimension not found? '
      dimsIds = dimsIds[s]
      addInfo = '+STATIC+'
    endif    
    
    if vartokeep.Unstag[p] then begin 
      MESSAGE, 'UNSTAGGER parameter currently not supported.', /INFORMATIONAL
      text = '  -:' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(s_var_info.name) + '('
      for i =0, N_ELEMENTS(dimsIds) - 1 do begin
        text += STRLOWCASE(str_equiv(s_dim_names[dimsIds[i]]))
        if i ne N_ELEMENTS(dimsIds) - 1 then  text += ','
      endfor
      text += ')  ; (ignored: UNSTAGGER parameter currently not supported.)'
      printf, unit, text
      continue
    endif
    
    if vartokeep.D3toD2[p] then begin
      s = WHERE(str_equiv(s_dim_names[dimsIds]) ne str_equiv('bottom_top_stag') and str_equiv(s_dim_names[dimsIds]) ne str_equiv('bottom_top'), c)
      if c eq N_ELEMENTS(dimsIds) then Message, 'Hmmm, third dimension not found? '
      dimsIds = dimsIds[s]
      addInfo = '+D3TOD2+'
    endif
    
    myDimIds = dimsIds
    for d = 0, N_ELEMENTS(dimsIds) - 1 do begin
      pdim = where(t_s_dim_ids eq dimsIds[d], cntd)
      if cntd eq 0 then message, 'Dimension of variable ' + s_var_info.name + ' not found'
      myDimIds[d] = t_dim_ids[pdim]
    end
        
    ; Define the variable.
    TvID = NCDF_VarDef(tid, s_var_info.name, myDimIds, $
      BYTE=tbyte, $
      CHAR=tchar, $
      DOUBLE=tdouble, $
      FLOAT=tfloat, $
      LONG=tlong, $
      SHORT=tshort)
      
    text = '  +:' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(s_var_info.name) + '(' 
    for i =0, N_ELEMENTS(myDimIds) - 1 do begin
      text += STRLOWCASE(str_equiv(t_dim_names[myDimIds[i]]))
      if i ne N_ELEMENTS(myDimIds) - 1 then  text += ','      
    endfor
    text += ') ' + addInfo + ' ; '
    printf, unit, text
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    
    if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
    
    ; Copy the variable attributes
    for sattid = 0, s_var_info.NATTS - 1 do begin
    
      sName = NCDF_ATTNAME(sid, svid, sattid)
      sAtt_info = NCDF_attINQ(sid, svid, sName)
      NCDF_ATTGET, sid, svid, sName, sValue
      
      ; Set the appropriate netCDF data type keyword.
      CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
      ENDCASE
      
      
      ; Add the attribute to the file.
      NCDF_AttPut, tid, TvID, sName, sValue,  $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort
        
      text = '      ' + sName + ': ' + STRLOWCASE(str_equiv(sValue))
      printf, unit, text
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    endfor
    
    if VARTOKEEP.acc[p] then begin ; we have to create a step - wise variable
      
      addInfo = 'STEP_WISE'
      
      ; Check the data type to see that it conforms to netCDF protocol.
      CASE StrUpCase(s_var_info.datatype) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
      ENDCASE
      
      ; Define the variable.
      varn = s_var_info.name + '_STEP'
      
      TvID = NCDF_VarDef(tid, varn, dimsIds, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LONG=tlong, $
        SHORT=tshort)
      text = '  +:' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(varn) + '('
      for i =0, N_ELEMENTS(s_var_info.dim) - 1 do begin
        text += STRLOWCASE(str_equiv(s_dim_names[s_var_info.dim[i]]))
        if i ne N_ELEMENTS(s_var_info.dim) - 1 then  text += ','
      endfor
      text += ')  ' + addInfo + ' ; (added)'
      printf, unit, text
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
      
      if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
      
      ; Copy the variable attributes
      for sattid = 0, s_var_info.NATTS - 1 do begin
      
        sName = NCDF_ATTNAME(sid, svid, sattid)
        sAtt_info = NCDF_attINQ(sid, svid, sName)
        NCDF_ATTGET, sid, svid, sName, sValue
        
        if StrUpCase(sAtt_info.DATATYPE) eq 'CHAR' then begin
          isHere = STRPOS(str_equiv(sValue),'ACCUMULATED')
          if isHere ne -1 then GEN_str_subst, ret, str_equiv(sValue),'ACCUMULATED','STEPWISE', sValue
        endif
        
        ; Set the appropriate netCDF data type keyword.
        CASE StrUpCase(sAtt_info.DATATYPE) OF
          'BYTE': tbyte = 1
          'CHAR': tchar = 1
          'DOUBLE': tdouble = 1
          'FLOAT': tfloat = 1
          'LONG': tlong = 1
          'SHORT': tshort = 1
        ENDCASE
                
        ; Add the attribute to the file.
        NCDF_AttPut, tid, TvID, sName, sValue,  $
          BYTE=tbyte, $
          CHAR=tchar, $
          DOUBLE=tdouble, $
          FLOAT=tfloat, $
          LENGTH=tlength, $
          LONG=tlong, $
          SHORT=tshort
          
        text = '    ' + sName + ': ' + STRLOWCASE(str_equiv(sValue))
        printf, unit, text
        undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
        
        endfor
        
      endif
      
      if VARTOKEEP.diff[p] then begin ; we have to create a diff - wise variable
      
      addInfo = 'DIFF_WISE'
      
      ; Check the data type to see that it conforms to netCDF protocol.
      CASE StrUpCase(s_var_info.datatype) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
      ENDCASE
      
      ; Define the variable.
      varn = s_var_info.name + '_DIFF'
      
      TvID = NCDF_VarDef(tid, varn, dimsIds, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LONG=tlong, $
        SHORT=tshort)
      text = '  +:' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(varn) + '('
      for i =0, N_ELEMENTS(s_var_info.dim) - 1 do begin
        text += STRLOWCASE(str_equiv(s_dim_names[s_var_info.dim[i]]))
        if i ne N_ELEMENTS(s_var_info.dim) - 1 then  text += ','
      endfor
      text += ')  ' + addInfo + ' ; (added)'
      printf, unit, text
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
      
      if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
      
      ; Copy the variable attributes
      for sattid = 0, s_var_info.NATTS - 1 do begin
      
        sName = NCDF_ATTNAME(sid, svid, sattid)
        sAtt_info = NCDF_attINQ(sid, svid, sName)
        NCDF_ATTGET, sid, svid, sName, sValue
        
        ; Set the appropriate netCDF data type keyword.
        CASE StrUpCase(sAtt_info.DATATYPE) OF
          'BYTE': tbyte = 1
          'CHAR': tchar = 1
          'DOUBLE': tdouble = 1
          'FLOAT': tfloat = 1
          'LONG': tlong = 1
          'SHORT': tshort = 1
        ENDCASE
        
        if sName eq 'description' then sValue = STRING(sValue) + ' (diff to previous step)'

        ; Add the attribute to the file.
        NCDF_AttPut, tid, TvID, sName, sValue,  $
          BYTE=tbyte, $
          CHAR=tchar, $
          DOUBLE=tdouble, $
          FLOAT=tfloat, $
          LENGTH=tlength, $
          LONG=tlong, $
          SHORT=tshort
          
        text = '    ' + sName + ': ' + STRLOWCASE(str_equiv(sValue))
        printf, unit, text
        undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
        
        endfor
        
      endif
    
  endfor
  
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
  NCDF_CLOSE, sid ; Close source file
  
  ; Did we miss some Vars ?
  printf, unit, ' '
  p = where(VARTOKEEP.found ne TRUE, cnt)
  if cnt ne 0 then for i=0, cnt-1 do printf, unit, '  !:' + VARTOKEEP.name[p[i]] + ' was not found in the Netcdf file!' 

  printf, unit, ' '
  printf, unit, ' '
  printf, unit, '-------------'
  printf, unit, '* Fill now * ' 
  printf, unit, '-------------'
  printf, unit, ' '
  
  POST_fill_ncdf, unit, tid, filelist, vartokeep, ts, spin_index, e_index
  
  printf, unit, ' '
  printf, unit, ' '
  printf, unit, '------------'
  printf, unit, '* SUCCESS * ' 
  printf, unit, '------------'
  printf, unit, ' '
  tott = MAKE_TIME_STEP(DMS=(QMS_TIME()-syst))
  printf, unit, 'Total time: ' + str_equiv(tott.hour) + ' hrs, ' + str_equiv(tott.minute) + ' mns, ' + str_equiv(tott.second) + ' secs.' 
  
  close, unit ; close log file  
  free_lun, Unit
  NCDF_CLOSE, tid ; Close file
  
end

pro POST_aggregate_Mass_directory, domain, directory, OUTdirectory, SPINUP_INDEX = spinup_index, END_index = end_index, VARTOKEEP_FILE = vartokeep_file

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    if N_ELEMENTS(unit) ne 0 then begin
;      printf, unit, ' '
;      printf, unit, ' '
;      printf, unit, '* ERROR : ' + !Error_State.Msg
;      printf, unit, ' '
;      printf, unit, 'End   : ' + TIME_to_STR(QMS_TIME())
;      printf, unit, ' '
;      close, unit
;      free_lun, Unit
;    endif
;    ok = WAVE_Error_Message(!Error_State.Msg)
;    RETURN
;  ENDIF
  
  ; ---------------
  ; Check the input
  ; ---------------
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select directory containing the directories to parse', /MULTIPLE_FILES)
  directories = FILE_SEARCH(directory, '[12][0123456789][0123456789][0123456789].[0123456789][0123456789]', /EXPAND_ENVIRONMENT) ;TODO: Update routine: not perfect
  ndirs = N_ELEMENTS(directories)
  if ndirs eq 0 then return
  if N_ELEMENTS(OUTdirectory) eq 0 then OUTdirectory = DIALOG_PICKFILE(TITLE='Please select directory to put the output', /DIRECTORY)  
  if OUTdirectory eq '' then return
  FILE_MKDIR, OUTdirectory
  
  OPENW, unit, OUTdirectory + '/wrf_agg_mass_d'+str_equiv(domain)+'.log', /GET_LUN 
    
  printf, unit, '' 
  printf, unit, 'WRF output mass aggregate.' 
  printf, unit, ''
  printf, unit, 'Start : ' + TIME_to_STR(QMS_TIME())
  
  printf, unit, ''
  printf, unit, 'Number of directories to aggregate: ' + str_equiv(ndirs)
  printf, unit, ''
  printf, unit, 'Creating output directories: '   
  outdirs = STRARR(Ndirs)
  for i=0, Ndirs-1 do begin
    if ~FILE_TEST(directories[i], /DIRECTORY) then  message, '$' + directories[i] + ' not good.'
    dirspl = STRSPLIT(directories[i], PATH_SEP(), /EXTRACT, COUNT=cnt)
    last = STRSPLIT(dirspl[cnt-1], '.', /EXTRACT, COUNT=cnt2)
    if cnt2 ne 2 then message, '$' + directories[i] + ' not good.'
    if not arg_okay(last, /NUMERIC) then message, '$' + directories[i] + ' not good.'
    if long(last[0]) gt 2100 or long(last[0]) lt 1900 then message, '$' + directories[i] + ' not good.'
    if long(last[1]) gt 12 or long(last[1]) lt 1 then message, '$' + directories[i] + ' not good.'
    outdirs[i] = FILE_EXPAND_PATH(OUTdirectory + PATH_SEP() + dirspl[cnt-1])
    FILE_MKDIR, outdirs[i] 
    printf, unit, '  + ' + outdirs[i] 
  endfor
  
  printf, unit, ''
  printf, unit, 'OK. Now start to aggregate : '   
  flush, unit
  for i=0, Ndirs-1 do begin
    printf, unit, '  Starting ' + directories[i] + ' ...'  
    flush, unit
    POST_aggregate_directory, domain, directories[i], SPINUP_INDEX = spinup_index, END_INDEX = end_index, OUTDIRECTORY = outdirs[i], VARTOKEEP_FILE = vartokeep_file
  endfor
  
  printf, unit, ' '
  printf, unit, ' '
  printf, unit, '------------'
  printf, unit, '* SUCCESS * ' 
  printf, unit, '------------'
  printf, unit, ' ' 

  printf, unit, 'End   : ' + TIME_to_STR(QMS_TIME())
  printf, unit, ' '
 
  close, unit ; close log file  
  free_lun, Unit
  
end
