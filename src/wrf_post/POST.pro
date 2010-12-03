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
;     Last modification:  29-Nov-2010 FaM
;-



;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    domain
;    directory
;
; :Keywords:
;    CNT
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 03 Dec 2010 
;       Modified:   03-Dec-2010 FaM
;                   First aparition
;
;-
function POST_list_files, domain, directory, CNT = cnt
; Lists all files for one domain id in a directory,
; INPUT: domain (1 , 2, 3 , ...) , directory to search.
; returns the path lists, and (optional) the number of files found (CNT)
  @WAVE.inc
  
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select directory to parse', /DIRECTORY)  
  if not FILE_TEST(directory, /DIRECTORY) then message, 'Directory is not a directory'
  
  if N_ELEMENTS(domain) ne 1 then message, 'domain is not set'
    
  fileList = FILE_SEARCH(directory, 'wrfout*', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT)
   
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
; :Private:
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
;   IDL> ok = POST_parse_Vartokeep(file, vartokeep, dimtokeep)
;   IDL> help, vartokeep, /str
;   ** Structure <2656108>, 7 tags, length=808, data length=802, refs=1:
;      NAME            STRING    Array[38]
;      STATIC          BYTE      Array[38]
;      ACC             BYTE      Array[38]
;      DIFF            BYTE      Array[38]
;      D3TOD2          BYTE      Array[38]
;      UNSTAG          BYTE      Array[38]
;      N_VARS          LONG                38
;   IDL> print, DIMTOKEEP
;   Time DateStrLen west_east south_north
;    
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2010.
;       
;       Modified::
;          29-Nov-2010 FaM
;          Extention of available actions to perform on the variables and upgrade to WAVE 0.1
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
  
  ; Close the files and deallocate the units:
  CLOSE, fid
  FREE_LUN, fid
  
  vartokeep = {name   : vars, $
           static : static, $
           acc    : acc, $
           diff   : diff, $
           D3toD2 : d3tod2, $
           Unstag : Unstag, $
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
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 2010.
;       
;       Modified::
;          29-Nov-2010 FaM
;          Doc for upgrade to WAVE 0.1
;-
function POST_absDAte_to_wrfstr, absDate
  
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
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
;    Describe the procedure.
;
; :Params:
;    tid
;    filelist
;    vartokeep
;    accs
;    statics
;    d3tod2
;    ts
;    spin_index
;
;
;
; :Author: fab
;-
pro POST_fill_ncdf, tid, filelist, vartokeep, accs, statics, d3tod2, ts, spin_index

; Fills the target NCDF file (aggregated file) with data.
; tid: the ncdf file ID
; filelist: the list of files to read and process
; vartokeep, accs, statics: the output from #POST_parse_Vartokeep#
; ts: the time serie of the aggregated file
; spin_index: where to pick up the first step in the original file (for example, with 12H spin-up and a 3H interval: 03H UTC -> index 5)
; OUTPUT:
; Nothing, the file is full. the log file is updated
  nvars = N_ELEMENTS(vartokeep)
  nfiles = N_ELEMENTS(filelist)
  nt = N_ELEMENTS(ts)
  cert = INTARR(nt)
  
  for f=0, nfiles-1 do begin
  
    fid = NCDF_OPEN(filelist[f], /NOWRITE)
    NCDF_VARGET, fid, 'Times', stimes
    ntimes = N_ELEMENTS(stimes[0,*])
    stimes = STRING(stimes[*,0:ntimes-1])
    ;String format : '2008-10-26_12:00:00; length 19
    times = MAKE_ABS_DATE(YEAR=STRMID(stimes,0,4), MONTH=STRMID(stimes,5,2),DAY=STRMID(stimes,8,2), $
      HOUR=STRMID(stimes,11,2),MINUTE=STRMID(stimes,14,2),SECOND=STRMID(stimes,17,2))
    startIdx = spin_index
    
    
    idx = where(ts.qms eq (times[startIdx]).qms, cnt)
    pe = where(ts.qms eq (times[ntimes-1]).qms, cnt)
    
    
    for v=0, nvars - 1 do begin
      vid = vartokeep[v]
      NCDF_VARGET, fid, vid, var
      ndims = SIZE(var, /N_DIMENSIONS)
      varsiz = SIZE(var, /DIMENSIONS)
      
      if accs[v] eq 'TRUE' then begin
        var = POST_ACC_TO_STEP(var) ; we have to deaccumulate
        vid += '_SW'
      end
      
      if statics[v] eq 'TRUE' then begin
        if f eq 0 then begin ; We need to do it just one time...
         if ndims eq 1 then message, 'Impossible' $
          else if ndims eq 2 then NCDF_VARPUT, tid, vid, var[*,0], OFFSET=[0] $
            else if ndims eq 3 then NCDF_VARPUT, tid, vid, var[*,*,0], OFFSET=[0,0] $
              else if ndims eq 4 then message, 'Impossible'
          printf, 1, '   Static field ' + vid + ' filled.'
          flush, 1
        endif       
      endif else if d3tod2[v] eq 'TRUE' then begin
         if ndims eq 1 then message, 'Impossible' $
          else if ndims eq 2 then message, 'Impossible' $
            else if ndims eq 3 then message, 'Impossible' $
              else if ndims eq 4 then NCDF_VARPUT, tid, vid, reform(var[*,*,0,startIdx:*]), OFFSET=[0,0,idx]
        ; otherwise do nothing        
      endif else begin
        if ndims eq 1 then NCDF_VARPUT, tid, vid, var[startIdx:*], OFFSET=[idx] $
          else if ndims eq 2 then NCDF_VARPUT, tid, vid, var[*,startIdx:*], OFFSET=[0,idx] $
            else if ndims eq 3 then NCDF_VARPUT, tid, vid, var[*,*,startIdx:*], OFFSET=[0,0,idx] $
              else if ndims eq 4 then NCDF_VARPUT, tid, vid, var[*,*,*,startIdx:*], OFFSET=[0,0,0,idx]
      endelse  
      cert[idx:pe] += 1 ; allright
    endfor
    
    printf, 1, FILE_BASENAME(filelist[f]) + ' processed.'
    flush, 1
    NCDF_CLOSE, fid
  endfor
  
  pcert = WHERE(cert ne nvars, cnt)
  if cnt ne 0 then message, 'Houston, not ok?'
  
  printf, 1, ''
  printf, 1, 'Retrieve the accumulated variables... '
  
  ; Go threw the de-accumulated variables
  da = WHERE(accs eq 'TRUE', ca)
  for i = 0L, ca-1 do begin
    vid = vartokeep[da[i]]
    NCDF_VARGET, tid, vid + '_SW', var 
    ndims = SIZE(var, /N_DIMENSIONS)
    varsiz = SIZE(var, /DIMENSIONS)
    
    if ndims ne 3 then message, 'Well, dim3 is expected'
    var = TOTAL(var, 3, /CUMULATIVE) ; we have to deaccumulate

    NCDF_VARPUT, tid, vid, var
    printf, 1, '  ' + vid + ' processed. '
    flush, 1
  endfor

  printf, 1, ''
  printf, 1, 'Full.'
  printf, 1, ''

end

;+
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
;          
; :Keywords:
;    OUTFILE: in, optional, type = string
;           the output file. If it already exists, it will be overwritten.
;           default is to copy the file in the same directory, adding the preffix crop_ to the file
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;
; :History:
;       Written by FaM, 03 Dec 2010 
;       Modified:   03-Dec-2010 FaM
;                   First aparition
;
;-
pro POST_crop_file, file, index, OUTFILE = outfile

  if ~FILE_TEST(file) then file = DIALOG_PICKFILE(TITLE='Please select the file to crop.')
  if ~KEYWORD_SET(outfile) then outfile = FILE_DIRNAME(file) + '/crop_' + FILE_BASENAME(file)

  sid = Ncdf_open(File, /NOWRITE)    
  if ~arg_okay(index, /NUMERIC) then begin
      NCDF_ATTGET, sid , 'GRID_ID', dom, /GLOBAL
      if dom eq 1 then index = 4 else index = 12
  endif
    
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
  NCDF_AttPut, tid, 'ORIG_FILE', FILE, /GLOBAL, /CHAR
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
  
      NCDF_VARGET, sid, vid, var
      ndims = SIZE(var, /N_DIMENSIONS)
        if ndims eq 1 then NCDF_VARPUT, tid, vid, var[index:*] $
          else if ndims eq 2 then NCDF_VARPUT, tid, vid, var[*,index:*]$
            else if ndims eq 3 then NCDF_VARPUT, tid, vid, var[*,*,index:*] $
              else if ndims eq 4 then NCDF_VARPUT, tid, vid, var[*,*,*,index:*]
              
  endfor
  
  
  NCDF_CLOSE, sid ; Close source file
  close, 1 ; close log file  
  NCDF_CLOSE, tid ; Close file


end

;+
; :Description:
;    Describe the procedure.
;
; :Params:
;    directory
;    domain
;    t1
;    step
;    spin_index
;
; :Keywords:
;    OUTdirectory
;    VARFILE
;    t0
;
; :Author: fab
;-
pro POST_aggregate_directory,  directory, domain, end_time = end_time, spinup_index = spinup_index, start_time = start_time, timestep = timestep, OUTdirectory = OUTdirectory, vartokeep_file = vartokeep_file

; THE UNIQUE PROCEDURE TO CALL. INPUT: 
; directory: dir where to search for files
; domain: domain id (1, 2, 3, ...)
; t1: end time in {ABS_DATE} (t1 = MAKE_ABS_DATE(year = ...) )
; step: (opt) time step in {TIME_STEP} format : step = MAKE_TIME_STEP(hour=1)
; spin_index: (opt) where to pick up the first step in the original file (for example, with 12H spin-up and a 3H interval: 03H UTC -> index 5)
; OUTdirectory : if not set, a dialog window is opened. If non-existent, it is created
; Varfile: (opt) the ASCII file to parse to obtain the variable lists to aggregate
; t0: (opt): first time of the aggregated time-serie. if not set, retrieved automatically
; OUTPUT:
; Two files: the aggregated NCDF file and a log file.
  @WAVE.inc
      
  fileLIST = POST_list_files(domain, directory, CNT = cnt)  
  if N_ELEMENTS(spin_index) eq 0 then if domain eq 1 then spin_index = 5 else spin_index = 13
  if N_ELEMENTS(step) eq 0 then if domain eq 1 then step = MAKE_TIME_STEP(hour=3) else step = MAKE_TIME_STEP(hour=1)
  if N_ELEMENTS(OUTdirectory) eq 0 then OUTdirectory = DIALOG_PICKFILE(TITLE='Please select directory where to put the output', /DIRECTORY)
  if N_ELEMENTS(VARFILE) eq 0 then VARFILE = WAVE_resource_dir + '/files/vars_311_default.txt'
  ;Go threw the variables.
  vtokeep = POST_parse_Vartokeep(VARFILE)
  
  if OUTdirectory eq '' then return
  
  root_out =  OUTdirectory
  if ~FILE_TEST(OUTdirectory, /DIRECTORY) then FILE_MKDIR, OUTdirectory
  
    ;Parse names for available times wrfout_d01_2008-10-26_12:00:00
  fnames = FILE_BASENAME(fileLIST)
  month = LONG(STRMID(fnames,16,2))
  year = LONG(STRMID(fnames,11,4))
  day = LONG(STRMID(fnames,19,2))
  hour = LONG(STRMID(fnames,22,2))
  ds = MAKE_ABS_DATE(MONTH=month, year=year, day = day, hour = hour)
  dummy = min(ds.qms, p)
  myt0 = MAKE_ABS_DATE(REFDATE=ds[0], QMS = step.dms * spin_index)  
  ok = 'Yes'
  
  if ~KEYWORD_SET(t0) then t0 = myt0
  if myt0.qms ne t0.qms then ok = DIALOG_MESSAGE('Are you sure? my t0 and your t0 do not match. ', /QUESTION)
  if ok eq 'No' then return  
  UNDEFINE, myt0, ds, dummy
  
  ; Ok. Lets go
  ts = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=step)
  nt = N_ELEMENTS(ts)
  
  ; Create the copy log file
  str = TIME_to_STR(t0) ; 22.10.2008 03:00:00
  OPENW, 1, root_out + '/wrf_cpy_'+ strmid(str,6,4) + '_' + strmid(str,3,2) + '_' + strmid(str,0,2) + '_d0' + str_equiv(domain) + '.log'
  
  text = 'WRF output aggregation'
  printf, 1, text 
  text = 'Start date : ' + TIME_to_STR(ts[0])
  printf, 1, text
  text = 'End   date : ' + TIME_to_STR(ts[nt-1])  
  printf, 1, text 
  
  ; Open the file you just created and copy the information in it to another file.
  sourceFile = fileLIST[0]
  destFile = root_out + '/wrf_agg_'+ strmid(str,6,4) + '_' + strmid(str,3,2) + '_' + strmid(str,0,2) + '_d0' + str_equiv(domain) + '.nc'
  
  printf, 1, ''
  text = 'Destination file : ' + destFile 
  printf, 1, text 
    
  sid = Ncdf_open(sourceFile, /NOWRITE)
  inq = NCDF_INQUIRE(sid)
  tid = NCDF_CREATE(destFile, /CLOBBER, /NET)
  NCDF_CONTROL, tid, /FILL
  
  printf, 1, ''
  printf, 1, '---------------'
  printf, 1, '* Dimensions * ' 
  printf, 1, '---------------'
  printf, 1, ' '
  
  idTime = -1
  ;Go threw the dimensions. No need to change anything but the time
  for i =0, inq.ndims-1 do begin
  
    NCDF_DIMINQ, sid, i, sName, sSize    
    if str_equiv(sName) eq 'TIME' then begin
      ssize = nt
      idTime = i
    endif
    if str_equiv(sName) eq 'BOTTOM_TOP' then idVert = i
    
    tdimid  = NCDF_DIMDEF(tid, sName, ssize)     
    if N_ELEMENTS(t_dim_names) eq 0 then t_dim_names = sName else t_dim_names=[t_dim_names,sName]
    if N_ELEMENTS(t_dim_Sizes) eq 0 then t_dim_Sizes = sSize else t_dim_Sizes=[t_dim_Sizes,sSize] 
    if N_ELEMENTS(t_dim_ids) eq 0 then t_dim_ids = tdimid else t_dim_ids=[t_dim_ids,tdimid] 
        
    printf, 1, ' ', sName, ': ', STR_equiv(sSize)
  endfor ; Dimensions OK
  
  printf, 1, ' '
  printf, 1, ' '
  printf, 1, '----------------------'
  printf, 1, '* Global attributes * ' 
  printf, 1, '----------------------'
  printf, 1, ' '
  
  ;Go threw the global Attribute. No need to change a lot but add some info  
  removAbleAtts = ['SIMULATION_START_DATE','JULYR','JULDAY']  
  
  for i =0, inq.Ngatts-1 do begin  
  
    sName = NCDF_ATTNAME(sid, i , /GLOBAL)        
    sAtt_info = NCDF_attINQ(sid, sName, /GLOBAL)
    NCDF_ATTGET, sid, sName, sValue, /GLOBAL
    
    ; go threw the atts we want to remove
    isHere = WHERE(removAbleAtts eq str_equiv(sName), cnt)
    if cnt ne 0 then continue
    
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
    
    text = ' ' + StrlowCase(sName) + ': ' + str_equiv(sValue)
    printf, 1, text
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
  endfor ; Att OK
  
  ; Add my own
  NCDF_AttPut, tid, 'creation_date', TIME_to_STR(QMS_TIME()), /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'source', 'File generated with the WAVE library by aggregation of WRF single runs.', /GLOBAL, /CHAR
  
  printf, 1, ' '
  printf, 1, ' '
  printf, 1, '--------------'
  printf, 1, '* Variables * ' 
  printf, 1, '--------------'
  printf, 1, ' '
 
  for svid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,svid)
    
    p = where(var_to_keep eq s_var_info.name, cnt)
    if cnt eq 0 then continue ;Nothing to be done
    
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
    addInfo = ''
    if STATIC[p] eq 'TRUE' then begin ; We want to turn the static field on
      s = WHERE(DIMSIDS ne idTime, c)
      if c eq N_ELEMENTS(dimsIds) then Message, 'Hmmm, Time dimension not encountered ? '
      dimsIds = dimsIds[s]
      addInfo = '+STATIC+'
    endif    
    
    if d3tod2[p] eq 'TRUE' then begin 
      s = WHERE(DIMSIDS ne idVert, c)
      if c eq N_ELEMENTS(dimsIds) then Message, 'Hmmm, Vert dimension not encountered ? '
      dimsIds = dimsIds[s]
      addInfo = '+3Dto2D+'
    endif
        
    ; Define the variable.
    TvID = NCDF_VarDef(tid, s_var_info.name, dimsIds, $
      BYTE=tbyte, $
      CHAR=tchar, $
      DOUBLE=tdouble, $
      FLOAT=tfloat, $
      LONG=tlong, $
      SHORT=tshort)
    text = ' ' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(s_var_info.name) + '(' 
    for i =0, N_ELEMENTS(dimsIds) - 1 do begin
      text += STRLOWCASE(str_equiv(t_dim_names[dimsIds[i]]))
      if i ne N_ELEMENTS(dimsIds) - 1 then  text += ','      
    endfor
    text += ')  ' + addInfo + ' ; '
    printf, 1, text
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
        
      text = '    ' + sName + ': ' + STRLOWCASE(str_equiv(sValue))
      printf, 1, text
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    endfor
    
    if acc[p] eq 'TRUE' then begin ; we have to create a step - wise variable
    
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
      varn = s_var_info.name + '_SW'
      
      TvID = NCDF_VarDef(tid, varn, dimsIds, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LONG=tlong, $
        SHORT=tshort)
      text = ' ' + STRLOWCASE(s_var_info.DATATYPE) + '  ' + str_equiv(varn) + '('
      for i =0, N_ELEMENTS(s_var_info.dim) - 1 do begin
        text += STRLOWCASE(str_equiv(t_dim_names[s_var_info.dim[i]]))
        if i ne N_ELEMENTS(s_var_info.dim) - 1 then  text += ','
      endfor
      text += ')  ' + addInfo + ' ; '
      printf, 1, text
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
        printf, 1, text
        undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
        
        endfor
        
      endif
    
  endfor
  
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
  NCDF_CLOSE, sid ; Close source file
  
  ; Go threw the files 
  
  printf, 1, ' '
  printf, 1, ' '
  printf, 1, '-------------'
  printf, 1, '* Fill now * ' 
  printf, 1, '-------------'
  printf, 1, ' '
  
  POST_fill_ncdf, tid, filelist, var_to_keep, acc, static, d3tod2, ts, spin_index
  
  printf, 1, ' '
  printf, 1, ' '
  printf, 1, '------------'
  printf, 1, '* SUCCESS * ' 
  printf, 1, '------------'
  printf, 1, ' '
  
  close, 1 ; close log file  
  NCDF_CLOSE, tid ; Close file
  
end


