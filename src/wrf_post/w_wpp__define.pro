; docformat = 'rst'
;+
;  WRF Product-Processor
;  
; :History:
;     Written by FaM, 2012.
;
;-

;+
; :Description:
;    Creates an instance of the wpp object.
;
; :Keywords:
;    NAMELIST: in, required, type=string
;              path to the namelist.wpp file
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;    CACHING: in, optional, type=boolean, default=1
;             if set (default), the input files are first copied to a local cache directory before reading
;
;-
Function w_WPP::Init, NAMELIST=namelist, PRINT=print, CACHING=caching

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    if OBJ_VALID(self.logger) then begin
      self.logger->addError
      obj_destroy, self.Logger
    endif else begin
      ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    endelse
    return, 0
  endif
  
  ; Check if everything needed is here  
  if N_ELEMENTS(NAMELIST) eq 0 then namelist = DIALOG_PICKFILE(TITLE='Please select the namelist.wpp file', /MUST_EXIST, FILTER='*.wpp')
  if namelist eq '' then Message, WAVE_Std_Message(/FILE)  
  self.namelist_file = namelist  
  if N_ELEMENTS(CACHING) eq 0 then self.do_cache = 1B else self.do_cache = caching
  if N_ELEMENTS(PRINT) eq 0 then print = 1    
  if ~ self->_Parse_Namelist() then Message, 'Unable to parse the namelist file. Please check it.'
  
  ; Let's go
  if self.do_cache then begin
    self.cachepath = self.log_directory + '/cache'+cgTimestamp(11, RANDOM_DIGITS=6, /VALID) 
    FILE_MKDIR, self.cachepath
  endif
  
  ; Logger  
  logf = self.log_directory + '/wpp_init_log_' + TIME_to_STR(QMS_TIME(), MASK='YYYY_MM_DD_HHTTSS') + '.log'
  self.Logger = Obj_New('ErrorLogger', logf, ALERT=1, DELETE_ON_DESTROY=0, TIMESTAMP=0)
  ; General info
  self.Logger->AddText, 'WPP logfile ' + self.title + ' - Domain ' + str_equiv(self.domain), PRINT=print
  self.Logger->AddText, '', PRINT=print  
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, 'Log directory: ' + self.log_directory, PRINT=print
  self.Logger->AddText, 'Compression level: ' + str_equiv(self.compress_level), PRINT=print
  t = (self.shuffle EQ 1) ? 'yes' : 'no'
  self.Logger->AddText, 'Shuffle: ' + t, PRINT=print 
  
  ; List input files
  self.Logger->AddText, 'Listing files in: ' + self.input_directory + ' ...'  , PRINT=print
  if ~ self->_list_files() then MESSAGE, 'No files found. Check the directory of the search pattern'  
  self.Logger->AddText, 'Found ' + str_equiv(self.n_ifiles) + ' files', PRINT=print
  self.Logger->AddText, '', PRINT=print
  
  ; Parse the variable files. Need a WRF template for this
  if self.do_cache then file = caching((*self.ifiles)[0], CACHEPATH=self.cachepath) else file = (*self.ifiles)[0]
  self.active_wrf = OBJ_NEW('w_WRF', FILE=file)
  if self.do_cache then file = caching((*self.ifiles)[0], CACHEPATH=self.cachepath, /DELETE)  
  self.Logger->AddText, 'Parse variable definitions ...', PRINT=print  
  if ~ self->_Parse_VarDefFile(self.vstatic_file, 'static', PRINT=print) then Message, 'Unable to parse the vardef file. Please check it: ' + self.vstatic_file
  if ~ self->_Parse_VarDefFile(self.v2d_file, '2d', PRINT=print) then Message, 'Unable to parse the vardef file. Please check it: ' + self.v2d_file 
  if ~ self->_Parse_VarDefFile(self.v3d_file, '3d_eta', PRINT=print) then Message, 'Unable to parse the vardef file. Please check it: ' + self.v3d_file
  if self.n_pressure_levels ne 0 then if ~ self->_Parse_VarDefFile(self.v3d_file, '3d_press', PRINT=print) then Message, 'Unable to parse the vardef file. Please check it: ' + self.v3d_file
  if ~ self->_Parse_VarDefFile(self.vsoil_file, '3d_soil', PRINT=print) then Message, 'Unable to parse the vardef file. Please check it: ' + self.vsoil_file
  OBJ_DESTROY, self.active_wrf
  
  
  ; Info
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '+----------------------------+', PRINT=print
  self.Logger->AddText, '+ Initialisation successfull +', PRINT=print
  self.Logger->AddText, '+----------------------------+', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->Flush
  
  OBJ_DESTROY, self.logger
    
  return, 1
  
end

;+
; :Description:
;    Destroy function.
;
;-
pro w_WPP::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
    
  obj_destroy, self.logger  
  obj_destroy, self.active_wrf
  ptr_free, self.active_time
  ptr_free, self.active_index
  ptr_free, self.ifiles
  ptr_free, self.pressure_levels 
  undefine, *self.vars
  ptr_free, self.vars   
  if FILE_TEST(self.cachepath, /DIRECTORY) then file_delete, self.cachepath, /RECURSIVE
  
END

;+
; :Description:
;   Parse the namelist file.
;
;-
function w_WPP::_parse_Namelist

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    if N_ELEMENTS(lun) ne 0 then FREE_LUN, lun
    ok = WAVE_Error_Message(!Error_State.Msg, /WARNING)
    return, 0
  ENDIF
  
  ; Open the file and loop over the lines
  OPENR, lun, self.namelist_file, /GET_LUN
  while ~ eof(lun) do begin
    line = ''
    readf, lun, line
    if line eq '' then continue
    line = (STRSPLIT(line, ';', /EXTRACT, /PRESERVE_NULL))[0] ; before the comment is the only interesting part
    if line eq '' then continue
    line = STRSPLIT(line, '=', /EXTRACT)
    if N_ELEMENTS(line) ne 2 then continue ; we just accept one = sign
    key = str_equiv(line[0])
    val = GEN_strtrim(line[1], /ALL)
    
    case (key) of
      'DOMAIN': begin
        self.domain = LONG(val)
      end
      'INPUT_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.input_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'LOG_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.log_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'SEARCH_PATTERN': begin
        matches = Where(StrMatch(val, '*{domain}*'), count)
        if count gt 0 then self.search_pattern = val
      end
      'DOM_SUFFIX': begin
        if val ne '' then self.dom_suffix = val
      end
      'PRODUCT_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.product_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'PATH_TO_VARIABLES_2D_FILE': begin
        if FILE_TEST(val) then self.v2d_file = utils_clean_path(val)
        if val eq '.' then begin
          val = FILE_DIRNAME(self.namelist_file)+'/variables_2d_wpp.csv'
          if FILE_TEST(val) then self.v2d_file = utils_clean_path(val)
        endif
      end
      'PATH_TO_VARIABLES_3D_FILE': begin
        if FILE_TEST(val) then self.v3d_file = utils_clean_path(val)
        if val eq '.' then begin
          _val = FILE_DIRNAME(self.namelist_file)+'/variables_3d_wpp.csv'
          if FILE_TEST(_val) then self.v3d_file = utils_clean_path(_val)
        endif
      end
      'PATH_TO_VARIABLES_SOIL_FILE': begin
        if FILE_TEST(val) then self.vsoil_file = utils_clean_path(val)
        if val eq '.' then begin
          _val = FILE_DIRNAME(self.namelist_file)+'/variables_soil_wpp.csv'
          if FILE_TEST(_val) then self.vsoil_file = utils_clean_path(_val)
        endif
      end
      'PATH_TO_VARIABLES_STATIC_FILE': begin
        if FILE_TEST(val) then self.vstatic_file = utils_clean_path(val)
        if val eq '.' then begin
          _val = FILE_DIRNAME(self.namelist_file)+'/variables_static_wpp.csv'
          if FILE_TEST(_val) then self.vstatic_file = utils_clean_path(_val)
        endif
      end
      'PRODUCT_DIRECTORY': begin        
        if FILE_TEST(val, /DIRECTORY) then self.product_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'PRESSURE_LEVELS': begin
        val = LONG(STRSPLIT(val, ',', /EXTRACT))
        if N_ELEMENTS(val) ne 0 then begin
          self.n_pressure_levels = N_ELEMENTS(val)
          self.pressure_levels = PTR_NEW(val)
        endif
      end
      'COMPRESS_LEVEL': begin
        val = LONG(val)
        if val ge 0 and val le 9 then self.compress_level = val
      end
      'SHUFFLE': begin
        val = LONG(val)
        if val eq 1 then self.shuffle = 1
      end
      'PROJECT_ACRONYM': begin
        self.project_acronym = val
      end
      'CREATED_BY': begin
        self.created_by = val
      end
      'INSTITUTION': begin
        self.institution = val
      end
      'TITLE': begin
        self.title = val
      end
      'NOTES': begin
        self.notes = val
      end
      else:
    endcase
  endwhile
  FREE_LUN, lun
  
  ; Check
  if self.domain eq 0 then Message, 'Problem while parsing domain field.'
  if self.input_directory eq '' then Message, 'Problem while parsing input_directory field.'
  if self.log_directory eq  '' then Message, 'Problem while parsing log_directory field.'
  if self.v2d_file eq '' then Message, 'Problem while parsing v2d_file field.'
  if self.v3d_file eq  '' then Message, 'Problem while parsing v3d_file field.'
  if self.vsoil_file eq '' then Message, 'Problem while parsing vsoil_file field.'
  if self.project_acronym eq '' then Message, 'Problem while parsing project_acronym field.'
  if self.search_pattern eq '' then Message, 'Problem while parsing search_pattern field.'
  if self.product_directory eq '' then Message, 'Problem while parsing product_directory field.'
  
  return, 1
  
end

;+
; :Description:
;    Parse a variables file
;    
;-
function w_WPP::_parse_VarDefFile, file, type, PRINT=print

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    ok = WAVE_Error_Message(!Error_State.Msg, /WARNING)
    return, 0
  ENDIF
  
  csv = READ_CSV(file, COUNT=cnt)
  if cnt lt 2 then return, 1 ; nothing to do
  
  ; ignore the first line (header)
  vardefs = REPLICATE({w_WPP_var}, cnt-1)
  vardefs.name = STRLOWCASE(str_equiv(csv.FIELD1[1:*]))
  vardefs.agg_method = STRLOWCASE(str_equiv(csv.FIELD2[1:*]))
  
  ; Just for the pressure, check the third field in the csv file
  if type eq '3d_press' then begin 
   p = where(STRLOWCASE(str_equiv(csv.FIELD3[1:*])) eq 'yes', cp)
   topress = BYTARR(cnt-1)
   if cp ne 0 then topress[p] = 1 
   if TOTAL(topress) eq 0 then return, 1
  endif
  
  ; Loop over the variables and check if they are available
  for i=0, N_ELEMENTS(vardefs) - 1 do begin
  
    v = vardefs[i]
    ok = self.active_wrf->get_Var_Info(v.name, units = unit, $
      description = description, $
      varname = varname, $
      dims = dims, $
      dimnames = dimnames)
            
    if ~ ok then begin
      self.Logger->AddText, 'VARIABLE: ' + v.name + ' not found.', PRINT=print
      if N_ELEMENTS(inds_to_remove) eq 0 then inds_to_remove = i else inds_to_remove=[inds_to_remove,i]
      continue
    endif
    if type eq '3d_press' then if ~topress[i] then if N_ELEMENTS(inds_to_remove) eq 0 then inds_to_remove = i else inds_to_remove=[inds_to_remove,i]
    
    v.description = STRLOWCASE(description)
    if unit eq '' then unit = '-'
    v.unit = STRLOWCASE(unit)
    v.type = type 
    matches = Where(StrMatch(dimnames, '*_stag'), count)
    if count eq 1 and type ne '3d_soil' then v.unstagger = TRUE ; no need to unstagger soil variables
    vardefs[i] = v
    
  endfor
  
  if N_ELEMENTS(inds_to_remove) ne 0 then utils_array_remove, inds_to_remove, vardefs
  
  ; Log the info
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '+ For type: ' + type + ', found ' + str_equiv(N_ELEMENTS(vardefs)) + ' variables', PRINT=print
  self.Logger->AddText, 'NAME            DESCRIPTION                                       UNIT       AGG_METHOD', PRINT=print
  for i=0, N_ELEMENTS(vardefs) - 1 do begin
     v = vardefs[i]
     ns = '                                                                                  '
     STRPUT, ns, v.name, 1
     STRPUT, ns, v.description, 17
     STRPUT, ns, v.unit, 67
     STRPUT, ns, v.agg_method, 78
     if v.unstagger eq TRUE then  ns += ' -> UNSTAG'
     self.Logger->AddText, ns, PRINT=print
  endfor
  
  ; Add the variables to the list
  if self.n_vars eq 0 then begin
    self.n_vars = N_ELEMENTS(vardefs)
    self.vars = PTR_NEW(vardefs, /NO_COPY)
  endif else begin
    vardefs = [*self.vars, vardefs]
    PTR_FREE, self.vars
    self.n_vars = N_ELEMENTS(vardefs)
    self.vars = PTR_NEW(vardefs, /NO_COPY)
  endelse
  
  return, 1
  
end

;+
; :Description:
;    Search for all available files for the selected domain in the input directory
;    
;-
function w_WPP::_list_files

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    ok = WAVE_Error_Message(!Error_State.Msg, /WARNING)
    return, 0
  ENDIF
  
  pattern = utils_replace_string(self.search_pattern, '{domain}', STRING(self.domain, FORMAT='(I02)'))
  filelist = FILE_SEARCH(self.input_directory, pattern, /EXPAND_ENVIRONMENT, COUNT=cnt)
  self.n_ifiles = cnt
  ptr_free, self.ifiles
  if cnt ne 0 then self.ifiles = PTR_NEW(filelist, /NO_COPY) else Message, 'No files found with following pattern: ' + pattern
  
  return, 1
  
end

;+
; :Description:
;    Simple routine to tell the wpp the current variable
;    to process, generate the active file paths, etc.
;
; :Params:
;    var: in, required, type={w_WPP_var}
;         the variable info structure
;    year: in, required, type=int
;          the year
;    agg: in, required, type=string
;          's','h','d','m' or 'y'
;    obj: in, optional, type=NCDF_FILE object
;         the destination object for the variable
; 
; :Keywords:
;    IS_STATIC: out, optional, type=boolean
;               is the variable is static or not
;
;-
pro w_WPP::_set_active_var, var, year, agg, obj, IS_STATIC=is_static

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2

  case (self.domain) of
    1: dom_str = 'd30km'
    2:  dom_str = 'd10km'
    else: dom_str = 'd02km'    
  endcase  
  if self.dom_suffix ne '' then dom_str += self.dom_suffix
   
  if var.type eq 'static' then begin
    f_dir = dom_str+'/static'
    yr_str = ''
  endif else begin
    f_dir = dom_str+'/'+ agg +'/'+var.type
    yr_str = '_' + STRING(year, FORMAT='(I4)')
  endelse
  
  f_name = self.project_acronym + '_' + utils_replace_string(f_dir, '/', '_') + '_' + var.name + yr_str + '.nc'
  f_path = utils_clean_path(self.product_directory + f_dir + '/' + f_name)
  l_path = utils_clean_path(self.log_directory + '/logs_idl/' + f_dir + '/' + 'check_'+ utils_replace_string(f_name, '.nc', '.sav'))
  f_log = 'log_'+ utils_replace_string(f_name, '.nc', '.log')
  f_log = utils_clean_path(self.log_directory + '/logs_ncdf/' + f_log)
  
  if agg ne 's' then begin
    FILE_MKDIR, FILE_DIRNAME(f_log)
    FILE_MKDIR, FILE_DIRNAME(f_path)
    FILE_MKDIR, FILE_DIRNAME(l_path)
  endif
  
  self.active_checkfile = l_path
  self.active_ncloggerfile = f_log
  self.active_ofile = f_path
  self.active_var = var
  self.active_agg = agg
  self.active_year = year
  if N_ELEMENTS(obj) ne 0 then self.active_dObj = obj
  
  is_static = self.active_var.type eq 'static'
  
end

;+
; :Description:
;   Defines one file per variable per year and per aggregation
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
; 
; :Returns:
;    the ncdf_file object
;
;-
function w_WPP::_define_file, FORCE=force, PRINT=print, MONTH = month

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  static = FALSE
  
  case (self.active_var.type) of
    'static': begin
      nz = 0
      static = TRUE
    end
    '2d': begin
      nz = 0
    end
    '3d_eta': begin
      z_dim_name = 'eta'
      z_var_name = z_dim_name
      z_var_long_name = 'Eta Levels (mass points)'
      z_var_units = ''
      if self.active_agg eq 'h' then begin
        nz = self.active_wrf->get_Dim('bottom_top')
        self.active_wrf->get_Time, dum, dumy, t0
        z = self.active_wrf->get_var('ZNU', t0=t0, t1=t0)
      endif else begin
        nz = self.active_wrf->get_Dim(z_dim_name)
        z = self.active_wrf->get_var(z_var_name)
      endelse
    end
    '3d_press': begin
      nz = self.n_pressure_levels
      z = *self.pressure_levels
      z_dim_name = 'pressure'
      z_var_name = z_dim_name
      z_var_long_name = 'Pressure Levels'
      z_var_units = 'hPa'
    end
    '3d_soil': begin
      z_dim_name = 'soil'
      z_var_name = z_dim_name
      z_var_long_name = 'depths of lower boundaries of soil layers'
      z_var_units = 'm'
      if self.active_agg eq 'h' then begin
        nz = self.active_wrf->get_Dim('soil_layers_stag')
        self.active_wrf->get_Time, dum, dumy, t0
        z = TOTAL(self.active_wrf->get_var('DZS', t0=t0, t1=t0),/CUMULATIVE)
      endif else begin
        nz = self.active_wrf->get_Dim(z_dim_name)
        z = self.active_wrf->get_var(z_var_name)
      endelse
    end
    else: message, 'Type not ok'
  endcase
  
  case (self.domain) of
    1: begin
      nested_string = 'NO'
      grid_string = '30km'
      nt_per_day = (self.active_agg EQ 'h') ? 8 : 1
    end
    2:  begin
      nested_string = 'YES'
      grid_string = '10km'
      nt_per_day = (self.active_agg EQ 'h') ? 24 : 1
    end
    else: begin
      nested_string = 'YES'
      grid_string = '02km'
      nt_per_day = (self.active_agg EQ 'h') ? 24 : 1
    end
  endcase
  if KEYWORD_SET(month) then begin
    time_str = TIME_to_STR(QMS_TIME(YEAR=self.active_year, month=month, day=1), MASK=' since YYYY-MM-DD HH:TT:SS')
  endif else begin
    time_str = TIME_to_STR(QMS_TIME(YEAR=self.active_year, month=1, day=1), MASK=' since YYYY-MM-DD HH:TT:SS')
  endelse
  case (self.active_agg) of
    's': time_str = 'hours since 2000-01-01 00:00:00' 
    'h': time_str = 'hours'  + time_str
    'd': time_str = 'days'   + time_str
    'm': time_str = 'months' + time_str
    'y': time_str = 'years'  + time_str 
    else: MESSAGE, 'type not OK'
  endcase  
           
  x_dim_name = 'west_east'
  y_dim_name = 'south_north'
  t_dim_name = 'time'
    
  ; Coordinates
  self.active_wrf->getProperty, tnt_c=tnt_c
  nx = tnt_c.nx
  ny = tnt_c.ny
  proj = tnt_c.proj
  x = INDGEN(nx, /LONG) * tnt_c.dx + tnt_c.x0
  y = INDGEN(ny, /LONG) * tnt_c.dy + tnt_c.y1  
  self.active_wrf->get_lonLat, lon, lat
  proj_envi_string = tnt_c.proj.envi
  proj_name = tnt_c.proj.name
  datum = tnt_c.proj.datum.name
      
  dObj = Obj_New('NCDF_FILE', self.active_ofile, /CREATE, /TIMESTAMP, /NETCDF4_FORMAT, CLOBBER=force, ErrorLoggerName=self.active_ncloggerfile)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
  dObj->SetMode, /DEFINE
  
  ; Dimensions  
  dObj->WriteDim, t_dim_name, /UNLIMITED
  dObj->WriteDim, x_dim_name, nx
  dObj->WriteDim, y_dim_name, ny
  if nz ne 0 then dObj->WriteDim, z_dim_name, nz
  
  ;WRF Version
  case (self.active_agg) of
    's':  wver = STRING(self.active_wrf->get_Gatt('TITLE'))
    'h':  wver = STRING(self.active_wrf->get_Gatt('TITLE'))
    else: wver = STRING(self.active_wrf->get_Gatt('WRF_VERSION'))
  endcase
  
  ; Global attributes
  dObj->WriteGlobalAttr, 'TITLE', self.title, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DATA_NOTES', self.notes, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'WRF_VERSION', wver, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'CREATED_BY', self.created_by, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'INSTITUTION', self.institution , DATATYPE='CHAR' 
  dObj->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'SOFTWARE_NOTES', 'IDL V' + !VERSION.RELEASE + ', WAVE post V0.1' , DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'VARNAME', self.active_var.name, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'PROJECTION', proj_name , DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'PROJ_ENVI_STRING', proj_envi_string, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DATUM', datum, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DOMAIN', self.domain, DATATYPE='LONG'
  dObj->WriteGlobalAttr, 'NESTED', nested_string, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'TIME_ZONE', 'UTC', DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'GRID_INFO', 'Grid spacing: Global Attributes DX and DY (unit: m), ' + $
                                       'Down left corner: Global Attributes X0 and Y0 (unit: m) ', DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DX', tnt_c.dx, DATATYPE='FLOAT'
  dObj->WriteGlobalAttr, 'DY', tnt_c.dy, DATATYPE='FLOAT'
  dObj->WriteGlobalAttr, 'X0', STRING(min(x), FORMAT='(F12.1)'), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'Y0', STRING(min(y), FORMAT='(F12.1)'), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'PRODUCT_LEVEL', self.active_agg, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'LEVEL_INFO', 'H: original simulation output; ' + $
                                       'D: daily; ' + $
                                       'M: monthly; ' + $
                                       'Y: yearly; ',  $
                                        DATATYPE='CHAR'
  
  ; Variables
  vn = 'time'
  dObj->WriteVarDef, vn, t_dim_name, DATATYPE='LONG'
  dObj->WriteVarAttr, vn, 'long_name', 'Time'
  dObj->WriteVarAttr, vn, 'units', time_str
  vn = 'west_east'
  dObj->WriteVarDef, vn, x_dim_name, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'x-coordinate in Cartesian system'
  dObj->WriteVarAttr, vn, 'units', 'm'
  vn = 'south_north'
  dObj->WriteVarDef, vn, y_dim_name, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'y-coordinate in Cartesian system'
  dObj->WriteVarAttr, vn, 'units', 'm'  
  vn = 'lon'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Longitude'
  dObj->WriteVarAttr, vn, 'units', 'degrees_east'
  vn = 'lat'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Latitude'
  dObj->WriteVarAttr, vn, 'units', 'degrees_north'
  if nz ne 0 then begin
    vn = z_var_name
    dObj->WriteVarDef, vn, z_dim_name, DATATYPE='FLOAT'
    dObj->WriteVarAttr, vn, 'long_name', z_var_long_name
    dObj->WriteVarAttr, vn, 'units', z_var_units
  endif
  
  ;Actual variable 
  if nz eq 0 then begin
    dims = [x_dim_name,y_dim_name,t_dim_name]
    if self.compress_level ne 0 then chunk_dimensions = [nx, ny, nt_per_day]
  endif else begin
    dims = [x_dim_name,y_dim_name,z_dim_name,t_dim_name]
    if self.compress_level ne 0 then chunk_dimensions = [nx, ny, nz, nt_per_day]
  endelse
  vn = self.active_var.name  
  dObj->WriteVarDef, vn, dims, DATATYPE='FLOAT', GZIP=self.compress_level, SHUFFLE=self.shuffle, CHUNK_DIMENSIONS=chunk_dimensions
  dObj->WriteVarAttr, vn, 'long_name', self.active_var.description
  dObj->WriteVarAttr, vn, 'units', self.active_var.unit
  dObj->WriteVarAttr, vn, 'agg_method', self.active_var.agg_method
  
  ; Fill with data
  dObj->SetMode, /DATA
  dObj->WriteVarData, 'west_east', x  
  dObj->WriteVarData, 'south_north', y  
  dObj->WriteVarData, 'lon', lon  
  dObj->WriteVarData, 'lat', lat  
  if nz ne 0 then dObj->WriteVarData, z_var_name, z
    
  ;Checks
  if ~static then begin
    flag = 'ACTIVE'
    data_check = BYTARR(self.active_n_time)
    save, flag, data_check, FILENAME=self.active_checkfile
  endif else begin
    ; Fill with data
    data = reform((self.active_wrf->get_var(self.active_var.name))[*,*,0])    
    dObj->WriteVarData, 'time', 0
    dObj->WriteVarData, self.active_var.name, data    
  endelse
  
  ;log
  self.logger->addText, ' new file: ' + self.active_ofile, PRINT=print
  self.logger->flush
  
  return, dObj
  
end


;+
; :Description:
;   Adds the active variable data at the right place into the right file
;
;-
pro w_WPP::_add_var_to_h_file, MONTH=month

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
    
  ;Check for time ok (flag, data_check)
  restore, FILENAME=self.active_checkfile
  if flag ne 'ACTIVE' then message, 'flag?'
  if total(data_check[*self.active_index]) ne 0 then Message, 'Check?'
  
  p0 = min(*self.active_index)
  if KEYWORD_SET(month) then begin
    tref = QMS_TIME(year=self.active_year,month=month,day=1)
  endif else begin
    tref = QMS_TIME(year=self.active_year,month=1,day=1)
  endelse
  time = LONG(((*self.active_time)[*self.active_index]-tref) / (MAKE_TIME_STEP(hour=1)).dms)
  
  ; Vardata
  if self.active_var.type eq '3d_press' then pressure_levels = *self.pressure_levels
  data = self.active_wrf->get_var(self.active_var.name, dummy, varnt, UNSTAGGER=self.active_var.unstagger, PRESSURE_LEVELS=pressure_levels)
  ; Set some tolerance level to avoid underflows
  pu = where(abs(data) lt (machar()).eps, cntu)
  if cntu ne 0 then data[pu] = 0.  
  
  dObj = self.active_dObj
  dObj->SetMode, /DATA
  ok = dObj->HasDim('time', OBJECT=tObj)
  tObj->parseDimension
  
  case (self.active_var.type) of
    '2d': begin
      if varnt eq 1 then begin
        ; "false static" case
        time=time[0]
        offset=[0,0,dObj->GetDimValue('time')]
      endif else begin 
        ; normal case
        data = data[*,*,1:*]
        offset=[0,0,p0]
      endelse
    end
    else:  begin
      data = data[*,*,*,1:*]
      offset=[0,0,0,p0]
    end
  endcase
  
  ;If this is fake, replace with nans
  if ~ self.active_valid then data[*] = !VALUES.F_NAN
    
  ; Fill with data
  dObj->WriteVarData, 'time', time, OFFSET=offset[N_ELEMENTS(offset)-1]
  dObj->WriteVarData, self.active_var.name, data, OFFSET=offset
  
  ;Checks
  flag = 'ACTIVE'
  data_check[*self.active_index] = 1
  if TOTAL(data_check) eq N_ELEMENTS(data_check) then flag = 'DONE'
  save, flag, data_check, FILENAME=self.active_checkfile
   
end

;+
; :Description:
;   Adds the active variable data at the right place into the right file
;
;-
pro w_WPP::_add_var_to_mean_file, ts, PRINT=print

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  if self.active_var.type eq 'static' then return
  
  ;Check for time ok (flag, data_check)
  restore, FILENAME=self.active_checkfile
  if flag ne 'ACTIVE' then message, 'flag?'
    
  self.active_wrf->get_time, wtime, wnt
    
  dObj = self.active_dObj
  dObj->SetMode, /DATA  
  for i=0, N_ELEMENTS(ts)-2 do begin
    case (self.active_agg) of
      'd': begin
        T0 = wtime[min(where(wtime gt ts[i]))]
        T1 = wtime[where(wtime eq ts[i+1])]
      end
      else: begin
        T0 = wtime[where(wtime eq ts[i])]
        T1 = wtime[max(where(wtime lt ts[i+1]))]
      end
    endcase 
    ; Some logging
    self.active_wrf->getProperty, PATH=path
    self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + '   Now reading var ' + self.active_var.name + ' from file: ' + path + ' ...', PRINT=print
    data = self.active_wrf->get_var(self.active_var.name, vartime, varnt, T0=t0, T1=t1)
    self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + '   Ok. Dimensions: [' + STRJOIN(str_equiv(SIZE(data, /DIMENSIONS)), ',') + ']   Starting aggregation: ' + TIME_to_STR(ts[i], /NOTI) + ' to ' + TIME_to_STR(ts[i+1], /NOTI), PRINT=print
    agg_method = str_equiv(self.active_wrf->get_VAtt(self.active_var.name, 'agg_method'))   
    if str_equiv(agg_method) eq 'WIND' then agg_method = 'MEAN'
    if self.active_agg eq 'm' or self.active_agg eq 'y' then vartime += (MAKE_TIME_STEP(DAY=1)).dms
    TS_AGG_GRID, data, vartime, agg, agg_time, AGG_METHOD=agg_method, NEW_TIME=[ts[i],ts[i+1]]
    self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + '   Ok. Dimensions: [' + STRJOIN(str_equiv(SIZE(agg, /DIMENSIONS)), ',') + ']. Now compute the number of valid values.', PRINT=print
    ; Set some tolerance level to avoid underflows
    pu = where(abs(agg) lt (machar()).eps, cntu)
    if cntu ne 0 then agg[pu] = 0.    
    TS_AGG_GRID, TEMPORARY(data), vartime, sig, agg_time, AGG_METHOD='N_SIG', NEW_TIME=[ts[i],ts[i+1]]
    sig = TEMPORARY(sig) / float(varnt)
    pno = where(sig lt 0.5, cntno)
    if cntno ne 0 then agg[pno] = !VALUES.F_NAN
    case (self.active_agg) of
      'd': t = LONG((ts[i]-QMS_TIME(year=self.active_year,month=1,day=1)) / (MAKE_TIME_STEP(day=1)).dms)
      'm': t = (MAKE_ABS_DATE(QMS=ts[i])).month - 1
      'y': t = 0
      else: MESSAGE, 'type not OK'
    endcase
    self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + '   Ok. writing to file for index: ' + str_equiv(i) +'...', PRINT=print
    dObj->WriteVarData, 'time', t, OFFSET=i
    offset = (self.active_var.type EQ '2d') ? [0,0,i] : [0,0,0,i]
    dObj->WriteVarData, self.active_var.name, agg, OFFSET=offset
  endfor
  
  flag = 'DONE'
  save, flag, data_check, FILENAME=self.active_checkfile

  
end

;+
; :Description:
;    Gives an opportunity to the user to check if all required files to process
;    one year are available in the input directory. If the user doesn't do 
;    it, w_WPP::process will throw a message if not enough files are available
;
; :Params:
;    year: in, required
;          the year to check
;
; :Keywords:
;    FILES: out
;           the list of the files for this year (as many filepaths as days).
;           Where files are missing the link to the first valid file is given 
;           as "template". You can can check the validity of the files with 
;           the output keyword `VALID`
;    NMISSING: out
;              the number of missing files (if any)
;    DAYSMISSING: out
;                 the days of missing files (if any)
;    VALID: out
;           byte array of same size as `FILES`
;
; :Returns:
;    1 if enough files have been found, 0 if not
;-
function w_WPP::check_filelist, year, $
    FILES=files, $
    NMISSING=nmissing, $
    DAYSMISSING=daysmissing, $
    VALID=valid, $
    MONTH = month

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel    
    undefine, files
    ok = WAVE_Error_Message(!Error_State.Msg)
    return, 0
  ENDIF

  if ~arg_okay(year, /NUMERIC) then Message, WAVE_Std_Message('YEAR', /ARG)
  if self.domain eq 1 then h=3 else h=1
  if KEYWORD_SET(month) then begin
    t0 = QMS_TIME(year=year,month=month,day=1,hour=h)
    if month eq 12 then begin
      t1 = QMS_TIME(year=year+1,month=1,day=1,hour=0)
    endif else begin
      t1 = QMS_TIME(year=year,month=month+1,day=1,hour=0)
    endelse   
    ndays = GEN_month_days(month, year)
    pattern = '*d'+STRING(self.domain, FORMAT='(I02)')+'_'+ STRING(year, FORMAT='(I4)') +'*'+ $
      STRING(month, FORMAT='(I02)')+'*'
    ts = MAKE_ENDED_TIME_SERIE(QMS_TIME(year=year,day=1,month=month), $
      QMS_TIME(year=year,day=ndays,month=month), $
      TIMESTEP=D_QMS)
  endif else begin
    t0 = QMS_TIME(year=year,month=1,day=1,hour=h)
    t1 = QMS_TIME(year=year+1,month=1,day=1,hour=0)
    ; check if files for this year are here
    GEN_date_doy, ret, ndays, YEAR=year, month=12, day=31
    pattern = '*d'+STRING(self.domain, FORMAT='(I02)')+'_'+ STRING(year, FORMAT='(I4)') +'*'
    ts = MAKE_ENDED_TIME_SERIE(QMS_TIME(year=year,day=1,month=1), $
      QMS_TIME(year=year,day=31,month=12), $
      TIMESTEP=D_QMS)
  endelse
  time = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(HOUR=h), NSTEPS=nt)
  self.active_n_time = nt
  PTR_FREE, self.active_time
  self.active_time = PTR_NEW(time)
  
  matches = Where(StrMatch(*self.ifiles, pattern), nfiles)
  if nfiles eq 0 then Message, 'Zero files found, big problem.'  
  files = (*self.ifiles)[matches] ; Just this one year first  
  files = files[UNIQ(files,SORT(files))]
  
  ; Now check for the single days
  valid = BYTARR(ndays) + 1B 
  ofiles = STRARR(ndays)
  for i=0L, ndays-1 do begin
    pattern = '*d'+STRING(self.domain, FORMAT='(I02)')+'_'+ TIME_to_STR(ts[i], MASK='yyyy*mm*dd') +'_*'
    matches = Where(StrMatch(files, pattern), nfiles)
    if nfiles eq 1 then begin
      ofiles[i] = files[matches]       
    endif else begin
      valid[i] = 0
      ofiles[i] = files[0]
    endelse
  endfor
  files = ofiles
  pmissing = where(~ valid, nmissing)
  if nmissing ne 0 then daysmissing = ts[pmissing]
  if nmissing eq 0 then return, 1 else return, 0
   
end

;+
; :Description:
;    Process static files
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;
;-
pro w_WPP::process_static, PRINT=print, FORCE=force

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    if OBJ_VALID(self.logger) then begin
      self.logger->addError
      obj_destroy, self.Logger
    endif else begin
      ok = WAVE_Error_Message(!Error_State.Msg)
    endelse
    return
  endif
  
  if N_ELEMENTS(PRINT) eq 0 then print = 1    
  
  ; Logger
  logf = self.log_directory + '/wpp_process_static_' + str_equiv(year) + '_log_' + TIME_to_STR(QMS_TIME(), MASK='YYYY_MM_DD_HHTTSS') + '.log'
  self.Logger = Obj_New('ErrorLogger', logf, ALERT=1, DELETE_ON_DESTROY=0, TIMESTAMP=0)
  
  obj_destroy, self.active_wrf
  ptr_free, self.active_time
  ptr_free, self.active_index
  
  logt0 = SYSTIME(/SECONDS)
   
  self.logger->addText, TIME_to_STR(QMS_TIME()) + '. Start to process static files...', PRINT=print
  self.logger->addText, '', PRINT=print
       
  ; Define
  ; Tpl Object  
  if self.do_cache then file = caching((*self.ifiles)[0], CACHEPATH=self.cachepath, logger=self.logger, PRINT=print) $
    else file = (*self.ifiles)[0]
  self.active_wrf = OBJ_NEW('w_WRF', FILE=file)
 
  self.logger->addText, 'Generating product files ...', PRINT=print
  self.logger->flush
  
  vars = (*self.vars)
  for i=0, self.n_vars-1 do begin
    self->_set_active_var, vars[i], 2000, 's', IS_STATIC=is_static
    if ~ is_static then continue    
    obj = self->_define_file(FORCE=force, PRINT=print)
    if ~ OBJ_VALID(obj) then Message, 'Problem by static file definition'
    OBJ_DESTROY, obj
  endfor
  
  self.logger->addText, 'Done generating product files', PRINT=print
  self.logger->addText, '', PRINT=print

  OBJ_DESTROY, self.active_wrf    
  if self.do_cache then file = caching((*self.ifiles)[0], CACHEPATH=self.cachepath, /DELETE, logger=self.logger, PRINT=print)
      
  delta =  LONG(SYSTIME(/SECONDS) - logt0)
  deltah =  delta / 60L / 60L
  deltam =  (delta-(deltaH*60L*60L)) / 60L
  deltas =  delta-(deltaH*60L*60L)-(deltaM*60L)
  
  self.logger->addText, '', PRINT=print
  self.logger->addText, TIME_to_STR(QMS_TIME()) + '. Done.', PRINT=print
  self.logger->addText, 'Time needed: ' + str_equiv(deltaH) + ' hours, ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '+ Static files processed successfully +', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->Flush
  
  self.active_n_time = 0
  PTR_FREE, self.active_time
  PTR_FREE, self.active_index
  OBJ_DESTROY, self.logger

end

;+
; :Description:
;    Process one year into hourly files
;    
; :Params:
;    year: in, required, type=numeric
;          the year to process
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;    NO_PROMPT_MISSING: in, optional, type=boolean, default=0
;                        if set the programm will not ask you for permission if files 
;                        are missing (dangerous)
;-
pro w_WPP::process_h, year, PRINT=print, FORCE=force, NO_PROMPT_MISSING=no_prompt_missing, MONTH = month

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    for d=0, N_ELEMENTS(objs)-1 do begin
      o = objs[d]
      obj_Destroy, o
    endfor
    undefine, objs
    if OBJ_VALID(self.logger) then begin
      self.logger->addError
      obj_destroy, self.Logger
    endif else begin
      ok = WAVE_Error_Message(!Error_State.Msg)
    endelse
    return
  endif
  
  if N_ELEMENTS(PRINT) eq 0 then print = 1    
  
  ; Logger
  case (self.domain) of
    1: dom_str = 'd30km'
    2:  dom_str = 'd10km'
    else: dom_str = 'd02km'
  endcase
  if self.dom_suffix ne '' then dom_str += self.dom_suffix
  logf = self.log_directory + '/wpp_process_h_' + dom_str + ' ' + str_equiv(year) + '_log_' + TIME_to_STR(QMS_TIME(), MASK='YYYY_MM_DD_HHTTSS') + '.log'
  self.Logger = Obj_New('ErrorLogger', logf, ALERT=1, DELETE_ON_DESTROY=0, TIMESTAMP=0)

  obj_destroy, self.active_wrf
  ptr_free, self.active_time
  ptr_free, self.active_index

  
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Start to process hourly files for year ' + str_equiv(year) + ' ...', PRINT=print
  self.logger->addText, '', PRINT=print  
  logt0 = SYSTIME(/SECONDS)
 
  if ~ self->check_filelist(year, FILES=files, NMISSING=nmissing, VALID=valid, DAYSMISSING=daysmissing, MONTH = month) then begin
  
   self.logger->addText, 'Not enough files to complete (missing ' + str_equiv(nmissing) + ')', PRINT=print
   for d=0, nmissing-1 do self.logger->addText, ' Missing day: ' + TIME_to_STR(daysmissing[d], MASK='YYYY.MM.DD'), PRINT=print
 
   messg = 'Continue without those files? (y or n)'
   if KEYWORD_SET(NO_PROMPT_MISSING) then begin
     Message, messg, /INFORMATIONAL
     Print, 'y'
     messg = 'y'
   endif else begin
     READ, messg, PROMPT=messg
   endelse
   GEN_str_log, ret, messg, ok   
   if ~ ok then Message, 'Stopped. Not enough files to aggregate year : ' + str_equiv(year)
   endif
      
  ; Define
  ; Tpl Object  
  if self.do_cache then file = caching(files[0], CACHEPATH=self.cachepath, logger=self.logger, PRINT=print) $
    else file = files[0]
  self.active_wrf = OBJ_NEW('w_WRF', FILE=file)
  self.active_valid = valid[0]
  
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Generating product files ...', PRINT=print
  self.logger->flush
  
  vars = (*self.vars)
  objs = OBJARR(self.n_vars)
  for i=0, self.n_vars-1 do begin
    self->_set_active_var, vars[i], year, 'h', IS_STATIC=is_static
    if is_static then continue
    objs[i] = self->_define_file(FORCE=force, PRINT=print, MONTH = month)
  endfor
  
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Done generating product files', PRINT=print
  self.logger->addText, '', PRINT=print

  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Now start to fill with data ...', PRINT=print
  self.logger->flush
  
  if self.domain eq 1 then ntperday = 8 else ntperday = 24
  
  for f=0, N_ELEMENTS(files)-1 do begin
  
    logti = SYSTIME(/SECONDS)
    if f ne 0 then begin
      if self.do_cache then file = caching(files[f], CACHEPATH=self.cachepath, logger=self.logger, PRINT=print) $
        else file = files[f]
      self.active_wrf = OBJ_NEW('w_WRF', FILE=file)
      self.active_valid = valid[f]
    endif
        
    PTR_FREE, self.active_index
    self.active_index = PTR_NEW(INDGEN(ntperday)+ntperday*f)
    
    ; Some useless check but one never knows
    if self.active_valid then begin
      self.active_wrf->get_time, wtime, wnt, wt0, wt1
      if self.domain eq 1 then if wnt ne 9 then Message, 'Times in original WRF file?'
      if self.domain ge 2 then if wnt ne 25 then Message, 'Times in original WRF file?'
      p0 = where(*self.active_time eq wtime[1], cnt)
      if cnt ne 1 then Message, 'T0 not found?'
      p1 = where(*self.active_time eq wt1, cnt)
      if cnt ne 1 then Message, 'T1 not found?'
      nt = p1-p0+1
      if self.domain eq 1 then if nt ne 8 then Message, 'Expected 8 time steps. Found: '+str_equiv(nt)
      if self.domain ge 2 then if nt ne 24 then Message, 'Expected 24 time steps. Found: '+str_equiv(nt)
      if TOTAL(*self.active_index - (INDGEN(nt)+p0[0])) ne 0 then Message, 'Aaaarg. (Unexpected time in file)'
    endif
    
    if self.active_valid then begin
      self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Process ' +TIME_to_STR((*self.active_time)[(*self.active_index)[0]], /NOTIME)+ $
             ' . Indexes in file : [' + str_equiv(min(*self.active_index)) + ',' + $
                str_equiv(max(*self.active_index)) + '] from ' + str_equiv(self.active_n_time-1) + $
                   ' ...', PRINT=print     
    endif else begin
       self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' File is missing: ' +TIME_to_STR((*self.active_time)[(*self.active_index)[0]], /NOTIME)+ $
             ' . Filling with NaNs ...', PRINT=print     
    endelse              
    self.logger->flush
    
    vars = (*self.vars)
    for i=0, self.n_vars-1 do begin
      self->_set_active_var, vars[i], year, 'h', objs[i], IS_STATIC=is_static
      if is_static then continue
      self->_add_var_to_h_file, MONTH = month
    endfor
    
    ;Clean
    OBJ_DESTROY, self.active_wrf    
    if self.do_cache then begin     
     del = f eq N_ELEMENTS(files)-1
     if ~ del then del = self.active_valid or valid[f+1]
     if del then file = caching(files[f], CACHEPATH=self.cachepath, /DELETE, logger=self.logger, PRINT=print)
    endif
    
    ; Log
    delta =  LONG(SYSTIME(/SECONDS) - logti)
    deltam =  delta / 60L 
    deltas =  delta-(deltaM*60L)   
    self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Done. Needed: ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.', PRINT=print
    self.logger->flush
  endfor
    
  ;final check
  vars = (*self.vars)
  for i=0, self.n_vars-1 do begin
    self->_set_active_var, vars[i], year, 'h', IS_STATIC=is_static
    if is_static then continue
    restore, FILENAME=self.active_checkfile
    if flag ne 'DONE' then print, 'File check for all indexes failed. Big problem: ' + self.active_ofile
    file_delete, self.active_checkfile
  endfor
  
  for d=0, N_ELEMENTS(objs)-1 do begin
    o = objs[d]
    if OBJ_VALID(o) then begin
      o->Close_File
      obj_Destroy, o
    endif
  endfor
  undefine, objs
  
  delta =  LONG(SYSTIME(/SECONDS) - logt0)
  deltah =  delta / 60L / 60L
  deltam =  (delta-(deltaH*60L*60L)) / 60L
  deltas =  delta-(deltaH*60L*60L)-(deltaM*60L)
  
  self.logger->addText, '', PRINT=print
  self.logger->addText, TIME_to_STR(QMS_TIME()) + '. Done.', PRINT=print
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Time needed: ' + str_equiv(deltaH) + ' hours, ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' + Hourly files for year '+STRING(year,FORMAT='(I4)')+' processed successfully +', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->Flush
  
  self.active_n_time = 0
  PTR_FREE, self.active_time
  PTR_FREE, self.active_index
  OBJ_DESTROY, self.logger
  
end

;+
; :Description:
;    Process one year into daily, monthly and yearly files (must follow a call from process_h)
;    
; :Params:
;    year: in, required, type=numeric
;          the year to process
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;
;-
pro w_WPP::process_means, agg, year, PRINT=print, FORCE=force, MONTH = month

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    if OBJ_VALID(self.logger) then begin
      self.logger->addError
      obj_destroy, self.Logger
    endif else begin
      ok = WAVE_Error_Message(!Error_State.Msg)
    endelse
    return
  endif
 
  if ~(agg eq 'd' or agg eq 'm' or agg eq 'y') then Message, '$AGG not valid' 
 

  logf = self.log_directory + '/wpp_process_m_' + agg + '_' + str_equiv(year) + '_log_' + TIME_to_STR(QMS_TIME(), MASK='YYYY_MM_DD_HHTTSS') + '.log'
  self.Logger = Obj_New('ErrorLogger', logf, ALERT=1, DELETE_ON_DESTROY=0, TIMESTAMP=0)
    
  if N_ELEMENTS(PRINT) eq 0 then print = 1    
  
  obj_destroy, self.active_wrf
  ptr_free, self.active_time
  ptr_free, self.active_index
  if KEYWORD_SET(month) then begin
    t0 = QMS_TIME(year=year,month=month,day=1,hour=h)
    if month eq 12 then begin
      t1 = QMS_TIME(year=year+1,month=1,day=1,hour=0)
    endif else begin
      t1 = QMS_TIME(year=year,month=month+1,day=1,hour=0)
    endelse
  endif else begin
    t0 = QMS_TIME(year=year, month=01, day=01, hour=0)
    t1 = QMS_TIME(year=year+1, month=01, day=01, hour=0)  
  endelse
  
  case (agg) of
    'd': begin
      ts = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(DAY=1), NSTEPS=nsteps)
    end
    'm': begin
      ts = MAKE_ENDED_TIME_SERIE(t0, t1, MONTH=1, NSTEPS=nsteps)
    end
    'y': begin
      ts = [t0,t1]
      nsteps=2
    end    
    else: MESSAGE, 'type not OK'
  endcase  
  
  self.active_n_time = nsteps-1
  self.active_time = PTR_NEW(ts[0:nsteps-2])
  
  logt0 = SYSTIME(/SECONDS)
     
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Start to process mean files (' + agg + ') for year ' + str_equiv(year) + ' ...', PRINT=print
  self.logger->addText, '', PRINT=print
  
  vars = (*self.vars)
  for i=0, self.n_vars-1 do begin
    if (vars[i]).type eq 'static' then continue    
    if (vars[i]).agg_method eq 'no' then continue
    case (agg) of
      'd': self->_set_active_var, vars[i], year, 'h'
      'm': self->_set_active_var, vars[i], year, 'd'
      'y': self->_set_active_var, vars[i], year, 'd'
    endcase    
    hfile = self.active_ofile
    if ~ FILE_TEST(hfile) then message, 'Necessary file not here. You sure you did everything in the right order?'
    self->_set_active_var, vars[i], year, agg   
    self.active_wrf = OBJ_NEW('w_WRF', FILE=hfile)
    dobj = self->_define_file(FORCE=force, PRINT=print)
    self.active_dObj = dobj
    self->_add_var_to_mean_file, ts, PRINT=print
    undefine, dObj, self.active_wrf
  endfor
    
  ;final check
  vars = (*self.vars)
  for i=0, self.n_vars-1 do begin
    self->_set_active_var, vars[i], year, agg
    if self.active_var.type eq 'static' then continue
    if self.active_var.agg_method eq 'no' then continue
    restore, FILENAME=self.active_checkfile
    if flag ne 'DONE' then print, 'File check for all indexes failed. Big problem: ' + self.active_ofile
    file_delete, self.active_checkfile
  endfor
    
  delta =  LONG(SYSTIME(/SECONDS) - logt0)
  deltah =  delta / 60L / 60L
  deltam =  (delta-(deltaH*60L*60L)) / 60L 
  deltas =  delta-(deltaH*60L*60L)-(deltaM*60L)
  
  self.logger->addText, '', PRINT=print
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Done.', PRINT=print
  self.logger->addText, '[' + TIME_to_STR(QMS_TIME()) + ']' + ' Time needed: ' + str_equiv(deltaH) + ' hours, ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '+ Mean (' + self.active_agg + ') files for year '+STRING(year,FORMAT='(I4)')+' processed successfully +', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->Flush
  
  self.active_n_time = 0
  PTR_FREE, self.active_time
  PTR_FREE, self.active_index
  OBJ_DESTROY, self.logger

end

;+
; :Description:
;    Process one year means for d, m, y files
;    
; :Params:
;    year: in, required, type=numeric
;          the year to process
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;
;-
pro w_WPP::process_all_means, year, PRINT=print, FORCE=force, MONTH=month

  self->process_means, 'd', year, PRINT=print, FORCE=force, MONTH=month
  self->process_means, 'm', year, PRINT=print, FORCE=force, MONTH=month
  if ~KEYWORD_SET(month) then begin
    self->process_means, 'y', year, PRINT=print, FORCE=force
  endif
end



;+
; :Description:
;    Process one year into ALL files
;    
; :Params:
;    year: in, required, type=numeric
;          the year to process
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;    NO_PROMPT_MISSING: in, optional, type=boolean, default=0
;                        if set the programm will not ask you for permission if files 
;                        are missing (dangerous)
;
;-
pro w_WPP::process, year, PRINT=print, FORCE=force, NO_PROMPT_MISSING=no_prompt_missing, MONTH=month

  self->process_h, year, PRINT=print, FORCE=force, NO_PROMPT_MISSING=no_prompt_missing, MONTH=month
  self->process_all_means, year, PRINT=print, FORCE=force, MONTH=month
  
end

;+
;   Class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;           class definition as a structure variable
;           
;-
pro w_WPP__Define, class

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  struct = { w_WPP_var   ,       $ ; Info container for one WRF output variable
    name       : ''      ,       $ ; Name of the variable
    description: ''      ,       $ ; Description of the variable
    unit       : ''      ,       $ ; Unit of the variable
    unstagger  : FALSE   ,       $ ; Must be unstaggered?
    agg_method : ''      ,       $ ; Aggregation method
    type       : ''              $ ; Type of the variable ('2d','3d_eta','3d_press','3d_soil')
    }
    
  struct = { w_WPP                       ,  $
    namelist_file         : ''           ,  $ ; Path to the namelist.wpp file
    domain                : 0L           ,  $ ; From the namelist: The domain to process
    dom_suffix            : ''           ,  $ ; From the namelist: a suffix to add to the domain identifier
    input_directory       : ''           ,  $ ; From the namelist: where to find the orginal WRF files
    log_directory         : ''           ,  $ ; From the namelist: where to put the log files
    product_directory     : ''           ,  $ ; From the namelist: where to put the product files
    search_pattern        : ''           ,  $ ; From the namelist: file search pattern
    v2d_file              : ''           ,  $ ; From the namelist: Path to the variables_2d_wpp.csv
    v3d_file              : ''           ,  $ ; From the namelist: Path to the variables_3d_wpp.csv
    vsoil_file            : ''           ,  $ ; From the namelist: Path to the variables_soil_wpp.csv
    vstatic_file          : ''           ,  $ ; From the namelist: Path to the variables_static_wpp.csv
    project_acronym       : ''           ,  $ ; From the namelist: project short name (to be added as prefix to the product filenames)
    created_by            : ''           ,  $ ; From the namelist: info string
    institution           : ''           ,  $ ; From the namelist: info string
    title                 : ''           ,  $ ; From the namelist: info string
    notes                 : ''           ,  $ ; From the namelist: info string
    compress_level        : 0L           ,  $ ; From the namelist: gzip compression level
    shuffle               : 0B           ,  $ ; From the namelist: suffle before compression yes or no
    do_cache              : 0B           ,  $ ; Use caching: default: yes
    cachepath             : ''           ,  $ ; path to the cache directory
    logger                : OBJ_NEW()    ,  $ ; Logger Object
    active_wrf            : OBJ_NEW()    ,  $ ; the active WRF object to take the info from
    active_dObj           : OBJ_NEW()    ,  $ ; the active ncdf file object to put the information into
    active_var            : {w_WPP_var}  ,  $ ; the active variable to define/fill
    active_valid          : 0B           ,  $ ; is the active file valid or should I replace it by NaNs?
    active_agg            : ''           ,  $ ; the active aggregation type ('h', 'd', 'm' or 'y')
    active_ofile          : ''           ,  $ ; the active output file to write into
    active_checkfile      : ''           ,  $ ; the active check save file (to be sure all times are written)
    active_ncloggerfile   : ''           ,  $ ; the active ncdf logger file (destroyed if no error)
    active_year           : 0L           ,  $ ; the active year
    active_n_time         : 0L           ,  $ ; the number of times in the year
    active_time           : PTR_NEW()    ,  $ ; the times in the year
    active_index          : PTR_NEW()    ,  $ ; the current indexes in time where to put the data
    n_ifiles              : 0L           ,  $ ; the number of input files
    ifiles                : PTR_NEW()    ,  $ ; input files in the input directory
    n_pressure_levels     : 0L           ,  $ ; Number of pressure levels
    pressure_levels       : PTR_NEW()    ,  $ ; From the namelist: pressure levels
    n_vars                : 0L           ,  $ ; Number or variables
    vars                  : PTR_NEW()       $ ; Array of {w_WPP_var}
    }
   
END
