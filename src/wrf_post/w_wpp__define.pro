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
      obj_destroy, self.errorLogger
    endif else begin
      ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    endelse
    return, 0
  endif
  
  ; Ceck if everything needed is here  
  if N_ELEMENTS(NAMELIST) eq 0 then namelist = DIALOG_PICKFILE(TITLE='Please select the namelist.wpp file', /MUST_EXIST, FILTER='*.wpp')
  if namelist eq '' then Message, WAVE_Std_Message(/FILE)  
  self.namelist_file = namelist  
  if N_ELEMENTS(CACHING) eq 0 then self.do_cache = 1B else self.do_cache = caching
  if N_ELEMENTS(PRINT) eq 0 then print = 1    
  if ~ self->_Parse_Namelist() then Message, 'Unable to parse the namelist file. Please check it.'
  
  ; Let's go
  if self.do_cache then begin
    self.cachepath = self.output_directory + '/cache'
    FILE_MKDIR, self.cachepath
  endif
  
  ; Logger
  logf = self.output_directory + '/wpp_log_' + TIME_to_STR(QMS_TIME(), MASK='YYYY_MM_DD_HHTTSS') + '.log'
  self.Logger = Obj_New('ErrorLogger', logf, ALERT=1, DELETE_ON_DESTROY=0, TIMESTAMP=0)
  ; General info
  self.Logger->AddText, 'WPP logfile ' + self.title + ' - Domain ' + str_equiv(self.domain), PRINT=print
  self.Logger->AddText, '', PRINT=print  
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, 'Output directory: ' + self.output_directory, PRINT=print
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
  ptr_free, self.vars 
  
  file_delete, self.cachepath, /RECURSIVE
  
END

;+
; :Description:
;   Parse the namelist file.
;
;-
function w_WPP::_Parse_Namelist

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
      'OUTPUT_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.output_directory = utils_clean_path(val, /MARK_DIRECTORY)
      end
      'SEARCH_PATTERN': begin
        matches = Where(StrMatch(val, '*{domain}*'), count)
        if count gt 0 then self.search_pattern = val
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
      'OUTPUT_DIRECTORY': begin
        if FILE_TEST(val, /DIRECTORY) then self.output_directory = utils_clean_path(val, /MARK_DIRECTORY)
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
  if self.output_directory eq  '' then Message, 'Problem while parsing output_directory field.'
  if self.v2d_file eq '' then Message, 'Problem while parsing v2d_file field.'
  if self.v3d_file eq  '' then Message, 'Problem while parsing v3d_file field.'
  if self.vsoil_file eq '' then Message, 'Problem while parsing vsoil_file field.'
  if self.project_acronym eq '' then Message, 'Problem while parsing project_acronym field.'
  if self.search_pattern eq '' then Message, 'Problem while parsing search_pattern field.'
  
  return, 1
  
end

;+
; :Description:
;    Parse a variables file
;    
;-
function w_WPP::_Parse_VarDefFile, file, type, PRINT=print

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
;-
pro w_WPP::_set_active_var, var, year

  ON_ERROR, 2

  case (self.domain) of
    1: begin 
      dom_str = 'd30km'
    end
    2:  begin 
      dom_str = 'd10km'
    end    
    else: begin
      dom_str = 'd02km'    
    end
  endcase
  
  f_dir = dom_str+'/h/'+var.type
  f_name = self.project_acronym + '_' + utils_replace_string(f_dir, '/', '_') + '_' + var.name + '_' + STRING(year, FORMAT='(I4)') + '.nc'
  f_path = utils_clean_path(self.output_directory + '/'+ self.project_acronym +'/'+ f_dir + '/' + f_name)
  l_path = utils_clean_path(self.output_directory + '/logs_idl/' + f_dir + '/' + 'check_'+ utils_replace_string(f_name, '.nc', '.sav'))
  f_log = 'log_'+ utils_replace_string(f_name, '.nc', '.log')
  f_log = utils_clean_path(self.output_directory + '/logs_ncdf/' + f_log)
  
  FILE_MKDIR, FILE_DIRNAME(f_log)
  FILE_MKDIR, FILE_DIRNAME(f_path)
  FILE_MKDIR, FILE_DIRNAME(l_path)
  
  self.active_checkfile = l_path
  self.active_ncloggerfile = f_log
  self.active_ofile = f_path
  self.active_var = var
  
end

;+
; :Description:
;   Defines one file per variable per year
;
; :Keywords:
;    FORCE: in, optional
;           set this keyword to overwrite existing NCDF files in the directory
;    PRINT: in, optional, type=boolean, default=1
;           if set (default), the log messages are printed in the console as well
;
;-
pro w_WPP::_define_file, FORCE=force, PRINT=print

  ON_ERROR, 2
  
  case (self.active_var.type) of
    '2d': begin
       nz = 0    
       type_str = '2d'
    end
    '3d_eta': begin
       z_dim_name = 'eta'
       nz = self.active_wrf->get_Dim('bottom_top')
       self.active_wrf->get_Time, dum, dumy, t0
       z = self.active_wrf->get_var('ZNU', t0=t0, t1=t0)
       type_str = '3d/eta'
       z_var_name = 'eta'
       z_var_long_name = 'Eta Levels (mass points)'
       z_var_units = ''       
    end
    '3d_press': begin
       z_dim_name = 'pressure'
       nz = self.n_pressure_levels
       type_str = '3d/press'
       z = *self.pressure_levels
       z_var_name = 'pressure'
       z_var_long_name = 'Pressure Levels'
       z_var_units = 'hPa'      
    end
    '3d_soil': begin
       z_dim_name = 'soil'
       nz = self.active_wrf->get_Dim('soil_layers_stag')
       type_str = '3d/soil'
       self.active_wrf->get_Time, dum, dumy, t0
       z = TOTAL(self.active_wrf->get_var('DZS', t0=t0, t1=t0),/CUMULATIVE)
       z_var_name = 'soil'
       z_var_long_name = 'depths of lower boundaries of soil layers'
       z_var_units = 'm'      
    end
    else: message, 'Type not ok'
  endcase
  
  case (self.domain) of
    1: begin 
      nested_string = 'NO'
      grid_string = '30km'
      h=3
      nt_per_day = 8
    end
    2:  begin 
      nested_string = 'YES'
      grid_string = '10km'
      h=1
      nt_per_day = 24
    end    
    else: begin
      nested_string = 'YES'
      grid_string = '02km'
      h=1
      nt_per_day = 24
    end
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
  timestep_string = str_equiv(h) + ' hours'
  
  dObj = Obj_New('NCDF_FILE', self.active_ofile, /CREATE, /TIMESTAMP, /NETCDF4_FORMAT, CLOBBER=force, ErrorLoggerName=self.active_ncloggerfile)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
  dObj->SetMode, /DEFINE
  
  ; Dimensions  
  dObj->WriteDim, t_dim_name, /UNLIMITED
  dObj->WriteDim, x_dim_name, nx
  dObj->WriteDim, y_dim_name, ny
  if nz ne 0 then dObj->WriteDim, z_dim_name, nz
  
  ; Global attributes
  dObj->WriteGlobalAttr, 'TITLE', self.title, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DATA_NOTES', self.notes, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'WRF_VERSION', STRING(self.active_wrf->get_Gatt('TITLE')), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'CREATED_BY', self.created_by, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'INSTITUTION', self.institution , DATATYPE='CHAR' 
  dObj->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'SOFTWARE_NOTES', 'IDL V' + !VERSION.RELEASE + ', WAVE post V0.1' , DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'VARNAME', self.active_var.name, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'PROJECTION', proj_name , DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'PROJ_ENVI_STRING', proj_envi_string, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DATUM', datum, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'TIMESTEP', timestep_string, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'NESTED', nested_string, DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'TIME_ZONE', 'UTC', DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'GRID_INFO', 'Grid spacing: Global Attributes DX and DY (unit: m), ' + $
                                       'Down left corner: Global Attributes X0 and Y0 (unit: m) ', DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'DX', tnt_c.dx, DATATYPE='FLOAT'
  dObj->WriteGlobalAttr, 'DY', tnt_c.dy, DATATYPE='FLOAT'
  dObj->WriteGlobalAttr, 'X0', min(x), DATATYPE='FLOAT'
  dObj->WriteGlobalAttr, 'Y0', min(y), DATATYPE='FLOAT'
  
  ; Variables
  vn = 'time'
  dObj->WriteVarDef, vn, t_dim_name, DATATYPE='LONG'
  dObj->WriteVarAttr, vn, 'long_name', 'Time'
  dObj->WriteVarAttr, vn, 'units', 'hours since 2000-01-01 00:00:00'
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
  
  Obj_Destroy, dObj
  
  ;Checks
  flag = 'ACTIVE'
  data_check = BYTARR(self.active_n_time)
  save, flag, data_check, FILENAME=self.active_checkfile
  
  ;log
  self.logger->addText, ' new file: ' + self.active_ofile, PRINT=print
  self.logger->flush
  
end


;+
; :Description:
;   Adds the active variable data at the right place into the right file
;
;-
pro w_WPP::_add_var_to_file

  ON_ERROR, 2
  
  ;Check for time ok
  ; flag, data_check, FILENAME=l_path
  restore, FILENAME=self.active_checkfile
  if flag ne 'ACTIVE' then message, 'flag?'
  if total(data_check[*self.active_index]) ne 0 then Message, 'Check?'
  p0 = min(*self.active_index)
  
  tref = QMS_TIME(year=2000,month=1,day=1,hour=0)
  time = LONG(((*self.active_time)[*self.active_index]-tref) / (MAKE_TIME_STEP(hour=1)).dms)
  
  ; Vardata
  if self.active_var.type eq '3d_press' then pressure_levels = *self.pressure_levels
  data = self.active_wrf->get_var(self.active_var.name, UNSTAGGER=self.active_var.unstagger, PRESSURE_LEVELS=pressure_levels)
  
  case (self.active_var.type) of
    '2d': begin
      data = data[*,*,1:*]
      offset=[0,0,p0]
    end
    else:  begin
      data = data[*,*,*,1:*]
      offset=[0,0,0,p0]
    end
  endcase
  
  dObj = Obj_New('NCDF_FILE', self.active_ofile, /TIMESTAMP, /MODIFY, ErrorLoggerName=self.active_ncloggerfile)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
  
  ; Fill with data
  dObj->SetMode, /DATA
  dObj->WriteVarData, 'time', time, OFFSET=offset
  dObj->WriteVarData, self.active_var.name, data, OFFSET=offset
  Obj_Destroy, dObj
  
  ;Checks
  flag = 'ACTIVE'
  data_check[*self.active_index] = 1
  if TOTAL(data_check) eq N_ELEMENTS(data_check) then flag = 'DONE'
  save, flag, data_check, FILENAME=self.active_checkfile
  
end

;+
; :Description:
;    Gives an opportunity to the user to check if all required files to process
;    one year are available in the input directory. If the user doesn'tdo 
;    it, the process routine will.
;
; :Params:
;    year: in, required
;          the year to check
;
; :Keywords:
;    FILES: out
;           if successful, the list of the files found for this year
;
; :Returns:
;    1 if enough files have been found, 0 if not
;-
function w_WPP::check_filelist, year, FILES=files

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

  ;Time
  if self.domain eq 1 then h=3 else h=1
  t0 = QMS_TIME(year=year,month=1,day=1,hour=h)
  t1 = QMS_TIME(year=year+1,month=1,day=1,hour=0)
  time = MAKE_ENDED_TIME_SERIE(t0, t1, TIMESTEP=MAKE_TIME_STEP(HOUR=h), NSTEPS=nt)
  self.active_n_time = nt
  PTR_FREE, self.active_time
  self.active_time = PTR_NEW(time)
  
  ; Find necessary data
  GEN_date_doy, ret, ntofind, YEAR=year, month=12, day=31
  pattern = '*wrfpost_d'+STRING(self.domain, FORMAT='(I02)')+'_'+ STRING(year, FORMAT='(I4)') +'*'
  matches = Where(StrMatch(*self.ifiles, pattern), nfiles)
  
  if nfiles ne ntofind then return, 0
  
  files = (*self.ifiles)[matches]  
  files = files[UNIQ(files,SORT(files))]      
  if N_ELEMENTS(files) ne ntofind then return, 0
  
  return, 1
  
end

;+
; :Description:
;    Process one year
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
pro w_WPP::process, year, PRINT=print, FORCE=force

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  catch, theError
  if theError ne 0 then begin
    catch, /cancel 
    self.logger->addError
    return
  endif
  
  if N_ELEMENTS(PRINT) eq 0 then print = 1    
  
  OBJ_DESTROY, self.active_wrf
  ptr_free, self.active_time
  ptr_free, self.active_index
  
  logt0 = SYSTIME(/SECONDS)
  
  if ~ self->check_filelist(year, FILES=files) then Message, 'Not enough files to aggregate year : ' + str_equiv(year)
   
  self.logger->addText, TIME_to_STR(QMS_TIME()) + '. Start to process year ' + str_equiv(year) + ' ...', PRINT=print
  self.logger->addText, '', PRINT=print
       
  ; Define
  ; Tpl Object  
  if self.do_cache then file = caching(files[0], CACHEPATH=self.cachepath, logger=self.logger, PRINT=print) else file = files[0]
  self.active_wrf = OBJ_NEW('w_WRF', FILE=file)
 
  self.logger->addText, 'Generating product files ...', PRINT=print
  self.logger->flush
  
  vars = (*self.vars)
  for i=0, self.n_vars-1 do begin
    self->_set_active_var, vars[i], year
    self->_define_file, FORCE=force, PRINT=print
  endfor
  
  self.logger->addText, 'Done generating product files', PRINT=print
  self.logger->addText, '', PRINT=print

  self.logger->addText, 'Now start to fill with data ...', PRINT=print
  self.logger->flush
  
  for f=0, N_ELEMENTS(files)-1 do begin
  
    logti = SYSTIME(/SECONDS)
  
    if f ne 0 then begin
      if self.do_cache then file = caching(files[f], CACHEPATH=self.cachepath, logger=self.logger, PRINT=print) else file = files[f]
      self.active_wrf = OBJ_NEW('w_WRF', FILE=file)
    endif
    
    self.active_wrf->get_time, wtime, wnt, wt0, wt1
    if self.domain eq 1 then if wnt ne 9 then Message, 'Times in original WRF file?'
    if self.domain ge 2 then if wnt ne 25 then Message, 'Times in original WRF file?'
    
    ;Check for time ok
    p0 = where(*self.active_time eq wtime[1], cnt)
    if cnt ne 1 then Message, 'T0 not found?'
    p1 = where(*self.active_time eq wt1, cnt)
    if cnt ne 1 then Message, 'T1 not found?'
    nt = p1-p0+1
    if self.domain eq 1 then if nt ne 8 then Message, 'Times?'
    if self.domain ge 2 then if nt ne 24 then Message, 'Times?'
    
    PTR_FREE, self.active_index
    self.active_index = PTR_NEW(INDGEN(nt)+p0[0])
    
    self.logger->addText, 'Process ' +TIME_to_STR((*self.active_time)[p0], /NOTIME)+ $
             ' . Indexes in file : [' + str_equiv(min(*self.active_index)) + ',' + $
                str_equiv(max(*self.active_index)) + '] from ' + str_equiv(self.active_n_time-1) + $
                   ' ...', PRINT=print                   
    self.logger->flush
    
    vars = (*self.vars)
    for i=0, self.n_vars-1 do begin
      self->_set_active_var, vars[i], year
      self->_add_var_to_file, PRINT=print
    endfor
    OBJ_DESTROY, self.active_wrf
    
    if self.do_cache then file = caching(files[f], CACHEPATH=self.cachepath, /DELETE, logger=self.logger, PRINT=print)
    
    delta =  LONG(SYSTIME(/SECONDS) - logti)
    deltam =  delta / 60L 
    deltas =  delta-(deltaM*60L)
   
    self.logger->addText, ' Done. Needed: ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.', PRINT=print
    self.logger->flush
  endfor
    
  ;final check
  vars = (*self.vars)
  for i=0, self.n_vars-1 do begin
    self->_set_active_var, vars[i], year
    restore, FILENAME=self.active_checkfile
    if flag ne 'DONE' then print, 'noooo'
  endfor
  
  delta =  LONG(SYSTIME(/SECONDS) - logt0)
  deltah =  delta / 60L / 60L
  deltam =  (delta-(deltaH*60L*60L)) / 60L 
  deltas =  delta-(deltaH*60L*60L)-(deltaM*60L)
  
  self.logger->addText, '', PRINT=print
  self.logger->addText, TIME_to_STR(QMS_TIME()) + '. Done.', PRINT=print
  self.logger->addText, 'Time needed: ' + str_equiv(deltaH) + ' hours, ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->AddText, '+----------------------------------+', PRINT=print
  self.Logger->AddText, '+ Processing year '+STRING(year,FORMAT='(I4)')+' successfull +', PRINT=print
  self.Logger->AddText, '+----------------------------------+', PRINT=print
  self.Logger->AddText, '', PRINT=print
  self.Logger->Flush
  
  self.active_n_time = 0
  PTR_FREE, self.active_time
  PTR_FREE, self.active_index
  
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
    input_directory       : ''           ,  $ ; From the namelist: where to find the orginal WRF files
    output_directory      : ''           ,  $ ; From the namelist: where to put the aggregated files
    search_pattern        : ''           ,  $ ; From the namelist: file search pattern
    v2d_file              : ''           ,  $ ; From the namelist: Path to the variables_2d_wpp.csv
    v3d_file              : ''           ,  $ ; From the namelist: Path to the variables_3d_wpp.csv
    vsoil_file            : ''           ,  $ ; From the namelist: Path to the variables_soil_wpp.csv
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
    active_var            : {w_WPP_var}  ,  $ ; the active variable to define/fill
    active_ofile          : ''           ,  $ ; the active output file to write into
    active_checkfile      : ''           ,  $ ; the active check save file (to be sure all times are written)
    active_ncloggerfile   : ''           ,  $ ; the active ncdf logger file (destroyed if no error)
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