PRO AMSR_E__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  struct = { AMSR_E               ,  $
            INHERITS Grid2D           ,  $
            file:               ''    ,  $
            type:               ''    ,  $ ; type of data granule: 'HDF'
            sea_ice:        PTR_NEW() ,  $ ; Sea_ICe_Daily_Tile
            time:      {ABS_DATE}     $ ; first available time
            }
    
END


pro AMSR_do_HDFEOS_V2_14, var, HDFVersion, METAdata, x0, y0, x1, y1, dx, dy, nx, ny, proj, datum, a, es, lon_0, lat_0, lat_ts

  ;list of values you want to have extracted
  required = ['GridName', 'UpperLeftPointMtrs', 'LowerRightMtrs', 'Projection', 'ProjParams', 'XDim', 'YDim']
  meta = strarr(n_elements(required))
  
  ;Checks if meta data section is available
  if n_elements(METAdata) eq 0 then begin
    print, "Meta data section of ", HDFVersion, " not found!"
    stop
  endif
  
  ;prepare meta data string for extraction  
  METAdata = STRSPLIT(METAdata, STRING(10b), /EXTRACT) ;split at newlines
  METAdata = STRCOMPRESS(METAdata) ;compress grouped blanks to only one
  METAdata = STRTRIM(METAdata, 2) ;cut leading and trailing blanks
  
  ;extract projection info
  found = 0
  for i=0, n_elements(METAdata)-1 do begin
    if(STRPOS(METAdata[i], var) ne -1) then begin 
      found = 1
      break
    endif else begin
      str_tmp = STRSPLIT(METAdata[i], '=', /EXTRACT)
      p = where(STRCMP(required, str_tmp[0], /FOLD_CASE), cnt) 
      if cnt eq 1 then meta[p] = str_tmp[1]
    endelse
  endfor
  
  ;projection values for requested variable found?
  if (found eq 0) then begin
    print, "Field ", var, " not found in meta data! (", HDFVersion, ")"
    stop
  endif

  ;all requested meta data found?
  for i=0, n_elements(meta)-1 do begin
    if meta[i] eq '' then begin
      print, required[i], " not found in meta data!"
      stop
    endif 
  endfor
  
  ;Copying number of grid points
  nx = long(meta[where(STRCMP(required, 'XDim', /FOLD_CASE))])
  ny = long(meta[where(STRCMP(required, 'YDim', /FOLD_CASE))])
  nx = nx[0]
  ny = ny[0]
  
  ;Copying upper left point values
  str_tmp = meta[where(STRCMP(required, 'UpperLeftPointMtrs', /FOLD_CASE))]
  str_bgn = STRPOS(str_tmp, '(')
  str_end = STRPOS(str_tmp, ')')
  if str_bgn eq -1 or str_end eq -1 or str_bgn ge str_end then stop
  str_tmp = STRSPLIT(STRMID(str_tmp, str_bgn+1, str_end-str_bgn-1), ',', /EXTRACT)
  if n_elements(str_tmp) ne 2 then stop
  x0 = float(str_tmp[0]) 
  y0 = float(str_tmp[1]) 

  ;Copying lower right point values
  str_tmp = meta[where(STRCMP(required, 'LowerRightMtrs', /FOLD_CASE))]
  str_bgn = STRPOS(str_tmp, '(')
  str_end = STRPOS(str_tmp, ')')
  if str_bgn eq -1 or str_end eq -1 or str_bgn ge str_end then stop
  str_tmp = STRSPLIT(STRMID(str_tmp, str_bgn+1, str_end-str_bgn-1), ',', /EXTRACT)
  if n_elements(str_tmp) ne 2 then stop
  x1 = float(str_tmp[0]) 
  y1 = float(str_tmp[1])

  ;Conversion of projection into Proj4 notation
  str_tmp = meta[where(STRCMP(required, 'Projection', /FOLD_CASE))]
  switch str_tmp of
    'GCTP_PS': begin
      proj = 'stere'
      datum = 'NAD83'
      break
    end
    else: begin
      print, "Unknown projection: ", str_tmp
      stop
    end
  endswitch

  ;Conversion of projection parameters into Proj4 notation
  str_tmp = meta[where(STRCMP(required, 'ProjParams', /FOLD_CASE))]
  str_bgn = STRPOS(str_tmp, '(')
  str_end = STRPOS(str_tmp, ')')
  if str_bgn eq -1 or str_end eq -1 or str_bgn ge str_end then stop
  str_tmp = STRSPLIT(STRMID(str_tmp, str_bgn+1, str_end-str_bgn-1), ',', /EXTRACT)
  if n_elements(str_tmp) ne 13 then stop
  a = float(str_tmp[0])
  es = float(str_tmp[1]) * (-1.0)
  lon_0 = float(str_tmp[4]) / 1000000.0
  lat_ts = float(str_tmp[5]) / 1000000.0
  
  ;Conversion of grid name into distance and lat_0
  str_tmp = meta[where(STRCMP(required, 'GridName', /FOLD_CASE))]
  str_tmp = STRMID(str_tmp, 1, strlen(str_tmp)-2)
  switch str_tmp of
    'NpPolarGrid25km': begin
      lat_0 = 90.0
      dx = 25000.0
      dy = 25000.0
      break
    end
    'SpPolarGrid25km': begin
      lat_0 = -90.0
      dx = 25000.0
      dy = 25000.0
      break
    end
    'NpPolarGrid12km': begin
      lat_0 = 90.0
      dx = 12500.0
      dy = 12500.0
      break
    end
    'SpPolarGrid12km': begin
      lat_0 = -90.0
      dx = 12500.0
      dy = 12500.0
      break
    end
    else: begin
      print, "Unknown grid: ", str_tmp
      stop
    end
  endswitch

end  

;----------------------------------------------------------------------------------
; .....
;----------------------------------------------------------------------------------

Function AMSR_E::Init, FILE = file

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
;  Catch, theError
;  IF theError NE 0 THEN BEGIN
;    Catch, /Cancel
;    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
;    RETURN, 0
;  ENDIF
  
  ;******************
  ; Check arguments *
  ;******************
  if not eos_exists() then return, 'EOS not supported. '
  if not KEYWORD_SET(file) then file = DIALOG_PICKFILE(TITLE='Please select AMSR file to read', /MUST_EXIST)
  if HDF_ISHDF(File) then self.type = 'HDF'
  if self.type eq '' then message, 'File type not supported'
  self.file = file
  
  var = 'SI_12km_NH_ICECON_DAY'
  
  ;open file descriptor
  Cdfid = HDF_SD_START(file, /READ)
  
  ;get number of datasets and global attributes
  HDF_SD_FILEINFO, Cdfid, num_datasets, num_attributes
  
  ;search for HDF version and meta data in global attributes and get the values
  for i=0, num_attributes-1 do begin
    HDF_SD_ATTRINFO, Cdfid, i, name=attr_name, data=attr_data
    switch attr_name of
      'HDFEOSVersion' : begin
        HDFVersion = attr_data
        break
      end
      'CoreMetadata.0': begin
        COREdata = attr_data
        break
      end
      'StructMetadata.0': begin
        METAdata = attr_data
        break
      end
    endswitch
  endfor
  
  ;destroy file descriptor
  HDF_SD_END, Cdfid
  
  ;HDF version found?
  if N_ELEMENTS(HDFVersion) eq 0 then begin
    message, "HDF Version not found"
    return, 0
  endif
  
  ;meta data found?
  if n_elements(COREdata) eq 0 then begin
    message, "Meta data not found"
    return, 0
  endif else begin
  
    ;extract time stamp from meta data string
    str_tmp = STRSPLIT(COREdata, 'OBJECT = RANGEBEGINNINGDATE', /EXTRACT, /REGEX)
    if n_elements(str_tmp) ne 3 then begin
      message, "Time information not found"
      return, 0
    endif
    str_tmp = STRSPLIT(str_tmp[1], 'END_', /EXTRACT, /REGEX)
    str_tmp = STRSPLIT(str_tmp[0], 'VALUE = ', /EXTRACT, /REGEX)
    str_tmp = STRSPLIT(str_tmp[1], '"', /EXTRACT, /REGEX)
    date = str_tmp[0]
    
    ;generate output file name
    time = MAKE_ABS_DATE(YEAR= STRMID(date,0,4), MONTH=STRMID(date,5,2), day = STRMID(date,8,2))
    self.time = time
  endelse
  
  ;run suitable method for found HDF version
  switch HDFVersion of
    'HDFEOS_V2.14': begin
      print, "Found HDF Version: ", HDFVersion
      AMSR_do_HDFEOS_V2_14, var, HDFVersion, METAdata, x0, y0, xn, yn, dx, dy, nx, ny, proj, datum, a, es, lon_0, lat_0, lat_ts
      break
    end
    else: begin
      message, "Unknown HDF Version: ", HDFVersion
      return, 0
    end
  endswitch
  
  ;initialize projection
  switch proj of
    'stere' : begin
      lat_ts = 70.0
      lon_0 = -45.0
      a = 6378273.0
      es = 0.006693883
      b = a * sqrt(1. - es)
      GIS_make_ellipsoid, ret, ell_ams, NAME='AMSR-E Ellipsoid', RA=6378273.0, RB=b
      GIS_make_datum, ret, dat_ams, NAME='AMSR-E-Ellipsoid', ELLIPSOID=ell_ams, DX=0, DY=0, DZ=0 ;Houghes, NAD 83      
      envi_proj_ams = 31 ;Polar Stereographic: a, b, lat0, lon0, x0, y0, [datum], name      
      proj_param_ams = str_equiv(envi_proj_ams) + ', ' + $ ;proj_id
        STRING(ell_ams.a, FORMAT='(F16.8)')+ ', ' + $       ;a
        STRING(ell_ams.b, FORMAT='(F16.8)') + ', ' + $      ;b
        STRING(lat_ts, FORMAT='(F16.8)') + ', ' + $ ;lat0
        STRING(lon_0, FORMAT='(F16.8)') + ', ' + $  ;lon0
        '0.0' + ', ' + $                    ;x0
        '0.0' + ', ' + $                    ;y0
        'AMSR-E Ellipsoid' + ', ' + $        ;datum
        'AMSR-E Polar Stereographic'        ;name        
       GIS_make_proj, ret, proj_ams, PARAM=proj_param_ams
      break
    end
    else: begin
      message, "Projection not implemented!"
      return, 0
    end
  endswitch
      
  meta = ''
  IF NOT self->grid2D::Init( $
    nx = nx             , $
    ny = ny             , $
    x0 = x0 + dx/2.     , $
    y0 = y0 - dy/2.     , $
    dx = dx             , $
    dy = dy             , $
    proj = proj_ams     , $
    meta = meta         , $
    _Extra = extra) THEN RETURN, 0
    
  Cdfid = HDF_SD_START(file, /READ)
  dataset_idx = HDF_SD_NAMETOINDEX(Cdfid, var)
  dataset_id = HDF_SD_SELECT(Cdfid, dataset_idx)
  HDF_SD_GETDATA, dataset_id, sea_ice
  HDF_SD_END, Cdfid  
  
  self.sea_ice = PTR_NEW(ROTATE(sea_ice,7), /NO_COPY)
    
  RETURN, 1
  
END

pro AMSR_E::Cleanup

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  

  Ptr_Free, self.lon 
  Ptr_Free, self.lat
  Ptr_Free, self.sea_ice
 
END

PRO AMSR_E::GetProperty,             $
            file = file               ,  $
            type = type               ,  $ ; type of data granule: 'HDF'
            Sea_ice = sea_ice         ,  $ ; Snow_Cover_Daily_Tile
            time = time               ,  $ ; first available time
            _Ref_Extra=extra
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  IF Arg_Present(file) NE 0 THEN file = self.file             
  IF Arg_Present(type) NE 0 THEN type = self.type             
  IF Arg_Present(type) NE 0 THEN snow_cov = *self.sea_ice      
  IF Arg_Present(time) NE 0 THEN time = self.time          
  
  self->GRID2D::GetProperty, _Extra=extra
  
end

function AMSR_E::get_time

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  return, self.time
    
end

function AMSR_E::get_Sea_ice

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  return, *self.sea_ice
    
end

function AMSR_E::plot_Sea_ice

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  var = self->get_Sea_ice()
  
  varname ='Sea Ice'
  
  nd = N_ELEMENTS(dimnames)
  
  self->Get_LonLat, lon, lat
  
  QuickPLot, var, COLORTABLE=13, TITLE= varname, WINDOW_TITLE='Sea Ice view: ' + self.fname, CBARTITLE='sea ice', $
                    COORDX=lon, COORDY=lat
    
end
