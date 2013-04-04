;+
; :Description: 
;     Creates a subset file of products containing any number of 2D variables.
; 
; :Params:
;    input_dir: in, required
;               the path to the path to the WRF files directory. either m, d, h or y 
;               
;    destFile: in, required
;              path to the output file
;              
;    varlist: in, required
;             list of variables to put in the files
; 
; :Keywords:
; 
;    CLOBBER: in, type = boolean
;            Set this keyword if you are opening a netCDF file that already exists and 
;            you want to overwrite the existing file. Input. Default is 0.
;            
;    DO_PLOT: in, type = boolean
;             Set this keyword to a path where you want to save the plot
;            
;    NETCDF4_FORMAT: in, type = boolean
;                    Set this keyowrd to create a new NetCDF 4 file
;                    
;    GZIP:       Set this keyword to an integer between zero and nine to specify the level 
;                of GZIP compression applied to the variables. (NETCDF4 only)          
;                  
;    SHUFFLE:    Set this keyword to apply the shuffle filter to the variable. (NETCDF4 only) 
;    
;    YEARS: See W_WPR->setYear
;    
;    REMOVE_MASK: in, type = array
;                 a mask of the same size as the product where to 
;                 replace data with nans
;    SHAPE: in, type = string
;           the shapefile to read (.shp), coordinate system defined by `SRC`
;    POLYGON: in, type = array
;             not implemented yet, coordinate system defined by `SRC`
;    MASK: in, type = array
;          a mask of the desired ROI (dimensions must match the grid)
;    GRID: in, type = w_Grid2D
;          a grig object
;    CORNERS: in, type = array 
;            LL and UR corners of the desired subset ([XLL,YLL,XUR,YUR]), coordinate system defined by `SRC`
;    SRC: in, optional
;         the polygon or shape coordinate system (datum or proj) default is WGS-84
;    REMOVE_ENTITITES:in, optional, type = long
;                     an array containing the id of the shape entities to remove from the shape
;                     All other entities are plotted normally.
;    KEEP_ENTITITES:in, optional, type = long
;                   an array containing the id of the shape entities to keep for the shape. 
;                   All other entities are ignored.
;    ROI_MASK_RULE: Set this keyword to an integer specifying the rule used to determine whether
;                   a given pixel should be set within the mask when computing a mask with
;                   polygons or shapes. Valid values include::
;                       * 0 = Boundary only. All pixels falling on a region's boundary are set.
;                       * 1 = Interior only. All pixels falling within the region's boundary, but not on the boundary, are set.
;                       * 2 = Boundary + Interior. All pixels falling on or within a region's boundary are set.
;                       * 3 = Pixel center point is used to test the appartenance to the ROI. This is the default!
;    MARGIN: in
;            set to a positive integer value to add a margin to the subset
;            (MARGIN=1 will put one grid point on each side of the subset, so two
;             more columns per dimension in total)
;
;
; :History:
;     Written by FaM & JuC, 2012.


pro w_pr_selectregion, input_dir, destFile, varlist,   $
    GZIP=gzip, SHUFFLE=shuffle, REMOVE_MASK=remove_mask, $
    NETCDF4_FORMAT=NETCDF4_FORMAT, CLOBBER=clobber,   $
    DO_PLOT=do_plot,   $
    YEARS=years,   $
    SHAPE=shape,  $
    POLYGON=polygon, MASK=mask,  $
    GRID=grid,    $
    CORNERS=corners, $
    SRC=src, $
    REMOVE_ENTITITES=remove_entitites, $
    KEEP_ENTITITES=keep_entitites, $
    MARGIN=margin, $
    ROI_MASK_RULE=roi_mask_rule
    
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  wpr = OBJ_NEW('w_WPR', DIRECTORY=input_dir)
  if ~ OBJ_VALID(wpr) then message, 'wpr obj not valid'
  
  ; Make a subset around the shape
  ok = wpr->definesubset(SHAPE=shape, MARGIN=margin, POLYGON=polygon, MASK=mask, GRID=grid, CORNERS=corners, SRC=src, $
                         REMOVE_ENTITITES=remove_entitites, KEEP_ENTITITES=keep_entitites, ROI_MASK_RULE=roi_mask_rule)
  wpr->Get_LonLat, lon, lat, nx, ny
  wpr->Get_XY, xx, yy, nx, ny
  wpr->getProperty, TNT_C=c
  wrf_proj = c.proj
  wpr->setYears, years
  wpr->getTime, time, nt, t0, t1
  
  ; Check for variables
  for i=0, N_ELEMENTS(varlist)-1 do if ~ wpr->hasVar(varlist[i]) then message, 'Variable: ' + varlist[i] + ' not found.'
  
  ;Check if pixels have to be masked
  _do_mask = 0
  if N_ELEMENTS(REMOVE_MASK) ne 0 then begin
     s = SIZE(remove_mask, /DIMENSIONS)
     if N_ELEMENTS(s) ne 2 then Message, WAVE_Std_Message('remove_mask', /NDIMS)
     if s[0] ne nx then Message, WAVE_Std_Message('remove_mask', /NELEMENTS)
     if s[1] ne ny then Message, WAVE_Std_Message('remove_mask', /NELEMENTS)
     pmask = where(remove_mask, cntmask)
     if cntmask eq 0 then Message, 'Nothing in mask?'
     _do_mask = 1
  endif
  
   if N_ELEMENTS(do_plot) ne 0 then begin
     if arg_okay(do_plot, TNAME='string') then pfile = do_plot
     map = OBJ_NEW('w_Map', wpr, YSIZE=600)
     ok = map->set_img(/HR_BLUE_MARBLE)
     ok = map->set_topography(/DEFAULT)
     if N_ELEMENTS(SHAPE) ne 0 then ok = map->set_shape_file(SHPFILE=shape, SHP_SRC=src)
     wpr->Get_XY, x, y, nx, ny, proj     
     colors = REPLICATE('red', nx*ny) 
     if _do_mask then colors[where(remove_mask)] = 'grey'
     for i=0, nx*ny-1 do ok = map->set_filled_point(x[i], y[i], SRC=proj, COLOR=colors[i])
     w_standard_2d_plot, map, PNG=pfile
   endif
  
  ;find a template file
  wpr->GetProperty, DIRECTORY=dir, HRES=res
  file_list=FILE_SEARCH(dir, '*_' + res +'_*.nc', count=filecnt)
  
  ; Open the source file in read-only mode.
  sObj = Obj_New('NCDF_FILE', file_list[0], /TIMESTAMP)
  IF Obj_Valid(sObj) EQ 0 THEN Message, 'Source object cannot be created.'
  
  ; Open the destination file for writing.
  dObj = Obj_New('NCDF_FILE', destFile, /CREATE, /TIMESTAMP, NETCDF4_FORMAT=netcdf4_format, CLOBBER=clobber)
  IF Obj_Valid(dObj) EQ 0 THEN Message, 'Destination object cannot be created.'
    
  ; Copy the Global attributes that we want to keep
  keep = ['TITLE', $
          'WRF_VERSION', $
           'CREATED_BY', $
           'INSTITUTION', $
           'CREATION_DATE', $
           'PROJECTION', $
           'PROJ_ENVI_STRING', $
           'DATUM', $
           'TIME_ZONE', $
           'GRID_INFO', $
           'DX', 'DY', $
           'X0', 'Y0', $
           'PRODUCT_LEVEL', $
           'LEVEL_INFO']           
  for j=0, N_ELEMENTS(keep)-1 do begin
    sObj->CopyGlobalAttrTo, keep[j], dObj
  endfor
  undefine, sObj
  
  ; Actualise the wrong ones
  dObj->WriteGlobalAttr, 'CREATION_DATE', TIME_to_STR(QMS_TIME()), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'X0',  STRING(min(xx), FORMAT='(F12.1)'), DATATYPE='CHAR'
  dObj->WriteGlobalAttr, 'Y0',  STRING(min(yy), FORMAT='(F12.1)'), DATATYPE='CHAR'
  
  
  ; dimensions  Time, south_north, west_east
  x_dim_name = 'west_east'
  y_dim_name = 'south_north'
  t_dim_name = 'time'
  dObj->WriteDim, t_dim_name, nt
  dObj->WriteDim, x_dim_name, nx
  dObj->WriteDim, y_dim_name, ny
  
  ; Variables
  vn = 'time'
  dObj->WriteVarDef, vn, t_dim_name, DATATYPE='LONG'
  dObj->WriteVarAttr, vn, 'long_name', 'Time'
  dObj->WriteVarAttr, vn, 'units', 'hours since 2000-01-01 00:00:00'
  ncdftime = LONG((time-QMS_TIME(year=2000,month=1,day=1, HOUR=0)) / (MAKE_TIME_STEP(HOUR=1)).dms)
  dObj->WriteVarData, vn, ncdftime
  
  vn = 'west_east'
  dObj->WriteVarDef, vn, x_dim_name, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'x-coordinate in cartesian system'
  dObj->WriteVarAttr, vn, 'units', 'm'
  dObj->WriteVarData, vn, REFORM(xx[*,0])
  
  vn = 'south_north'
  dObj->WriteVarDef, vn, y_dim_name, DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'y-coordinate in cartesian system'
  dObj->WriteVarAttr, vn, 'units', 'm'
  dObj->WriteVarData, vn, REFORM(yy[0,*])
    
  vn = 'lon'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Longitude'
  dObj->WriteVarAttr, vn, 'units', 'degrees_east'
  dObj->WriteVarData, vn, lon
  
  vn = 'lat'
  dObj->WriteVarDef, vn, [x_dim_name,y_dim_name], DATATYPE='FLOAT'
  dObj->WriteVarAttr, vn, 'long_name', 'Latitude'
  dObj->WriteVarAttr, vn, 'units', 'degrees_north'
  dObj->WriteVarData, vn, lat
    
  for i=0, N_ELEMENTS(varlist)-1 do begin
    data = wpr->getVarData(varlist[i], INFO=inf)
    vn = inf.id
    if _do_mask then begin
      nt = N_ELEMENTS(data[0,0,*])
      data = REFORM(TEMPORARY(data), nx*ny, nt)
      data[where(remove_mask), *] = !VALUES.F_NAN
      data = REFORM(TEMPORARY(data), nx, ny, nt)
    endif
    dObj->WriteVarDef, vn, [x_dim_name,y_dim_name,t_dim_name], DATATYPE='FLOAT', GZIP=gzip, SHUFFLE=shuffle
    dObj->WriteVarAttr, vn, 'long_name', inf.description
    dObj->WriteVarAttr, vn, 'units', inf.unit
    dObj->WriteVarData, vn, TEMPORARY(data)
  endfor
 
  undefine, wpr, dObj
  
end