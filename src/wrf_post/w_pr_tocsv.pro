;+
; :Description: 
;     Creates a subset file of products containing any number of 2D variables.
;     The output format is CSV. That is, you can have one csv file per variable
;     and with different grid points (default when the ROI is a subset), or one file
;     with several variables (default when the ROI is a single grid point)
; 
; :Params:
;    input_dir: in, required
;               the path to the path to the WRF files directory. either m, d, h or y 
;               
;    dest: in, required
;          path to the output directory (case 1) or output file (case 2)
;             
;    varlist: in, required
;             list of variables to put in the files
; 
; :Keywords:
; 
;    CLOBBER: in, type = boolean
;            Set this keyword if you are opening a file that already exists and 
;            you want to overwrite the existing file. Input. Default is 0.  
;    YEARS: See W_WPR->setYear
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
;     Written by FaM, 2013.
;
;-
pro w_pr_tocsv, input_dir, destFile, varlist,  $ 
    CLOBBER=clobber,   $
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
  wpr->GetProperty, HRES=hres, TRES=tres
  
  h = wpr->getVarData('hgt')
  
  ; Check for variables
  for i=0, N_ELEMENTS(varlist)-1 do if ~ wpr->hasVar(varlist[i]) then message, 'Variable: ' + varlist[i] + ' not found.'
  
  ; Check the cases
  if (nx eq ny) and (nx eq 1) then begin
    ; timeseries case
    if FILE_TEST(destFile, /DIRECTORY) then begin
      Message, 'I want an output file path, not a directory.', /INFO
      return
    endif
    if FILE_TEST(destFile) and ~ KEYWORD_SET(CLOBBER) then begin
      Message, 'File already exists. Set /CLOBBER to overwrite.', /INFO
      return
    endif

    wpr->GetProperty, TNT_C=c
    GIS_make_datum, ret, _src
    GIS_coord_trafo, ret, CORNERS[0], CORNERS[1], x_dst, y_dst, SRC=_src, DST=c.proj
    dis = SQRT((xx[0]-x_dst)^2+(yy[0]-y_dst)^2) / 1000.
           
    id = utils_replace_string(FILE_BASENAME(destfile), '.dat', '')
    s = w_ts_Station(NAME=id, LOC_X=lon, LOC_Y=lat, ELEVATION=h, ID=id, DESCRIPTION='HAR closest pixel. Distance to point: ' + cgNumber_Formatter(dis) + ' km')  
    for i=0, N_ELEMENTS(varlist) - 1 do begin
      data = wpr->getVarData(varlist[i], time,  INFO=inf)
      name = inf.name
      description = inf.description
      unit = inf.unit
      if inf.type eq '3d_press' or inf.type eq '3d_eta' then Message, 'Not yet'
      if inf.type eq '3d_soil' then begin
        levs = str_equiv(LONG(wpr->getSoilLevels()*100))
        levs = ['_0-'+levs[0]+'cm', $
          '_' +levs[0]+'-'+levs[1]+'cm' , $
          '_' +levs[1]+'-'+levs[2]+'cm' , $
          '_' +levs[2]+'-'+levs[3]+'cm']
        for j=0, N_ELEMENTS(levs) - 1 do begin
          var = w_ts_Data(data[j,*], time, NAME=name+levs[j], DESCRIPTION=description, UNIT=unit)
          var->setPeriod
          s->addVar, var
        endfor
      endif else begin
        var = w_ts_Data(data, time, NAME=name, DESCRIPTION=description, UNIT=unit)
        var->setPeriod
        s->addVar, var
      endelse
    endfor
    s->setPeriod
    s->ASCIIwrite, FILE=destfile
  endif else begin
    ;ROI case
    
    if ~ FILE_TEST(destFile, /DIRECTORY) then begin
      Message, 'I want a directory.', /INFO
      return
    endif
    
    for v=0, N_ELEMENTS(varlist) - 1 do begin      
      vn = varlist[v]
      data = wpr->getVarData(vn, time, nt, INFO=inf)
      if inf.type eq '3d_press' or inf.type eq '3d_eta' or inf.type eq '3d_soil' then Message, 'Not yet'
      unit = inf.unit
      if vn eq 'PSFC' then begin
        data = 0.01 * data
        unit = 'hPa'
      endif
      if vn eq 'Q2' then begin
        data = 1000. * data
        unit = 'g.kg-1'
      endif
      vn = inf.id
      stat = OBJ_NEW('w_ts_Station', NAME=vn, DESCRIPTION=inf.description)
      ascii_file = destFile + '/'+hres+'_'+tres+'_' + vn +'.dat'
      ascii_title = 'WRF variable: ' + inf.id + '. Description: ' + inf.description + '. Unit: ' + unit + '.'
      data = reform(data, N_ELEMENTS(lon), nt)
      text = 'WRF_' + str_equiv(INDGEN(N_ELEMENTS(lon))+1)
      
      for i=0, N_ELEMENTS(lon)-1 do begin
        des = 'Model height: ' + str_equiv(FIX(h[i])) + ' m, Lon: ' + str_equiv(lon[i]) + ', Lat: ' + str_equiv(lat[i])
        stat->addVar, OBJ_NEW('w_ts_Data', data[i,*], time, NAME=str_equiv(text[i]), DESCRIPTION=des, UNIT=unit, VALIDITY='POINT')
      endfor  
      stat->setPeriod   
      stat->ASCIIwrite, FILE=ascii_file, TITLE=ascii_title, FORMAT=format
      undefine, stat      
    endfor
  endelse
  
  undefine, wpr, s
  
end