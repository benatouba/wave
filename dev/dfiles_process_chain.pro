;+
; :Description:
;    This procedure extracts the corresponding WRF timeseries at the nearest point to a station location and adds it
;    to the TS_Station object.
;
; :Params:
;    statObj: in, required, type=ts_station object
;             The station to which the WRF timeseries should be added
;    wrfObj: in, required, type=wrf object
;            The WRF grid from which to retrieve the timeseries
;    stat_var: in, required, type=string
;             The station variable name
;    wrf_var: in, required, type=string
;            The corresponding WRF variable name
;
; :Keywords:
;    RES: in, required, type=int
;         The resolution of the WRF grid, only needed for naming purposes
;    MIN_DAYS: in, required, type=int, default value=3*365
;              The number of the minimum of days that should be valid for the chosen variable. (Stations with less days are rejected)
;    GRAD: in, optional, type=3d array
;          If set, the existing gradient array is used for computing the altitudinal gradient. Must be of same size as WRF data array
;    KERNEL_SIZE: in, optional, type=integer
;                 is the edge length of the kernel. Must be an odd number
;                 (e.g. KERNEL_SIZE=3 equates to a kernel size of 3x3=9 values)
;                 If set, the altitudinal gradient is computed and used for data correction.
;    DEFAULT_VAL: in, optional, default=0
;                 the default value set where no alt gradient can be computed (boundaries, invalid values).
;                 A reasonable value for temperature would be -0.0098
;    CLIP_MIN: in, optional
;              clip the computed gradient to a minimum value
;    CROPBORDER: in, optional, type=integer
;                number of grid points to crop on each side of the wrf grid
;
; :Author: CoK, 2012
;-
function dfiles_process_chain_get_data, statObj, wrfObj, stat_var, wrf_var, RES=res, MIN_DAYS=min_days, GRAD=grad, KERNEL_SIZE=kernel_size, DEFAULT_VAL=default_val, CLIP_MIN=clip_min, $
                                        CROPBORDER=cropborder

  @WAVE.inc
  COMPILE_OPT IDL2
  ; Standard error handling.
  ON_ERROR, 2
  
  ;Get station location, elevation
  statObj->getProperty, NAME=name, ID=id, LOC_X=loc_x, LOC_Y=loc_y, SRC=src, ELEVATION=elev
  ok = wrfObj->define_subset(CROPBORDER=cropborder)
  wrfObj->getProperty, nx=nx, ny=ny
  
  ;Test if station is in grid and remove stations near grid borders
  wrfObj->transform_LonLat, loc_x, loc_y, src, i_dst, j_dst 
  p = where((i_dst lt (-0.5)) or (j_dst lt (-0.5)) or (i_dst gt (nx+0.5)) or (j_dst gt (ny+0.5)), COMPLEMENT=p_in, NCOMPLEMENT=cnt_in)
  ok=wrfObj->define_subset()
  if cnt_in eq 0 then return, FALSE
  
  ;Test if variable is valid (days missing)
  varObj = statObj->getVar(stat_var)
  datval = varObj->valid()
  nbval = TOTAL(datval)
  if N_ELEMENTS(MIN_DAYS) eq 0 then min_days=3*365
  if nbval lt min_days then return, FALSE
  
  ;Get WRF nearest station location and height
  w_dat=wrfObj->get_TimeSerie(wrf_var, $
    loc_x, loc_y, time, nt, $
    src = src, $
    t0 = t0, $
    t1 = t1, $
    dist_x = dist_x, $
    dist_y = dist_y, $
    point_i = w_i, $
    point_j = w_j, $
    point_lon = w_lon, $
    point_lat = w_lat, $
    unit= w_unit, $
    description = w_desc, $
    varname = varname , $ ;
    dims = dims, $ ;
    dimnames = dimnames )
    
  h=wrfObj->get_var('hgt') ;STATIC KEYWORD NOT AVAILABLE FOR WPR
  
     if w_i eq nx then w_i-=1
   if w_j eq ny then w_j-=1

  dist=sqrt(dist_x^2+dist_y^2)
  dh=elev-h[w_i, w_j] ; wenn w_j= 180..müsst man nicht -1 settzen?
  
  ;######Altitudinal Gradient#######
  if N_ELEMENTS(KERNEL_SIZE) ne 0 then begin
  
  
  
    ngrid=long(kernel_size/2)
    CORNERS=[w_i-ngrid, w_j-ngrid, w_i+ngrid, w_j+ngrid]
    ok=wrfObj->define_subset(CORNERS=CORNERS)
    alt_v=wrfObj->get_var(wrf_var)
    alt_h=wrfObj->get_var('hgt')
    ok=wrfObj->define_subset()
     
    g=w_altitudinal_gradient(alt_v, alt_h, KERNEL_SIZE=kernel_size, DEFAULT_VAL=default_val, CLIP_MIN=clip_min, SIG=sign)
    grad=REFORM(g[ngrid,ngrid,*])
    sig=REFORM(sign[ngrid,ngrid,*])
  endif
  ;########Use existing GRAD array if given#####
  if N_ELEMENTS(GRAD) ne 0 then begin
    s=size(grad)
    if s[0] gt 1 then  w_grad = w_dat + dh * grad[w_i, w_j, *] else $
      w_grad=w_dat+dh*grad
  endif
  
  ;########Add WRF data to station######
  
  statObj->setProperty, DESCRIPTION='NCDC ; WRF'+string(res, FORMAT='(I2)')+' ; '+str_equiv(stat_var)+' ; Time UTC  ; Dist '+string(dist, FORMAT='(I5)')+' ; dH '+string(dh, FORMAT='(I5)')
  
  var = OBJ_NEW('w_ts_Data', w_dat, time, NAME='WRF'+string(res, FORMAT='(I2)')+'_'+stat_var+'_orig', DESCRIPTION=w_desc, UNIT=w_unit, $
    VALIDITY='INTERVAL', MISSING='NaN', TIMESTEP=MAKE_TIME_STEP(day=1) )
  if OBJ_VALID(var) then statObj->addVar, var, /REPLACE
  
  if N_ELEMENTS(grad) ne 0 then begin
    var = OBJ_NEW('w_ts_Data', w_grad, time, NAME='WRF'+string(res, FORMAT='(I2)')+'_'+stat_var+'_corr', DESCRIPTION=w_desc, UNIT=w_unit, $
      VALIDITY='INTERVAL', MISSING='NaN', TIMESTEP=MAKE_TIME_STEP(day=1))
    if OBJ_VALID(var) then statObj->addVar, var, /REPLACE
    
    
    var = OBJ_NEW('w_ts_Data', grad, time, NAME='WRF'+string(res, FORMAT='(I2)')+'_'+stat_var+'_grad', DESCRIPTION='Altitudinal gradient' , UNIT=w_unit+'/m', $
      VALIDITY='INTERVAL', MISSING='NaN', TIMESTEP=MAKE_TIME_STEP(day=1) )
    if OBJ_VALID(var) then statObj->addVar, var, /REPLACE
    
    var = OBJ_NEW('w_ts_Data', sig, time, NAME='WRF'+string(res,FORMAT='(I2)')+'_'+stat_var+'_gsig', DESCRIPTION='Altitudinal gradient significance', UNIT='', $
      VALIDITY='INTERVAL', MISSING='NaN', TIMESTEP=MAKE_TIME_STEP(day=1))
    if OBJ_VALID(var) then statObj->addVar, var, /REPLACE
  endif
  
  return, TRUE
  
end



;+
; :Description:
;    This procedure retrieves timeseries from WRF10 and WRF30 products corresponding to given NCDC stations.
;    The NCDC station data and the extracted WRF timeseries are saved in one ASCII file.
;
; :Params:
;    nc_var: in, required, type=string
;             The NCDC station variable name
;    wrf_var: in, required, type=string
;            The corresponding WRF variable name
;
; :Keywords:
;    MIN_DAYS: in, required, type=int
;              The number of the minimum of days that should be valid for the chosen variable. (Stations with less days are rejected)
;    GRAD: in, optional, type=3d array
;          If set, the existing gradient array is used for computing the altitudinal gradient. Must be of same size as WRF data array
;    KERNEL_SIZE: in, optional, type=integer
;                 is the edge length of the kernel. Must be an odd number
;                 (e.g. KERNEL_SIZE=3 equates to a kernel size of 3x3=9 values)
;                 If set, the altitudinal gradient is computed and used for data correction.
;    DEFAULT_VAL: in, optional, default=0
;                 the default value set where no alt gradient can be computed (boundaries, invalid values).
;                 A reasonable value for temperature would be -0.0098
;    CLIP_MIN: in, optional
;              clip the computed gradient to a minimum value
;
; :Author: CoK, 2012
;-
pro dfiles_process_chain, nc_var, wrf_var, MIN_DAYS=min_days, GRAD=grad, KERNEL_SIZE=kernel_size, DEFAULT_VAL=default_val, CLIP_MIN=clip_min, SAV=sav

  @WAVE.inc
  COMPILE_OPT IDL2
  ; Standard error handling.
  ON_ERROR, 2
  
  ;#####Get WRF and Station data#####
  ;  wrf30=OBJ_NEW('w_WPR', DIRECTORY='V:\TIP-PR1\WET\d30km\d')
  ;  wrf10=OBJ_NEW('w_WPR', DIRECTORY='V:\TIP-PR1\WET\d10km\d')
  wrf30=OBJ_NEW('w_WPR', DIRECTORY='D:\Studium\Arbeit\zhadang products\d30km\d')
  wrf10=OBJ_NEW('w_WPR', DIRECTORY='D:\Studium\Arbeit\zhadang products\d10km\d')
  
  if KEYWORD_SET(SAV) then begin
  RESTORE, 'D:/Actual workspace/Dipl_stuff/NCDC_STAT_30km_cropped.sav' 
  ncdc->keepVar, nc_var
  endif else $
  ncdc=w_ncdc_read_gsod_file( DIRECTORY='D:\Studium\diplomarbeit\DATA\ncdc_filtered_30', KEEP_VARS=nc_var)
  
  ;ncdc=w_ncdc_read_gsod_file( DIRECTORY='D:\Studium\diplomarbeit\DATA\test_data', KEEP_VARS=nc_var)
  
  
  root='D:\Studium\diplomarbeit\DATA\ncdc_wrf_data\'
  
  d1cnt=0
  d2cnt=0
  
  ids = ncdc->getStatIds(Count=cnt)
  
  for i=0, cnt-1 do begin
  
  
    statObj = ncdc->getStat(STATID=ids[i])
    
    statObj->getProperty, NAME=stat_name
    
    
    ok=dfiles_process_chain_get_data(statObj, wrf30, nc_var, wrf_var, RES=30, MIN_DAYS=min_days, KERNEL_SIZE=kernel_size, $
      DEFAULT_VAL=default_val, CLIP_MIN=clip_min, CROPBORDER=10)
    if ~ok then continue
    
    statObj->write_ASCII_file, FILE=root+str_equiv(nc_var)+'/d30km'+'/NCDC_WRF30_'+str_equiv(nc_var)+'_'+ids[i]+'.dat', TITLE='NCDC/WRF30 Timeline'
    d1cnt+=1
    
    ok=dfiles_process_chain_get_data(statObj, wrf10, nc_var, wrf_var, RES=10, MIN_DAYS=min_days, KERNEL_SIZE=kernel_size, $
      DEFAULT_VAL=default_val, CLIP_MIN=clip_min, CROPBORDER=5)
    if ~ok then continue
    
    statObj->write_ASCII_file, FILE=root+str_equiv(nc_var)+'/d10km'+'/NCDC_WRF10_'+str_equiv(nc_var)+'_'+ids[i]+'.dat', TITLE= 'NCDC/WRF10 Timeline'
    d2cnt+=1
    
    
  endfor
   print, string(d1cnt, FORMAT='(I5)')+' out of '+string(cnt, FORMAT='(I5)')+' NCDC stations were saved for variable '+nc_var[0]+' in domain 1.'
    
   print, string(d2cnt, FORMAT='(I5)')+' NCDC stations were saved for variable '+nc_var[0]+' in domain 2.'
  
end