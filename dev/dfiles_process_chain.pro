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
;    GRAD_VAR: in, optional, type=3d array
;              the gradient variable name in WRF product 
;
; :Author: CoK, 2012
;-
function dfiles_process_chain_get_data, statObj, wrfObj, stat_var, wrf_var, GRAD_VAR=grad_var, RES=res, MIN_DAYS=min_days

  @WAVE.inc
  COMPILE_OPT IDL2
  ; Standard error handling.
  ON_ERROR, 2
  
  ;Get station location, elevation
  statObj->getProperty, NAME=name, ID=id, LOC_X=loc_x, LOC_Y=loc_y, SRC=src, ELEVATION=elev
  wrfObj->getProperty, nx=nx, ny=ny
    
  ;Test if station is in grid and remove stations near grid borders
  wrfObj->transform_LonLat, loc_x, loc_y, src, i_dst, j_dst 
  p = where((i_dst lt (-0.5)) or (j_dst lt (-0.5)) or (i_dst gt (nx-0.5)) or (j_dst gt (ny-0.5)), COMPLEMENT=p_in, NCOMPLEMENT=cnt_in)
  if cnt_in eq 0 then return, FALSE
  
  ;Test if variable is valid (days missing)
  varObj = statObj->getVar(stat_var)
  datval = varObj->valid()
  nbval = TOTAL(datval)
  if nbval lt min_days then return, FALSE
  
  ;Get WRF nearest station location and height
  w_dat = wrfObj->get_TimeSerie(wrf_var, $
    loc_x, loc_y, time,  $    
    src=src, $
    dist_x=dist_x, $
    dist_y=dist_y, $
    unit=w_unit, $
    description =w_desc)
  
  time += D_QMS ; mean time issue
    
  h = wrfObj->get_TimeSerie('hgt', loc_x, loc_y, src=src) 
  dist = sqrt(dist_x^2+dist_y^2)
  dh = elev - h 
  
  if N_ELEMENTS(GRAD_VAR) ne 0 then begin
    w_grad = wrfObj->get_TimeSerie(grad_var, $
      loc_x, loc_y, src=src, $
      unit= w_grad_unit, $
      description = w_grad_desc)
    w_corr = w_dat + dh * w_grad
  endif
  
  ;########Add WRF data to station######  
  statObj->setProperty, DESCRIPTION='NCDC;WRF'+string(res, FORMAT='(I2)')+';'+str_equiv(stat_var)+';Time=UTC;Dist='+string(dist, FORMAT='(I5)')+$
    ';dH='+string(dh, FORMAT='(I5)'+';wrfH='+string(h, FORMAT='(I5)')
  
  var = OBJ_NEW('w_ts_Data', w_dat, time, NAME='WRF'+string(res, FORMAT='(I2)')+'_'+str_equiv(stat_var)+'_orig', DESCRIPTION=w_desc, UNIT=w_unit, $
    VALIDITY='INTERVAL', MISSING='NaN', TIMESTEP=MAKE_TIME_STEP(day=1) )
  if OBJ_VALID(var) then statObj->addVar, var, /REPLACE
  
  if N_ELEMENTS(GRAD_VAR) ne 0 then begin
    var = OBJ_NEW('w_ts_Data', w_corr, time, NAME='WRF'+string(res, FORMAT='(I2)')+'_'+str_equiv(stat_var)+'_corr', DESCRIPTION=w_desc+' (height corrected)', UNIT=w_unit, $
      VALIDITY='INTERVAL')
    if OBJ_VALID(var) then statObj->addVar, var, /REPLACE        
    var = OBJ_NEW('w_ts_Data', w_grad, time, NAME='WRF'+string(res, FORMAT='(I2)')+'_'+str_equiv(stat_var)+'_grad', DESCRIPTION=w_grad_desc, UNIT=w_grad_unit, $
      VALIDITY='INTERVAL')
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
pro dfiles_process_chain, nc_var, wrf_var, GRAD_VAR=grad_var, SAV=sav

  @WAVE.inc
  COMPILE_OPT IDL2
  ; Standard error handling.
  ON_ERROR, 2
  
  ;#####Get WRF and Station data#####
  ;  wrf30=OBJ_NEW('w_WPR', DIRECTORY='V:\TIP-PR1\WET\d30km\d')
  ;  wrf10=OBJ_NEW('w_WPR', DIRECTORY='V:\TIP-PR1\WET\d10km\d')
  wrf30=OBJ_NEW('w_WPR', DIRECTORY='D:\Studium\Arbeit\zhadang products\d30km\d', CROPBORDER=10)
  wrf10=OBJ_NEW('w_WPR', DIRECTORY='D:\Studium\Arbeit\zhadang products\d10km\d', CROPBORDER=5)
  min_days=365*3
    
  if KEYWORD_SET(SAV) then begin
    RESTORE, 'D:/Actual workspace/Dipl_stuff/NCDC_STAT_30km_cropped.sav'
    ncdc->selVar, nc_var
  endif else $
    ncdc=w_ncdc_read_gsod_file(DIRECTORY='D:\Studium\diplomarbeit\DATA\NCDC', KEEP_VARS=nc_var)
    
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
  
  undefine, wrf10, wrf30
  
  print, string(d1cnt, FORMAT='(I5)')+' out of '+string(cnt, FORMAT='(I5)')+' NCDC stations were saved for variable '+nc_var[0]+' in domain 1.'
  print, string(d2cnt, FORMAT='(I5)')+' NCDC stations were saved for variable '+nc_var[0]+' in domain 2.'
  
end
