function w_test_wpp_file_directory, RESET=reset
  
  rootd = '/media/Fab_1TB/WPP_TEST_PACK/'
  
  if ~ FILE_TEST(rootd + 'pp_files') then message, 'Directory not valid'
  if ~ FILE_TEST(rootd + 'namelists') then message, 'Directory not valid'
  
  if KEYWORD_SET(RESET) then begin
    FILE_DELETE, rootd + 'out_idl', /RECURSIVE
    FILE_DELETE, rootd + 'products', /RECURSIVE
  endif
  
  return, rootd

end

pro w_test_wpp_make_prods, RESET=reset

  rootd = w_test_wpp_file_directory(RESET=reset)
  
  FILE_MKDIR, rootd + 'products'
  FILE_MKDIR, rootd + 'out_idl'
  
  logt0 = SYSTIME(/SECONDS)
  
  wpp = OBJ_NEW('w_WPP', NAMELIST=rootd + 'namelists/namelist_d01.wpp')
  wpp->process, 2008, /NO_PROMPT_MISSING
  wpp->process, 2009
  undefine, wpp
  wpp = OBJ_NEW('w_WPP', NAMELIST=rootd + 'namelists/namelist_d02.wpp')
  wpp->process, 2008, /NO_PROMPT_MISSING
  wpp->process, 2009
  undefine, wpp
  wpp = OBJ_NEW('w_WPP', NAMELIST=rootd + 'namelists/namelist_d08.wpp')
  wpp->process, 2008, /NO_PROMPT_MISSING
  wpp->process, 2009
  undefine, wpp  
  
  delta =  LONG(SYSTIME(/SECONDS) - logt0)
  deltam =  delta / 60L
  deltas =  delta-(deltaM*60L)
  print, ' Test products done. Needed: ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.'
  
end

function w_test_wpp_make_prods_pick_rdm_file, DOMAIN=domain

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  common W_TEST_WPP_CMN, seed, list_d01, list_d02, list_d03
  
  error = 0
  if N_ELEMENTS(list_d01) eq 0 then begin
    pattern = 'wrfpost_d01*zip'
    list_d01 = FILE_SEARCH(w_test_wpp_file_directory() + 'pp_files/2009', pattern, /EXPAND_ENVIRONMENT)
    pattern = 'wrfpost_d02*zip'
    list_d02 = FILE_SEARCH(w_test_wpp_file_directory() + 'pp_files/2009', pattern, /EXPAND_ENVIRONMENT)
    pattern = 'wrfpost_d08*zip'
    list_d03 = FILE_SEARCH(w_test_wpp_file_directory() + 'pp_files/2009', pattern, /EXPAND_ENVIRONMENT)
  endif
  
  cnt = 365
  
  if domain eq 1 then list = list_d01
  if domain eq 2 then list = list_d02
  if domain eq 3 then list = list_d03
    
  rdm = LONG(RANDOMU(Seed) * (cnt-1))
  rdm = 56
  
;  FILE_DELETE, w_test_wpp_file_directory() + 'cache/', /RECURSIVE
;  FILE_MKDIR, w_test_wpp_file_directory() + 'cache/'
  return, caching(list[rdm], CACHEPATH= w_test_wpp_file_directory() + 'cache/')
  
end



;pro w_test_wpp_nans, error
;
;  ; Set Up environnement
;  COMPILE_OPT idl2
;  @WAVE.inc
;  
;  error = 0
;  
;  rootd = w_test_wpp_file_directory() + 'products/'
;  
;  dom_str = ['d30km','d10km','d02km_06']
;  
;  for j = 0, 2 do begin
;  
;    f = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_t2_2008.nc'
;    w = OBJ_NEW('w_WRF', FILE=f)
;    t = w->get_Var('t2', time, nt)
;    pt = wherE(time eq QMS_TIME(year=2008, month=10, day = 1, hour=1))
;    p = WHERE(FINITE(t[*,*,0:pt-1]), cnt)
;    if cnt ne 0 then error += 1
;    p = WHERE(~FINITE(t[*,*,pt:*]), cnt)
;    if cnt ne 0 then error += 1
;    undefine, w
;    
;    f = rootd + dom_str[j] + '/m/2d/kin_' + dom_str[j] + '_m_2d_t2_2008.nc'
;    w = OBJ_NEW('w_WRF', FILE=f)
;    t = w->get_Var('t2', time, nt)
;    p = WHERE(FINITE(t[*,*,0:8]), cnt)
;    if cnt ne 0 then error += 1
;    p = WHERE(~FINITE(t[*,*,9:11]), cnt)
;    if cnt ne 0 then error += 1
;    undefine, w
;    
;    f = rootd + dom_str[j] + '/y/2d/kin_' + dom_str[j] + '_y_2d_t2_2008.nc'
;    w = OBJ_NEW('w_WRF', FILE=f)
;    t = w->get_Var('t2', time, nt)
;    p = WHERE(FINITE(t), cnt)
;    if cnt ne 0 then error += 1
;    undefine, w
;    
;    if error ne 0 then message, '% W_TEST_WPP_NANS NOT passed for domain ' +  dom_str[j], /CONTINUE else print, 'OK for domain ' +  dom_str[j]
;    
;  endfor
;  
;end

pro w_test_wpp_nans, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
    f = rootd + 'd02km_06/h/2d/kin_d02km_06_h_2d_t2_2008.nc'
    w = OBJ_NEW('w_WRF', FILE=f)
    t = w->get_Var('t2', time, nt)
    pt = wherE(time eq QMS_TIME(year=2008, month=10, day = 1, hour=1))
    p = WHERE(FINITE(t[*,*,0:pt-1]), cnt)
    if cnt ne 0 then error += 1
    p = WHERE(~FINITE(t[*,*,pt:*]), cnt)
    if cnt ne 0 then error += 1
    undefine, w
    
    f = rootd + 'd02km_06/m/2d/kin_d02km_06_m_2d_t2_2008.nc'
    w = OBJ_NEW('w_WRF', FILE=f)
    t = w->get_Var('t2', time, nt)
    p = WHERE(FINITE(t[*,*,0:8]), cnt)
    if cnt ne 0 then error += 1
    p = WHERE(~FINITE(t[*,*,9:11]), cnt)
    if cnt ne 0 then error += 1
    undefine, w
    
    f = rootd +  'd02km_06/y/2d/kin_d02km_06_y_2d_t2_2008.nc'
    w = OBJ_NEW('w_WRF', FILE=f)
    t = w->get_Var('t2', time, nt)
    p = WHERE(FINITE(t), cnt)
    if cnt ne 0 then error += 1
    undefine, w
    
    if error ne 0 then message, '% W_TEST_WPP_NANS NOT passed' ;, /CONTINUE else print, 'OK for domain ' +  dom_str[j]
  
end



pro w_test_wpp_landsurf, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc   

  error = 0  
  
  dom_str = ['d30km','d10km','d02km_06']

  rootd = w_test_wpp_file_directory() + 'products/'
  
  for j= 0, 2 do begin

  f = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_lu_index_2009.nc'  
  w = OBJ_NEW('w_WRF', FILE=f) 
  
  v = w->get_Var('lu_index', time, nt)
  
  if nt ne 365 then error += 1
  
;  map = OBJ_NEW('w_Map', w, YSIZE=600)
;  for i=0, nt-1 do begin
;    w_wrf_Landcover_map, map, w, bar_tags, bar_title, TO_PLOT=v[*,*,i]
;    w_standard_2d_plot, map, TITLE='LC ' + TIME_to_STR(time[i], /NOTIME), BAR_TITLE=bar_title, BAR_TAGS=bar_tags, $
;                         STD_PNG='LC_WRF30_map_' + TIME_to_STR(time[i], MASK='YYYY_MM_DD') + '.png' , /PIXMAP, XFACTOR=1.25                 
;  endfor
  
  undefine, w, map
  
  if error ne 0 then message, '% W_TEST_WPP_LANDSURF NOT passed for domain ' +  dom_str[j] ;, /CONTINUE else print, 'W_TEST_WPP_LANDSURF passed'
  
  endfor
  
end



pro w_test_prcp_c_h, error
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc   

  error = 0  

  rootd = w_test_wpp_file_directory()  + 'products/'
  
   dom_str = ['d30km','d10km','d02km_06']
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
  
  oId = NCDF_OPEN(f)   
  
  NCDF_VARGET, oId, 'RAINC', acc
  acc1 = acc[*,*,0:(N_elements(acc[0,0,*])-2)]
  acc2 = acc[*,*,1:(N_elements(acc[0,0,*])-1)]
  expected = acc2 -acc1
  NCDF_CLOSE, oId
  
  tpl = OBJ_NEW('w_WRF', FILE=f)
  tpl->get_time, time, nt, t0, t1
  t0 += time[1]-time[0]
;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=28, HOUR=03)
;  t1 = QMS_TIME(year=2009, MONTH=06, DAY=29, HOUR=00)
  undefine, tpl
  
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_c_2009.nc')
  totest = product->get_Var('PRCP_C', T0=t0, T1=t1)
  
  if total(ABS(expected-totest)) ne 0 then error += 1
;  print, total(ABS(expected-totest))
;  print, max(ABS(expected-totest))
  
  undefine, product
  
  if error ne 0 then message, '% W_TEST_WPP_PRCP_C_H NOT passed for domain ' + dom_str[j]  ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
  
  endfor
  
end


pro w_test_prcp_c_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  ;  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'RAINC', acc
    
    expected = acc[*,*,dom_div[j]] - acc[*,*,0]    ; for domain d01
    ;  expected = acc[*,*,24] - acc[*,*,0]   ; for domain d02 & d03
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/2d/kin_'+ dom_str[j] + '_d_2d_prcp_c_2009.nc')
;    totest = product->get_Var('PRCP_C')
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(YEAR=2008,day=28,month=06)
    undefine, tpl
    
    totest = product->get_Var('PRCP_C',T0=t0, T1=t0)
    
     mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;      print, mean_err, err_perc
    if max(err_perc) gt 0.005 then error += 1
    
;    if total(ABS(expected - totest)) gt 0.001 then error += 1
;    print, total(ABS(expected - totest))
;    print, max(ABS(expected - totest))
    
    NCDF_CLOSE, oId
    undefine, product, expected, totest
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_C_D NOT passed for domain ' + dom_str[j]  , /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
  endfor
  
end


pro w_test_prcp_c_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] +'_h_2d_prcp_c_2009.nc')
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('PRCP_C', t, nt, T0=t0, T1=t1)
;      expected = total(a, 3) / nt
      expected = total(a, 3)
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/2d/kin_'+ dom_str[j] +'_m_2d_prcp_c_2009.nc')
      totest = product_month->get_Var('PRCP_C', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
;      mean_val = max(ABS(expected))
;      err_perc = mean_err / mean_val
      ;      print, mean_err, err_perc
      if mean_err gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_2D_PRCP_C_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE
;      endif else print, 'W_TEST_WPP_PRCP_C_M OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      ;
      ;      if max(ABS(expected - totest)) gt 0.001 then error += 1
      ;      if error ne 0 then message, '% W_TEST_WPP_PRCP_C_M NOT passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE else print, 'W_TEST_WPP_PRCP_C_M OK for domain'+ dom_str[j] + ' month ' + str_equiv(i+1)
      
      undefine, product_month
      
    endfor
    
    undefine, product, product_month, expected
    
  endfor
  
end


pro w_test_prcp_c_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j = 0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_c_2009.nc')
    a = product->get_Var('PRCP_C')
    expected = total(a,3, /DOUBLE)
    ;  w_QuickPlot, expected, TITLE='orig'
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/2d/kin_' + dom_str[j] + '_y_2d_prcp_c_2009.nc')
    totest = product_year->get_Var('PRCP_C')
    
    ;  w_QuickPlot,totest, TITLE='prod'
    ;  w_QuickPlot, expected - totest, TITLE='diff'
    ;  print, total(ABS(expected - totest))
;      print, max(ABS(expected - totest))
    
      if max(ABS(expected - totest)) gt 0.01 then error += 1
    
;    mean_err = MAX(ABS(expected-totest))
;    mean_val = MEAN(ABS(expected))
;    err_perc = mean_err / mean_val
;    print, mean_err, mean_val
;    if max(err_perc) gt 0.005 then error += 1
;        print, max(err_perc)
    
    undefine, product, product_year, totest, expected
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_C_Y NOT passed for domain' + dom_str[j] ;, /CONTINUE else print, 'W_TEST_WPP_PRCP_C_Y passed'
    
  endfor
  
end


pro w_test_prcp_nc_h, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory()  + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'RAINNC', acc
    acc1 = acc[*,*,0:(N_elements(acc[0,0,*])-2)]
    acc2 = acc[*,*,1:(N_elements(acc[0,0,*])-1)]
    expected = acc2 -acc1
    NCDF_CLOSE, oId
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=28, HOUR=03)
    ;  t1 = QMS_TIME(year=2009, MONTH=06, DAY=29, HOUR=00)
    undefine, tpl
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_nc_2009.nc')
    totest = product->get_Var('PRCP_NC', T0=t0, T1=t1)
    
    if total(ABS(expected-totest)) ne 0 then error += 1
;    print, total(ABS(expected-totest))
;    print, max(ABS(expected-totest))
    
    undefine, product
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_NC_H NOT passed for domain' + dom_str[j] ;, /CONTINUE else print, 'W_TEST_WPP_PRCP_NC_H passed'
    
  endfor
  
end


pro w_test_prcp_nc_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  ;  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'RAINNC', acc
    
    expected = acc[*,*,dom_div[j]] - acc[*,*,0]
    
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/2d/kin_' + dom_str[j] + '_d_2d_prcp_nc_2009.nc')
;    totest = product->get_Var('PRCP_NC')
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(YEAR=2008,day=28,month=06)
    undefine, tpl
    
    totest = product->get_Var('PRCP_NC',T0=t0, T1=t0)
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;      print, mean_err, err_perc
    if max(err_perc) gt 0.005 then error += 1
    
;    if total(ABS(expected - totest)) gt 0.001 then error += 1
;    print, total(ABS(expected - b))
;    print, max(ABS(expected - b))
    
    NCDF_CLOSE, oId
    undefine, product
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_NC_D NOT passed_for domain' + dom_str[j]  ;, /CONTINUE else print, 'OK for domain' + dom_str[j]
    
  endfor
  
end


pro w_test_prcp_nc_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd+ dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_nc_2009.nc')
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('PRCP_NC', t, nt, T0=t0, T1=t1)
      expected = total(a, 3)
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/2d/kin_'+ dom_str[j] +'_m_2d_prcp_nc_2009.nc')
      totest = product_month->get_Var('PRCP_NC', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
      ;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_2D_PRCP_C_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE
;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      
      ;if max(ABS(expected - totest)) gt 0.001 then error += 1
      ;if error ne 0 then message, '% W_TEST_WPP_PRCP_C_M NOT passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE else print, 'OK for domain'+ dom_str[j] + ' month ' + str_equiv(i+1)
      
      undefine, product_month
      
    endfor
    
    undefine, product, totest, expected
    
  endfor
  
end


pro w_test_prcp_nc_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j = 0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] +'_h_2d_prcp_nc_2009.nc')
    a = product->get_Var('PRCP_NC')
    expected = total(a,3, /DOUBLE)
    ;w_QuickPlot, expected, TITLE='orig'
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/2d/kin_' + dom_str[j] + '_y_2d_prcp_nc_2009.nc')
    totest = product_year->get_Var('PRCP_NC')
    
    ;w_QuickPlot,totest, TITLE='prod'
    ;w_QuickPlot, expected - totest, TITLE='diff'
    ;print, total(ABS(expected - totest))
    ;print, max(ABS(expected - totest))
    
    ;    if max(ABS(expected - totest)) gt 0.01 then error += 1
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then error += 1
;   print, max(err_perc)
    
    undefine, product, product_year, totest, expected
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_NC_Y NOT passed for domain' + dom_str[j] ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
  endfor
  
end


pro w_test_prcp_h, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'RAINC', acc_rainc
    NCDF_VARGET, oId, 'RAINNC', acc_rainnc
    
    acc_rain = acc_rainc + acc_rainnc
    
    acc_rain1 = acc_rain[*,*,0:(N_elements(acc_rain[0,0,*])-2)]
    acc_rain2 = acc_rain[*,*,1:(N_elements(acc_rain[0,0,*])-1)]
    
    expected = acc_rain2 - acc_rain1
    
    NCDF_CLOSE, oId
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=28, HOUR=03)
    ;  t1 = QMS_TIME(year=2009, MONTH=06, DAY=29, HOUR=00)
    undefine, tpl
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_2009.nc')
    totest = product->get_Var('PRCP', T0=t0, T1=t1)
    
    if total(ABS(expected-totest)) ne 0 then error += 1
;    print, total(ABS(expected-totest))
;    print, max(ABS(expected-totest))
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_H NOT passed for domain' + dom_str[j] ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
    undefine, product, totest, expected
    
  endfor
  
end


pro w_test_prcp_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  ;  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'RAINC', acc_rainc
    NCDF_VARGET, oId, 'RAINNC', acc_rainnc
    
    a = acc_rainc[*,*,dom_div[j]] - acc_rainc[*,*,0]
    b = acc_rainnc[*,*,dom_div[j]] - acc_rainnc[*,*,0]
    
    expected = a + b
    
    NCDF_CLOSE, oId
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/2d/kin_' + dom_str[j] + '_d_2d_prcp_2009.nc')
;    totest = product->get_Var('PRCP')
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(YEAR=2008,day=28,month=06)
    undefine, tpl
    
    totest = product->get_Var('PRCP',T0=t0, T1=t0)
        
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;      print, mean_err, err_perc
    if max(err_perc) gt 0.005 then error += 1
;    print, max(err_perc)
    
;        if total(ABS(expected-totest)) gt 0.003 then error += 1
;        print, total(ABS(expected-totest))
;        print, max(ABS(expected-totest))
    
    undefine, product
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_D NOT passed for domain' + dom_str[j] ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
  endfor
  
end


pro w_test_prcp_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_2009.nc')
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('PRCP', t, nt, T0=t0, T1=t1)
      expected = total(a, 3)
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/2d/kin_'+ dom_str[j] +'_m_2d_prcp_2009.nc')
      totest = product_month->get_Var('PRCP', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
      ;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
        ;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_2D_PRCP_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE
      ;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      
      ;if max(ABS(expected - totest)) gt 0.001 then error += 1
      ;if error ne 0 then message, '% W_TEST_WPP_PRCP_C_M NOT passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE else print, 'OK for domain'+ dom_str[j] + ' month ' + str_equiv(i+1)
      
      undefine, product_month
      
    endfor
    
    undefine, product, totest, expected
    
  endfor
  
end


pro w_test_prcp_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j = 0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_2009.nc')
    a = product->get_Var('PRCP')
    expected = total(a,3, /DOUBLE)
    ;  w_QuickPlot, expected, TITLE='orig'
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/2d/kin_' + dom_str[j] + '_y_2d_prcp_2009.nc')
    totest = product_year->get_Var('PRCP')
    
    ;  w_QuickPlot,totest, TITLE='prod'
    ;  w_QuickPlot, expected - totest, TITLE='diff'
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then error += 1
;    print, max(err_perc)
    
    
    ;    if max(ABS(expected - totest)) gt 0.01 then error += 1
    ;      print, total(ABS(expected - totest))
    ;      print, max(ABS(expected - totest))
    
    undefine, product, product_year, expected, totest
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_Y NOT passed for domain ' + dom_str[j]   ;, /CONTINUE else print, ' OK ' + dom_str[j]
    
  endfor
  
end


pro w_test_prcp_fr_h, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'RAINC', acc_rainc
    NCDF_VARGET, oId, 'RAINNC', acc_rainnc
    NCDF_VARGET, oId, 'SR', sr_rain
    
    acc_rain = acc_rainc + acc_rainnc
    
    nt = N_elements(acc_rain[0,0,*])
    acc_rain1 = acc_rain[*,*,0:nt-2]
    acc_rain2 = acc_rain[*,*,1:nt-1]
    
    expected = acc_rain2 - acc_rain1
    
    sr = sr_rain[*,*,1:*]
    
    fr = sr * expected
    
    NCDF_CLOSE, oId
    
    ;  w_QuickPlot, sr, TITLE='sr'
    ;  w_QuickPlot, expected, TITLE='expected'
    ;  w_QuickPlot, fr, TITLE='orig'
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=28, HOUR=03)
    ;  t1 = QMS_TIME(year=2009, MONTH=06, DAY=29, HOUR=00)
    undefine, tpl
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_' + dom_str[j] + '_h_2d_prcp_fr_2009.nc')
    totest = product->get_Var('PRCP_FR', T0=t0, T1=t1)
    
    ;  w_QuickPlot, totest, TITLE='prod'
    ;  w_QuickPlot, fr - totest, TITLE='diff'
    ;  print, total(ABS(fr-totest))
    ;  print, max(ABS(fr - totest))
    
;    if max(ABS(fr - totest)) gt 0.0001 then error += 1
    
        mean_err = MAX(ABS(fr-totest))
    mean_val = MEAN(ABS(fr))
    err_perc = mean_err / mean_val
;        print, mean_err, mean_val, err_perc
    if max(err_perc) gt 0.005 then error += 1
;    print, max(err_perc)
    
    if error ne 0 then message, '% W_TEST_WPP_PRCP_FR_H NOT passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
    undefine, product
    
  endfor
  
end


pro w_test_t2_h, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  ;  domH = [3,1,1]
  ;  dom_div = [8, 24, 24]
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'T2', expected
    
    expected = expected[*,*,1:*]
    
    NCDF_CLOSE, oId
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_'+ dom_str[j] + '_h_2d_t2_2009.nc')
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]

    undefine, tpl
    
    totest = product->get_Var('T2', T0=t0, T1=t1)
    
    if total(ABS(expected-totest)) ne 0 then error += 1
    ;  print, total(ABS(expected-totest))
    ;  print, max(ABS(expected-totest))
    
    if error ne 0 then message, '%TEST_WPP_T2_H not passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
    undefine, product
    
  endfor
  
end


pro w_test_t2_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  ;  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'T2', expected
    
    expected = expected[*,*,1:*]
    a = ((total(expected,3))/dom_div[j])
    
    NCDF_CLOSE, oId
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/2d/kin_'+ dom_str[j] + '_d_2d_t2_2009.nc')
    totest = product->get_Var('T2')
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    
    undefine, tpl
    
    b = product->get_Var('T2',T0=t0, T1=t0)
    
    err_perc = ABS((a-b)/a)
    if max(err_perc) gt 0.005 then error += 1
    ;  print, max(err_perc)
    
    if error ne 0 then message, '%TEST_WPP_T2_D not passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'OK  for domain ' + dom_str[j]
    
    undefine, product, expected, a, b
    
  endfor
  
end


pro w_test_t2_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'  
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_'+ dom_str[j] + '_h_2d_t2_2009.nc')
  
  for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('T2', t, nt, T0=t0, T1=t1)
      expected = total(a, 3) / nt
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/2d/kin_'+ dom_str[j] +'_m_2d_t2_2009.nc')
      totest = product_month->get_Var('T2', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_2D_T2_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE
;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      undefine, product_month
    endfor
    
    undefine, product, product_month, expected
    
  endfor
 
end


pro w_test_t2_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j = 0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/2d/kin_'+ dom_str[j] + '_h_2d_t2_2009.nc')
    
    for i= 0, 11 do begin
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      a = product->get_Var('T2', time, nt, T0=t0, T1=t1)
      if N_ELEMENTS(expected) eq 0 then begin
        expected = TOTAL(a, 3) / nt
      endif else begin
        expected = expected + TOTAL(a, 3) / nt
      endelse
    endfor
    expected = expected / 12
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/2d/kin_' + dom_str[j] + '_y_2d_t2_2009.nc')
    totest = product_year->get_Var('T2')
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then begin
    
      ;      print, max(err_perc)
      error += 1
      message, '%TEST_WPP_2D_T2_Y not passed for domain ' + dom_str[j], /CONTINUE
    ;    endif else print, ' OK ' + dom_str[j]
    endif
    
    undefine, product, product_year, expected, totest
    
  endfor
  
end


pro w_test_3d_eta_u_h, error
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'  
  
  dom_str = ['d30km','d10km','d02km_06']
;  domH = [3,1,1]
;  dom_div = [8, 24, 24]
  
  for j = 0, 2 do begin
  
  f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
  oId = NCDF_OPEN(f)

  NCDF_VARGET, oId, 'U', expected
  
  expected = utils_wrf_unstagger(expected,0)
  
  expected = expected[*,*,*,1:*]
;  w_QuickPlot, expected, TITLE='orig'

  NCDF_CLOSE, oId
  
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_' + dom_str[j] + '_h_3d_eta_u_2009.nc')
  
  tpl = OBJ_NEW('w_WRF', FILE=f)
  tpl->get_time, time, nt, t0, t1
  t0 += time[1]-time[0]
;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=28, HOUR=03)
;  t1 = QMS_TIME(year=2009, MONTH=06, DAY=29, HOUR=00)
  undefine, tpl
  
  totest = product->get_Var('u', T0=t0, T1=t1)
;  w_QuickPlot, totest, TITLE='prod'  
;  w_QuickPlot, expected - totest, TITLE='diff'
  
  if total(ABS(expected-totest)) ne 0 then error += 1
;  print, total(ABS(expected - totest))
;  print, max(ABS(expected - totest))

  if error ne 0 then message, '%TEST_WPP_3D_ETA_U_H not passed for domain ' + dom_str[j]  ;, /CONTINUE else print, 'W_TEST_WPP_3D_ETA_U_H passed'

  undefine, product
  
  endfor

end


pro w_test_3d_eta_u_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
;  domH = [3,1,1]
  dom_div = [8, 24, 24]
  
  for j=0, 2 do begin

    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    oId = NCDF_OPEN(f)
    NCDF_VARGET, oId, 'U', expected
    
    expected = utils_wrf_unstagger(expected,0)
    
    expected = expected[*,*,*,1:*]
    
    a = ((total(expected,4))/dom_div[j])
    
    NCDF_CLOSE, oId
    
    ;  w_QuickPlot, a, TITLE='orig'
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] +'/d/3d_eta/kin_' + dom_str[j] + '_d_3d_eta_u_2009.nc')
    totest = product->get_Var('u')
    
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(YEAR=2008,day=28,month=06)
    undefine, tpl
    
    b = product->get_Var('u',T0=t0, T1=t0)
    ;  w_QuickPlot, b, TITLE='prod'
    ;  w_QuickPlot, a - b, TITLE='diff'
    
    if total(ABS(a - b)) ne 0 then error += 1
;    print, total(ABS(a - b))
;    print, max(ABS(a - b))
    
    if error ne 0 then message, '%TEST_WPP_3D_ETA_U_D not passed for domain ' + dom_str[j] , /CONTINUE ;else print, ' OK ' + dom_str[j]
    
    undefine, product
    
  endfor
  
end


pro w_test_3d_eta_u_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_'+ dom_str[j] +'_h_3d_eta_u_2009.nc')
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('u', t, nt, T0=t0, T1=t1)
      expected = total(a, 4) / nt
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/3d_eta/kin_'+ dom_str[j] +'_m_3d_eta_u_2009.nc')
      totest = product_month->get_Var('u', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_3D_ETA_U_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1), /CONTINUE
;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      undefine, product_month
    endfor
    
    undefine, product, product_year, expected
    
  endfor
  
end


pro w_test_3d_eta_u_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_' + dom_str[j] + '_h_3d_eta_u_2009.nc')
    
    for i= 0, 11 do begin
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      a = product->get_Var('u', time, nt, T0=t0, T1=t1)
      if N_ELEMENTS(expected) eq 0 then begin
        expected = TOTAL(a, 4) / nt
      endif else begin
        expected = expected + TOTAL(a, 4) / nt
      endelse
    endfor
    expected = expected / 12
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/3d_eta/kin_' + dom_str[j] + '_y_3d_eta_u_2009.nc')
    totest = product_year->get_Var('u')
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then begin
    
;      print, max(err_perc)
      error += 1
      message, '%TEST_WPP_3D_ETA_U_Y not passed for domain ' + dom_str[j] ;, /CONTINUE
      
;    endif else print, ' OK ' + dom_str[j]
    endif
    
    undefine, product, product_year, expected
    
  endfor
  
end


pro w_test_3d_eta_v_h, error
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'  
  
  dom_str = ['d30km','d10km','d02km_06']
  
  for j = 0, 2 do begin
  
  f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
  oId = NCDF_OPEN(f)

  NCDF_VARGET, oId, 'V', expected
  
  expected = utils_wrf_unstagger(expected,1)
  
  expected = expected[*,*,*,1:*]
;  w_QuickPlot, expected, TITLE='orig'

  NCDF_CLOSE, oId
  
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] +'/h/3d_eta/kin_'+ dom_str[j] + '_h_3d_eta_v_2009.nc')
  
  tpl = OBJ_NEW('w_WRF', FILE=f)
  tpl->get_time, time, nt, t0, t1
  t0 += time[1]-time[0]
;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=28, HOUR=03)
;  t1 = QMS_TIME(year=2009, MONTH=06, DAY=29, HOUR=00)
  undefine, tpl
  
  totest = product->get_Var('v', T0=t0, T1=t1)
;  w_QuickPlot, totest, TITLE='prod'  
;  w_QuickPlot, expected - totest, TITLE='diff'
  
  if total(ABS(expected-totest)) ne 0 then error += 1
;  print, total(ABS(expected - totest))
;  print, max(ABS(expected - totest))

  if error ne 0 then message, '%TEST_WPP_3D_ETA_V_H not passed  for domain ' + dom_str[j] ;, /CONTINUE else print, 'W_TEST_WPP_3D_ETA_V_H passed'

  undefine, product
  
  endfor

end


pro w_test_3d_eta_v_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'  
  
  dom_str = ['d30km','d10km','d02km_06']
  dom_div = [8, 24, 24]
  
  for j = 0, 2 do begin
  
  f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
  oId = NCDF_OPEN(f)
  NCDF_VARGET, oId, 'V', expected
  
  expected = utils_wrf_unstagger(expected,1)
  
  expected = expected[*,*,*,1:*]
  
  a = ((total(expected,4))/dom_div[j])   ; for domain d01
;  a = ((total(expected,4))/24)  ; for domain d02 & d03
  
  NCDF_CLOSE, oId
  
;  w_QuickPlot, a, TITLE='orig'
 
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] +'/d/3d_eta/kin_'+ dom_str[j] + '_d_3d_eta_v_2009.nc')
  totest = product->get_Var('v')
  
  product->get_Time, time
  
  tpl = OBJ_NEW('w_WRF', FILE=f)
  tpl->get_time, time, nt, t0, t1
  t0 += time[1]-time[0]
  ;  t0 = QMS_TIME(YEAR=2008,day=28,month=06)
  undefine, tpl
  
  b = product->get_Var('v',T0=t0, T1=t0)
;  w_QuickPlot, b, TITLE='prod'
;  w_QuickPlot, a - b, TITLE='diff'
  
  if total(ABS(a - b)) ne 0 then error += 1
;  print, total(ABS(a - b))
;  print, max(ABS(a - b))
  
  if error ne 0 then message, '%TEST_WPP_3D_ETA_V_D not passed' ;, /CONTINUE ;else print, 'W_TEST_WPP_3D_ETA_V_D passed'
  
  undefine, product
  
  endfor
  
end


pro w_test_3d_eta_v_m, error


  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_'+ dom_str[j] +'_h_3d_eta_v_2009.nc')
    
    ;  t0 = QMS_TIME(year=2009, month=1, day=1, HOUR=domH[j])
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('v', t, nt, T0=t0, T1=t1)
      expected = total(a, 4) / nt
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/3d_eta/kin_'+ dom_str[j] +'_m_3d_eta_v_2009.nc')
      totest = product_month->get_Var('v', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_3D_ETA_V_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1) ;, /CONTINUE
;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      undefine, product_month
      
    endfor
    
    undefine, product, product_year, expected
    
  endfor
  
end


pro w_test_3d_eta_v_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_' + dom_str[j] + '_h_3d_eta_v_2009.nc')
    
    for i= 0, 11 do begin
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      a = product->get_Var('v', time, nt, T0=t0, T1=t1)
      if N_ELEMENTS(expected) eq 0 then begin
        expected = TOTAL(a, 4) / nt
      endif else begin
        expected = expected + TOTAL(a, 4) / nt
      endelse
    endfor
    expected = expected / 12
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/3d_eta/kin_' + dom_str[j] + '_y_3d_eta_v_2009.nc')
    totest = product_year->get_Var('v')
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then begin
    
      ;      print, max(err_perc)
      error += 1
      message, '%TEST_WPP_3D_ETA_V_Y not passed for domain ' + dom_str[j], /CONTINUE
;    endif else print, ' OK ' + dom_str[j]
    endif
    
    undefine, product, product_year, expected, totest
    
  endfor
  
end


pro w_test_3d_eta_qvapor_h, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  
  for j = 0, 2 do begin
  
  f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
  oId = NCDF_OPEN(f)
  
  NCDF_VARGET, oId, 'QVAPOR', expected
  
  expected = expected[*,*,*,1:*]
  ;  w_QuickPlot, expected, TITLE='orig'
  
  NCDF_CLOSE, oId
  
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_' + dom_str[j] +'_h_3d_eta_qvapor_2009.nc')
  
  tpl = OBJ_NEW('w_WRF', FILE=f)
  tpl->get_time, time, nt, t0, t1
  t0 += time[1]-time[0]

  undefine, tpl
  
  totest = product->get_Var('QVAPOR', T0=t0, T1=t1)
  
  mean_err = MAX(ABS(expected-totest))
  mean_val = MEAN(ABS(expected))
  err_perc = mean_err / mean_val
;  print, mean_err, err_perc
  
  if max(err_perc) gt 0.005 then error += 1
;    print, max(err_perc)
    
    if error ne 0 then message, '%TEST_WPP_3D_ETA_QVAPOR_H not passed for domain' + dom_str[j] ; , /CONTINUE else print, 'W_TEST_WPP_3D_ETA_QVAPOR_H passed'
    
    undefine, product
    
  endfor  
    
end


pro w_test_3d_eta_qvapor_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  dom_div = [8, 24, 24]
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'QVAPOR', expected
    
    expected = expected[*,*,*,1:*]
    a = ((total(expected,4))/dom_div[j])
    ;  w_QuickPlot, a, TITLE='orig'
    
    NCDF_CLOSE, oId
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/3d_eta/kin_' + dom_str[j] + '_d_3d_eta_qvapor_2009.nc')
    
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    undefine, tpl
    
    b = product->get_Var('QVAPOR',T0=t0, T1=t0)
    
    ;  if total(ABS(a - b)) ne 0 then begin
    ;  error += 1
    ;  print, total(ABS(a - b))
    ;  print, max(ABS(a - b))
    ;
    ;  message, '%TEST_WPP_3D_ETA_QVAPOR_D not passed' , /CONTINUE
    ;
    ;  endif else print, ' OK '
    
    
    if total(ABS(a - b)) ne 0 then error += 1
    ;  print, total(ABS(a - b))
    ;  print, max(ABS(a - b))
    
    if error ne 0 then message, '%TEST_WPP_3D_ETA_QVAPOR_D not passed for domain ' + dom_str[j]  ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
  endfor
  
  undefine, product, a, b
  
end


pro w_test_3d_eta_qvapor_m, error

; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_'+ dom_str[j] +'_h_3d_eta_qvapor_2009.nc')
    
    ;  t0 = QMS_TIME(year=2009, month=1, day=1, HOUR=domH[j])
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('QVAPOR', t, nt, T0=t0, T1=t1)
      expected = total(a, 4) / nt
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/3d_eta/kin_'+ dom_str[j] +'_m_3d_eta_qvapor_2009.nc')
      totest = product_month->get_Var('QVAPOR', T0=t2, T1=t2)
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_3D_ETA_QVAPOR_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1) ;, /CONTINUE
;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      undefine, product_month, expected, totest
    endfor
    
    undefine, product
    
  endfor
  
end


pro w_test_3d_eta_qvapor_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_eta/kin_'+dom_str[j]+'_h_3d_eta_qvapor_2009.nc')
    
    for i= 0, 11 do begin
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      a = product->get_Var('QVAPOR', time, nt, T0=t0, T1=t1)
      if N_ELEMENTS(expected) eq 0 then begin
        expected = TOTAL(a, 4) / nt
      endif else begin
        expected = expected + TOTAL(a, 4) / nt
      endelse
    endfor
    expected = expected / 12
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd+dom_str[j]+'/y/3d_eta/kin_'+dom_str[j]+'_y_3d_eta_qvapor_2009.nc')
    totest = product_year->get_Var('QVAPOR')
    
        mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then begin
    
;      print, max(err_perc)
      error += 1
      message, '%TEST_WPP_3D_ETA_V_Y not passed for domain ' + dom_str[j] ;, /CONTINUE
;    endif else print, ' OK ' + dom_str[j]
    endif
     
    undefine, product, product_year, expected, totest
  endfor
  
end


pro w_test_3d_soil_tslb_h, error

; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  
  for j = 0, 2 do begin
  
  f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
  oId = NCDF_OPEN(f)
  NCDF_VARGET, oId, 'TSLB', expected
  
  expected = expected[*,*,*,1:*]
;  w_QuickPlot, expected, TITLE='orig'
  NCDF_CLOSE, oId
  
  product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_soil/kin_' + dom_str[j] + '_h_3d_soil_tslb_2009.nc')
  
  tpl = OBJ_NEW('w_WRF', FILE=f)
  tpl->get_time, time, nt, t0, t1
  t0 += time[1]-time[0]
  undefine, tpl

  totest = product->get_Var('TSLB', T0=t0, T1=t1)
;  w_QuickPlot, expected, TITLE='prod'
;  w_QuickPlot, expected-totest, TITLE='diff'
  
  if total(ABS(expected-totest)) ne 0 then error += 1
;  print, total(ABS(expected-totest))
;  print, max(ABS(expected-totest))
  
  if error ne 0 then message, '%TEST_WPP_3D_SOIL_TSLB_H not passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'W_TEST_WPP_3D_SOIL_TSLB_H passed'

  undefine, product
  
  endfor
  
end


pro w_test_3d_soil_tslb_d, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  dom_div = [8, 24, 24]
  
  for j = 0, 2 do begin
  
    f = w_test_wpp_make_prods_pick_rdm_file(DOMAIN=str_equiv(j+1))
    
    oId = NCDF_OPEN(f)
    
    NCDF_VARGET, oId, 'TSLB', expected
    
    expected = expected[*,*,*,1:*]
    
    a = ((total(expected,4))/dom_div[j])
    
    
    NCDF_CLOSE, oId
    
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/3d_soil/kin_' + dom_str[j] + '_d_3d_soil_tslb_2009.nc')
    
    product->get_Time, time
    
    tpl = OBJ_NEW('w_WRF', FILE=f)
    tpl->get_time, time, nt, t0, t1
    t0 += time[1]-time[0]
    ;  t0 = QMS_TIME(YEAR=2008,day=28,month=06)
    undefine, tpl
    
    b = product->get_Var('TSLB',T0=t0, T1=t0)
    ;  w_QuickPlot, b, TITLE='prod'
    ;  w_QuickPlot, a-b, TITLE='diff'
    
    if total(ABS(a - b)) ne 0 then error += 1
;    print, total(ABS(a - b))
;    print, max(ABS(a - b))
    
    if error ne 0 then message, '%TEST_WPP_3D_SOIL_TSLB_D not passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'W_TEST_WPP_3D_SOIL_TSLB_D passed'
    
    undefine, product
    
  endfor
  
end


pro w_test_3d_soil_tslb_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_soil/kin_' + dom_str[j] + '_h_3d_soil_tslb_2009.nc')
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      a = product->get_Var('TSLB', t, nt, T0=t0, T1=t1)
      expected = total(a, 4) / nt
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/3d_soil/kin_' + dom_str[j] + '_m_3d_soil_tslb_2009.nc')
      totest = product_month->get_Var('TSLB', T0=t2, T1=t2)
      
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;        print, max(err_perc)
        error += 1
        message, '%TEST_WPP_3D_ETA_TSLB_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1) ;, /CONTINUE
      ;      endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      undefine, product_month, expected, totest
    
    endfor
    
    undefine, product
    
  endfor
  
end


pro w_test_3d_soil_tslb_y, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_soil/kin_' + dom_str[j] + '_h_3d_soil_tslb_2009.nc')
    
    for i= 0, 11 do begin
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      a = product->get_Var('TSLB', time, nt, T0=t0, T1=t1)
      if N_ELEMENTS(expected) eq 0 then begin
        expected = TOTAL(a, 4) / nt
      endif else begin
        expected = expected + TOTAL(a, 4) / nt
      endelse
    endfor
    expected = expected / 12
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/3d_soil/kin_' + dom_str[j] + '_y_3d_soil_tslb_2009.nc')
    totest = product_year->get_Var('TSLB')
    
    err_perc = ABS((expected-totest)/expected)
    if max(err_perc) gt 0.005 then error += 1
    ;  print, max(err_perc)
    
    if error ne 0 then message, '%TEST_WPP_3D_SOIL_TSLB_Y not passed for domain ' + dom_str[j] ; , /CONTINUE else print, ' OK for domain ' + dom_str[j]
    
    undefine, product_year, product, totest, expected
    
  endfor
  
end


pro template_read_press_qvapor_h, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  dom_nr = ['d01','d02','d03']
  
  for j = 0,2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_press/kin_'+ dom_str[j] + '_h_3d_press_qvapor_2009.nc')
    
    t0 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=15)
    
    wrf = product->get_Var('QVAPOR', T0=t0, T1=t0)
    
    ncl = wrf * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_15:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    pf = where(~ FINITE(wrf), cnt)
    if cnt ne 0 then wrf[pf] = -999999
    ;    w_QuickPlot, ABS((ncl-wrf)/ncl), TITLE='diff %'
    
    if max(ABS(ncl - wrf)) gt 0.000001 then error +=1
    ;    print, total(ABS(ncl - wrf))
    ;    print, max(ABS(ncl - wrf))
    
    if error ne 0 then message, '%TEST_WPP_3D_PRESS_QVAPOR_H not passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'OK for domain ' + dom_str[j]
    
    undefine, product, wrf
    
  endfor
  
end


pro template_read_d01_press_qvapor_d, error
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
    product = OBJ_NEW('w_WRF', FILE = rootd+'d30km/h/3d_press/kin_d30km_h_3d_press_qvapor_2009.nc')
    
    t0 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=03)
    t1 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=06)
    t2 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=09)
    t3 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=12)
    t4 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=15)
    t5 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=18)
    t6 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=21)
    t7 = QMS_TIME(year=2009, MONTH=07, DAY=19, HOUR=00)
    
    wrf1 = product->get_Var('QVAPOR', T0=t0, T1=t0)
    wrf2 = product->get_Var('QVAPOR', T0=t1, T1=t1)
    wrf3 = product->get_Var('QVAPOR', T0=t2, T1=t2)
    wrf4 = product->get_Var('QVAPOR', T0=t3, T1=t3)
    wrf5 = product->get_Var('QVAPOR', T0=t4, T1=t4)
    wrf6 = product->get_Var('QVAPOR', T0=t5, T1=t5)
    wrf7 = product->get_Var('QVAPOR', T0=t6, T1=t6)
    wrf8 = product->get_Var('QVAPOR', T0=t7, T1=t7)
    
    
    ncl1 = wrf1 * 0.    
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_03:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl1[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    
;    pf = where(~ FINITE(wrf1))
;    wrf1[pf] = -999999
;    w_QuickPlot, ABS((ncl-wrf1)/ncl)

    
    ncl2 = wrf2 * 0.    
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_06:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl2[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl3 = wrf3 * 0.
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_09:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl3[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    
   
   ncl4 = wrf4 * 0.
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_12:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl4[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
   
   
   ncl5 = wrf5 * 0.
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_15:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl5[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
   
   ncl6 = wrf6 * 0.    
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_18:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl6[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl7 = wrf7 * 0.    
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-18_21:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl7[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl8 = wrf8 * 0.    
    
    file = rootd+'ncl_out/wrfd01_qvapor_plane2009-07-19_00:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
     readf,lun, line
     ncl8[k] = FLOAT(line)   
     k+=1 
    endwhile       
    CLOSE, lun
    FREE_LUN, lun

    
    ncl = ((ncl1 + ncl2 + ncl3 + ncl4 + ncl5 + ncl6 + ncl7 + ncl8)/8)
    ncl_test = ncl[*,*,19,*]
    
    wrf = OBJ_NEW('w_WRF', FILE = rootd+'d30km/d/3d_press/kin_d30km_d_3d_press_qvapor_2009.nc')
    a = QMS_TIME(year=2009, MONTH=07, DAY=18)
    totest = wrf->get_Var('QVAPOR', T0=a, T1=a)
    
;    pf = where(~ FINITE(totest))
;    totest[pf] = -999999
    pf = where(~ FINITE(totest), cnt)
    if cnt ne 0 then totest[pf] = -999999

    totest_test = totest[*,*,19,*]
    
    error = 0
    
;    w_QuickPlot, ABS(ncl_test-totest_test), TITLE='diff'
;    print, total(ABS(ncl_test-totest_test))
;    print, max(ABS(ncl_test-totest_test))
    
    if max(ABS(ncl_test-totest_test)) gt 0.000001 then error +=1
    
;    w_QuickPlot, ABS((ncl_test-totest_test)/ncl_test), TITLE='diff %'

  if error ne 0 then message, '%TEST_WPP_3D_PRESS_QVAPOR_D not passed' ;, /CONTINUE else print, 'W_TEST_WPP_3D_PRESS_QVAPOR_D passed'
  
  undefine, product
  undefine, wrf
    
end
    
    
pro template_read_d02_d03_press_qvapor_d, error
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d10km','d02km_06']
  dom_nr = ['d02', 'd03']
  
  for j=0, 1 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] +'/h/3d_press/kin_' + dom_str[j] + '_h_3d_press_qvapor_2009.nc')
    
    t0 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=01)
    t1 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=02)
    t2 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=03)
    t3 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=04)
    t4 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=05)
    t5 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=06)
    t6 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=07)
    t7 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=08)
    t8 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=09)
    t9 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=10)
    t10 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=11)
    t11 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=12)
    t12 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=13)
    t13 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=14)
    t14 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=15)
    t15 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=16)
    t16 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=17)
    t17 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=18)
    t18 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=19)
    t19 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=20)
    t20 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=21)
    t21 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=22)
    t22 = QMS_TIME(year=2009, MONTH=07, DAY=18, HOUR=23)
    t23 = QMS_TIME(year=2009, MONTH=07, DAY=19, HOUR=00)
    
    wrf1 = product->get_Var('QVAPOR', T0=t0, T1=t0)
    wrf2 = product->get_Var('QVAPOR', T0=t1, T1=t1)
    wrf3 = product->get_Var('QVAPOR', T0=t2, T1=t2)
    wrf4 = product->get_Var('QVAPOR', T0=t3, T1=t3)
    wrf5 = product->get_Var('QVAPOR', T0=t4, T1=t4)
    wrf6 = product->get_Var('QVAPOR', T0=t5, T1=t5)
    wrf7 = product->get_Var('QVAPOR', T0=t6, T1=t6)
    wrf8 = product->get_Var('QVAPOR', T0=t7, T1=t7)
    wrf9 = product->get_Var('QVAPOR', T0=t8, T1=t8)
    wrf10 = product->get_Var('QVAPOR', T0=t9, T1=t9)
    wrf11 = product->get_Var('QVAPOR', T0=t10, T1=t10)
    wrf12 = product->get_Var('QVAPOR', T0=t11, T1=t11)
    wrf13 = product->get_Var('QVAPOR', T0=t12, T1=t12)
    wrf14 = product->get_Var('QVAPOR', T0=t13, T1=t13)
    wrf15 = product->get_Var('QVAPOR', T0=t14, T1=t14)
    wrf16 = product->get_Var('QVAPOR', T0=t15, T1=t15)
    wrf17 = product->get_Var('QVAPOR', T0=t16, T1=t16)
    wrf18 = product->get_Var('QVAPOR', T0=t17, T1=t17)
    wrf19 = product->get_Var('QVAPOR', T0=t18, T1=t18)
    wrf20 = product->get_Var('QVAPOR', T0=t19, T1=t19)
    wrf21 = product->get_Var('QVAPOR', T0=t20, T1=t20)
    wrf22 = product->get_Var('QVAPOR', T0=t21, T1=t21)
    wrf23 = product->get_Var('QVAPOR', T0=t22, T1=t22)
    wrf24 = product->get_Var('QVAPOR', T0=t23, T1=t23)
    
    
    ncl1 = wrf1 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_01:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    ;    pf = where(~ FINITE(wrf1))
    ;    wrf1[pf] = -999999
    ;    w_QuickPlot, ABS((ncl-wrf1)/ncl)
    
    
    ncl2 = wrf2 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_02:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl2[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl3 = wrf3 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_03:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl3[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl4 = wrf4 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_04:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl4[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl5 = wrf5 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_05:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl5[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    ncl6 = wrf6 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_06:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl6[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl7 = wrf7 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_07:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl7[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl8 = wrf8 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_08:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl8[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    ncl9 = wrf9 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_09:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl10 = wrf10 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_10:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl11 = wrf11 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_11:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl12 = wrf12 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_12:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl13 = wrf13 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_13:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl14 = wrf14 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_14:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl15 = wrf15 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_15:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl16 = wrf16 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_16:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl17 = wrf17 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_17:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl18 = wrf18 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_18:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl19 = wrf19 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_19:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl20 = wrf20 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_20:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl21 = wrf21 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_21:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl22 = wrf22 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_22:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl23 = wrf23 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-18_23:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl24 = wrf24 * 0.
    
    file = rootd+'ncl_out/wrf' + dom_nr[j] + '_qvapor_plane2009-07-19_00:00:00'
    OPENR, lun, file, /GET_LUN
    line = ''
    k=0LL
    while ~eof(lun) do begin
      readf,lun, line
      ncl1[k] = FLOAT(line)
      k+=1
    endwhile
    CLOSE, lun
    FREE_LUN, lun
    
    
    ncl = ((ncl1 + ncl2 + ncl3 + ncl4 + ncl5 + ncl6 + ncl7 + ncl8 + ncl9 + ncl10 + ncl11 + ncl12 $
      + ncl13 + ncl14 + ncl15 + ncl16 + ncl17 + ncl18+ ncl19+ ncl20+ ncl21+ ncl22+ ncl23+ ncl24)/24)
    ncl_test = ncl[*,*,19,*]
    
    wrf = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/d/3d_press/kin_' + dom_str[j] + '_d_3d_press_qvapor_2009.nc')
    a = QMS_TIME(year=2009, MONTH=07, DAY=18)
    totest = wrf->get_Var('QVAPOR', T0=a, T1=a)
    
    ;    pf = where(~ FINITE(totest))
    ;    totest[pf] = -999999
    pf = where(~ FINITE(totest), cnt)
    if cnt ne 0 then totest[pf] = -999999
    
    totest_test = totest[*,*,19,*]
    
    ;    error = 0
    
    ;    w_QuickPlot, ABS(ncl_test-totest_test), TITLE='diff'
    ;    print, total(ABS(ncl_test-totest_test))
    ;    print, max(ABS(ncl_test-totest_test))
    
    ;    if max(ABS(ncl_test-totest_test)) gt 0.000001 then error +=1
    ;    if max(ABS(ncl_test-totest_test)) gt 0.00001 then error +=1
    if max(ABS(ncl_test-totest_test)) gt 0.0001 then error +=1

;    print, max(ABS(ncl_test-totest_test))
;    
;    mean_err = MAX(ABS(ncl_test-totest_test))
;    mean_val = MEAN(ABS(ncl_test))
;    err_perc = mean_err / mean_val
;    print, mean_err, err_perc
;    if max(err_perc) gt 0.005 then error +=1
;    print, max(err_perc)
    
    ;    w_QuickPlot, ABS((ncl_test-totest_test)/ncl_test), TITLE='diff %'
    
    if error ne 0 then message, '%TEST_WPP_3D_PRESS_QVAPOR_D not passed for domain ' + dom_str[j] ;, /CONTINUE else print, 'OK for domain' + dom_str[j]
    
    undefine, product
    undefine, wrf
    
  endfor
  
end
    

pro w_test_3d_press_qvapor_m, error

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_press/kin_'+ dom_str[j] + '_h_3d_press_qvapor_2009.nc')
    
    for i= 0, 11 do begin
    
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      
      ;  t0 = QMS_TIME(year=2009, MONTH=06, DAY=01, HOUR=03)
      ;  t1 = QMS_TIME(year=2009, MONTH=07, DAY=01, HOUR=00)
      
      a = product->get_Var('QVAPOR', T0=t0, T1=t1)
      b = total(a,4, /DOUBLE)
      expected = b/(N_elements(a[0,0,0,*]))
      ;  w_QuickPlot, expected, TITLE='orig'
      
      ;  t2 = QMS_TIME(year=2009, MONTH=06)
      
      t2 = QMS_TIME(year=2009, month=i+1, day=1)
      
      product_month = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/m/3d_press/kin_' + dom_str[j] + '_m_3d_press_qvapor_2009.nc')
      totest = product_month->get_Var('QVAPOR', T0=t2, T1=t2)
      ;  w_QuickPlot,totest, TITLE='prod'
      ;  w_QuickPlot, expected - totest, TITLE='diff'
      ;  print, total(ABS(expected - totest))
      ;  print, max(ABS(expected - totest))
      
      ;  if max(ABS(expected - totest)) gt 0.0001 then error += 1
      
      mean_err = MAX(ABS(expected-totest))
      mean_val = MEAN(ABS(expected))
      err_perc = mean_err / mean_val
      ;      print, mean_err, err_perc
      if max(err_perc) gt 0.005 then begin
;                print, max(err_perc)
        error += 1
        message, '%TEST_WPP_3D_ETA_TSLB_M not passed for domain ' + dom_str[j] + ' for month ' + str_equiv(i+1) ;, /CONTINUE
;     endif else print, ' OK ' + dom_str[j] + ' month ' + str_equiv(i+1)
      endif
      
    ;  if error ne 0 then message, '%TEST_WPP_3D_PRESS_QVAPOR_M not passed' , /CONTINUE else print, 'W_TEST_WPP_3D_PRESS_QVAPOR_M passed'
    endfor
    undefine, product, product_month, expected, totest
  endfor
  
end


pro w_test_3d_press_qvapor_y, errors

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  error = 0
  
  rootd = w_test_wpp_file_directory() + 'products/'
  
  dom_str = ['d30km','d10km','d02km_06']
  domH = [3,1,1]
  
  for j=0, 2 do begin
  
    product = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/h/3d_press/kin_' + dom_str[j] + '_h_3d_press_qvapor_2009.nc')
    
    for i= 0, 11 do begin
      t0 = QMS_TIME(year=2009, month=i+1, day=1, HOUR=domH[j])
      if i lt 11 then t1 = QMS_TIME(year=2009, month=i+2, day=1, HOUR=0) $
      else t1 = QMS_TIME(year=2010, month=1, day=1, HOUR=0)
      a = product->get_Var('QVAPOR', time, nt, T0=t0, T1=t1)
      if N_ELEMENTS(expected) eq 0 then begin
        expected = TOTAL(a, 4) / nt
      endif else begin
        expected = expected + TOTAL(a, 4) / nt
      endelse
    endfor
    expected = expected / 12
    
    product_year = OBJ_NEW('w_WRF', FILE = rootd + dom_str[j] + '/y/3d_press/kin_' + dom_str[j] + '_y_3d_press_qvapor_2009.nc')
    totest = product_year->get_Var('QVAPOR')
    
    mean_err = MAX(ABS(expected-totest))
    mean_val = MEAN(ABS(expected))
    err_perc = mean_err / mean_val
    ;    print, mean_err, err_perc
    if max(err_perc) gt 0.005 then begin
    
      ;      print, max(err_perc)
      error += 1
      message, '%TEST_WPP_3D_PRESS_QVAPOR_Y not passed for domain ' + dom_str[j] ;, /CONTINUE
    ;    endif else print, ' OK ' + dom_str[j]
    endif
    
    ;  if max(ABS(expected - totest)) gt 0.0001 then error += 1
    ;  if error ne 0 then message, '%TEST_WPP_3D_PRESS_QVAPOR_Y not passed' , /CONTINUE else print, 'W_TEST_WPP_3D_PRESS_QVAPOR_Y passed'
    
    undefine, product_year, product, totest, expected
    
  endfor
  
end


pro w_test_wpp, RESET=reset

  rootd = w_test_wpp_file_directory(RESET=reset)
  
  if ~ FILE_TEST(rootd + 'products') then w_test_wpp_make_prods
  
  toterror = 0
  
  w_test_wpp_nans, error
  
  if error ne 0 then begin
;    Print, 'w_test_nans not passed'
    toterror += 1
  endif
  
  w_test_wpp_landsurf, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_prcp_c_h, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_prcp_c_d, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3   
  
  w_test_prcp_c_m, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_prcp_c_y, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_prcp_nc_h, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_prcp_nc_d, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_prcp_nc_m, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3 
  
  w_test_prcp_nc_y, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_prcp_h, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_prcp_d, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_prcp_m, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3 
  
  w_test_prcp_y, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
 
  w_test_prcp_fr_h, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_t2_h, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_t2_d, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_t2_m, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_t2_y, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_eta_u_h, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_eta_u_d, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_eta_u_m, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_eta_u_y, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_eta_v_h, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_eta_v_d, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_eta_v_m, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_eta_v_y, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_eta_qvapor_h, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
 
  w_test_3d_eta_qvapor_d, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_eta_qvapor_m, error  
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_eta_qvapor_y, error    
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_soil_tslb_h, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  w_test_3d_soil_tslb_d, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_soil_tslb_m, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_soil_tslb_y, error 
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  template_read_press_qvapor_h, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  template_read_d01_press_qvapor_d, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  template_read_d02_d03_press_qvapor_d, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_press_qvapor_m, error
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3
  
  w_test_3d_press_qvapor_y, errors
  if error eq 1 then toterror += 1
  if error eq 2 then toterror += 2
  if error eq 3 then toterror += 3

  if toterror ne 0 then message, '% W_TEST_PRODUCTS NOT passed', /CONTINUE else print, '% TEST_WPP_PRODUCTS passed'
  
end