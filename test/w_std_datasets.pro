pro w_std_datasets, WRF30=wrf30, WRF10=wrf10, WRF2=wrf2, TRMM=trmm, WRFPROD=wrfprod

  fdir = w_test_file_directory()
    
  if ARG_PRESENT(WRF30) then wrf30 = OBJ_NEW('w_WRF', FILE=fdir+'WRF/wrfout_d01_2008-10-26')
  if ARG_PRESENT(WRF10) then wrf10 = OBJ_NEW('w_WRF', FILE=fdir+'WRF/wrfout_d02_2008-10-26')
  if ARG_PRESENT(WRF2) then wrf2 = OBJ_NEW('w_WRF', FILE=fdir+'WRF/wrfout_d03_2008-10-26')
  if ARG_PRESENT(WRFPROD) then wrfprod = OBJ_NEW('w_WRF', FILE=fdir+'WRF/tibet_d10km_h_2d_t2_2008.nc')
  if ARG_PRESENT(TRMM) then trmm = OBJ_NEW('w_TRMM', FILE=fdir+'TRMM/3B43.000801.6.nc')
  
end