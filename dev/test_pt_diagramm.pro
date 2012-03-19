pro test_pt_diagramm
  
  wrf = OBJ_NEW('w_WRF', FILE='/home/mowglie/wrfout_d01_2008-10-26.nc')
  
  wrf->get_time, time, nt

  _loc_x = [12,   30,   60,   90,   130]
  _loc_y = [12,   30,   60,   90,   130]
  nloc = N_ELEMENTS(_loc_x)
  
  for t=0, nt-1 do begin
     tc = wrf->get_Var("tc", ti, T0=time[t],T1=time[t])
     p = wrf->get_Var("pressure", T0=time[t],T1=time[t])
;     td = wrf->get_Var("td", T0=time[t],T1=time[t])
     
     print, 'Now working on time: ' +  TIME_to_STR(ti)
     
     for l=0, nloc-1 do begin
      x = _loc_x[l]
      y = _loc_y[l]      
      print, 'Now working on loc: ' +  str_equiv(x) + ', ' +  str_equiv(y) 
      ploc = p[x,y,*]
      tcloc = tc[x,y,*]
      
      skewt_logp_diagram, tcloc, ploc, ANGLE=45
      
       undefine, wrf
       return
     endfor
          
  endfor 
    
  undefine, wrf

end