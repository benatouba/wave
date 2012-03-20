pro test_pt_diagramm, OUTPUT_DIR=output_dir
  
  ; choose output directory if no keyword is set
  if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = DIALOG_PICKFILE(TITLE='Please select output data directory', /MUST_EXIST, /DIRECTORY)
  
  ; number of time points (nt) and absolute time(time) of simulations
  wrf = OBJ_NEW('w_WRF', FILE='/home/mowglie/wrfout_d01_2008-10-26.nc')
  wrf->get_time, time, nt

  ; coordinates of wrf simulations for which skewT-logp-diagram will be generated
  _loc_x = [12,   30,   60,   90,   130]
  _loc_y = [12,   30,   60,   90,   130]
  nloc = N_ELEMENTS(_loc_x)
  
  ; for each timepoint and each location (specified by coordinates) a pt-diagram is generated
  for t=0, nt-1 do begin
     tc = wrf->get_Var("tc", ti, T0=time[t],T1=time[t])
     p = wrf->get_Var("pressure", T0=time[t],T1=time[t])
     td = wrf->get_Var("td", T0=time[t],T1=time[t])
     
     print, 'Now working on time: ' +  TIME_to_STR(ti)
     for l=0, nloc-1 do begin
        x = _loc_x[l]
        y = _loc_y[l]      
        print, 'Now working on loc: ' +  str_equiv(x) + ', ' +  str_equiv(y) 
        ploc = p[x,y,*]
        tcloc = tc[x,y,*]
        tdloc = td[x,y,*]
          

        _figtitle= 'Sounding at x='+STRING(x,FORMAT='(I3)')+', y='+STRING(y,FORMAT='(I3)')+', Time='+TIME_to_STR(ti)+'!C'
        pngname=''+STRING(x,FORMAT='(I3)')+'_'+STRING(y,FORMAT='(I3)')+'_'+STRING((t+1),FORMAT='(I2)')+''
        
      
        FILE_MKDIR,output_dir+'/skewT_logP_diagrams'
        skewt_logp_diagram, tcloc, ploc, ANGLE=45., FIGTITLE=_figtitle, STD_PNG=output_dir+'/skewT_logP_diagrams/'+pngname+'.png'

     endfor
          
  endfor 
    
  undefine, wrf

end