
pro examples_w_TimeLinePlot
  
  data = LOADDATA(17)
  n = N_ELEMENTS(data)
  startT = QMS_TIME(year = 2011, day = 01, month = 01)
  step = MAKE_TIME_STEP(minute = 30)
  time = MAKE_TIME_SERIE(startT, NSTEPS=n, TIMESTEP=step)  
  
  ;Simple single (default) plot  
  w_TimeLinePlot, data, time
  
  ;Change time interval
  w_TimeLinePlot, data, time, HOURS = 12, TITLE = 'Changed the X axis'
  
  ; Add some options
  w_TimeLinePlot, data, time, 'My data', COMENT1='Random data', COLOR1= 'red',  $
                  THICKNESS=2, STYLE1= 5, TITLE='Example single plot with options', YTITLE='Unit', range = [-10,110], HORILINE=0
                  
  ; Add a second plot
  data2 = LOADDATA(17)
  data2 = data2[15:78]
  time2  = time[15:78]
  w_TimeLinePlot, data, time, 'My data', COMENT1='Random data', COLOR1= 'red',  $
                  THICKNESS=2, STYLE1= 5, TITLE='Example single plot with options', YTITLE='Unit', range = [-10,110], HORILINE=0,$
                  data2, time2, 'blue', 'My data2', COMENT2='Sample of random data', STYLE2=0 
                  
  
  ; Zoom in
  zoom = [QMS_TIME(year = 2011, day = 01, month = 01, hour = 12),QMS_TIME(year = 2011, day = 02, month = 01, hour = 12)]
  w_TimeLinePlot, data, time, 'My data', COMENT1='Random data', COLOR1= 'red', ZOOM=zoom, $
                  THICKNESS=2, STYLE1= 5, TITLE='Example single plot with zoom', YTITLE='Unit', range = [-10,110], HORILINE=0,$
                  data2, time2, 'blue', 'My data2', COMENT2='Sample of random data', STYLE2=0
  
  ; Add a new axis
  data3 = LOADDATA(17)
  w_TimeLinePlot, data, time, 'My data', COMENT1='Random data', COLOR1= 'red',  $
                  THICKNESS=2, STYLE1= 5, TITLE='Example single plot with new axis', YTITLE='Unit', range = [-10,110], HORILINE=0,$
                  data2, time2, 'blue', 'My data2', COMENT2='Sample of random data', STYLE2=0, $
                  data3/100., time, 'dark green', 'My data3', COMENT3='Other Units', NEWAXIS=3, NEWTITLE='Units/100', NEWRANGE=[0.2,0.8]
                  
  
  ok = DIALOG_MESSAGE('Do you want to close all windows? (IDL> cgDelete, /All)', /QUESTION)
  if ok eq 'Yes' then cgDelete, /All
  

end

pro examples_w_ScatterPlot
  
  x = LOADDATA(17)
  n = N_ELEMENTS(x)
  y = x * 2. + 1.
    
  w_ScatterPlot, x, y, XTITLE='X title', YTITLE='Y title', TITLE= 'Test plot'
  
  seed = 1
  noise = RandomU(seed, n)*10. - 5.
  y = x + noise + 20.
  
  w_ScatterPlot, x, y, XTITLE='X title', YTITLE='Y title', TITLE= 'Test plot 2', /LEGEND_UL, COLOR='red', PSYM=6  
  
  ok = DIALOG_MESSAGE('Do you want to close all windows? (IDL> cgDelete, /All)', /QUESTION)
  if ok eq 'Yes' then cgDelete, /All
  
end
