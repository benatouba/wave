
pro examples_w_TimeLinePlot
  
  data = cgDemoData(17)
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
  data2 = cgDemoData(17)
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
  data3 = cgDemoData(17)
  w_TimeLinePlot, data, time, 'My data', COMENT1='Random data', COLOR1= 'red',  $
                  STYLE1= 5, TITLE='Example single plot with new axis', YTITLE='Unit', range = [-10,110], HORILINE=0,$
                  data2, time2, 'blue', 'My data2', COMENT2='Sample of random data', STYLE2=0, $
                  data3/100., time, 'dark green', 'My data3', COMENT3='Other Units', NEWAXIS=3, NEWTITLE='Units/100', NEWRANGE=[0.2,0.8]
                  
  
  ok = DIALOG_MESSAGE('Do you want to close all windows? (IDL> cgDelete, /All)', /QUESTION)
  if ok eq 'Yes' then cgDelete, /All
  

end

pro examples_w_ScatterPlot
  
  x = cgDemoData(17)
  n = N_ELEMENTS(x)
  y = x * 2. + 1.
    
  w_ScatterPlot, x, y, XTITLE='X title', YTITLE='Y title', TITLE= 'Test plot'
  
  seed = 1
  noise = RandomU(seed, n)*10. - 5.
  y = x + noise + 20.
  
  w_ScatterPlot, x, y, XTITLE='Data 1', YTITLE='Data 2', TITLE= 'My nice scatter plot', /LEGEND_UL, COLOR='red', PSYM=6  
  
  ok = DIALOG_MESSAGE('Do you want to close all windows? (IDL> cgDelete, /All)', /QUESTION)
  if ok eq 'Yes' then cgDelete, /All
  
end


pro examples_w_working_with_gis_objects      

  
end

; Make its own map from the scratch (you will probably never need this...)
pro examples_working_with_w_map, RESIZABLE = resizable 
   
   ; 1. define a grid for the map
   GIS_make_proj, ret, proj, PARAM='1, WGS-84' ; This is the standard lat-lon rectangular projection (easy but ugly ;-)
   x0 = 5.  ; top left corner lon 
   y0 = 60. ; top left corner lat
   x1 = 20. ; bot right corner lon 
   y1 = 45. ; bot right corner lat
   dx = 1. ; Original grid resolution (ignored afterwards)
   dy = 1. ; Original grid resolution (ignored afterwards)  
   grid = OBJ_NEW('w_Grid2D', x0 = x0, y0 = y0, x1 = x1, y1 = y1, dx = dx, dy = dy, proj = proj)
      
   ; 2. Define the map from this grid
   map = OBJ_NEW('w_Map', grid, YSIZE= 600) ; size of the final image
   OBJ_DESTROY, grid ;no need for this anymore
   
   ; 3. Get some info about the map 
   map->show_img, RESIZABLE = resizable
   map->GetProperty, tnt_c = map_coord, XSIZE=xs, YSIZE=ys
      
   print, 'Image size in pixels: [' + STRING(xs, FORMAT='(I3)') + ',' +STRING(ys, FORMAT='(I3)') +']'
   print, 'Resolution of one pix in lon: ' + STRING(map_coord.dx, FORMAT='(F4.2)') + ' and in lat: ' + STRING(map_coord.dy, FORMAT='(F4.2)')
   
   ; 3. Meliorate the map
   dummy = map->set_map_params(INTERVAL=5, COLOR='black', STYLE=1) ; For the lat lons contours
   
   ; shape file
   shp = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/MAPPING/DEU_adm3.shp'
   GIS_make_datum, ret, shp_src, NAME = 'WGS-84' ; the source coordinate system of the shape file. In this case, i know this is lat-lon in WGS-84 but for other shapes you currently have to check it by yourself.
   dummy = map->set_shape_file(SHPFILE=shp, shp_src = shp_src, COLOR = 'dark red')
   map->show_img, RESIZABLE = resizable
   
   ; This is too much, I am just interested in Berlin
   dummy = map->set_shape_file() ; remove all shapes
   dummy = map->set_shape_file(/COUNTRIES) ; add countries outlines
   dummy = map->set_shape_file(SHPFILE=shp, shp_src = shp_src, COLOR = 'black', KEEP_ENTITITES=189, THICK=2) 
   ; I looked at my shape file with envi and found out that berlin was the 189th entity
   map->show_img, RESIZABLE = resizable
   
   ;4. get some data to plot
   modisFile = '/home/fab/disk/IDLWorkspace/WAVE_TEST_PACK/MODIS/MODIS_b.hdf'
   modis_ = OBJ_NEW('w_MODIS', FILE=modisFile)
   ; have a look
   modis_->get_Varlist, /PRINT
   lst = modis_->get_Var('LST_Day_1km')
   ; have a first look at the data
   w_QuickPlot, lst, COLORTABLE=13, TITLE='Modis LST', WID=wid
   
   ; I See some values are missing, a simple plot will not look satisfying:
   CTLOAD, 13 ; I want to have it colorfull
   dummy = map->set_plot_params(N_LEVELS=256) ; I want a lot of colors 
   dummy = map->set_data(lst, modis_)  
   map->show_img, RESIZABLE = resizable
   map->show_color_bar, RESIZABLE = resizable
   
   ; So I prepare my data so it looks better:   
   pmissing = where(lst lt 1, cnt)  
   lst = lst - 273.15 ; to kelvins
   lst[pmissing] = -999.
   
   ;5. Plot it   
   dummy = map->set_plot_params(N_LEVELS=256, NEUTRAL_COLOR='grey') ; I want a lot of colors 
   dummy = map->set_data(lst, modis_, MISSING=-999.)  
   map->show_img, RESIZABLE = resizable
   map->show_color_bar, RESIZABLE = resizable
   
   ;6. Alternative
   pok = where(lst gt 1, cnt)  
   cgWindow, WXSIZE=500, WYSIZE=300
   cgHistoplot, lst[pok], BINSIZE = 3, LOCATIONS = locs, /WINDOW
  
    ok = DIALOG_MESSAGE('Do you want to close all windows?', /QUESTION) ;else ok = 'no'
    if ok eq 'Yes' then begin
      cgDelete, /All
      WIDGET_CONTROL, wid, /DESTROY
    endif  
     
   OBJ_DESTROY, modis_
   OBJ_DESTROY, map
  
end

