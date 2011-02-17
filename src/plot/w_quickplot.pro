FUNCTION w_QuickPlot_Aspect, aspectRatio, MARGIN=margin, WindowAspect=wAspectRatio

  ; This function calculates the correct aspect ratios for printing.
  ON_ERROR, 2
  
  ; Check for aspect ratio parameter and possibilities.  
  IF N_PARAMS() EQ 0 THEN aspectRatio = 1.0
  
  IF aspectRatio EQ 0 THEN BEGIN
    MESSAGE, 'Aspect Ratio of 0. Changing to 1...', /Informational
    aspectRatio = 1.0
  ENDIF
  
  s = SIZE(aspectRatio)
  IF s[s[0]+1] NE 4 and s[s[0]+1] NE 5 THEN MESSAGE, 'Aspect Ratio is not a FLOAT. Take care...', /Informational
  
  ; Check for margins.  
  IF N_ELEMENTS(margin) EQ 0 THEN margin = 0.15
  
  ; Error checking.  
  IF margin LT 0 OR margin GE 0.5 THEN $
    MESSAGE, 'The MARGIN keyword value must be between 0.0 and 0.5.'
    
  ; Calculate the aspect ratio of the current window.    
  IF N_Elements(wAspectRatio) EQ 0 THEN wAspectRatio = FLOAT(!D.Y_VSIZE) / !D.X_VSIZE
  
  ; Calculate normalized positions in window.  
  IF (aspectRatio LE wAspectRatio) THEN BEGIN
    xstart = margin
    ystart = 0.5 - (0.5 - margin) * (aspectRatio / wAspectRatio)
    xend = 1.0 - margin
    yend = 0.5 + (0.5 - margin) * (aspectRatio / wAspectRatio)
  ENDIF ELSE BEGIN
    print, 'Houston, the image is smaller in X then the Window ? How did this happen?'
    xstart = 0.5 - (0.5 - margin) * (wAspectRatio / aspectRatio)
    ystart = margin
    xend = 0.5 + (0.5 - margin) * (wAspectRatio / aspectRatio)
    yend = 1.0 - margin
  ENDELSE
  
  position = [xstart, ystart, xend, yend]
  
  RETURN, position
  
END

;-------------------------------------------------------------------------

PRO w_QuickPlot_Zoom_Button_Event, event

  ; Event handler to perform window zooming.

  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; What kind of zooming is wanted?
  
  Widget_Control, event.id, Get_UValue=zoomIt
  CASE zoomIt OF
  
    'ZOOM_IN': BEGIN
      info.plotView->GetProperty, Viewplane_Rect=thisRect
      thisRect(0) = (thisRect(0) + 0.05) < thisRect(2)
      thisRect(1) = (thisRect(1) + 0.05) < thisRect(3)
      thisRect(2) = (thisRect(2) - 0.1) > thisRect(0)
      thisRect(3) = (thisRect(3) - 0.1) > thisRect(1)
      info.plotView->SetProperty, Viewplane_Rect=thisRect
    END
    
    'ZOOM_OUT': BEGIN
      info.plotView->GetProperty, Viewplane_Rect=thisRect
      thisRect(0) = thisRect(0) - 0.05
      thisRect(1) = thisRect(1) - 0.05
      thisRect(2) = thisRect(2) + 0.1
      thisRect(3) = thisRect(3) + 0.1
      info.plotView->SetProperty, Viewplane_Rect=thisRect
    END
    
  ENDCASE
  
  ; Redisplay the view.
  
  info.thisWindow->Draw, info.plotView
  
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END

;-------------------------------------------------------------------

PRO w_QuickPlot_DimChange, event

  ; Event handler to perform image processing options
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; What processing is wanted?  
  Widget_Control, event.id, Get_UValue=thisOperation
  
  dim = STRMID(thisOperation,0,1)
  ind = STRMID(thisOperation,2,3)
   
  CASE dim OF
  
    '3': info.varinds = [long(ind),info.varinds[1]]
    '4': info.varinds = [info.varinds[0],long(ind)]

  ENDCASE
  
  data = info.var[*,*,info.varinds[0],info.varinds[1]]
  
  ; Update image and processed image data.  
  info.thisImage->SetProperty, Data=BytScl(data)
  *info.processPtr = data
  
  tunderTitle = ''
  if info.ndim eq 3 then begin
     if N_ELEMENTS(info.dimnames) gt 2 then tunderTitle =  info.dimnames[2] + ' index : ' + str_equiv(info.VARINDS[0]) $
      else tunderTitle = '3D index : ' + str_equiv(info.VARINDS[0])
  endif
  if info.ndim eq 4 then begin
    if N_ELEMENTS(info.dimnames) gt 3 then tunderTitle =  info.dimnames[2] + ' index : ' + str_equiv(info.VARINDS[0]) + '   ' +info.dimnames[3]  + ' index : ' + str_equiv(info.VARINDS[1]) $
      else tunderTitle = '3D index : ' + str_equiv(info.VARINDS[0]) + '   4D index : ' + str_equiv(info.VARINDS[1])
  end
  
  info.underTitle->GetProperty, STRINGS = str
  info.underTitle->SetProperty, STRINGS = [str[0],tunderTitle]  
  pfin = where(finite(data) eq 1, cntfin)  
  if cntfin eq 0 then MESSAGE, '$data has no finite element.'
  range = [min(data[pfin]), max(data[pfin])]
  if range[0] eq range[1] then range[1] += 1
  info.cbar->SetProperty, Range = range
  typ = SIZE(range, /TYPE)
  if typ eq 1 then range = LONG(range)
  if typ eq 5 then range = FLOAT(range)
  info.minvalueObj->set_value, range[0]
  info.maxvalueObj->set_value, range[1]
  
  ; Redisplay the view.  
  info.thisWindow->Draw, info.plotView
  
  Widget_Control, event.top, Set_UValue=info, /No_Copy
  
END

;-------------------------------------------------------------------

PRO w_QuickPlot_Output, event

   ; This event handler creates output files.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Get a snapshop of window contents. (TVRD equivalent.)

Wait, 0.5 ; To allow menu to disappear.
info.thisWindow->GetProperty, Image_Data=snapshot

   ; What kind of file is wanted?

Widget_Control, event.id, GET_UValue=whichFileType
CASE whichFileType OF

   'GIF': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='w_QuickPlot.gif')
      IF filename NE '' THEN Write_GIF, filename, image2d, r, g, b
      END

   'BMP': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='w_QuickPlot.bmp')
      IF filename NE '' THEN Write_BMP, filename, image2d, r, g, b
      END

   'PNG': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='w_QuickPlot.png')
      IF filename NE '' THEN Write_PNG, filename, image2d, r, g, b
      END

   'PICT': BEGIN

         ; Because we are using RGB color for this model, we have
         ; a 3-m-n array. Use Color_Quan to create a 2D image and
         ; appropriate color tables for the GIF file.

      image2D = Color_Quan(snapshot, 1, r, g, b)
      filename = Dialog_Pickfile(/Write, File='w_QuickPlot.pict')
      IF filename NE '' THEN Write_PICT, filename, image2d, r, g, b
      END

   'JPEG': BEGIN

      filename = Dialog_Pickfile(/Write, File='w_QuickPlot.jpg')
      IF filename NE '' THEN Write_JPEG, filename, snapshot, True=1
      END


   'TIFF': BEGIN

      filename = Dialog_Pickfile(/Write, File='w_QuickPlot.tif')
      IF filename NE '' THEN BEGIN

         ; TIFF files should have their Y direction reversed for
         ; compatibility with most other software.

         Write_TIFF, filename, Reverse(snapshot,3)
      ENDIF
      END

ENDCASE

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO w_QuickPlot_Exit, event

   ; Exit the program via the EXIT button.
   ; The w_QuickPlot_CLEANUP procedure will be called automatically.

Widget_Control, event.top, /Destroy
END
;-------------------------------------------------------------------



PRO w_QuickPlot_Printing, event

   ; PostScript printing and printer setup handled here.

Widget_Control, event.top, Get_UValue=info, /No_Copy

   ; Which button?

Widget_Control, event.id, Get_UValue=ButtonValue
CASE buttonValue OF

   'PRINT': BEGIN
      result = Dialog_PrintJob(info.thisPrinter)
      IF result EQ 1 THEN BEGIN

            ; I want the output on the page to have the same aspect ratio
            ; (ratio of height to width) as I see in the display window.

         info.thisWindow->GetProperty, Dimensions=wdims
         info.thisPrinter->GetProperty, Dimensions=pdims
         plotAspect = Float(wdims[1]) / wdims[0]
         windowAspect = Float(pdims[1]) / pdims[0]
         position = w_QuickPlot_Aspect(plotAspect, WindowAspect=windowAspect, Margin=0.075)
         info.plotView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
            Location=[position[0], position[1]], Units=3

            ; Print it. May take a little time. Alert the user.

         Widget_Control, Hourglass=1
         info.thisPrinter->Draw, info.plotView
         info.thisPrinter->NewDocument
         Widget_Control, Hourglass=0

            ; Put things back the way you found them.

         info.plotView->SetProperty, Location=[0,0], Dimensions=[0,0]

      ENDIF
      END

   'SETUP': BEGIN
      result = Dialog_PrinterSetup(info.thisPrinter)
      IF result EQ 1 THEN BEGIN

            ; I want the output on the page to have the same aspect ratio
            ; (ratio of height to width) as I see in the display window.

         info.thisWindow->GetProperty, Dimensions=wdims
         info.thisPrinter->GetProperty, Dimensions=pdims
         plotAspect = Float(wdims[1]) / wdims[0]
         windowAspect = Float(pdims[1]) / pdims[0]
         position = w_QuickPlot_Aspect(plotAspect, WindowAspect=windowAspect, Margin=0.075)
         info.plotView->SetProperty, Dimensions=[position[2]-position[0], position[3]-position[1]], $
            Location=[position[0], position[1]], Units=3

            ; Print it. May take a little time. Alert the user.

         Widget_Control, Hourglass=1
         info.thisPrinter->Draw, info.plotView
         info.thisPrinter->NewDocument
         Widget_Control, Hourglass=0

            ; Put things back the way you found them.

         info.plotView->SetProperty, Location=[0,0], Dimensions=[0,0]

      ENDIF

      END

ENDCASE

   ; Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO w_QuickPlot_Image_Colors, event

  ; This event handler changes color tables.

  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; What kind of event is this?
  
  thisEvent = Tag_Names(event, /Structure_Name)
  CASE thisEvent OF
  
    "WIDGET_BUTTON": BEGIN
      TVLCT, info.r, info.g, info.b
      XColors, Group_Leader=event.top, NotifyID=[event.id, event.top]
    ENDCASE
    
    "XCOLORS_LOAD": BEGIN
      info.r = event.r
      info.g = event.g
      info.b = event.b
      IF Obj_Valid(info.thisPalette) THEN info.thisPalette->SetProperty, $
        Red=event.r, Green=event.g, Blue=event.b
    ENDCASE
    
  ENDCASE
  
  ; Draw the graphic display.
  
  info.thisWindow->Draw, info.plotView
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------


PRO w_QuickPlot_Change_Range, event

  ; This event handler changes color tables.

  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; What kind of event is this?  
  thisEvent = Tag_Names(event, /Structure_Name)
  CASE thisEvent OF
  
    "FSC_FIELD_EVENT": BEGIN
    
      minR = info.minvalueObj->Get_Value()
      maxR = info.maxvalueObj->Get_Value()
      
      if Finite(minR) and Finite(maxR) then begin    
      
        if ~ arg_okay(minR, /NUMERIC) then mini = info.brange[0] else mini = info.brange[0] > minR < info.brange[1]
        if ~ arg_okay(maxR, /NUMERIC) then maxi = info.brange[1] else maxi = info.brange[0] > maxR < info.brange[1]
        
        if maxi le mini then maxi = info.brange[1]
        if mini ge maxi then mini = info.brange[0]
        
        info.thisImage->SetProperty, DATA=BYTSCL(*info.processPtr, max = maxi, min = mini)
        info.cbar->SetProperty,  Range=[mini, maxi]
        
        info.minvalueObj->Set_Value, mini
        info.maxvalueObj->Set_Value, maxi
        
      endif
      
    ENDCASE
    
  ENDCASE
  
  ; Draw the graphic display.
  
  info.thisWindow->Draw, info.plotView
  Widget_Control, event.top, Set_UValue=info, /No_Copy
END ;------------------------------------------------------------------------------



PRO w_QuickPlot_CLEANUP, id

    ; Come here when the widget dies. Free all the program
    ; objects, pointers, pixmaps, etc. and release memory.

Widget_Control, id, Get_UValue=info
IF N_Elements(info) NE 0 THEN BEGIN
   Obj_Destroy, info.thisContainer
   Ptr_Free, info.imagePtr
   Ptr_Free, info.processPtr
ENDIF
END
;---------------------------------------------------------------------



PRO w_QuickPlot_DrawWidget_Event, event

    ; This event handler handles draw widget events, including
    ; image value selection events.

Widget_Control, event.top, Get_UValue=info, /No_Copy

possibleEvents = ['DOWN', 'UP', 'MOTION', 'SCROLL', 'EXPOSE']
thisEvent = possibleEvents[event.type]
CASE thisEvent OF
   'DOWN': Widget_Control, event.id, Draw_Motion_Events=1
   'UP': Widget_Control, event.id, Draw_Motion_Events=0
   'EXPOSE': info.thisWindow->Draw, info.plotView
   ELSE:
ENDCASE

IF thisEvent NE 'EXPOSE' THEN BEGIN

   hit = info.thisWindow->Pickdata(info.plotView, info.thisImage, [event.x, event.y], xyz)
   xpt = Floor(xyz[0])
   ypt = Floor(xyz[1])
   IF xpt LT 0 OR xpt GT (info.xsize-1) OR ypt LT 0 OR ypt GT (info.ysize-1) THEN BEGIN
      Widget_Control, info.xvalueID, Set_Value=-999
      Widget_Control, info.yvalueID, Set_Value=-999
      if info.addCoord then begin
        Widget_Control, info.xcoordID, Set_Value=-999
        Widget_Control, info.ycoordID, Set_Value=-999
      endif
      IF info.truecolor EQ 0 THEN $
         Widget_Control, info.valueID, Set_Value='-9999.0'
   ENDIF ELSE BEGIN
      Widget_Control, info.xvalueID, Set_Value=xpt
      Widget_Control, info.yvalueID, Set_Value=ypt
      
      if info.addCoord then begin
        Widget_Control, info.xcoordID, Set_Value=info.coordX[xpt,ypt]
        Widget_Control, info.ycoordID, Set_Value=info.coordY[xpt,ypt]
      endif
      
      IF info.truecolor EQ 0 THEN $
         Widget_Control, info.valueID, Set_Value=Float((*info.processPtr)[xpt, ypt])
   ENDELSE

ENDIF

    ;Put the info structure back.

Widget_Control, event.top, Set_UValue=info, /No_Copy
END
;-------------------------------------------------------------------



PRO w_QuickPlot_Resize, event

  ; This is main event handler for the TLB. It handles resize events.
  Widget_Control, event.top, Get_UValue=info, /No_Copy
  
  ; Resize the draw widget.  
  info.thisWindow->SetProperty, Dimension=[event.x > 400, event.y-info.basesize]

; TODO: keepratio doesnt work
;  ; Keep the aspect ratio of the graphic?  
;  IF info.keepAspect THEN BEGIN
;    
;    ; Get the new viewplane rectangle.  
;    waspect = (event.y-info.basesize) / Float(event.x > 400)
;    IF waspect GE 1.0 THEN BEGIN
;      ylen = 1.0 * waspect
;      xlen = 1.0
;      ystart = (-ylen/2.0) + 0.5 ; Center it.
;      xstart = 0.0
;    ENDIF ELSE BEGIN
;      xlen = 1.0 / waspect
;      ylen = 1.0
;      xstart = (-xlen/2.0) + 0.5 ; Center it.
;      ystart = 0.0
;    ENDELSE
;    
;    ; Reset the viewplane rectangle.    
;    info.plotView->SetProperty,Viewplane_Rect=[xstart, ystart, xlen, ylen]
;    
;  ENDIF
  
  ; Redisplay the graphic.  
  info.thisWindow->Draw, info.plotView
  
  ;Put the info structure back.  
  Widget_Control, event.top, Set_UValue=info, /No_Copy
  
END

pro w_quickplot_delete_all

 id = 1
 while (id ne 0) do begin
   id = WIDGET_INFO(0, FIND_BY_UNAME = 'W_QUICKPLOT') 
   WIDGET_CONTROL, id, /DESTROY
 endwhile

end

pro w_QuickPlot, image, $ ; The image to plot (2D, 3D, or 4D)
                CbarTitle = CbarTitle, $ ; The title of the color bar
                CoordX=coordX, $ ; If available, the X coordinates of each pixel into image (2D). If set, the cursor position is indicated below the plot
                CoordY=coordY, $ ; If available, the Y coordinates of each pixel into image (2D). If set, the cursor position is indicated below the plot
                winYSize = wysize, $ ; the size of the window in Y pixels. (default: 500)
                Colortable=colortable,  $ ; the colorbar to use (default: BW)
                XTitle=xtitle,  $ ; X axis title
                YTitle=ytitle, $ ; Y axis title
                Title=Title,  $ ; Plot title
                Window_Title = Window_Title, $ ; Window title          
                dimnames = dimnames,  $ ; the dimensions names, if available (vector of strings of the same lenght as the number of dimensions of image)
                dim3tags = dim3tags,  $ ; vector of strings of the dimension of N_ELEMENTS(image[0,0,*,0]) 
                dim4tags = dim4tags,  $ ; vector of strings of the dimension of N_ELEMENTS(image[0,0,0,*]) 
                INTERPOLATE = interpolate, $ ; bilinear interoplation of the image
                wid = wid, $
                Group_Leader = group
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2   
     
  if N_Params() eq 0 then Return
  if ~KEYWORD_SET(daxis) then daxis = [0,1]
  
  ; Get dimensions of data.  
  s = SIZE(image)
  ndim = s[0]
  truecolor = 0
  
  if ndim lt 2 or ndim gt 4 then Message, 'Must pass a 2D, 3D or 4D image data set.'
  xsize = s[1]
  ysize = s[2]
  ; Calculate the aspect ratios (height/width) for the image + colorBar and for the display window.
  imageAspect = DOUBLE(ysize) / xsize  
   
  addCoord = FALSE  
  ; Check for keyword parameters. Define default values if necessary.  
  if N_Elements(xrange) eq 0 then xrange = [0,xsize]
  if N_Elements(yrange) eq 0 then yrange = [0,ysize]
  if N_Elements(wysize) eq 0 then wysize = 500
  wxsize = wysize / imageAspect < 800  ;for the color bar
  if N_Elements(xtitle) eq 0 then xtitle = ''
  if N_Elements(ytitle) eq 0 then ytitle = ''
  if N_Elements(Title) eq 0 then Title=''
  if N_Elements(colortable) eq 0 then colortable = 0
  if N_Elements(Window_Title) eq 0 then Window_Title = 'w_QuickPlot quick view interface'
  if N_ELEMENTS(CoordX) ne 0 then begin
    addCoord = TRUE
  endif else begin
    CoordX = 0
    coordY = 0
  endelse
  
  keepAspect = Keyword_Set(keepAspect)
  windowAspect = Float(wysize) / wxsize
  if windowAspect gt (2*imageAspect) then begin
    wysize = 1.3 * imageAspect * wxsize
    windowAspect = Float(wysize) / wxsize
  endif
  
  ; If this is an 8-bit image, we will need a color palette.  
  thisPalette = Obj_New('IDLgrPalette')
  thisPalette->LoadCT, colortable
  thisPalette->GetProperty, Red=r, Blue=b, Green=g
  
  ; Define other drawing colors.  
  bckColor = [255, 255, 255]
  titleColor = [0, 0, 60]
  axisColor = [100, 100, 100]
  
  varinds=[0,0]  
  ; Create the image object.
  imagePtr = Ptr_New(image[*,*,varinds[0],varinds[1]])
  processPtr = Ptr_New(image[*,*,varinds[0],varinds[1]])
  thisImage = Obj_New('IDLgrImage', BytScl(*imagePtr), Dimensions=[xsize,ysize], Palette=thisPalette, INTERPOLATE = interpolate)
    
  ; Create scaling parameters for the image. I get position coordinates for a normalized window from
  ; my w_QuickPlot_Aspect function. Then use my FSC_Normalize function to create scaling factors for the image.
  ; Compute optimal x and y margins
  
  relBar = 20D / wxsize
  relmarg = 40D / wxsize
  relImx = DOUBLE(xsize) / wxsize
  relImy = DOUBLE(ysize) / wysize
  
  barImAspect = relImy / (relBar+relmarg+relImx)  
  pos = w_QuickPlot_Aspect(ImageAspect, WindowAspect=windowAspect, Margin=0.14)      
  largY = (pos[2]-pos[0]) * wxsize * imageAspect /  wysize    
  top = 0.5 + largY / 2
  bot = 0.5 - largY / 2
  shif= 0.14 * 2. / 3. 
    
  posim  = [pos[0] + shif, bot, pos[2] + shif, top]    
  posbar = [posim[0]-relmarg-relBar, bot, posim[0]-relmarg, top]   

  titlecenter = (posim[2]-posim[0]) / 2. + posim[0]
    
  xs = FSC_Normalize([0,xsize], Position=[posim[0], posim[2]])
  ys = FSC_Normalize([0,ysize], Position=[posim[1], posim[3]])
  thisImage->SetProperty, XCoord_Conv=xs, YCoord_Conv=ys
    
  ; Set up font objects for the axes titles.  
  helvetica14pt = Obj_New('IDLgrFont', 'Helvetica', Size=10)
  helvetica10pt = Obj_New('IDLgrFont', 'Helvetica', Size=8)
  
  ; Create title objects for the axes. Color them yellow.  
  xTitle = Obj_New('IDLgrText', xtitle, Recompute_Dimensions=1, Font=helvetica10pt)
  yTitle = Obj_New('IDLgrText', ytitle, Recompute_Dimensions=1, Font=helvetica10pt)
  
  ; Create a plot title object. I want the title centered just above the upper X Axis.  
  plotTitle = Obj_New('IDLgrText', Title, Color=titleColor, $
    Location=[0.5, pos[3]+0.05, 0.0], Alignment=0.5, Font=helvetica14pt, $
    Recompute_Dimensions=1)
  
  ; Create an undertitle object.
  underTitle = 'Dimensions : ['
  for i=0, ndim - 1 do begin
    if N_ELEMENTS(dimnames) gt i then  underTitle += dimnames[i] + ': '
    underTitle += str_equiv(s[i+1])
    if i eq ndim - 1 then underTitle += ']' else underTitle += ', '
  endfor
  
  tunderTitle = ''
  if ndim eq 3 then begin
     if N_ELEMENTS(dimnames) gt 2 then tunderTitle =  dimnames[2] + ' index : ' + str_equiv(VARINDS[0]) $
      else tunderTitle = '3D index : ' + str_equiv(VARINDS[0])
  endif
  if ndim eq 4 then begin
    if N_ELEMENTS(dimnames) gt 3 then tunderTitle =  dimnames[2] + ' index : ' + str_equiv(VARINDS[0]) + '   ' +dimnames[3]  + ' index : ' + str_equiv(VARINDS[1]) $
      else tunderTitle = '3D index : ' + str_equiv(VARINDS[0]) + '   4D index : ' + str_equiv(VARINDS[1])
  end
  underTitle = Obj_New('IDLgrText', [underTitle , tunderTitle], Color=axisColor, $
    Location=[[1-0.02,0.05], [1-0.02,0.01]], Alignment=1, Font=helvetica10pt, $
    Recompute_Dimensions=1)
    
  ; Set up scaling for the axes. Notice that we use the axis
  ; range here, not the size of the image, as before. 
  xs = FSC_Normalize(xrange, Position=[posim[0], posim[2]])
  ys = FSC_Normalize(yrange, Position=[posim[1], posim[3]])
  
  ; Create the four axis objects (box axes). Make the titles
  ; with helvetica 10 point fonts.
  
  xAxis1 = Obj_New("IDLgrAxis", 0, Color=axisColor, Ticklen=0.025, $
    Minor=4, Range=xrange, /Exact, Title=xTitle, XCoord_Conv=xs,  $
    Location=[1000, posim[1], 0.125])
  xAxis1->GetProperty, Ticktext=xAxisText
  xAxisText->SetProperty, Font=helvetica10pt, Recompute_Dimensions=2
  
  xAxis2 = Obj_New("IDLgrAxis", 0, Color=axisColor, Ticklen=0.025, $
    Minor=4, /NoText, Range=xrange, TickDir=1, /Exact, XCoord_Conv=xs, $
    Location=[1000, posim[3], 0.125])
    
  yAxis1 = Obj_New("IDLgrAxis", 1, Color=axisColor, Ticklen=0.025, $
    Minor=4, Title=ytitle, Range=yrange, /Exact, YCoord_conv=ys, $
    Location=[posim[0] ,1000, 0.125])
  yAxis1->GetProperty, Ticktext=yAxisText
  yAxisText->SetProperty, Font=helvetica10pt, Recompute_Dimensions=2
  
  yAxis2 = Obj_New("IDLgrAxis", 1, Color=axisColor, Ticklen=0.025, $
    Minor=4, /NoText, Range=yrange, TickDir=1, /Exact, YCoord_conv=ys, $
    Location=[posim[2], 1000, 0.125])
    
  ; Create colorbar 
  pfin = where(finite(*imagePtr) eq 1, cntfin)  
  if cntfin eq 0 then MESSAGE, '$data has no finite element.'
  brange = [min((*imagePtr)[pfin]), max((*imagePtr)[pfin])]
  if brange[0] eq brange[1] then brange[1] += 1
  cbar =  Obj_New('VColorBar', Palette=THISPALETTE, Range=brange, $
   Position=posbar, color = axisColor, Title=CbarTitle)

    
  ; Create a plot model and add axes, image, title to it.    
  plotModel = Obj_New('IDLgrModel', Select=1)
  plotModel->Add, thisImage
  plotModel->Add, xAxis1
  plotModel->Add, xAxis2
  plotModel->Add, yAxis1
  plotModel->Add, yAxis2
  plotModel->Add, plotTitle
  plotModel->Add, underTitle
  plotModel->Add, cbar
  
  ; Create a plot view. Add the model to the view. Notice
  ; the view is created to give space around the region
  ; where the "action" in the plot takes place. The extra
  ; space has to be large enough to accomodate axis annotation.  
  viewRect = [0.0, 0.0, 1.0, 1.0]
  plotView = Obj_New('IDLgrView', Viewplane_Rect=viewRect, $
    Location=[0,0], Color=bckColor)
  plotView->Add, plotModel
  
  ; Create the widgets for this program.  
  tlb = Widget_Base(Title=Window_Title, MBar=menubase, TLB_Size_Events=1, Column=1, Base_Align_Center=1, UNAME = 'W_QUICKPLOT')
    
  ; Create the draw widget. RETAIN=0 is necessary to generate EXPOSE events.    
  drawID = Widget_Draw(tlb, XSize=wxsize, YSize=wysize, $
    Graphics_Level=2, Expose_Events=1, Retain=0, $
    Event_Pro='w_QuickPlot_DrawWidget_Event', Button_Events=1)
    
  ; Create image range widgets.    
  typ = SIZE(brange, /TYPE)
  if typ eq 1 then brange = LONG(brange)
  if typ eq 5 then brange = FLOAT(brange)
  medBase = Widget_Base(tlb, Colum=1, /Base_Align_Center)
  medlabelBase = Widget_Base(medBase, Row=1)
  rangeBase = Widget_Base(medBase, Row=1)
  minvalueID = FSC_Field(rangeBase, Title='Range min:', Value=brange[0], DECIMAL = 2,  $
      XSize=8, OBJECT=minvalueObj, /CR_ONLY, EVENT_PRO='w_QuickPlot_Change_Range')
  maxvalueID = FSC_Field(rangeBase, Title='Range max:', Value=brange[1], DECIMAL = 2,  $
      XSize=8, OBJECT=maxvalueObj, /CR_ONLY, EVENT_PRO='w_QuickPlot_Change_Range')
    
  ; Create image value and location widgets.    
  bottomBase = Widget_Base(tlb, Colum=1, /Base_Align_Center)
  labelBase = Widget_Base(bottomBase, Row=1)
;  dummy = Widget_Label(labelBase, Value='Click (and drag) inside image for Image Value')
  if addCoord then begin
    valueBase = Widget_Base(bottomBase, Row=2)
    xvalueID = CW_Field(valueBase, Title='X Location:', Value=-999, /Integer, XSize=6, ROW=1)
    yvalueID = CW_Field(valueBase, Title='  Y Location:', Value=-999, /Integer, XSize=6, ROW=1)
    valueID =  CW_Field(valueBase, Title='  Image Value:', Value='-9999.0', XSize=12, /Float, ROW=1)
    xcoordID = CW_Field(valueBase, Title='X Coord   :', Value=-999, /Float, XSize=6, ROW=2)
    ycoordID = CW_Field(valueBase, Title='  Y Coord   :', Value=-999, /Float, XSize=6, ROW=2)    
  endif else begin
    valueBase = Widget_Base(bottomBase, Row=1)
    xvalueID = CW_Field(valueBase, Title='X Location:', Value=-999, /Integer, XSize=6)
    yvalueID = CW_Field(valueBase, Title='  Y Location:', Value=-999, /Integer, XSize=6)
    xcoordID = -1
    ycoordID = -1
    valueID =  CW_Field(valueBase, Title='  Image Value:', Value='-9999.0', XSize=12, /Float)
  endelse
  
 
  
  ; Create FILE menu buttons for printing and exiting.  
  filer = Widget_Button(menubase, Value='File', /Menu)
  
  ; Create OUTPUT menu buttons for formatted output files.  
  output = Widget_Button(filer, Value='Save As...', /Menu)
  
  ; Check for availability of Gif files.  
  thisVersion = Float(!Version.Release)
  if thisVersion lt 5.4 then haveGif = 1 ELSE haveGif = 0
  
  if havegif then b = Widget_Button(output, Value='Gif File', $
    UValue='Gif', Event_Pro='w_QuickPlot_Output')
  button = Widget_Button(output, Value='JPEG File', $
    UValue='JPEG', Event_Pro='w_QuickPlot_Output')
  button = Widget_Button(output, Value='TifF File', $
    UValue='TifF', Event_Pro='w_QuickPlot_Output')
  button = Widget_Button(output, Value='PNG File', $
    UValue='PNG', Event_Pro='w_QuickPlot_Output')
  button = Widget_Button(output, Value='PICT File', $
    UValue='PICT', Event_Pro='w_QuickPlot_Output')
  button = Widget_Button(output, Value='BMP File', $
    UValue='BMP', Event_Pro='w_QuickPlot_Output')
    
  dummy = Widget_Button(filer, Value='Print', $
    Event_Pro='w_QuickPlot_Printing', UValue='PRINT')
  dummy = Widget_Button(filer, Value='Print Setup', $
    Event_Pro='w_QuickPlot_Printing', UValue='SETUP')
  dummy = Widget_Button(filer, /Separator, Value='Quit', $
    Event_Pro='w_QuickPlot_Exit')
 
  ; Create COLORS menu buttons for changing colors.
  colorID = Widget_Button(menubase, Value='Colors', /Menu)
  ; Image Colors
  imagecolors = Widget_Button(colorID, Value='Image Colors...', Event_Pro='w_QuickPlot_Image_Colors')
    
   ; Create other dims menu buttons.
   if ndim gt 2 then begin
     ; dim 3
     if N_ELEMENTS(dimnames) gt 2 then buttname = dimnames[2] else buttname = '3D Vars'
     d3 = Widget_Button(menubase, Menu=1, Value=buttname)
     nd = s[3]
     nten = (nd) / 10 + 1
     if ~KEYWORD_SET(dim3tags) then dim3tags = STRING(INDGEN(nd), FORMAT = '(I3)')
     tentags = STRING(INDGEN(nten), FORMAT = '(I3)')
     for e = 0, nten-1 do begin
       ten = Widget_Button(d3, Value=tentags[e], /Menu)
       for t = e*10, ((e+1)*10 < nd-1) do dummy = Widget_Button(ten, Value=dim3tags[t], UValue='3_'+STRING(t, FORMAT = '(I3)'), Event_Pro='w_QuickPlot_DimChange')
     endfor
     if ndim eq 4 then begin
       ; dim 4
       if N_ELEMENTS(dimnames) gt 3 then buttname = dimnames[3] else buttname = '4D Vars'
       d4 = Widget_Button(menubase, Menu=1, Value=buttname)
       nd = s[4]
       nten = (nd) / 10 + 1
       if ~KEYWORD_SET(dim4tags) then dim4tags = STRING(INDGEN(nd), FORMAT = '(I3)')
       tentags = STRING(INDGEN(nten), FORMAT = '(I3)')
       for e = 0, nten-1 do begin
         ten = Widget_Button(d4, Value=tentags[e], /Menu)
         for t = e*10, ((e+1)*10 < nd-1) do dummy = Widget_Button(ten, Value=dim4tags[t], UValue='4_'+STRING(t, FORMAT = '(I3)'), Event_Pro='w_QuickPlot_DimChange')
       endfor
     endif
   end
   
  if N_ELEMENTS(dimnames) eq 0 then dimnames = ['3D Vars','4D Vars']
        
  ; Realize the widgets and get the window object.    
  Widget_Control, tlb, /Realize
  Widget_Control, drawID, Get_Value=thisWindow
  
  ; Find out the size of the base below the image
  ; so you can keep it in the face of window resizing.  
  geom = Widget_Info(bottomBase, /Geometry)
  basesize = geom.scr_ysize
  
  ; Load the palette into the window. This will cause the
  ; image to be output through the palette always, even
  ; when displayed on 24-bit displays.  
  thisWindow->SetProperty, Palette=thisPalette
  
  ; Draw the graphic in the window.  
  thisWindow->Draw, plotView
  
  ; Get a printer object for this graphic.  
  thisPrinter = Obj_New('IDLgrPrinter')
  
  ; Create a container object to hold all the other
  ; objects. This will make it easy to free all the
  ; objects when we are finished with the program.
  ; Make a container to hold all the objects you created.  
  thisContainer = Obj_New('IDL_Container')
  thisContainer->Add, thisWindow
  thisContainer->Add, plotView
  thisContainer->Add, thisPrinter
  thisContainer->Add, xTitle
  thisContainer->Add, yTitle
  thisContainer->Add, thisPalette
  thisContainer->Add, helvetica14pt
  thisContainer->Add, helvetica10pt
  thisContainer->Add, xaxis1
  thisContainer->Add, xaxis2
  thisContainer->Add, yaxis1
  thisContainer->Add, yaxis2
  thisContainer->Add, plotTitle
  thisContainer->Add, underTitle
  thisContainer->Add, cbar
  thisContainer->Add, minvalueObj
  thisContainer->Add, maxvalueObj
  
  s = Size(*imagePtr, /Dimensions)
  
  ; Create an info structure to hold program information.
  
  info = { thisContainer:thisContainer, $  ; The container object.
    thisPalette:thisPalette, $      ; The palette for INDEXED color.
    thisWindow:thisWindow, $        ; The window object.
    plotView:plotView, $            ; The view that will be rendered.
    thisPrinter:thisPrinter, $      ; The printer object.
    thisImage:thisImage, $          ; The image object.
    processPtr:processPtr, $        ; The pointer to the processed image.
    imagePtr:imagePtr, $            ; The pointer to the original image.
    xsize:xsize, $                  ; The X size of the image
    ysize:ysize, $                  ; The Y size of the image.
    plotTitle:plotTitle, $          ; The plot title.
    underTitle:underTitle, $        ; The undertitle.
    viewRect:viewRect, $            ; The original viewplane rectangle.
    xAxis1:xAxis1, $                ; Bottom X axis.
    xAxis2:xAxis2, $                ; Top X axis
    yAxis1:yAxis1, $                ; Left Y axis.
    yAxis2:yAxis2, $                ; Right Y axis.
    r:r, $                          ; The red color table values.
    g:g, $                          ; The green color table values.
    b:b, $                          ; The blue color table values.
    bckColor:bckColor, $            ; The background color.
    titleColor:titleColor, $        ; The title color.
    axisColor:axisColor, $          ; The axis color.
    truecolor:truecolor, $          ; A flag indicating a 24-bit image.
    xvalueID:xvalueID, $            ; X location widget ID.
    yvalueID:yvalueID, $            ; Y location widget ID.
    brange:brange, $                ; data range (orig)
    minvalueID:minvalueID, $        ; minval location widget ID.
    maxvalueID:maxvalueID, $        ; maxval location widget ID.
    minvalueObj:minvalueObj, $      ; minval object.
    maxvalueObj:maxvalueObj, $      ; maxval object.
    valueID:valueID, $              ; Image value widget ID.
    basesize:basesize, $            ; The height of the label base.
    keepAspect:keepAspect, $        ; The image keep aspect flag.
    varinds:varinds, $              ; The indexes in dim 3 and 4 from which the current image is taken
    ndim:ndim, $                    ; The dims of the var to plot
    var:image , $                   ; The variable
    cbar:cbar , $                   ; The colorbar
    addCoord:addCoord , $           ; The coordinates
    dimnames:dimnames , $           ; The coordinates
    coordX:coordX , $               ; The coordinates
    coordY:coordy , $               ; The coordinates
    xcoordID:xcoordID , $           ; The coordinates
    ycoordID:ycoordID , $           ; The coordinates
    drawID:drawID }                 ; The draw widget ID.
    
  Widget_Control, tlb, Set_UValue=info, /No_Copy
  
  XManager, 'w_QuickPlot', tlb, Cleanup='w_QuickPlot_Cleanup', Group_Leader=group, /No_Block, Event_Handler='w_QuickPlot_Resize'
  wid = tlb
  
END