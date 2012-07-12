;+
; This object provides an interface for all geo-localized data sets (NCDF, HFD, grids, and more).
; 
; This object is not supposed to be use alone but it should be implemented by child classes. Here 
; is the list of routines that must be implemented by the child class::
;   
;    - w_GISdata::getVarNames
;    - w_GISdata::hasVar
;    - w_GISdata::getVarData
; 
; Following services are given by this interface and should not be re-implemented:: 
;    
;    - w_GISdata::getVarTS
;    - w_GISdata::defineSubset
;    
;
; :History:
;     Written by FaM, 2012.
;-

;+
; :Description:
;    Initialize the object instance
;    
; :Params:
;    grid: in, required, type = {w_Grid2d}
;          the grid corresondng to the dataset
; 
; :Returns: 
;    1 if the object is created successfully, 0 if not
;
;-
function w_GISdata::init, grid, _EXTRA=extra
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  if ~ OBJ_VALID(GRID) then MEssage, '$GRID is not valid'
  if ~ OBJ_ISA(GRID, 'w_Grid2d') then MEssage, '$GRID is not valid'
  
  if ~ self->w_grid2d::Init(GRID=grid) then return, 0

  self.ogrid = grid->reGrid()  
  
  if not self->defineSubset(_EXTRA=extra) THEN return, 0
  
  return, 1
  
end

;+
; :Description:
;    Destroy the object instance
;
;-
pro w_GISdata::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_Grid2D::Cleanup
  undefine, self.ogrid
  
end

;+
; :Description:
;    Get access to some object properties.
;          
; :Keywords:
;    OGRID: out, type={w_grid2d}
;           the original grido object (non subsetted)
;    ORDER: out, type=long
;           order of the data in the original file. 0 means origin is Down left, 1 means Upper left
;    SUBSET: out, type=long array
;            subset into the original file [x0, nx, y0, ny]
;
;-
pro w_GISdata::getProperty, OGRID=ogrid, $
                            ORDER=order, $
                            SUBSET=subset, $
                            _Ref_Extra=extra

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  ogrid=self.ogrid
  order=self.order
  subset=self.subset
  self->w_Grid2D::GetProperty, _Extra=extra
 
end

;+
; :Description:
;    Returns the properties of the object one at a time. Only
;    the properties accessible with the homonym routine are 
;    accessible here.
;
;
; :Params:
;    thisProperty: in, required
;                  A string variable that is equivalent to a field in the object's    
;                  class structure. See the getProperty routine for which properties  
;                  can be returned. The property is case insensitive.
;                  
; :Returns:
;    The value of a particular object property. Note that pointer       
;    properties will return the variable the pointer points to.
;                    
; :History:
;    Modifications::
;     (c) David Fanning
;     FaM, 2012: Adapted to the WAVE
;
;-
function w_GISdata::getProperty, thisProperty

  ; SET UP ENVIRONNEMENT
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  avail = ['TNT_C', $
           'OGRID', $
           'ORDER', $
           'SUBSET']
  
  ; Check if ok
  index = Where(StrPos(avail, str_equiv(thisProperty)) EQ 0, count)
  index = index[0]    
  CASE count OF
    0: Message, 'Property ' + StrUpCase(thisProperty) + ' could not be found.'
    1:
    ELSE: Message, 'Ambiguous property. Use more characters to specify it.'
  ENDCASE
  
  ; Now david fanning's stuff
  
  ; Get the self structure as a structure, rather than as an object.
  Call_Procedure, StrLowCase(Obj_Class(self)) + '__define', classStruct
  
  ; Find the property in this class structure.
  index = Where(StrPos(Tag_Names(classStruct), StrUpCase(thisProperty)) EQ 0, count)
  index = index[0]
  
  ; What happened?
  CASE count OF
    0: Message, 'Property ' + StrUpCase(thisProperty) + ' could not be found.'
    1: propertyValue = self.(index)
    ELSE: Message, 'Ambiguous property. Use more characters to specify it.'
  ENDCASE
  
  ; If this is a pointer, you want the thing pointed to.
  IF Size(propertyValue, /TNAME) EQ 'POINTER' THEN propertyValue = *propertyValue
  return, propertyValue
  
end

;+
; :Description:
;    To obtain the list af available variables in the dataset.
;
; :Keywords:
;    COUNT: out, optional
;           the number of variables
;    PRINT: in, optional
;           set this keyword to print the variables (and info)
;           in the console
;           
; :Returns:
;   An array of variable ids
;
;-
function w_GISdata::getVarNames, COUNT=count, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  count = 0
  
  Message, "If we arrived here, it is that you didn't implement the w_GISdata interface well", /INFORMATIONAL
  
  return, dummy
  
end

;+
; :Description:
;    Checks if a variable is available
;
; :Params:
;    id: in, required
;        the variable ID
;
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data. It can be adapted to your needs,
;          but must contain the tags:: 
;            - name
;            - id
;            - description
;            - unit
;            
; :Returns:
;   1 if the variable is available, 0 if not
;   
;-
function w_GISdata::hasVar, id, INFO=info
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  undefine, info
  
  Message, "If we arrived here, it is that you didn't implement the w_GISdata interface well", /INFORMATIONAL
  
  return, 0

end

;+
; :Description:
;    Get the data for a specific variable, at the dimensions of the subset.
;
; :Params:
;    id: in, required
;        the variable ID
;
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data. It can be adapted to your needs,
;          but must contain the tags:: 
;            - name
;            - id
;            - description
;            - unit
;    _EXTRA: in/out, optional
;            place holder for your own creative needs when you implement this function
;            
; :Returns:
;   the data array
;-
function w_GISdata::getVarData, id, INFO=info, _EXTRA=extra
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
    
  undefine, info
  
  Message, "If we arrived here, it is that you didn't implement the w_GISdata interface well", /INFORMATIONAL
  
  return, dummy

end

;+
; :Description:
;    Obtain a variable at the nearest grid cell from a point (timeserie)
;
; :Params:
;    id: in, required
;        the Id of the variable to look for
;    x: in, required
;       the X coordinate of the point, defined in `SRC`
;    y: in, required
;       the Y coordinate of the point, defined in `SRC`
;
; :Keywords:
;    SRC: in, optional
;         the source of the (X,Y) couple. If not set, (X,Y) are understood as (I,J)
;         in the current grid.
;    INFO: out, optional
;          INFO structure (see getVarData). Some tags are added to the structure::
;            - original_grid_i: I coordinate of the selected grid point
;            - original_grid_j: J coordinate of the selected grid point
;            - point_lon: the longitude of the selected grid point
;            - point_lat: the latitude of the selected grid point
;            - dist_x: the X distance between the grid point and (X,Y)
;            - dist_y: the Y distance between the grid point and (X,Y)
;    BILINEAR: in, optional
;              if set, bilinear interpolation is performed instead of 
;              nearest neighbor
;
;    _EXTRA: in, optional
;            any keyword accepted by `getVarData` (or its child implementations)
;
; :Returns:
;   1 if the subset has been set correctly, 0 if not
;   
;-
function w_GISdata::getVarTS, id, x, y, SRC=src, INFO=info, BILINEAR=bilinear, _EXTRA=extra
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
    
  undefine, info
  if N_PARAMS() lt 3 then Message, WAVE_Std_Message(/NARG)  
  if ~arg_okay(Id, /SCALAR) then MEssage, WAVE_Std_Message('id', /SCALAR)
  
  if ~ self->hasVar(id, INFO=info) then Massage, 'Variable not found: ' + str_equiv(id)
    
  ; Gis
  if N_ELEMENTS(src) EQ 0 then _src = self else _src = src  
  ; This is to obtain the indexes in the original grid
  self.ogrid->transform,  x, y, point_i, point_j, SRC=_src, /NEAREST, E_DST=_x, N_DST=_y  
  ; This is to obtain lat and lons of the selected grid point
  self.ogrid->transform, point_i, point_j, dummy, dummy, src=self.ogrid, $
         LON_DST=point_lon, LAT_DST=point_lat, E_DST=point_x, N_DST=point_y    
  dist_x = _x - point_x
  dist_y = _y - point_y   
  self.ogrid->getProperty, TNT_C=oc
  if point_i lt 0 or point_i ge oc.nx $
   or point_j lt 0 or point_j ge oc.ny then Message, 'Nearest grid point lies outside the grid.'
  
  
  ; Out 
  _info = {original_grid_i:point_i, original_grid_j:point_j, point_lon:point_lon, point_lat:point_lat, dist_x:dist_x, dist_y:dist_y}
  if arg_okay(info, /STRUCT) then info = CREATE_STRUCT(info, _info) else info = _info
  
  _subset = self.subset
  if KEYWORD_SET(BILINEAR) then begin
    self.ogrid->transform,  x, y, d_i, d_j, SRC=_src
    point_i = FLOOR(d_i)
    point_j = FLOOR(d_j)    
    if point_i+1 ge oc.nx $
     or point_j+1 ge oc.ny then Message, 'Nearest grid point for interpolation lies outside the grid.'
    if point_i lt 0 $
     or point_j lt 0 then Message, 'Nearest grid point for interpolation lies outside the grid.'
     
    self.subset = [point_i, 2, point_j, 2]
    value = self->getVarData(id, _EXTRA=extra)        
    if SIZE(value, /TNAME) ne 'DOUBLE' then begin
      d_i = FLOAT(d_i)
      d_j = FLOAT(d_j)      
    endif    
    d_i -= point_i & d_j -= point_j
    wx = [1 - d_i, d_i]
    wy = [1 - d_j, d_j]
    r0 = wx[0] * value[0,0,*] + wx[1] * value[1,0,*]
    r1 = wx[0] * value[0,1,*] + wx[1] * value[1,1,*]        
    value = REFORM(wy[0] * r0 +  wy[1] * r1)     
  endif else begin    
    self.subset = [point_i, 1, point_j, 1]
    value = self->getVarData(id, _EXTRA=extra)    
  endelse
  self.subset= _subset
  
  return, value

end

;+
; :Description: 
;    Defines a subset of the GIS data. This has two major consequences. First,
;    the geolocalisation of the object is changed (w_grid2D) accordingly. 
;    Second, the `GetVarData` fucntion is automatically returning data corresponding
;    to the chosen subset.
;    
;    Call this function without arguments to reset the GIS data object to its original
;    form.
;    
;    Internally, the w_Grid2D::set_ROI method is called and the indexes of the
;    spatially heterogen ROI are then used to create the subset.
;    
;    Currently one can define a subset with 6 different methods::
;      1. SHAPE: use a .shp file as imput, the ROI is generated using the 
;                IDLanROI object to transform the vertices into a mask
;                (don't forget to set the SRC keyword)
;      2. POLYGON: similar to SHAPE but the polygon is directly specified
;                 (don't forget to set the SRC keyword)
;      3. MASK: simply define the ROI using a mask (of dimensions corresponding to the grid)
;      4. CROPBORDER: remove a certain number of pixels from the borders of the grid
;      5. GRID: the ROI is defined by the position of another grid within the object grid
;      6. CORNERS: the ROI is defined by setting the LL and UR corners of the desired subset
;                 (don't forget to set the SRC keyword)
;
;
; :Keywords:
;    SHAPE: in, type = string
;           the shapefile to read (.shp), coordinate system defined by `SRC`
;    POLYGON: in, type = array
;             not implemented yet, coordinate system defined by `SRC`
;    MASK: in, type = array
;          a mask of the desired ROI (dimensions must match the grid)
;    CROPBORDER: in, type = long
;                number of pixels to remove from each border
;    GRID: in, type = w_Grid2D
;          a grig object
;    CORNERS: in, type = array 
;            LL and UR corners of the desired subset ([XLL,YLL,XUR,YUR]), coordinate system defined by `SRC`
;    NO_ERASE: in, type = boolean
;              set this keyword to update the ROI in place of replacing it
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
; :Returns:
;   1 if the subset has been set correctly, 0 if not
;
;-
function w_gisdata::defineSubset, $
    SHAPE=shape,  $
    POLYGON=polygon, MASK=mask,  $
    CROPBORDER=cropborder,  $
    GRID=grid,    $
    CORNERS=corners, $
    NO_ERASE=no_erase, $
    SRC=src, $
    REMOVE_ENTITITES=remove_entitites, $
    KEEP_ENTITITES=keep_entitites, $
    MARGIN=margin, $
    ROI_MASK_RULE=roi_mask_rule

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    self.subset = [0,0,0,0]
    RETURN, 0
  ENDIF
  
  if ~ OBJ_VALID(self.ogrid) then Message, 'Original grid not initialized yet!'
            
  if not self.ogrid->set_ROI(SHAPE=shape,  $
    POLYGON=polygon, MASK=mask,  $
    CROPBORDER=cropborder,  $
    GRID=grid,    $
    CORNERS=corners, $
    NO_ERASE=no_erase, $
    SRC=src, $
    REMOVE_ENTITITES=remove_entitites, $
    KEEP_ENTITITES=keep_entitites, $
    ROI_MASK_RULE=roi_mask_rule) then Message, 'Something went wrong while making a ROI.'
  
  if self.ogrid->is_ROI() then begin
    self.ogrid->get_ROI, SUBSET=subset, MARGIN=margin
    case self.order of
      0: begin
       self.subset = subset
      end
      1: begin
       self.ogrid->getProperty, TNT_C=c
       subset[2] = c.ny - subset[2]  - 1 ; up an down TODO: test this!
       self.subset = subset
      end
      else: Message, 'Order not ok'
    endcase

    new_grid = self.ogrid->reGrid(/TO_ROI, MARGIN=margin)
    IF NOT self->w_Grid2D::ReInit(GRID=new_grid) then Message, 'Something went wrong while making a new grid.'
    dummy = self.ogrid->set_ROI()    
    undefine, new_grid
  endif else begin
    self.subset = [0,0,0,0]
  endelse
    
  return, 1
    
end

;+
; :Description:
;    Plots a desired variable for quick visualisation purposes
;             
; :Params:
;    id : in, required, type = integer/ string
;         variable index
;
; :Keywords:
;    WID: out
;         the widget id   
;    _EXTRA: in, optional
;            Any keyword accepted by the getVarData() function
;    
;-
pro w_GISdata::QuickPlotVar, id, WID=wid, _EXTRA = extra
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN
  ENDIF
  
  if ~ self->hasVar(id, INFO=info) then Message, '$' + str_equiv(id) + ' is not a correct variable ID.' 
  var = self->getVarData(id, time, _EXTRA=extra)

  title = info.name + ' - ' + info.description 
  
  self->Get_LonLat, lon, lat, nx, ny  
  
  if N_ELEMENTS(time ne 0) then begin ; We found the time dimension in the file  
    tsrt = TIME_to_STR(time)
  endif   
  
  nd = SIZE(var, /N_DIMENSIONS)
  case nd of
    2: begin
       dimnames = ['EAST-WEST','SOUTH-NORTH']
       w_QuickPlot, var, COLORTABLE=13, TITLE=title, WINDOW_TITLE='QuickPlot: ' + info.name, $
                      dimnames=dimnames, CBARTITLE=info.unit, COORDX=lon, COORDY=lat, WID=wid 
    end
    3: begin
       dimnames = ['EAST-WEST','SOUTH-NORTH','TIME']
       w_QuickPlot, var, COLORTABLE=13, TITLE=title, WINDOW_TITLE='QuickPlot: ' + info.name, $
                      dimnames=dimnames, CBARTITLE=info.unit, COORDX=lon, COORDY=lat, WID=wid , $
                       dim3tags=tsrt
    end
    4: begin
       dimnames = ['EAST-WEST','SOUTH-NORTH','Z','TIME']
       w_QuickPlot, var, COLORTABLE=13, TITLE=title, WINDOW_TITLE='QuickPlot: ' + info.name, $
                      dimnames=dimnames, CBARTITLE=info.unit, COORDX=lon, COORDY=lat, WID=wid , $
                       dim4tags=tsrt
    end
    else: MESSAGE, 'Variable is not of suitable dimension.'
  endcase      

end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_gisdata__Define
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_GISdata                       , $
            INHERITS w_Grid2D               , $
            ogrid  : OBJ_NEW()              , $ ; grid of the original datafile (uncropped) (the geoloc of the current cropped file is managed by the parent class)
            order  : 0L                     , $ ; order of the data in the original file. 0 means origin is Down left, 1 means Upper left
            subset : [0L,0L,0L,0L]            $ ; subset into the original file [x0, nx, y0, ny]
            }
    
end