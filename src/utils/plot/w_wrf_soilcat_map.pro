;+
; :Description:
;    Utilitary routine to plot WRF soil category on a map. It just
;    sets the map levels and colors accordingly and returns the map for later
;    plotting. Additionally, it returns the bar tags for legend.
;    
;    Default behiavior is to remove the soil classes that are not present in
;    the data to reduce the number of classes in the legend. Set the /ALL_CLASSES 
;    keyword to avoid it.
;        
; :Params:
;    map: in, required
;         the map to fill
;    wrf: in, required
;         the wrf object to get the land cover from
;               
;    bar_tags: out
;              the bar tags for legend
;    bar_title: out
;               the bar title for legend
;
; :Keywords:
;    ALL_CLASSES: in, optional
;                 if set, all classes are in the legend. Default is to remove
;                 clases that are not in the data from the legend                 
;
; :History:
;     Written by FaM, 2011.
;
;-
pro w_wrf_soilcat_map, map, wrf, bar_tags, bar_title, ALL_CLASSES = all_classes

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  if ~OBJ_VALID(map) and ~OBJ_ISA(map, 'w_Map') then message, WAVE_Std_Message('map', /ARG)
  if ~OBJ_VALID(wrf) and ~OBJ_ISA(wrf, 'w_wrf') then message, WAVE_Std_Message('wrf', /ARG)
  
  toplot = wrf->get_var('soiltop')
  
  levels = INDGEN(16)+1
  
  bar_tags = ['Sand' , $  ; 255 255 0
    'Loamy Sand' , $ ; 237, 244, 33
    'Sandy Loam' , $ ; 234, 221, 0
    'Silt Loam' , $ ; 137 164 173
    'Silt' , $ ; 209 187 130
    'Loam' , $ ; 209 187 150
    'Sandy Clay Loam' , $ ; 160 91 0
    'Silty Clay Loam' , $ ; 215 139 0
    'Clay Loam' , $ ; 214 188 22
    'Sandy Clay' , $ ; 160 91 91
    'Silty Clay' , $ ; 255 212 40
    'Clay' , $ ;  180 50 32
    'Organic Material' , $ ; 13 107 22
    'Water' , $ ; 72 109 162
    'Bedrock' , $ ; 123 123 123
    'Other (land-ice)'] ; 255 255 255
    
    
  r = [255,237,234,137,209,209,160,215,214,160,255,180,13,72,123,255]
  g = [255,244,221,164,187,187,91,139,188,91,212,50,107,109,123,255]
  b = [0,33,0,173,130,150,0,0,22,91,40,32,22,162,123,255]
  
  if not KEYWORD_SET(ALL_CLASSES) then begin
    u = toplot[UNIQ(toplot, SORT(toplot))]
    levels = levels[u-1]
    bar_tags = bar_tags[u-1]
    r = r[u-1]
    g = g[u-1]
    b = b[u-1]
  endif
  
  bar_title = 'Category'
  
  d = map->set_Plot_Params()
  d = map->set_data(toplot, wrf)
  d = map->set_Plot_Params(COLORS=[[r],[g],[b]] , LEVELS=levels)
  
end