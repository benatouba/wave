;+
; :Description:
;    Utilitary routine to plot WRF landcover (LU_INDEX) on a map. It just
;    sets the map levels and colors accordingly and returns the map for later
;    plotting. Additionally, it returns the bar tags for legend.
;    
;    Default behiavior is to remove the land classes that are not present in
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
pro w_wrf_Landcover_map, map, wrf, bar_tags, bar_title, ALL_CLASSES = all_classes

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR, 2
  
  if ~OBJ_VALID(map) and ~OBJ_ISA(map, 'w_Map') then message, WAVE_Std_Message('map', /ARG)
  if ~OBJ_VALID(wrf) and ~OBJ_ISA(wrf, 'w_wrf') then message, WAVE_Std_Message('wrf', /ARG)
  
  toplot = (wrf->get_var('lucat'))[*,*,0]
  
  levels = [INDGEN(24)+1,28]
  
  bar_tags = ['Urban and Built-up Land' , $ ; 246, 1 , 0
    'Dryland Cropland and Pasture' , $ ; 215, 207, 60
    'Irrigated Cropland and Pasture' , $ ; 174, 114, 41
    'Mixed Dryland/Irrigated Cropland and Pasture' , $ ;
    'Cropland/Grassland Mosaic' , $; 191 191 122
    'Cropland/Woodland Mosaic' , $; 88 97 40
    'Grassland' , $; 220 215 185
    'Shrubland' , $; 200 180 111
    'Mixed Shrubland/Grassland' , $; 233 240 187
    'Savanna' , $; 253 212 2
    'Deciduous Broadleaf Forest' , $; ; 108 169 102
    'Deciduous Needleleaf Forest' , $; ; 29 101 51
    'Evergreen Broadleaf' , $; 108 169 102
    'Evergreen Needleleaf' , $; ; 29 101 51
    'Mixed Forest' , $; 189 204 147
    'Water Bodies' , $; 72 109 162
    'Herbaceous Wetland' , $; 129 204 184
    'Wooden Wetland' , $; 130 179 176
    'Barren or Sparsely Vegetated' , $; 179 175 164
    'Herbaceous Tundra' , $; 130 186 157
    'Wooded Tundra' , $; 209 187 130
    'Mixed Tundra' , $; 206 221 40
    'Bare Ground Tundra' , $; 206 221 40
    'Snow or Ice' , $ ; 231 239 252
    'Inland Water'];
    
  r = [246,215,174,174,191,88,108,200,233,253,108,29,108,29,189,32,129,130,179,130,209,206,206,231,92]
  g = [1,207,114,114,191,97,169,180,240,212,169,101,169,101,204,69,204,179,175,186,187,221,221,239,129]
  b = [0,60,41,41,122,40,102,111,187,2,102,51,102,51,147,122,184,176,164,157,130,40,40,252,182]
   
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