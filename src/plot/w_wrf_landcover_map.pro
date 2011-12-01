pro w_wrf_Landcover_map, map, landcover, bar_tags, ALL_CLASSES = all_classes

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  lu_tags = ['Urban and Built-up Land' , $ ; 246, 1 , 0
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
  
  ;      r = [246,215,174,174,191,88,108,200,233,253,108,29,108,29,189,32,129,130,179,130,209,206,206,255,92]
  ;      g = [1,207,114,114,191,97,169,180,240,212,169,101,169,101,204,69,204,179,175,186,187,221,221,0,129]
  ;      b = [0,60,41,41,122,40,102,111,187,2,102,51,102,51,147,122,184,176,164,157,130,40,40,255,182]
    
  levels = [INDGEN(24)+1,28]
    
  if not KEYWORD_SET(ALL_CLASSES) then begin
    u = landcover[UNIQ(landcover, SORT(landcover))]
    for i=0, N_ELEMENTS(u)-1 do begin
      p = where(levels eq u[i])
      if N_ELEMENTS(_r) eq 0 then _r = r[p] else _r = [_r,r[p]]
      if N_ELEMENTS(_g) eq 0 then _g = g[p] else _g = [_g,g[p]]
      if N_ELEMENTS(_b) eq 0 then _b = b[p] else _b = [_b,b[p]]
      if N_ELEMENTS(levs) eq 0 then levs = levels[p] else levs = [levs,levels[p]]
      if N_ELEMENTS(bar_tags) eq 0 then bar_tags = lu_tags[p] else bar_tags = [bar_tags,lu_tags[p]]
    endfor
  endif else begin
   levs = levels
   bar_tags = lu_tags
  endelse 
  
  pal = [[_r],[_g],[_b]]  
  
  d = map->set_Plot_Params()
  d = map->set_data(landcover)
  d = map->set_Plot_Params(COLORS = pal, LEVELS=levs)
  
end