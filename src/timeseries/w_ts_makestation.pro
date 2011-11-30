function w_ts_MakeStation, variables, NAME=name, $ ; The name of the station
                                      ID=id, $ ; Station ID
                                      DESCRIPTION=description, $ ; A short description of the station 
                                      ELEVATION=elevation, $ ; altitude in m
                                      LOC_X=loc_x, $ ; X location in SRC
                                      LOC_Y=loc_y, $ ; Y location in SRC
                                      SRC=src ; Location information ({TNT_DATUM}, {TNT_PROJ})


  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
;  ON_ERROR, 2
 
  ; Check input
  if N_ELEMENTS(variables) eq 0 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~ arg_okay(variables, STRUCT={w_ts_Data}) then  MESSAGE, WAVE_Std_Message('variables', /ARG)
  
  ; Check input and set defaults
  if N_ELEMENTS(NAME) eq 0 then _name = 'Station' else _name = name
  if N_ELEMENTS(ID) eq 0 then _id = '' else _id = id
  if N_ELEMENTS(DESCRIPTION) eq 0 then _description = '' else _description = description
  if N_ELEMENTS(ELEVATION) eq 0 then _elevation = 0 else _elevation = elevation
  if N_ELEMENTS(LOC_X) eq 0 then _loc_x = 0 else _loc_x = loc_x
  if N_ELEMENTS(LOC_Y) eq 0 then _loc_y = 0 else _loc_y = loc_y
  if N_ELEMENTS(SRC) eq 0 then GIS_make_datum, ret, _src, NAME = 'WGS-84' else _src = src  
  
  ; Check variables  
  _nvars = N_ELEMENTS(variables)
  _varnames = STRARR(_nvars)
  for i=0, _nvars-1 do begin
    _var = variables[i]
    _varnames[i] = _var.name
    if N_ELEMENTS(_op_time) eq 0 then begin
      _op_time = utils_minmax(*_var.time)
    endif else begin
      _op_time[0] = min(*_var.time) < _op_time[0] 
      _op_time[1] = max(*_var.time) > _op_time[1]       
    endelse
  endfor

  out = {w_ts_Station}
  ;    name:            '', $ ; The name of the station
  ;    id:              '', $ ; Station ID
  ;    description:     '', $ ; A short description of the station
  ;    elevation :      0D, $ ; altitude in m
  ;    loc_x :          0D, $ ; X location in SRC
  ;    loc_y :          0D, $ ; Y location in SRC
  ;    src:      PTR_NEW(), $ ; Location information ({TNT_DATUM}, {TNT_PROJ})
  ;    op_time:  [0LL,0LL], $ ; Operating period [QMS,QMS]
  ;    nvars:           0L, $ ; Number of variables
  ;    varnames: PTR_NEW(), $ ; Variable names
  ;    vars:     PTR_NEW()  $ ; Variables (W_WAR)
  out.name = _name
  out.id = _id
  out.description = _description
  out.elevation = _elevation
  out.loc_x = _loc_x
  out.loc_y = _loc_y
  out.src = PTR_NEW(_src)
  out.op_time = _op_time
  out.nvars = _nvars
  out.varnames = PTR_NEW(_varnames)
  out.vars = PTR_NEW(variables)
  
  return, out
  
end