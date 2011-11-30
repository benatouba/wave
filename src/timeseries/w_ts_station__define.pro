pro w_ts_Station__Define

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  struct = {w_ts_Station         , $
           name:            '', $ ; The name of the station
           id:              '', $ ; Station ID
           description:     '', $ ; A short description of the station 
           elevation :      0D, $ ; altitude in m
           loc_x :          0D, $ ; X location in SRC
           loc_y :          0D, $ ; Y location in SRC
           src:      PTR_NEW(), $ ; Location information ({TNT_DATUM}, {TNT_PROJ})
           op_time:  [0LL,0LL], $ ; Operating period [QMS,QMS]
           nvars:           0L, $ ; Number of variables
           varnames: PTR_NEW(), $ ; Variable names
           vars:     PTR_NEW()  $ ; Variables (W_WAR)
           }    

end