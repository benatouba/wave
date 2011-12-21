pro w_ts_Data__Define

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  struct = {w_ts_Data        , $
           name:           '', $ ; The name of the variable
           description:    '', $ ; A short description of the variable
           nt:             0L, $ ; Number of times
           time:    PTR_NEW(), $ ; Array of QMS/ABS_DATE of nt elements
           data:    PTR_NEW(), $ ; Data array of nt elements
           unit:           '', $ ; The variable unit
           type:          0L , $ ; The variable type (IDL)
           regular:     FALSE, $ ; If the TS is regular
           step : {TIME_STEP}, $ ; Probale Timestep
           agg_method: 'NONE', $ ; Aggregation method. See `TS_AGG`
           valid:     'POINT', $ ; If it is aggregated
           missing: PTR_NEW()  $ ; Missing data values
           }

end