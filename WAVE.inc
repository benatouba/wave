;***********************************************************************
;                                                                      *
; Author(s)   :  F. Maussion                                           *
; Name        :  WAVE.inc                                              *
; Version     :  WAVE 0.0                                              *
; Language    :  IDL 7.0 and higher                                    *
; Date        :  13-Jul-2010                                           *
; Last Update :  13-Jul-2010  FaM                                      *
;                                                                      *
;                                                                      *
; IDL include file for WAVE library users COMMON block definitions.    *
;                                                                      *
; The WAVE library should not be based on common blocks.               * 
; They should be used parsimoniously.                                  * 
;                                                                      *
;***********************************************************************

  @TNT.inc ; The WAVE library is based on the TNT library
  
  ;**************************
  ; Programming environment *
  ;**************************

  common WAVE_cmn, WAVE_root, WAVE_resource_dir, D_QMS, H_QMS, M_QMS, S_QMS  