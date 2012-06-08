function w_test_wpp_file_directory, RESET=reset
  
  rootd = '/home/mowglie/WPP_TEST_PACK/'
  
  if ~ FILE_TEST(rootd + 'pp_files') then message, 'Directory not valid'
  if ~ FILE_TEST(rootd + 'namelists') then message, 'Directory not valid'
  
  if KEYWORD_SET(RESET) then begin
    FILE_DELETE, rootd + 'out_idl', /RECURSIVE
    FILE_DELETE, rootd + 'products', /RECURSIVE
  endif
  
  return, rootd

end

pro w_test_wpp_make_prods, RESET=reset

  rootd = w_test_wpp_file_directory(RESET=reset)
  
  FILE_MKDIR, rootd + 'products'
  FILE_MKDIR, rootd + 'out_idl'
  
  logt0 = SYSTIME(/SECONDS)
  
;  wpp = OBJ_NEW('w_WPP', NAMELIST=rootd + 'namelists/namelist_d01.wpp')
;  wpp->process, 2008, /NO_PROMPT_MISSING
;  wpp->process, 2009
;  undefine, wpp
;  wpp = OBJ_NEW('w_WPP', NAMELIST=rootd + 'namelists/namelist_d02.wpp')
;  wpp->process, 2008, /NO_PROMPT_MISSING
;  wpp->process, 2009
;  undefine, wpp
  wpp = OBJ_NEW('w_WPP', NAMELIST=rootd + 'namelists/namelist_d08.wpp')
  wpp->process, 2008, /NO_PROMPT_MISSING
  wpp->process, 2009
  undefine, wpp  
  
  delta =  LONG(SYSTIME(/SECONDS) - logt0)
  deltam =  delta / 60L
  deltas =  delta-(deltaM*60L)
  print, ' Test products done. Needed: ' + str_equiv(deltaM) + ' minutes, ' + str_equiv(deltaS) + ' seconds.'
  
end

pro w_test_wpp, RESET=reset

  rootd = w_test_wpp_file_directory(RESET=reset)
  
  if ~ FILE_TEST(rootd + 'products') then w_test_wpp_make_prods
  
end