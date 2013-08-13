function w_test_file_directory, RESET = reset

  common W_TEST_CMN, TEST_CMN_ROOT_DIR
  ; Put the WAVE test pack path here.
  
  if N_ELEMENTS(TEST_CMN_ROOT_DIR) eq 0  or KEYWORD_SET(RESET) then TEST_CMN_ROOT_DIR = '/home/mowglie/disk/Data/WAVE_TEST_PACK/'
  
  if  ~FILE_TEST(TEST_CMN_ROOT_DIR) then TEST_CMN_ROOT_DIR = DIALOG_PICKFILE(TITLE='Please indicate the test directory', /MUST_EXIST, /DIRECTORY)
  
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/WRF/') then TEST_CMN_ROOT_DIR = w_TEST_file_directory( /RESET)
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/MAPPING/') then TEST_CMN_ROOT_DIR = w_TEST_file_directory( /RESET)
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/MODIS/') then TEST_CMN_ROOT_DIR = w_TEST_file_directory( /RESET)
  if ~FILE_TEST(TEST_CMN_ROOT_DIR + '/TRMM/') then TEST_CMN_ROOT_DIR = w_TEST_file_directory( /RESET)
  
  return, TEST_CMN_ROOT_DIR
  
end