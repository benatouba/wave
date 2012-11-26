;+
; :Description:
;    Makes a copy of the selected products into a new directory.
;    
; :Keywords:
;    INPUT_DIR: in, required, type=string
;               Path to the original products directory
;    OUTPUT_DIR: in, required, type=string
;                Path to the new products directory
;    DRES: in, required, type=string
;          the resolution, e.g. DRES=['d30km','d10km']
;    VTYPE: in, required, type=string
;           the variable type, e.g. VTYPE=['2d]
;    VARIABLES: in, required, type=string
;               the variable, e.g. VARIABLES=['hgt', 'landmask', 'prcp', 'prcp_fr', 'psfc', 'q2', 'swdown', 't2', 'u10', 'v10']
;    AGG_STEPS: in, required, type=string
;               the aggregation, e.g. AGG_STEPS=['static','d','m','y']
;    YEARS: in, optional, type=long
;           the years, e.g. YEARS=[2009,2010,2011]
;    FORCE: in, optional, type=boolean
;           default is not to overwrite existing files in the output directory.
;           Set this keyword to force overwriting
;
; :History:
;     Written by FaM, 2012.
;
;-
pro w_pr_selector, INPUT_DIR=input_dir, $
    OUTPUT_DIR=output_dir, $
    DRES=dres, $
    VTYPE=vtype, $
    VARIABLES=variables, $
    YEARS=years, $
    AGG_STEPS=agg_steps, $
    FORCE=force
    
  if N_ELEMENTS(INPUT_DIR) eq 0 then input_dir = Dialog_Pickfile(TITLE='Please select the input product directory', /DIRECTORY, /MUST_EXIST)
  if input_dir eq '' then return
  if ~FILE_TEST(input_dir, /DIRECTORY) then message, '$INPUT_DIR not ok'
  if N_ELEMENTS(OUTPUT_DIR) eq 0 then output_dir = Dialog_Pickfile(TITLE='Please select the output product directory', /DIRECTORY, /MUST_EXIST)
  if output_dir eq '' then return
  if ~FILE_TEST(output_dir, /DIRECTORY) then message, '$OUTPUT_DIR not ok'
  
  force = KEYWORD_SET(FORCE)
  
  ; Parse dir
  dir = utils_clean_path(input_dir, /MARK_DIRECTORY)
  file_list = FILE_SEARCH(dir, '*.nc', count=filecnt)
  if filecnt eq 0 then message, 'No files in the directory?'
  
  if N_ELEMENTS(YEARS) ne 0 then message, '$YEARS keyword not implemented yet, sorry'
    
  si = 0LL
  nf = 0LL
  for i=0, N_ELEMENTS(dres)-1 do begin
    pok = where(StrMatch(file_list, '*/'+dres[i]+'/*'), cnt)
    if cnt eq 0 then continue
    _file_list = file_list[pok]
    for j=0, N_ELEMENTS(agg_steps)-1 do begin
      pok = where(StrMatch(_file_list, '*/'+agg_steps[j]+'/*'), cnt)
      if cnt eq 0 then continue
      __file_list = _file_list[pok]
      for q=0, N_ELEMENTS(vtype)-1 do begin
        pok = where(StrMatch(__file_list, '*/'+vtype[q]+'/*'), cnt)
        if cnt eq 0 then continue
        ___file_list = __file_list[pok]
        for k=0, N_ELEMENTS(variables)-1 do begin
          pok = where(StrMatch(___file_list, '*_'+variables[k]+'_2*'), cnt)
          if cnt eq 0 then begin ; try static case
            pok = where(StrMatch(___file_list, '*static_'+variables[k]+'.*'), cnt)
          endif
          if cnt eq 0 then continue
          ____file_list = ___file_list[pok]
          for l=0, N_ELEMENTS(____file_list)-1 do begin
            f = ____file_list[l]
            arr_file = output_dir + PATH_SEP() + utils_replace_string(f, dir, '')
            if FILE_TEST(arr_file) and ~ force then continue
            FILE_MKDIR, FILE_DIRNAME(arr_file)
            print, 'Copying file : ' + f
            print, ' to : ' + arr_file + ' ...'
            FILE_COPY, f, arr_file, FORCE=force
            si += (FILE_INFO(arr_file)).size
            nf += 1
          endfor
        endfor
      endfor
    endfor
  endfor
  si = cgNumber_Formatter(si/1000000000d) + ' Gb'
  print, 'Done. Number of files copied: ' + str_equiv(nf) + '. Total size copied: ' + si
    
end