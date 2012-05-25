pro w_pr_html_index_downfile, sfile, file_list, www_path, dir, si, filecnt, RES=res, TRES=tres

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(RES) eq 0 then res = ['d30km','d10km','d02km']
  if N_ELEMENTS(TRES) eq 0 then tres = ['static','h','d','m','y']
  
  si = 0LL  
  for i=0, N_ELEMENTS(res)-1 do begin
    pok = where(StrMatch(file_list, '*'+res[i]+'/*'), cnt)
    if cnt eq 0 then continue
    _file_list = file_list[pok]
    for j=0, N_ELEMENTS(tres)-1 do begin
      pok = where(StrMatch(_file_list, '*/'+tres[j]+'/*'), cnt)
      if cnt eq 0 then continue
      __file_list = _file_list[pok]
      if N_ELEMENTS(my_file_list) eq 0 then my_file_list = __file_list else my_file_list = [my_file_list, __file_list]
      for l=0, N_ELEMENTS(__file_list)-1 do si += (FILE_INFO(dir+__file_list[l])).size
    endfor
  endfor
  filecnt = N_ELEMENTS(my_file_list)
  si = Number_Formatter(si/1000000000d) + ' Gb'
  
  OPENW, lu, sfile, /GET_LUN
    
  printf, lu, '#!/bin/csh
  printf, lu, '#################################################################
  printf, lu, '# Csh Script to retrieve '+str_equiv(filecnt)+' online Data files of WRF-WET,
  printf, lu, '# total '+si+'. This script uses wget to download data.
  printf, lu, '#
  printf, lu, '# Highlight this script by Select All, Copy and Paste it into a file;
  printf, lu, '# make the file executable and run it on command line.
  printf, lu, '# 
  printf, lu, '# You need pass in your email AND password as parameters   
  printf, lu, '# to execute this script.
  printf, lu, '#
  printf, lu, '# Contact fabien.maussion@tu-berlin.de for further assistance.
  printf, lu, '#################################################################   
  printf, lu, '' 
  printf, lu, 'set username = $1' 
  printf, lu, 'set pswd = $2' 
  printf, lu, '' 
  printf, lu, 'if(x$pswd == x) then' 
  printf, lu, 'echo' 
  printf, lu, 'echo Usage: $0 YourUsername YourPassword' 
  printf, lu, 'echo' 
  printf, lu, 'exit 1' 
  printf, lu, 'endif' 
  printf, lu, 'if(x$username == x) then' 
  printf, lu, 'echo' 
  printf, lu, 'echo Usage: $0 YourUsername YourPassword' 
  printf, lu, 'echo' 
  printf, lu, 'exit 1' 
  printf, lu, 'endif' 
  printf, lu, ''   
  printf, lu, "set v = `wget -V |grep 'GNU Wget ' | cut -d ' ' -f 3`"
  printf, lu, "set a = `echo $v | cut -d '.' -f 1`"
  printf, lu, "set b = `echo $v | cut -d '.' -f 2`"
  printf, lu, 'if(100 * $a + $b > 109) then'
  printf, lu, " set opt = 'wget --no-check-certificate'"
  printf, lu, 'else'
  printf, lu, " set opt = 'wget'"
  printf, lu, 'endif'
  printf, lu, 'set optpss = "--http-user=$username --http-password=$pswd"'
  printf, lu, 'set opt = "$opt $optpss --force-directories --no-host-directories --cut-dirs=1 ' + www_path + '"'
  printf, lu, 'set filelist = ( \'
  
  ; Files  
  for l=0, N_ELEMENTS(my_file_list)-1 do printf, lu, my_file_list[l] + ' \'
  
  printf, lu, ')'
  printf, lu, 'while($#filelist > 0)'
  printf, lu, 'set syscmd = "$opt$filelist[1]"'
  printf, lu, 'echo "$syscmd ..."'
  printf, lu, '$syscmd'
  printf, lu, 'shift filelist'
  printf, lu, 'end'
  
  printf, lu, 'exit 0'
  
  close, lu ; close file
  free_lun, lu
  
end

pro w_pr_html_index, FILE=file, PR_ROOT_DIR=pr_root_dir

  ; Set up environnement and Error handling
  @WAVE.inc
  COMPILE_OPT IDL2
  
  if N_ELEMENTS(FILE) eq 0 then file = Dialog_Pickfile(/Write, File='pr_index.html', $
        TITLE='Please select where to create the index file')
  if file eq '' then return
  if N_ELEMENTS(PR_ROOT_DIR) eq 0 then pr_root_dir = Dialog_Pickfile(TITLE='Please select the WWW product directory', /DIRECTORY, /MUST_EXIST)
  if pr_root_dir eq '' then return
  if ~FILE_TEST(pr_root_dir, /DIRECTORY) then message, '$PR_ROOT_DIR not ok'  
  
  ; Parse dir
  dir = utils_clean_path(pr_root_dir, /MARK_DIRECTORY)
  file_list = FILE_SEARCH(dir, '*.nc', count=filecnt)
  if filecnt eq 0 then message, 'No files in the directory?'
  www_path = 'http://www.klima.tu-berlin.de/forschung/WET/products/'
  file_list = utils_replace_string(file_list, dir, '')   
  
  ; Create shell scripts    
  sfile_all = FILE_DIRNAME(FILE)+ '/down_all.csh'
  w_pr_html_index_downfile, sfile_all, file_list, www_path, dir, si, filecnt
  res='d30km'
  sfile_30 = FILE_DIRNAME(FILE)+ '/down_'+res+'.csh'
  w_pr_html_index_downfile, sfile_30, file_list, www_path, dir, si_30, n30, RES=res
  res='d10km'
  sfile_10 = FILE_DIRNAME(FILE)+ '/down_'+res+'.csh'
  w_pr_html_index_downfile, sfile_10, file_list, www_path, dir, si_10, n10, RES=res
  
  ; Copy readme file
  FILE_COPY, WAVE_resource_dir + '/post/readme_rar.txt',  FILE_DIRNAME(FILE)+ '/readme_rar.txt', /OVERWRITE
  
  ; Create html file
  OPENW, lu, file, /GET_LUN
  
  ; Header
  printf, lu, '<html>'
  printf, lu, ''
  printf, lu, '<head>'
  printf, lu, '<title>Fachgebiet Klimatologie - Institut f&uuml;r &Ouml;kologie - Technische Universit&auml;t Berlin</title>'
  printf, lu, '<link rev=made href="mailto:fabien.maussion@tu-berlin.de">'
  printf, lu, '</head>
  printf, lu, ''
  printf, lu, '<body>'
  
  ;Title
  printf, lu, '<h1>TU Berlin - Regional Atmospheric Reanalysis Tibet</h1>'
  printf, lu, 'WRF Regional Atmospheric Reanalysis data products, Tibet and Central Asia<br>'
  printf, lu, 'Version: 1.0<br>
  printf, lu, 'User guide: <a href="'+ utils_replace_string(www_path,'products/', '') + 'readme_rar.txt">readme_rar.txt</a><br>'
  printf, lu, 'Copyright: Fachgebiet Klimatologie - Institut f&uuml;r &Ouml;kologie - Technische Universit&auml;t Berlin<br>'
  printf, lu, 'This webpage is confidential. Please do not share this link to others.<br>'
  printf, lu, 'Last update: ' + TIME_to_STR(QMS_TIME(), MASK='YYYY.MM.DD') + '<br>'
  
  ;Contact
  printf, lu, ''
  printf, lu, '<h4>Contact</h3>'
  printf, lu, '<td><a href="mailto:fabien.maussion@tu-berlin.de?subject=WRF-Products">F. Maussion</a>, TU Berlin</td>'
  
  ;Data
  
  ; Download scripts
  printf, lu, '<h3><pre>  </pre>Download scripts (uses wGet)</h3>'
  sf = FILE_BASENAME(sfile_all)
  printf, lu, '<a href="'+ utils_replace_string(www_path,'products/', '') + sf + '">'+sf+'</a> ('+str_equiv(filecnt)+' files, '+si+')<br>'
  sf = FILE_BASENAME(sfile_30)
  printf, lu, '<a href="'+ utils_replace_string(www_path,'products/', '') + sf + '">'+sf+'</a> ('+str_equiv(n30)+' files, '+si_30+')<br>'
  sf = FILE_BASENAME(sfile_10)
  printf, lu, '<a href="'+ utils_replace_string(www_path,'products/', '') + sf + '">'+sf+'</a> ('+str_equiv(n10)+' files, '+si_10+')<br>'
  
  ; Single links
  printf, lu, '<h3><pre>  </pre>Direct links to the single files</h3>'
  printf, lu, 'Firefox users could use for example <a href="http://flashgot.net/">FlashGot</a> to download all files at once.<br>'
  res = ['d30km','d10km','d02km']
  for i=0, N_ELEMENTS(res)-1 do begin
    pok = where(StrMatch(file_list, '*'+res[i]+'/*'), cnt)
    if cnt eq 0 then continue
    _file_list = file_list[pok]
    printf, lu, '<h3>'+res[i]+'</h3>'
    tres = ['static','h','d','m','y']
    ltres = ['static','hourly','daily','monthly','yearly']
    for j=0, N_ELEMENTS(tres)-1 do begin
      pok = where(StrMatch(_file_list, '*/'+tres[j]+'/*'), cnt)
      if cnt eq 0 then continue
      __file_list = _file_list[pok]
      printf, lu, '<h4>'+res[i]+'/'+ltres[j]+'</h4>'
      for l=0, N_ELEMENTS(__file_list)-1 do begin
        f = __file_list[l]
        s = FILE_INFO(dir + f)
        s = Number_Formatter(s.SIZE/1000000.) + ' Mb'
        printf, lu, '<a href="'+ www_path + f + '">' + FILE_BASENAME(f) + '</a> ('+s+')<br>'
      endfor
    endfor
  endfor
  
  ; End
  printf, lu, ''
  printf, lu, '<br>'
  printf, lu, '<br>'
  printf, lu, 'Copyright: Fachgebiet Klimatologie - Institut f&uuml;r &Ouml;kologie - Technische Universit&auml;t Berlin'
  printf, lu, ''
  printf, lu, '</body>'
  printf, lu, '</html>'
  
  
  close, lu ; close file  
  free_lun, lu
  
end