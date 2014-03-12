function prods_find_missingyears, dir 

    files = FILE_SEARCH(dir, '*.nc', count=filecnt)  
    
    files_bname = FILE_BASENAME(files)    
    files_trunk =  files_bname
    for i=0L, filecnt-1 do files_trunk[i] = STRMID(files_bname[i], 0, STRLEN(files_bname[i])-6)
    
    uq_tr = files_trunk[UNIQ(files_trunk, SORT(files_trunk))]
    
    out = []
    
    for i = 0L, N_ELEMENTS(uq_tr) - 1 do begin
      ok = Where(StrMatch(files_bname, '*'+uq_tr[i]+'*'), ny)
      if ny ne 13 then begin
        stat = Where(StrMatch(files_bname[ok[0]], '*static*'), ns)
        if ns ne 1 then begin
          print, w_str(ny, FORMAT='(I2)') + ' ', files[ok[0]]
          out = [out, files[ok]]
        endif  
      endif
    endfor
    
    return, out
  
end

function prods_find_missing10, dir 

    files = FILE_SEARCH(dir, '*.nc', count=filecnt)  
    
    files_bname = FILE_BASENAME(files)    
    
    out = []
    p30 = Where(StrMatch(files_bname, '*tibet_d30km*'), n30)
    for i = 0L, n30 - 1 do begin
      f10 = utils_replace_string(files_bname[p30[i]], 'tibet_d30km', 'tibet_d10km')
      stat = Where(files_bname eq f10, ns)
      if ns ne 1 then begin
          print, f10
          out = [out, f10]
        endif  
    endfor
      
    return, out
  
end

function prods_find_missing30, dir 

    files = FILE_SEARCH(dir, '*.nc', count=filecnt)  
    
    files_bname = FILE_BASENAME(files)    
    
    out = []
    p30 = Where(StrMatch(files_bname, '*tibet_d10km*'), n30)
    for i = 0L, n30 - 1 do begin
      f10 = utils_replace_string(files_bname[p30[i]], 'tibet_d10km', 'tibet_d30km')
      stat = Where(files_bname eq f10, ns)
      if ns ne 1 then begin
          print, f10
          out = [out, f10]
        endif  
    endfor
      
    return, out
  
end