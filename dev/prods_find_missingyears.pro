pro prods_find_missingyears, dir 

    files = FILE_SEARCH(dir, '*.nc', count=filecnt)  
    
    files_bname = FILE_BASENAME(files)    
    files_trunk =  files_bname
    for i=0, filecnt-1 do files_trunk[i] = STRMID(files_bname[i], 0, STRLEN(files_bname[i])-6)
    
    uq_tr = files_trunk[UNIQ(files_trunk, SORT(files_trunk))]
    
    for i = 0, N_ELEMENTS(uq_tr) - 1 do begin
      ok = Where(StrMatch(files_bname, '*'+uq_tr[i]+'*'), ny)
      if ny ne 13 then begin
        stat = Where(StrMatch(files_bname[ok[0]], '*static*'), ns)
        if ns ne 1 then print, files[ok[0]]
      endif
    endfor
  
end