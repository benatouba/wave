;+
;
;   This command is similar to IDL's loadct or Coyote's cgLoadCT and gives
;   access to a large number of new colour tables, mostly taken from the 
;   NCL colormaps library 
;   (http://www.ncl.ucar.edu/Document/Graphics/color_table_gallery.shtml).
;   You can visualise all available tables (including the ones you added)
;   in the $WAVE/res/colormaps directory.
;   
;   The most interesting feature of this tool is its flexibility, allowing 
;   you to add or define virtualy ANY colortable you could desire. For 
;   example, you could visit the cpt-city website to get access to thousands
;   of colortables (http://soliton.vm.bytemark.co.uk/pub/cpt-city/index.html),
;   download one of them (using the *.c3g format) and add it to the WAVE 
;   res/colortables directory directory.
;   
;   Moreover, it allows you to create very easily your own color table by 
;   creating a file called xxxx.rgb (where xxxx is the color table name)
;   and moving it to the WAVE res/colortables directory.
;   For example, here's a sample color table with 8 colors::
;      ncolors=8
;      # r   g   b
;      160 32  240
;      0   0   180
;      60  100 230
;      120 155 242
;      176 224 230
;      46  139 87
;      100 225 0
;      210 255 47
;   
;   Don't forget to update the table list when you add your own table to the WAVE!
;
; :Categories:
;    Graphics
;    
; :History:
;     Written by FaM, 2012.
;
;-

;+
; :Private:
; 
; :Description:
;    Parses a RGB ascii file.
;
; :Params:
;    file: in, required
;          path to the file
;    pal: out
;         the palette
;    n: out
;       the number of colors in the palette
;  
;  :Returns:
;    1 if successed, 0 if not
;-
function w_LoadCT_parse_rgb, file, pal, n

  @WAVE.inc
  compile_opt idl2
  
  catch, theError
  if theError ne 0 then begin
    catch, /cancel
    if N_ELEMENTS(lun) ne 0 then FREE_LUN, lun
    print, 'Could not read file: ' + FILE_BASENAME(file)
    return, 0
  endif
  
  r = BYTARR(256)
  g = BYTARR(256)
  b = BYTARR(256)
  
  OPENR, lun, file, /GET_LUN
  line = ''
  n=0LL
  while ~eof(lun) do begin
    readf, lun, line
    line = strtrim(line,2)
    line = STRJOIN(STRTOK(line, STRING(9B), /REGEX, /EXTRACT), ' ')
    l = STRSPLIT(line, ' ', /EXTRACT)
    if N_ELEMENTS(l) lt 3 then continue
    if ~arg_okay(l[0:2], /NUMERIC) then continue
    if n ge 256 then continue
    r[n]=l[0]
    g[n]=l[1]
    b[n]=l[2]
    n+=1
  endwhile
  CLOSE, lun
  FREE_LUN, lun
  
  if n eq 0 then Message, 'no'

  pal = [TRANSPOSE(r), TRANSPOSE(g), TRANSPOSE(b)]
  
  return, 1

end

;+
; :Private:
; 
; :Description:
;    Parses a C3G ascii file.
;
; :Params:
;    file: in, required
;          path to the file
;    pal: out
;         the palette
;    n: out
;       the number of colors in the palette
;  
;  :Returns:
;    1 if successed, 0 if not
;-
function w_LoadCT_parse_c3g, file, pal, n

  @WAVE.inc
  compile_opt idl2
  
;  catch, theError
;  if theError ne 0 then begin
;    catch, /cancel
;    if N_ELEMENTS(lun) ne 0 then FREE_LUN, lun
;    print, 'Could not read file: ' + FILE_BASENAME(file)
;    return, 0
;  endif
  
  r = BYTARR(256)
  g = BYTARR(256)
  b = BYTARR(256)
  
  OPENR, lun, file, /GET_LUN
  line = ''
  n=0LL
  while ~eof(lun) do begin
    readf, lun, line
    line = strtrim(line,2)
    if N_ELEMENTS(BYTE(line)) lt 4 then continue
    if TOTAL((byte(line))[0:3] - byte('rgb(')) ne 0 then continue 
    l = STRSPLIT(line, '(', /EXTRACT)
    l = (STRSPLIT(l[1], ')', /EXTRACT))[0]
    l = STRSPLIT(l, ',', /EXTRACT)
    if ~arg_okay(l[0:2], /NUMERIC) then continue
    if n ge 256 then continue
    r[n]=l[0]
    g[n]=l[1]
    b[n]=l[2]
    n+=1
  endwhile
  CLOSE, lun
  FREE_LUN, lun
  
  if n eq 0 then Message, 'no'

  pal = [TRANSPOSE(r), TRANSPOSE(g), TRANSPOSE(b)]
  
  return, 1

end

;+
; :Private:
; 
; :Description:
;    Returns the parsed color palettes and updates them if necessary
;
; :Keywords:
;    UPDATE: in, option
;            if the list must be updated before returned
;            
;  :Returns:
;    An array of tables structures
;-
function w_LoadCT_getTables, UPDATE=UPDATE

  @WAVE.inc
  compile_opt idl2
  
  WAVE_root, root
  dir = root+'/res/colortables/'
  savf = dir + 'cmaps.sav'
  if ~ FILE_TEST(savf) then UPDATE=1
  if N_ELEMENTS(UPDATE) eq 0 then UPDATE=0
  
  if UPDATE then begin
  
    files = FILE_SEARCH(dir, '*.{rgb,ncmap,gp,c3g}', /EXPAND_ENVIRONMENT, COUNT=nfiles)
    files = files[sort(files)]
    names = STRARR(nfiles)
    suff = STRARR(nfiles)
    
    for i=0, nfiles-1 do begin
      l = STRSPLIT(FILE_BASENAME(files[i]), '.', /EXTRACT)
      names[i] = l[0]
      suff[i] = l[1]
    endfor
    
    suff = suff[UNIQ(names)]
    files = files[UNIQ(names)]
    names = names[UNIQ(names)]
    nfiles = N_ELEMENTS(files)
    id = 0L
    
    for i=0, nfiles-1 do begin
      if str_equiv(suff[i]) eq 'C3G' then ok = w_LoadCT_parse_c3g(files[i], pal, nc) $
       else ok = w_LoadCT_parse_rgb(files[i], pal, nc)
      if ~ ok then continue
      t = {name:names[i], id:id, pal:pal, nc:nc}
      id += 1
      
      if N_ELEMENTS(tables) eq 0 then tables = t else tables = [tables, t]
    endfor
    nt = N_ELEMENTS(tables)
    
    visdir = root+'/res/colormaps/'
    print, 'Found ' + str_equiv(nt) +' colortables: '
    print, ' ID  Name                     Ncolors'
    for i=0, nt-1 do begin
      t = tables[i]
      ns = '                                                  '
      STRPUT, ns, str_equiv(t.id) , 1
      STRPUT, ns, t.name, 5
      STRPUT, ns, str_equiv(t.nc) , 30      
      print, ns
      
      pngf = visdir + t.name + '.png'     
      cgDisplay, 1000, 300, /FREE, /PIXMAP
      xwin = !D.WINDOW      
      dum = BYTARR(100) + 1
      r = reform(t.pal[0,0:t.nc-1]) # dum
      g = reform(t.pal[1,0:t.nc-1]) # dum
      b = reform(t.pal[2,0:t.nc-1]) # dum
      img = [[[r]],[[g]],[[b]]]            
      cgImage, img, POSITION=[0.05,0.15,0.95,0.7], /AXIS, AXKEYWORDS={YTICKS:1, YTICKNAME:[' ', ' ']} 
      title = t.name
      cgText, 0.05, 0.8, title, /NORMAL,  COLOR='black', charsize=5, FONT=-1, CHARTHICK=2
      title = 'ncolors = ' + str_equiv(t.nc)   
      cgText, 0.95, 0.8, title, /NORMAL,  COLOR='black', charsize=3, FONT=-1, CHARTHICK=1, ALIGNMENT=1.
      newimg = tvrd(TRUE=1)
      if FILE_TEST(pngf) then begin
        READ_PNG, pngf, oldimg
        if total(ABS(oldimg-newimg)) ne 0 then begin
          print, 'replacing: ' + pngf
          write_png, pngf, newimg
        endif
      endif else write_png, pngf, newimg  
      if xwin ne -1 then wdelete, xwin      
    endfor
    save, tables, FILENAME=savf
  endif else begin
    restore, FILENAME=savf
  endelse
  
  return, tables
  
end


;+
; :Description:
;   This command is similar to IDL's loadct or Coyote's cgLoadCT and gives
;   access to a large number of new colour tables.
;
; :Params:
;    table: in, optional, default=IDL 0
;           the table to load. Either the table name or the table ID. Since the ids
;           are likely to change with time, it is is strongly recommended to use the 
;           table name in your code.
;           See the $WAVE/res/colormaps directory for the list of available colortables      
;
; :Keywords:
;    UPDATE: in, optional, type=boolean, default=0
;            set this keyword to update the table list before loading your table
;    GET_RGB_TABLE: out, optional, type=array
;                   set to a named variable to get the RGB palette. If set, the table
;                   will NOT be loaded in IDL
;    ROW: in, optional, type=boolean, default=0
;       Set this keyword to indicate you are getting the RGB_TABLE vectors
;       for use in the IDL's object graphics routines. Whereas TVLCT expects color 
;       tables to be 256x3 (column vectors), the object graphics routines expect them 
;       to be 3x256 (row vectors). Setting this keyword will transpose the vectors 
;       before they are returned.
;    GET_NAME: out, optional, type=string
;              set to a named variable to get the table name              
;    GET_NCOLORS: out, optional, type=long
;                 set to a named variable to get the table number of colors. 
;                 This is very usefull (if not very very important) if yo plan
;                 to use `w_gr_datalevels` after a call to w_Loadct          
;    REVERSE: in, optional, type=boolean, default=0
;             If this keyword is set, the color table vectors are reversed.
;    TALK: in, optional, type=boolean, default=0
;          IDL used to talk when it loaded a colortable. Set this keyword
;          if you want w_LoadCT do talk, too
;    WINDOW: in, optional, type=boolean, default=0
;            Set this keyword to add the command to an cgWindow application.
;    WINID: in, optional, type=integer                 
;           The window index number of an cgWindow to receive the color vectors.
;           If this parameter is absent, the color table vectors are sent to the
;           current cgWindow.
;    ADDCMD: in, optional, type=boolean, default=0
;             Set this keyword to add the command to the resizeable graphics window cgWindow.
;-
pro w_LoadCT, table, $
    UPDATE=update, $
    GET_RGB_TABLE=get_rgb_table, $
    ROW=row, $
    GET_NAME=get_name, $
    GET_NCOLORS=get_ncolors, $
    REVERSE=reverse, $
    TALK=talk, $
    WINID=winID, $
    WINDOW=window, $
    ADDCMD=addcmd
    
  @WAVE.inc
  compile_opt idl2  
  
  ; Are you adding this command to an cgWindow command list?
  ; Should this be added to a resizeable graphics window?
  IF Keyword_Set(addcmd) AND ((!D.Flags AND 256) NE 0) THEN BEGIN
    windowIDs = cgQuery(COUNT=wincnt)
    IF N_Elements(winid) NE 0 THEN BEGIN
      IF (wincnt GT 0) THEN BEGIN
        index = Where(windowIDs EQ winID)
        IF index[0] NE -1 THEN cgSet, winid
      ENDIF
    ENDIF
    IF wincnt EQ 0 THEN cgWindow
    cgWindow, 'w_LoadCT', table, $
      UPDATE=update, $
      GET_RGB_TABLE=get_rgb_table, $
      ROW=row, $
      GET_NAME=get_name, $
      GET_NCOLORS=get_ncolors, $
      REVERSE=reverse, $
      TALK=talk, $
      WINID=winID, $
      ADDCMD=1
    return
  ENDIF
    
  reverse = KEYWORD_SET(reverse)
  
  ;Do my stuff
  tables = w_LoadCT_getTables(UPDATE=update)
  nt = N_ELEMENTS(tables)  
  
  if N_ELEMENTS(table) eq 0 then begin
    cgLoadCT, 0
    return
  endif
  
  if arg_okay(table, TYPE=IDL_STRING, /SCALAR) then begin
    p = WHERE(str_equiv(tables.Name) eq str_equiv(table), cnt)
    if cnt ne 0 then out_id = p[0] else MESSAGE, 'Table not found: ' + table
  endif else if arg_okay(table, /INTEGER, /SCALAR) then begin
    if table lt 0 or table ge nt then MESSAGE, 'Table not found: ' + str_equiv(table)
    out_id = table    
  endif else MESSAGE, WAVE_Std_Message('table', /ARG)
  
  t = tables[out_id]
  GET_NCOLORS = t.nc
  GET_NAME=t.name
  r = reform(t.pal[0,*])
  g = reform(t.pal[1,*])
  b = reform(t.pal[2,*])    
  
  if KEYWORD_SET(TALK) then print, '% W_LoadCT: Loading table ' + GET_NAME + ', ncolors: ' + str_equiv(GET_NCOLORS)
  
  ; Need to reverse the colors?
  IF reverse THEN BEGIN
     r = Reverse(r)
     g = Reverse(g)
     b = Reverse(b)
  ENDIF

  ; Load a color_table, if needed. Otherwise, load color vectors.
  IF Arg_Present(get_rgb_table) THEN BEGIN
    get_rgb_table = [[r], [g], [b]]
    IF Keyword_Set(row) THEN get_rgb_table = Transpose(get_rgb_table)
  ENDIF ELSE BEGIN
    TVLCT, r, g, b
  ENDELSE
  
  ; If the WINDOW keyword is set, send these colors to a cgWindow object.
  IF Keyword_Set(window) THEN BEGIN
  
    ; Does a window object exist somewhere?
    DefSysV, '!FSC_WINDOW_LIST', EXISTS=exists
    IF exists THEN BEGIN
      theList = !FSC_WINDOW_LIST
      IF Obj_Valid(theList) THEN BEGIN
        structs = theList -> Get_Item(/ALL, /DEREFERENCE)
        IF Size(structs, /TNAME) EQ 'POINTER' THEN RETURN
        IF N_Elements(winID) EQ 0 THEN BEGIN
          winID = N_Elements(structs) - 1
        ENDIF ELSE BEGIN
          index = Where(structs.wid[*] EQ winID, count)
          IF count GT 0 THEN winID = index[0] ELSE BEGIN
            Message, 'Cannot find an cgWindow with window index ' + StrTrim(winID, 2) + '.'
          ENDELSE
        ENDELSE
        thisWindowStruct = structs[winID]
        IF Obj_Valid(thisWindowStruct.windowObj) THEN BEGIN
        
          ; Get the RGB vectors from the current color table to load.
          ; This is necessary because the cgWindow object does not use
          ; the concept of loading its colors at the BOTTOM. So, if the
          ; BOTTOM keyword is used here, the cgWindow colors would be
          ; incorrect if we used the r,g,b vectors directly.
          TVLCT, rr, gg, bb, /GET
          thisWindowStruct.windowObj -> LoadColors, rr, gg, bb
        ENDIF
        RETURN
      ENDIF
    ENDIF
  ENDIF
    
  
end