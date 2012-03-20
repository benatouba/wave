; docformat = 'rst'
;+
;
;This bundle of procedures is a tool set available to the WAVE user.
;
; :Author:
;       Fabien Maussion::
;           FG Klimatologie
;           TU Berlin
;  
; :Version:
;       WAVE V0.1
;       
; :History:
;     Last modification:  06-Apr-2011 FaM
;-

;+
; :Description:
;      This procedure simply takes xs and ys in form of an array to make a 2d grid.
;      
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    ax: in, required, type=array, default=none
;        The 1 dimensional array of x-coordinates (e.g. longitudes).
;    ay: in, required, type=array, default=none
;        The 1 dimensional array of y-coordinates (e.g. latitudes).
;    x: out, type=array, default=none
;       The 2 dimensional array of x-coordinates (e.g. longitudes).
;    y: out, type=array, default=none
;       The 2 dimensional array of y-coordinates (e.g. latitudes).
;       
; :Examples:
;        This is how the output may look like::
;              IDL> ax = [91.,92.,93.]
;              IDL> ay = [31.,32.]
;              IDL> utils_1d_to_2d, ax, ay, x, y
;              IDL> print, x
;                   91.000000       92.000000       93.000000
;                   91.000000       92.000000       93.000000
;              IDL> print, y
;                   31.000000       31.000000       31.000000
;                   32.000000       32.000000       32.000000
;
;
; :History:
;       Written by FaM, 2010
;       
;-
pro utils_1d_to_2d, ax, ay, x, y

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN
  ENDIF  

  if n_params() ne 4 then Message, WAVE_Std_Message(/NARG)
  
  if not arg_okay(ax, /NUMERIC) then Message, WAVE_Std_Message('ax', /NUMERIC)
  if not arg_okay(ay, /NUMERIC) then Message, WAVE_Std_Message('ay', /NUMERIC)
    
  if SIZE(ax, /N_DIMENSIONS) ne 1 then Message, WAVE_Std_Message('ax', DIMARRAY=1)
  if SIZE(ay, /N_DIMENSIONS) ne 1 then Message, WAVE_Std_Message('ay', DIMARRAY=1)
  
  nx = n_elements(ax)
  ny = n_elements(ay)
  
;  if nx lt 2 then Message, '$ax has not enough elements.'
;  if ny lt 2 then Message, '$ay has not enough elements.'
  
  y = (LONARR(nx) + 1) # ay ; "the georef-eq" 2-dimensional array
  x = ax # (LONARR(ny) + 1) ; "the georef-eq" 2-dimensional array
  
end

;+
; :Description:
;    Extract any bit from a number.
;
; :Params:
;    number: in, required
;            array of numbers
;    index: in, required
;           the index of the required byte (e.g. from 0 to 15 for integer, 0 to 8 for bytes, etc.)
;
; :Returns:
; 
;   An array of integers of the same dimension as number containing 0 or 1.
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_bit_extract, number, index

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  on_error, 2
  
  if ~ arg_okay(index, /SCALAR, /NUMERIC) then Message, WAVE_Std_Message('index', /ARG)  
  if ~ arg_okay(number, /NUMERIC) then Message, WAVE_Std_Message('number', /ARG)  
  
  dummy=fix(number/(2^index))
  
  return,dummy mod 2

end


;+
; :Description:
;    This function cleans a path string (removes multiple separators)
;
; :Params:
;    path: in, required, string
;          string or array of strings to clean
; 
; :Keywords:
;    MARK_DIRECTORY: in, optional
;                    Set this keyword to include a directory separator character at 
;                    the end of the returned directory name string.
;          
; 
; :Returns:
;          string or array of strings, clean
; 
; :Examples:
;    Very simple::
;    
;      IDL> path = '/home//fab/tmp/'
;      IDL> print, utils_clean_path(path)
;      /home/fab/tmp
;                
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_clean_path, path, MARK_DIRECTORY = mark_directory
  
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  on_error, 2
  
  if ~ arg_okay(path, TYPE=IDL_STRING) then Message, WAVE_Std_Message('path', /ARG)  
  
  n = N_ELEMENTS(path)
  out = path
  
  for i = 0, n-1 do begin
    split = STRSPLIT(path[i], PATH_SEP(), /EXTRACT, COUNT = cnt)
    out[i] = ''
    for j=0, cnt-1 do out[i] += PATH_SEP() + split[j]    
    if KEYWORD_SET(MARK_DIRECTORY) then out[i] += PATH_SEP()
  endfor
      
  return, out
  
end


;+
; :Description:
; 
;       Contract a vector or up to 25 vectors by removing specified elements.
;       
;       Copied from the ASTRON library!!! 
;       
;       If more than one element is to be removed, then HISTOGRAM is used
;       to generate a 'keep' subscripting vector.    To minimize the length of 
;       the subscripting vector, it is only computed between the minimum and 
;       maximum values of the index.   Therefore, the slowest case of REMOVE
;       is when both the first and last element are removed.
;       
; :Params:
;       index: in, required
;               scalar or vector giving the index number of elements to
;               be removed from vectors.  Duplicate entries in index are
;               ignored.    An error will occur if one attempts to remove
;               all the elements of a vector.
;
;
;       v1: in, out, required
;           Elements specifed by INDEX will be removed from v1.  
;           Upon return v1 will contain N fewer elements,
;           where N is the number of distinct values in INDEX.
;           ::
;               v2,v3,...v25 - additional vectors containing
;               the same number of elements as v1.  These will be
;               contracted in the same manner as v1.
;
; :Examples:
;    ::
;       (1) If INDEX = [2,4,6,4] and V = [1,3,4,3,2,5,7,3] then after the call
;
;               IDL> utils_array_remove,index,v      
;
;       V will contain the values [1,3,3,5,3]
;
;       (2) Suppose one has a wavelength vector W, and three associated flux
;       vectors F1, F2, and F3.    Remove all points where a quality vector,
;       EPS is negative
;
;               IDL> bad = where( EPS LT 0, Nbad)
;               IDL> if Nbad GT 0 then utils_array_remove, bad, w, f1, f2, f3
;
;
; :History:
;       Written W. Landsman        ST Systems Co.       April 28, 1988
;       Cleaned up code          W. Landsman            September, 1992
;       Major rewrite for improved speed   W. Landsman    April 2000
;       Accept up to 25 variables, use SCOPE_VARFETCH internally
;              W. Landsman   Feb 2010
;       Fix occasional integer overflow problem  V. Geers  Feb 2011
;       
;       
;-
pro utils_array_remove, index, v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, $
    v15, v16, v17, v18, v19, v20, v21, v22, v23, v24, v25
    
  On_error,2
  compile_opt idl2,strictarrsubs
  
  npar = N_params()
  nvar = npar-1
  if npar LT 2 then begin
    print,'Syntax - utils_array_remove, index, v1, [v2, v3, v4,..., v25]'
    return
  endif
  vv = 'v' + strtrim(indgen(nvar)+1, 2)
  
  
  npts = N_elements(v1)
  
  max_index = max(index, MIN = min_index)
  
  if ( min_index LT 0 ) || (max_index GT npts-1) then message, $
    'ERROR - Index vector is out of range'
    
  if ( max_index Eq min_index ) then begin
    Ngood = 0
    if npts EQ 1 then message, $
      'ERROR - Cannot delete all elements from a vector'
  endif else begin  ;Remove only 1 element?
  
  
    ;  Begin case where more than 1 element is to be removed.   Use HISTOGRAM
    ;  to determine then indices to keep
  
    nhist = max_index - min_index +1
    
    hist = histogram( index)      ;Find unique index values to remove
    keep = where( hist EQ 0, Ngood ) + min_index
    
    if ngood EQ 0 then begin
      if ( npts LE nhist ) then message, $
        'ERROR - Cannot delete all elements from a vector'
    endif
  endelse
  
  imin = min_index - 1
  imax = max_index + 1
  i0 = (min_index EQ 0) + 2*(max_index EQ npts-1)
  case i0 of
    3: begin
      for i=0, nvar-1 do  $
        (SCOPE_VARFETCH(vv[i],LEVEL=0)) = $
        (SCOPE_VARFETCH(vv[i],LEVEL=0))[keep]
      return
    end
    
    1:  ii = Ngood EQ 0 ? imax + lindgen(npts-imax) : $
      [keep, imax + lindgen(npts-imax) ]
    2:  ii = Ngood EQ 0 ? lindgen(imin+1)               :  $
      [lindgen(imin+1), keep ]
    0:   ii = Ngood EQ 0 ? [lindgen(imin+1), imax + lindgen(npts-imax) ]  : $
      [lindgen(imin+1), keep, imax + lindgen(npts-imax) ]
  endcase
  
  for i=0,nvar-1 do  $
    (SCOPE_VARFETCH(vv[i],LEVEL=0)) =    $
    (SCOPE_VARFETCH(vv[i],LEVEL=0))[ii]
    
  return
  
end

;+
; NAME:        
;       TAG_EXIST()
; PURPOSE:              
;       To test whether a tag name exists in a structure.
; EXPLANATION:               
;       Routine obtains a list of tagnames and tests whether the requested one
;       exists or not. The search is recursive so if any tag names in the 
;       structure are themselves structures the search drops down to that level.
;       (However, see the keyword TOP_LEVEL).
;               
; CALLING SEQUENCE: 
;       status = TAG_EXIST(str, tag, [ INDEX =, /TOP_LEVEL, /QUIET ] )
;    
; INPUT PARAMETERS:     
;       str  -  structure variable to search
;       tag  -  tag name to search for, scalar string
;
; OUTPUTS:
;       Function returns 1b if tag name exists or 0b if it does not.
;                              
; OPTIONAL INPUT KEYWORD:
;       /TOP_LEVEL = If set, then only the top level of the structure is
;                           searched.
;       /QUIET - if set, then do not print messages if invalid parameters given
;       /RECURSE - does nothing but kept for compatibility with the
;                  Solarsoft version for which recursion is not the default 
;        http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/struct/tag_exist.pro
; OPTIONAL OUTPUT KEYWORD:
;       INDEX = index of matching tag, scalar longward, -1 if tag name does
;               not exist
;
; EXAMPLE:
;       Determine if the tag 'THICK' is in the !P system variable
;       
;       IDL> print,tag_exist(!P,'THICK')
;
; PROCEDURE CALLS:
;       None.
;
; MODIFICATION HISTORY:     : 
;       Written,       C D Pike, RAL, 18-May-94               
;       Passed out index of matching tag,  D Zarro, ARC/GSFC, 27-Jan-95     
;       William Thompson, GSFC, 6 March 1996    Added keyword TOP_LEVEL
;       Zarro, GSFC, 1 August 1996    Added call to help 
;       Use SIZE(/TNAME) rather than DATATYPE()  W. Landsman  October 2001
;       Added /RECURSE and /QUIET for compatibility with Solarsoft version
;                W. Landsman  March 2009
;       Slightly faster algorithm   W. Landsman    July 2009
;       July 2009 update was not setting Index keyword  W. L   Sep 2009. 
;-            
function utils_tag_exist, str, tag,index=index, top_level=top_level,recurse=recurse, $
         quiet=quiet

;
;  check quantity of input
;
compile_opt idl2
if n_params() lt 2 then begin
   print,'Use:  status = tag_exist(structure, tag_name)'
   return,0b
endif

;
;  check quality of input
;

if size(str,/TNAME) ne 'STRUCT' or size(tag,/TNAME) ne 'STRING' then begin
 if not keyword_set(quiet) then begin 
   if size(str,/TNAME) ne 'STRUCT' then help,str
   if size(tag,/TNAME) ne 'STRING' then help,tag
   print,'Use: status = tag_exist(str, tag)'
   print,'str = structure variable'
   print,'tag = string variable'
  endif 
   return,0b
endif

  tn = tag_names(str)

  nt = where(tn eq strupcase(tag)) & index=nt[0]
  no_match = index EQ -1

 if no_match  and not keyword_set(top_level) then begin
       status= 0b
       for i=0,n_elements(tn)-1 do begin
        if size(str.(i),/TNAME) eq 'STRUCT' then $
                status=utils_tag_exist(str.(i),tag,index=index)
        if status then return,1b
      endfor
    return,0b

endif else return,~no_match
end

;+
; :Description:
;    Removes a tag from a structure.
;
; :Params:
;    struct: in, required
;            the structure
;    tagname: in, required
;             the tag name
;
; :History:
;     Written by FaM, 2011.
;
;-
pro utils_remove_tag, struct, tagname

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  if n_params() ne 2 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~ arg_okay(tagname, TYPE='IDL_STRING') then MESSAGE, WAVE_Std_Message('tagname', /STRING)

  searchtag=strupcase(tagname)
  tagnames=tag_names(struct)
  a=[-1]
  if n_elements(tagname) eq 1 then a=[a,where(tagnames $
    ne searchtag)] else for i=0,n_elements(tagnames)-1 do $
    if (where(searchtag eq tagnames[i]))[0] eq -1 then a=[a,i]
  if n_elements(a) eq 1 then return
  if a[1] eq -1 then return
  newstruct=create_struct(tagnames[a[1]],struct.(a[1]))
  if n_elements(a) gt 2 then for i=2,n_elements(a)-1 $
    do newstruct=create_struct(newstruct,tagnames[a[i]],struct.(a[i]))
  struct=newstruct
  
end


;+
;
;   This function replaces in a given string or vector of strings all values by an other. Length or only one char didn't matter.
;   It could also be used to delete a substring from a string.
;
; :Params:
;   text:       the text where to replace some informations
;   in_string:  the search string
;   rep_string: the string which should replace in_string
;
; :Keywords:
;   no_of_replace: if set two a number, this means in text as many times of no_of_replace in_string is reaplced by rep_string
;   pos:           if set to a number the replacement starts at this string position
;   count:         this argument returns the number of replaces
;
; :Returns:
;   Result is the new text
;
; :Examples:
;   
;   Ex::
;   
;     help,replace_string('Dies ist ein Test',' ','_')
;     <Expression>    STRING    = 'Dies_ist_ein_Test'
;     help,replace_string('Dies ist ein Test',' ','_',pos=5)
;     <Expression>    STRING    = 'Dies ist_ein_Test'
;     help,replace_string('Dies ist ein Test',' ','_',pos=5,no=1)
;     <Expression>    STRING    = 'Dies ist_ein Test'
;     help,replace_string('Dies ist ein Test','ist','ist')
;     <Expression>    STRING    = 'Dies ist ein Test'
;     help,replace_string('Dies ist ein Test, ist ein','ist','ist nicht')
;     <Expression>    STRING    = 'Dies ist nicht ein Test, ist nicht ein'
;     help,replace_string('\\\\\\\\\','\','/')
;     <Expression>    STRING    = '/////////'
;     help,replace_string('["..\idl_html\idl_work_cat.htm"]','cat','cat_org')
;     <Expression>    STRING    = '["..\idl_html\idl_work_cat_org.htm"]'
;     print,replace_string(['12:33:00','12:33:00','12:33:00'],':','')
;     123300 123300 123300
;     print,replace_string(['12:33:00','12:33:00','12:33:00'],':','',pos=5)
;     12:3300 12:3300 12:3300
;     print,replace_string( 'asdf___ertz_j','__', '')
;     asdf_ertz_j
;     print,replace_string(['12:33:00','12:33:00','12:33:00'],':','',pos=5,count=c),c
;     12:3300 12:3300 12:3300
;     3
;     print,replace_string(['12:33:00','12:33:00','12:33:00'],':','',count=c),c
;     123300 123300 123300
;     6
;
;
; :History:
;   Written by: R.Bauer (ICG-1) , 1998-Sep-06 ::
;     1998-09-26 bug removed with start_pos and a vector of strings
;     1998-09-26 special replacement include if a sign should be replaced by an other by n times
;     1999-09-07 bug removed with replacing '___' by '_'
;     1999-10-01 count added
;     2000-03-08 bug with no_of_replaces removed
;                if text is an array no_of_replaces is used for each element
;     2001-02-13 Loop in LONG
;     2004-06-03 added a test if submitted parameters have values
;
; :Copyright:
;   
;    Copyright (c) 1998, Forschungszentrum Juelich GmbH ICG-1
;    All rights reserved.
;    Unauthorized reproduction prohibited.
;    This software may be used, copied, or redistributed as long as it is not
;    sold and this copyright notice is reproduced on each copy made.  This
;    routine is provided as is without any express or implied warranties
;    whatsoever.
;
;-
FUNCTION utils_replace_string, text,in_string,rep_string,pos=pos,no_of_replaces=no_of_replaces,count=count_n_replace

   IF N_PARAMS() LT 3 THEN  MESSAGE, WAVE_Std_Message(/NARG)

   If n_elements(text) eq 0 or n_elements(in_string) eq 0 or n_elements(rep_string) eq 0 then $
     MESSAGE, 'problem by passig parameters, please check the input data variables'

   counter=0
   count_n_replace=0

   IF N_ELEMENTS(no_of_replaces) GT 0 THEN number=no_of_replaces ELSE number=1E+30

   length_in_string=STRLEN(in_string)
   length_rep_string=STRLEN(rep_string)

; Sonderfall, wenn genau 1 Zeichen der Lanege 1 durch 1 anderes Zeichen der Laenge 1 und n mal ersetzt werden soll
; Dieses Verfahren ist einfach schneller
   IF length_rep_string NE 0 THEN BEGIN
      IF length_in_string + length_rep_string EQ 2 AND number EQ 1E+30 THEN BEGIN
         new_text=BYTE(text)
         IF N_ELEMENTS(pos) EQ 0 THEN BEGIN
            change=WHERE(new_text EQ ((BYTE(in_string))[0]),count_change)
            IF count_change GT 0 THEN new_text[change]=(BYTE(rep_string))[0]
            ENDIF ELSE BEGIN
            change=WHERE(new_text[pos:*] EQ ((BYTE(in_string))[0]),count_change)
            IF count_change GT 0 THEN new_text[pos+change]=(BYTE(rep_string))[0]
         ENDELSE
         count_n_replace=count_change
         RETURN,STRING(new_text)
      ENDIF
   ENDIF

; alle anderen Faelle werden so behandelt
   IF N_ELEMENTS(pos) EQ 0 THEN start_pos=0 ELSE start_pos=pos

   n_text=N_ELEMENTS(text)-1

   FOR i=0L,n_text DO BEGIN
   counter=0
      new_text=text[i]
      IF STRPOS(new_text,in_string) NE -1 AND in_string NE rep_string THEN  BEGIN
         pos_in_string=1
         text_length=STRLEN(new_text)
         WHILE pos_in_string NE -1 AND counter LT number DO BEGIN
            pos_in_string=STRPOS(new_text,in_string,start_pos)
            IF pos_in_string GT -1 THEN BEGIN
               count_n_replace=count_n_replace+1
               new_text=STRMID(new_text,0,pos_in_string)+rep_string+STRMID(new_text,pos_in_string+length_in_string,text_length)
               start_pos=pos_in_string+length_rep_string
            ENDIF
            counter=counter+1
         ENDWHILE
         if n_elements(result) eq 0 then result=new_text else result=[result,new_text]
         IF N_ELEMENTS(pos) EQ 0 THEN start_pos=0 ELSE start_pos=pos
         ENDIF ELSE BEGIN
         if n_elements(result) eq 0 then result=new_text else result=[result,new_text]
         IF N_ELEMENTS(pos) EQ 0 THEN start_pos=0 ELSE start_pos=pos
      ENDELSE

   ENDFOR
   RETURN,result

END

;+
; :Description:
;    This function simply deaccumulates a field (like RAINNC).
;    It substracts the step i from the step i+1 on the last 
;    dimension of the array, supposed to be time.
;
; :Categories:
;    WAVE/UTILS
;    
; :Params:
;    accumulated: in, required, default=none
;                 the accumulated variable (dim 1 or 3)
;
;
; :History:
;       Written by FaM, 2010
;-
function utils_acc_to_step, accumulated

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  ON_ERROR, 2
    
  siz = SIZE(accumulated, /DIMENSIONS)
  
  if N_ELEMENTS(siz) eq 3 then begin
      n = siz[2]
      tempsubs = accumulated * 0.
      tempsubs[*,*,1:n-1] = accumulated[*,*,0:n-2]
      out = accumulated - tempsubs  
      out[*,*,0] = 0
      return, out 
  endif else if N_ELEMENTS(siz) eq 1 then begin
      n = siz[0]
      tempsubs = accumulated * 0
      tempsubs[1:n-1] = accumulated[0:n-2]
      out = accumulated - tempsubs  
      out[0] = 0
      return, accumulated - tempsubs 
  endif else begin
      Message, WAVE_Std_Message('accumulated', /NDIMS)
      return, -1
  end
  
end

;+
; :Description:
;    This procedure retrieves the georeferenced equivalent positions from a lat-lon array,
;    using the nearest neighbor algorithm. 
;    
;    Therefore the output is an one dimensional array of size N_ELEMENTS(flon) or [4,N_ELEMENTS(flon)]. 
;    
; :Categories:
;    WAVE/UTILS
;    
; :Params:
;    ilon: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the longitudes corresponding to the original array.
;    ilat: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the latitudes corresponding to the original array.
;    flon: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the longitudes corresponding to desired grid.
;    flat: in, required, type=float/array, default=none
;          The 1 or 2 dimensional array of the latitudes corresponding to desired grid.
;
; :Keywords:
;    DISTANCES: out, optional
;               set this keyword to a named variable to obtain the distances between 
;               each neirest neighbor (same dim as output)
;    CLASSICAL: in, optional
;               set this keyword to force using the naive approach of the algorithm
;    TRIANGULATION: in, optional
;                   set this keyword to force using the delaunay approach of the algorithm (default)
;    FOURPOINTS: in, optional
;                set this keyword to get the 4-neirest points (currently using the naive approach, very slow)
;                
; :Returns:
;    an array of the same dimensions as flon containing the indexes in of the clothest 
;    point in the input grid. 
;     
;
; :History:
;       Written by FaM, 2010
;-
function utils_nearest_neighbor, ilon, ilat, flon, flat, DISTANCES = distances, $
                  CLASSICAL = classical, TRIANGULATION = triangulation, FOURPOINTS = fourpoints

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  ;********
  ; Check *
  ;********
  
  if n_params() ne 4 then MESSAGE, WAVE_Std_Message(/NARG)
  
  if not arg_okay(flon, /NUMERIC) or not arg_okay(flat, /NUMERIC) or not arg_okay(ilon, /NUMERIC) or not arg_okay(ilat, /NUMERIC) then $
    MESSAGE,'lat and/or lons not in numerical format'
  
  if ~array_processing(ilon, ilat) then MESSAGE, WAVE_Std_Message('ilon ilat',/ARG)
  if ~array_processing(flon, flat) then MESSAGE, WAVE_Std_Message('flon flat',/ARG)
  
  doDist = ARG_PRESENT(DISTANCES)
  
  ;********************
  ; Define output dim *
  ;********************  
  n = N_ELEMENTS(flon)
  IF KEYWORD_SET(FOURPOINTS) then begin
    if n eq 1 then begin
      out = LONARR(4) 
      if doDist then distances = MAKE_ARRAY(4, /NOZERO, TYPE=SIZE(flon, /TYPE))
    endif else begin
     out = lonarr(4,n) 
     if doDist then distances = MAKE_ARRAY(4, n, /NOZERO, TYPE=SIZE(flon, /TYPE))
    endelse  
  endif else begin
    if n eq 1 then begin
      out = 0L 
      if doDist then distances =  0D
    endif else begin
     out = lonarr(n)
     if doDist then distances = MAKE_ARRAY(n, /NOZERO, TYPE=SIZE(flon, /TYPE))
     endelse
  endelse 

  ;********************
  ; Go threw Keywords *
  ;********************  
  algo = ''
  if KEYWORD_SET(CLASSICAL) then algo = 'CLASSICAL' $
    else if KEYWORD_SET(TRIANGULATION) then algo = 'TRIANGLE' $
      else if n eq 1 then algo = 'CLASSICAL' else algo = 'TRIANGLE'        

  
  if KEYWORD_SET(FOURPOINTS) then algo = 'CLASSICAL_F'
  
  ;**********
  ; Lets go *
  ;**********
  
  CASE algo OF
  
    'CLASSICAL': begin
    
      for i = 0l, n - 1 do begin      
        quad = (flon[i] - ilon)^2 + (flat[i]- ilat)^2
        minquad = min(quad, p)    
        out[i] = p[0]
         if doDist then DISTANCES[i] = minquad        
      endfor
      
    end
    
    'CLASSICAL_F': begin
    
      for i = 0l, n - 1 do begin
        quad = (flon[i] - ilon)^2 + (flat[i]- ilat)^2
        s = sort(quad)
        out[*,i] = s[0:3]
        if doDist then distances[*,i] = quad[s[0:3]]
      endfor
      
    end
    
    'TRIANGLE': begin
    
      n1 = n_elements(ilon)
      x1 = ilon[*] & y1 = ilat[*]
      x2 = flon[*] & y2 = flat[*]    
      
      triangulate, x1, y1, c ; Compute Delaunay triangulation     
       
      out = GRIDDATA(x1,y1,LINDGEN(n1), XOUT=x2, YOUT=y2, /NEAREST_N, TRIANGLES =c)
      if doDist then distances = (ilon[out] - x2)^2 + (ilat[out] - y2)^2         
      
    end
        
  ENDCASE
          
    ;********
    ;* DONE *
    ;********
    if doDist then DISTANCES=sqrt(DISTANCES)
    if doDist then DISTANCES=reform(DISTANCES, N_ELEMENTS(flon[*,0]), N_ELEMENTS(flon[0,*]))
    
    return, reform(LONG(out), N_ELEMENTS(flon[*,0]), N_ELEMENTS(flon[0,*]))
  
end



;+
; :Description:
; 
;    This procedure follows the use of `utils_nearest_neighbor`.
;    
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    pos: in, required
;         The positions obtained with a previous call of `utils_nearest_neighbor`.
;    data: in, required
;          The 1 or 2 dimensional array of the data to fit.
;    
; :Returns:
;     the fitted data   
;
; :History:
;       Written by FaM, 2010
;-
function utils_compute_nearest_neighbor, pos, data

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  if n_params() lt 2 then message, WAVE_Std_Message(/NARG)  
  if not arg_okay(pos, /NUMERIC) then  message, 'pos not in num format'
  
  out = MAKE_ARRAY(N_ELEMENTS(pos[*,0]), N_ELEMENTS(pos[0,*]), /NOZERO, TYPE=SIZE(data, /TYPE))  
    
  out[*] = data[pos]
    
  ;********
  ;* DONE *
  ;********

  return, out
  
end

;+
; :Description:
; 
;    This procedure reads all TRMM 3B42 netcdf files from a directory, 
;    sorts them by date and aggregates the precipitation field to a 
;    single NCDF file.
;    
;    Various options are given to the user, one of them being the possibility
;    to "shift" the time serie. Originaly, TRMM 3hourly files at e.g. 09H UTC
;    represent the rainfall rate from 07H30 to 10H30. The user can choose 
;    wether to keep the time as it is with the keyword 'NOSHIFT'(standard method 
;    also used by the NASA to aggregate 3 Hourly files to daily files) () or
;    to "shift" the time serie: the prcp at 09H UTC is then the summ of half 
;    the prcp in the 06H file and half the prcp in the 09H file.
;    
;       
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    directory: in, required, type=string , default=none
;               all the 3B42 files in this directory will be recursively parsed.
; :Keywords:
;    START_TIME: in, optional, type=time, default=first available time in directory
;                starting time in the aggregated output file.
;    END_TIME: in, optional, type=time, default=last available time in directory
;             ending time in the aggregated output file.
;    OUTFILE: in, optional, type=string, default= directory+'/3B42_agg.'+date+'.nc'
;             path tto the output file. To be WAVE compliant, the file MUST BEGIN with "3B42_agg"
;    NOSHIFT: in, optional
;             if set, aggregate the files using their original timestamp. 
;             default is to set values every 3 hours as the summ of half the 
;             previous and half of the next file (see stupid TRMM conventions)
;    SUBSET_IJ: in, optional
;              set to aggregate only a subset of the file (see 'w_TRMM')
;    SUBSET_LL: in, optional
;              set to aggregate only a subset of the file (see 'w_TRMM')
;    LL_DATUM: in, optional
;               the datum for 'SUBSET_LL'  (see 'w_TRMM')
; 
; :History:
;       Written by FaM, 2010
;-
pro utils_trmm_aggregate_3B42, directory, START_TIME = start_time, END_TIME = end_time, OUTFILE = outfile, $
    NOSHIFT = noshift, SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum
    
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
    Catch, theError
    IF theError NE 0 THEN BEGIN
      Catch, /Cancel
      ok = WAVE_Error_Message(!Error_State.Msg) + ' Will not aggregate...'
      if arg_okay(outfile, TYPE = IDL_STRING) then if FILE_TEST(outfile) then FILE_DELETE, outfile
      if N_ELEMENTS(lun) ne 0 then begin 
        printf, lun, 'ERROR. Stopped aggregation.'
        FREE_LUN, lun
      endif
      RETURN
    ENDIF
  
  ; ---------------
  ; Check the input
  ; ---------------
  
  if N_ELEMENTS(directory) eq 0 then directory = DIALOG_PICKFILE(TITLE='Please select directory to parse', /DIRECTORY)
  if not FILE_TEST(directory, /DIRECTORY) then message, 'Directory is not a directory'
  
  fileList = FILE_SEARCH(directory, '3B42.*', /MATCH_INITIAL_DOT, /EXPAND_ENVIRONMENT, count = cfiles)
  if cfiles eq 0 then Message, '$directory is not set properly.'
  
  fnames = FILE_BASENAME(fileList)
  so = SORT(fnames)
  if N_ELEMENTS(fnames[UNIQ(fnames, so)]) ne cfiles then Message, 'TRMM files not unique.'
  fileList = fileList[so]
    
  step = MAKE_TIME_STEP(hour=3)
  
  ; ---------------------
  ; Create the time serie
  ; ---------------------
  ;Parse names for available times
  fname = FILE_BASENAME(filelist)
  for i=0, cfiles-1 do begin
    tsp = STRSPLIT(fname[i], '.',/EXTRACT)
    tsd = tsp[1]
    y = LONG(STRMID(tsd,0,2))
    if y lt 80 then y +=2000 else y+=1900
    m = LONG(STRMID(tsd,2,2))
    d = LONG(STRMID(tsd,4,2))
    h = LONG(tsp[2])
    if N_ELEMENTS(time) eq 0 then time = QMS_TIME(YEAR=y, MONTH=m, DAY=d, HOUR=h) $
    else  time = [time, QMS_TIME(YEAR=y, MONTH=m, DAY=d, HOUR=h)]
  endfor
  time = time[sort(time)]    
  if ~check_TimeSerie(time) then message, '  The files do not form a continous time serie.'
  
  if ~KEYWORD_SET(NOSHIFT) then time = time[1:(N_ELEMENTS(time)-1)]
  nt = N_ELEMENTS(time)
  p0 = 0
  p1 = nt-1
  if KEYWORD_SET(START_TIME) then begin
    if ~check_WTIME(START_TIME, OUT_QMS=t0) then Message, WAVE_Std_Message('START_TIME', /ARG)
    p0 = where(time eq t0, cnt)
    if cnt ne 1 then Message, 'Your start time (' + TIME_to_STR(t0) + $
      ') do not match in my TS: (' + TIME_to_STR(time[0]) + '->'+ TIME_to_STR(time[nt-1])+'). Taking yours.', /INFORMATIONAL
  endif
  if KEYWORD_SET(END_TIME) then begin
    if ~check_WTIME(END_TIME, OUT_QMS=t1) then Message, WAVE_Std_Message('END_TIME', /ARG)
    p1 = where(time eq t1, cnt)
    if cnt ne 1 then Message, 'Your end time (' + TIME_to_STR(t1) + $
      ') do not match in my TS: (' + TIME_to_STR(time[0]) + '->'+ TIME_to_STR(time[nt-1])+'). Taking yours.', /INFORMATIONAL
  endif
  
  time = time[p0:p1]
  nt = N_ELEMENTS(time)
  t0 = time[0]
  t1 = time[nt-1]
  
  ; -------------------
  ; Create the log file
  ; -------------------
  str = TIME_to_STR(t0) ; 22.10.2008 03:00:00
  str = strmid(str,6,4) + '_' + strmid(str,3,2) + '_' + strmid(str,0,2)
  
  if ~KEYWORD_SET(OUTFILE) then outfile = DIRECTORY + '/3B42_agg.' + str + '.nc'
  if STRMID(FILE_BASENAME(OUTFILE),0,8) ne '3B42_agg' then Message, 'The output file MUST have the suffix 3B42_agg'
  
  OPENW, lun, FILE_DIRNAME(outfile) + '/trmm_agg_'+ str + '.log', /GET_LUN
  
  printf, lun, 'TRMM 3B42 aggregation'
  printf, lun, ''
  printf, lun, 'Number of files: ' + str_equiv(cfiles)  
  printf, lun, 'Start date : ' + TIME_to_STR(time[0])
  printf, lun, 'End   date : ' + TIME_to_STR(time[nt-1])
  
  ; Open the file you just created and copy the information in it to another file.
  printf, lun, ''
  text = 'Destination file : ' + outfile
  printf, lun, text
  
  ; ---------------------------
  ; Read the Netcdf template file
  ; ---------------------------
  template = OBJ_NEW('w_TRMM', FILE=fileLIST[0], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)
  TEMPLATE->get_ncdf_coordinates, lon, lat, nx, ny
  TEMPLATE->GetProperty, SUBSET = subset
  OBJ_DESTROY, TEMPLATE
  ; ---------------------------
  ; Create the Netcdf out file
  ; ---------------------------
  tid = NCDF_CREATE(outfile, /CLOBBER, /NET)
  NCDF_CONTROL, tid, /FILL
  ;Define dimensions
  dimTimeid  = NCDF_DIMDEF(tid, 'time', nt)
  dimLonid  = NCDF_DIMDEF(tid, 'lon', nx)
  dimLatid  = NCDF_DIMDEF(tid, 'lat', ny)
  
  ; Define tvariables
  Timeid = NCDF_VarDef(tid, 'time', dimTimeid, /LONG)
  ; Add attributes
  NCDF_AttPut, tid, Timeid, 'zone', 'UTC', /CHAR
  d = TIME_to_STR(time[0])
  str = 'hours since ' + STRMID(d,6,4) + '-' + STRMID(d,3,2) + '-' + STRMID(d,0,2) + ' ' + STRMID(d,11,2)
  NCDF_AttPut, tid, Timeid, 'units', str, /CHAR
  
  Lonid = NCDF_VarDef(tid, 'lon', dimLonid, /FLOAT)
  NCDF_AttPut, tid, Lonid, 'units', 'degrees_east', /CHAR
  Latid = NCDF_VarDef(tid, 'lat', dimLatid, /FLOAT)
  NCDF_AttPut, tid, Latid, 'units', 'degrees_north', /CHAR
  
  pcpid = NCDF_VarDef(tid, 'precipitation', [dimLonid, dimLatid, dimTimeid], /FLOAT)
  NCDF_AttPut, tid, pcpid, 'units', 'mm 3hrs-1', /CHAR
  
  chkid = NCDF_VarDef(tid, 'check', dimTimeid, /LONG)
  NCDF_AttPut, tid, chkid, 'units', 'nb file per step', /CHAR
  
  ; Add global attributes to the file.
  NCDF_AttPut, tid, 'creation_date', TIME_to_STR(QMS_TIME()), /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'conventions', 'COARDS', /GLOBAL, /CHAR
  
  if KEYWORD_SET(NOSHIFT) then tinfo = 'The value at each UTC step is the accumulated prcp over the three PREVIOUS hours (mm 3hrs-1).' $
  else TINFO = 'The value at each UTC step is the accumulated prcp over the three SURROUNDING hours (mm 3hrs-1).'
  NCDF_AttPut, tid, 'time_info', tinfo, /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'source', 'TRMM 3B42 three-hourly product.', /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'title', 'TRMM 3B42 agg file', /GLOBAL, /CHAR
  NCDF_AttPut, tid, 'subset', 'subset = ['+ str_equiv(subset[0]) +', '+ str_equiv(subset[1]) +', '+ str_equiv(subset[2]) +', '+ str_equiv(subset[3]) +']', /GLOBAL, /CHAR
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
  
  printf, lun,  'Destination file defined.'
  printf, lun, 'Now starting to fill... '
    
  hours = INDGEN(nt, /LONG) * 3L
  NCDF_VARPUT, tid, Timeid, hours
  NCDF_VARPUT, tid, Lonid, REFORM(lon[*,0])
  NCDF_VARPUT, tid, Latid, REFORM(lat[0,*])
  if ~KEYWORD_SET(NOSHIFT) then NCDF_VARPUT, tid, pcpid, FLTARR(nx,ny,nt)
  NCDF_VARPUT, tid, chkid, LONARR(nt)
  
  for i = 0, cfiles-1 do begin
  
    t_obj = OBJ_NEW('w_TRMM', FILE=fileLIST[i], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)
    
    if ~OBJ_VALID(t_obj) then t_obj = OBJ_NEW('w_TRMM', FILE=fileLIST[i], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)
    if ~OBJ_VALID(t_obj) then t_obj = OBJ_NEW('w_TRMM', FILE=fileLIST[i], SUBSET_LL = subset_ll, SUBSET_IJ = SUBSET_ij, LL_DATUM = ll_datum)
    if ~OBJ_VALID(t_obj) then Message, 'FILE: ' + fileLIST[i] + ' is a problem.'
    
    
    pcp = (t_obj->get_prcp(t) > 0) * 3
    
    undefine, chk
    if KEYWORD_SET(NOSHIFT) then begin
      idx = where(time eq t, cnt)
      if cnt eq 1 then begin 
       NCDF_VARPUT, tid, pcpid, pcp, OFFSET = [0,0,idx]
       NCDF_VARGET, tid, chkid, chk, OFFSET = idx, count = 1
       NCDF_VARPUT, tid, chkid, chk + 1, OFFSET = idx
       undefine, chk
      endif
    endif else begin
      idx = where(time eq t, cnt)
      if cnt eq 1 then begin
        NCDF_VARGET, tid, pcpid, pcpold, OFFSET = [0,0,idx], count = [nx,ny,1]
        NCDF_VARPUT, tid, pcpid, pcpold + pcp/2., OFFSET = [0,0,idx]
        NCDF_VARGET, tid, chkid, chk, OFFSET = idx, count = 1
        NCDF_VARPUT, tid, chkid, chk + 1, OFFSET = idx
      endif     
      undefine, chk
      idx = where(time eq t + 3LL*H_QMS, cnt)
      if cnt eq 1 then begin
        NCDF_VARGET, tid, pcpid, pcpold, OFFSET = [0,0,idx], count = [nx,ny,1]
        NCDF_VARPUT, tid, pcpid, pcpold + pcp/2., OFFSET = [0,0,idx]
        NCDF_VARGET, tid, chkid, chk, OFFSET = idx, count = 1
        NCDF_VARPUT, tid, chkid, chk + 1, OFFSET = idx
      endif      
    endelse
    OBJ_DESTROY, t_obj
    printf, lun,  '  ' + fileLIST[i] + ' processed.'
    
  endfor
  
  NCDF_VARGET, tid, chkid, chk
  if KEYWORD_SET(NOSHIFT) then should = 1 else should = 2
  p = where(chk ne should, cnt)
  if cnt ne 0 then Message, 'Problem with check', /INFORMATIONAL
  if cnt ne 0 then printf, lun,  'Problem with check'
   
  printf, lun,  'DONE!'
  FREE_LUN, lun
  NCDF_CLOSE, tid
  
end

;+
; :Description:
;    
;    !!! Not implemented yet !!!
;    see 'utils_TRMM_aggregate_3B42' 
; TODO: Implement procedure   
;-
pro utils_trmm_aggregate_3B43
    

  
end


;+
; :Description:
;    Reads specific tags from the 'CoreMetadata.0' attribute of MODIS/EOS files.
;
; :Categories:
;    WAVE/UTILS
;
; :Params:
;    pvlstring: in, type= , default=none
;    objstring: in, type= , default=none
;    n_char: in, type= , default=none
;
; :History:
;       Written by FaM, 2010
;-
function utils_eod_get_metadata, pvlstring, objstring, n_char

  ; Extract a beginning date object
  objstring = strupcase(objstring)
  val_str = '"'
  obj_start = strpos(pvlstring,objstring)
  if obj_start ne -1  then obj_start = strpos(pvlstring,val_str,obj_start)+1
  info = strmid(pvlstring,obj_start,n_char)
  return, info
    
end


;+
; :Description:
; 
;    This procedure reads a time from a ncdf file that uses the COARDS convention.    
;    Returns TRUE if time is found, FALSE in all other cases
;
; :Params:
;    cdfid: in, required, type = long
;           the active file cdfid
;    time: out, optional, type = time
;          the time serie (if found)
;    time0: out, optional, type = time
;           the first time in the time serie (if found)
;    time1: out, optional, type = time
;           the last time in the time serie (if found)
;    nt: out, optional, type = long
;        the number of elements in the time serie

; :Keywords:
;     VarName: in, optional, type = String
;              to specify the name of the variable which is coards compliant
;         
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :History:
;     Written by FaM, 2010.
;-
function utils_nc_coards_time, cdfid, time, time0, time1, nt, VARNAME = varname

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN, FALSE
  ENDIF  
  
  s_list = ['time','times','xtime']
  
  inq = NCDF_INQUIRE(Cdfid)
  vok = -1
  
  if KEYWORD_SET(VarName) then begin    
      
    vinf = NCDF_VARINQ(Cdfid, VarName)
    vname = vinf.name
    vtype = vinf.DATATYPE
    vok = NCDF_VARID(Cdfid, VarName) 
    
  endif else begin
  
    for varid = 0, inq.NVARS - 1 do begin
      vinf = NCDF_VARINQ(Cdfid, varid)
      vname = vinf.name
      vtype = vinf.DATATYPE
      p = where(str_equiv(s_list) eq str_equiv(vname), cnt)
      if cnt ne 0 then begin
        test = 1
      endif
      
      if cnt ne 0 and vtype ne 'CHAR' and  vtype ne 'BYTE' then begin
        vok = varid
        break
      endif
    endfor
    
  endelse
        
  if vok lt 0 then return, FALSE
  
  
  ; Read time0 from att
  NCDF_ATTGET, Cdfid, vok , 'units', ts
  ts = STRSPLIT(STRING(ts), ' ', /EXTRACT)
  psince = WHERE(str_equiv(ts) eq str_equiv('since'), csince)
  if csince ne 1 then begin
    ; Read time0 from att
    NCDF_ATTGET, Cdfid, vok , 'description', ts
    ts = STRSPLIT(STRING(ts), ' ', /EXTRACT)
    psince = WHERE(str_equiv(ts) eq str_equiv('since'), csince)    
  endif
  if csince ne 1 then return, FALSE
  
  unit = ts[psince - 1]
  d = ts[psince + 1]
  t = ts[psince + 2]  
  if N_ELEMENTS(ts) gt psince + 3 then Message, 'Time contains a zone (which is currently not supported) or is not of suitable format'
   
  d = STRSPLIT(d,'-', /EXTRACT)   
  if N_ELEMENTS(d) ne 3 then return, FALSE  
  y = LONG(d[0])
  mo = LONG(d[1])
  d = LONG(d[2])
  
  t = STRSPLIT(t,':', /EXTRACT)   
  if N_ELEMENTS(t) eq 1 then begin
   h = LONG(t[0])
  endif else if N_ELEMENTS(t) eq 2 then begin
   h = LONG(t[0])
   mi = LONG(t[1])
  endif else if N_ELEMENTS(t) eq 3 then begin
   h = LONG(t[0])
   mi = LONG(t[1])
   s = LONG(t[2])
   milli = LONG64( (DOUBLE(t[2]) - FLOOR(double(t[2]))) * 1000D )
  endif else return, FALSE  
       
  is_OLD = FALSE
  if y lt 1900 then begin 
    is_OLD = TRUE
    jd0 = JULDAY(mo,d,y,h,mi,s)
    time0 = QMS_TIME(YEAR=1900, MONTH=01, DAY=01)
    jd1 = TIME_to_JD(time0)
  endif else time0 = QMS_TIME(YEAR=y, MONTH=mo, DAY=d, HOUR = h, MINUTE=mi, SECOND=s, MILLISECOND=milli)
  
  NCDF_VARGET, Cdfid, vok, u
  
  fac = 0LL
  is_reg = '' ; to see if we have to handle the unregular cases
  case (str_equiv(UNIT)) of
    'SEC': fac = S_QMS
    'SECS': fac = S_QMS
    'S': fac = S_QMS
    'SS': fac = S_QMS
    'SECOND': fac = S_QMS
    'SECONDS': fac = S_QMS
    'MINUTE':  fac = M_QMS
    'MINUTES': fac = M_QMS
    'MIN': fac = M_QMS
    'MINS': fac = M_QMS
    'HOUR': fac = H_QMS
    'HOURS': fac = H_QMS
    'HR': fac = H_QMS
    'HRS': fac = H_QMS
    'H': fac = H_QMS
    'HS': fac = H_QMS
    'DAY': fac = D_QMS
    'DAYS': fac = D_QMS
    'D': fac = D_QMS
    'DS': fac = D_QMS
    'MONTH': is_reg = 'M'
    'MONTHS': is_reg = 'M'
    'MON': is_reg = 'M'
    'YEAR': is_reg = 'Y'
    'YEARS': is_reg = 'Y'
    'YR': is_reg = 'Y'
    'YRS': is_reg = 'Y'  
    else: return, FALSE
  endcase
  
  if is_reg eq '' then begin ; usual case
    if is_OLD then begin
      deltaU = (jd1 - jd0) * D_QMS / fac
      if MIN(u) lt deltaU then deltaU = 0
    endif else deltaU = 0
    time = time0 + fac * LONG64(u - deltaU)
    nt = N_ELEMENTS(time)
  endif else begin 
    nt = N_ELEMENTS(u)
    time = LON64ARR(nt)
    if is_reg eq 'M' then for i=0, N_ELEMENTS(u)-1 do time[i] = MAKE_REL_DATE(time0, MONTH=u[i])
    if is_reg eq 'Y' then for i=0, N_ELEMENTS(u)-1 do time[i] = MAKE_REL_DATE(time0, YEAR=u[i])
  endelse

  time0 = time[0]
  time1 = time[nt-1]
  
  return, TRUE

end

;+
; :Description:
;    This procedure reads a time from a WRF ncdf file
;    Returns TRUE if time is found, FALSE in all other cases
;
; :Params:
;    cdfid: in, required, type = long
;           the active file cdfid
;    time: out, optional, type = time
;          the time serie (if found)
;    time0: out, optional, type = time
;           the first time in the time serie (if found)
;    time1: out, optional, type = time
;           the last time in the time serie (if found)
;    nt: out, optional, type = long
;        the number of elements in the time serie
;        
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :History:
;     Written by FaM, 2010.
;-
function utils_wrf_time, cdfid, time, time0, time1, nt

  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    RETURN, FALSE
  ENDIF  
  
  NCDF_VARGET, cdfid, 'Times', stimes
  ntimes = N_ELEMENTS(stimes[0,*])
  stimes = STRING(stimes[*,0:ntimes-1])    
  
  if stimes[0] eq '0000-00-00_00:00:00' then begin ;Check if geogrid
    NCDF_ATTGET, cdfid, 'TITLE', title, /GLOBAL
    isHere = STRPOS(str_equiv(title), 'GEOGRID')
    if isHere ne -1 then time = QMS_TIME(year = 2000, month = 01, day = 01) else Message, 'Really dont know whate this is'
  endif else begin  
  ;String format : '2008-10-26_12:00:00; length 19
  time = QMS_TIME(YEAR=STRMID(stimes,0,4), MONTH=STRMID(stimes,5,2),DAY=STRMID(stimes,8,2), $
      HOUR=STRMID(stimes,11,2),MINUTE=STRMID(stimes,14,2),SECOND=STRMID(stimes,17,2))
  endelse
  
  nt = N_ELEMENTS(time)
  time0 = time[0]
  time1 = time[nt-1]
  
  return, TRUE

end
 
;+
; :Description:
;   This procedure reads lons and lats from a ncdf file, trying several known variable names. 
;   Returns TRUE if time is found, FALSE in all other cases
;   
; :Params:
;    cdfid: in, required, type = long
;           the active file cdfid
;    lon_id: out, optional, type = long
;            the netcdf variable ID of the longitudes (if found)
;    lat_id: out, optional, type = long
;            the netcdf variable ID of the latitudes (if found)
;        
; :Returns:
;   TRUE if time is found, FALSE in all other cases
;
; :History:
;     Written by FaM, 2010.
;-
function utils_nc_LonLat, cdfid, lon_id, lat_id

  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
    
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN, FALSE
  ENDIF  
  
  lon_list = ['lon','longitude','longitudes','lons','xlong','xlong_m', 'dimlon','x', 'lon_3']
  lat_list = ['lat','latitude','latitudes' ,'lats','xlat' ,'xlat_m', 'dimlat','y', 'lat_3']
  
  inq = NCDF_INQUIRE(Cdfid)
  
  lon_id = -1
  lat_id = -1
  for varid = 0, inq.NVARS - 1 do begin
    vname = (NCDF_VARINQ(Cdfid, varid)).name
    p = where(str_equiv(lon_list) eq str_equiv(vname), cnt)
    if cnt ne 0 then lon_id = varid
    p = where(str_equiv(lat_list) eq str_equiv(vname), cnt)
    if cnt ne 0 then lat_id = varid
    if lat_id ge 0 and lon_id ge 0 then break
  endfor
  
  if lat_id lt 0 or lon_id lt 0 then return, FALSE
  
  return, TRUE

end

;+
; :Description:
; 
;    This procedure is a low level WRF pre-processing tool to copy a WRF geo_em file
;    and change the land_cat dimension to 33.
;    The newly created file will be striclty identical exept for the land_cat dimension value,
;    the LANDUSEF variable and the num_land_cat global attribute. 
;
; :Params:
;    file: in, optional, type = string
;          the file to copy. It will not be modified. If not present, a dialog window will open  
; :Keywords:
;    NEW_LUF: in, optional, type = float array
;             The new lu fraction (dimensions: nx, ny, 33). If not set, the original fraction is kept
;             and the classes 25 to 33 are all set to 0. 
;             
;             If set, the mew dominant Category (LU_INDEX) will be computed, as well as the new LANDMASK.
; TODO: Update routine: Careful - SOILCTOP, SOILCBOT, SCT_DOM, SCB_DOM are NOT actualized, but this should be ok...
;             
; :History:
;       Written by FaM, 16 Dec 2010 
;-
pro utils_usgs_24_to_33, file, NEW_LUF = new_luf

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
;  ON_ERROR, 2

  if N_ELEMENTS(file) eq 0 then file = DIALOG_PICKFILE(TITLE='Please select the geo file to copy.')
  outfile = file + '.33'
  
  ; Check new_luf
  tochange = KEYWORD_SET(new_luf)
  
  if tochange then begin
  
    if ~arg_okay(NEW_LUF, /NUMERIC, N_DIM=3) then message, WAVE_Std_Message('new_luf',/ARG)
    dims = SIZE(new_luf, /DIMENSIONS)
    luf = FLOAT(NEW_LUF)
    dummy = MAX(luf, pd, DIMENSION=3)
    inds = ARRAY_INDICES(luf, pd)
    inds = reform(inds[2, *], dims[0], dims[1])
    dominant = inds + 1
    
    ; if 50-50,  then water
    water = luf[*,*,15]
    pwat = where(water ge 0.5, cnt)
    if cnt ne 0 then dominant[pwat] = 16
   
    ; If water lt 49, then second max
    pnowat = where(dominant eq 16 and water le 0.5, cnt)  
    water[pnowat] = 0.
    myluf = luf
    myluf[*,*,15] = water  
    dummy = MAX(myluf, pd, DIMENSION=3)
    inds = ARRAY_INDICES(myluf, pd)
    inds = reform(inds[2, *], dims[0], dims[1])
    dominant[pnowat] = inds[pnowat] + 1
    undefine, myluf
    
    ; calculate Landmask
    lm = dominant * 0 + 1 
    p = where(dominant eq 16, cnt)
    if cnt ne 0 then lm[p] = 0
    
  end

  sid = Ncdf_open(File, /NOWRITE)        
  inq = NCDF_INQUIRE(sid)
  tid = NCDF_CREATE(outfile, /CLOBBER)
  NCDF_CONTROL, tid, /FILL
    
  for i =0, inq.ndims-1 do begin
  
    NCDF_DIMINQ, sid, i, sName, sSize  
    if str_equiv(sName) eq 'LAND_CAT' then ssize = 33
    
    if str_equiv(sName) eq 'TIME' then dummy = NCDF_DIMDEF(tid, sName, /UNLIMITED) $
    else dummy = NCDF_DIMDEF(tid, sName, ssize)

  endfor ; Dimensions OK
    
  for i =0, inq.Ngatts-1 do begin  
  
    sName = NCDF_ATTNAME(sid, i , /GLOBAL)        
    sAtt_info = NCDF_attINQ(sid, sName, /GLOBAL)
    NCDF_ATTGET, sid, sName, sValue, /GLOBAL
        
    ; Set the appropriate netCDF data type keyword.
    CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1      
    ENDCASE
    
    if str_equiv(sName) eq str_equiv('num_land_cat') then sValue = 33

    ; Add the attribute to the file.
    NCDF_AttPut, tid, sName, svalue, /GLOBAL, $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    
  endfor ; Att OK
    
  for svid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,svid)
      
    ; Check the data type to see that it conforms to netCDF protocol.
    CASE StrUpCase(s_var_info.datatype) OF
      'BYTE': tbyte = 1
      'CHAR': tchar = 1
      'DOUBLE': tdouble = 1
      'FLOAT': tfloat = 1
      'LONG': tlong = 1
      'SHORT': tshort = 1
    ENDCASE
        
    ; If the dimension names are present, use them to get the dimension IDs, which are needed to define the variable.
    dimsIds = s_var_info.dim
    ; Define the variable.
    TvID = NCDF_VarDef(tid, s_var_info.name, dimsIds, $
      BYTE=tbyte, $
      CHAR=tchar, $
      DOUBLE=tdouble, $
      FLOAT=tfloat, $
      LONG=tlong, $
      SHORT=tshort)
    undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    
    if s_var_info.natts eq 0 then continue ; no need to continue (just for time actually)
    
    ; Copy the variable attributes
    for sattid = 0, s_var_info.NATTS - 1 do begin
    
      sName = NCDF_ATTNAME(sid, svid, sattid)
      sAtt_info = NCDF_attINQ(sid, svid, sName)
      NCDF_ATTGET, sid, svid, sName, sValue
      
      ; Set the appropriate netCDF data type keyword.
      CASE StrUpCase(sAtt_info.DATATYPE) OF
        'BYTE': tbyte = 1
        'CHAR': tchar = 1
        'DOUBLE': tdouble = 1
        'FLOAT': tfloat = 1
        'LONG': tlong = 1
        'SHORT': tshort = 1
      ENDCASE
      
      if StrUpCase(sAtt_info.DATATYPE) eq 'CHAR' then begin
          isHere = STRPOS(str_equiv(sValue),str_equiv('24-category'))
          if isHere ne -1 then GEN_str_subst, ret, STRING(sValue),'24','33', sValue
          if isHere ne -1 and tochange then GEN_str_subst, ret, STRING(sValue),'USGS','user-defined', sValue          
      endif
      
      ; Add the attribute to the file.
      NCDF_AttPut, tid, TvID, sName, sValue,  $
        BYTE=tbyte, $
        CHAR=tchar, $
        DOUBLE=tdouble, $
        FLOAT=tfloat, $
        LENGTH=tlength, $
        LONG=tlong, $
        SHORT=tshort     
      undefine, tbyte,tchar,tdouble,tfloat,tlength,tlong,tshort
    endfor
      
  endfor
  
  NCDF_CONTROL, tid, /ENDEF ; Switch to normal Fill mode
 
  for vid =0, inq.NVARS-1 do begin
  
    s_var_info = NCDF_VARINQ(sid,vid)
    ndims = s_var_info.ndims
   
    NCDF_VARGET, sid, vid, var
    
    if str_equiv(s_var_info.name) eq str_equiv('LANDUSEF') then begin
      if tochange then var = LUF $
      else begin 
        dim = SIZE(var, /DIMENSIONS)
        var = [[[var]],[[FLTARR(dim[0],dim[1],9)]]]
      endelse
    endif
        
    if str_equiv(s_var_info.name) eq str_equiv('LANDMASK') and tochange then var = FLOAT(lm)
    if str_equiv(s_var_info.name) eq str_equiv('LU_INDEX') and tochange then var = FLOAT(dominant)
    
    NCDF_VARPUT, tid, vid, var

  endfor  
  
  NCDF_CLOSE, sid ; Close source file
  NCDF_CLOSE, tid ; Close file

end


;+
; :Description:
;    This function makes a simple mean aggregation of a 2d array to a small array
;    using a given ratio.
;
; :Params:
;    array: in, required, type = float
;           the 2d array to aggregate
;    ratio: in, required, type = long
;           the ratio (it must be a divider of both X and Y dimensions)
; 
; :Examples:
;   A very easy usage:: 
;     IDL> array = [[0,0,1,1],[2,2,1,1],[2,2,2,2],[3,3,3,3]]
;     IDL> print, array
;            0       0       1       1
;            2       2       1       1
;            2       2       2       2
;            3       3       3       3
;     IDL> print, utils_aggregate_Grid_data(array, 2)
;            1.0000000       1.0000000
;            2.5000000       2.5000000
;
; :History:
;     Written by FaM, 2010.
;-
function utils_aggregate_grid_data, array, ratio ; TODO: Update routine: add grid update

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
  
  if ~arg_okay(array, /NUMERIC, /ARRAY, N_DIM=2) then message, WAVE_Std_Message('array',/ARG)
  if ~arg_okay(ratio, /NUMERIC, /SCALAR) then message, WAVE_Std_Message('ratio',/ARG)
     
  siz = SIZE(array, /DIMENSIONS)
  nxin = siz[0]
  nyin = siz[1]
  
  if nxin / DOUBLE(ratio) ne (nxin / ratio) or nyin / DOUBLE(ratio) ne (nyin / ratio) then message, 'Ratio is not a divider of array.'

  nxout = nxin / ratio
  nyout = nyin / ratio
  
  agg = DBLARR(nxout,nyout)
  tempx = DBLARR(nxout,nyin)
    
  for i = 0, nxin-1 do tempx[i/ratio,*] += array[i,*]            
  for j = 0, nyin-1 do agg[*,j/ratio] += tempx[*,j]      

  return, agg / DOUBLE(ratio*ratio)
  
end

;+
; :Description:
;    This function converts any colors vector (rgb, strings, indexes in a 
;    color table...) to 24-bit long integers.
;    It is strongly inspired from the Coyote Graphics routines.
;    
; :Returns:
;    A vector of 24-bit long integers.
;
; :Keywords:
;    COLORS: in, optional
;            if it is a 1-dimensional vector, the colors to convert (strings in cgcolor, 
;            indexes in the current color table, etc.)
;            if it is a two dimensional vector, it must be 3xN dimensional whereas the three 
;            first columns are the RGB values. 
;            it not set, the colors will be deduced from the other keywords
;    NCOLORS: in, optional
;             the number of colors in the output vector (only if COLORS is not set)
;    CMIN: in, optional
;          The lowest color index of the colors to be loaded in
;          the color vector (only if COLORS is not set)
;    CMAX: in, optional
;          The highest color index of the colors to be loaded in
;          the color vector (only if COLORS is not set)
;    INVERTCOLORS: in, optional 
;                  Setting this keyword inverts the colors in the color vector (only if COLORS is not set)
;    R: out, optional
;       the equivalent Rgb triplet
;    G: out, optional
;       the equivalent rGb triplet
;    B: out, optional
;       the equivalent rgB triplet
;
; :History:
;     Written by FaM, 2011.
;-
function utils_color_convert, COLORS = colors, NCOLORS = ncolors, CMIN = cmin, CMAX = cmax, INVERTCOLORS = INVERTCOLORS, R = r, G = g, B = b

  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2

  if N_ELEMENTS(cmin) eq 0 then cmin = 0
  if N_ELEMENTS(cmax) eq 0 then cmax = 255
  
  ; Check parameters and keywords.
  IF N_Elements(colors) EQ 0 THEN BEGIN ; NO colors given
    IF N_Elements(ncolors) EQ 0 THEN begin
     _colors = 'white' 
      NCOLORS = 1
    endif else if NCOLORS gt 1 then _colors = Scale_Vector(Indgen(ncolors), cmin, cmax) else _COLORS = cmin
    
  ENDIF ELSE begin ; COLORS given
    if arg_okay(colors, N_DIM=2) then begin
      dims = SIZE(colors, /DIMENSIONS)
      if dims[1] ne 3 then message, WAVE_Std_Message('colors', /ARG)
      _colors = COLOR24(colors)
    endif else _colors = colors
  endelse
  
  ncolors = N_Elements(_colors)
  
  ; I would prefer to draw in 24-bit color
  CASE !D.Name OF
    'X': BEGIN
      Device, Get_Visual_Depth=theDepth
      IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
    END
    'WIN': BEGIN
      Device, Get_Visual_Depth=theDepth
      IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
    END
    'Z': BEGIN
      Device, Get_Pixel_Depth=theDepth
      IF theDepth GE 24 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      Device, GET_DECOMPOSED=theState, DECOMPOSED=supportsTrueColor
    END
    'PS': BEGIN
      IF Float(!Version.Release) GE 7.1 THEN supportsTrueColor = 1 ELSE supportsTrueColor = 0
      IF supportsTrueColor THEN BEGIN
        Device, DECOMPOSED=1
        theState = 1
      ENDIF
    END
    ELSE: BEGIN
      supportsTrueColor = 0
    END
  ENDCASE
  
  ; Set up the colors for drawing. All 24-bit if it supports true color.
  IF supportsTrueColor THEN BEGIN
    CASE Size(_colors, /TNAME) OF
      'STRING': BEGIN
        _colors = cgColor(_colors, DECOMPOSED=1, FILE=file)
      END
      'INT': BEGIN
        TVLCT, r, g, b, /GET
        temp = LonArr(ncolors)
        FOR j=0,ncolors-1 DO BEGIN
          temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
        ENDFOR
        _colors = Temporary(temp)
      END
      'ULONG':
      'LONG': BEGIN
      
        ; If the state is NOT using decomposed color, these are probably
        ; color index numbers, rather than long integers to be decomposed.
        IF theState EQ 0 THEN BEGIN
          TVLCT, r, g, b, /GET
          temp = LonArr(ncolors)
          FOR j=0,ncolors-1 DO BEGIN
            temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
          ENDFOR
          _colors = Temporary(temp)
        ENDIF
        
        ; If the maximum value of these long integers in not over 255, then
        ; we can be pretty sure these are color index numbers. At least I'm
        ; going to treat them that way for now and see what kind of trouble I
        ; get in.
        IF Max(_colors) LE 255 THEN BEGIN
          TVLCT, r, g, b, /GET
          temp = LonArr(ncolors)
          FOR j=0,ncolors-1 DO BEGIN
            temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
          ENDFOR
          _colors = Temporary(temp)
        ENDIF
      END
      'BYTE': BEGIN
        TVLCT, r, g, b, /GET
        temp = LonArr(ncolors)
        FOR j=0,ncolors-1 DO BEGIN
          temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
        ENDFOR
        _colors = Temporary(temp)
      END
      ELSE: BEGIN
        TVLCT, r, g, b, /GET
        temp = LonArr(ncolors)
        FOR j=0,ncolors-1 DO BEGIN
          temp[j] = Color24([r[_colors[j]], g[_colors[j]], b[_colors[j]]])
        ENDFOR
        _colors = Temporary(temp)
      END
    ENDCASE
  ENDIF ELSE BEGIN  
   ; I'd rather not go here
    MESSAGE, 'NO True color?'  
  ENDELSE
  
  if KEYWORD_SET(INVERTCOLORS) then _colors  = ROTATE(_colors, 2)
  
  utils_color_rgb, _colors, r, g, b  
  return, _colors  
  
end

;+
; :Description:
;    Converts a 24 bit color vector into a rgb triple.
;
; :Params:
;    color: in, required, type = LONG
;           the colors to converts
;    r: out
;    g: out
;    b: out
;    
; :History:
;     Written by FaM, 2011.
;-
pro utils_color_rgb, color, r, g, b
   
  ; Set Up environnement
  @WAVE.inc
  COMPILE_OPT idl2
   
  if ~ arg_okay(color, TYPE=IDL_LONG) then message, WAVE_Std_Message('color', /ARG)
    
  UNDEFINE, r, g, b
  r = BYTE(color) * 0B
  g = r
  b = r
  
  for i = 0, N_ELEMENTS(color)-1 do begin  
    bi = ROTATE(BitGet(LONG(color[i])), 2)  
    tr = 0L
    tg = 0L
    tb = 0L
    for j = 0, 7 do tr += bi[j] * 2 ^ j
    for j = 8, 15 do tg += bi[j] * 2 ^ (j-8)
    for j = 16, 23 do tb += bi[j] * 2 ^ (j-16)     
    r[i] = tr
    g[i] = tg
    b[i] = tb     
  endfor

end

;+
; :Description:
;    To generate an ASCII template from an ascci file.
;
; :Keywords:
;    FILE: in, optional
;          the ascii file to template (if not set, a dialog window will open)
;
; :History:
;     Written by FaM, 2010.
;     
;-
pro utils_make_template, FILE = file

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc

  if ~KEYWORD_SET(file) then file = DIALOG_PICKFILE(FILTER='*.dat', TITLE='Please select data file to template', /FIX_FILTER, /MUST_EXIST)
  
  template = ASCII_TEMPLATE(file)

  GEN_str_subst, ret, file, '.dat', '.tpl', tpl_file_path
  SAVE, template, filename=tpl_file_path
  ok = DIALOG_MESSAGE('File: ' + tpl_file_path + ' saved')
  
end

;+
; :Description:
;    Compares two {TNT_ELLIPSOID} structuress
;
; :Params:
;    ell1: in, required
;           the first ell
;    ell2: in, required
;           the second ell
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the ell are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_ellipsoid, ell1, ell2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(ell1, STRUCT={TNT_ELLIPSOID}) then return, 0
  if not arg_okay(ell2, STRUCT={TNT_ELLIPSOID}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(ell1.name) ne  str_equiv(ell2.name)  then return, 0 
  if ABS(ell1.a - ell2.a) gt epsilon then return, 0 
  if ABS(ell1.b - ell2.b) gt epsilon then return, 0 
            
  return, 1
    
end

;+
; :Description:
;    Compares two {TNT_DATUM} structuress
;
; :Params:
;    dat1: in, required
;           the first dat
;    dat2: in, required
;           the second dat
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the dat are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_datum, dat1, dat2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(dat1, STRUCT={TNT_DATUM}) then return, 0
  if not arg_okay(dat2, STRUCT={TNT_DATUM}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(dat1.name) ne  str_equiv(dat2.name)  then return, 0 
  if ~ utils_compare_ellipsoid(dat1.ellipsoid, dat1.ellipsoid, epsilon = epsilon) then return, 0 
  if ABS(dat1.dx - dat2.dx) gt epsilon then return, 0 
  if ABS(dat1.dy - dat2.dy) gt epsilon then return, 0 
  if ABS(dat1.dz - dat2.dz) gt epsilon then return, 0 
            
  return, 1
    
end

;+
; :Description:
;    Compares two {TNT_PROJ} structuress
;
; :Params:
;    proj1: in, required
;           the first proj
;    proj2: in, required
;           the second proj
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the proj are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_proj, proj1, proj2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
  
  if not arg_okay(proj1, STRUCT={TNT_PROJ}) then return, 0
  if not arg_okay(proj2, STRUCT={TNT_PROJ}) then return, 0
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if str_equiv(proj1.name) ne  str_equiv(proj2.name)  then return, 0 
  if ~ utils_compare_datum(proj1.datum, proj2.datum, epsilon = epsilon) then return, 0 
  if proj1.idx ne proj2.idx then return, 0 
  if ABS(proj1.a - proj2.a) gt epsilon then return, 0 
  if ABS(proj1.b - proj2.b) gt epsilon then return, 0 
  if ABS(proj1.azi - proj2.azi) gt epsilon then return, 0 
  if total(ABS(proj1.lat - proj2.lat)) gt 3*epsilon then return, 0 
  if total(ABS(proj1.lon - proj2.lon)) gt 3*epsilon then return, 0 
  if total(ABS(proj1.xy - proj2.xy)) gt 2*epsilon then return, 0 
  if ABS(proj1.h - proj2.h) gt epsilon then return, 0 
  if total(ABS(proj1.sp - proj2.sp)) gt 2*epsilon then return, 0 
  if proj1.zone ne proj2.zone then return, 0 
  if proj1.flag ne proj2.flag then return, 0 
  if ABS(proj1.k0 - proj2.k0) gt epsilon then return, 0 
  if ABS(proj1.rot - proj2.rot) gt epsilon then return, 0 
  if total(ABS(proj1.shp - proj2.shp)) gt 2*epsilon then return, 0 
  if ABS(proj1.ang - proj2.ang) gt epsilon then return, 0   
  if total(ABS(proj1.som - proj2.som)) gt 2*epsilon then return, 0 
  if total(ABS(proj1.sat - proj2.sat)) gt 2*epsilon then return, 0 
  if str_equiv(proj1.envi) ne  str_equiv(proj2.envi)  then return, 0 
          
  return, 1
    
end

;+
; :Description:
;    Compares two w_grid_2d objects
;
; :Params:
;    grid1: in, required
;           the first grid
;    grid2: in, required
;           the second grid
;
; :Keywords:
;    epsilon: in, optional, default = (MACHAR(DOUBLE=1)).eps
;             the tolerance
;  
;  :Returns:
;    1 if the grids are the same, 0 if not
;  
;  
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_compare_grid, grid1, grid2, epsilon = epsilon
    
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg)
    RETURN, 0
  ENDIF
    
  if ~ OBJ_VALID(grid1) then return, 0
  if ~ OBJ_ISA(grid1, 'w_Grid2D') then  return, 0
  if ~ OBJ_VALID(grid2) then return, 0
  if ~ OBJ_ISA(grid2, 'w_Grid2D') then  return, 0
  
  grid1->GetProperty, TNT_C=c1
  grid2->GetProperty, TNT_C=c2
  
  if N_ELEMENTS(epsilon) eq 0 then epsilon = (MACHAR(DOUBLE=1)).eps
  
  if ABS(c1.nx - c2.nx) gt 0 then return, 0
  if ABS(c1.ny - c2.ny) gt 0 then return, 0
  if ABS(c1.dx - c2.dx) gt epsilon then return, 0
  if ABS(c1.dy - c2.dy) gt epsilon then return, 0
  if ABS(c1.x0 - c2.x0) gt epsilon then return, 0
  if ABS(c1.y0 - c2.y0) gt epsilon then return, 0
  if ABS(c1.y1 - c2.y1) gt epsilon then return, 0
  if ABS(c1.x1 - c2.x1) gt epsilon then return, 0
  if ~ utils_compare_proj(c1.proj, c2.proj, epsilon = epsilon)  then return, 0 
        
  return, 1
    
end


;+
; :Description:
;    This functions combines one or more grids to make the "biggest possible"
;    grid with it.
;
; :Params:
;    grids: in, required
;           an array of grid objects
;
; :Returns:
;    a w_grid_2d object, mosaic of the input grids.
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_mosaic_grid, grids


  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  ON_ERROR,2
  
  if N_ELEMENTS(grids) eq 0 then MESSAGE, WAVE_Std_Message(/NARG)
  if ~ OBJ_VALID(grids[0]) then MESSAGE, WAVE_Std_Message('grids', /ARG)
  if ~ OBJ_ISA(grids[0], 'w_Grid2D') then MESSAGE, WAVE_Std_Message('grids', /ARG)
  
  (grids[0])->getProperty, tnt_c = c  , meta = meta
  x0 = c.x0
  y0 = c.y0
  x1 = c.x1
  y1 = c.y1
  dx = c.dx
  dy = c.dy
  proj = c.proj
  
  for i = 1, N_ELEMENTS(grids)-1 do begin
    if ~ OBJ_VALID(grids[i]) then MESSAGE, WAVE_Std_Message('grids', /ARG)
    if ~ OBJ_ISA(grids[i], 'w_Grid2D') then MESSAGE, WAVE_Std_Message('grids', /ARG)
    (grids[i])->getProperty, tnt_c = c  
    if ~ utils_compare_proj(proj, c.proj) then MESSAGE, 'Projections do not match.'
    if ABS(dx - c.dx) gt (MACHAR()).eps then MESSAGE, 'Dxs do not match.'
    if ABS(dy - c.dy) gt (MACHAR()).eps then MESSAGE, 'Dys do not match.'
    x0 = min([x0,c.x0]) 
    y0 = max([y0,c.y0])
    x1 = max([x1,c.x1])
    y1 = min([y1,c.y1])    
  endfor
  
  return, OBJ_NEW('w_Grid2D', x0=x0, y0=y0, x1=x1, y1=y1, dx=dx, dy=dy, PROJ=proj, META=meta + ' mosaic')
      
end


;+
; :Description:
;    Calculates relative humidity from ARW WRF model output. 
;    You should not call it by wourself but use `w_WRF::get_Var('rh')` instead.
;    
;    Check out the equvalent NCL function 
;    ‘wrf_rh <http://www.ncl.ucar.edu/Document/Functions/Built-in/wrf_rh.shtml>‘
;    
; :Params:
;    qv: in, required
;        Water vapor mixing ratio in [kg/kg].
;    p: in, required
;        Full pressure (perturbation + base state pressure) with the same dimension as qv. Units must be [Pa]. 
;    t: in, required
;        Temperature in [K] with the same dimension as qv. This variable can be calculated with `utils_wrf_tk`. 
;
;  :Returns:
;    Relative humidity [%]
;
; :History:
;     Written by FaM, 2010.
;-
function utils_wrf_rh, qv, p, t
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not array_processing(qv, p, t) then message, WAVE_Std_Message(/ARG)

  SVP1=0.6112D
  SVP2=17.67D
  SVP3=29.65D
  SVPT0=273.15D
  
  R_D=287.04D
  R_V=461.6D
  EP_2=R_D/R_V
  
  EP_3=0.622D
  
  PRESSURE = P
  TEMPERATURE = T
  
  ES = 10.D*SVP1*EXP(SVP2* (TEMPERATURE-SVPT0)/(TEMPERATURE-SVP3))
  
  QVS = EP_3 * ES / (0.01D * PRESSURE - (1.D - EP_3)*ES)
  
  RH = 100.D*((QV/QVS < 1.0D) > 0.0D)
  
  RETURN, rh
  
END

;+
; :Description:
;    Calculates temperature in [K] from ARW WRF model output. 
;    You should not call it by wourself but use `w_WRF::get_Var('tk')` instead.
;        
;    Check out the equvalent NCL function 
;    ‘wrf_tk <http://www.ncl.ucar.edu/Document/Functions/Built-in/wrf_tk.shtml>‘
;    
; :Params:
;    p: in, required
;        Full pressure (perturbation + base state pressure). Units must be [Pa]. 
;    theta: in, required
;           Potential temperature (i.e, perturbation + reference temperature) 
;           with the same dimension as p. Units must be [K]. 
;
;  :Returns:
;    Temperature in [K]. The multi-dimensional array has the same size as p.
;
; :History:
;     Written by FaM, 2010.
;-
function utils_wrf_tk, p, theta

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not array_processing(p, theta) then message, WAVE_Std_Message(/ARG)
  
  P1000MB=100000.
  R_D=287.04
  CP=7.*R_D/2.
  
  PI = (p/P1000MB)^(R_D/CP)
  return, PI*theta
  
end

;+
; :Description:
;    Calculates sea level pressure (hPa) from ARW WRF model output.
;    You should not call it by wourself but use `w_WRF::get_Var('slp')` instead.
;    
;    Check out the equvalent NCL function 
;    ‘wrf_slp <http://www.ncl.ucar.edu/Document/Functions/Built-in/wrf_slp.shtml>‘
;    
; :Params:
;    Z: in, required    
;       Geopotential height in [m] with at least 3 dimensions. It must be on the ARW WRF unstaggered grid. 
;    
;    T: in, required    
;       Temperature in [K]. An array with the same dimensionality as Z. This variable can be calculated by wrf_tk.
;    
;    P: in, required    
;       Full pressure (perturbation + base state pressure) in [Pa]. An array of the same dimensionality as Z.
;    
;    Q: in, required    
;        Water vapor mixing ratio in [kg/kg]. An array of the same dimensionality as Z. 
;
;  :Returns:
;    Sea level pressure in [hPa]. (2-dimensional surface array)
;
; :History:
;     Written by FaM, 2010.
;-
function utils_wrf_slp, Z, T, P, Q

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not array_processing(Z, T, P, Q) then message, WAVE_Std_Message(/ARG)
  dims = size(z, /DIMENSIONS)
  ndims = N_ELEMENTS(dims)
  if ndims ne 3 then message, WAVE_Std_Message(DIMARRAY = 3)
  nx = dims[0]
  ny = dims[1]
  nz = dims[2]
  
  R=287.04
  G=9.81
  GAMMA=0.0065
  TC=273.16 + 17.5
  PCONST=10000.
  
  ;c  Find least zeta level that is PCONST Pa above the surface.  We
  ;c  later use this level to extrapolate a surface pressure and
  ;c  temperature, which is supposed to reduce the effect of the diurnal
  ;c  heating cycle in the pressure field.
  
  level = INTARR(nx,ny) - 1
  for k=0, nz-1 do begin
    pok = where((P[*,*,k] lt (P[*,*,0] - PCONST)) and level eq -1, cnt)
    if cnt ne 0 then level[pok] = k
  endfor
  
  pnok = where(level eq -1, cnt)
  if cnt ne 0 then message, 'Error_in_finding_100_hPa_up'
  
  
  KLO = (LEVEL-1) > 0
  KHI = (KLO+1) < (nz-1)
  
  pnok = where((KLO - KHI) eq 0, cnt)
  if cnt ne 0 then message, 'Trapping levels are weird.'
  
  PLO = FLTARR(nx,ny)
  PHI = FLTARR(nx,ny)
  THI = FLTARR(nx,ny)
  TLO = FLTARR(nx,ny)
  QHI = FLTARR(nx,ny)
  QLO = FLTARR(nx,ny)
  ZHI = FLTARR(nx,ny)
  ZLO = FLTARR(nx,ny)
  
  nn = nx*ny
  inds = indgen(nn)
  indsO = inds + klo[*] * nn
  indsI = inds + khi[*] * nn
  
  PLO[*,*] = P[indsO]
  PHI[*,*] = P[indsI]
  
  ZLO[*,*] = Z[indsO]
  ZHI[*,*] = Z[indsI]
  
  TLO[*,*] = T[indsO]
  THI[*,*] = T[indsI]
  
  QLO[*,*] = Q[indsO]
  QHI[*,*] = Q[indsI]
  
  TLO = TLO* (1.+0.608*QLO)
  THI = THI* (1.+0.608*QHI)
  
  P_AT_PCONST = P[*,*,0] - PCONST
  T_AT_PCONST = THI - (THI-TLO)*ALOG(P_AT_PCONST/PHI)* ALOG(PLO/PHI)
  Z_AT_PCONST = ZHI - (ZHI-ZLO)*ALOG(P_AT_PCONST/PHI)*ALOG(PLO/PHI)
  T_SURF = T_AT_PCONST* (P[*,*,0]/P_AT_PCONST)^(GAMMA*R/G)
  T_SEA_LEVEL = T_AT_PCONST + GAMMA*Z_AT_PCONST
  
  
  ;c If we follow a traditional computation, there is a correction to the
  ;c sea level temperature if both the surface and sea level
  ;c temperatures are *too* hot.  
  L1 = T_SEA_LEVEL LT TC
  L2 = T_SURF LE TC
  L3 = ~L1
  T_SEA_LEVEL =  TC - 0.005 * (T_SURF-TC)^2
  pok = where(L2 AND L3, cnt)
  if cnt ne 0 then T_SEA_LEVEL[pok] = TC
  
  ;c     The grand finale: ta da!
  ;c   z_half_lowest=zetahalf(1)/ztop*(ztop-terrain(i,j))+terrain(i,j)
  Z_HALF_LOWEST = Z[*,*,0]
  
  ;C Convert to hPa in this step, by multiplying by 0.01. The original
  ;C Fortran routine didn't do this, but the NCL script that called it
  ;C did, so we moved it here.
  SEA_LEVEL_PRESSURE = 0.01 * (P[*,*,0]*EXP((2.*G*Z_HALF_LOWEST)/(R* (T_SEA_LEVEL+T_SURF))))
  
  return, SEA_LEVEL_PRESSURE
  
end

function utils_wrf_td, p, qvapor

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not array_processing(p, qvapor) then message, WAVE_Std_Message(/ARG)
  
  qv = qvapor > 0
  p = p*0.01
  
  ; vapor pressure
  tdc = qv * p / (0.622 + qv)
  
  ;avoid problems near zero
  tdc = tdc > 0.001
  
  return,  (243.5*alog(tdc)-440.8)/ (19.48-alog(tdc))
  
end

;+
; :Description:
;    Unstaggers an input variable along a specified dimension.
;    You should not call it by wourself but use `w_WRF::get_Var('', /UNSTAGGER)` instead.  
;
; :Params:
;    varin: in, required
;          Variable that needs to be unstaggered. Must be at least a 2 dimensional variable 
;          with the dimensions nx x ny, or a 3 dimensional variable with  
;          dimensions nx x ny x nz.
;          
;    unstagDim: in, required
;               Along which dimension must the variable be unstaggered. 
;               Options are 0 (X), 1 (Y) or 2 (Z). 
;
;
; :Returns:
;    The unstaggered variable
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_wrf_unstagger, varin, unstagDim
  
  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if not arg_okay(varin, /NUMERIC) then message, WAVE_Std_Message(/ARG)
  if not arg_okay(unstagDim, /NUMERIC, /SCALAR) then message, WAVE_Std_Message(/ARG)
  
  dims = SIZE(varin, /DIMENSIONS)
  nd = N_ELEMENTS(dims)
  if unstagDim ge nd or unstagDim lt 0 then message, WAVE_Std_Message('unstagDim', /RANGE)
  
  n = dims[unstagDim]
  case (unstagDim) of
    0: varout = 0.5*(varin[0:n-2,*,*,*,*] + varin[1:n-1,*,*,*,*])
    1: varout = 0.5*(varin[*,0:n-2,*,*,*] + varin[*,1:n-1,*,*,*])
    2: varout = 0.5*(varin[*,*,0:n-2,*,*] + varin[*,*,1:n-1,*,*])    
    else: Message, 'You sure?'       
  endcase

 return, reform(varout)

end

function utils_wrf_intrp3d, varin, z_in, loc_param, EXTRAPOLATE=extrapolate

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  ON_ERROR, 2
  
  if not arg_okay(varin, /NUMERIC) then message, WAVE_Std_Message(/ARG)
  if not arg_okay(z_in, /NUMERIC) then message, WAVE_Std_Message(/ARG)
  if not array_processing(varin, z_in) then message, WAVE_Std_Message(/ARG)  
  if not arg_okay(loc_param, /NUMERIC) then message, WAVE_Std_Message(/ARG)
  
  dims = SIZE(reform(varin), /DIMENSIONS)
  nd = N_ELEMENTS(dims) 
  
  _extra = ~KEYWORD_SET(EXTRAPOLATE) 
  
  nlocs = N_ELEMENTS(loc_param)
  
  if nd eq 3 then begin
    out_var = FLTARR(dims[0], dims[1], nlocs)
    for i=0, dims[0]-1 do begin
      for j=0, dims[1]-1 do begin
        _z_in = z_in[i,j,*]
        out_var[i,j,*] = INTERPOL(varin[i,j,*],_z_in,loc_param)
        if _EXTRA then begin
          p = where(loc_param gt max(_z_in) or loc_param lt min(_z_in), cnt)
          if cnt ne 0 then  out_var[i,j,p] = !VALUES.F_NAN
        endif
      endfor
    endfor
  endif else if nd eq 4 then begin
    out_var = FLTARR(dims[0], dims[1], nlocs, dims[3])
    for t=0, dims[3]-1 do out_var[*,*,*,t] = utils_wrf_intrp3d(REFORM(varin[*,*,*,t]), REFORM(z_in[*,*,*,t]), loc_param, EXTRAPOLATE=extrapolate)
  endif else Message, WAVE_Std_Message('varIn', /ARG)
  
  return, out_var
  
end

;+
; :Description:
;    Computes the mean deviation between two datasets
;
; :Params:
;    ref: in, required
;         the reference data
;    data: in, required
;         the data to compare to the reference
;
; :Returns:
;    The mean deviation (scalar)
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_MD, ref, data

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc
  
  if ~ array_processing(ref, data) then message, 'ref and data not compatible'
    
  return, mean(data - ref, /NAN)

end

;+
; :Description:
;    Computes the root mean square deviation between two datasets
;
; :Params:
;    ref: in, required
;         the reference data
;    data: in, required
;         the data to compare to the reference
;
; :Returns:
;    The  root mean square deviation (scalar)
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_RMSD, ref, data

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc  

  if ~ array_processing(ref, data) then message, 'ref and data not compatible'
    
  diff = data - ref
  return, SQRT(mean(diff*diff, /NAN))

end

;+
; :Description:
;    Computes the explained variance between two datasets
;
; :Params:
;    ref: in, required
;         the reference data
;    data: in, required
;         the data to compare to the reference
;
; :Returns:
;    The explained variance (scalar)
;
; :History:
;     Written by FaM, 2011.
;
;
;-
function utils_R2, ref, data

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc  

  if ~ array_processing(ref, data) then message, 'ref and data not compatible'
 
  return, CORRELATE(ref, data)^2 

end

; Copyright (c) 1999, Forschungszentrum Juelich GmbH ICG-1
; All rights reserved.
; Unauthorized reproduction prohibited.
; This software may be used, copied, or redistributed as long as it is not
; sold and this copyright notice is reproduced on each copy made.  This
; routine is provided as is without any express or implied warranties
; whatsoever.
;
;+
; :Description:
;    Test if directory exist
;
; :Params:
;    path: The variable to be tested.
;
;
;
; :Returns:
;     This function returns 1 if directory exist else it returns 0
; :History:
;     Written by:     R.Bauer , 26.01.99
;
;-
function utils_is_dir, path

   errvar=0
   CATCH,errvar
   IF errvar NE 0 THEN RETURN,0
   CD,curr=curr,path
   CD,curr
   RETURN,1
  
end


FUNCTION utils_AVERAGE, ARRAY, DIMENSION, MISSING=MISSING, DOUBLE=double, NSIG = nsig

  ON_ERROR,2
  ;
  ;  Check the input parameters.
  ;
  IF N_PARAMS() LT 1 THEN MESSAGE,  $
    'Syntax:  Result = AVERAGE( ARRAY  [, DIMENSION ] )'
  IF N_ELEMENTS(ARRAY) EQ 0 THEN MESSAGE, $
    'ARRAY not defined'
  ;
  ;  Dimension not passed.  Return a simple average.  If the keyword MISSING was
  ;  sent, then only average together the points not equal to the missing value.
  ;  If there are no non-missing pixels, then return the missing value.
  ;
  IF N_PARAMS(0) EQ 1 THEN BEGIN
    IF N_ELEMENTS(MISSING) EQ 1 THEN BEGIN
      W = WHERE(ARRAY NE MISSING, COUNT)
      IF COUNT GT 0 THEN BEGIN
        AVER = TOTAL(ARRAY(W), DOUBLE=double) / COUNT
        NSIG = COUNT
      ENDIF ELSE BEGIN
        AVER = MISSING
        NSIG = 0
      ENDELSE        
    END ELSE BEGIN
     AVER = TOTAL(ARRAY, DOUBLE=double) / N_ELEMENTS(ARRAY)
     NSIG = N_ELEMENTS(ARRAY)
    ENDELSE
  ;
  ;  Dimension passed.  Check DIMENSION, and make sure that ARRAY is an array.
  ;
  END ELSE BEGIN
    IF N_ELEMENTS(DIMENSION) EQ 0 THEN BEGIN
      MESSAGE,'DIMENSION not defined'
    END ELSE IF N_ELEMENTS(DIMENSION) NE 1 THEN BEGIN
      MESSAGE,'DIMENSION must be a scalar'
    ENDIF
    S = SIZE(ARRAY)
    IF S(0) EQ 0 THEN MESSAGE,'ARRAY must be an array'
    ;
    ;  Return an array collapsed along one of the dimensions.  If the keyword
    ;  MISSING was passed, then
    ;
    IF (DIMENSION GE 1) AND (DIMENSION LE S(0)) THEN BEGIN
      IF N_ELEMENTS(MISSING) EQ 1 THEN BEGIN
        ;
        ;  Start by calculating the numerator, substituting 0 where-ever the missing
        ;  pixel flag is seen.
        ;
        AVER = ARRAY
        W = WHERE(ARRAY EQ MISSING, COUNT)
        IF COUNT GT 0 THEN AVER(W) = 0
        AVER  = TOTAL(AVER, DIMENSION, DOUBLE=double)
        ;
        ;  Next calculate the denominator as the total number of points which are good.
        ;  Substitute the MISSING pixel value where-ever there are no good pixels to
        ;  average together.
        ;
        DENOM = TOTAL(ARRAY NE MISSING, DIMENSION, DOUBLE=double)
        AVER = TEMPORARY(AVER) / (DENOM > 1)
        W = WHERE(DENOM EQ 0, COUNT)
        IF COUNT GT 0 THEN AVER(W) = MISSING
        NSIG = LONG(TEMPORARY(DENOM))
        
      ;
      ;  Otherwise, simply divide the total by the number of pixels along that
      ;  dimension.
      ;
      END ELSE BEGIN 
       AVER = TOTAL(ARRAY,DIMENSION, DOUBLE=double) / S(DIMENSION)
       NSIG = AVER * 0 + S(DIMENSION)
      ENDELSE
    END ELSE BEGIN
      MESSAGE,'Dimension out of range'
    ENDELSE
  ENDELSE
  ;

  RETURN, AVER
END

;+
; Project     : SOHO - CDS
;
; Name        : SIG_ARRAY
;
; Purpose     : Returns the standard deviation of an array.
;
; Category    : Class3, Numerical, Error-analysis, Statistics
;
; Explanation : Calculate the standard deviation value of an array, or over one
;   dimension of an array as a function of all the other
;   dimensions.
;
; Syntax      : Result = SIG_ARRAY( ARRAY  [, DIMENSION] )
;
; Examples    :
;
; Inputs      : ARRAY   = The array to determine the standard deviation from.
;
; Opt. Inputs : DIMENSION = The dimension to calculate the standard deviation
;         over.
;
; Outputs     : The result of the function is the standard deviation value of
;   the array when called with one parameter.
;
;   If DIMENSION is passed, then the result is an array with all
;   the dimensions of the input array except for the dimension
;   specified, each element of which is the standard deviation of
;   the corresponding vector in the input array.
;
;   For example, if A is an array with dimensions of (3,4,5), then
;   the command:
;
;     B = SIG_ARRAY(A,2)
;
;   is equivalent to
;
;     B = FLTARR(3,5)
;     FOR J = 0,4 DO BEGIN
;       FOR I = 0,2 DO BEGIN
;       B(I,J) = SIG_ARRAY(A(I,*,J), N)
;       ENDFOR
;     ENDFOR
;
; Opt. Outputs: None.
;
; Keywords    : MISSING = Value signifying missing pixels.  Any pixels with
;       this value are not included in the calculation.  If
;       there are no non-missing pixels, then MISSING is
;       returned.
;
;   N_PAR = The number of fitted parameters to take into account
;       when determining the standard deviation.  The default
;       value is one.  The number of degrees of freedom is
;       N_ELEMENTS(ARRAY) - N_PAR.  The value of SIG_ARRAY
;       varies as one over the square root of the number of
;       degrees of freedom.
;
; Calls       : AVERAGE
;
; Common      : None.
;
; Restrictions: None.
;
; Side effects: None.
;
; Prev. Hist. : Based on an earlier routine called SIGMA by W. Thompson, 1986.
;
; History     : Version 1, 26-Mar-1996, William Thompson, GSFC
;   Version 2, 26-Feb-1997, William Thompson, GSFC
;     Corrected problem with roundoff error when the
;     distribution width is small compared to the data.
;   Version 3, 25-Feb-1997, William Thompson, GSFC
;     Make sure that one doesn't try to take square root of a
;     negative number due to roundoff error.
;   Version 4, 11-Apr-1998, William Thompson, GSFC
;     Corrected bug involving incorrect application of NPAR
;     adjustment.
;   Version 5, 25-Sep-1998, William Thompson, GSFC
;     Improved way that round-off error is handled when the
;     DIMENSION parameter is used.  Rather than normalizing
;     to a single average over the array, a separate average
;     is calculated for each pixel of the reduced array.
;
; Contact     : WTHOMPSON
;-
FUNCTION utils_SIG_ARRAY,ARRAY,DIMENSION,N_PAR=N_PAR,MISSING=MISSING

  ON_ERROR,2                      ;Return to caller if an error occurs
  ;
  ;  Check the number of parameters passed.
  ;
  IF N_PARAMS() EQ 0 THEN MESSAGE,  $
    'Syntax:  Result = SIG_ARRAY( ARRAY  [, DIMENSION ] )
  ;
  ;  Get the number of dimensions of the array.
  ;
  S = SIZE(ARRAY)
  IF S(0) EQ 0 THEN MESSAGE, 'Variable ARRAY must be an array'
  ;
  ;  If the DIMENSION parameter was passed, then check it for validity.
  ;  over that dimension.
  ;
  IF N_PARAMS() EQ 2 THEN BEGIN
    IF ((DIMENSION LT 1) OR (DIMENSION GT S(0))) THEN MESSAGE, $
      'DIMENSION out of range'
  ENDIF
  ;
  ;  Renormalize the data to the average value.  This avoids problems with
  ;  roundoff error.
  ;
  IF (N_PARAMS() EQ 2) AND (S(0) GT 1) THEN BEGIN
    STEMP = LONARR(3)
    IF DIMENSION EQ 1 THEN STEMP(0) = 1 ELSE  $
      STEMP(0) = PRODUCT(S(1:DIMENSION-1))
    STEMP(1) = S(DIMENSION)
    IF DIMENSION EQ S(0) THEN STEMP(2) = 1 ELSE $
      STEMP(2) = PRODUCT(S(DIMENSION+1:S(0)))
    TEMP = REFORM(ARRAY, STEMP)
    A0 = utils_AVERAGE(ARRAY, DIMENSION, MISSING=MISSING)
    FOR I = 0,S(DIMENSION)-1 DO TEMP(*,I,*) = TEMP(*,I,*) - A0
    TEMP = REFORM(TEMP, S(1:S(0)), /OVERWRITE)
  END ELSE BEGIN
    A0 = utils_AVERAGE(ARRAY, MISSING=MISSING)
    TEMP = ARRAY - A0
  ENDELSE
  ;
  ;  Change the missing value to reflect the renormalized data.
  ;
  IF N_ELEMENTS(MISSING) EQ 1 THEN BEGIN
    W = WHERE(ARRAY NE MISSING, COUNT)
    IF COUNT GT 0 THEN BEGIN
      TMISSING = MIN(ARRAY(W))
      IF TMISSING LT 0 THEN TMISSING = 1.1*TMISSING
      IF TMISSING GT 0 THEN TMISSING = 0.9*TMISSING
      IF TMISSING EQ 0 THEN TMISSING = -1
      W = WHERE(ARRAY EQ MISSING, COUNT)
      IF COUNT GT 0 THEN TEMP(W) = TMISSING
    END ELSE TMISSING = TEMP(0)   ;All pixels are missing
  ENDIF
  ;
  ;  Form the square of the array, taking into account any missing pixels.
  ;
  A_SQR = TEMP^2
  IF N_ELEMENTS(TMISSING) EQ 1 THEN BEGIN
    W = WHERE(TEMP EQ TMISSING, COUNT)
    IF COUNT GT 0 THEN A_SQR(W) = TMISSING
  ENDIF
  ;
  ;  Calculate the average of the array and of the square of the array.
  ;
  IF N_PARAMS() EQ 2 THEN BEGIN
    A_AVG = utils_AVERAGE(TEMP, DIMENSION, MISSING=TMISSING)
    A_SQR = utils_AVERAGE(A_SQR, DIMENSION, MISSING=TMISSING)
    N = S(DIMENSION)
  END ELSE BEGIN
    A_AVG = 0
    A_SQR = utils_AVERAGE(A_SQR, MISSING=TMISSING)
    N = N_ELEMENTS(TEMP)
  ENDELSE
  ;
  ;  Take into account the number of free parameters.
  ;
  IF N_ELEMENTS(N_PAR) EQ 1 THEN NPAR = N_PAR ELSE NPAR = 1
  SIG = SQRT(ABS(A_SQR - A_AVG^2) * (N / ((N - NPAR) > 1.)))
  ;
  ;  Set any missing pixels to the missing pixel flag value.
  ;
  IF N_ELEMENTS(TMISSING) EQ 1 THEN BEGIN
    W = WHERE(A_AVG EQ TMISSING, COUNT)
    IF COUNT GT 0 THEN SIG(W) = MISSING
  ENDIF
  ;
  RETURN, SIG
END


function utils_Random_Indices, len, n_in
  swap = n_in gt len/2
  IF swap THEN n = len-n_in ELSE n = n_in
  inds = LonArr(n, /NOZERO)
  M = n
  WHILE n GT 0 DO BEGIN
    inds[M-n] = Long( RandomU(seed, n)*len )
    inds = inds[Sort(inds)]
    u = Uniq(inds)
    n = M-n_elements(u)
    inds[0] = inds[u]
  ENDWHILE
  
  IF swap THEN inds = Where(Histogram(inds,MIN=0,MAX=len-1) EQ 0)
  RETURN, inds
end

FUNCTION utils_RandPerm, numberOfElements, SEED=seed
  x = Lindgen(numberOfElements)
  RETURN, x[Sort(Randomu(seed, numberOfElements))]
END

function utils_chunk_sum_d3, x

  compile_opt idl2
  forward_function utils_chunk_sum_d3
  
  np = 50
  siz = size(x)
  if siz[0] ne 3 then message, 'Dim 3 !'
  nt = siz[3]
  nchunk = nt / np
  
  sums = dblarr(siz[1],siz[2], nchunk+1)
  for j=0, nchunk-1 do sums[*,*,j] = total(x[*,*,j*np:(j+1)*np-1], 3, /double)
  left = nt - nchunk * np
  if left gt 0 then sums[*,*,nchunk] = total(x[*,*,nchunk*np:*], 3, /double)
  if nchunk gt np then s=utils_chunk_sum_d3(sums) else s=total(sums, 3)
  
  return, s
  
end


;+
; :Description:
;    Simple wrapper for the IDL built-in LABEL_REGION function, but
;    with the inclusion of border pixels to the labeling.
;
; :Params:
;    mask: in, required, type=boolean
;          the mask to label
;    entitites: out
;               array of longs of the same dim as mask with the entities
;    nentities: out
;               the number of entitites found
;
; :Keywords:
;    ALL_NEIGHBORS: set this keyword to compute regions
;                   with diagonal contact as well.
;
;
; :History:
;     Written by FaM, 2011.
;
;-
pro utils_label_entities, mask, entitites, nentities, ALL_NEIGHBORS=all_neighbors

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc  
  ON_ERROR, 2

  if ~arg_okay(mask, /NUMERIC, N_DIM=2) then message, WAVE_Std_Message('mask', /ARG)
   
  nx = N_ELEMENTS(mask[*,0])
  ny = N_ELEMENTS(mask[0,*])
  
  _mask = LONARR(nx+2,ny+2)
  _mask[1:nx,1:ny] = mask
  
  entitites = LABEL_REGION(_mask, ALL_NEIGHBORS=all_neighbors)
  entitites = entitites[1:nx,1:ny]  
  nentities = max(entitites)

end

function utils_minmax, val
  
  return, [min(val, /NAN), max(val, /NAN)]
  
end

function utils_neighbors, mask, cnt
  
  _mask = 0 > FLOAT(mask) < 1
  if TOTAL(utils_minmax(_mask) - [0,1]) ne 0 then message, 'you sure this is a correct mask?'
  
  sm = SMOOTH(_mask, 3)
  
  p = where(sm ne 0, cnt)
  if cnt eq 0 then return, -1  
  sm[p] = 1
    
  return, where((sm-_mask) eq 1, cnt)    
  
end

pro utils_array_el_num, array, elements, cnt, n_el, BY_OCCURENCE=by_occurence
   
  _array = array[SORT(array)]
  elements = _array[UNIQ(_array)]
  cnt = [-1, VALUE_LOCATE(_array, elements)] 
  cnt = cnt[1:*] - cnt[0:N_ELEMENTS(cnt)-2]  
  n_el = N_ELEMENTS(cnt)
  
  if KEYWORD_SET(BY_OCCURENCE) then begin
    s = REVERSE(sort(cnt))
    cnt = cnt[s]
    elements =  elements[s]
  end
  
end

