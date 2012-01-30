; docformat = 'rst'
;+
;  
; This is a beefed-up IDL_CONTAINER object written as a utility object
; for the w_TS_Data object and related objects. 
;
; :Categories:
;    Timeseries, Objects
;
; :History:
;    Modifications::
;     (c) David Fanning
;     FaM, 2012: Adapted to the WAVE
;
;-

;+
; :Description:
;    This method searches the IDL container object to find objects with a particular name.
;     If found, the object reference to that object is returned.
;
; :Params:
;    searchName: in, required
;                The string name of the object you are searching for in 
;                this container
;                
; :Keywords:
;    Case_Sensitive: in, optional
;                    Set this keyword to 1 to indicate a case-sensitive search. By default, the
;                    search is case-insensitive.
;    Count: in, optional
;           Set this keyword to a named variable that upon exit will contain the number
;           of objects returned that meet the searchName description. (Output)
;    RegExp: in, optional
;            Set this keyword to 1 to indicate the searchName is a regular expression.
;    _Extra: in, optional
;            Any keywords supported by STREGEX can also be used. Requires REGEXP to be set.
;
;-
function w_ts_Container::FindByName, searchName, $
                                     Case_Sensitive=case_sensitive, $
                                     Count=count, $
                                     RegExp=regexp, $
                                     _Extra=extra

   ; Return to caller on error.
   ON_ERROR, 2
   
   ; Assume there are no matches.
   count = 0

   ; Search name must be a scalar.
   IF N_Elements(searchName) NE 1 THEN Message,'Search expression must be a scalar string.'

   ; Get the names of all the child objects.
   children = self->IDL_CONTAINER::Get(/All, Count=numChildren)
   IF numChildren EQ 0 THEN RETURN, Obj_New()

   ; We assume we can get the NAME as a property.
   names = StrArr(numChildren)
   FOR childNo = 0L, numChildren - 1 DO BEGIN      
      names[childNo] = (children[childNo])->getProperty('NAME')
   ENDFOR

   ; Does the user want to evaluate a regular expression?
   ; If not, do a simple search for the search name.
   fold_case = Keyword_Set(case_sensitive) EQ 0
   IF Keyword_Set (regexp) THEN BEGIN
        mask = StRegex(names, searchName, FOLD_CASE=fold_case, /BOOLEAN, _Extra=extra)
   ENDIF ELSE BEGIN
        mask = StrMatch(names, searchName, FOLD_CASE=fold_case)
   ENDELSE

   ; Transform boolean array to index array. A side effect is that
   ; the count value will be set.
   matches = Where(mask, count)
   IF N_Elements(matches) EQ 1 THEN matches = matches[0]
   
   ; Get the matching objects.
   IF count GT 0 THEN BEGIN
        matchingObjects = self->Get(POSITION=matches)
   ENDIF ELSE matchingObjects = Obj_New()
   
   RETURN, matchingObjects

END

;+
;   Class definition module. 
;
; :Params:
;    class: out, optional, type=structure
;           class definition as a structure variable
;           
;-
pro w_ts_Container__DEFINE, class
    
    class = {w_ts_Container, INHERITS IDL_CONTAINER}
 
end