; docformat = 'rst'
;+
;
;  Central program file for the WAVE library. 
;  
;  It initializes the common block variables and defines the tools
;  for error handling within the WAVE.
;  
;  COMMON BLOCK VARIABLES::
;  
;   All TNT common blocks  
;   WAVE_cmn :
;     WAVE_root : path to the WAVE root directory
;     WAVE_resource_dir : path to the WAVE resource files directory
;     WAVE_output_dir : path to the default output directory for plots, etc.
;     D_QMS : number or quattro milliseconds in a day
;     H_QMS : number or quattro milliseconds in a hour
;     M_QMS : number or quattro milliseconds in a minute
;     S_QMS : number or quattro milliseconds in a second
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
;     Written by FaM, 2010.
;-

;+
; :Description:
; 
;    This procedure initializes the WAVE system.
;
;    Usually, you should not call this procedure yourself, unless you
;    really wish to reinitialize the TIME system.
;
; :Categories:
;    General/WAVE
;    
; :Private: 
;
; :History:
;       Written by FaM, 2009
;-
pro WAVE_init

  @WAVE.inc

  ;********************
  ; Check IDL release *
  ;********************

  if strmid(!version.release, 0, 1) lt 7 then begin
    print, 'WAVE library needs IDL 7.0 and higher to run !
    return
  endif
      
  
  ;*********************
  ; Determine WAVE_ROOT *
  ;*********************
  WAVE_root, WAVE_root

  ;*************************
  ; Resource file handling *
  ;*************************
  WAVE_resource_dir  = WAVE_root + 'res/'

  ;****************************
  ; Initialize WAVE libraries *
  ;****************************
  print, ''
  TIME_init
  w_graphics_init
  !ORDER = 0 
  window, /FREE, /PIXMAP
  if !D.WINDOW ge 0 then wdelete, !D.WINDOW
  
  print, ''
  print, '*******************'
  print, '* Ride the Wave ! *'
  print, '*******************'
  

  ;*******
  ; Exit *
  ;*******
  return

end

;+
; :Description:
;    This function intends to be a device independent error message handling.
;    It is inpired from the "Error_message.pro" code from David. W. Fanning
;
; :Categories:
;    General/WAVE   
;
; :Params:
;    theMessage: in, optional, type = string, default = !Error_State.Msg
;          The message to signalize.
;
; :Keywords:
;    WARNING: in, optional, type = boolean, default = 0
;          if the message is to handle as a warning, and not as an error
;    INFO: in, optional, type = boolean, default = 0
;          if the message is to handle as an info, and not as an error
;    noTRACEBACK: in, optional, type = boolean, default = 0
;          if set, no traceback of the error will be written
;    NONAME: in, optional, type = boolean, default = 0
;          prevent writing the origin of the error
;    TITLE: in, optional, type = string, default = 0
;          title of the dialog window if `talk` is set
;    TALK: in, optional, type = boolean, default = 0
;          if you want the error message to be popped up
;    _Extra: in, optional
;            all keywords accepted by `message`
;
; :Returns: 
;   'OK'
; 
; :History:
;       Written by FaM, 2010. Arranged from D. W. Fanning "Error_Message.pro")
;-
FUNCTION WAVE_Error_Message, theMessage   , $
    WARNING   = warning                   , $
    INFO   = info                         , $
    noTRACEBACK = notraceback             , $
    NONAME = noname                       , $
    TITLE = title                         , $ 
    TALK = talk                           , $ 
    _Extra=extra
    
   On_Error, 2
   
   ; Check for presence and type of message.
   
   IF N_Elements(theMessage) EQ 0 THEN theMessage = !Error_State.Msg
   s = Size(theMessage)
   messageType = s[s[0]+1]
   IF messageType NE 7 THEN BEGIN
      Message, "The message parameter must be a string.", _Extra=extra
   ENDIF
   if ~KEYWORD_SET(WARNING) and ~KEYWORD_SET(INFO) then error = 1
   if NOT KEYWORD_SET(NOTRACEBACK) then traceback = 1
   
   ; Get the call stack and the calling routine's name.
   Help, Calls=callStack
   callingRoutine = (StrSplit(StrCompress(callStack[1])," ", /Extract))[0]
   
   ; Are widgets supported?
   IF !D.Name EQ 'PS' THEN BEGIN
      widgetsSupported = 1
   ENDIF ELSE BEGIN
      widgetsSupported = ((!D.Flags AND 65536L) NE 0)
   ENDELSE

   ; Is the talk keyword set? If not, no dialogs.
   IF not Keyword_Set(TALK) THEN widgetsSupported = 0
   
   ; It is not enough to know if widgets are supported. In CRON jobs, widgets are
   ; supported, but there is no X connection and pop-up dialogs are not allowed.
   ; Here is a quick test to see if we can connect to a windowing system. If not,
   ; then we are going to assume widgets are not supported.
   Catch, theError
   IF theError NE 0 THEN BEGIN
      Catch, /CANCEL
      widgetsSupported = 0
      GOTO, testWidgetSupport
   ENDIF
   theWindow = !D.Window
   IF (!D.Flags AND 256) NE 0 THEN Window, /FREE, XSIZE=5, YSIZE=5, /PIXMAP
   Catch, /CANCEL
   
   testWidgetSupport: ; Come here if you choke on creating a window.
   IF !D.Window NE theWindow THEN BEGIN
      WDelete, !D.Window
      IF theWindow GE 0 THEN WSet, theWindow
   ENDIF
   
   IF widgetsSupported THEN BEGIN ; !!! We want a dialog window
   
      ; If this is an error produced with the MESSAGE command, it is a trapped
      ; error and will have the name "IDL_M_USER_ERR".
      IF !ERROR_STATE.NAME EQ "IDL_M_USER_ERR" THEN BEGIN
   
         IF N_Elements(title) EQ 0 THEN title = 'Trapped Error'
   
            ; If the message has the name of the calling routine in it,
            ; it should be stripped out. Can you find a colon in the string?
   
         ; Is the calling routine an object method? If so, special processing
         ; is required. Object methods will have two colons together.
         doublecolon = StrPos(theMessage, "::")
         IF doublecolon NE -1 THEN BEGIN
   
            prefix = StrMid(theMessage, 0, doublecolon+2)
            submessage = StrMid(theMessage, doublecolon+2)
            colon = StrPos(submessage, ":")
            IF colon NE -1 THEN BEGIN
   
               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.
               IF StrMid(theMessage, 0, colon+StrLen(prefix)) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1+StrLen(prefix))
            ENDIF
         ENDIF ELSE BEGIN
   
            colon = StrPos(theMessage, ":")
            IF colon NE -1 THEN BEGIN
   
               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.
               IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1)
            ENDIF
   
         ENDELSE
   
   
         ; Add the calling routine's name, unless NONAME is set.
         IF Keyword_Set(noname) THEN BEGIN
            answer = Dialog_Message(theMessage, Title=title, _Extra=extra, $
               Error=error, Information=warning)
         ENDIF ELSE BEGIN
            answer = Dialog_Message(StrUpCase(callingRoutine) + ": " + $
               theMessage, Title=title, _Extra=extra, $
               Error=error, Information=warning)
         ENDELSE
   
      ENDIF ELSE BEGIN
   
         ; Otherwise, this is an IDL system error.
         IF N_Elements(title) EQ 0 THEN title = 'System Error'
   
         IF StrUpCase(callingRoutine) EQ "$MAIN$" THEN $
            answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
               Error=error, Information=warning) ELSE $
         IF Keyword_Set(noname) THEN BEGIN
            answer = Dialog_Message(theMessage, _Extra=extra, Title=title, $
               Error=error, Information=warning)
         ENDIF ELSE BEGIN
            answer = Dialog_Message(StrUpCase(callingRoutine) + "--> " + $
               theMessage, _Extra=extra, Title=title, $
               Error=error, Information=warning)
         ENDELSE
      ENDELSE
      
   ENDIF ELSE BEGIN  ; !!! We do not want a dialog window
       
         ; If the message has the name of the calling routine in it,
         ; it should be stripped out. Can you find a colon in the string?
   
         ; Is the calling routine an object method? If so, special processing
         ; is required. Object methods will have two colons together.
         doublecolon = StrPos(theMessage, "::")
         IF doublecolon NE -1 THEN BEGIN
   
            prefix = StrMid(theMessage, 0, doublecolon+2)
            submessage = StrMid(theMessage, doublecolon+2)
            colon = StrPos(submessage, ":")
            IF colon NE -1 THEN BEGIN
   
               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.
               IF StrMid(theMessage, 0, colon+StrLen(prefix)) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1+StrLen(prefix))
            ENDIF
         ENDIF ELSE BEGIN
   
            colon = StrPos(theMessage, ":")
            IF colon NE -1 THEN BEGIN
   
               ; Extract the text up to the colon. Is this the same as
               ; the callingRoutine? If so, strip it.
               IF StrMid(theMessage, 0, colon) EQ callingRoutine THEN $
                  theMessage = StrMid(theMessage, colon+1)
            ENDIF
   
         ENDELSE
         Message, theMessage, /Continue, /NoPrint, /NoName, /NoPrefix, _Extra=extra
         text = ''
         if KEYWORD_SET(ERROR) then text += 'ERROR! '
         if KEYWORD_SET(WARNING) then text += 'WARNING! '
         if KEYWORD_SET(INFO) then text += 'INFO. '
         text += theMessage
         IF Keyword_Set(noname) then Print, text $
           ELSE Print, '%' + callingRoutine + ': ' + text   
         answer = 'OK'
   ENDELSE
      
   ; Provide traceback information if requested and this is NOT a warning message.
   IF ~Keyword_Set(NOTRACEBACK) AND Keyword_Set(error) THEN BEGIN
      Help, /Last_Message, Output=traceback
      Print,''
      Print, 'Traceback Report from ' + StrUpCase(callingRoutine) + ':'
      Print, ''
      FOR j=0,N_Elements(traceback)-1 DO Print, "     " + traceback[j]
   ENDIF
   
   RETURN, answer
END

;+
; :Description:
;    This function generates standard messages strings (usually for I/O error handling).
;
; :Params:
;    var: in, optional, type = string, default = 'unknown'
;
; :Keywords:
;    ARG: in, optional, type = boolean
;          "Argument type not correct" 
;    SCALAR: in, optional, type = boolean
;          "A scalar is expected." 
;    STRING: in, optional, type = boolean
;          "A string is expected." 
;    NUMERIC: in, optional, type = boolean
;          "A number is expected." 
;    DIMARRAY: in, optional, type = boolean
;          "An array of dim ' + str_equiv(dimarray) +' is expected." 
;    NELEMENTS: in, optional, type = int
;          "An array of ' + str_equiv(NELEMENTS) +' elements is expected." 
;    RANGE: in, optional, type = boolean
;          " is out of range" 
;    STRUCT: in, optional, type = struct
;          "A structure of type {' + tag_names(struct,/STRUCT) + '} is expected." 
;    OBJ: in, optional, type = boolean
;          "An instance of ' + str_equiv(OBJ) +' is expected." 
;    FILE: in, optional, type = boolean
;          "File doesnt exist or is not valid." 
;    NARG: in, optional, type = boolean
;          "Incorrect number of arguments." 
;    NDIMS: in, optional, type = integer
;          "Argument not correct. Unexpected number of dimensions: ' + str_equiv(NDIMS)"
;    
; :Returns: 
;   A string of the standard message.
; 
; :Examples: 
;   Some possible messages::
;   
;     IDL> print, WAVE_Std_Message('temp', /RANGE)
;     Argument not correct : $TEMP is out of range.
; 
;     IDL> print, WAVE_Std_Message('Grid', OBJ = 'w_Grid2D')
;     Argument not correct : $GRID. An instance of w_Grid2D is expected.
; 
;     IDL> print, WAVE_Std_Message('time', STRUCT={ABS_DATE})
;     Argument type not correct : $TIME. A structure of type {ABS_DATE} is expected.
;     
;     IDL> print, WAVE_Std_Message('subset', NELEMENTS = 4)
;     Argument not correct : $SUBSET. An array of 4 elements is expected.
;     
;     IDL> print, WAVE_Std_Message()
;     Error unknown or not specified. 
;     
; :History:
;       Written by FaM, 2010.
;-
function WAVE_Std_Message, var   , $
           ARG = arg       , $ 
           SCALAR = scalar       , $ 
           STRING = STRING , $ 
           NUMERIC = numeric , $
           DIMARRAY = dimarray , $
           NELEMENTS = nelements , $
           RANGE = range , $
           STRUCT = struct , $
           OBJ = OBJ , $
           FILE = file , $
           NDIMS = ndims , $
           NARG = narg
           
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  ; Standard error handling.
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /CANCEL
    void = WAVE_Error_Message()
    RETURN, 0
  ENDIF  
           
  if N_ELEMENTS(var) eq 0 then var = 'unknown'
  var = str_equiv(var)
  
  mess = 'Error unknown or not specified.'
  
  if KEYWORD_SET(ARG) then mess = 'Argument type not correct : $' + var + '.'
  if KEYWORD_SET(SCALAR) then mess = 'Argument type not correct : $' + var + '. A scalar is expected.'
  if KEYWORD_SET(NUMERIC) then mess = 'Argument type not correct : $' + var + '. A number is expected.'
  if KEYWORD_SET(STRING) then mess = 'Argument type not correct : $' + var + '. A string is expected.'
  if KEYWORD_SET(RANGE) then mess = 'Argument not correct : $' + var + ' is out of range.'
  if KEYWORD_SET(DIMARRAY) then mess = 'Argument not correct : $' + var + '. An array of dim ' + str_equiv(dimarray) +' is expected.'
  if KEYWORD_SET(NDIMS) then mess = 'Argument not correct : $' + var + '. Unexpected number of dimensions: ' + str_equiv(NDIMS)
  if KEYWORD_SET(NELEMENTS) then mess = 'Argument not correct : $' + var + '. An array of ' + str_equiv(NELEMENTS) +' elements is expected.'
  if KEYWORD_SET(OBJ) then mess = 'Argument not correct : $' + var + '. An instance of ' + str_equiv(OBJ) +' is expected.'
  if KEYWORD_SET(FILE) then mess = 'File doesnt exist or is not valid.'
  if KEYWORD_SET(STRUCT) then begin
    ok = var_info(struct) eq IDL_STRUCT
    if ok then sn = '. A structure of type {' + tag_names(struct,/STRUCT) + '} is expected.' else sn = '. A structure is expected.'
    mess = 'Argument type not correct : $' + var + sn
  end  
  if KEYWORD_SET(NARG) then mess = 'Incorrect number of arguments.'
  
  return, mess  

end
