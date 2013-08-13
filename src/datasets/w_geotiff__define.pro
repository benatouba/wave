; docformat = 'rst'
;+
;
;  w_GEOTIFF is a class to read geotiff files. 
;  It is rather a draft than something operational. It 
;  can only read automaticaly a few proj-types, the rest should
;  be implemented when needed. 
;
;
;-  

;+
; :Description:
;    Initialize the object instance
; 
; :Params:
;    file: in, optional
;          the path to the file to open
;    
;    grid: in, optional
;          the grid object
;    
; :Keywords:
;    _EXTRA: in, optional
;            any keyword accepted by `w_GISdata::defineSubset`
;            
;               
; :Returns: 
;    1 if the object is created successfully, 0 if not
;
;-
function w_GEOTIFF::init, file, grid, NO_DELTA=no_delta, _EXTRA=extra

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  Catch, theError
  IF theError NE 0 THEN BEGIN
    Catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    RETURN, 0
  ENDIF
  
  ; Check arguments
  if N_ELEMENTS(file) ne 1 then file = DIALOG_PICKFILE(TITLE='Please select tif file to read', /MUST_EXIST)
  if ~ FILE_TEST(file) then Message, 'File not found or not valid: ' + file
  if ~ QUERY_TIFF(file, info, GEOTIFF=geotiff) then Message, 'TIFF file not query-able'
  self.file = file
  self.geotiff = PTR_NEW(geotiff)
  self.info = PTR_NEW(info)
  
  if N_ELEMENTS(grid) ne 0 then begin
    ; User defined grid geoloc
    if ~ OBJ_VALID(grid) then Message, WAVE_Std_Message('grid', /ARG)
    if ~ OBJ_ISA(grid, 'w_Grid2d') then Message, WAVE_Std_Message('grid', /ARG)
  endif else begin
  
    ;Try to make geo-info alone (aaarg)
    nx = info.Dimensions[0]
    ny = info.Dimensions[1]
    
    ; Get the fields of the geotiff structure.
    fields = Tag_Names(geotiff)
    
    ; We can only handle raster images with projected coordinate systems, unless this is
    ; a GeoTiff file with Geographic model.
    gtModelIndex = Where(fields eq 'GTMODELTYPEGEOKEY', gtModelType)
    if gtModelType gt 0 then begin
      ; This is for LatLon projection grids
      dx = (geotiff.ModelPixelScaleTag)[0]
      dy = (geotiff.ModelPixelScaleTag)[1]
      
      ; Get the tie points (upper left + half pix).
      if KEYWORD_SET(NO_DELTA) then begin
        x0 = (geotiff.ModelTiePointTag)[3]
        y0 = (geotiff.ModelTiePointTag)[4]
      endif else begin
        x0 = (geotiff.ModelTiePointTag)[3] + dx/2
        y0 = (geotiff.ModelTiePointTag)[4] - dy/2
      endelse
      
      case (geotiff.gtModelTypeGeoKey) of
        1: begin
          d = Where(fields eq 'GEOGCITATIONGEOKEY', cnt)
          if cnt ne 0 then begin
            ; I need to know the datum. Currently WGS84 should be enough
            if geotiff.GEOGCITATIONGEOKEY ne 'GCS_WGS_1984' then Message, 'Projection unknown. Contact Fabi.'
            d = Where(fields eq 'GTCITATIONGEOKEY', cnt)
            if cnt ne 0 then begin
              ; I suppose its UTM. Let's parse
              key = str_equiv(geotiff.GTCITATIONGEOKEY)
              d = STRPOS(key, 'UTM_ZONE_')
              if d eq -1 then Message, 'Projection unknown. Contact Fabi.'
              zone = STRING((BYTE(key))[d+9:d+10])
            endif else Message, 'Projection unknown. Contact Fabi.'
          endif else begin
            d = Where(fields eq 'PROJECTEDCSTYPEGEOKEY', cnt)
            if cnt ne 0 then begin
              ; I need to know the datum. Currently WGS84 should be enough
              kkey = STRMID(str_equiv(geotiff.PROJECTEDCSTYPEGEOKEY), 0, 3)
              zone = STRMID(str_equiv(geotiff.PROJECTEDCSTYPEGEOKEY), 3, 2)
              case (kkey) of
                '326': ; UTM Northern emisphere
                '327': zone = '-' + zone ; UTM Southern emisphere
                else: Message, 'Projection unknown. Contact Fabi.
              endcase
            endif else  Message, 'Projection unknown. Contact Fabi.'
          endelse
                    
          ;Projection
          GIS_make_proj, ret, proj, PARAM='2, ' + zone
          grid = OBJ_NEW('w_Grid2D', nx=nx, $
            ny=ny, $
            dx=dx, $
            dy=dy, $
            x0=x0, $
            y0=y0, $
            proj=proj)
        end
        2: begin
          d = Where(fields eq 'GEOGCITATIONGEOKEY', cnt)
          if cnt ne 0 then begin
            ; I need to know the datum. Currently WGS84 should be enough
            if ~ (geotiff.GEOGCITATIONGEOKEY eq 'GCS_WGS_1984' $
              or geotiff.GEOGCITATIONGEOKEY eq 'WGS 84') $
              then Message, 'Projection unknown. Contact Fabi.'
          endif else Message, 'Projection unknown. Contact Fabi.'
          
          ;Projection
          GIS_make_proj, ret, proj, PARAM='1, WGS-84'
          grid = OBJ_NEW('w_Grid2D', nx=nx, $
            ny=ny, $
            dx=dx, $
            dy=dy, $
            x0=x0, $
            y0=y0, $
            proj=proj)
        end
        else: Message, 'Projection unknown. Contact Fabi.'
      endcase
    endif else Message, 'Projection unknown. Contact Fabi.'
  endelse
  
  ok = self->w_GISdata::init(grid, _EXTRA=extra)
  if ~ ok then return, 0
  undefine, grid
  
  self.order = 1 ;TODO: important! Are all geotiff files UL corner???
  
  return, 1
  
end

;+
; :Description:
;    Destroy the object instance
;
;-
pro w_GEOTIFF::cleanup

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  

  self->w_gisdata::Cleanup 
  ptr_free, self.info
  ptr_free, self.geotiff
  
end

;+
; :Description:
;    To obtain the list af available variables in the dataset.
;
; :Keywords:
;    COUNT: out, optional
;           the number of variables
;    PRINT: in, optional
;           set this keyword to print the variables (and info)
;           in the console
;           
; :Returns:
;   An array of variable ids
;
;-
function w_GEOTIFF::getVarNames, COUNT=count, PRINT=print

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  nv = (*self.info).channels
  
  varid = 0 
  varnames = '0' 
  varndims = 2
  varunits = '' 
  vardescriptions = ''
  vartypes = ''
  count = N_ELEMENTS(varid)
    
  if KEYWORD_SET(PRINT) then begin
    print, '   ID   NAME            DESCRIPTION                                 UNIT                   TYPE'
    
    for i = 0L, count-1 do begin
      ns = '                                                                                                                                  '
      STRPUT, ns, str_equiv(varid[i]), 3
      STRPUT, ns, STRLOWCASE(str_equiv(varnames[i])), 4 + 4
      STRPUT, ns, STRLOWCASE(str_equiv(vardescriptions[i])), 20 + 4
      STRPUT, ns, STRLOWCASE(str_equiv(varunits[i])), 70 - 2
      STRPUT, ns, STRLOWCASE(str_equiv(vartypes[i])), 81 + 10
      print, ns
    endfor
  endif
    
  return, varnames
  
end

;+
; :Description:
;    Checks if a variable is available
;
; :Params:
;    id: in, required
;        the variable ID
;
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data
;          
; :Returns:
;   1 if the variable is available, 0 if not
;   
;-
function w_GEOTIFF::hasVar, id, INFO=info
  
  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  undefine, info
  
  n = self->GetVarNames()  
  p = where(str_equiv(n) eq str_equiv(id), cnt)   
  if cnt eq 0 then return, 0
  
  name = '0'
  unit = '' 
  description = ''

  info = {id:id, name:name, description:description, unit:unit}
  
  return, 1

end

;+
; :Description:
;    Get the data for a specific variable, at the dimensions of the subset.
; 
; :Params:
;    id: in, optional
;        the variable ID. If not set, the TRMM precipitation will be returned instead
;    time: out, type = qms
;          the variable time
;    nt: out, type = long
;        the variable number of times
;        
; :Keywords:
;    INFO: out, optional
;          a structure containing information about the data. Contains the tags:: 
;            - name
;            - id
;            - description
;            - unit
;            
; :Returns:
;   the data array
;   
;-
function w_GEOTIFF::getVarData, id, time, nt, INFO=info, T0=t0, T1=t1

  ; Set up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  undefine, info, time, nt
  
  if N_ELEMENTS(id) eq 0 then begin
    id = '0'
  endif
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  if TOTAL(self.subset) ne 0 then begin
    sub_rect = [self.subset[0], self.subset[2], self.subset[1], self.subset[3]]
  endif
 
  out = READ_TIFF(self.file, SUB_RECT=sub_rect)
  
  return, ROTATE(out, 7)
  
end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_GEOTIFF__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_GEOTIFF                  ,  $
            INHERITS w_GISdata         ,  $
            file:                ''    ,  $ ; .tif file
            info:           PTR_NEW()  ,  $ ; info struct
            geotiff:        PTR_NEW()     $ ; geotiff struct
          }
    
end