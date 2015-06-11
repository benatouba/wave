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
  compile_opt IDL2

  catch, theError
  if theError ne 0 then begin
    catch, /Cancel
    ok = WAVE_Error_Message(!Error_State.Msg + ' Wont create the object. Returning... ')
    return, 0
  endif

  ; Check arguments
  if n_elements(file) ne 1 then file = dialog_pickfile(TITLE='Please select tif file to read', /MUST_EXIST)
  if ~ file_test(file) then message, 'File not found or not valid: ' + file
  if ~ query_tiff(file, info, GEOTIFF=geotiff) then message, 'TIFF file not query-able'
  self.file = file
  self.geotiff = ptr_new(geotiff)
  self.info = ptr_new(info)

  if n_elements(grid) ne 0 then begin
    ; User defined grid geoloc
    if ~ obj_valid(grid) then message, WAVE_Std_Message('grid', /ARG)
    if ~ obj_isa(grid, 'w_Grid2d') then message, WAVE_Std_Message('grid', /ARG)
  endif else begin

    ;Try to make geo-info alone (aaarg)
    nx = info.Dimensions[0]
    ny = info.Dimensions[1]

    ; Get the fields of the geotiff structure.
    fields = tag_names(geotiff)

    ; We can only handle raster images with projected coordinate systems, unless this is
    ; a GeoTiff file with Geographic model.
    gtModelIndex = where(fields eq 'GTMODELTYPEGEOKEY', gtModelType)
    if gtModelType gt 0 then begin
      ; This is for LatLon projection grids
      dx = (geotiff.ModelPixelScaleTag)[0]
      dy = (geotiff.ModelPixelScaleTag)[1]

      ; Get the tie points (upper left + half pix).
      if keyword_set(NO_DELTA) then begin
        x0 = (geotiff.ModelTiePointTag)[3]
        y0 = (geotiff.ModelTiePointTag)[4]
      endif else begin
        x0 = (geotiff.ModelTiePointTag)[3] + dx/2
        y0 = (geotiff.ModelTiePointTag)[4] - dy/2
      endelse

      case (geotiff.gtModelTypeGeoKey) of
        1: begin
          d = where(fields eq 'GEOGCITATIONGEOKEY', cnt)
          if cnt ne 0 then begin
            ; I need to know the datum. Currently WGS84 should be enough
            if ~ (geotiff.GEOGCITATIONGEOKEY eq 'GCS_WGS_1984' or geotiff.GEOGCITATIONGEOKEY eq 'WGS 84') then message, 'Projection unknown. Contact Fabi.'
            d = where(fields eq 'GTCITATIONGEOKEY', cnt)
            if cnt ne 0 then begin
                key = str_equiv(geotiff.GTCITATIONGEOKEY)
              if key eq 'LOCAL TRANSVERSE MERCATOR' then begin
                GIS_make_datum, ret, wgs
                k0 = geotiff.PROJSCALEATNATORIGINGEOKEY
                lon0 = geotiff.PROJNATORIGINLONGGEOKEY
                ; 3 - Transverse Mercator
                ;   a, b, lat0, lon0, x0, y0, k0, [datum], name
                pars = w_str(wgs.ellipsoid.a) + ', ' + w_str(wgs.ellipsoid.b) + ', 0., ' + w_str(lon0, 8) + ', 0., 0., ' + w_str(k0, 4)
                GIS_make_proj, ret, proj, PARAM= '3, ' + pars + ', WGS-84, LOCAL TRANSVERSE MERCATOR'
              endif else begin
                ; I suppose its UTM. Let's parse
                d = strpos(key, 'UTM_ZONE_')
                if d eq -1 then message, 'Projection unknown. Contact Fabi.'
                zone = string((byte(key))[d+9:d+10])
                if string((byte(key))[d+11]) eq 'S' then zone = '-' + zone
                ;Projection
                GIS_make_proj, ret, proj, PARAM='2, ' + zone
              endelse
            endif else message, 'Projection unknown. Contact Fabi.'
          endif else begin
            d = where(fields eq 'PROJECTEDCSTYPEGEOKEY', cnt)
            if cnt ne 0 then begin
              ; I need to know the datum. Currently WGS84 should be enough
              kkey = strmid(str_equiv(geotiff.PROJECTEDCSTYPEGEOKEY), 0, 3)
              zone = strmid(str_equiv(geotiff.PROJECTEDCSTYPEGEOKEY), 3, 2)
              case (kkey) of
                '326': ; UTM Northern emisphere
                '327': zone = '-' + zone ; UTM Southern emisphere
                else: message, 'Projection unknown. Contact Fabi.
              endcase
            endif else  message, 'Projection unknown. Contact Fabi.'
            ;Projection
            GIS_make_proj, ret, proj, PARAM='2, ' + zone
          endelse
          
          grid = obj_new('w_Grid2D', nx=nx, $
            ny=ny, $
            dx=dx, $
            dy=dy, $
            x0=x0, $
            y0=y0, $
            proj=proj)
            
        end
        2: begin
          d = where(fields eq 'GEOGCITATIONGEOKEY', cnt)
          if cnt ne 0 then begin
            ; I need to know the datum. Currently WGS84 should be enough
            if ~ (geotiff.GEOGCITATIONGEOKEY eq 'GCS_WGS_1984' $
              or geotiff.GEOGCITATIONGEOKEY eq 'WGS 84') $
              then message, 'Projection unknown. Contact Fabi.'
          endif else begin
            d = where(fields eq 'GEOGRAPHICTYPEGEOKEY', cnt)
            if cnt ne 0 then begin
              ; I need to know the datum. Currently WGS84 should be enough
              if geotiff.GEOGRAPHICTYPEGEOKEY ne 4326 then message, 'Projection unknown. Contact Fabi.'
            endif else   message, 'Projection unknown. Contact Fabi.'
          endelse
          ;Projection
          GIS_make_proj, ret, proj, PARAM='1, WGS-84'
          grid = obj_new('w_Grid2D', nx=nx, $
            ny=ny, $
            dx=dx, $
            dy=dy, $
            x0=x0, $
            y0=y0, $
            proj=proj)
        end
        else: message, 'Projection unknown. Contact Fabi.'
      endcase
    endif else message, 'Projection unknown. Contact Fabi.'
  endelse

  case (info.orientation) of
    0: self.order = 0
    1: self.order = 1
    4: self.order = 0
    else: message, 'GEOTIFF orientation not supported'
  endcase

  ok = self->w_GISdata::init(grid, _EXTRA=extra)
  if ~ ok then return, 0
  undefine, grid

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
;    Get access to some params. 
;
; :Keywords:
;    INFO: out, optional
;          structure containing information about the image in the file
;    GEOTIFF: out, optional
;          GeoTIFF tags and keys found in the file
;    _Ref_Extra: out
;                all parent classed property
;                
;-      
pro w_GEOTIFF::GetProperty,  $
    INFO=info, $
    GEOTIFF=geotiff, $
    _Ref_Extra=extra
    
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2
  
  IF Arg_Present(INFO) THEN info = *self.info
  IF Arg_Present(GEOTIFF) THEN geotiff = *self.geotiff
  
  self->w_GISdata::GetProperty, _Extra=extra
  
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
  
  tifinfo = *self.info
  
  if N_ELEMENTS(id) eq 0 then begin
    id = '0'
  endif
  
  if ~ self->hasVar(id, INFO=info) then Message, 'Variable Id not found: ' + str_equiv(id)
  
  if TOTAL(self.subset) ne 0 then begin
    sub_rect = [self.subset[0], self.subset[2], self.subset[1], self.subset[3]]
  endif
 
  out = READ_TIFF(self.file, SUB_RECT=sub_rect, INTERLEAVE=2)
  
  if self.order eq 1 then for i=0, tifinfo.CHANNELS-1 do out[*,*,i] = ROTATE(out[*,*,i], 7)
  
  return, out
  
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