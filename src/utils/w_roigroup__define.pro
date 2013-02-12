;+ 
;
; The only reason for this wrapper to exist is to overcome an 
; IDLanROI bug.  IDLanROI, at this date, makes IDL explode when it
; hast more than ~ 1000 ROIs to process. So I re-implemented
; the IDLanROI compute_mask and contain_points to check for
; a max umber of rois of 1000. If there are more rois, the smaller
; ones (in number of vertices, not in size) are roughly destroyed 
;
; :Author: FaM, 2012
;-

Function w_ROIGroup::Init
  
  return, self->IDLanROIGroup::Init()

end

;+
; :Description:
;    the routine that removes the ROIs
;
; :Private:
;
;-
pro w_ROIGroup::_filter_rois

  ; SEt up environnement
  @WAVE.inc
  COMPILE_OPT IDL2
  
  nents = self->count()
  if nents gt 1000 then begin
    nverts = LONARR(nents)
    for i=0, nents-1 do begin
      (self->get(POSITION=i))->GetProperty, N_VERTS=nv
      nverts[i] = nv
    endfor
    all = self->get(/ALL)
    ss = sort(nverts)
    ntoremove = nents - 1000
    for i=0, ntoremove do begin
      r = all[ss[i]]
      self->remove,  r
      OBJ_DESTROY, r
    endfor
  endif
  
end


;+
; :Description:
;    Wrapper for the IDLanROI function.
;    
; :Keywords:
;    INITIALIZE
;    DIMENSIONS
;    MASK_IN
;    LOCATION
;    MASK_RULE
;    PLANE_NORMAL
;    PLANE_XAXIS
;    RUN_LENGTH
;
;-
function w_ROIGroup::ComputeMask, INITIALIZE=initialize, $
    DIMENSIONS=dimensions, $
    MASK_IN=mask_in, $
    LOCATION=location, $
    MASK_RULE=mask_rule, $
    PLANE_NORMAL=plane_normal, $
    PLANE_XAXIS=plane_xaxis, $
    RUN_LENGTH=run_length
    
  if !VERSION.RELEASE ne '8.2.2' then self->_filter_rois
    
  return, self->IDLanROIGroup::ComputeMask(INITIALIZE=initialize, $
    DIMENSIONS=dimensions, $
    MASK_IN=mask_in, $
    LOCATION=location, $
    MASK_RULE=mask_rule, $
    PLANE_NORMAL=plane_normal, $
    PLANE_XAXIS=plane_xaxis, $
    RUN_LENGTH=run_length)
    
end

;+
; :Description:
;    Wrapper for the IDLanROI function.
;
; :Params:
;    X
;    Y
;    Z
;    
;-
function w_ROIGroup::ContainsPoints, X, Y, Z
    
  if !VERSION.RELEASE ne '8.2.2' then self->_filter_rois
  
  if N_ELEMENTS(Z) ne 0 then return, self->IDLanROIGroup::ContainsPoints(X, Y, Z)
  if N_ELEMENTS(Y) ne 0 then return, self->IDLanROIGroup::ContainsPoints(X, Y)
  return, self->IDLanROIGroup::ContainsPoints(X)
    
end

;+
; :Description:
;    Class structure definition 
;
;-
pro w_ROIGroup__Define, class
 
  ; SET UP ENVIRONNEMENT
  @WAVE.inc
  COMPILE_OPT IDL2  
  
  class = { w_ROIGroup               ,  $
            INHERITS IDLanROIGroup      $
            }
    
end