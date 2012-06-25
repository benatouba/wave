; 1.7 11/15/05
;
; iinterpol
; ---------
;
; An improved interpol, which is really just a wrapper to call interpol,
; but forcing it to do the things you would hope it would do by default:
;
;   Given a y and an x vector, and a set of new x's to interpolate to:
;     - If either point used for interpolation is NaN, the interpolated
;       point is also (interpol does actually manage this.)
;     - Instead of extrapolating beyond the bounds of the data (why? why?)
;       supply NaN as a bad data value for any interpolation points
;       outside of the input data.
;     - Don't blow up if some of the input x's are Nan, too.
;
; The assumption is made that we are always dealing with floats or doubles.
; and that NaN is used as a bad data flag.
;
; The x vector must be monotonically increasing or decreasing.
;
; Also:
;
;  /LN - interpolates linearly with x transformed as ln(x) - "standard"
;        for interpolating in vertical pressure coords. 
;
;  /LONG180 - interpolates longitude data taking into account any SINGLE 
;        wraparound problem. Any pair of points separated by more than 270
;        degrees is assumed to wrap around. This option works for
;        data in the range -180 to +180. (See book 11-51)
;
;  /LONG360 - interpolates as above, but for data in the range 0 to 360
;
;  /UM_STYLE - As seen in old dynamics UM, UM doc paper S1, and in the
;        UM routine V_INT, where new x points are outside the range spanned
;        by the original x points, there is no extrapolation. Instead,
;        y values are set to the value at the nearest y point - in
;        vertical interpolation, this means either the top or bottom level,
;        as appropriate. Interpolation is in ln(p), too - no need to set /LN
;
;  /CONST - as above, but without the logarithms
;
;  /QUADRATIC - inherits interpol's keyword for quadratic interpolation
;  /SPLINE - ditto cubic spline
;
; Alan Geer 26/6/2003
;

function w_interpol, y, x, x_new, ln = ln, long180 = long180, $
                      long360 = long360, um_style = um_style, const = const, $
                      quadratic = quadratic, spline = spline

   if ~keyword_set(quadratic) then quadratic = 0
   if ~keyword_set(spline) then spline = 0
   
   size = size(x)
   if size[2] ne 4 or size[2] ne 5 then _x = DOUBLE(x) else _x=x


   BAD_DATA_FLAG = !values.f_nan ; NaN carry through any calculations we might make

   on_error, 2 ; on error, return to caller

   ; Check we are not being asked to interpolate from and to the
   ; same set of levels with NaNs in the y array, this code sometimes
   ; returns good data levels as NaNs as well. As a quick fix, just 
   ; return the input y
   if array_equal(_x,x_new) then begin
      y_return = y
      return, y  
   endif

   ; Eliminate any bad data in the x vector
   iGoodX = where(finite(_x), count)
   if count ne 0 then begin
   
      working_x = reform(_x[iGoodX]) ; no sensible language would pass 
                                    ; by reference by default!
      working_y = reform(y[iGoodX]) ; Reform to deal with e.g. [1,1,17] arrays       

      ; First, deal with the X values
      ; -----------------------------

      ; Use natural log transform if requested
      if keyword_set(LN) or keyword_set(UM_STYLE) then begin 
     
         if min(working_x) lt 0 or min(x_new) lt 0 then $
           message, '-ve values, so interpolation in log coords wont work' 
         working_x = alog(working_x)
   working_x_new = alog(x_new)
   
      endif else begin

         working_x_new = x_new
      
      endelse
 
      ; Now tag on a point at the closest useable value to + and - infinity
      ; so as to prevent interpol extrapolating. 
   
      ; Check data type of x and divine the biggest value we can use 
      size = size(working_x)
      if size[0] ne 1 then message, 'X vector is wrong size: '+string(size[0])   
      if size[2] eq 4 then begin      
         ; Vector of FLOATs
         machine_precision = machar()      
      endif else begin
         if size[2] eq 5 then begin      
            ;Vector of DOUBLEs
            machine_precision = machar(/double)        
         endif else begin      
            message, 'X vector is wrong data type: '+string(size[2])   
         endelse
      endelse  
    
      minus_big = -machine_precision.xmax
      plus_big = machine_precision.xmax

      ; Check whether values increasing or decreasing
      iMax = where(working_x eq max(working_x))
      iMin = where(working_x eq min(working_x))
      if iMax ge iMin then begin
         working_x = [minus_big, working_x, plus_big]
      endif else begin
         working_x = [plus_big, working_x, minus_big]
      endelse 

      if keyword_set(UM_STYLE) or keyword_set (CONST) then begin
        ; Set all values outside range to the value at the closest
        ; x point
        temp_nY = n_elements(working_y)
        working_y = [working_y[0], working_y, working_y[temp_nY-1]]
      endif else begin
        ; Alan style: set points outside range to NaN
        working_y = [BAD_DATA_FLAG, working_y, BAD_DATA_FLAG]
      endelse

      ; Second, deal with the Y-values
      ; ------------------------------
      
      if keyword_set(LONG360) then begin
         longitude = 360
      endif else begin
         if keyword_set(LONG180) then begin
      longitude = 180
         endif else begin
      longitude = 0
   endelse
      endelse

      ; Deal with longitude wraparound issues if needed 
      ; Method is to find the wraparound point (more than one cannot
      ; be handled here). This point is used to divide the y vector into
      ; two sets. We add 360 degrees to the set with the lower longitudes. 
      if longitude ne 0 then begin
      
         nY = n_elements(working_y)
         gapsize = abs(working_y[1:*]-working_y[0:nY-2])
   iBigGaps = 1 + where(gapsize gt 270,nBigGaps)
   case nBigGaps of
   0: ; No wraparounds found (No More Big Gaps)
   1: begin

      message, /info, 'Big gap found: '+string(working_y[iBigGaps-1])+$
         string(working_y[iBigGaps])
      if working_y[iBigGaps] gt string(working_y[iBigGaps-1]) then begin

               working_y[0:iBigGaps-1] = working_y[0:iBigGaps-1]+360

      endif else begin

               working_y[iBigGaps:*] = working_y[iBigGaps:*]+360

      endelse

   end
   else: begin
      message, string(format='(i0)', nBigGaps)+$
         ' wraparounds found. Can only handle one.'
   end
   endcase
         
      endif 

      ; Beg an answer from IDL:   
      interpolated_y = interpol( working_y, working_x, working_x_new, $
         quadratic = quadratic, spline = spline)

      ; Deal with any longitude issues
      if longitude ne 0 then begin
         if nBigGaps gt 0 then begin
   
            ; Where we have added 360 degrees, there may be "over scale"
      ; interpolated values. Correct these. 
      iLongitudeOverScale = where(interpolated_y gt longitude, count)
      if count gt 0 then begin
               interpolated_y[iLongitudeOverScale] = $
            interpolated_y[iLongitudeOverScale] - 360
            endif

         endif
      endif 

      return, interpolated_y
      
  endif else begin
  
     ; There was no good x data at all
     return, replicate(BAD_DATA_FLAG, n_elements(x_new))
  
  endelse
   
end 
