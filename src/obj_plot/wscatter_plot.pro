pro WScatter_plot, x, y, xtitle, ytitle, PNG = png, title= title, PIXMAP = pixmap, range = range, NOFIT = nofit, NOCORR = nocorr

  ; Set Up environnement
  COMPILE_OPT idl2
  @WAVE.inc                   

  ; prepare the plot  
  device, DECOMPOSED=0, TRUE_COLOR=24, RETAIN=2  
  
  if ~KEYWORD_SET(range) then range = [min([x,y]),max([x,y])]
    
  loadct, 5
  plot, x, y, title = title,  COLOR=0, BACKGROUND=255, CHARSIZE=1.8, /DEVICE, PSYM=1, XTICK_GET=xs, $
   CHARTHICK = 1.6, THICK=2., XTITLE = xtitle, Ytitle = Ytitle,  POSITION = [80,80,520,520], XRange=range, yrange= range
  
  plots, xs, xs, color = 0, LINESTYLE=5
  
  if not KEYWORD_SET(NOFIT) then begin
  
    l = LINFIT(x, y  , /DOUBLE)
    
    plots, xs, (l[0] + l[1]*xs), color = 0, NOCLIP = 0
    
    if l[0] lt 0 then sign = '- ' else sign = '+ '
    
    text = 'y = ' + STRING(l[1], FORMAT='(F5.2)') + ' x ' + sign + STRING(abs(l[0]), FORMAT='(F6.2)')
    XYOUTS, 95, 490, text, CHARSIZE= 2., COLOR=0, CHARTHICK=1., /DEVICE
        
  endif
  
  if NOt KEYWORD_SET(nocorr) then begin
          r2 = CORRELATE(x,y)
    r2 = r2*r2
      text = 'r2 = ' + STRING(r2, FORMAT='(F5.2)')
    XYOUTS, 95, 460, text, CHARSIZE= 2., COLOR=0, CHARTHICK=1., /DEVICE
  endif
  
  if KEYWORD_SET(PNG) then begin
    img = TVRD(/true)
    WRITE_PNG, png , img
  end
  
end