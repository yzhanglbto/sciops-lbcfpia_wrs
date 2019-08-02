function getimagewrs, z
  ; z is the Zernike vector in microns, starting from Z=2 (tip)
  ; z[0] is seeing in arcseconds
  @WRS.common
  
  COMMON imagedata, uampl, filter2, seeing
  
  ;  t0 = systime(1)
  fact = 2.*!pi/wrs.lambda
  nzer = n_elements(z)
  phase = zgrid(*,0)*0.  ; empty array for phase
  for j=1, nzer-1 do phase += fact*z[j]*zernike_estim(j+1,zgrid)
  
  tmp = fltarr(ngrid*2,ngrid*2)
  uampl = complex(tmp, tmp)
  uampl(inside) = complex(cos(phase), sin(phase))
  
  seeing = z[0]
  
  ;---------  compute the image ----------------------
  imh = abs(shift(fft(shift(uampl,ngrid,ngrid),/inverse),ngrid,ngrid))^2
  if (sflag gt 0) then begin ; exact seing blur
    filter2 = exp(-2.*!pi^2*(seeing/2.35/asperpix/2/ngrid)^2*r^2) ; unbinned image
    imh = abs(fft(shift(fft(imh),ngrid,ngrid)*filter2))
    impix = rebin(imh,fovpix,fovpix) ; rebinning into CCD pixels
  endif else begin
    rr = shift(dist(fovpix),fovpix/2,fovpix/2)
    filter2 = exp(-2.*!pi^2*(seeing/2.35/ccdpix/fovpix)^2*rr^2) ; binned image
    impix = rebin(imh,fovpix,fovpix) ; rebinning into CCD pixels
    impix = abs(fft(shift(fft(impix),fovpix/2,fovpix/2)*filter2)) ; Seeing blur
  endelse
  ;  tvscl, congrid(impix,2*ngrid,2*ngrid)
  return, impix/total(impix)
end