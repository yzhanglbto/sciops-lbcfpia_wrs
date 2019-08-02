function newimage, a, jzer
  ; a is the amplitude change of aberration (micron), Jzer is the Zernike number
  ; (1=seeing, 4=focus etc.)
  
  @WRS.common
  COMMON imagedata, uampl, filter2, seeing
  
  newampl = uampl
  if (jzer gt 1) then begin  ; Change Zernike coefficient
    newphase =  2.*!pi/wrs.lambda*a*zernike_estim(jzer,zgrid)
    newampl(inside) *= complex(cos(newphase), sin(newphase))
    filter = filter2
  endif else begin  ; new seeing
    newseeing = seeing + a
    if (sflag gt 0) then begin
      filter = exp(-2.*!pi^2*(newseeing/2.35/asperpix/2/ngrid)^2*r^2) ; unbinned image
    endif else begin
      rr = shift(dist(fovpix),fovpix/2,fovpix/2)
      filter = exp(-2.*!pi^2*(newseeing/2.35/ccdpix/fovpix)^2*rr^2) ; binned image
    endelse
  endelse
  ;---------  compute the image ----------------------
  imh = abs(shift(fft(shift(newampl,ngrid,ngrid),/inverse),ngrid,ngrid))^2
  if (sflag gt 0) then begin ; exact seing blur
    imh = abs(fft(shift(fft(imh),ngrid,ngrid)*filter))
    impix = rebin(imh,fovpix,fovpix) ; rebinning into CCD pixels
  endif else begin
    impix = rebin(imh,fovpix,fovpix) ; rebinning into CCD pixels
    impix = abs(fft(shift(fft(impix),fovpix/2,fovpix/2)*filter)) ; Seeing blur
  endelse
  ;  tvscl, congrid(impix,2*ngrid,2*ngrid)
  return, impix/total(impix)
end