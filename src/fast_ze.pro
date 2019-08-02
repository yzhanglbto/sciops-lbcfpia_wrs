pro fast_ze, impix1, zestim, lambda, pixscale, Dia

fits_read, '~/test_lbc.fits', impix1

lambda=0.55
Dia=8.4

siz=size(impix1)

ngrid = 1024
D = dia
eps = 0.2
lambda = 0.55
pixel = 0.23
sflag = 1 ; fast seeing blur, set to 1 for slow calc.


asperpix = 206265.*(1e-6*lambda)/D ; optimal sampling FWHM in asec
print, 'Nyquist ', asperpix
ccdpix = float(pixel)
k = floor(alog10(ccdpix/asperpix)/alog10(2.)) +1

npixperpix = 2^k
fovpix = 2*ngrid/npixperpix     ; CCD field size

asperpix = ccdpix/npixperpix
size = 206266.*(1e-6*lambda)/asperpix
Rpix = ngrid/size*D


print, 'Rebinning factor: ', npixperpix
print, 'Grid pixel: ',asperpix,' arcsec'
print, 'Grid size: ', size,' m'
print, 'CCD pixel:  ',ccdpix,' arcsec'
print, 'CCD field:  ',fovpix*ccdpix,' arcsec'
print, 'CCD format: ', fovpix


 n =siz(1)/2
 xx = (findgen(2*n)-n)#replicate(1,2*n)
 yy = transpose(xx)

  thresh = max(impix1)*0
  impix = impix1
  impix (where(impix le thresh)) = 0.

 imh0 = total(impix)

 xc = total(xx*impix)/imh0 ;
 yc = total(yy*impix)/imh0 ;
 mxx = total(impix*(xx-xc)^2)/imh0
 myy = total(impix*(yy-yc)^2)/imh0
 mxy = total(impix*(xx-xc)*(yy-yc))/imh0

  scale = 13.5
  a2 = scale*(xc-0.5)*!pi*0.5
  a3 = scale*(yc-0.5)*!pi*0.5

print, a2,a3


  a4 = 13.5*sqrt((mxx + myy)*0.5)/1.102
  a4 = sqrt( (a4^2. - (0.5/2.35)^2) > 0.) ; subtract 0.5arcsec seeing

  print, a4
  stop
  
;  a4 = scale*sqrt((mxx + myy)*0.5)/1.102/1.35
  a5 = scale*mxy*(mxx*myy)^(-0.25)/1.45
  a6 = scale*(mxx - myy)*0.5*(mxx*myy)^(-0.25)/1.45
  zestim = [0.,a2,a3,a4,a5,a6]*donpar.lambda/(2.*!pi) ; estimated Zernike aberrations
  zestim[0] = 0.2
  
end