pro getmom, impix1, zestim
  ; returns the vector of Zernike aberrations in microns
  @WRS.common
  
  n = ngrid/npixperpix
  xx = (findgen(2*n)-n)#replicate(1,2*n)
  yy = transpose(xx)
  
  thresh = max(impix1)*wrs.thresh
  impix = impix1
  impix (where(impix le thresh)) = 0.
  
  imh0 = total(impix)
  
  xc = total(xx*impix)/imh0 ;
  yc = total(yy*impix)/imh0 ;
  mxx = total(impix*(xx-xc)^2)/imh0
  myy = total(impix*(yy-yc)^2)/imh0
  mxy = total(impix*(xx-xc)*(yy-yc))/imh0
  
  scale = npixperpix/(Ngrid/Rpix)
  
  a2 = scale*(xc+0.5)*!pi*0.5
  a3 = scale*(yc+0.5)*!pi*0.5
  
  ; stop
  a4 = scale*sqrt((mxx + myy)*0.5)/1.102
  a4 = sqrt( (a4^2 - (0.5/2.35)^2) > 0) ; subtract 0.5arcsec seeing
  ;  a4 = scale*sqrt((mxx + myy)*0.5)/1.102/1.35
  a5 = scale*mxy*(mxx*myy)^(-0.25)/1.45
  a6 = scale*(mxx - myy)*0.5*(mxx*myy)^(-0.25)/1.45
  zestim = [0.,a2,a3,a4,a5,a6]*wrs.lambda/(2.*!pi) ; estimated Zernike aberrations
  zestim[0] = 0.5
  
end