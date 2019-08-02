pro init
  ; Pre-compute the parameters and save them in the COMMOM block
  
  @WRS.common
  
  ngrid = wrs.ngrid
  d = wrs.d
  eps = wrs.eps
  lambda = wrs.lambda
  pixel = wrs.pixel
  sflag = 0 ; fast seeing blur, set to 1 for slow calc.
  
  
  ; Find reasonable limits on the grid parameters
  asperpix =  206265.*(1e-6*lambda)/D ; maximum fine-pixel size
  
  ccdpix = float(pixel)
  k = floor(alog10(ccdpix/asperpix)/alog10(2.)) +1
  npixperpix = 2^k
  fovpix = 2*ngrid/npixperpix     ; CCD field size
  asperpix = ccdpix/npixperpix
  size = 206266.*(1e-6*lambda)/asperpix
  pupil_size=size
  Rpix = ngrid/size*D
  
  PRINTF,33, 'WRS INITIALIZATION...'
  printF,33, 'Rebinning factor: ', npixperpix
  printF,33, 'Grid pixel: ',asperpix,' arcsec'
  printF,33, 'Grid size: ', size,' m'
  printF,33, 'CCD pixel:  ',ccdpix,' arcsec'
  printF,33, 'FOV field:  ',fovpix*ccdpix,' arcsec'
  printF,33, 'FOV pixels: ', fovpix
  PRINTF,33,''
  
  
  r = shift(dist(2*ngrid),ngrid,ngrid) ;distance from grid center, pixs
  inside = where( (r le Rpix) and (r ge Rpix*eps) )
  pupil = fltarr(2*ngrid,2*ngrid)    ; zero array
  pupil(inside) = 1
  n = n_elements(inside)
  
  x = (findgen(2*ngrid) - ngrid) # replicate(1.,2*ngrid)
  theta = atan(transpose(x),x)
  theta(ngrid,ngrid)=0.
  zgrid = fltarr(n,2)
  zgrid(*,0) = r(inside)/Rpix
  zgrid(*,1) = theta(inside)
end