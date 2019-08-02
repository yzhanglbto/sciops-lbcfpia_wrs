function extract, img, xc, yc, nccd

  @WRS.common
  
  
  ix1 = xc-nccd >0 & ix2 = xc+nccd < n_elements(img(*,0))-1
  iy1 = yc-nccd >0 & iy2 = yc+nccd < n_elements(img(0,*))-1
  
  img1 = img[ix1:ix2,iy1:iy2] ; cut out the required part
  img1 = img1 - min(img1)  ; subtract background
  itot = total(img1)
  
  ; find the center-of-gravity
  nx = n_elements(img1(*,0)) & ny = n_elements(img1(0,*))
  xx = (findgen(nx)-nx/2)#replicate(1,ny)
  ix = total(img1*xx)/itot + 2
  
  yy = replicate(1,nx) # (findgen(ny)-ny/2)
  iy = total(img1*yy)/itot +2
  
  ix = fix(ix)+ nx/2 & iy = fix(iy)+ ny/2
  
  ix1 = ix-nccd/2 >0 & ix2 = ix1+nccd < nx-1
  iy1 = iy-nccd/2 >0 & iy2 = iy1+nccd < ny-1
  
  if (ix2-ix1 lt nccd-1) or (iy2-iy1 lt nccd-1) then begin
    print, 'Image is cut on one side!'
    return, -1
  endif
  
  impix = img1[ix1:ix2,iy1:iy2]
  i = (sort(impix))[fix(0.1*fovpix^2)]
  backgr = impix[i] ; 10% quantile of pixel distribution
  ;  stop
  
  impix = float(impix) - backgr
  flux = total(impix)
  print, 'Total flux, ADU: ', flux
  impix = impix/flux
  sigpix = (impix > 0)*flux*wrs.eadu + wrs.ron^2  ; variance in each pixel
  
  return, impix
end