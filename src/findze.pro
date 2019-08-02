pro findze, impix, zres, nzer, chi2, model, red=red, blue=blue
  ; Nzer is the highest Zernike number, zres is the  result
  ; 3 Jul 2015 Separated displays for blue and red channels

  @WRS.common
  
  ncycle=WRS.ncycles
  
  impix=impix/total(impix)
  nzer = nzer > 6
  n1 = n_elements(zres)
  nzer1 = n1 > nzer
  z0 = fltarr(nzer1)
  z0[0:n1-1] = zres
  
  impixnorm = impix/total(impix)
  impixmax = max(impix)
  
  xi = fltarr(nzer)
  for j=1,nzer-1 do xi[j] = wrs.lambda/(2.*!pi)*0.5/(long(sqrt(8L*(j+1L)-6L)-1L)/2L) ; 0.5/n amplitudes
  xi[0] = 0.1
  xi[6:*]=xi[5]
  ;  xi = xi*0.5 ; Added Dec. 7, 2005
  ;  xi = xi*2. ; Added Dec. 7, 2005
  
  indonut = where(impixnorm gt impixmax*wrs.thresh)
  ;   indonut = where(impixnorm ge 0.)
  ;  sig = sqrt(sigpix(indonut)) ; pixel rms due to noise
  
  im = impixnorm(indonut)
  n = n_elements(indonut)
  chi2old = n^2
  chi2norm = total(im^2)
  
  ncycle = wrs.ncycles
  thresh0 = wrs.threshSVD  ; initial SVD threshold
  norm = max(impix)
  thresh = thresh0  ; SVD inversion threshold, initial
  print, 'Z  ',indgen(nzer)+1, format='(A4,100I8)'
  printF,33, 'Z  ',indgen(nzer)+1, format='(A4,100I8)'
  
  
  ;  print, 'um ', z0[0:nzer-1], format='(A4,100F8.3)'
  lambda = 1. ; for L-M method
  
  for k=0,ncycle-1 do begin
    model = GETIMAGEWRS(z0)
    im0 = model(indonut)
    chi2 = sqrt(total((im0 - im)^2)/chi2norm )
    
    
    
    print, 'Cycle: ',k+1, '  RMS= ',chi2*100, ' percent'
    print, 'um ', z0[0:nzer-1], format='(A4,100F8.3)'
    printF,33, 'Cycle: ',k+1, '  RMS= ',chi2*100, ' percent'
    printF,33, 'um ', z0[0:nzer-1], format='(A4,100F8.3)'
    
    
    thresh = thresh*0.5 ; > 1e-3
    ;   thresh = chi2*0.1  ; Dec 7, 2005
    
    ;  if ((chi2 ge chi2old*0.99) and (k gt 3)) then break
    if (chi2 lt 1e-4) then break
    
    
    ; do not degrade aberrations
    if (chi2 le chi2old) then begin
      zres=z0
      lambda = lambda*0.1
      if ((chi2 ge chi2old*0.99) and (k gt 3)) then break
      chi2old = chi2
    endif else  begin
      z0 = zres
      thresh = thresh0
      lambda = lambda*10.
      print, 'Divergence... Now LM parameter = ', lambda
      printF,33, 'Divergence... Now LM parameter = ', lambda
      
      
    endelse
    
    
    if (k mod 2 eq 0) then begin
      imat = fltarr(n,nzer)
      print, 'Computing the interaction matrix...'
      printF,33, 'Computing the interaction matrix...'
      
      for j=0,nzer-1 do imat(*,j) =  ((newimage(xi[j],j+1))(indonut) - im0)/xi[j]
      
      
      tmat = (transpose(imat) # imat)
      for j=0,nzer-1 do tmat[j,j] *= (1.+lambda) ; L-M correction
      svd_invert,  tmat, tmp, thresh
      invmat = tmp # transpose(imat)
    endif
    
    dif = im - im0
    ;  dif = (im - im0)/sig
    dz = invmat # dif
    z0[0:nzer-1] += 0.5*dz
    ;  z0[0:nzer-1] += dz
    
    z0[0] = z0[0] > 0.2
    z0[0] = z0[0] < 1.5
    
    d1 = min(dif) & d2 = max(dif)
    ; display the image (left: input, right: model)
    ;  tmp = fltarr(512,256)
    ;  tmp(0:255,*) = congrid(impix, 256,256)
    ;  tmp(256:511,*) = congrid(model, 256,256 )
    ;;  tmp(256:511,*) = congrid((impix-model-d1)/(d2-d1)*impixmax, 256,256 )
    ;;  tmp(256:511,*) = congrid((impix-model+0.5*impixmax), 256,256 )
    ;  tvframe, tmp, /asp
    
    IF WRS.DEBUG EQ 1 THEN BEGIN
      if keyword_set(red) then begin
      WINDOW,11, xsize=300, ysize=300, title='RED CHANNEL ITERATION # '+strtrim(k,1)
      endif else begin
      WINDOW,31, xsize=300, ysize=300, title='BLUE CHANNEL ITERATION # '+strtrim(k,1)
      endelse
      TVFRAME, MODEL, /ASP
      
    ENDIF
    
  endfor


  IF WRS.DEBUG EQ 1 THEN BEGIN
  if keyword_set(red) then begin
    wdelete,11
  endif else begin
    wdelete,31
  endelse
ENDIF
  ;  zres = z0[0:nzer-1] ; Do not truncate to permit further fitting
  
  print, 'Fitting done!'
  printF,33, 'Fitting done!'
  
end