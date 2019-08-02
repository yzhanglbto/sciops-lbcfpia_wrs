pro WFreconstructor_LBC, impix, zres, nzer, chi2, model
; Nzer is the highest Zernike number, zres is the  result

@WRS.common

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

   indonut = where(impixnorm gt impixmax*0.) 
  im = impixnorm(indonut)
  n = n_elements(indonut)
  chi2old = n^2
  chi2norm = total(im^2)
  ncycle = 30
  thresh0 = 0.01  ; initial SVD threshold
  norm = max(impix)
  thresh = thresh0  ; SVD inversion threshold, initial
  print, 'Z  ',indgen(nzer)+1, format='(A4,100I8)'  
  lambda = 1. ; for L-M method



for k=0,ncycle-1 do begin
  model = getimage(z0)
  im0 = model(indonut)
  chis2 = sqrt(total((im0 - im)^2)/chi2norm )
  
  
  print, 'Cycle: ',k+1, '  RMS= ',chis2*100, ' percent' 
;  print, 'um ', z0[0:nzer-1], format='(A4,100F8.3)'

  thresh = thresh*0.5 ; > 1e-3
  if (chis2 lt 1e-4) then break
  if (chis2 le chi2old) then begin
     zres=z0 
     lambda = lambda*0.1
     if ((chis2 ge chi2old*0.99) and (k gt 3)) then break
     chi2old = chis2 
  endif else  begin
     z0 = zres
     thresh = thresh0
     lambda = lambda*10.
     ;print, 'Divergence... Now LM parameter = ', lambda
  endelse

  
  if (k mod 2 eq 0) then begin
    imat = fltarr(n,nzer)
    ;print, 'Computing the interaction matrix...'
    for j=0,nzer-1 do imat(*,j) =  ((newimage(xi[j],j+1))(indonut) - im0)/xi[j]
    tmat = (transpose(imat) # imat)
    svd_invert,  tmat, tmp, thresh
    invmat = tmp # transpose(imat)
  endif


  dif = im - im0  
;  dif = (im - im0)/sig 
  dz = invmat # dif  
  z0[0:nzer-1] += 0.7*dz
;  z0[0:nzer-1] += dz
  z0[0] = z0[0] > 0.2
  z0[0] = z0[0] < 2.0
  

  d1 = min(dif) & d2 = max(dif)
endfor

print, 'Fitting done!'

end