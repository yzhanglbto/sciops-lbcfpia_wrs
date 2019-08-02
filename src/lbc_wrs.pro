  ;+
  ; :Description:
  ;    WRS LBC wavefront reconstruction software.
  ;    This function is the core of WRS. It takes an input image with defocused pupils and returns the associated Zernike coeffiecients.
  ;     
  ;
  ; :Params:
  ;    data         -- input image 
  ;    timest       -- timestamp used for the filenames of the saved information (in case the save_wrs keyword in the config file is ON).
  ;    \display     -- Keyword to display all the inrmation regarding the selected pupils
  ;    FAST_MODE    -- Keyword to enabling fast_mode (only defocus and astigmatism will be estimated)
  ;
  ;
  ;
  ; :Author: Marco Stangalini marco.stangalini@inaf.it
  ; first version: 5 Apr 2014 (MS)
  ; 20 Apr 2014 log file added (MS)
  ; 24 Apr 2014 Sigma threshold, smoothing and dx are read from param file (MS)
  ; 28 Apr 2014 Automatic threshold adjustment in case it is too low and no pupil is selected (MS)
  ; 29 Apr 2014 Check of the pupil selected added. WRS will eventually report on the bad quality of the pupil selected. (MS)
  ; 4 May 2014  Bug fixed. Extrafocal sign corrected. (MS)
  ; 4 May 2014  IDL version check added. It solves the problem with the IDL smooth function which requires the /edge_mirror keyword for IDL ver. higher than 8.1 (MS)
  ; 7 May 2014  Coma estimate added in the fast mode too
  ; 14 May 2014 Coma check added. If it is larger than 1.1 mu then the software will try to reduce it before adding more Zernike.
  ; 9 Jun 2014 Constraint on z4 added for visualization purposes. If it is > 21 then in the maps it is forced to be 19 to fit the window size
  ; 9 Jun 2014 dx added as input in coma call 
  ; 12 Jun 2014 defocus bias removed to match lbcfpia_wrs requirements
  ; 12 Jun 2014 Coma interactive adjustment added
  ; 01 Sept 2014 Added first order coma estimation keyword
  ; 26 Mar 2015 Sph check added by MS
  ; 3 Jul 2015 Separated displays for blue and red channels
  ; 21 Dec 2015 Display of the final model fixed. 
  ; 
  ;-
function LBC_WRS, data, timest, display=display, fast_mode=fast_mode, red=red, blue=blue

@WRS.common 
smoothing=WRS.smoothing
sig=WRS.default_sigma
dx=fovpix/2



printf,33, 'Searching for pupils...'
if  keyword_set(display) then begin
  if keyword_set(red) then begin
  Po=FINDPUP(data, sig, dx, timest, /display, /red)
  endif else begin
  Po=FINDPUP(data, sig, dx, timest, /display, /blue)  
  endelse  
endif else begin
  Po=FINDPUP(data, sig, dx, timest)
endelse




;if there is no pupil available WRS will try to lower the detection threshold
IF PO[0] EQ 0 THEN BEGIN
  PRINT, 'WARNING NO PUPIL DETECTED TRYING AUTOMATIC NOISE THRESHOLD ADJUSTMENT ...'
  PRINTF,33, 'WARNING NO PUPIL DETECTED TRYING AUTOMATIC NOISE THRESHOLD ADJUSTMENT ...'

  WHILE PO[0] EQ 0 DO BEGIN
  SIG=SIG*wrs.threshold_adj
  PRINT, 'Threshold adjustment: ', SIG, ' sigma'
  PRINTF,33, 'Threshold adjustment: ', SIG, ' sigma'

  if  keyword_set(display) then begin
     ;Po=FINDPUP(data, sig, dx, timest, /display)
    if keyword_set(red) then begin
      Po=FINDPUP(data, sig, dx, timest, /display, /red)
    endif else begin
      Po=FINDPUP(data, sig, dx, timest, /display, /blue)
    endelse
  endif else begin
    Po=FINDPUP(data, sig, dx, timest)
  endelse
  if sig lt 0.98 then begin
    print, 'Warning, the detection threshold is too low or the is no good star available in the image.'
    printf, 33, 'Warning, the detection threshold is too low or the is no good star available in the image.'
    close,33
    retall
  endif
  ENDWHILE
ENDIF

PRINTF,33, 'BEST SUITED PUPIL @ X,Y ', PO[0], PO[1] 


;preparing the image for the analysis 
PRINTF, 33, 'CROPPING IMAGE...'
sky, data, std, /silent
wh=where(data lt std)
data(wh)=0
siz=size(po)

if !VERSION.RELEASE ge 8.3 then begin
data=smooth(data,smoothing, /edge_mirror)
endif else begin 
  data=smooth(data,smoothing)
endelse

  
endelseimg=fltarr(2*dx,2*dx)
count=0
pupilgrid=fltarr(fovpix,fovpix)
zerny=fltarr(10,siz(1))
total_I=fltarr(siz(1))
img=data(po[0]-dx:po[0]+dx-1, po[1]-dx:po[1]+dx-1)
si=size(img)

pupil_size = 206266.*(1e-6*wrs.lambda)/asperpix
scale=pupil_size/fovpix



if  keyword_set(fast_mode) then begin
      PRINTF,33, 'COMPUTING THE FIRST 6 ZERNIKE TERMS... '
      getmom, img, ze
      if wrs.interactive eq 1 then begin
        PRINTF,33, 'INTERACTIVE COMA ADJUSTMENT ON... '
        if keyword_set(red) then begin
        L=INTERACTIVE_COMA(img, std, scale, dx, /red)
        endif else begin
        L=INTERACTIVE_COMA(img, std, scale, dx, /blue)
        endelse
        wrs.interactive=0
      ENDIF ELSE BEGIN
      L=coma(img, std, scale, dx)
      ENDELSE
      ze=[ze, L]
      Ltot=sqrt(l(0)^2+L(1)^2)
      if (ltot gt wrs.COMA_THRESHOLD) then begin
        print, 'Warning, coma is too large...disabling high order aberrations' 
        printf,33, 'Warning, coma is too large...disabling high order aberrations' 
        printf,33, 'WRS will try to adjust the pupil shape using only coma'
        ze[4:5]=0.
      endif
      
      
endif else begin 
  ;slow mode 
    zet=fltarr(wrs.nzer)
    getmom, img, ze
    ltot=0.
    
    if (wrs.first_order_adj eq 1) then begin
        if wrs.interactive eq 1 then begin
          PRINTF,33, 'INTERACTIVE COMA ADJUSTMENT ON... '
        if keyword_set(red) then begin
        L=INTERACTIVE_COMA(img, std, scale, dx, /red)
        endif else begin
        L=INTERACTIVE_COMA(img, std, scale, dx, /blue)
        endelse          
          
          wrs.interactive=0
        ENDIF ELSE BEGIN
          L=coma(img, std, scale, dx)
        ENDELSE
        Ltot=sqrt(L(0)^2+L(1)^2)
    endif
    
      ans1=0
      sph=check_sph(img, std, scale, dx)
;     check_sph return, [mdistance, Ly, Lx, I1, I2, maxprof, Iprof0]
;     new return, [mdistance, Ly, Lx, I1, I2, maxprof, Iprof0, S1_I1, S1_I2, S2_I1, S2_I2, S3_I1, S3_I2, S4_I1, S4_I2]

      sphthr=sph(4)-sph(3)
      
      ;pupil quadrants Z11 
      s1=0
      s2=0
      s3=0
      s4=0 
      
      ;check if Z11 is present at all quadrants else coma
      if sph(7) gt wrs.sphthr*sph(8)+sph(8) or sph(8) gt wrs.sphthr*sph(7)+sph(7) then s1=1
      if sph(9) gt wrs.sphthr*sph(10)+sph(10) or sph(10) gt wrs.sphthr*sph(9)+sph(9) then s2=1
      if sph(11) gt wrs.sphthr*sph(12)+sph(12) or sph(12) gt wrs.sphthr*sph(11)+sph(11) then s3=1
      if sph(13) gt wrs.sphthr*sph(14)+sph(14) or sph(14) gt wrs.sphthr*sph(13)+sph(13) then s4=1
      sf=s1+s2+s3+s4

      if sph(3) gt wrs.sphthr*sph(4)+sph(4) or sph(4) gt wrs.sphthr*sph(3)+sph(3) and wrs.sphcheck eq 1 and sf ge 3  then begin
            
      print, ''
      print, ''
      print, '==========================================================='
      print, '==========================================================='
      print, '==========================================================='
      print, 'If I am not wrong Z11 is very large'
      printf, 33, 'Warning Z11 large...entering Z11 priority mode'
      print, '==========================================================='
      print, '==========================================================='
      print, '==========================================================='
;      print, 'If you see large Sph, options 1 or 2 are suggested.'
;      print, 'If you see coma larger than Sph than suggested option is 0'
;      
;      print, '1 = Sph close to the obscuration'
;      print, '2 = Sph toward the edges of the pupil' 
;      print, '0 = Continue normally without giving priority to Z11'
;      read, ans1 
;      wdelete, 14
;      if ans1 eq 1 or ans1 eq 2 then begin
        printf, 33, 'Z11 estimated from check_sph.pro'
      goto, jump_coma
;      endif 
      endif 
      
    
    
    if ltot gt wrs.COMA_THRESHOLD and (wrs.first_order_adj eq 1) then begin
      print, 'Warning coma is too large...disabling high order aberrations'
      printf,33, 'Warning coma is too large...disabling high order aberrations'
      printf,33, 'WRS will try to adjust the pupil shape using only coma'
      ze[4:5]=0.
      ze=[ze, L]
      zet(0:N_elements(ze)-1)=ze
      ze=zet
    endif else begin
      
      
    PRINTF,33, 'COMPUTING ZERNIKE TERMS... '
    if wrs.COMA_INIT_MORPH eq 1 then begin
    L=coma(img, std, scale, dx)
    ze=[ze,L]
    endif
    if wrs.COMA_MORPH eq 1 then begin
      L=coma(img, std, scale, dx)
    endif

      jump_coma: printf, 33, ''
      
      if  sph(3) gt wrs.sphthr*sph(4)+sph(4) or sph(4) gt wrs.sphthr*sph(3)+sph(3) and wrs.sphcheck eq 1 and sf ge 3 then begin
       
        ;printf, 33, 'first order coma est. skipped as per user request'
            ze[4:5]=0
            ze=[ze, [0*sph(1), 0*sph(2)]]
            if sph(3) gt wrs.sphthr*sph(4)+sph(4) then begin
            ze=[ze,[0.,0.,0.5]]
            endif
            if sph(4) gt wrs.sphthr*sph(3)+sph(3) then begin
            ze=[ze,[0.,0.,-0.5]] 
            endif
        endif
        if keyword_set(red) then begin
        findze, img, ze, WRS.nzer, chi2, model, /red
        endif else begin
        findze, img, ze, WRS.nzer, chi2, model, /blue
        endelse
    
    if wrs.COMA_MORPH eq 1 then begin
      ze[6:7]=L
    endif  

    endelse
endelse

zeim=ze
zeim[1:2]=0.
if abs(zeim[3]) gt 16. then begin
zeim[3]=16
endif


model = getimagewrs(zeim)
if (WRS.efoc lt 0) then ze[4:5] *= -1. 

if keyword_set(display) then begin
  !p.multi=[0,2,1]
  
  if keyword_set(red) then begin
   window,2,xs=600,ys=300, title='RED CHANNEL'
  tvframe, model(fovpix/2-dx:fovpix/2+dx-1, fovpix/2-dx:fovpix/2+dx-1) , /asp, title='MODEL'
  tvframe, img(fovpix/2-dx:fovpix/2+dx-1, fovpix/2-dx:fovpix/2+dx-1),/ASP,title='REAL'
  print, '***************** Zernike coeffs in mu starting from defocus **********************'
  print, ze[3:*]
  print, '***********************************************************************************'
  if wrs.save_wrs eq 1 then begin
    image=tvrd(TRUE=3)
    WRITE_JPEG, wrs.save_dir+'RED_WRS_images_'+timest+'.jpg', image , true=3
    printf,33, wrs.save_dir+'RED_WRS_images_'+timest+'.jpg SAVED'
  endif
  !p.multi=0
  
  endif else begin
    window,20,xs=600,ys=300, title='BLUE CHANNEL'
    tvframe, model(fovpix/2-dx:fovpix/2+dx-1, fovpix/2-dx:fovpix/2+dx-1) , /asp, title='MODEL'
    tvframe, img(fovpix/2-dx:fovpix/2+dx-1, fovpix/2-dx:fovpix/2+dx-1),/ASP,title='REAL'
    print, '***************** Zernike coeffs in mu starting from defocus **********************'
    print, ze[3:*]
    print, '***********************************************************************************'
    if wrs.save_wrs eq 1 then begin
      image=tvrd(TRUE=3)
      WRITE_JPEG, wrs.save_dir+'BLUE_WRS_images_'+timest+'.jpg', image , true=3
      printf,33, wrs.save_dir+'BLUE_WRS_images_'+timest+'.jpg SAVED'
    endif
    !p.multi=0
    
    
  endelse
  
  
  
endif


if wrs.save_wrs eq 1 then begin
  if keyword_set(red) then begin
  save, filename=wrs.save_dir+'RED_WRS_results_'+timest+'.save', model, img, ze, WRS
  printf,33, 'RED WRS results saved in: '+wrs.save_dir+'WRS_results_'+timest+'.save'
  endif else begin
    save, filename=wrs.save_dir+'BLUE_WRS_results_'+timest+'.save', model, img, ze, WRS
    printf,33, 'BLUE WRS results saved in: '+wrs.save_dir+'WRS_results_'+timest+'.save'  
  endelse
  
  
  
endif



return, ze
end