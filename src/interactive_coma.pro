;+
; :Description:
;    The function estimates the amount of coma in LBC images in interactive mode.
;
; :Params:
;    img:  the working pupil in WRS
;    std:  the noise level in the LBC image
;    scale:the scale factor of the pixels in mu
;
;
;
; :Author: Marco Stangalini
; first version 12 Jun 2014
;-
; 3 Jul 2015 Separated displays for blue and red channels


function interactive_coma, img, std, scale, dx, red=red, blue=blue


  pos_outer=fltarr(2)
  pos_inner=fltarr(2)
  sizeimg=size(img)
  scalefactorimg=512./sizeimg(1)

  cen=centroid(img gt std*1.1)
  img=shift(img, dx-cen(0), dx-cen(1))
  
  im=float(smooth(img,3,/edge_mirror))
  dex=deriv(im)
  dey=rot(deriv(rot(im,90)),-90)
  contorni=(dex+dey)
  contorni=contorni/max(contorni)
  con=congrid(contorni, dx*2,dx*2) > 0.1
  ;result = RADON(con, RHO=rho, THETA=theta)
  ;backproject = RADON(result, /BACKPROJECT, RHO=rho, THETA=theta)
  
  Q = Search2D(con,dx,dx,0.,1.0, /increase)
  index_y = Q / (SIZE(con))[1]
  index_x = Q - (index_y * (SIZE(con))[1])
  
  ma=con*0.
  ma(index_x, index_y)=1
  
  
  if keyword_set(red) then begin
  WINDOW,5, xs=512, ys=512, title='RED CHANNEL Interactive coma adj'
  endif else begin
  WINDOW,15, xs=512, ys=512, title='BLUE CHANNEL Interactive coma adj'
  endelse
  
  
  tvscl, congrid(img,512,512)
  xyouts, 0,10, 'Click approximately on the PUPIL center (outer circle)', chars=1.5, color=255, charthick=1.5, /device
  cursor, x0,y0, /device
  pos_outer(0)=x0/scalefactorimg
  pos_outer(1)=y0/scalefactorimg
  wait,1
  tvscl, congrid(img,512,512)
  xyouts, 0,40, 'Pupil center estimated', chars=1.3, color=255, charthick=1.5, /device
  xyouts, 0,10, 'Now click approximately on the OBSCURATION center...', chars=1.5, color=255, charthick=1.5, /device
  cursor, x0,y0, /device
  pos_inner(0)=x0/scalefactorimg
  pos_inner(1)=y0/scalefactorimg
  
  mim=max(im, maxim)
 pos_max=array_indices(im,maxim)

if pos_max(0) gt dx then begin 
  signLx=-1
endif else begin
  signLx=1
endelse
if pos_max(1) gt dx then begin
  signLy=-1
endif else begin
  signLy=1
endelse


L=sqrt((pos_outer(0)-pos_inner(0))^2.+(pos_outer(1)-pos_inner(1))^2.)
Lx=-pos_outer(0)+pos_inner(0)
Ly=-pos_outer(1)+pos_inner(1)



Lx=-scale*(Lx)/0.8
Ly=-scale*(Ly)/0.8
  
  if keyword_set(red) then begin
  wdelete,5
  endif else begin
  wdelete,15
  endelse
  
  ;print, ly, lx
  plots, pos_outer(0), pos_outer(1), /data, psym=5
  plots, pos_inner(0), pos_inner(1), /data, psym=6
  
  
  return, [Ly,Lx]
  
  
end