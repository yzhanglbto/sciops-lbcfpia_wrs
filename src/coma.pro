  ;+
  ; :Description:
  ;    The function estimates the amount of coma in LBC images.
  ;
  ; :Params:
  ;    img:  the working pupil in WRS
  ;    std:  the noise level in the LBC image
  ;    scale:the scale factor of the pixels in mu
  ;
  ;
  ;
  ; :Author: Marco Stangalini
  ; first version 7 May 2014
  ; 9 Jun 2014 - Bug fixed. If the central obscuration is not visible the code was giving NAN
  ; 12 Jun 2014 Bug fixed that prevented the correct estimate of the obscuration
  ;-
function coma, img, std, scale, dx

cen=centroid(img gt std*1.1)
img=shift(img, dx-cen(0), dx-cen(1))
im=float(smooth(img,3,/edge_mirror))
dex=deriv(im)
dey=rot(deriv(rot(im,90)),-90)
contorni=(dex+dey)
contorni=contorni/max(contorni)
;con=congrid(contorni, dx*2,dx*2) > 0.1
;result = RADON(con, RHO=rho, THETA=theta)
;backproject = RADON(result, /BACKPROJECT, RHO=rho, THETA=theta)
;Q = Search2D(con,dx,dx,0.,1.0, /increase)
;index_y = Q / (SIZE(con))[1]
;index_x = Q - (index_y * (SIZE(con))[1])
;ma=con*0.
;ma(index_x, index_y)=1


si=size(im)
m1=0b
m2=0b
count=0
im=float(im)
temp=im
outer=im gt std
radgain=0.5



for count=1,40 do begin
  temp=img gt std*(1.+count*0.1)
  pos_outer=CENTROID(temp)
  dis=DIST(si(1), si(2))
  dis=SHIFT(dis, dx,dx)
  w1=where(temp eq 1)
  dis=dis lt radgain*si(1)/2.
  black=dis*(1-temp)
  b1=where(black eq 1)
  m1=[m1, float(n_elements(w1))]
  m2=[m2, float(n_elements(b1))]
  
  ;tvframe, temp+black,/asp
  
  
  
endfor

;*******************************
m1=m1/max(m1)
m2=m2/max(m2)
abdiff = abs(m2-0.05)  ;form absolute difference
mindiff = min(abdiff,index_m2)  ;find smallest difference
abdiff = abs(m1-0.05)  ;form absolute difference
mindiff = min(abdiff,index_m1)  ;find smallest difference
;*******************************


;_______________________________________
m0=max(m1*m2, mi)
temp=img gt std*(1.+(mi)*0.1)
dis=DIST(si(1), si(2))
dis=SHIFT(dis, si(1)/2, si(1)/2)
w1=where(temp eq 1)
dis=dis lt radgain*si(1)/2.
black=dis*(1-temp)
pos_outer=CENTROID(temp+black)
b1=where(black eq 1)
pos_inner=CENTROID(black)
;_______________________________________


;**********TEST SOLUTION****************
;temp=img gt std*(1.+(index_m1)*0.1)
;dis=DIST(si(1), si(2))
;dis=SHIFT(dis, si(1)/2, si(1)/2)
;w1=where(temp eq 1)
;dis=dis lt radgain*si(1)/2.
;black=dis*(1-temp)
;pos_outer=CENTROID(temp+black)
;
;temp=img gt std*(1.+(index_m2)*0.1)
;dis=DIST(si(1), si(2))
;dis=SHIFT(dis, si(1)/2, si(1)/2)
;w1=where(temp eq 1)
;dis=dis lt radgain*si(1)/2.
;black=dis*(1-temp)
;b1=where(black eq 1)
;pos_inner=CENTROID(black)
;***************************************


if finite(pos_inner(0)) eq 0 then begin
  pos_inner(0)=si(1)/2
  PRINTF,33, 'Warning: central obscuration may be absent or not well definable...trying to go on anyway'
endif
if finite(pos_inner(1)) eq 0 then begin
  pos_inner(1)=si(1)/2
endif


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
Lx=signLx*scale*abs(Lx)
Ly=signLy*scale*abs(Ly)
;print, ly, lx
plots, pos_outer(0), pos_outer(1), /data, psym=5
plots, pos_inner(0), pos_inner(1), /data, psym=6


return, [Ly,Lx]


end