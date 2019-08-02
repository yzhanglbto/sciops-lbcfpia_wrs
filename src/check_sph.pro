function check_sph, img, std, scale, dx, mdistance=mdistance

;history
;created in Oct. 2015 
;20 Feb 2016 added coma check. If all quadrants of the pupil show a radial profile consistent with Z11 then Z11 correction is activated, else coma priority is given


  cen=centroid(img gt std*1.1)
  img=shift(img, dx-cen(0), dx-cen(1))
  im=float(smooth(img,3,/edge_mirror))
 
  im=float(im)
  temp=im
  cendisk=centroid(temp gt std*2.)
  massimo=max(temp, maxim)
  maxim=float(array_indices(temp, maxim))
  
  sizep=size(img)
  ;pro cartop,fxy,x0,y0,rmax,r,theta,frt,missing=missing
  cartop, img, sizep(1)/2, sizep(2)/2, sizep(2)/2, r, theta, frt
  Iprof0=smooth(total(frt, 2), 5, /edge_mirror)
  
  sizepol=size(frt)
  thsize=sizepol(2)
  sector_size=round(thsize/4)
  
  sect1=smooth(total(frt(*,0:sector_size-1), 2), 5, /edge_mirror)
  sect2=smooth(total(frt(*,sector_size:2*sector_size-1), 2), 5, /edge_mirror)
  sect3=smooth(total(frt(*,2*sector_size:3*sector_size-1), 2), 5, /edge_mirror)
  sect4=smooth(total(frt(*,3*sector_size:4*sector_size-3), 2), 5, /edge_mirror)
  sect1=sect1/max(sect1)
  sect2=sect2/max(sect2)
  sect3=sect3/max(sect3)
  sect4=sect4/max(sect4)
  
  iprof0=iprof0/max(iprof0)
  maskp=iprof0*0.
  whp=where(iprof0 gt 0.5)
  maskp(whp)=1
  I1p=(whp(0))
  I2p=(whp(n_elements(whp)-1))
  delta_Ipos=round((I2p-I1p)/2.)  
  I1=Iprof0(whp(0)+round(delta_Ipos/2))
  I2=Iprof0(whp(n_elements(whp)-1)-round(delta_Ipos/2))
  print, I1, I2
  maxp=max(Iprof0, maxprof)

  maskp=sect1*0.
  whp=where(sect1 gt 0.5)
  maskp(whp)=1
  S1_I1p=(whp(0))
  S1_I2p=(whp(n_elements(whp)-1))
  delta_Ipos=round((S1_I2p-S1_I1p)/2.)
  S1_I1=sect1(whp(0)+round(delta_Ipos/2))
  S1_I2=sect1(whp(n_elements(whp)-1)-round(delta_Ipos/2))
  print, S1_I1, S1_I2


  maskp=sect2*0.
  whp=where(sect2 gt 0.5)
  maskp(whp)=1
  S2_I1p=(whp(0))
  S2_I2p=(whp(n_elements(whp)-1))
  delta_Ipos=round((S2_I2p-S2_I1p)/2.)
  S2_I1=sect2(whp(0)+round(delta_Ipos/2))
  S2_I2=sect2(whp(n_elements(whp)-1)-round(delta_Ipos/2))
  print, S2_I1, S2_I2


  maskp=sect3*0.
  whp=where(sect3 gt 0.5)
  maskp(whp)=1
  S3_I1p=(whp(0))
  S3_I2p=(whp(n_elements(whp)-1))
  delta_Ipos=round((S3_I2p-S3_I1p)/2.)
  S3_I1=sect3(whp(0)+round(delta_Ipos/2))
  S3_I2=sect3(whp(n_elements(whp)-1)-round(delta_Ipos/2))
  print, S3_I1, S3_I2



  maskp=sect4*0.
  whp=where(sect4 gt 0.5)
  maskp(whp)=1
  S4_I1p=(whp(0))
  S4_I2p=(whp(n_elements(whp)-1))
  delta_Ipos=round((S4_I2p-S4_I1p)/2.)
  S4_I1=sect4(whp(0)+round(delta_Ipos/2))
  S4_I2=sect4(whp(n_elements(whp)-1)-round(delta_Ipos/2))
  print, S4_I1, S4_I2


loadct, 13
  plots, (maxim(0)), (maxim(1)), /data, psym=4, color=100, thick=2
  plots, (cendisk(0)), (cendisk(1)), /data, psym=4, color=200, thick=2
  plots, 5,5, psym=4, color=200
  plots, 5,8, psym=4, color=100
  xyouts, 7,5, 'est. center of pupil', color=200
  xyouts, 7,8, 'est. max', color=100

  
  mdistance=maxprof ;SQRT((cendisk(1)-maxim(1))^2.+(cendisk(0)-maxim(0))^2.)
  Lx=-maxim(0)+cendisk(0)
  Ly=-maxim(1)+cendisk(1)
loadct, 0
  
  if maxim(0) gt dx then begin
    signLx=-1
  endif else begin
    signLx=1
  endelse
  if maxim(1) gt dx then begin
    signLy=-1
  endif else begin
    signLy=1
  endelse
  


  
  Lx=signLx*scale*abs(Lx)
  Ly=signLy*scale*abs(Ly)

return, [mdistance, Ly, Lx, I1, I2, maxprof, Iprof0, S1_I1, S1_I2, S2_I1, S2_I2, S3_I1, S3_I2, S4_I1, S4_I2]
  end
  