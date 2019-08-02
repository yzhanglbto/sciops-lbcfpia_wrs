pro AI_test


timest = '2016Feb10_17:55:16'
sig=3
dx=22
close,33
HomeDir = GetEnv("WRS_CFG_HOME")
If HomeDir EQ "" Then HomeDir="."
ParFile = HomeDir+'/WRS_BLUE.par'
readpar, ParFile
inv=fltarr(7,10)
OPENW, 33, 'WRS_report.dat', /append
  filename='/home/shark/Desktop/LBC_imgs/lbcb.20151208.053950.fits'
  fits_read,filename, data, header
  Po=FINDPUP(data, sig, dx, timest, /display)
  inv(*,0)=invariant_moments(po)
  filename='/home/shark/Desktop/LBC_imgs/lbcb.20160114.131215.fits'
  fits_read,filename, data, header
  Po=FINDPUP(data, sig, dx, timest, /display)
  inv(*,1)=invariant_moments(po)
  filename='/home/shark/Desktop/LBC_imgs/lbcb.20160114.131651.fits'
  fits_read,filename, data, header
  Po=FINDPUP(data, sig, dx, timest, /display)
  inv(*,2)=invariant_moments(po)

  

stop
end