pro saveres, resfile, z, chi2, imfile

  @WRS.common
  
  openw, unit, resfile,/get_lun, /append
  printf, unit, imfile, wrs.xc, wrs.yc, flux, chi2, z,  $
    format='(A20,2I6,E10.3,F8.4,100F8.3)'
  close, unit
  free_lun, unit
  print, 'Results are saved!'
  
end