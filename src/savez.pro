pro savez, z, file
  ; Save Zernike vector in ASCII file
  
  openw, unit, file,/get_lun
  fmt='(F8.3,1X,I2)'
  for i=0,n_elements(z)-1 do printf, unit, z[i], i+1, format=fmt
  close, unit
  free_lun, unit
  print, 'Zernike vector is saved in ',file
end
;------------------------------------------------