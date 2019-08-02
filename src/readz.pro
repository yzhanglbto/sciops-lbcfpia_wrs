pro readz, z, file
  ; Read Zernike vector from ASCII file
  
  res = findfile(file, count=c)
  if (c eq 0) then begin
    print, 'File ',file,' is not found, exiting'
    return
  endif
  
  openr, unit, file,/get_lun
  tmp = fltarr(300)
  i = 0 & x = 0.0
  while not eof(unit) do begin
    readf, unit, x
    tmp[i] = x
    i +=1
  endwhile
  close, unit
  free_lun, unit
  if (i eq 0) then z=-1 else z = tmp[0:i-1]
  print, i,' Zernike coefficients are retrieved from ',file
  
end