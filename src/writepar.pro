pro writepar, file
  ; Write parameters into a file
  
  @WRS.common
  
  openw, unit, file, /get_lun
  
  n = n_tags(wrs)
  parnames = tag_names(wrs)
  
  printf, unit, '{wrs, '
  for i=0,n-1 do  begin
    if (i eq n-1) then sep = "}" else sep = ","
    if (datatype(wrs.(i)) eq 'STR') then $
      printf, unit, parnames[i],": '",wrs.(i),"'"+sep else $
      printf, unit, parnames[i],": ",wrs.(i),sep
  endfor
  
  close, unit
  free_lun, unit
  print, 'Parameters are saved in ',file
end