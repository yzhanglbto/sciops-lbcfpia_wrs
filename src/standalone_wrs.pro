pro standalone_wrs, filename, SendTCS=SendTCS, Red=Red, Blue=Blue

homedir='~'

close,33
fits_read,filename, data, header

;for PROP simulations 
;restore, filename,/v
;*****************************************
if keyword_set(red) then begin
  res=CALL_WRS(data, /red)
endif

if keyword_set(blue) then begin
  res=CALL_WRS(data, /blue)
endif


if keyword_set(SendTCS) then begin
  if keyword_set(red) then begin
    side='right'
  endif else begin
    side='left'
  endelse
  
  
Outfile='TCS_commands'
writeOutFile, res, res*0.0, OutFile=OutFile
PrgName = HomeDir+Path_Sep()+"TCSSendWavefront"+Path_Sep()+"TCSSendWavefront " + OutFile + " " + side
Spawn, PrgName
endif


end
