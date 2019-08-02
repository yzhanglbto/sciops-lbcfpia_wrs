function CALL_WRS, data, ze, red=red, blue=blue
;4 May 2014 Bugs fixed. M1 commands computation added
;19 Jun 2014 Homedir added by KS
; 20140901 Bug fixed. Number of Zernike used fixed.
; 20150326 keywordy blue and red added by MS
; 20150703  make_wrs2 call added to compile and set on the fly the WRS routines needed


;make_wrs2 ;compiles the necessary routines and prepares the WRS environment

  @WRS.common
  timest=SYSTIME()
  month=STRMID(timest, 4, 3)
  date=STRMID(timest, 8, 2)
  time=STRMID(timest, 11, 8)
  year=STRMID(timest, 20, 4)
  timest=year+month+date+'_'+time
  coefflast=[0,0]
  

  HomeDir = GetEnv("WRS_CFG_HOME")
  If HomeDir EQ "" Then HomeDir="."

  if keyword_set(red) then begin
    ParFile = HomeDir+'/WRS_RED.par'
    readpar, ParFile
    print, 'RED par file read'
  endif else begin
    ParFile = HomeDir+'/WRS_BLUE.par'
    readpar, ParFile
    print, 'BLUE par file read'

  endelse
   
  
  PRINT, '--------------------------------'
  PRINT, 'WRS ver. '+ string(wrs.soft_ver)
  PRINT, '--------------------------------'
  PRINT, timest
  PRINT, '--------------------------------'
  OPENW, 33, 'WRS_report.dat', /append
  
  
  if keyword_set(red) then begin
    printf, 33, 'LBC-RED configuration file read'
  endif else begin
    printf, 33, 'LBC-BLUE configuration file read'
  endelse
  
  
  printf,33, 'WRS configuration summary: '
  printf,33, WRS
  printf,33,''
  
  
  IF WRS.DISP EQ 1 AND WRS.FAST_MODE EQ 1 THEN BEGIN
    PRINT, 'RUNNING WRS IN FAST MODE'
    PRINTf,33, 'WRS FAST MODE ON -- Display mode on'
    if keyword_set(red) then begin
    ze=LBC_WRS(data, timest,  /display, /fast_mode, /red)
    endif else begin
    ze=LBC_WRS(data, timest,  /display, /fast_mode, /blue)
    endelse
  ENDIF
  
  IF WRS.DISP EQ 0 AND WRS.FAST_MODE EQ 1 THEN BEGIN
    PRINT, 'RUNNING WRS IN FAST MODE'
    PRINTf,33, 'WRS FAST MODE ON -- Display mode off'
    ze=LBC_WRS(data, timest, /fast_mode)
  ENDIF
  
  IF WRS.DISP EQ 1 AND WRS.FAST_MODE EQ 0 THEN BEGIN
    PRINT, 'RUNNING WRS IN SLOW MODE'
    PRINTf,33, 'WRS FAST MODE off -- Display mode on'
    printf, 33, 'max number of Zernike to be estimated ', wrs.nzer
    if keyword_set(red) then begin
    ze=LBC_WRS(data, timest,  /display, /red)
    endif else begin
    ze=LBC_WRS(data, timest,  /display, /blue)
    endelse
  ENDIF
  
  IF WRS.DISP EQ 0 AND WRS.FAST_MODE EQ 0 THEN BEGIN
    PRINT, 'RUNNING WRS IN SLOW MODE'
    PRINTf,33, 'WRS FAST MODE off -- Display mode off'
    printf,33, 'max number of Zernike to be estimated ', wrs.nzer
    ze=LBC_WRS(data, timest)
  ENDIF
  
  
  PRINTF,33,'Zernike coeffs. estimated [mu]: '
  PRINTF,33, 'Seeing [arcsec], tip, tilt, Defocus, 45 deg ast, 0 deg ast, Y coma, X coma, Y trefoil, X Trefoil, Sph,...'
  printf, 33, ZE



ze_Temp=fltarr(12)
ze_temp(0:wrs.nzer-1)=ze
ze=ze_Temp

  printf,33, 'Applying Ze mapping ...'  
  ZE[0]=WRS.a1*ZE[0]
  ZE[1]=WRS.a2*ZE[1]
  ZE[2]=WRS.a3*ZE[2]
  ZE[3]=WRS.a4*ZE[3]
  ZE[4]=WRS.a5*ZE[4]
  ZE[5]=WRS.a6*ZE[5]
  ZE[6]=WRS.a7*ZE[6]
  ZE[7]=WRS.a8*ZE[7]
    
  if WRS.FAST_MODE EQ 0 THEN BEGIN
  ZE[8]=WRS.a9*ZE[8]
  ZE[9]=WRS.a10*ZE[9]
  ZE[10]=WRS.a11*ZE[10]
  endif
  printf,33, 'Ze mapping succesfully applied'
  
  PRINTF,33,'M1 commands: '
  PRINTF,33, 'Seeing [arcsec], tip, tilt, Defocus, 45 deg ast, 0 deg ast, Y coma, X coma, Y trefoil, X Trefoil, Sph,...'
  printf, 33, ZE
  Printf,33, 'WRS iteration completed on ', SYSTIME()
  printf, 33, '=========================================================================================================================='
  close, 33

return, ze
end
