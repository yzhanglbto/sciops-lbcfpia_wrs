pro make_wrs2

  close,33
  ; resolve_routine cannot accept a path in the name
  ; resolve_routine doesn't handle generic library names (i.e. ztools)
  resolve_routine, 'readfits', /IS_FUNCTION
  resolve_routine, 'cova_zern1', /COMPILE_FULL_FILE, /EITHER
  resolve_routine, 'readpar', /COMPILE_FULL_FILE, /EITHER
  resolve_routine, 'cartop', /COMPILE_FULL_FILE, /EITHER

  COMMON param, WRS,ngrid,r,zgrid,pupil,Rpix,inside,asperpix,ccdpix,npixperpix,fovpix,sflag,sigpix,fl
  common impix,immod,zres,z0,chis2,iter,xc,yc,efoc,ws0,ws1,img


;return
end
