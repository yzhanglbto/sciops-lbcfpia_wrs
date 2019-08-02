pro fit, impix, immod, zres, efoc, chi2
  ; preliminary and final fitting
  
  @WRS.common
  
  getmom, impix, zres
  
  nzer = wrs.nzer
  if (wrs.static ne '') then readz, z0, wrs.static else z0 = fltarr(nzer)
  
  z0[0:5] = zres
  if (efoc lt 0) then z0[3:5] *= -1.
  zres = z0
  
  find, impix, zres, nzer, chi2, immod
  
end