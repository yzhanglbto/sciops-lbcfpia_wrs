;
; Estimate sky level in Nx x Ny 50% overlapping regions,
; fit 2nd order poly in x/y to resulting Nx x Ny "image",
; finally interpolate to size of input image.
;
; uses sky procedure from astron library
; created nov 22, 2007
; adjust Np from 200 for Tech images - feb 29, 2008 jmh
; Compute average sigma of background and return in function parameter. Feb 02, 2010.
;
Function Fit_BackGnd, inIma, Sigma_Bck=Sigma_Bck, Dbg=Dbg
  ;
  ; Check that input is an image (2d array), get its dimensions in X,Y
  ;
  If Size(inIma, /N_Dimensions) Ne 2 Then Message, "Input must be a 2-d array"
  Sz = Size(inIma, /Dimensions)
  Sx = Sz[0]
  Sy = Sz[1]
  ;
  ; Sky level is estimated in Nx x Ny regions of Np x Np 
  ; pixels in size
  ;
  Np = 200
  If Sx lt 300 or Sy lt 300 Then Np = 64    ;for Tech Images
  Nd = Np/2
  Nx = Floor(Sx/Nd)-1
  Ny = Floor(Sy/Nd)-1

  If KeyWord_Set(Dbg) Then $
     ExtDbgLog, String("Fit_BackGnd: Np,Nx,Ny = ",Np,",",Nx,",",Ny)

  ; Background "image"
  bi = FltArr(Nx,Ny)
  Sigma_Bck = 0.0D0
  For I=0,Nx-1 Do Begin
     For J=0,Ny-1 Do Begin
        ; compute start/end position in x/y of region
        ; where sky level has to be estimated
        xs = I*Nd
        xe = xs+Np-1
        ys = J*Nd
        ye = ys+Np-1

        ;Print, "xs, xe = ",xs,xe
        ;Print, "ys, ye = ",ys,ye

        ; estimate sky level and put in "pixel" I,J
        sky,inIma[xs:xe,ys:ye],skymode,skysig,/silent
        bi[I,J] = skymode
        Sigma_Bck += skysig
        ; If KeyWord_Set(Dbg) Then ExtDbgLog, ToS(I)+" "+ToS(J)+" "+ToS(SkyMode)+" "+ToS(SkySig)
     EndFor
  EndFor
  Sigma_Bck = Sigma_Bck/(Nx*Ny)
  If KeyWord_Set(Dbg) Then DebugLog, "Average sky sigma: "+strtrim(Sigma_Bck)
  ;
  ; Now fit 2nd order poly in X,Y to estimated sky level
  ;
  sf = SFit(bi,2)
  ;
  ; Interpolate to original image size and return result
  ;
  Return, ConGrid(sf,Sx,Sy,/interp)
End
