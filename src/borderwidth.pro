;
; Measure the "width" of a "border", passed as input image,
; whose shape is assumed to be elliptical, with fitted ellipse
; parameters given by FitElP
;
; Uses: unfold
;
Function BorderWidth, BordImg, FitElP, $
                      Dist=Dist, NSamples=NSamples, NPts=NPts, Dbg=Dbg

  If Not KeyWord_Set(Dist)     Then Dist = 20.
  If Not KeyWord_Set(NSamples) Then NSamples = 128
  If Not KeyWord_Set(NPts)     Then NPts = 64
  UImg = UnFold(BordImg, FitElP, Dist, NSamples, NPts)
  Tr = Total(UImg,1)
  Res = GaussFit(FindGen(N_Elements(Tr)),Tr,A,NTerms=5)
  If KeyWord_Set(Dbg) Then Begin
     tvscl,UImg
     Plot, Tr, /NoErase
     OPlot, Res, PSym=1
  EndIf

Return, A[2]*2.3548
End
