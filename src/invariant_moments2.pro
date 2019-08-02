;
; Test, e.g., with the astron library
;    g=psf_gaussian(NPixel=80,FWHM=20)
;    print, invariant_moments(g)
;       m00 = 453.234
;       m10 =  40.0
;       m01 =  40.0
;
Function Invariant_Moments2, Ima

  Q = Size(Ima)
  If Q[0] NE 2 Then Message, "Input must be a 2-d array"

  NX = Q[1]
  NY = Q[2]

  ;
  ; NOTE: we set the center of a pixel at 0.5, 0.5
  ;
  XX = (FIndGen(NX)+0.5)#Replicate(1.0,NY)
  YY = Replicate(1.0,NX)#(FIndGen(NY)+0.5)

  ; Print, XX
  ; Print, YY

  m00 = Total(Ima) ; Total "mass"
  ;; CHECK that m00 is not 0

  m10 = Total(XX*Ima)
  m01 = Total(YY*Ima)

  x_c = m10/m00
  y_c = m01/m00

  ; Print, "Centroid: ", x_c, y_c

  XX_c  = XX-x_c
  YY_c  = YY-y_c

  XX_c2 = XX_c*XX_c
  YY_c2 = YY_c*YY_c

  XX_c3 = XX_c2*XX_c
  YY_c3 = YY_c2*YY_c

  eta_11 = Total(XX_c*YY_c*Ima)

  mu_20 = Total(XX_c2*Ima)
  mu_02 = Total(YY_c2*Ima)

  mu_30 = Total(XX_c3*Ima)
  mu_03 = Total(YY_c3*Ima)

  mu_11 = Total(XX_c*YY_c*Ima)
  mu_21 = Total(XX_c2*YY_c*Ima)
  mu_12 = Total(XX_c*YY_c2*Ima)

  ;Print, "mu_20: ", mu_20
  ;Print, "mu_02: ", mu_02

  m00_2 = m00*m00
  eta_11 = mu_11/m00_2
  eta_20 = mu_20/m00_2
  eta_02 = mu_02/m00_2

  m00_3 = m00_2*m00
  eta_30 = mu_30/m00_3
  eta_21 = mu_21/m00_3
  eta_12 = mu_12/m00_3
  eta_03 = mu_03/m00_3

  ;
  ; And, finally, the invariant moments!
  ;
  M1 = eta_20+eta_02
  M2 = (eta_20-eta_02)^2+4.0*eta_11^2
  M3 = (eta_30-3.0*eta_12)^2+(3.0*eta_21-eta_03)^2
  M4 = (eta_30+eta_12)^2+(eta_21+eta_03)^2
  M5 = (eta_30-3.0*eta_12)*(eta_30+eta_12)+((eta_30+eta_12)^2-3.0*(eta_21-eta_03)^2)+ $
    (3.0*eta_21-eta_03)*(eta_21+eta_03)*(3.0*(eta_30+eta_12)^2-(eta_21+eta_03)^2)
  M6 = (eta_20-eta_02)*((eta_30+eta_12)^2-(eta_21+eta_03)^2)+4.0*eta_11*(eta_30+eta_12)*(eta_21+eta_03)
  M7 = (3*eta_21-eta_03)*(eta_30+eta_12)*((eta_30+eta_12)^2-3.0*(eta_21+eta_03)^2)+ $
    (3.0*eta_12-eta_30)*(eta_21+eta_03)*(3.0*(eta_12+eta_30)^2-(eta_21+eta_03)^2)

  Return, [M1, M2, M3, M4, M5, M6, M7]
End
