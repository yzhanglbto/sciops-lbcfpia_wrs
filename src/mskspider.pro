;+
; Create a mask for the spider
;
; @param Sx {in}{required} Size of mask in X
; @param Sy {in}{required} Size of mask in Y
; @param cX {in}{required} Center of mask in X
; @param cY {in}{required} Center of mask in Y
; @param meanR {in}{required} Mean radius of pupil
; @param RotAngle {in}{required} Rotation angle
; @keyword Intra {in}{optional}{type=boolean} If set, mask must be
; generated fro intra-focal pupil.
;
; @returns Mask for the spider
;
; @history Documentation (IDLDoc) added on May 16, 2006
; @author Andrea Baruffolo, INAF - OAPd
;-
Function mskSpider, Sx, Sy, cX, cY, meanR, RotAngle, EstW=EstW, Intra=Intra

XX = (findgen(sx)-sx/2)#replicate(1,sx)
YY = Transpose((findgen(sy)-sy/2)#replicate(1,sy))

msk = ATan(YY,XX)

w = 60.0/180.0*!PI

Q = Where(msk LT -w/2 OR msk GT w/2, Complement=NQ)
msk[Q] = 1
msk[NQ] = 0

D = Shift(Dist(Sx,Sy),Sx/2,Sy/2)
;;; Q = Where(D LT meanR*0.33,Complement=NQ)
If KeyWord_Set(EstW) Then Begin
    Q = Where(D LT estW*0.6,Complement=NQ)
Endif Else Begin
    Q = Where(D LT meanR*0.3,Complement=NQ)
EndElse
D[Q] = 1
If Size(NQ,/N_Dimens) GT 0 Then D[NQ] = 0

msk *= D

ra = 90.0+RotAngle
If Not KeyWord_Set(Intra) Then ra += 180.0

msk = rot(msk, -ra, /interp)
;
; Note: sign of shift has been verified!
;
Return, Shift(msk,Cx-Sx/2,Cy-Sy/2)
End
