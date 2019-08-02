; Add documentation please!!!
; 
; Fits an ellipse to the external border of the 'blob'
; then computes the rms of the deviation of the fitted
; ellipse from a circle.
;
Function AnaBlob, inBlob

Blob=inBlob
Q = Size(inBlob)
Sx = Q[1]
Sy = Q[2]

;
; Compute edge of blob
;
rb = Sobel(inBlob)

;
; Now find only "external" edge, by finding point
; with minumum X coordinate and following edge
; using search2d
;
rbQ = Where(rb GT 0)
mxrb = Max(rb)
rbQX = rbQ MOD Sx
mrbQX = Min(rbQX,QQ)
rbQ = Search2D( rb, mrbQX, rbQ[QQ]/Sx, 0.9, 1.1*mxrb )

rbX = rbQ MOD Sx
rbY = rbQ / Sx

;
; Fit ellipse to points
;
P = mpFitEllipse(rbX,rbY,Weights=rb[rbQ],/Tilt,/Quiet)

rbX -= P[2]
rbY -= P[3]

ct = Cos(P[4])
st = Sin(P[4])

XX = rbX*ct-rbY*st
YY = rbX*st+rby*ct

Theta = ATan(YY,XX)
fX = P[0]*Cos(Theta)
fY = P[1]*Sin(Theta)

Delta = Sqrt(fX*fX+fY*fY)-Sqrt(XX*XX+YY*YY)
NPts = N_Elements(XX)
;; Print,NPts
StdDev = Sqrt(Total(Delta*Delta)/NPts)

Return, stdDev
End
;
; =====================================================================
;
;
;
Function AnaBlobs, inBlobs

Blobs=inBlobs
Q = Size(inBlobs)
Np = Q[1]
Sx = Q[2]
Sy = Q[3]

stdDev = FltArr(Np)

For I=0,Np-1 Do Begin
    ;
    ; Compute edge of blob
    ;
    stdDev[I] = AnaBlob(Reform(inBlobs[I,*,*],Sx,Sy))
EndFor

Return, stdDev
End
