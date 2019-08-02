Function tryCentroid, Pup, Sx, Sy, Cx, Cy, MeanR,dbg=dbg

D = Shift(Dist(Sx,Sy),Cx,Cy)
Q = Where(D LT meanR*0.3,Complement=NQ)
D[Q] = 1
If Size(NQ,/N_Dimens) GT 0 Then D[NQ] = 0

mskPup = Pup/LeeFilt(Pup,Sx/3)*D
IMax = Max(mskPup)
If KeyWord_Set(Dbg) Then ExtDbgLog,"IMax = "+ToS(IMax)

mskPup[Q] = IMax-mskPup[Q]

TraceX = Total(mskPup,2)/Sy
TraceY = Total(mskPup,1)/Sx

If KeyWord_Set(Dbg) Then ExtDbgLog,String("Input Cntr: ",Cx,Cy)
If KeyWord_Set(Dbg) Then ExtDbgLog,"Barycenter: "+$
                               String(Total(TraceX*FIndGen(Sx))/Total(TraceX),Total(TraceY*FIndGen(Sy))/Total(TraceY))

Q = Where(TraceX GT 0)
ResX = Poly_Fit(Q,TraceX[Q],2,yFit=yFit)
Q = Where(TraceY GT 0)
ResY = Poly_Fit(Q,TraceY[Q],2,yFit=yFit)

xMax = -ResX[1]/(2.0*ResX[2])
yMax = -ResY[1]/(2.0*ResY[2])
If KeyWord_Set(Dbg) Then ExtDbgLog,String("Parab. Fit: ",xMax,yMax)
;
; Just for fun, compute FWHM of parabola.
; (It is comparable to dimension of fitted circle)
;
If 0 Then Begin
    a = ResX
    d = (a[2]*xMax+a[1])*xMax+a[0]
    a[0] -= d/2.0
    Wx = (-a[1]-Sqrt(a[1]*a[1]-4*a[0]*a[2]))/(2*a[2]) - (-a[1]+Sqrt(a[1]*a[1]-4*a[0]*a[2]))/(2*a[2])
    a = ResY
    d = (a[2]*yMax+a[1])*yMax+a[0]
    a[0] -= d/2.0
    Wy = (-a[1]-Sqrt(a[1]*a[1]-4*a[0]*a[2]))/(2*a[2]) - (-a[1]+Sqrt(a[1]*a[1]-4*a[0]*a[2]))/(2*a[2])
    ExtDbgLog,"Avg Radius="+ToS((Wx+Wy)/4.0)
EndIf

Return,[xMax,yMax]
End
