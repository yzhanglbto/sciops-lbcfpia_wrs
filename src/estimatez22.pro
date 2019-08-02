Function estimateZ22, pup, xc, yc, inR, outR, m1=m1, m2=m2, m3=m3, dbg=dbg

Size = (Size(Pup,/Dimensions))[0]
Sz_2 = Size/2

delta = (outR-inR)/3
R1 = inR+delta
R2 = R1+Delta

Disk = Shift(Dist(Size),Sz_2,Sz_2)

q1 = Where((Disk GT inR) AND (Disk LE R1))
N1 = Size(q1,/N_Dimensions)
q2 = Where((Disk GT R1) AND (Disk LE R2))
N2 = Size(q2,/N_Dimensions)
q3 = Where((Disk GT R2) AND (Disk LE outR))
N3 = Size(q3,/N_Dimensions)

;;;Print,'N1 = '+ToS(N1)+', N2 = '+ToS(N2)+', N3 = '+ToS(N3)

If ( (N1 LT 1) OR (N2 LT 1) OR (N3 LT 1)) Then Begin
    LogAllWays,"Not enough points for estimating Z22"
    Return, 0.0
EndIf

If KeyWord_Set(dbg) Then Begin
  m1 = FltArr(Size,Size)
  m2 = FltArr(Size,Size)
  m3 = FltArr(Size,Size)
  m1[q1] = 1.
  m2[q2] = 1.
  m3[q3] = 1.
EndIf

NTPQ1 = Total(pup[q1])/N1
z22 = (Total(pup[q3])/N3-NTPQ1)/(Total(pup[q2])/N2-NTPQ1)

Return, z22
End
