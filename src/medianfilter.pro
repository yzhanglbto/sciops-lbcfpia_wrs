Function MedianFilter, Ima, NSigma=NSigma, BoxSize=BoxSize, Dbg=Dbg

If Not KeyWord_Set(NSigma) Then NSigma=7
If Not KeyWord_Set(BoxSize) Then BoxSize=5

inIma = Ima
mIma = Median(inIma, BoxSize)
Diff = inIma-mIma
md = Median(Diff)
Diff -= md
sg = Sqrt(Total(Diff*Diff)/N_Elements(Diff))
Q = Where(Diff LT -NSigma*sg OR Diff GT NSigma*sg)
If (Size(Q))[0] NE 0 Then Begin
    If KeyWord_Set(DBG) Then Print, "Points substituted: ",N_Elements(Q)
    inIma[Q] = mIma[Q]
EndIf

Return,inIma
End
