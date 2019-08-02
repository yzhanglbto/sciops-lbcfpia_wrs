Function getBlobSz, blob, Frac=Frac
;;;;Help,blob
If Not KeyWord_Set(Frac) Then Frac=0.15

sz = [0.,0.]
For I=0,1 Do Begin
    prj = Total(Float(blob),I+1)
    mxp = Max(prj, Min=mnp)
    maxK = N_Elements(prj)-1
    thr = mnp+Frac*(mxp-mnp)
    K = 0
    While prj[K] LE thr Do Begin
        K++
        If K EQ maxK Then Break
    Endwhile
    x1 = K
    K = maxK
    While prj[K] LE thr Do Begin
        K--
        If K EQ 0 Then Break
    Endwhile
    sz[I] = K-x1+1
EndFor

;;TVScl,blob
;;Wait,0.5
Return, (sz[0]+sz[1])/2.
End
