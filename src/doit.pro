Pro DoIt, p, sobel=sobel

q = Size(p,/Dimensions)
sx = q[0]
sy = q[1]
For I=1,8 Do Begin
	t = p GT I*Max(p)/10.0
	If KeyWord_Set(Sobel) Then t = Sobel(t)
	TvScl,t, sx * ((I-1) MOD 4), (I-1)/4*sy
	Print,Invariant_Moments(t)
EndFor

Return
End
