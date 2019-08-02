;
; Make mask to measure Z22
;
Function mkZ22Mask, Size, Radius, Cx, Cy, Type, RBidon=RBidon

Sz_2 = Size/2
If Not KeyWord_Set(RBidon) Then Begin
   InRadius = Radius/Sqrt(2.0)
   RBidon = Radius/5.0
Endif Else Begin
   InRadius = Sqrt((Radius*Radius+RBidon*RBidon)/2.)
Endelse

Delta = (Radius-RBidon)/3
R1 = RBidon+Delta
R2 = R1+Delta
R3 = Radius

Disk = Shift(Dist(Size),Sz_2,Sz_2)
If N_Elements(Type) GE 0 Then Begin

    rMask = 1.0*(Disk LE Radius)
    If Type EQ 1 Then Begin
        rMask[0:Sz_2-1,*] *= -1.0
        If Type EQ 1 Then rMask = Rotate(rMask,1)
    EndIf

    If Type EQ 2 Then Begin
        q = Where(Disk LE InRadius)
        rMask *= -1.0
        rMask[q] = 1.0
    EndIf

    If Type EQ 3 Then Begin
        q = Where(Disk LT RBidon)
        rMask[q] = 0.0
        q = Where((Disk GT R1) And (Disk LE R2))
        rMask[q] = 0.0
    EndIf


Endif

Return, Shift(rMask,Cx-Sz_2,Cy-Sz_2)
End
