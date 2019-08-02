Function TraceSaddlePt, Trace, strtPt
;
; trace must be 1-dim array
;
LastX = Size(Trace,/N_Elements)-1
I = strtPt
While I GT 0 Do Begin
    If Trace[I-1] LE Trace[I] Then Break
    I--
EndWhile
strtX = I

I = strtPt
While I LT LastX Do Begin
    If Trace[I+1] LE Trace[I] Then Break
;;        Print,I,Trace[I-1],Trace[I],Trace[I+1]
    I++
EndWhile
;; print,strtPt,strtX,I

;
; little "trick": if we didn't find a turning point in X+ or X- then
; estimate size as twice ....
;
;;; Print,"TraceSaddlePt: strtX,I,strtPt,LastX",strtX,I,strtPt,LastX
Width = I-strtX
If strtX LT 2 And I GT LastX-2 Then Return,0
If strtX EQ 0 Then Return, 2*(I-strtPt)
If I EQ LastX Then Return, 2*(strtPt-strtX)
Return,Width
End
