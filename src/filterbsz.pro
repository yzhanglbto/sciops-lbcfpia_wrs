Function FilterBSz, inBsz, moments=m

bsz = inBsz

Q = Size(bsz)
If Q[0] NE 1 Then Message, "Input array must be 1-dimensional"
;
; If we are passed one size only, it makes no sense to do any filtering
;
If Q[1] LT 2 Then Begin
    Print, "WARNING: 1 star only used for computation!!"
    Return, bsz
EndIf

;; Print, "Input: ",bsz

While 1 Do Begin
    m = Moment(bsz)
    lim = Sqrt(m[1])
    ;; Print,"mean, stddev: ",m[0],lim
    If lim LT m[0]/15. Then Break ;; If std dev gets small, we're satisfied
    lim *= 2
    Q = Where(bsz GE m[0]-lim AND bsz LE m[0]+lim, Complement=NQ)
    snq = Size(NQ)
    If snq[0] NE 0 Then Begin
        ;; Print, "Rejected: ", bsz[NQ]
        bsz = bsz[Q]
    Endif Else Break
EndWhile

;; Print, "Output: ",bsz

Return, bsz
End
