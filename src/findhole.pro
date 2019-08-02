Function FindHole, Pup, Sx, QIn, SmoothedPupil=SmoothedPupil, DoPlot=DoPlot, Nrc=Nrc

Cx = 0
Cy = 0

If N_Elements(Sx) EQ 0 Then Sx = 80

If N_Elements(QIn) EQ 0 Then Begin
    D = Shift(Dist(80,80),40,40)
    QIn = Where(D LT 0.333*35)
EndIf

smoothSz = 15
smoothedPupil = Pup/Smooth(Pup,smoothSz)
;;; medianedPupil = Median(Pup,3)/Smooth(Pup,smoothSz)
dummy = Min(smoothedPupil[QIn],i)
minPos = QIn[i]
;
; First estimation of center
;
Cx = minPos MOD Sx
Cy = minPos / Sx

;; Print, "First estimate Cx,Cy: ",Cx,Cy

If Not KeyWord_Set(Nrc) Then Nrc = 12

x1 = Cx-Nrc
If x1 LT 0 Then x1=0
x2 = Cx+Nrc
If x2 Ge Sx Then x2 = Sx-1

y1 = Cy-Nrc
If y1 LT 0 Then y1=0 
y2 = Cy+Nrc
If y2 Ge Sx Then y2 = Sx-1

TraceX = Total(smoothedPupil[x1:x2,y1:y2],2)
TraceY = Total(smoothedPupil[x1:x2,y1:y2],1)

If Keyword_Set(DoPlot) Then Begin
   Plot,TraceX,/YNoZ
   Oplot,TraceY
EndIf

;
; Initial point used to estimate location and width of "saddle"
;
ix = N_Elements(TraceX)/2
iy = N_Elements(TraceY)/2

Cx = Cx-Nrc+ix
Cy = Cy-Nrc+iy
Wx = TraceSaddlePt(TraceX,ix)
Wy = TraceSaddlePt(TraceY,iy)

;; Print,"Cx,Cy,Wx,Wy:" ,Cx,Cy,Wx,Wy

Return,[Cx,Cy,Wx,Wy]
End
