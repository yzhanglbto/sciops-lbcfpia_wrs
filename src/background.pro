;+
; Estimate background by calculating image histogram, finding max of
; histogram and fitting a parabola around it.
; Background is given by the "right" intercept of parabola with
; X axis.
;
; @param Ima {in}{required} Image for which background has to be
;    estimated.
; @keyword dbg {in}{optional}{type=boolean} If set, additional output is produced.
; @returns Estimated value of background
; @author Andrea Baruffolo, INAF - OAPd
;-
Function Background, Ima, dbg=dbg
;
; Use binsize eq 3 to filter out "spikes" in histogram
;
BinSize = 3
hi = histogram(Ima, binsize=3, omin=MinH, omax=MaxH)
;
; Location of histogram maximum
;
qv = max(hi,qvPos)

If KeyWord_Set(dbg) Then Begin
    Window,/Free
    ExtDbgLog,"Max at "+ToS(qvPos)
    NPts=100
    strtI = qvPos-Npts+1
    If strtI LT 0 Then strtI = 0
    endI = qvPos+Npts
    ExtDbgLog,"strtI="+Tos(strtI)+", endI="+ToS(endI)
    NelHi = Size(hi, /N_Elements)
    If endI GE NelHi Then endI = NelHi-1
    ;; Plot, IndGen(2*NPts)-NPts+qvPos+1,hi[strtI:endI]
    NPlot = endI-strtI+1
    XPlot = IndGen(NPlot)+strtI
    Plot, XPlot,hi[strtI:endI]
EndIf
;
; Select part of histogram above a certain threshold
;
xx = where(hi GE 0.5*qv)
If KeyWord_Set(dbg) Then ExtDbgLog,String("N pts above 0.5 max: ",Size(xx,/N_Elements))
;
; Fit a second order polynomial
;
A = Poly_Fit(xx, hi(xx), 2, YFit=YFit)

If KeyWord_Set(dbg) Then Begin
    OPlot, xx, YFit, PSym=1
Endif
;
; Find roots
;
S1 = (-a[1]+Sqrt(a[1]*a[1]-4*a[0]*a[2]))/(2*a[2])
S2 = (-a[1]-Sqrt(a[1]*a[1]-4*a[0]*a[2]))/(2*a[2])
;
; Select the "righmost" solution
;
Res = S1
If S2 GT S1 Then Res = S2

If KeyWord_Set(dbg) Then Begin
    ExtDbgLog, ToS(A)
    ExtDbgLog, ToS(S1)
    ExtDbgLog, ToS(S2)
    ExtDbgLog, "Bck between: "+ToS(S1*BinSize+MinH)+", and: "+ToS(S2*BinSize+MinH)
    OPlot,[Res,Res],[0,qv]
Endif

BackGround = Res*BinSize+MinH

Return, BackGround
End
