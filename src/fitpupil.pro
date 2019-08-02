;+
; Fits ellipses on inner and outer border of pupil, returns a few
; parameters that are proportional to the aberrations (see "Return 
; Value", below).
;
; @param Pup {in}{required} Two dimensional array containing 1 image of
;    the pupil to be fitted.
; @param RA {in}{required} Rotator angle
; @keyword Intra {in}{optional}{type=boolean} If set then image of
;    pupil supplied in Pup parameter is intra-focal.
; @keyword DoExtThr {in}{optional}{type=boolean} If set then
;    thresholding is applied to the external edge of the pupil.
; @keyword Interior {out}{optional} Used for debugging. Set to the
;    name of a variable that will contain the "internal" part of the
;    pupil (i.e. that containing the central hole).
; @keyword outBX {out}{optional} Set to computed X coordinates of
;    outer border. Used for overplotting fitted border to image (for
;    display and debug)
; @keyword outBY {out}{optional} Set to computed Y coordinates of
;    outer border. Used for overplotting fitted border to image (for
;    display and debug)
; @keyword inBX {out}{optional} Set to computed X coordinates of
;    inner border. Used for overplotting fitted border to image (for
;    display and debug)
; @keyword inBY {out}{optional} Set to computed Y coordinates of
;    inner border. Used for overplotting fitted border to image (for
;    display and debug)
; @keyword OuterEllipseFit {out}{optional} Set to the array of 
;    parameters of the ellipse fitted to the outer border of the
;    pupil
; @keyword InnerEllipseFit {out}{optional} Set to the array of 
;    parameters of the ellipse fitted to the inner border of the
;    pupil
; @keyword PupEdge {out}{optional} A two-dimensional array (image)
;    of the "inner"/"outer" borders of the input pupil. In practice
;    this is the 'Sobel' of the input pupil after some pre-processing.
; @keyword DoDisp {in}{optional}{type=boolean} If set then the
;    procedure displays some images while processing data. In general
;    the result is messy and not very useful, unless you really know
;    how the procedure works.
;
; @returns Array of fitted coefficients:
;    <Table>
;    <TR><TH Align="Center">Index</TH><TH>Parameter</TH></TR>
;    <TR><TD Align="Right"> 0</TD><TD>Difference in position of center for
;    outer/inner ellipse, X component</TD></TR>
;    <TR><TD Align="Right"> 1</TD><TD>Difference in position of center for
;    outer/inner ellipse, Y component</TD></TR>
;    <TR><TD Align="Right"> 2</TD><TD>Major minus minor axis, outer ellipse</TD></TR>
;    <TR><TD Align="Right"> 3</TD><TD>Sum of major plus minor axis, outer ellipse</TD></TR>
;    <TR><TD Align="Right"> 4</TD><TD>Sum of major and minor axis for inner ellipse</TD></TR>
;    <TR><TD Align="Right"> 5</TD><TD>Rotation angle (in degrees, between 0 and 180)</TD></TR>
;    <TR><TD Align="Right"> 6</TD><TD>Normalized difference in flux btw "right" and
;    "left" part of pupil</TD></TR>
;    <TR><TD Align="Right"> 7</TD><TD>Normalized difference in flux btw "upper" and
;    "lower" part of pupil</TD></TR>
;    <TR><TD Align="Right"> 8</TD><TD>Normalized difference in flux btw "inner" and
;    "outer" part of pupil</TD></TR>
;    <TR><TD Align="Right"> 9</TD><TD>Ratio of avg flux in "inner" part of pupil over "outer"</TD></TR>
;    <TR><TD Align="Right">10</TD><TD>Ratio of decentering of pupils over pupil size
;    (avg "outer" radius)</TD></TR>
;    <TR><TD Align="Right">11</TD><TD>Ratio of average radius of inner ellipse over
;    average radius of outer ellipse</TD></TR>
;    </Table>
;
; @history 16/May/2006: Documentation (IDLDoc) added.<BR>
; @history 21/Nov/2007: Error handling around Line 119 (mask for external borders)<BR>
; @history 26/Nov/2007: Added Inner/OuterEllipseFit and PupEdge
; keywords. Improved documentation.<BR>
; @author Andrea Baruffolo, INAF - OAPd
;-
Function FitPupil, Pup, RA, Intra=Intra, DoExtThr=DoExtThr, DoIntFit=DoIntFit, Interior=Interior, $
                   outBX=outBx, outBY=outBY, inBX=inBX, inBY=inBY, DoDisp=DoDisp, $
                   estOR=estOR, OuterEllipseFit=POut, InnerEllipseFit=PIn, PupEdge=sP, Dbg=Dbg, $
                   oldInit=oldInit, Red=Red, OldPhot=OldPhot, tec=tec, chan=chan

If Not KeyWord_Set(NIters) Then NIters = 0

Sz = Size(Pup,/Dimensions)
Sx = Sz[0]
Sx_2 = Sx/2
Sy = Sz[1]
Sy_2 = Sy/2

If KeyWord_Set(DoDisp) Then Begin
    SaveWin = !D.Window
    Window,/Free,Retain=2,XSize=5*Sx,YSize=2*Sy
EndIf

Pup = MedianFilter(Pup)

;;Print,minmax(pup)
;
;
;
msz = Sx_2
Q = Where(Shift(dist(Sx),msz,msz)/msz GT 1.0)
;
; This used to set the outer part of image to its "background"
; doesn't work well when part of another star is in the field
; Pup[Q] = Median(Pup[Q])
;
; Find borders of pupil using Sobel
;
sP = Sobel(Pup)

;
; Determine approximate center and avg radius of pupil
;
mPup = Pup
;;mPup[Q] = Median(Pup)
;;bckP = Median(mPup)
; use same minflux as in findpupil


If 0 Then Begin
   bckP=50
   If KeyWord_Set(oldInit) Then Begin
      mxp = max(mPup*mkmsk(Sx,Sy,Sx_2,Sy_2,Sx_2-3),mq)
;;;; msk0 = mPup GT (bckP+0.15*(mxp-bckP))
      msk0 = mPup GT (bckP+0.10*(mxp-bckP))
      If KeyWord_Set(Dbg) Then Begin
         ExtDbgLog, String("backP: "+ToS(bckP)+", Thre: "+ToS(bckP+0.10*(mxp-bckP)))
      EndIf

      If KeyWord_Set(DoDisp) Then TVScl, msk0

; This could be starting from a point set to '0'
;;; Q = Search2D(msk0,Sx/4+Sx/8,Sy/2,0.5,1)

      strtX = mq MOD Sx
      strtY = mq / Sx
      Q = Search2D(msk0,strtX,strtY,0.5,1)
   Endif Else Begin
      mxp = max(mPup*mkmsk(Sx,Sy,Sx_2,Sy_2,Sx_2*0.75),mq)
      cent_mask = mkmsk(Sx,Sy,Sx_2,Sy_2,12)
      dummy = (mPup GT (bckP+0.10*(mxp-bckP))) OR cent_mask
   ;;;If KeyWord_Set(DoDisp) Then TVScl, dummy
      Q = Search2d(dummy,Sx_2,Sy_2,0.5,1)
   EndElse
   
   minX = Min(Q Mod Sx, max=maxX)
   cenX = Fix((minX+maxX)/2.0+0.5)
   radX = Fix((maxX-minX)/2.0+0.5)
   
   minY = Min(Q/Sx, max=maxY)
   cenY = Fix((minY+maxY)/2.0+0.5)
   radY = Fix((maxY-minY)/2.0+0.5)
   
   rad = (RadX+RadY)/2.
   If RadX GT RadY Then Begin
      inR = RadY
      exR = RadX
   Endif Else Begin
      inR = RadX
      exR = RadY
   Endelse
   If KeyWord_Set(Dbg) And KeyWord_Set(DoDisp) Then Begin
                                ;myMask = msk0 * 0.0
                                ;myMask[Q] = 1.0
                                ;TvScl, myMask
      PlotS,[minX,maxX,maxX,minX,minX],[minY,minY,maxY,maxY,minY],/Device
   EndIf

   If KeyWord_Set(Dbg) Then Begin
      ExtDbgLog,"FitPupil, parameters of mask for external border:"
      ExtDbgLog,"minX: "+ToS(minX)+", maxX: "+ToS(maxX)+", minY: "+ToS(minY)+", maxY: "+ToS(maxY)
      ExtDbgLog,"cenX: "+ToS(cenX)+", cenY: "+ToS(cenY)
      ExtDbgLog,"radX: "+ToS(radX)+", radY: "+ToS(radY)+",rad: "+ToS(rad)
      ExtDbgLog,"inR: "+ToS(inR)+",exR: "+ToS(exR)
   EndIf
Endif Else Begin
   cenX = Sx_2
   cenY = Sy_2
   EllP = [(Sx-6)/4,(Sy-6)/4,CenX,CenY,0]
   dst  = (Sx-6)/4.
   NPts = 64
   NSamples = 128
   up = Unfold(mPup,EllP,dst,NSamples,NPts)
   scl = 2*dst/Npts
   prj = Total(up,1)/NSamples
   thr = Max(prj,IMax)/2.
   For I=IMax,N_Elements(prj)-1 Do Begin
      If prj[I] LT thr Then Break
   Endfor
   dx = I*scl
   radX = dx
   radY = dx
   rad = dx
   inR = dx
   exR = dx
   minx=EllP[2]-dx
   maxx=EllP[2]+dx
   miny=EllP[3]-dx
   maxy=EllP[3]+dx
   If KeyWord_Set(Dbg) And KeyWord_Set(DoDisp) Then Begin
      PlotS,[minX,maxX,maxX,minX,minX],[minY,minY,maxY,maxY,minY],/Device
   EndIf
Endelse

;
; determine mask for external border
;
D = Shift(Dist(Sx,Sy),cenX,cenY)

;;; Q = Where(D LT exR+1 AND D GT inR-8,complement=NQ)
;;; Q = Where(D GT inR-10,complement=NQ)
Q = Where(D GT inR-8 AND D LT 1.2*rad,complement=NQ)
If Size(Q,/Dimensions) Eq 0 Then Return, -1

QIn = Where(D LT 0.333*rad,complement=NQIn)

msk = Pup
msk[Q] = 1
If Size(NQ,/N_Dimensions) GT 0 Then msk[NQ] = 0
;
; Pupils on blue tech chip show vignetting.
; We modify the mask for the outer diameter in order to
; not fit the 'distorted' part of the pupil.
;
If KeyWord_Set(Tec) And chan EQ 'blue' Then Begin
   tbmsk = MkTechBlueMask(Sx, Sy, cenX, cenY, 100.) ;;; <=== not a cfg parameter yet!
   msk = msk * tbmsk
EndIf

If KeyWord_Set(DoDisp) Then Begin
                             ;TVScl,Pup
   Tvscl,sP,Sx,0
   TVScl,msk,Sx*2,0
Endif


;
; apply mask for external border
; compute x,y for each nonzero pixel
; fit ellipse using (gradient) intensity as weight
;
; was just img = msk*sP
msP = Median(sP*msk)
minVal = (msP+0.1*(Max(sp*msk)-msP))
sP1 = (sP > minVal)-minVal
img = msk*sP1
;;; Print,"img: ",minmax(img)
UseSearch2D = 0
If Not KeyWord_Set(DoExtThr) Then Begin
    If UseSearch2D EQ 0 Then Begin
;;;        Print,"***************************"
        Q = Where(img GT 0)
    Endif Else Begin
;;    mni=min(img,qpos,max=mxi)
        mxi=max(img,qpos)
        Q = Search2D(img,qpos MOD Sx, qpos/Sx, 0.21*mxi, mxi)
        img = img*0.
        img[q] = 1.0
    EndElse
Endif Else Begin
    mni=min(img,qpos,max=mxi)
    ;;;Print,"Thr ext sobel: ",mni+0.333*(mxi-mni)
    Q = Where(img GT mni+0.21*(mxi-mni),Complement=NQ)
    img[NQ] = 0
Endelse
If KeyWord_Set(DoDisp) Then TVScl,img,Sx*3,0
;
; Fit external border
;
X = Float(Q Mod Sx)
Y = Float(Fix(Q/Sx))
W = img[Q]
P = mpFitEllipse(X,Y,/Tilt,Weights=W,/Quiet)

POut = P
;Print,P

;
; Plot a few points of the fitted ellipse
;
NPts=51
phi = DIndGen(NPts)*2D*!dpi/(NPts-1)

xe=p[0]*Cos(phi)
ye=p[1]*Sin(phi)

t = p[4]

xr =  xe*cos(t)+ye*sin(t)
yr = -xe*sin(t)+ye*cos(t)

xr = p[2]+xr
yr = p[3]+yr

If KeyWord_Set(Dbg) Then Begin
   ExtDbgLog, String("Ellipse center: ",p[2],p[3])
   ExtDbgLog, String("Min/Max axis: ",p[0],p[1])
EndIf

outBX = xr
outBY = yr

;;;; print,xr
;;;; print,yr

If KeyWord_Set(DoDisp) Then Begin
    PlotS,xr,yr,psym=3,/device
    PlotS,Sx*3+xr,yr,psym=3,/device
EndIf
; WShow

;
; From fitted ellipse parameters compute first part
; of parameters used in computation of aberrations
;
Res = FltArr(13)

If P[0] GT P[1] Then Begin
    maxEx = P[0]
    minEx = P[1]
Endif Else Begin
    maxEx = P[1]
    minEx = P[0]
    P[4] = P[4]+!DPI/2.0
EndElse

;;; Print,"P[4] = ",P[4]*180./!DPI

Res[2] = maxEx-minEx
Res[3] = P[0]+P[1]
Res[5] = (P[4]*180./!DPI) MOD 180

;;Print,Res
;

;
; =============================================================================
;                      Now we fit the central "hole"
; =============================================================================
;

;
; Find location of central "hole" in pupil
;
;;;; smoothSz = Long(Sqrt(Sx)+0.5)

If 0 Then Begin
    UseSmooth = 0
    smoothSz = 15
    smoothedPupil = Pup/Smooth(Pup,smoothSz)
    dummy = Min(smoothedPupil[QIn],i)
    minPos = QIn[i]

    If UseSmooth GT 0 Then Begin
        Cx = minPos MOD Sx
        Cy = minPos / Sx
    Endif

    If UseSmooth EQ 0 Then Begin
        Nrc = 10
    ;;; TraceX = Total(Pup[*,Sy_2-Nrc:Sy_2+Nrc],2)
    ;;; TraceY = Total(Pup[Sx_2-Nrc:Sx_2+Nrc,*],1)
        TraceX = Total(smoothedPupil[*,Sy_2-Nrc:Sy_2+Nrc],2)
        TraceY = Total(smoothedPupil[Sx_2-Nrc:Sx_2+Nrc,*],1)
        Cx = TraceSaddlePt(TraceX)
        Cy = TraceSaddlePt(TraceY)
    Endif
Endif

Cxy = FindHole(Pup,Sx,QIn,smoothedPupil=smoothedPupil)
Cx = Cxy[0] ; Est. X Position of central hole
Cy = Cxy[1] ; Est. Y Position of central hole
Wx = Cxy[2] ; Est. width in X of central hole
Wy = Cxy[3] ; Est. width in Y of central hole

;Print,"FindHole: Cxy = ",Cxy
;
; estimated width the largest of the two
; unless one is zero that means that hole is really "large" so we use
; mean radius in mskspider as size of mask
;
EstW = (Wx GT Wy) ? Wx : Wy
If Wx Eq 0 OR Wy EQ 0 Then EstW = 0

;
; draw a cross on the estimated center
;
crossSz = 3
If KeyWord_Set(DoDisp) Then Begin
    PlotS,[Cx,Cx],[Cy-crossSz,Cy+crossSz],/device
    PlotS,[Cx-crossSz,Cx+crossSz],[Cy,Cy],/device
    TVScl,smoothedPupil,Sx*4,0
    PlotS,Sx+[Cx,Cx],[Cy-crossSz,Cy+crossSz],/device
    PlotS,Sx+[Cx-crossSz,Cx+crossSz],[Cy,Cy],/device
    PlotS,Sx*4+[Cx,Cx],[Cy-crossSz,Cy+crossSz],/device
    PlotS,Sx*4+[Cx-crossSz,Cx+crossSz],[Cy,Cy],/device
EndIf

;;; D = Shift(Dist(Sx,Sy),cX,cY)

meanR = Res[3]/2

;Mask = D
;Q = Where(D LT meanR*0.333,Complement=NQ)
;Mask[Q] = 1
;Mask[NQ] = 0

;; Print,"mskSpider: Sx,Sy,Cx,Cy.. ",Sx,Sy,Cx,Cy

MaxIters = 10
NIter = 0
;;;;;;; Repeat Begin

    Mask = mskSpider(Sx,Sy,Cx,Cy,meanR,RA,Intra=Intra) ;;;;;;;;;;; ,EstW=EstW)

    If KeyWord_Set(DoDisp) Then  TvScl, Mask, 0, Sy
    nImg = Mask*Sp
    If KeyWord_Set(DoDisp) Then  TvScl, nImg, Sx,Sy
    ;If KeyWord_Set(DoDisp) Then  TvScl, nImg, 2*Sx,Sy
;
;
;
;;;    Q = Where(nImg GT 0.333*Max(nImg),Complement=NNQQ)
    MaxNImg = Max(nImg,qpos)

    ;;Print,"thr on sp = ",0.444*MaxNImg

    Q = Where(nImg GT 0.444*MaxNImg,Complement=NNQQ)
; 
; PATCH
;
    If (Size(Q))[0] EQ 0 Then Begin
        If MeanR LT 4 Then MeanR=4
        Mask = mskSpider(Sx,Sy,Sx/2,Sy/2,meanR,RA,Intra=Intra) ;;;;;;;;;;; ,EstW=EstW)
        If KeyWord_Set(DoDisp) Then  TvScl, Mask, 0, Sy
        nImg = Mask*Sp
        If KeyWord_Set(DoDisp) Then  TvScl, nImg, Sx,Sy
        MaxNImg = Max(nImg,qpos)
        Q = Where(nImg GT 0.111*MaxNImg,Complement=NNQQ)
    EndIf


    xxx = nImg*0
    xxx[Q] = nImg[q]

    If KeyWord_Set(DoDisp) Then  TvScl, xxx, 2*Sx,Sy
    If KeyWord_Set(DoDisp) Then  TvScl, xxx, 3*Sx,Sy


    ;;;;Q = Search2D(nImg, qpos MOD Sx, qpos/Sx, 0.4*MaxNImg, MaxNImg)

    X = Float(Q Mod Sx)
    Y = Float(Fix(Q/Sx))
;;;;;W = nImg[Q]
    W = nImg[Q]
;;;;;Print,W
    PError=FltArr(5)
    PIn = mpFitEllipse(X,Y,/Tilt,Weights=W,/Quiet,/Circular,PError=PError)

;;;; ***************************************************************
;;;; **** TRICK TRICK TRICK TRICK TRICK TRICK TRICK TRICK TRICK ****
;;;; ***************************************************************

    If KeyWord_Set(Dbg) Then Begin
       ExtDbgLog," *** PIn[2] = "+ToS(PIn[2])
       ExtDbgLog," *** PIn[3] = "+ToS(PIn[3])
    EndIf

    If Not KeyWord_Set(DoIntFit) Then Begin
       If KeyWord_Set(Dbg) Then Begin
          ExtDbgLog, "===================================================================="
          ExtDbgLog, "Calling tryCentroid...."
       EndIf
       Ctroid = tryCentroid(Pup,Sx,Sy,PIn[2],PIn[3],MeanR,Dbg=Dbg)
       PIn[2] = Ctroid[0]
       PIn[3] = Ctroid[1]
    EndIf

    If KeyWord_Set(Dbg) Then Begin
       ExtDbgLog," *** PIn[2] = "+ToS(PIn[2])
       ExtDbgLog," *** PIn[3] = "+ToS(PIn[3])
    EndIf

;;;; ***************************************************************

    DeltaCx = Cx-PIn[2]
    DeltaCy = Cy-PIn[3]
;;;;;    Print,NIter,"      DCx,DCy=",DeltaCx,DeltaCy

;;    If Abs(DeltaCx) GT 5 OR Abs(DeltaCy) GT 5 Then Break

    Cx = PIn[2]
    Cy = PIn[3]

    NIter++
;;;; Endrep Until (Abs(DeltaCx) LT 0.005 AND Abs(DeltaCy) LT 0.005) OR NIter GE MaxIters
;;;;Print,"=============>PIn=",Pin

;
; Do a "sanity check" on the fitted coefficients
;

;
; average outer/internal radius
;
avgOR = (POut[0]+POut[1])/2.0
;;;Print,"Average outer diameter",2*avgOR
avgIR = (PIn[0]+PIn[1])/2.0

ddx = PIn[2]-POut[2]
ddy = PIn[3]-POut[3]
Res[10] = Sqrt(ddx*ddx+ddy*ddy)/avgOR
;If Res[10] GT 0.5 Then Begin
;    Print,"WARNING: center of hole too eccentric"
;    Print,"Center is: ",PIn[2],PIn[3]
;    Print,"AvgIR: ",avgIR
;Endif
Res[11] = avgIR/avgOR
;If Res[11] GT 0.5 Then Begin
;    Print,"WARNING: diameter of central hole too big"
;Endif

;If KeyWord_Set(estOR) Then Begin
;;;    Print,"avgOR: ",avgOR,", estOR: ",estOR
;    If avgOR GT 1.3*estOR Then Begin
;        Print,"*** WARN **** Fitted outer radius too large: "
;        Print,"is: ",avgOR,", expected: ",estOR
;    EndIf
;Endif

;Print,"InnerMsk: ",Sx,Sy,PIn[2],PIn[3],avgIR
;Print,"OuterMsk: ",Sx,Sy,POut[2],POut[3],avgOR
;; InnerMsk = mkmsk(Sx,Sy,PIn[2],PIn[3],avgIR)
;; OuterMsk = mkmsk(Sx,Sy,POut[2],POut[3],avgOR)-InnerMsk
;;; TvScl,OuterMsk
;; BrillIn = Total(Pup*InnerMsk)/Total(InnerMsk)
;; BrillOut = Total(Pup*OuterMsk)/Total(OuterMsk)
;; Print,"Relative Brillance of Hole: ",BrillIn/BrillOut
;; Res[9] = BrillIn/BrillOut

;; Print,"InOut: ",Sx,Sy,POut[2],POut[3],avgOR

RoddR = POut[0]
If POut[1] LT POut[0] Then RoddR = POut[1]

;If KeyWord_Set() Then Begin
;   MaskCx = Cxy[0]
;   MaskCy = Cxy[1]
;Endif Else Begin
   MaskCx = POut[2]
   MaskCy = POut[3]
;EndElse

If KeyWord_Set(Red) Then RBidon = 0.575 Else RBidon = 0.5
If Not KeyWord_Set(OldPhot) Then Begin
   mskR = Sqrt((RoddR*RoddR+RBidon*RBidon)/2.)
   inMsk = mkmsk(Sx,Sy,MaskCx,MaskCy,mskR) ;;;;;;; ;;;;  0.333)
Endif Else Begin
   inMsk = mkmsk(Sx,Sy,MaskCx,MaskCy,RoddR*0.35) ;;;;;;; un tiro corto e un tiro lungo...
EndElse

NPtsIn=Size(Where(inMsk GT 0),/N_Elements)
outMsk = mkmsk(Sx,Sy,MaskCx,MaskCy,RoddR)-inMsk
NPtsOut=Size(Where(outMsk GT 0),/N_Elements)
Res[9] = (Total(Pup*inMsk)/NPtsIn)/(Total(Pup*outMsk)/NPtsOut)


unitMsk = mkMsk(Sx,Sy,POut[2],POut[3],RoddR)

;; Print,"mkRoddierMask: ",Sx,RoddR,POut[2],POut[3]

rMsk = mkRoddierMask(Sx,RoddR,POut[2],POut[3],0)
Res[6] = -1.0*Total(Pup*rMsk)/Total(Pup*unitMsk)
;;Print,"Res[6]: ",Res[6]

rMsk = mkRoddierMask(Sx,RoddR,POut[2],POut[3],1)
Res[7] = -1.0*Total(Pup*rMsk)/Total(Pup*unitMsk)
;;Print,"Res[7]: ",Res[7]

If Not KeyWord_Set(OldPhot) Then Begin
   rMsk = mkRoddierMask(Sx,RoddR,POut[2],POut[3],2,RBidon=RBidon)
Endif Else Begin
   rMsk = mkRoddierMask(Sx,RoddR,POut[2],POut[3],2)
EndElse

Res[8] = Total(Pup*rMsk)/Total(Pup*unitMsk)

;;; xxx = nImg
;;; xxx[NNQQ] = 0


;;Print,"int: ",PIn

NPts=21
phi = DIndGen(NPts)*2D*!dpi/(NPts-1)

xe=PIn[0]*Cos(phi)
ye=Pin[1]*Sin(phi)

t = PIn[4]

xr =  xe*cos(t)+ye*sin(t)
yr = -xe*sin(t)+ye*cos(t)

xr = PIn[2]+xr
yr = PIn[3]+yr

inBX = xr
inBY = yr

If KeyWord_Set(DoDisp) Then Begin
    PlotS,xr,yr,psym=3,/device
    PlotS,Sx*3+xr,Sy+yr,psym=3,/device
EndIf
;
; compute last set of parameters that will be used
; in assessing aberrations
;
Res[0] = P[2]-Pin[2]
Res[1] = P[3]-Pin[3]
Res[4] = Pin[0]+Pin[1]

If KeyWord_Set(DoDisp) Then WSet, SaveWin

z22 = estimatez22(Pup, P[2], P[3], avgIR, avgOR)
If KeyWord_Set(dbg) Then DataLog, "z22: "+ToS(z22)
Res[12] = z22

Return,Res
End
