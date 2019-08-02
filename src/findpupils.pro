;+
; Given a file to be processed, finds pupil locations. More detailed
; description of algorithm TO BE WRITTEN.
;
; @param ima {in}{required} Part of image were to look for pupils.
; @keyword Pups {out}{optional} Array of extracted pupils.
; @keyword llbPos {out}{optional} Two dimensional array containing
;    lower left coordinates of the extracted boxes (that contain
;    detected pupils).
; @keyword NoDisplay {in}{optional}{type=boolean} Do not output
;    anything on the display.
; @keyword HitOrMiss {in}{optional}{type=boolean} <Strong>DEPRECATED</Strong> use
;    hit-or-miss morphological operator to detect pupils. Leftover of
;    previous experiments, doesn't work well (if at all).
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
; @keyword invMoments {out}{optional} Array of invariant moments for
;    detected blobs
;
; @returns Number of pupils extracted
;
; @history Flux threshold computed on the basis of background sigma. Feb 02, 2010
; @history Documentation (IDLDoc) added on May 11, 2006
; @history Added invariant moments keyword on Nov 21, 2007
; @history Removed unused PRad keyword on May 14, 2009
; @author Andrea Baruffolo, INAF - OAPd
; @version $Id$
;-
Function FindPupils, ima, Pups=Pups, llbPos=llbPos, $
                     cfg=cfg, $
                     NoDisplay=NoDisplay, HitOrMiss=HitOrMiss, $
                     MinFlux=MinFlux, dbg=dbg, $
                     estOR=estOR,NoBckSub=NoBckSub, $
                     invMoments=invMoments, blobs=blobs, objs=objs, $
                     oldBck=oldBck, chan=chan, newExtr=NewExtr, $
                     MaxCandidates=MaxCandidates, Tec=Tec

If KeyWord_Set(dbg) Then ExtDbgLog, "Findpupils, entering ... "

If KeyWord_Set(cfg) Then Begin
   BackGndNSigma = cfg.BackGndNSigma
   MinBlobSize   = cfg.MinBlobSize
   MaxBlobSize   = cfg.MaxBlobSize
Endif Else Begin
   BackGndNSigma = 1.0
   MinBlobSize   = 10
   MaxBlobSize   = 200
Endelse
;
; Estimate backgound
;
  If KeyWord_Set(oldBck) Then Begin
     If KeyWord_Set(DBg) Then ExtDbgLog, "Using 'OLD' background subtraction scheme"
     bck = background(ima,dbg=dbg)
     ExtDbgLog,"Background level: "+StrCompress(String(bck),/Remove)
  Endif Else Begin
     If KeyWord_Set(DBg) Then ExtDbgLog, "Using 'NEW' background subtraction scheme"
;     If Not KeyWord_Set(Tec) Then Begin
        bck = fit_backgnd(ima,sigma_bck=sigma_bck, dbg=dbg)
        ExtDbgLog, "background: "+ToS(Median(bck))
;     Endif Else Begin
;        sky, ima, bck, skysig, /Silent
;        If KeyWord_Set(Dbg) Then Help, bck
;     EndElse
  EndElse

  If KeyWord_Set(dbg) Then Begin
     ExtDbgLog, "median ima: "+ToS(Median(ima))
  EndIf

  ima = Median(ima-bck,3)
;
; Set Plot color to white
!P.COLOR = 'FFFFFF'x

; Image sizes
;
  Sz = Size(ima,/Dimensions)
  NCols = Sz[0]
  NRows = Sz[1]
  NCols_2 = NCols/2
  NRows_2 = NRows/2
;
; Flux threshold. All points below this threshold will be discarded.
;
  If Not KeyWord_Set(MinFlux) Then MinFlux=sigma_bck ; changed by JMH

  msk = ima GE MinFlux

  addDisp = 0 ; do additional display
  If Not KeyWord_Set(NoDisplay) Then Begin
     If KeyWord_Set(dbg) Then addDisp = 1
     YSize=NRows_2
     ;;If KeyWord_Set(dbg) Then YSize *=2
    ;;; Window,0,XSize=NCols_2,YSize=YSize*2,Retain=2
     If Not KeyWord_Set(chan) Then chan='blue'
     WinID = 0 ; Window Index controls default position of window in X-display
     If chan EQ 'red' Then WinID=1
     Title = 'Pupils for '+StrUpCase(chan)+' channel'
     Window,WinID,XSize=NCols_2,YSize=YSize,Retain=2,Title=Title
  Endif
;; TVScl,ConGrid(msk,NCols_2,NRows_2)
;; return,msk

;
; Remove small features (hot spots, cosmics, etc)
; If we skip this step, then on "pathological" images
; the search for blobls takes ages, since it examines 
; also very small blobs (which are effectively removed
; by eroding/dilating).
;
  se = StrElm(4)
  msk = msk*Dilate(Erode(msk,se),se)

;
; Display input image ...
;
; disp_ima = Sqrt(ConGrid(ima > 0,NCols_2,NRows_2,/Interp))
; TVScl,disp_ima


  MinV=minFlux*0.7
  If KeyWord_Set(oldBck) Then MinV = MinV+bck
  MaxV=20000
  If KeyWord_Set(Dbg) Then ExtDbgLog,"FindPupils: hist_equal MinV, MaxV = "+ToS(MinV)+" "+ToS(MaxV)
  disp_ima = hist_equal(ima,MinV=MinV,MaxV=MaxV)
  If NOT KeyWord_Set(NoDisplay) Then TV,ConGrid(disp_ima,NCols_2,NRows_2,/int)


; TVScl,disp_ima,0,YSize
; TVScl,ConGrid(msk,NCols_2,NRows_2,/Interp)

;
; Keep track of number of blobs found
;
  NumBlobs = 0
;
; Keep track of largest blob size
; 
  maxBSz = -1

  If KeyWord_Set(dbg) Then ExtDbgLog, "Searching for pupils... "

  If Not KeyWord_Set(MaxCandidates) Then MaxCandidates = 20

  TotalBlobs = 0L
  While NumBlobs LT MaxCandidates Do Begin

     Q   = Where(msk GT 0)
     
     N_Q = N_Elements(Q)
     
     If (Size(Q))[0] EQ 0 Then Break
     
     ;;; If Size(Q,/Dimensions) EQ 0 Then Break

     ; choose blobs at random
     ; rIdx = Long(RandomU(Seed)*N_Q)
     ; choose blob with highest intensity
     dummy = Max(msk[Q]*ima[Q],rIdx)

     X = Q[rIdx] MOD NCols
     Y = Q[rIdx]/NCols
     Blob = Search2D(msk, X, Y, 0.9, 1.1,/Diagonal)

     If N_Elements(Blob) LE !PI*MinBlobSize*MinBlobSize Then Begin
        If KeyWord_Set(dbg) Then ExtDbgLog, "Blob is too small: "+ToS(N_Elements(Blob))
        msk[Blob] = 0
        Continue
     EndIf

     bb = BytArr(MaxBlobSize,MaxBlobSize)
     obj = FltArr(MaxBlobSize,MaxBlobSize)

     minX = min(Blob MOD NCols, max=maxX)
     minY = min(Blob / NCols, max=maxY)

     sizeX = maxX-minX+1
     sizeY = maxY-minY+1
     If KeyWord_Set(dbg) Then ExtDbgLog,"Blob size: "+String(sizeX,SizeY,N_Elements(Blob))


     If (sizeX LE MaxBlobSize AND sizeY LE MaxBlobSize) Then Begin
        TotalBlobs += 1
        posX = (MaxBlobSize-sizeX)/2
        posY = (MaxBlobSize-sizeY)/2
        bb[posX:posX+sizeX-1,posY:posY+sizeY-1] = msk[minX:maxX,minY:maxY]
        obj[posX:posX+sizeX-1,posY:posY+sizeY-1] = ima[minX:maxX,minY:maxY]

        If N_Elements(blobArr) Eq 0 Then Begin
           blobArr = Reform(bb,1,MaxBlobSize,MaxBlobSize)
           objArr  = Reform(obj,1,MaxBlobSize,MaxBlobSize)
        Endif Else Begin
           blobArr = [blobArr, Reform(bb,1,MaxBlobSize,MaxBlobSize)]
           objArr  = [objArr, Reform(obj,1,MaxBlobSize,MaxBlobSize)]
        EndElse

        bSizeX = maxX-minX+1
        bSizeY = maxY-minY+1
        ;
        ; For the blue tech channel we use invariant moments to check pupils
        ;
        inv_moms = invariant_moments(bb)
        If KeyWord_Set(Tec) And chan EQ 'blue' Then Begin
           If inv_moms[0] LT 0.163 AND inv_moms[1] LT 0.001 Then Begin
              GoodBlob = 1 
              ExtDbgLog,"Blob accepted, inv_moms="+ToS(inv_moms[0])+", "+ToS(inv_moms[1])
           Endif Else Begin 
              GoodBlob = 0 
              ExtDbgLog,"Blob rejected, inv_moms="+ToS(inv_moms[0])+", "+ToS(inv_moms[1])
           EndElse
           res = inv_moms[0]
        EndIf Else Begin
           res = AnaBlob(bb)
           If res LT Sqrt(2) Then Begin
              GoodBlob = 1 
              ExtDbgLog,"Blob accepted, res="+ToS(res)
           Endif Else Begin
              GoodBlob = 0
              ExtDbgLog,"Blob rejected, res="+ToS(res)
           EndElse
        EndElse

        arr = [inv_moms,GoodBlob]
        If N_Elements(invM) Eq 0 Then Begin
           invM = Reform(arr,1,8)
        EndIf Else Begin
           invM = [invM, Reform(arr,1,8)]
        EndElse
        ;
        ; If res LT X then "this is a good blob"
        ;
        If GoodBlob Eq 1 Then Begin
           bsz2 = getBlobSz(msk[minX:maxX,minY:maxY]*ima[minX:maxX,minY:maxY])
           If KeyWord_Set(dbg) Then ExtDbgLog,"bsz2="+ToS(bsz2)
           If N_Elements(blobsPos) EQ 0 Then Begin
              blobsPos = Reform([minX,maxX,minY,maxY],1,4)
              ;; blobsSz = [ bSizeX GT bSizeY ? bSizeX : bSizeY ]
              ;; blobsSz  = [ (bSizeX+bSizeY)/2.0 ]
              blobsSz2 = [ bsz2 ]
           Endif Else Begin
              blobsPos = [blobsPos, Reform([minX,maxX,minY,maxY],1,4)]
              ;; blobsSz = [ blobsSz, (bSizeX+bSizeY)/2.0 ]
              blobsSz2 = [ blobsSz2, bsz2 ]
           EndElse
           If addDisp GT 0 Then PlotS,[minX,maxX,maxX,minX,minX]/2.,YSize+[minY,minY,maxY,maxY,minY]/2.,/Device
           If maxBSz LE bSizeX Then maxBSz = bSizeX
           If maxBSz LE bSizeY Then maxBSz = bSizeY
           NumBlobs++
        Endif Else Begin
           If addDisp GT 0 Then Begin
              PlotS,[minX,maxX]/2.,[minY,maxY]/2.,/Device
              PlotS,[maxX,minX]/2.,[minY,maxY]/2.,/Device
           EndIf
           Dummy = 1 ;; just to retain the "else" block, for the time being.
        EndElse

     Endif Else Begin
        If KeyWord_Set(dbg) Then ExtDbgLog,$
           String("Whoops, this blob is too big (",sizeX,"x",sizeY,"), will skip it")
     EndElse

     msk[Blob] = 0
  Endwhile

  If TotalBlobs GT 0 Then blobs = blobArr

  If NumBlobs LT 1 Then Return, 0

  objs = objArr
  invMoments = Transpose(invM)

;; Print, SysTime()
  If KeyWord_Set(dbg) Then ExtDbgLog,"Num blobs found: "+ToS(NumBlobs)

;
; We haven't find any pupil...
;

 If KeyWord_Set(dbg) Then ExtDbgLog,"Max blob size "+ToS(maxBSz)

  a=FilterBSz(blobsSz2, moments=moments)

  If N_Elements(moments) GT 0 AND KeyWord_Set(dbg) Then $
     ExtDbgLog, String("Average good diameter = ",moments[0], " +/-  ",Sqrt(moments[1]))
;
; How many pixels to retain on both sides
;
  side=3
;
; Size of "extraction" box
;
  exSz = maxBSz+side*2
  nGoodBlobs = NumBlobs
;
; Avoid blobs that are too close to the image borders
;
  For I=0,NumBlobs-1 Do Begin
     sx = blobsPos[I,0]
     ex = blobsPos[I,1]
     sy = blobsPos[I,2]
     ey = blobsPos[I,3]

     If Not KeyWord_Set(NewExtr) Then Begin
        nsx = sx-(exSz-(ex-sx+1))/2
        nex = nsx+exSz-1
        nsy = sy-(exSz-(ey-sy+1))/2
        ney = nsy+exSz-1
     Endif Else Begin
        thisExSz=blobsSz2[I]+side*2
        nsx = sx-(thisExSz-(ex-sx+1))/2
        nex = nsx+thisExSz-1
        nsy = sy-(thisExSz-(ey-sy+1))/2
        ney = nsy+thisExSz-1
     EndElse

     If nsx LT 0 OR nex GE NCols OR nsy LT 0 OR ney GE NRows Then Begin
        blobsPos[I,*] = [-1,-1,-1,-1]
        nGoodBlobs--
     Endif Else Begin
        blobsPos[I,0] = nsx
        blobsPos[I,1] = nex
        blobsPos[I,2] = nsy
        blobsPos[I,3] = ney
     EndElse
  EndFor
;
; Actually extract data from input image
;
  If nGoodBlobs LT 1 Then Return, 0
  Pups = FltArr(nGoodBlobs,exSz,exSz)
  llbPos = FltArr(3,nGoodBlobs)
  K = 0
  For I=0,NumBlobs-1 Do Begin

     sx = blobsPos[I,0]
     ex = blobsPos[I,1]
     sy = blobsPos[I,2]
     ey = blobsPos[I,3]

     If sx GE 0 Then Begin
        If Not KeyWord_Set(NewExtr) Then Begin
           Pups[K,*,*] = ima[ sx:ex, sy:ey ]
           llbPos[0,K] = sx
           llbPos[1,K] = sy
	   If Not KeyWord_Set(NoDisplay) Then $
              PlotS,[sx,ex,ex,sx,sx]/2.,[sy,sy,ey,ey,sy]/2.,/Device
                                ;TVScl, Blobs[K,*,*]
                                ;Wait, 0.2
        Endif Else Begin
           dx = ex-sx+1
           dy = ey-sy+1
           ssxx = Fix((exSz-dx)/2)
           ssyy = Fix((exSz-dy)/2)
           eexx = ssxx+dx-1
           eeyy = ssyy+dy-1
           Pups[K,ssxx:eexx,ssyy:eeyy] = ima[ sx:ex, sy:ey ]
	   If Not KeyWord_Set(NoDisplay) Then $
              PlotS,[sx,ex,ex,sx,sx]/2.,[sy,sy,ey,ey,sy]/2.,/Device
        EndElse
           K++
     EndIf
  EndFor

  If KeyWord_Set(dbg) Then ExtDbgLog, "Number of 'good' pupils : "+ToS(nGoodBlobs)

  ;;;;;;;;;;;;;; If Not KeyWord_Set(NoBckSub) Then Pups = Pups - bck 

  Return, nGoodBlobs
End
