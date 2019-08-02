;+
; Given a set of extra-focal images of the pupil in input, computes
; low-order aberration coefficients by fitting ellipses to the
; internal and external borders of the pupi. Returns coma (X and Y),
; astigmatism (X & Y), spherical.
;
; @param P {in}{required} Array of pupils to use in computation.
; @param RA {in}{required} Rotator Angle in Degrees. Defaulted to
;    zero if not specified.
; @keyword bPos {in}{optional} Array of position of lower left corner of
;    pupil image, used for plotting only.
; @keyword NoDisplay {in}{optional}{type=boolean} Do not output
;    anything on the display.
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
; @keyword CurrentFocus {in}{required} The current focus of the telescope.
;
; @returns Array of 11 elements containing:
; <Table border=1 cellspacing=0>
; <TR><TD Align="Center">Index</TD><TD Align="Center">Zernike</TD></TR>
; <TR><TD Align="Center">4</TD>    <TD Align="Center">Z5</TD></TR>
; <TR><TD Align="Center">5</TD>    <TD Align="Center">Z6</TD></TR>
; <TR><TD Align="Center">6</TD>    <TD Align="Center">Z7</TD></TR>
; <TR><TD Align="Center">7</TD>    <TD Align="Center">Z8</TD></TR>
; <TR><TD Align="Center">10</TD>   <TD Align="Center">Z11</TD></TR>
; </Table>
;
; @author Andrea Baruffolo, INAF - OAPd
;-
Function CalcAbCoeffs, P, RA, cfg=cfg, $
                       bPos=bPos, DoIntFit=DoIntFit, estOR=estOR, $
                       NoDisplay=NoDisplay, dbg=dbg, DispPup=DispPup,$
                       CurrentFocus=CurrentFocus, SphGain=SphGain, $
                       PEdges=PEdges, pFits=pFits, $
                       estSeeing=estSeeing, Red=Red, OldPhot=OldPhot, Z22=Z22, $
                       Tec=Tec, chan=chan

If Not KeyWord_Set(RA) Then RA=0
;
; Take coefficients from configuration if available
; else fall-back to defaults
;
If KeyWord_Set(cfg) Then Begin
   SeeingZero       = cfg.SeeingZero      
   SeeingSlope      = cfg.SeeingSlope     
   AstigScale       = cfg.AstigScale      
   ComaScale        = cfg.ComaScale       
   SphScale         = cfg.SphScale        
   SphZero          = cfg.SphZero         
   SphSlope         = cfg.SphSlope        
   SphOffset        = cfg.SphOffset       
   SphRedCorrection = cfg.SphRedCorrection
   FocusAlphaRed    = cfg.FocusAlphaRed   
   FocusAlphaBlue   = cfg.FocusAlphaBlue  
   FocusScale       = cfg.FocusScale      
   FocusGain        = cfg.FocusGain       
   BadSeeingLimit   = cfg.BadSeeingLimit  
   LargeHoleLimit   = cfg.LargeHoleLimit  
   SmallHoleLimit   = cfg.SmallHoleLimit  
   SmallHoleSph     = cfg.SmallHoleSph
   BlueZ22Zero      = cfg.BlueZ22Zero
   BlueZ22Slope     = cfg.BlueZ22Slope
   BlueZ22Gain      = cfg.BlueZ22Gain
   RedZ22Zero       = cfg.RedZ22Zero
   RedZ22Slope      = cfg.RedZ22Slope
   RedZ22Gain       = cfg.RedZ22Gain
Endif Else Begin
   SeeingZero       = -1.564
   SeeingSlope      = 0.953
   AstigScale       = 220.
   ComaScale        = 200.
   SphScale         = 220.
   SphZero          = 10.6
   SphSlope         = 55.0
   SphOffset        = 88
   SphRedCorrection = 225.
   FocusAlphaRed    = 78.
   FocusAlphaBlue   = 78.
   FocusScale       = 26385.22
   FocusGain        = 1.0
   BadSeeingLimit   = 2.0
   LargeHoleLimit   = 0.25
   SmallHoleLimit   = 1.5
   SmallHoleSph     = 500.
   BlueZ22Zero      = 1.90
   BlueZ22Slope     = 300.0
   BlueZ22Gain      = 0.7
   RedZ22Zero       = 2.55
   RedZ22Slope      = 230.0
   RedZ22Gain       = 0.7
EndElse

LogAllWays, "BlueZ22Zero: "+String(BlueZ22Zero,Format='(F7.3)')
LogAllWays, "RedZ22Zero: "+String(RedZ22Zero,Format='(F7.3)')
;
; P is array [NumStars,Sx,Sy]
;
Sz = Size(P,/Dim)
NumStars = Sz[0]
Sx = Sz[1]
Sy = Sz[2]
;
; Will hold results from fitting
;
Res = FltArr(NumStars,13)
Seeings = FltArr(NumStars)
;
; Will hold "pupil edges"
;
PEdges = FltArr(NumStars,Sx,Sy)
PFits = FltArr(NumStars,5)
;
; Will hold computed aberrations to correct
;
ab = FltArr(22)

;
; Fit ellipses to pupils
;
If KeyWord_Set(DispPup) Then Begin
    Window,2,XSize=Sx*5,YSize=Sy*4
    WSet,0
EndIf

If Not KeyWord_Set(NoDisplay) Then PurgeWins

ExtDbgLog,"     DeltaXc       DeltaYc     DeltaAxis     AvgDiam     AvgIntDiam    PosAngle      "+$
          "Dx/Sx      Up/Down       In/Out    CoeffRelIll   RelPosHole   RelSizeHole"
ExtDbgLog,"-------------------------------------------------------------------------------------"+$
          "-------------------------------------------------------------------------"

bpos = double(bpos)
ValidResults = IntArr(NumStars)+1

For I=0,NumStars-1 Do Begin
    R = FitPupil( Reform(P[I,*,*],Sx,Sy), RA, /DoExtThr, DoIntFit=DoIntFit,$
                  outBX=obx, outBY=oby, inBX=ibx, inBY=iby, estOR=estOR, dodisp=dbg, $
                  OuterEllipseFit=OutP, PupEdge=pEdge, dbg=dbg, red=red, OldPhot=OldPhot, tec=tec, chan=chan )
    If Size(R,/Dimensions) EQ 0 Then Begin
       ValidResults[I] = 0
       Continue
    Endif

; Pupil Diameter Sanity Check 30-MAR-2009 JMH
    If R[3] GT 160. OR R[3] LT 12. Then Begin
        LogAllWays, "Rejected pupil "+ToS(I)+" with diameter "+ToS(R[3])
        ValidResults[I] = 0
        Continue
    Endif

    PEdges[I,*,*] = pEdge
    pFits[I,*] = OutP
    bw = BorderWidth(pEdge,OutP)
    ExtDbgLog, "Border width = "+ToS(bw)
    estSeeing = SeeingZero+SeeingSlope*sqrt(bw)
    Seeings[I] = estSeeing
    ExtDbgLog, "==> Estimated seeing: "+String(estSeeing,Format='(F4.2)')
    ExtDbgLog, "Result of pupil fit: "+String(R)
    If Not KeyWord_Set(NoDisplay) Then Begin
        If KeyWord_Set(Red) Then Begin
                !P.COLOR='0000FF'x
        EndIf Else Begin
                !P.COLOR='FF0000'x
        Endelse
        PlotS, (bPos[0,I]+obx)/2,(bPos[1,I]+oby)/2,PSym=3,/Device
        PlotS, (bPos[0,I]+ibx)/2,(bPos[1,I]+iby)/2,PSym=3,/Device
        If KeyWord_Set(DispPup) Then Begin
            WSet,2
            strtX = (I MOD 5)*Sx
            strtY = (I/5) * Sy
            TVScl, Reform(P[I,*,*],Sx,Sy), strtX, strtY
            PlotS, (strtX+obx),(strtY+oby),PSym=3,/Device
            PlotS, (strtX+ibx),(strtY+iby),PSym=3,/Device
            WSet,0
        EndIf
    Endif
    ExtDbgLog,String(R)
    Res[I,*] = R
    bPos[2,i] = R[3]
Endfor
;
; If any fit failed then remove corresponding entries 
; in result array
;
Q = Where(ValidResults EQ 0, Complement=NQ)
If Size(NQ, /N_Dimensions) EQ 0 Then Begin
   ErrorLog, "Unable to fit any pupil"
   Return, 0
EndIf
If Size(Q, /Dimensions) NE 0 Then Begin
   Res = Res[NQ,*]
   ExtDbgLog,"Processed "+ToS(NumStars)+" Pupils"
   NumStars = N_Elements(NQ)
   ExtDbgLog, "Fit failed for "+ToS(N_Elements(Q))+" pupils"
   ExtDbgLog, "Retaining "+ToS(NumStars)+" pupils"
EndIf

ExtDbgLog, String(Res)

DX        = Median(Res[*,0])
DY        = Median(Res[*,1])
DiffR     = Median(Res[*,2])
ExtD      = Median(Res[*,3])
IntD      = Median(Res[*,4])
PA        = Median(Res[*,5])
Mx        = Median(Res[*,6])
My        = Median(Res[*,7])
InOut     = Median(Res[*,8])
Z22est    = Median(Res[*,12])
estSeeing = Median(Seeings)
;
; Determine z22
;
If estSeeing GT badSeeingLimit Then Begin
   LogAllWays, "Seeing worse than limit => NO correction for sph5 will be computed/applied"
   ab[21] = 0.                  ; seeing worse than limit => we don't correct for sph5
   Z22 = 0.
Endif Else Begin
   If KeyWord_Set(Red) Then Begin
      Z22 = -1.0*(RedZ22Slope*(Z22est-RedZ22Zero))  ; Gain is applied below
   Endif Else Begin
      Z22 = -1.0*(BlueZ22Slope*(Z22est-BlueZ22Zero))
   Endelse
   LogAllWays, "Estimated Z22 parameter: "+String(Z22est,Format='(F7.3)')
   LogAllWays, "Computed Z22 correction: "+String(Z22,Format='(F7.1)')
   ab[21] = Z22
EndElse


If KeyWord_Set(dbg) Then Begin
    ExtDbgLog,"--------------------------------------------------------------------------------------"+$
      "------------------------------------------------------------------------"
    ExtDbgLog,String(DX,DY,DiffR,ExtD,IntD,PA,Mx,My,Median(Res[*,8]),Median(Res[*,9]),Median(Res[*,10]),Median(Res[*,11]),Z22)
Endif

LogAllWays, String("Median external diameter: ",ToS(ExtD)," px",Format="(A,F6.2,A)")

If KeyWord_Set(dbg) Then Begin
    ExtDbgLog,String("DeltaX = ", DX, "    (related to coma)")
    ExtDbgLog,String("DeltaY = ", DY, "    (related to coma)")
    ExtDbgLog,String("Diff R = ", DiffR, " (related to astigmatism)")
    ExtDbgLog,String("Ext. D = ", ExtD, "  (related to spherical)")
    ExtDbgLog,String("Int. D = ", IntD, "  (related to spherical)")
    ExtDbgLog,String("PAngle = ", PA, "    (used to project aberr)")
Endif

QuadA = FltArr(NumStars)
QuadB = QuadA

For I=0,NumStars-1 Do Begin
    QuadA[I] = Res[I,5] MOD 180
    If QuadA[I] LT 0 Then QuadA[I] += 180
    If KeyWord_Set(dbg) Then ExtDbgLog,ToS(QuadA[I])
EndFor
If KeyWord_Set(dbg) Then ExtDbgLog,"----"
For I=0,NumStars-1 Do Begin
    QuadB[I] = Res[I,5] MOD 180
    While QuadB[I] GT  90 Do QuadB[I] -= 180
    While QuadB[I] LT -90 Do QuadB[I] += 180
    If KeyWord_Set(dbg) Then ExtDbgLog,ToS(QuadB[I])
EndFor
If KeyWord_Set(dbg) Then ExtDbgLog,"----"

PA_A = Median(QuadA)
PA_B = Median(QuadB)
If KeyWord_Set(dbg) Then Begin
   ExtDbgLog,"PA_A="+Tos(PA_A)+" PA_B="+ToS(PA_B)
   ExtDbgLog,"PA_A-PA_B="+ToS(PA_A-PA_B)
Endif

RAr = RA/180.*!PI
;
; Astigmatisms
; Scale factor and orientation verified on June 20, 2006
;
If KeyWord_Set(dbg) Then ExtDbgLog,"PosAngle Ellipse: "+ToS(PA_A)
Ang = 2*(PA_A+RA)
If KeyWord_Set(dbg) Then ExtDbgLog,"Astigm Angle: "+ToS(Ang)
Ang /= 180./!PI
z5 = 2.5*AstigScale*DiffR*sin(Ang)
z6 = 2.5*AstigScale*DiffR*cos(Ang)

;
; Coma
;
;z7 = (-Dy*cos(RAr)+DX*sin(RAr))*220.
;z8 = ( Dx*cos(RAr)+Dy*sin(RAr))*220.
z7 = (-Dy*cos(RAr)+DX*sin(RAr))*ComaScale ;changed from 220 to 200 25-OCT-2008 JMH, OPK
z8 = ( Dx*cos(RAr)+Dy*sin(RAr))*ComaScale ;changed from 220 to 200 25-OCT-2008 JMH, OPK

mz7 = (-My*cos(RAr)+Mx*sin(RAr))*ExtD*ComaScale/3.
mz8 = ( Mx*cos(RAr)+My*sin(RAr))*ExtD*ComaScale/3.

;
; Spherical Aberration
;
;;; mz11 = InOut*ExtD*200.
mz11 = InOut*ExtD*100./2.    ;empirical correction by JMH 20060702

;;; z11 = -220.*(10.15-IntD/ExtD*63.8)
;;; z11 = 220.*(12.8-IntD/ExtD*55.0)
If KeyWord_Set(dbg) Then ExtDbgLog,"IntD, ExtD: "+ToS(IntD)+ToS(ExtD)

; z11 = 220.*(10.6-IntD/ExtD*55.0)+150 ; Calibration of Oct 22, 2006
; z11 = 220.*(10.6-IntD/ExtD*55.0)+200 ; Estimate of 11JAN08 JMH
;z11 = 220.*(10.6-IntD/ExtD*55.0)-100 ; Adjustment of 20OCT08 JMH
;z11 = 220.*(10.6-IntD/ExtD*55.0)-400 ; Adjustment of 21OCT08 OPK/AR
; based on ARs analysis of extra/intra focal images taken with 20OCT08 line 
;z11 = 220.*(10.6-IntD/ExtD*55.0)-700 ; Second Adjustment of 21OCT08 OPK/AR
;z11 = 220.*(10.6-IntD/ExtD*55.0)-200 ; backing off the large offset 25OCT08 JMH, OPK
;z11 = 220.*(10.6-IntD/ExtD*55.0)-500 ; restoring most of the offset 25OCT08+ JMH
; z11 = 220.*(10.6-IntD/ExtD*55.0)-700 ; Second Adjustment of 21OCT08 OPK/AR
; Re-instated -700 offset during tech night #2, 21NOV08 UT OPK/AR/JMH/MP
; z11 = 220.*(10.6-IntD/ExtD*55.0)-200 ; backing off the large offset
; 25OCT08 JMH, OPK

LogAllWays, "IntD: "+ToS(IntD)
LogAllWays, "ExtD: "+ToS(ExtD)

;
; Gain will be applied below
;
z11 = SphScale*(SphZero-IntD/ExtD*SphSlope)+SphOffset ; ON purpose to have z11=0 for IntD/ExtD=0.2 (19Jan09, RR+AB)

;On the Red channel z11 is ~300 larger (because of larger obstruction)
;
If KeyWord_Set(Red) Then z11 += SphRedCorrection ;estimate of 06DEC07 (with 150 above)
; If KeyWord_Set(Red) Then z11 += -150. ; estimate of 11JAN08 JMH (with 200 above)


; correct for seeing blurring of the pupil
; (factor was calculated from the Rakich/Thompson synthetic data )
;
; Correction dropped on 19JAN09. This correction shouldn't be applied
; if we aim to IntD/ExtD=0.2 (see computation of z11 above)
;
; z11 += 350.0*estSeeing

;;;z11 = (z11+150.)/0.38

;
; Focus
;
;alpha = 77.86          ;empirically determined delta pupil size (pixels) per delta focus (mm)
;alpha = 75.32          ;empirically determined delta pupil size (pixels) per delta focus (mm), 21Oct06 JMH
;alpha = 76.64          ; determined for LBC-Blue V-BESSEL  09-JUN-2007 JMH
;alpha = 75.64          ; determined for LBC-Blue V-BESSEL  06-NOV-2007 JMH
; on 20090221 I uncommented the following 4 lines to restore alpha=77.0. We have been having to add Z4~2000nm manually
;after dofpia to focus the obviously out of focus guide stars. This may be because we have had to correct for Z22 and have
; still, after Z22~-300nm total, have not removed Z22. We hope restoring alpha to 77.0 will prevent the need for these manual corrections.
;If KeyWord_Set(Red) Then $
;   alpha = 77.0 $       ; alpha for red channel (same as Blue on purpose, Red V-BESSEL will have non-zero focus offset)
;Else $
;   alpha = 77.0         ; adjusted for LBC-Blue V-BESSEL  21-NOV-2008 JMH to compensate Z11 zeropoint shift of 25-OCT-08

;If KeyWord_Set(Red) Then $
;   alpha = 83.0 $       ; alpha for red channel (same as Blue on purpose, Red V-BESSEL will have non-zero focus offset)
;Else $
;   alpha = 83.0         ; adjusted for dofpia
; 
; 21 Feb 2009 - we found that after changing alpha back to 77, we still had to adjust Z4, up to -2000nm total in DX! 
; so we made these adjustments to bring alpha(red) back to 83 and blue was close. We were using r-SLOAN and z-SLOAN filters.
;If KeyWord_Set(Red) Then $
;   alpha = 81.0 $       ; alpha for red channel (same as Blue on purpose, Red V-BESSEL will have non-zero focus offset) (from 83 to 81)
;Else $
;   alpha = 78.0         ; adjusted for LBC-Blue V-BESSEL  21-NOV-2008 JMH to compensate Z11 zeropoint shift of 25-OCT-08
If KeyWord_Set(Red) Then $
   alpha = FocusAlphaRed $       ; 22Feb09 JMH & OPK - setting Red to 78 since last nightand tonight we are having to add Z4 manually to red.
Else $
   alpha = FocusAlphaBlue         ; adjusted for LBC-Blue V-BESSEL  21-NOV-2008 JMH to compensate Z11 zeropoint shift of 25-OCT-08


; larger value of alpha moves the focus toward more positive numbers

;scale = 1000./0.072    ;empirically determined mirror movement (nm) per focus (mm) derived from PSF subsystem
scale = FocusScale   ;Corrected mirror movement (nm) per focus (mm) derived from PSF subsystem by 72.03/37.89 (1.9)
; this adjusts value of Z4 to match the 24-MAY-2008 version of PSF zernikes JMH
; PSF now makes a smaller position correction of M1 for a given Z4

if CurrentFocus gt 0 then begin
    LogAllWays, '*** WARNING: INTRA-FOCAL IMAGE! ***'
    z4 = (CurrentFocus - ExtD/alpha)*scale
endif else z4 = (ExtD/alpha + CurrentFocus)*scale

photEst = Median(Res[*,9])
;; If KeyWord_Set(Dbg) Then $
LogAllWays, "M-factor: "+String(photEst,Format='(F4.2)')

If KeyWord_Set(dbg) Then Begin
   ExtDbgLog, "Aberrations from fitted ellipses: "
   ExtDbgLog,String('Z4 = ',strtrim(z4,2), '    Z5 = ', strtrim(z5,2),$
                    '    Z6 = ',strtrim(z6,2), '    Z7 = ', strtrim(z7,2),$
                    '    Z8 = ',strtrim(z8,2), '    Z11 = ',strtrim(z11,2))
   ExtDbgLog, "Aberrations from 'photometric' method: "
   ExtDbgLog,String('Mz7 = ',strtrim(mz7,2),$
                    '   Mz8 = ',strtrim(mz8,2),'    Mz11 = ',strtrim(mz11,2))
EndIf

ab[3]  = z4
ab[4]  = z5
ab[5]  = z6

;
; If z7,z8 aberrations are large, use values from photometric approach
;
;If abs(mz7) GT 1000. Then ab[6] = mz7 Else ab[6]  = z7
;If abs(mz8) GT 1000. Then ab[7] = mz8 Else ab[7]  = z8
;
; For z11 always use photometric approach
;
;ab[10] = mz11

; --------------------------------------------------------------------
; For John and anyone else who might want to play with these
; parameters, here is the "decision tree".
; First we look at the seeing: if it is larger than 2" we say that
; it is not possible to reliably estimate the spherical, so we
; don't apply any z11 correction.
; Then we look at the ratio of the light in the "inner" over that in
; the "outer" part of the pupil.
; If that number is
;   < 0.45 => we say that the central hole is "too big" to be 
;             properly fitted by the current version of the code:
;             we apply a fixed amount of negative spherical and
;             the coma as determined by the "photometric" approach.
;   > 0.80 => we say that the central hole is "too small": we
;             apply a fixed amount of positive spherical and
;             the coma as determined by the "photometric" approach.
; In all other cases we apply the corrections as derived from the
; fitting of the pupil.
; 

;
; NOTE: badSeeingLimit, largeHoleLimit and smallHoleLimit taken from
; configuration or defaulted at beginning of this proc
;

;
; Determine z11
;
If estSeeing GT badSeeingLimit Then Begin
   ab[10] = 0. ; seeing worse than limit => we don't correct for sph3
Endif Else Begin
   If photEst LT largeHoleLimit Then Begin
      ; ab[10] = -500.
       LogAllWays, "WARNING: large negative sph3" 
       ab[10] = z11
   Endif Else Begin
      If photEst GT smallHoleLimit Then Begin
         ab[10] = SmallHoleSph/SphGain
      Endif Else Begin
         ab[10] = z11
      EndElse
   Endelse
EndElse
;
; Determine coma (z7, z8)
;   
If photEst GE largeHoleLimit And photEst LE smallHoleLimit Then Begin
   ; use geometric estimates
   ab[6] = z7
   ab[7] = z8
Endif Else Begin
   ; use photometric estimates
   ab[6] = mz7
   ab[7] = mz8
EndElse
;
; Print computed aberrations before correction for x-talk
;
LogAllWays,"Computed ab's: "+'Z4 = '+strtrim(ab[3],2)+ '  Z5 = '+ strtrim(ab[4],2)+$
           '  Z6 = '+strtrim(ab[5],2)+ '  Z7 = '+ strtrim(ab[6],2)+$
           '  Z8 = '+strtrim(ab[7],2)+ '  Z11 = '+strtrim(ab[10],2)+$
           '  Z22 = '+strtrim(ab[21],2)
;
; Correct for Z4/Z11/Z22 "cross-talk"
;
inZ = [[ab[3]],[ab[10]],[ab[21]]]
If KeyWord_Set(Red) Then ZIMatrix = cfg.RedZIMatrix Else ZIMatrix = cfg.BlueZIMatrix
outZ = ZIMatrix##inZ

ab[ 3] = outZ[0]
ab[10] = outZ[1]
ab[21] = outZ[2]
;
; Print computed aberrations after correction for x-talk
;
LogAllWays,"xtalk corr'd : "+'Z4 = '+strtrim(ab[3],2)+ '  Z5 = '+ strtrim(ab[4],2)+$
           '  Z6 = '+strtrim(ab[5],2)+ '  Z7 = '+ strtrim(ab[6],2)+$
           '  Z8 = '+strtrim(ab[7],2)+ '  Z11 = '+strtrim(ab[10],2)+$
           '  Z22 = '+strtrim(ab[21],2)
;
; Apply gains (i.e. dump/boost factor from aberration to correction)
;
ab[ 3] *= FocusGain
ab[10] *= SphGain
If KeyWord_Set(Red) Then Begin
   ab[21] *= RedZ22Gain
Endif Else Begin
   ab[21] *= BlueZ22Gain
Endelse
;
; Print computed aberrations after application of gains
;
LogAllWays,"Gain corr'd  : "+'Z4 = '+strtrim(ab[3],2)+ '  Z5 = '+ strtrim(ab[4],2)+$
           '  Z6 = '+strtrim(ab[5],2)+ '  Z7 = '+ strtrim(ab[6],2)+$
           '  Z8 = '+strtrim(ab[7],2)+ '  Z11 = '+strtrim(ab[10],2)+$
           '  Z22 = '+strtrim(ab[21],2)
;
; Return computed corrections
;
Return, ab
End
