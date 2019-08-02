;+
; DOFPIA - First attempt to make automatic IA on LBC science chips
; created by Andrea Baruffolo - January 2009
; modified April-2014 to use WRS instead of old lbcfpia
;
;
; The procedure for semi-automatic Image Analysis on LBC focal plane is named
; 'dofpia_wrs.pro' and is located in '~lbceng/WRS/src/'.
; To run it:
; - do a 'source ~lbceng/WRS/setup.csh' (to set IDL_PATH and WRS_HOME)
; - cd to a writeable directory (for log files)
; - run IDL by typing 'idl'
; - at IDL prompt type 'dofpia_wrs'
;
; In a nutshell, the procedure:
; 1- sends initial +600mm sph3 offset to both mirrors
; 2- loads in the LBC UIF the OB RB_rVfastextra.ob
; 3- tells the LBC UIF to start the OB
; 4- waits for data to appear in /Repository
; 5- analyzes the data using LBCFPIA (always sending the Zernike corrections)
; 6- iterates points 3-5 until all aberrations fall below 400nm (in absolute value)
; 7- sends a final, seeing dependent, sph3 correction after convergence has been reached.
;
;
; @keyword Sim {in}{optional}{type=boolean} Run without interacting with LBC camera
; @keyword X2 {in}{optional}{type=boolean} Double the OB exposure timeto 32 sec
; @keyword RedOnly {in}{optional}{type=boolean} only work on LBC-Red
; @keyword BlueOnly {in}{optional}{type=boolean} only work on LBC-Blue
; @keyword RedInitial {in}{optional}{type=real} value of Red initial Z11 offset in nm
; @keyword BlueInitial {in}{optional}{type=real} value of Blue initial Z11 offset in nm
; @keyword RedLimit {in}{optional}{type=real} value of Red convergence limit in nm
; @keyword BlueLimit {in}{optional}{type=real} value of Blue convergence limit in nm
; @keyword MaxIterations {in}{optional}{type=integer} maximum loops 
; @keyword First {in}{optional}{type=boolean} increase initial Z11 offsets
; @keyword BackOut {in}{optional}{type=boolean} remove previous Z11 offsets
; @keyword AskMe {in}{optional}{type=boolean} query before sending Zernikes
;
; @author Andrea Baruffolo, INAF - OAPd
; @version $Id$
;-

; --------------------------------------------------------------------
;      Andrea Baruffolo - INAF - Osservatorio Astronomico di Padova
;               vicolo Osservatorio 5, 35122 Padova, Italy
;  Tel: +39-049-8293404 Operator: +39-049-8293411  Fax: +39-049-8759840
;
; last modified by J. M. Hill - 19-FEB-2009 
;                (fixed quotes typo in message waiting for file to appear)
;                (increased convergence threshold from 200 nm to 300 nm)
;                (added convergence threshold for focus also)
;                (removed the /interactive flag from the initial SendSph3)
;                (changed PWD to HomeDir+/src-sph3 from LBCFPIA_HOME to allow operation by other users)
;                (removed user interaction to Play OB)
;                (use environment variable LBCFPIA_DATADIR for data path)
; last modified by J. M. Hill - 20-FEB-2009
;                (reduce initial sph3 offset to +600 nm)
; last modified by J. M. Hill 24-FEB-2009
;                (loosen tolerance on astigmatism to 400 nm)
;                (add keyword X2 to increase exposure times)
;                (adjust flags to continue analysis of channel after convergence)
;                (add flags to suppress analysis of channel) 
; last modified by J. M. Hill - 26-FEB-2009
;                (add different OBs for BlueOnly and RedOnly cases) 
;                (trap all zeroes coeffs for LBCFPIA insane values)
; last modified by J. M. Hill - 19-MAR-2009
;                (add parameters for RedLimit, BlueLimit convergence threshold)
;                (add parameters for RedInitial, BlueInitial sending of Z11)
;                (add parameter MaxIterations for limit on number of loops)
; last modified by J. M. Hill - 20-MAR-2009
;                (add flag /First to increase the initial Z11 send)
;                (add flag /BackOut to remove initial Z11 send after failure)
;                (Blue gets +600 nm Z11, Red gets +500 nm Z11)
;                (added 'S' key to interrupt the iteration loop)
; last modified by J. M. Hill - 24-MAR-2009
;                (corrected erroneous Z4/Z11 compensation, now Z4 = -2.0*Z11)
;                (added /AskMe flag passed indirectly to lbcfpia to cancel /ForceSend)
; last modified by J. M. Hill - 18-APR-2009
;                (added delay when only using one camera so that
;                previous OB has time to finish before next one starts)
; updated by Baru - 02-JUN-2009
;                (read parameters from configuration file)
;
; last modified by J. M. Hill - 04-JUN-2009
;                (change DoRed/DoBlue information from 0-2 to text)
;                (improve formatting of limit print statement)
; last modified by J. M. Hill - 12-JUN-2009
;                (added cfg=cfg parameter to the lbcfpia calls in dofpia)
; last modified by O. P. Kuhn - 04-SEP-2009
;                (Z22 threshold reduced from 100 to 50)
;                (default directory to /TestRepo)
; last modified by J. M. Hill - 16-SEP-2009
;                (Adjust threshold automatically as a function of
;                estimated seeing)
; last modified by A. Baruffolo - 27-APR-2010
;                (Limits array aligned with that in use on the mountain)
; last modified by K. Summers - 17-JUN-2014
;                (use WRS_HOME and WRS_DATADIR env vars)
;

; Test Program for Manual Z11
Pro SendSph3Interactive, Red=Red, Sim=Sim
If KeyWord_Set(Red) Then chan='RED' Else chan='BLUE'
Print, 'How much Sph3 to send to '+chan+' channel now?'
Response = ''
Read, Response
Sph3 = Float(Response)
If Abs(Sph3) LT 1 Then Begin
    Print, 'No Sph3 correction to send'
Endif Else Begin
    Print, 'Sending '+String(Sph3,Format='(F7.2)')+$
      'nm Sph3 to '+chan+' channel'
    SendSph3, Sph3, Red=Red, Sim=Sim, Logfile=Logfile, WRSMode=1
Endelse
Return
End


; Returns 1 if all elements within limits
;
Function CheckAbLimits, aberrIn, Limit=Limit, Seeing=Seeing
;
If Not KeyWord_Set(Limit) Then limit = 420.
limit = Abs(limit)

If Not KeyWord_Set(Seeing) Then seeing = 0.
seeing = Abs(seeing)

; index into aberrIn array
idx = [3,4,5,6,7,10,21]

; array of limit values
;
; changed z4,z5,z6,z11 limits from 400 to 250 (in lbcfpia.cfg file),
; and z22 limit to 50
; based on the frequent presence of residual constant astig in data
; OPK - 15 April 2010
; this line below seems to be ignored in the flow of the program, 
; instead criteria are all what is set in lbcfpia.cfg - except z22=100
; 
Limits = [limit,limit,limit,400,400,limit,50]
;Limits = [limit,limit,limit,limit,limit,limit,50]
;Limits = [limit,limit,limit,limit,limit,limit,101] 
;changed Z22 threshold from 50 back to 101 because of constant buildup of
; 100 nm Z22 in marginal seeing conditions - causing mirror panic eventually.
; on 24 Oct 2009, we changed BlueZ22Zero from 2.3 to 1.5. We think this will
; result in not sending such large (100) values of Z22 to Blue, and therefore
; are reinstating the 100 nm threshold. 

;Print, "Limits: ", Limits
Print, "Limits: ", Limits, format="(a8,7f8.1)"

; check for all zero coefficients
; where LBCPFIA thought there was an insane value

values = Abs(Float(aberrIn[idx]))

Z = Where(values GT 0)

If (Size(Z))[0] EQ 0 Then Begin
    Print, "Not converged because all coefficients are zero."
    Return, 0
Endif

; check aberration coeffcients relative to limits
; if estimated seeing is worse than 1 arcsec, multiply the thresholds
;    by the seeing value JMH 20090916
;If seeing GT 1.0 Then Begin
;    diffs = Abs(Float(aberrIn[idx])) - seeing * Limits
;Endif Else Begin
;    diffs = Abs(Float(aberrIn[idx])) - Limits
;Endelse
; moved the threshold for increasing the limits down from seeing 
;   1.0 to 0.8 JMH 20111018 
; Thus limits are 20% looser above 0.8 arcsec estimated seeing.
If seeing GT 0.8 Then Begin
    diffs = Abs(Float(aberrIn[idx])) - (seeing/0.8) * Limits
Endif Else Begin
    diffs = Abs(Float(aberrIn[idx])) - Limits
Endelse

;; Print,Abs(Float(aberr[idx]))

Q = Where(diffs GE 0)

If (Size(Q))[0] GT 0 Then Return, 0

Return, 1
End


; Main Program
Pro DoFPIA_wrs, Sim=Sim, X2=X2, RedOnly=RedOnly, BlueOnly=BlueOnly, $
            RedLimit=RedLimit, BlueLimit=BlueLimit, $
            RedInitial=RedInitial, BlueInitial=BlueInitial, $
            MaxIterations=MaxIterations, First=First, BackOut=BackOut, AskMe=AskMe
;
; Read configuration parameters from file
;
cfg = lbcfpia_read_cfg(ErrCode=ErrCode,ErrMsg=ErrMsg,WRSMode=1)
If ErrCode NE 0 Then Message, ErrMsg

; Check flag for increasing exposure time by gain of 2
If KeyWord_Set(X2) Then Gain='TWO' Else Gain='ONE'

; Check parameters for aberration threshold of convergence
If Not KeyWord_Set(RedLimit) Then RedLimit = cfg.RedLimit
RedLimit = Abs(RedLimit)

If Not KeyWord_Set(BlueLimit) Then BlueLimit = cfg.BlueLimit
BlueLimit = Abs(BlueLimit)


; Check parameters for initial send of Z11
If Keyword_Set(First) AND NOT Keyword_Set(RedInitial) Then RedInitial = cfg.FirstRedInitial

If Not KeyWord_Set(RedInitial) Then RedInitial = cfg.RedInitial

If Keyword_Set(First) AND NOT Keyword_Set(BlueInitial) Then BlueInitial = cfg.FirstBlueInitial

If Not KeyWord_Set(BlueInitial) Then BlueInitial = cfg.BlueInitial


; Check parameter for maximum number of iterations
If Not KeyWord_Set(MaxIterations) Then MaxIterations = cfg.DOFPIAMaxIter
If MaxIterations LE 0 Then MaxIterations = 1


; Setup Loop Control Parameters
; 2 means ready to analyze, 1 means converged, 0 means don't analyze
LoopMessage = ["not used", "converged", "analyzing"]
DoRed  = 2
DoBlue = 2
LoopCount = 1

If KeyWord_Set(RedOnly) Then DoBlue=0 ; Don't analyze Blue

If KeyWord_Set(BlueOnly) Then DoRed=0 ; Don't analyze Red

If KeyWord_Set(BlueOnly) AND KeyWord_Set(RedOnly) Then Begin
    Print, "Error - BlueOnly and RedOnly must be used separately."
    Return ; Exit Program
Endif

RedSeeing = 0.0
BlueSeeing = 0.0

BadZ11Red = 1
BadZ11Blue = 1


; Check flag to Back Out initial correction if a previous script has failed
;
If KeyWord_Set(BackOut) Then Begin
BACK_Z11:
    If DoRed GE 1 Then Begin
        Print, "Reversing ",-RedInitial,"nm Sph3 (Z11) to RED channel"
        SendSph3, -RedInitial, /Red, Sim=Sim, Logfile=Logfile, WRSMode=1
    Endif
    If DoBlue GE 1 Then Begin
        Print, "Reversing ",-BlueInitial,"nm Sph3 (Z11) to BLUE channel"
        SendSph3, -BlueInitial, Sim=Sim, Logfile=Logfile, WRSMode=1
    Endif
    Return ; Exit Program
Endif


; Send initial Sph3 (Z11) to mirrors 
;   How can we move both mirrors in parallel to save time?
;
If DoRed GE 1 Then Begin
   Print, "Sending ",RedInitial,"nm Sph3 (Z11) to RED channel"
    SendSph3, RedInitial, /Red, Sim=Sim, Logfile=Logfile, WRSMode=1
Endif
If DoBlue GE 1 Then Begin
    Print, "Sending ",BlueInitial,"nm Sph3 (Z11) to BLUE channel"
    SendSph3, BlueInitial, Sim=Sim, Logfile=Logfile, WRSMode=1
Endif


; Take home directory (path to OB file) from env var
HomeDir = GetEnv("WRS_HOME")
If HomeDir EQ "" Then HomeDir="."
;Print, HomeDir
DataDir = GetEnv("WRS_DATADIR")
;If DataDir EQ "" Then DataDir="/Repository"
If DataDir EQ "" Then DataDir="/TestRepo"


If ( KeyWord_Set(AskMe) AND Not KeyWord_Set(Sim) ) Then Begin
    Print, "DOFPIA in /AskMe mode will prompt before sending Zernikes."
EndIf


; Load IA OB 
Print, "Loading OB DoBlue:", DoBlue, "  DoRed:", DoRed
; warning OB directory path is hardwired to the particular sub-directory
; warning load-ob.pl has a hardwired path to the OB file
;
; could we trap errors from these spawn calls?
If Not KeyWord_Set(Sim) Then Begin
  If (DoBlue EQ 0) AND (DoRed GE 1) Then Begin
      Print, "Loading OB ", HomeDir+"/src/load-ob-red.pl"
      Spawn, HomeDir+"/src/load-ob-red.pl", Result, ErrResult
  EndIf  
  If (DoBlue GE 1) AND (DoRed EQ 0) Then Begin
      Print, "Loading OB ", HomeDir+"/src/load-ob-blue.pl"
      Spawn, HomeDir+"/src/load-ob-blue.pl", Result, ErrResult
  EndIf
  If (DoBlue GE 1) AND (DoRed GE 1) Then Begin
      Print, "Loading OB ", HomeDir+"/src/load-ob.pl"
      Spawn, HomeDir+"/src/load-ob.pl", Result, ErrResult
  EndIf
  If (DoBlue EQ 0) AND (DoRed EQ 0) Then Begin
      Print, "Not loading OB"
  EndIf

Wait, 3 ; for the OB loading
Endif Else Begin
  Print, "Simulation mode without loading OB"
EndElse


; Loop
Repeat Begin
;
; Run OB
;    print, "OB Loaded, about to play it, proceed? (y/n)"
;    response = ''
;    read, response
;    If StrLowCase(Response) NE 'y' Then Return

; The following delay is needed if you are not analyzing both Red and Blue.
; This is because LBC needs ~19 sec after readout to complete the OB.
; One analysis can happen in less than this time so the new play
; command collides with the previous one.
; Added by JMH 18-APR-2009.
    If DoRed EQ 0 OR DoBlue EQ 0 Then WAIT, 10

    Print, "Running OB, Iteration = ", LoopCount
    If Not KeyWord_Set(Sim) Then Begin
        If Gain EQ "TWO" Then Begin
            Spawn, HomeDir+"/src/play-obx2.pl", Result, ErrResult
; warning above directory path is hardwired to the particular
; sub-directory
        Endif Else Begin
            Spawn, HomeDir+"/src/play-ob.pl", Result, ErrResult
; warning above directory path is hardwired to the particular sub-directory
        Endelse
    Endif Else Begin
        Print, "Simulation mode without playing OB"
    EndElse

;
; Wait for image to appear
    Print, 'Waiting for OB to finish and data files to appear in '+DataDir
    If Gain EQ "TWO" Then Begin
        Wait, 49
    Endif Else Begin
        Wait, 33 
    Endelse

    If (DoBlue EQ 0) AND (DoRed GE 1) Then Begin
        Check = Wait4OBFile_Red(dir=DataDir)
    EndIf  
    If (DoBlue GE 1) AND (DoRed EQ 0) Then Begin
        Check = Wait4OBFile_Blue(dir=DataDir)
    EndIf
    If (DoBlue GE 1) AND (DoRed GE 1) Then Begin
        Check = Wait4OBFiles(dir=DataDir)
    EndIf

;
; Check to see if the user pressed any key, but without waiting
; http://idlastro.gsfc.nasa.gov/idl_html_help/GET_KBRD.html
;
; Warning: This check happens two places in the repeat loop
;
    KeyString = GET_KBRD(0)
; If the string is a capital S, the user wants to stop the loop
    If KeyString EQ 'S' Then Begin
        Print, "User Stop requested after ", LoopCount, " iterations."
        Break
    Endif
; If the string is a capital A, the user wants LBCFPIA to ask before
; sending corrections to the mirrors
    If KeyString EQ 'A' Then Begin
        Print, "AskMe requested after ", LoopCount, " iterations."
        AskMe=1
    EndIf
; If the string is a captial F, the user wants LBCFPIA to stop asking
; before sending corrections to the mirrors
    If KeyString EQ 'F' Then Begin
        Print, "AskMe cancelled after ", LoopCount, " iterations."
        AskMe=0
    EndIf

    If Check NE 1 Then Begin
        Print, '******* WARNING *******'
        Print, 'Data files DID NOT appear in '+DataDir
        Print, 'Pls double check and then: press Q to exit or any other key to continue'
        Response = ''
        Read, Response
        If Response EQ 'Q' Then Return
    EndIf

;
; do LBCFPIA on Red
    If DoRed GE 1 Then Begin
        If Not KeyWord_Set(Sim) Then Begin
            If Keyword_Set(AskMe) Then Begin
                lbcfpia_WRS, /Red, ires=ires, estSeeing=RedSeeing, cfg=cfg
            Endif Else Begin
                lbcfpia_WRS, /Red, /ForceSend, ires=ires, estSeeing=RedSeeing, cfg=cfg
            Endelse
        ;Print, 'Are you satisfied with RED CHANNEL correction? (y/n)'
        ;Response = ''
        ;Read, Response
        ;If StrLowCase(Response) EQ 'y' Then Begin
            Print, "Checking for Red Convergence"
            If N_ELEMENTS(ires) GT 0 Then Begin
                If CheckAbLimits(ires, Limit=RedLimit, Seeing=RedSeeing) EQ 1 Then Begin
                    Print, "Red Convergence reached for seeing of ", RedSeeing, " arcsec."
                    DoRed = 1
                Endif
                ; Check if there has been any Z11 correction
                If abs(ires[10]) GT 0 Then BadZ11Red = 0
            Endif Else Begin
                Print, "Red WRS analysis failed."
                If LoopCount LE 1 Then Begin
                    Print, "Going to backoff the initial Z11 and exit."
                    GOTO, BACK_Z11
                EndIf
            EndElse
        Endif Else Begin
            lbcfpia_WRS, /Red, /DontSend, ires=ires, estSeeing=RedSeeing, cfg=cfg
            Print, "Simulation Checking for Red Convergence"
            If N_ELEMENTS(ires) GT 0 Then Begin
                If CheckAbLimits(ires, Limit=RedLimit, Seeing=RedSeeing) EQ 1 Then Begin
                    Print, "Red Convergence reached for seeing of ", RedSeeing, " arcsec."
                    DoRed = 1
                Endif
                ; Check if there has been any Z11 correction
                If abs(ires[10]) GT 0 Then BadZ11Red = 0
            Endif Else Begin
                Print, "Red WRS analysis failed."
                If LoopCount LE 1 Then Begin
                    Print, "Going to backoff the initial Z11 and exit."
                    GOTO, BACK_Z11
                EndIf
            EndElse
        EndElse
    EndIf

;
; do LBCFPIA on Blue
    If DoBlue GE 1 Then Begin
        If Not KeyWord_Set(Sim) Then Begin
            If KeyWord_Set(AskMe) Then Begin
                lbcfpia_WRS, ires=ires, estSeeing=BlueSeeing, cfg=cfg
            Endif Else Begin
                lbcfpia_WRS, /ForceSend, ires=ires, estSeeing=BlueSeeing, cfg=cfg
            EndElse
        ;Print, 'Are you satisfied with BLUE CHANNEL correction? (y/n)'
        ;Response = ''
        ;Read, Response
        ;If StrLowCase(Response) EQ 'y' Then Begin
            Print, "Checking for Blue Convergence"
            If N_ELEMENTS(ires) GT 0 Then Begin
                If CheckAbLimits(ires, Limit=BlueLimit, Seeing=BlueSeeing) EQ 1 Then Begin
                    Print, "Blue Convergence reached for seeing of ", BlueSeeing, " arcsec."
                    DoBlue = 1
                Endif
                ; Check if there has been any Z11 correction
                If abs(ires[10]) GT 0 Then BadZ11Blue = 0
            Endif Else Begin
                Print, "Blue WRS analysis failed."
                If LoopCount LE 1 Then Begin
                    Print, "Going to backoff the initial Z11 and exit."
                    GOTO, BACK_Z11
                EndIf
            EndElse
        Endif Else Begin
            lbcfpia_WRS, /DontSend, ires=ires, estSeeing=BlueSeeing, cfg=cfg
            Print, "Simulation Checking for Blue Convergence"
            If N_ELEMENTS(ires) GT 0 Then Begin
                If CheckAbLimits(ires, Limit=BlueLimit, Seeing=BlueSeeing) EQ 1 Then Begin
                    Print, "Blue Convergence reached for seeing of ", BlueSeeing, " arcsec."
                    DoBlue = 1
                EndIf
                ; Check if there has been any Z11 correction
                If abs(ires[10]) GT 0 Then BadZ11Blue = 0
            Endif Else Begin
                Print, "Blue WRS analysis failed."
                If LoopCount LE 1 Then Begin
                    Print, "Going to backoff the initial Z11 and exit."
                    GOTO, BACK_Z11
                EndIf
            EndElse
        EndElse
    EndIf

;
; Check to see if the user pressed any key, but without waiting
; http://idlastro.gsfc.nasa.gov/idl_html_help/GET_KBRD.html
;
; Warning: This check happens two places in the repeat loop
;
    KeyString = GET_KBRD(0)
; If the string is a capital S, the user wants to stop the loop
    If KeyString EQ 'S' Then Begin
        Print, "User Stop requested after ", LoopCount, " iterations."
        Break
    Endif
; If the string is a capital A, the user wants LBCFPIA to ask before
; sending corrections to the mirrors
    If KeyString EQ 'A' Then Begin
        Print, "AskMe requested after ", LoopCount, " iterations."
        AskMe=1
    EndIf
; If the string is a captial F, the user wants LBCFPIA to stop asking
; before sending corrections to the mirrors
    If KeyString EQ 'F' Then Begin
        Print, "AskMe cancelled after ", LoopCount, " iterations."
        AskMe=0
    EndIf



; Increment the loop counter
    LoopCount = LoopCount + 1


;
; Are we finished?
    Print,"Red is "+LoopMessage(DoRed)+",  Blue is "+LoopMessage(DoBlue)

Endrep Until ( ((DoRed LE 1) AND (DoBlue LE 1)) OR (LoopCount GT MaxIterations) )

If (LoopCount GE MaxIterations) Then Print, "Maximum number of iterations exceeded ", MaxIterations, " without convergence."


; Send the final corrections
 ;
FINAL_Z11:
If DoRed GE 1 Then Begin
    If BadZ11Red GE 1 Then Begin
        Print, "Reversing ",-RedInitial,"nm Sph3 (Z11) to RED channel since there was no Z11 correction."
        SendSph3, -RedInitial, /Red, Sim=Sim, Logfile=Logfile, WRSMode=1
    Endif Else Begin
        Sph3 = cfg.RedFinalZ11Zero+cfg.RedFinalZ11Scale*RedSeeing
        Print, "Sending last Sph3 (Z11) correction for RED channel ",Sph3
        SendSph3, Sph3, /Red, Sim=Sim, Logfile=Logfile, WRSMode=1
    EndElse
Endif

If DoBlue GE 1 Then Begin 
    If BadZ11Blue GE 1 Then Begin
        Print, "Reversing ",-BlueInitial,"nm Sph3 (Z11) to BLUE channel since there was no Z11 correction."
        SendSph3, -BlueInitial, Sim=Sim, Logfile=Logfile, WRSMode=1
    Endif Else Begin
        Sph3 = cfg.BlueFinalZ11Zero+cfg.BlueFinalZ11Scale*BlueSeeing
        Print, "Sending last Sph3 (Z11) correction for BLUE channel ",Sph3
        SendSph3, Sph3, Sim=Sim, Logfile=Logfile, WRSMode=1
    EndElse
Endif

Print, "DOFPIA finished"
Return
End
