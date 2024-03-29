;+
; Main program for fitting an extra focal image of the pupil acquired
; with scientific array of LBC.
;
; @keyword Red {in}{optional}{type=string} Specifies that
;    images from red channel are to be processed.
; @keyword DataDir {in}{optional}{type=string} Full path of the
;    directory where images are stored. If omitted, it is taken from
;    the LBCFPIA_DATADIR environment variable. If not defined then a
;    default is used.
; @keyword Interactive {in}{optional}{type=boolean} If set, file to be
;    processed is not selected automatically (the newest file in DataDir)
;    but interactively.
; @keyword DontSend {in}{optional}{type=boolean} If set, then computed
;    corrections are not sent to TCS.
; @keyword NoDisplay {in}{optional}{type=boolean} Do not output
;    anything on the display.
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
; @keyword Pups {out}{optional} Name of a variable that will receive
;    the array of pupils selected for IA.
; @keyword Cen {out}{optional} Name of a variable that will receive
;    the array of center positions of the pupils selected for IA.
; @keyword File {in}{optional} Name of a file, instead of interactive
;    keyword or using the latest file
; @keyword UniqueOut {in}{optional}{type=boolean} If set, creates a
;    unique output name for each file, changing "fits" to "coeffs" and
;    writing to the current directory
; @keyword ImgSec {in}{optional} Array for the image section to sample
;    for pupils [X1,X2,Y1,Y2]. Default is [2403,4450,3408,4608], the top
;    1000 pixels of chip 2.  This is IRAF 1-based DETSEC format.
;    If not a multiextension FITS file, interprets ImgSec as 0-based.
; @keyword LBCChip {in}{optional} specifies the chip on which to do the
;    analysis (default = 2)
; @keyword NoBckSub {in}{optional}{type=boolean} If set, does not
;    subtract the background from the pupils. This was the default prior
;    to and including src-20060628.
; @keyword NoPreProc {in}{optional}{type=boolean} If set,
;    preprocessing step (i.e. bias subtraction and FF division) is
;    not performed.
; @keyword PupCen {out}{optional} Contains the xy coordinates and the
;    diameters of all found pupils.
; @keyword Log {out}{optional} Contains the name of the output log file, 
;     by default called lbcfpialog (opk)
;
; @history Added IDLDoc documentation on May 15, 2006
; @history Added capability to estimate focus & tilt on June 23, 2006 JDE
; @history Changed ImgSec to DETSEC format on July 10, 2006 JMH
; @history Changed Spawn of TCSSendWavefront to absolute path Nov 2006
; @history Read FOCUSOFF and FILTEOFF from header on January 26, 2007 JMH
; @history Added 'Red' keyword on Nov 21, 2007 ABA
; @history Increased Z4 formatting to I6 - 19-Mar-2009 JMH
; @history Adjusted Z4 limit 4000 --> 6000 nm - 19-Mar-2009 JMH
; @history Added compensation of Z4 with Z11 correction - 19-Mar-2009 JMH
; @history Added wiq wavefront image estimation - 19-Mar-2009 JMH
; @history Added wiq and filter to logfile output - 19-Mar-2009 JMH
; @history Adjusted the scale of Z4/Z11 correction - 26-Mar-2009 JMH
; @history Implement parameter variables and Z22 - 02-Jun-2009 ABA
; @history Adjusted format for logfile output - 09-Jun-2009 JMH
; @history Exit if the configuration file is not used - 18-Jun-2009 JMH
;
;
; @history Converted to lbcfpia_wrs MS
; @history Added rotation of zernikes to M1 frame - 15-June-2014 JMH
; @history call_wrs keywords blue and red added at line 255. 26 Mar 2015 MS
;

; @author Andrea Baruffolo, INAF - OAPd
; @version $Id$
;-
Pro lbcfpia_wrs, DataDir=DataDir, Interactive=Interactive, $
             Red=Red, Tec=Tec, $
             DontSend=DontSend, NoDisplay=NoDisplay, $
             cfg=cfg, $
             dbg=dbg, LogLevel=LogLevel, $
             Pups=P, Cen=Cen, PrintRes=PrintRes,$
             PRad=PRad,File=File,UniqueOut=UniqueOut, DoIntFit=DoIntFit,$
             ImgSec=ImgSec,LBCChip=LBCChip,NoBckSub=NoBckSub,PupCen=PupCen, $
             DoPreProc=DoPreProc, Blobs=Blobs, Seeing=Seeing, $
             SphGain=SphGain, LogFile=LogFile, $
             invMoments=invMoments, objs=objs, oldbck=oldbck, $
             pEdges=pEdges, pFits=pFits, $
             ires=ires, rough_ires=rough_ires, OldPhot=OldPhot, $
             NewExtr=NewExtr, MaxCandidates=MaxCandidates, $
             ForceSend=ForceSend, estSeeing=estSeeing, $
             DontWrite=DontWrite, OutFile=OutFile, rawima=rawima

  If KeyWord_Set(cfg) Then Begin
    PRad        = cfg.PupilRadius
    ImgSec      = cfg.ImgSec
    LBCChip     = cfg.LBCChip
    SphGain     = cfg.SphGain
    Z4Z11Factor = cfg.Z4Z11Factor
    Z4Limit     = cfg.Z4Limit
    z4Crazy     = cfg.z4Crazy
    z11Limit    = cfg.z11Limit
    z11Crazy    = cfg.z11Crazy
    z22Lower    = cfg.z22Lower
    z22Upper    = cfg.z22Upper
    FocOffsTecRed  = cfg.FocOffsTecRed
    FocOffsTecBlue = cfg.FocOffsTecBlue
  Endif Else Begin
; Comment out the next two lines if you want to use the old defaults.
    LogAllWays, "No configuration parameters specified, returning."
    LogAllWays, "If you must use LBCFPIA standalone, use the following command:"
    LogAllWays, "cfg = lbcfpia_read_cfg(ErrCode=ErrCode,ErrMsg=ErrMsg, /dbg)"
    LogAllWays, "then call LBCFPIA as: lbcfpia, cfg=cfg"
    Return
; Otherwise we have exitted so you don't accidentally run w/o configuration.
    If Not KeyWord_Set(PRad) Then PRad=33
    If Not KeyWord_Set(ImgSec) Then Begin
        If KeyWord_Set(Tec) Then ImgSec = [50,2097,0,255] $
        Else ImgSec = [2403,4450,3201,4608]
    EndIf
    If Not KeyWord_Set(LBCChip) Then LBCChip = 2            
    If Not KeyWord_Set(SphGain) Then SphGain = 1.0
    Z4Z11Factor = -1.9
    z4Limit = 6000
    z4Crazy = 60000
    z11Limit = 1000
    z11Crazy = 3000
    z22Lower =  50.
    z22Upper = 150.
    FocOffsTecRed  = 14855.9
    FocOffsTecBlue = 16281.0
  Endelse

  get_date,dte
  print,dte

  If KeyWord_Set(dbg) Then LogLevel = 4
  If KeyWord_Set(LogLevel) Then SetLogLevel,LogLevel

  If Not KeyWord_Set(Tec) Then Begin
    If Not KeyWord_Set(Red) Then Begin
      basename = 'lbcb' 
      chan = 'blue'
      side = 'left'
      If Not KeyWord_Set(LogFile) Then Begin 
        LogFile = ('/home/LBCeng/WRSlogs/'+chan+dte+'.Log')
      endif else begin
        LogFile = LogFile
      endelse
    Endif Else Begin
      basename = 'lbcr'
      chan = 'red'
      side = 'right'
      If Not KeyWord_Set(LogFile) Then Begin 
        LogFile = ('/home/LBCeng/WRSlogs/'+chan+dte+'.Log')
      endif else begin
        LogFile = LogFile
      endelse
    EndElse
  EndIf Else Begin
    If Not KeyWord_Set(Red) Then Begin
      basename = 'lbcbtec' 
      chan = 'blue'
      side = 'left'
      If Not KeyWord_Set(LogFile) Then Begin 
        LogFile = ('/home/LBCeng/WRSlogs/'+chan+dte+'.TecLog')
      endif else begin
        LogFile = LogFile
      endelse
    Endif Else Begin
      basename = 'lbcrtec'
      chan = 'red'
      side = 'right'
      If Not KeyWord_Set(LogFile) Then Begin 
        LogFile = ('/home/LBCeng/WRSlogs/'+chan+dte+'.TecLog')
      endif else begin
        LogFile = LogFile
      endelse
    EndElse
  EndElse

; If not specified by caller, take data dir from env var
  If Not KeyWord_Set(DataDir) Then DataDir = GetEnv("LBCFPIA_DATADIR")
; If not specified at all, assume data dir is /Repository
  If DataDir EQ "" Then Begin
     DataDir = "/Repository"
  Endif

; Take home directory (path to executable for TCSSendWavefront) from env var
  HomeDir = GetEnv("WRS_HOME")
  If HomeDir EQ "" Then HomeDir="."

; check whether logfile already exists, if not open & change permisssions
; insure that all potential FPIA users can write to it
  If (file_test(logfile) EQ 0) Then Begin
	; Open the logfile
  	OpenW, outUnit, logfile, /Get_Lun , /append
       file_chmod,logfile,/a_write,/a_read
  EndIf else begin
  	OpenW, outUnit, logfile, /Get_Lun , /append
  EndElse
;
; Fallback to default if necessary
;
  If KeyWord_Set(Interactive) Then Begin
     FileName = Dialog_PickFile(Path=DataDir,Filter=basename+".*.*.fits",/Multi)
     If Size(FileName, /N_Elements) EQ 0 Then Begin
       If FileName EQ '' Then Begin
          LogAllWays, "No file selected, returning"
          Return
       EndIf
     EndIf Else Begin
       If N_Elements(FileName) EQ 0 Then Begin
          LogAllWays, "No file selected, returning"
          Return
       EndIf
     EndElse
  Endif Else If Keyword_Set(File) Then Begin
     FileName = DataDir + Path_Sep() + File
  Endif Else begin
     FileName = GetNewestFile(DataDir,basename,dbg=dbg)
  Endelse

  ExtDbgLog, "Before GetImage ImgSec is: "+String(ImgSec)

  NFiles = N_Elements(FileName)
  If NFiles GT 1 Then Begin
     ; Cast to ULong (32 bit unsigned) to avoid overflow
     Ima = ULong(GetImage(FileName[0], ImgSec, RA=RA, dbg=dbg, LBCChip=LBCChip))
     For I=1,NFiles-1 Do Begin
        Ima += GetImage(FileName[I], ImgSec, RA=RA, dbg=dbg, LBCChip=LBCChip)
     EndFor
     FileName = FileName[0]
     rawima=ima
  Endif Else Begin
     LogAllWays, "Processing: "+FileName
     Ima = GetImage(FileName, ImgSec, RA=RA, dbg=dbg, LBCChip=LBCChip)
  EndElse

  ExtDbgLog, "After GetImage ImgSec is: "+String(ImgSec)
  ExtDbgLog, "Rotator Angle = "+ToS(RA)

  If KeyWord_Set(DoPreProc) Then Begin
     ExtDbgLog, "Bias subtraction and ff division..."
     Ima = PreProcIma(Ima, ImgSec, LBCChip=LBCChip, dbg=dbg)
  EndIf

  If KeyWord_Set(Seeing) Then Begin
     ExtDbgLog, "Convolving with seeing FWHM = "+ToS(Seeing)
     Ima = Convolve(Ima, PSF_Gaussian(NPixel=32, FWHM=Seeing, NDimen=2, /Normalize))
  EndIf




;WRS added instead of calcabcoeffs and FindPupils by Marco Stangalini
;==============================================================================================
;==================================CALLING WRS FOR ZE ESTIM.===================================
;==============================================================================================

if keyword_set(red) then begin
res=CALL_WRS(ima, /red)
endif else begin
res=CALL_WRS(ima, /blue)
endelse

res=res*1e3 ; conversion from mu to nm
;==============================================================================================
;==============================================================================================
;==============================================================================================


;Included to solve a problem with res dimension
re=fltarr(22)
re(0:n_elements(res)-1)=res
res=re




; get the current filter and focus offsets
  hdr = headfits(FileName)
  CurrentFocus = double(sxpar(hdr, 'FOCUSOFF'))
  OffsetFocus =  double(sxpar(hdr, 'FILTEOFF'))
  filter = sxpar(hdr,'FILTER')

; calculate the extrafocal distance
;  CurrentFocus = CurrentFocus - OffsetFocus

  LogAllWays, "Focus Offset = "+ToS(CurrentFocus)+"  Filter = "+filter+"  Rotator = "+ToS(RA)

; added on 13 June 2014 JH OK
deltaZ4 = CurrentFocus * (1000 / 0.0379) ; converts focus offset of primary (mm) to deltaz4(nm)
res(3) = deltaZ4 + res(3) ; res(3) around 20000 nm, addition of deltaZ4 (negative number) should result in a value near zero.


  If KeyWord_Set(Tec) Then Begin
      If KeyWord_Set(Red) Then res[3] -= FocOffsTecRed Else res[3] -= FocOffsTecBlue
  EndIf


  ; Use the LBC Rotator Angle to rotate the WRS Zernikes into the
  ;   coordinates of the primary mirror
  ; This was done in calcabcoeffs.pro for LBCFPIA.
  ; REF: 481s062 for zernike orders
  ; Signs match from those used by D. L. Miller in zernike_rotate.pro
  ; Added by JMH 20140615 UT

  newres = res ; copy the array of Zernikes, zero-based indexing
  RAr = RA/180.*!PI ; convert the rotator angle to radians
  ; This does nothing to Tip and Tilt Z2, Z3
  ; Focus, Spherical, 5th Spherical have order=0 ---> azimuthal symmetry
  ; Astigmatism, order=2
  newres[4] = res[4]*cos(2.0*RAr) + res[5]*sin(2.0*RAr)  ; Z5 
  newres[5] = res[5]*cos(2.0*RAr) - res[4]*sin(2.0*RAr)  ; Z6
  ; Coma, order=1
  newres[6] = res[6]*cos(1.0*RAr) + res[7]*sin(1.0*RAr)  ; Z7
  newres[7] = res[7]*cos(1.0*RAr) - res[6]*sin(1.0*RAr)  ; Z8
  ; Trefoil, order=3
  newres[8] = res[8]*cos(3.0*RAr) + res[9]*sin(3.0*RAr)  ; Z9
  newres[9] = res[9]*cos(3.0*RAr) - res[8]*sin(3.0*RAr)  ; Z10
  ; 5th Astigmatism, order=2
  newres[11] = res[11]*cos(2.0*RAr) + res[12]*sin(2.0*RAr) ; Z12
  newres[12] = res[12]*cos(2.0*RAr) - res[11]*sin(2.0*RAr) ; Z13
  ; Quadrafoil, order=4
  newres[13] = res[13]*cos(4.0*RAr) + res[14]*sin(4.0*RAr) ; Z14
  newres[14] = res[14]*cos(4.0*RAr) - res[13]*sin(4.0*RAr) ; Z15
  ; 5th Coma, order=1
  newres[15] = res[15]*cos(1.0*RAr) + res[16]*sin(1.0*RAr) ; Z16
  newres[16] = res[16]*cos(1.0*RAr) - res[15]*sin(1.0*RAr) ; Z17
  ; 5th Trefoil, order=3
  newres[17] = res[17]*cos(3.0*RAr) + res[18]*sin(3.0*RAr) ; Z18
  newres[18] = res[18]*cos(3.0*RAr) - res[17]*sin(3.0*RAr) ; Z19
  ; Pentafoil, order=5
  newres[19] = res[19]*cos(5.0*RAr) + res[20]*sin(5.0*RAr) ; Z20
  newres[20] = res[20]*cos(5.0*RAr) - res[19]*sin(5.0*RAr) ; Z21
  res = newres ; copy back to the original array

  print, "***************** Rotated Zernikes in nm starting from focus **********************"
  print, res[3:10]

  ; End of Zernike rotation for Z4-Z22

  ; recover the estimated seeing used by WRS - JMH 20140615
  estSeeing = res[0]/1000.0 ; arcsec FWHM


  ;
  ; This is used for debugging only. It is returned to
  ; caller if rough_ires keyword specified and contains
  ; the computed aberrations without any 'sanitization'
  ; (i.e. limit to z4 or z11)
  ;
  rough_ires = Fix(res)

  If Not KeyWord_Set(OutFile) Then Begin
     if KeyWord_Set(UniqueOut) then begin
        filechunks = strsplit(filename,'/',/extract)
        filechunks = strsplit(filechunks[n_elements(filechunks)-1],'.',/extract)
        OutFile = filechunks[0]+'.'+filechunks[1]+'.'+filechunks[2]+'.coeffs'
     endif else Begin
        OutFile = 'lbciaCoeffs_'+chan+'.dat'
     Endelse 
  Endif 

;
; Check aberration results for sanity
;
  If abs(res[3]) GT z4Limit Then Begin
     LogAllWays,"WARNING: Z4 TOO LARGE, will consider defocus only "+ToS(res[3])
     LogAllWays,"         all other aberration coefficients set zero."
     z4 = res[3]
     res[*] = 0.
     res[3] = z4 
  EndIf

  If abs(res[3]) GT z4Crazy Then Begin
     LogAllWays,"WARNING: Z4 INSANELY LARGE, do not use this correction "+ToS(res[3])
     LogAllWays,"         all aberration coefficients set zero."
     z4 = res[3]
     res[*] = 0.
  EndIf

  If abs(res[10]) GT z11Limit Then Begin
     LogAllWays,"WARNING: Z11 TOO LARGE "+ToS(res[10])
     If res[10] GT 0. Then Begin
        LogAllWays,"         Truncating value to "+ToS(z11Limit)
        res[10] = z11Limit 
     EndIf Else Begin
        LogAllWays,"         Truncating value to "+ToS(-z11Limit)
        res[10] = -z11Limit
     EndElse
  EndIf

  If abs(res[10]) GT z11Crazy Then Begin
     LogAllWays,"WARNING: Z11 INSANELY LARGE, do not use this correction "+ToS(res[10])
     LogAllWays,"         all aberration coefficients set zero."
     z11 = res[10]
     res[*] = 0.
  Endif

  ;; If abs(res[21]) LT z22Lower Then res[21] = 0.
  If abs(res[21]) GT z22Upper Then res[21] = res[21] LT 0 ? -z22Upper : z22Upper

;
; Compensation of Z11 correction with Z4 - added by JMH 19-Mar-2009
;     This is needed because a Z11 correction changes the extrafocal pupil diameter.  
;     See Issue #2031
;
  If abs(res[10]) GT 0. Then Begin
;      res[3] = res[3] + 0.9 * res[10] ; empirical factor 19-MAR-2009
      res[3] = res[3] + Z4Z11Factor * res[10] ; corrected factor 26-MAR-2009
  EndIf
  
;
; Estimation of image quality from residual aberrations - added by JMH 19-Mar-2009
;     This is adapted from A. Rakich calculation of 25-Nov-2008
;
  wiq = 0.00054289*res[3]*res[3] + 0.00030276*res[4]*res[4] + 0.00030276*res[5]*res[5] + $
        0.00070225*res[6]*res[6] + 0.00070225*res[7]*res[7] + 0.0013913*res[10]*res[10]
  wiq = 0.01691 * sqrt(wiq + 21.4554)



  If Not KeyWord_Set(DontWrite) Then writeOutFile, res, res*0.0, OutFile=OutFile

  If KeyWord_Set(PrintRes) Then LogAllWays,String(Res)

  IRes = Fix(Res)

  LogAllWays,"--------------------------------------------------------------------------"
  LogAllWays,"          Computed Aberrations (nm)              |   (arcsec)   | channel "
  LogAllWays," Defoc   AstX   AstY   ComaY  ComaX   Sph        |  Est.   Est. |         "
  LogAllWays," (Z4)    (Z5)   (Z6)   (Z7)   (Z8)   (Z11)  (Z22)| seeing  WIQ  |         "
  LogAllWays,"--------------------------------------------------------------------------"
  Msg = String(ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],ires[21],estSeeing,wiq,chan,$
               Format="(I6,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,3X,F4.2,3X,F4.2,3X,A5)")
  ;Msg = String(ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],ires[21],estSeeing,wiq,$
  ;             Format="(I6,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,3X,F4.2,3X,F4.2)")
  LogAllWays,Msg
  ; Msg = "Estimated UNCALIBRATED Z22 = "+ToS(Z22)
  ; LogAllWays,Msg
  LogAllWays,"--------------------------------------------------------------------------"




;
; send corrections to telescope
;
  If Not KeyWord_Set(DontSend) Then Begin
      If Not KeyWord_Set(ForceSend) Then Begin
          print, 'Do you want to send (y/n)?'
          response = ''
          read, response
      Endif Else Begin
          response = 'y'
      EndElse
     If StrLowCase(response) eq 'y' then Begin
        PrgName = HomeDir+Path_Sep()+"TCSSendWavefront"+Path_Sep()+"TCSSendWavefront " + OutFile + " " + side
        Spawn, PrgName
     EndIf
  EndIf Else Begin
     LogAllWays, "Will NOT send corrections to telescope, as per user request."
     response = 'n'
  EndElse

; write to a log file   
; (format adjusted to add seeing by JMH 20080129)
; (format adjusted to add Z22 by AB/JMH 20090609)
  Printf,outunit,ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],ires[21],response,estSeeing,wiq,filename,filter, $
         Format="(I6,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,A3,2X,F4.2,2x,F4.2,2X,A54,2X,A8)"

  free_lun, OutUnit
  Return
End
