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
; by default called lbcfpialog (opk)
;
; @history Added IDLDoc documentation on May 15, 2006
; @history Added capability to estimate focus & tilt on June 23, 2006 JDE
; @history Changed ImgSec to DETSEC format on July 10, 2006 JMH
; @history Changed Spawn of TCSSendWavefront to absolute path Nov 2006
; @history Read FOCUSOFF and FILTEOFF from header on January 26, 2007 JMH
; @history Added 'Red' keyword on Nov 21, 2007 ABA
; @history Special Test Version for Tech Chips on Feb 29, 2008 JMH
; @author Andrea Baruffolo, INAF - OAPd
; @version $Id$
;-
Pro tecfpia, DataDir=DataDir, Interactive=Interactive, $
             DontSend=DontSend, NoDisplay=NoDisplay, $
             dbg=dbg, Pups=P, Cen=Cen, PrintRes=PrintRes,pippo=pippo,$
             PRad=PRad,File=File,UniqueOut=UniqueOut, DoIntFit=DoIntFit,$
             ImgSec=ImgSec,LBCChip=LBCChip,NoBckSub=NoBckSub,PupCen=PupCen, $
             DoPreProc=DoPreProc, Blobs=Blobs, Seeing=Seeing, $
             SphGain=SphGain, LogFile=LogFile, $
             Red=Red, invMoments=invMoments, objs=objs, oldbck=oldbck, $
             pEdges=pEdges, pFits=pFits, UseFitOnly=UseFitOnly, $
             ires=ires, rough_ires=rough_ires, OldPhot=OldPhot, $
             NewExtr=NewExtr

  get_date,dte
  print,dte
  If Not KeyWord_Set(PRad) Then PRad=24                     ;Tech
  If Not KeyWord_Set(ImgSec) Then ImgSec = [51,2300,2,255]  ;Tech
;;;;If Not KeyWord_Set(ImgSec) Then ImgSec = [2403,4202,1,900]
  If Not KeyWord_Set(LBCChip) Then LBCChip = 2            
  If Not KeyWord_Set(SphGain) Then SphGain = 1.0
  If Not KeyWord_Set(Red) Then Begin
      basename = 'lbcbtec'                                  ;Tech
     chan = 'blue'
     side = 'left'
     If Not KeyWord_Set(LogFile) Then Begin 
         ;LogFile = ("tecfpiaBlueLog")                       ;Tech
         LogFile = ('/home/LBCeng/FPIAlogs/'+dte+'tecBlueLog')                       ;Tech
     endif else begin
         LogFile = LogFile
     endelse
  Endif Else Begin
     basename = 'lbcrtec'                                   ;Tech
     chan = 'red'
     side = 'right'
     If Not KeyWord_Set(LogFile) Then Begin 
         ;LogFile = ("tecfpiaRedLog")                        ;Tech
         LogFile = ('/home/LBCeng/FPIAlogs/'+dte+'tecRedLog')                       ;Tech
     endif else begin
         LogFile = LogFile
     endelse
  EndElse

; If not specified by caller, take data dir from env var
  If Not KeyWord_Set(DataDir) Then DataDir = GetEnv("LBCFPIA_DATADIR")
; If not specified at all, assume data dir is /Repository
  If DataDir EQ "" Then Begin
     DataDir = "/Repository"
  Endif

; Take home directory (path to executable for TCSSendWavefront) from env var
  HomeDir = GetEnv("LBCFPIA_HOME")
  If HomeDir EQ "" Then HomeDir="."

; Open the logfile
  OpenW, outUnit, logfile, /Get_Lun , /append
;
; Fallback to default if necessary
;
  If KeyWord_Set(Interactive) Then Begin
     FileName = Dialog_PickFile(Path=DataDir,Filter=basename+".*.*.fits")
     If FileName EQ '' Then Begin
        Print, "No file selected, returning"
        Return
     EndIf
  Endif Else If Keyword_Set(File) Then Begin
     FileName = DataDir + Path_Sep() + File
  Endif Else begin
     FileName = GetNewestFile(DataDir,basename,dbg=dbg)
  Endelse

  Print, "Processing: "+FileName
  If KeyWord_Set(dbg) Then Begin
     Print,"Before GetImage ImgSec is: ",ImgSec
  EndIf
  Ima = GetImage(FileName, ImgSec, RA=RA, dbg=dbg, LBCChip=LBCChip)
  If KeyWord_Set(dbg) Then Begin
     Print,"After GetImage ImgSec is: ",ImgSec
     Print, "Rotator Angle = ",RA
  EndIf

  If KeyWord_Set(DoPreProc) Then Begin
     If KeyWord_Set(dbg) Then Print, "Bias subtraction and ff division..."
     Ima = PreProcIma(Ima, ImgSec, LBCChip=LBCChip, dbg=dbg)
  EndIf

  If KeyWord_Set(Seeing) Then Begin
     If KeyWord_Set(dbg) Then Print, "Convolving with seeing FWHM = ",Seeing
     Ima = Convolve(Ima, PSF_Gaussian(NPixel=32, FWHM=Seeing, NDimen=2, /Normalize))
  EndIf

  Print,"Searching for pupils... " ;;; ,Format='(A,$)'
  Flush,-1
  NPups = FindPupils(Ima, Pups=P, PRad=PRad, llbPos=llbPos, $
                     estOR=estOR, dbg=dbg, NoDisplay=NoDisplay, $
                     NoBckSub=NoBckSub, invMoments=invMoments, $
                     blobs=blobs, objs=objs, oldbck=oldbck, $
                     NewExtr=NewExtr, chan=chan)
;;Print,"done."
  Flush,-1

  If NPups LT 1 Then Begin
     Print, "----------------------------------------------------"
     Print, "     No GOOD pupils found... check input image!"
     Print, "----------------------------------------------------"
     Return
  Endif Else Begin
     Print, "Computing aberrations using "+StrCompress(String(NPups),/Remove)+" pupils"
     Flush,-1
  EndElse

;;; help,cen,P

; get the current filter and focus offsets
  hdr = headfits(FileName)
  CurrentFocus = double(sxpar(hdr, 'FOCUSOFF'))
  OffsetFocus =  double(sxpar(hdr, 'FILTEOFF'))
  filter = sxpar(hdr,'FILTER')

; calculate the extrafocal distance
;  CurrentFocus = CurrentFocus - OffsetFocus

  Print, "Focus Offset = ", CurrentFocus, "  Filter = ", filter

  res = CalcAbCoeffs(P, RA, estOR=estOR, dbg=dbg, bPos=llbPos, DoIntFit=DoIntFit, $
                     NoDisplay=NoDisplay,CurrentFocus=CurrentFocus, SphGain=SphGain, $
                     pEdges=pEdges,pFits=pFits,UseFitOnly=UseFitOnly,estSeeing=estSeeing, $
                     Red=Red, /OldPhot)

  ;
  ; This is used for debugging only. It is returned to
  ; caller if rough_ires keyword specified and contains
  ; the computed aberrations without any 'sanitization'
  ; (i.e. limit to z4 or z11)
  ;
  rough_ires = Fix(res)

  if KeyWord_Set(UniqueOut) then begin
     filechunks = strsplit(filename,'/',/extract)
     filechunks = strsplit(filechunks[n_elements(filechunks)-1],'.',/extract)
     OutFile = filechunks[0]+'.'+filechunks[1]+'.'+filechunks[2]+'.coeffs'
  endif else Begin
     OutFile = 'lbciaCoeffs_'+chan+'.dat'
  Endelse 

; Check aberration results for sanity
  z4Limit = 2000
  z4Crazy = 50000

  z11Limit = 1000
  z11Crazy = 3000

  If abs(res[3]) GT z4Limit Then Begin
     Print,"WARNING: Z4 TOO LARGE, will consider defocus only"
     Print,"         all other aberration coefficients set zero."
     z4 = res[3]
     res[*] = 0.
     res[3] = z4 
  EndIf

  If abs(res[3]) GT z4Crazy Then Begin
     Print,"WARNING: Z4 INSANELY LARGE, do not use this correction"
     Print,"         all aberration coefficients set zero."
     z4 = res[3]
     res[*] = 0.
  EndIf

  If abs(res[10]) GT z11Limit Then Begin
     Print,"WARNING: Z11 TOO LARGE"
     If res[10] GT 0. Then Begin
        Print,"         Truncating value to ", z11Limit
        res[10] = z11Limit 
     EndIf Else Begin
        Print,"         Truncating value to ", -z11Limit
        res[10] = -z11Limit
     EndElse
  EndIf

  If abs(res[10]) GT z11Crazy Then Begin
     Print,"WARNING: Z11 INSANELY LARGE, do not use this correction"
     Print,"         all aberration coefficients set zero."
     z11 = res[10]
     res[*] = 0.
  EndIf

  writeOutFile, res, res*0.0, OutFile=OutFile

  If KeyWord_Set(PrintRes) Then Print,Res

  IRes = Fix(Res)
  Print,"------------------------------------------------"
  Print,"      Computed Aberrations (nm)"
  Print,"Defoc   AstX   AstY   ComaX  ComaY   Sph    Est."
  Print," (Z4)   (Z5)   (Z6)   (Z7)   (Z8)   (Z11) seeing"
  Print,"------------------------------------------------"
  Print,ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],estSeeing,Format="(I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,3X,F4.2)"
  Print,"------------------------------------------------"
  ; Print,"Estimated seeing: "+strtrim(estSeeing,2)+" arcsec."
;
;Finds the tilt (pretty confident in RX_0 and RY_0, check RX and RY)
;
;;Empirically determined tilt (arcsec) per delta Pupil diameter from top to bottom (pixels) 
  scale = 42                    ;(atan(100/(13.5*4608))*180/!pi*3600)/8

;;Pupil Centers in image coordinates
  PupCen = llbpos
  PupCen[0,*] = PupCen[0,*] +  PRad + ImgSec[0] 
  PupCen[1,*] = PupCen[1,*] +  PRad + ImgSec[2] 
  if LBCChip eq 1 then begin
     Pupcen[0,*] += 4096
  endif else if LBCCHip eq 2 then begin
     Pupcen[0,*] += 2048
  endif else if LBCChip eq 3 then begin
                                ;leave as is
  endif else if LBCChip eq 4 then begin
     tempx = Pupcen[0,*]
     PupCen[0,*] = PupCen[1,*] 
     PupCen[0,*] = 4608 - PupCen[0,*] + 768
     PupCen[1,*] = tempx + 4608
  endif else message,'Not a valid chip number for LBCChip'

  If NPups ge 3 then begin
     maxx = max(PupCen[0,*],min=minx)
     maxy = max(PupCen[1,*],min=miny)

                                ;Rotator Angle in Radians
     RotAng = RA*!PI/180d
     
     ;;Fits a plane to the the XY coordinates and Average Pupil Diameter
     plane = griddata(PupCen[0,*],PupCen[1,*],PupCen[2,*],$
                      METHOD='PolynomialRegression',power=1)

     ;;finds the tilt in the best-fit plane and calculates RX and RY
     nxel = n_elements(plane[*,0])
     nyel = n_elements(plane[0,*])
     RX_0 = (plane[0,0] - plane[0,nxel-1])*scale*4608/(maxy-miny)
     RY_0 = (plane[nyel-1,0] - plane[0,0])*scale*4608/(maxx-minx)

     If KeyWord_Set(dbg) Then print, 'RX_0 = ',strtrim(rx_0,2),'   RY_0 = ',strtrim(ry_0,2)

     RX =  RX_0*cos(RotAng)+RY_0*sin(RotAng)
     RY = -RX_0*sin(RotAng)+RY_0*cos(RotAng)

     If KeyWord_Set(dbg) Then print, 'RX   = ',strtrim(rx,2),  '   RY   = ',strtrim(ry,2)

  endif else begin
     If KeyWord_Set(dbg) Then print, 'Not enough pupils to calculate the tilt, consider increasing ImgSec'
  EndElse

;
; send corrections to telescope
;
  If Not KeyWord_Set(DontSend) Then Begin
     print, 'Do you want to send (y/n)?'
     response = ''
     read, response
     If response eq 'y' then Begin
        PrgName = HomeDir+Path_Sep()+"TCSSendWavefront"+Path_Sep()+"TCSSendWavefront " + OutFile + " " + side
        Spawn, PrgName
     EndIf
  EndIf Else Begin
     Print, "Will NOT send corrections to telescope, as per user request."
     response = 'n'
  EndElse

; write to a log file   (format adjusted to add seeing by JMH 20080129)
  Printf,outunit,ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],response,estSeeing,filename,$
         Format="(I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,A3,2X,F4.2,2X,A54)"

  free_lun, OutUnit
  Return
End
