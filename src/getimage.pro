;+
; Given the full path name of a FITS file get data corresponding to the
; specified image section.
;
;
; @param FileName {in}{required}{type=string} full pathname of file to be
;    processed. <Strong>File must contain data for CCD#2 ONLY, full
;    frame.</Strong> This may change in the future (will add code to
;    auto-detect data from chip #2, automatically extract useful
;    portion of image).
; @param ImgSec {in}{required}{type=array} 4-elements integer array
;    that specifies the section of the mosaic from which
;    image data has been extracted (IRAF convention, if I'm correct).
; @keyword RA {out}{optional} LBC rotator angle, taken from the FITS
;    header of the input file.
; @keyword LBCChip {in}{optional}{type=integer} Chip number,
;    defaults to 2 (chip #2 in mosaic).
; @keyword Trimmed (in){optional}{type=boolean} If set, then 
;    pre- and over-scan regions have already been trimmed, so
;    first 50 columns in image must NOT be skipped.
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
;
; @returns Array containing portion of image useful for pupil
;    detection.
;
; @history Added IDLDoc documentation on May 11, 2006
; @history Modified to handle FITS files with no extensions, June 11, 2006
; @author Andrea Baruffolo, INAF - OAPd
; @version $Id$
;-
Function GetImage, FileName, ImgSec, RA=RA, LBCChip=LBCChip, Trimmed=Trimmed, $
                   dbg=dbg

If N_Params() LT 1 Then Message, "You must provide me at least a file name!"
If N_Elements(ImgSec) EQ 0 Then ImgSec = [2403,4450,3201,4608]
;
; Read in file, trying to catch the case when file is incomplete
;
NRep = 0
MaxNRep = 10

For NRep=1,MaxNRep Do Begin
;
; Check if file has extensions
;
   ;; Print, "****** TRY: "+ToS(NRep)
   FITS_Open, FileName, fcb
   If fcb.NExtend GT 0 Then begin
      If KeyWord_Set(LBCChip) Then Begin
         If LBCChip GE 1 AND LBCChip LE 4 Then $
            ExtName=String(LBCChip,Format='("LBCCHIP",I1)') $
         Else Message, 'Not a valid chip number for LBCChip'
      EndIf Else ExtName='LBCCHIP2' 
   EndIf Else ExtName=''
;
; Read image data and header
;
   ReadFailure = 0
   FITS_Read, fcb, Ima, ExtName=ExtName, Header, /No_Abort, Message
   If (!Err EQ -1) Then Begin
      ReadFailure = 1
      If StrCmp(!Error_State.NAME, 'IDL_M_FILE_EOF', /Fold_Case) Then Begin
         LogAllWays, 'File probably incomplete: '+FileName
         FITS_Close, fcb
         ;;; Print, 'Waiting ... '+StrCompress(String(NRep),/Remove_All)
         Wait, 1
      EndIf Else Begin
         FITS_Close, fcb
         ErrorLog, 'Error encountered while trying to read file '+FileName
         ErrorLog, !Error_State.Msg
         Message, !Error_State.Msg
         Return, 0
      EndElse
   Endif
   If ReadFailure EQ 0 Then Break
Endfor
If ReadFailure NE 0 Then Begin
   Msg = 'Time out while waiting for file '+FileName
   ErrorLog, Msg
   Message, Msg
EndIf
;
; close file
;
FITS_Close, fcb
;
; Get rotation angle from header
;
RA = SXPAR(Header,"ROTANGLE")

;Print, "IMGSEC =", ImgSec

If STRLEN(ExtName) GT 0 Then begin  ; assume an LBC image
   ; Get DETSEC from header: one-based
   DETstr = SXPAR(Header,"DETSEC")
   ; convert string to an array of integers
   ; STRPUT, DETstr, ',', STRPOS(DETstr,':')
   ; STRPUT, DETstr, ',', STRPOS(DETstr,':')
   ; STRPUT, DETstr, ' ', STRPOS(DETstr,'[')
   ; STRPUT, DETstr, ' ', STRPOS(DETstr,']')
   ; DETstr = STRTRIM(DETstr,2)
   DETSec = FIX ( STRSPLIT(DETstr,'[,:]',/EXTRACT) ) 
   ExtDbgLog, "DETSEC ="+String(DETSec)

   ; [2403:4450,1:4608]
   If DetSec[0] GT 2403 OR DetSec[1] LT 4450 OR DetSec[2] GT 1 OR DetSec[3] LT 4608 Then Begin
      ExtDbgLog, "Looks like image is windowed... using DETSec from image header"
      ImgSec = DETSec
   EndIf


   IDLSec = DETSec

   ; If image is already trimmed in X, we don't need to add an offset
   If KeyWord_Set(Trimmed) Then XOffset = 0 Else XOffset = 50
   
   IDLSec[0] = ImgSec[0] - DETSec[0] + XOffset
   IDLSec[1] = ImgSec[1] - DETSec[0] + XOffset
   IDLSec[2] = ImgSec[2] - DETSec[2]
   IDLSec[3] = ImgSec[3] - DETSec[2]

   ; Print,"IDLSEC =",IDLSec

endif else IDLSec = ImgSec ;use specified image section directly: zero-based
;
; Extract portion of image useful for pupils detection
;
Return, Ima[IDLSec[0]:IDLSec[1],IDLSec[2]:IDLSec[3]]

End
