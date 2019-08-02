;+
; Pre-process image data by subtracting bias and dividing by FF.
;
; @param Data {in}{required}{type=array} two-dimensional array
;    containing image data to be pre-processed.
; @param ImgSec {in}{required}{type=array} 4-elements integer array
;    that specifies the section of the mosaic from which
;    image data has been extracted (IRAF convention, if I'm correct).
; @keyword LBCChip {in}{optional}{type=integer} Chip number,
;    defaults to 2 (chip #2 in mosaic).
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
;
; @author Andrea Baruffolo, INAF - OAPd
; @version $Id$
;-
Function PreProcIma, Data, ImgSec, LBCChip=LBCChip, $
                     dbg=dbg

;
; Directory where calibration data (bias, FF) reside
;
CalDataDir = GetEnv("LBCFPIA_CALDATADIR")
If CalDataDir EQ "" Then CalDataDir = '..'+Path_Sep()+'data'

Bias = GetImage(CalDataDir+Path_Sep()+"masterbias.20060504.fits",$
                ImgSec, LBCChip=LBCChip, /Trimmed, dbg=dbg)

Print, "Bias level: ", Moment(Bias)

Flat = GetImage(CalDataDir+Path_Sep()+"masterflat.B_390.20060504.fits",$
                ImgSec, LBCChip=LBCChip, /Trimmed, dbg=dbg)



Q = Where(Flat GT 0, Complement=NQ)
Data[Q] = (Data[Q]-Bias[Q])/Flat[Q]
If (Size(NQ))[0] NE 0 Then Data[NQ] = 0

Return, Data
End
