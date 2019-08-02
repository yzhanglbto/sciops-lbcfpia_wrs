;+
; Given the name of a directory where LBC data is stored, returns the
; name of the "most recent" file. 
; File names are assumed to follow the pattern:
;   lbc[br].YYYYMMDD.HHMMSS.fits
; Thus the "most recent" (or "newest") file is the one whose name
; ranks as last in a lexicographically ordered (1) list of all file
; names. <BR>
; (1) Paul E. Black, <A
; HRef="http://www.nist.gov/dads/HTML/lexicgrphcl.html" target=new>Lexicographical Order</A>.
; 
; @param dir {in}{required}{type=string} Full path to the
;    directory containing LBC data
;
; @param basename {in}{required}{type=string} Basename of file
;    to look for. Should be 'lbcb' or 'lbcr'.
;
; @keyword today {in}{optional}{type=string} For testing and
;    debugging. Instead of computing the current date in constructing
;    the file name matching pattern use the supplied string.
;
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
;
; @returns A string containing the name of the most recently image
;    acquired with LBC.
;
; @history Added IDLDoc documentation on May 16, 2006
; @history Added parameter 'basename' Nov 21, 2007, ABa
; @history Added delay for minimum file size Apr 17, 2009 JMH
; @author Andrea Baruffolo, INAF - OAPd
;-
Function GetNewestFile, dir, basename, today=today, dbg=dbg

If N_Params() Ne 2 Then $ 
   Message, "You must specify both search directory and basename"

If Not KeyWord_Set(today) Then Begin
   get_date, today
   today = StrJoin(StrSplit(today,'-',/Extract))
EndIf

If KeyWord_Set(dbg) Then Begin
   Print, "today = "+today
EndIf

fileList = File_Search(dir+Path_Sep()+basename+"."+today+".*.fits", Count=NumFiles, /NoSort) ;; we'll do our own sorting

If NumFiles EQ 0 Then Begin
    Print, "No Files Found!! in ", dir, " for ", today
    Return,""
Endif

fileIdx = (Sort(fileList))[NumFiles-1]

; added check of file size to be sure file is finished writing - JMH 20090417

;
; Check tentatively removed by AB. Relying of file size is not robust
; against changes in readout window size. Added check in getimage.pro
;

;newfile = fileList[fileIdx]
;IterN = 0

;Repeat Begin
;    If IterN GT 0 Then Wait, 1
;    inforesult = File_Info(newfile)
    ;Print, inforesult.size
;    IterN += 1
;    If IterN GE 10 Then Return, newfile
; Endrep Until inforesult.size GE 6508800

Return, fileList[fileIdx]
End
