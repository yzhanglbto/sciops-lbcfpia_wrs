;
; waits until 1 new file is received
; 
Function Wait4OBFile_Blue, dir=dir, maxiter=maxiter, dbg=dbg

If Not KeyWord_Set(dir) Then dir='/Repository'
If Not KeyWord_Set(maxiter) Then maxiter = 99

get_date, today
today = StrJoin(StrSplit(today,'-',/Extract))

basename = 'lbcb'

Print, "Waiting for file in ", dir+Path_Sep()+basename+"."+today+".??????.fits"
fileList = File_Search(dir+Path_Sep()+basename+"."+today+".??????.fits", Count=NumFilesOld)
FileFound = 0
IterN = 0
Repeat Begin
    Wait, 1
    fileList = File_Search(dir+Path_Sep()+basename+"."+today+".??????.fits", Count=NumFilesNew)
    ; If (NumFilesNew GT NumFilesOld) Then Begin
        ; Print, "Found "+(NumFilesNew-NumFilesOld)+" new file[s]" 
        ;FileFound = 1
        ;NewFileName = fileList[NumFilesNew-1]
    ;Endif
    IterN += 1
    If IterN GE maxIter Then Return, 0
Endrep Until (NumFilesNew-NumFilesOld) GE 1

Return, 1
End
