Function WaitNewFile, dir, basename

get_date, today
today = StrJoin(StrSplit(today,'-',/Extract))

fileList = File_Search(dir+Path_Sep()+basename+"."+today+".??????.fits", Count=NumFilesOld)
FileFound = 0
IterN=0
Ticks = ['|','/','-','\']
; Print, "No new files ", Ticks[0], Format='(A,A1,TL2,$)'
Print, Format='($,%"No new files %s\r")', Ticks[0]
Repeat Begin
    Wait,0.33
    fileList = File_Search(dir+Path_Sep()+basename+"."+today+".??????.fits", Count=NumFilesNew)
    If (NumFilesNew GT NumFilesOld) Then Begin
        Print, "Found "+String(NumFilesNew-NumFilesOld)+" new file[s]"
        FileFound = 1
        NewFileName = fileList[NumFilesNew-1]
        Print, "Found new file: "+NewFileName
    Endif Else Begin
        ; Print, "No new files ",Ticks[IterN+1 MOD 4], Format='(A,A1,TL2,$)'
        Print, Format='($,%"No new files %s\r")', Ticks[(IterN+1) MOD 4]
    Endelse
    IterN += 1
Endrep Until FileFound

Return, NewFileName
End

