Pro anatech, red=red, DataDir=DataDir, rawima=rawima

If Not KeyWord_Set(DataDir) Then DataDir = GetEnv("LBCFPIA_DATADIR")
; If not specified at all, assume data dir is /Repository
If DataDir EQ "" Then Begin
    DataDir = "/Repository"
Endif

If KeyWord_Set(Red) Then filter = "lbcrtec.*.*_2.fits" Else filter = "lbcbtec.*.*_2.fits"

fileList = Dialog_PickFile(Path=DataDir,/Multiple,filter=filter)



LBCFPIA, File=File_BaseName(fileList), /DontWrite, $
         DataDir=DataDir, ires=ires, $
         /Tec, Red=Red, /DontSend, Dbg=Dbg, rawima=rawima

Return
End
