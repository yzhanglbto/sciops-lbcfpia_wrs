;+
; GET_LAST_TECH_FILES - Build list of image files acquired during last
;                       science exposure.
;
; Build list of files, containing data from LBC tech chips, that have
; been acquired during the last science exposure.
;
; cur_time parameter for testing
;-
Function Get_Last_Tech_Files, channel, expTime, outFileList, $
                              DataDir=DataDir, today=today, cur_time=cur_time, $
                              errMsg=errMsg, dbg=dbg

If Not KeyWord_Set(DataDir) Then DataDir = '/Repository'

If Not KeyWord_Set(cur_time) Then Begin
   cur_time = Long(StrJoin(StrSplit((StrSplit(SysTime(/UTC),/Extract))[3],':',/Extract)))
   If KeyWord_Set(dbg) Then Print, "cur_time = ", cur_time
EndIf

If Not KeyWord_Set(today) Then Begin
   get_date, today
   today = StrJoin(StrSplit(today,'-',/Extract))
EndIf

If (channel EQ 'b') OR (channel EQ 'r') Then $
   basename = 'lbc'+channel+'tec' $
Else Begin
   errMsg = 'Get_Last_Tech_Files: Wrong channel specification '+String(channel)
    If KeyWord_Set(dbg) Then Print, errMsg
   Return, -1
EndElse

fileList = File_Search(DataDir+Path_Sep()+basename+"."+today+".*_2.fits", Count=NumFiles, /NoSort)

If NumFiles EQ 0 Then Begin
    errMsg = 'No files found in '+String(DataDir)+' for date '+String(today)+', exp. time '+String(expTime)
    If KeyWord_Set(dbg) Then Print, errMsg
    Return, -1
Endif

NumMatches  = 0
MaxMatches  = 100
outFileList = StrArr(MaxMatches)

start_time = Clock_To_Sec(cur_time)-expTime
If (start_time LT 0) Then start_time += 86400
start_time = Sec_To_Clock(start_time)


For I=NumFiles-1,0,-1 Do Begin
  ; extract time
  ith_file_time = Long(StrMid((StrSplit(fileList[I],'.',/Extract))[2],0,6))
  If ith_file_time GT start_time Then Begin
    outFileList[NumMatches] = fileList[I]
    NumMatches += 1
    If NumMatches GE MaxMatches Then Break
  EndIf
EndFor

outFileList = File_BaseName(outFileList[0:NumMatches-1])

Return, NumMatches
End
