Pro Read_cfg, FileName, $
                   ValueProc=ValueProc, SectionProc=SectionProc, $
                   ErrCode=ErrCode, ErrMsg=ErrMsg, Dbg=Dbg

MaxErrors = 10
NumErrors =  0
ErrCode = 0
ErrMsg  = StrArr(MaxErrors)

On_IOError, IOErr

OpenR, in, FileName, /Get_Lun

LineN = 0
iLine = ""
RegExpValue = '(^[^ '+String(9B)+']+)[ '+String(9B)+']*=[ '+String(9B)+']*(.+)'
RegExpSection = '^\[([^ '+String(9B)+']+)\]'
While Not EOF(in) Do Begin
  ReadF, in, iLine
  LineN += 1
  ; Remove leading and trailing blanks
  iLine = StrTrim(iLine, 2)
  ;;; Print, iLine
  FirstChar = StrMid(iLine,0,1)
  If (FirstChar EQ '#') OR (FirstChar EQ ';') Then Begin
    If KeyWord_Set(dbg) Then Print, "REM" + StrMid(iLine,1)
  EndIf Else Begin
    Case 1 Of 
      (StRegEx(iLine, RegExpValue, /Boolean)): Begin
          ; Print, "Match found"
          Res = StRegEx(iLine, RegExpValue, /Extract, /SubExpr)
          If KeyWord_Set(dbg) Then Print, "Name = "+Res[1]+", Value = "+Res[2]
          If KeyWord_Set(ValueProc) Then Call_Procedure, ValueProc, Res[1], Res[2], $
                                                         ErrCode=MyErrCode, ErrMsg=MyErrMsg
        End
      (StRegEx(iLine, RegExpSection, /Boolean)): Begin
          ; Print, "Match found"
          Res = StRegEx(iLine, RegExpSection, /Extract, /SubExpr)
          If KeyWord_Set(dbg) Then Print, "New Section "+Res[1]
          If KeyWord_Set(SectionProc) Then Call_Procedure, SectionProc, Res[1], $
                                                         ErrCode=MyErrCode, ErrMsg=MyErrMsg
        End
      Else: Begin
        If KeyWord_Set(dbg) Then Print, "Incorrect line: "
        If KeyWord_Set(dbg) Then Print, iLine
      End
    EndCase
    If MyErrCode NE 0 Then Begin
      ErrCode = 1
      ErrMsg[NumErrors] = 'Error '+ToS(MyErrCode)+' ('+MyErrMsg+') at line '+ToS(LineN)
      NumErrors += 1
      If NumErrors EQ MaxErrors Then Return
    EndIf
  EndElse
EndWhile

Free_Lun, in

If NumErrors GT 0 Then ErrMsg = ErrMsg[0:NumErrors-1] Else ErrMsg = ""

Return

IOErr:
ErrCode = !Error_State.Code
ErrMsg  = !Error_State.Msg
If N_Elements(in) GT 0 Then Free_Lun, in
;
; Error reading configuration file
; Configuration parameters not changed
; Calling application in charge to decide what to do (e.g. stop execution or proceed)
;
Return
End
