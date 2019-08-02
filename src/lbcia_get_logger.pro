Function LBCIA_get_logger

  DefSysV, '!lbciaLog', Exists=Exists
  If Exists NE 1 Then Begin
    DefSysV, '!lbciaLog', Obj_New('lbciaLogger')
  EndIf Else Begin
    If Not Obj_Valid(!lbciaLog) Then !lbciaLog = Obj_New('lbciaLogger')
  EndElse

Return, !lbciaLog
End
