;
; Close all windows with index greater than 32, i.e. all those created
; using /free param to WINDOW call.
;
Pro PurgeWins

   Catch, Error_Status
   If Error_Status EQ 0 Then Begin
       Device, Window_State=Cur_Wins
       Cur_Wins = Cur_Wins[32:*]
       Q = Where(Cur_Wins EQ 1)
       If Size(Q,/N_Dimensions) NE 1 Then Return
       Q += 32
       For I=0,Size(Q,/N_Elements)-1 Do WDelete,Q[I]
   Endif
   Catch,/Cancel

Return
End
