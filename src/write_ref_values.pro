;+
;-
Function write_ref_values, vals, errMsg=errMsg, dbg=dbg,OutFile=OutFile

    If N_Elements(vals) EQ 0 Then Begin
       errMsg = 'write_ref_values: Array of reference values to store is empty!'
       Return, -1
    EndIf

    If N_Elements(OutFile) eq 0 then OutFile="lbciaRefValues.dat"

    Catch, Error_Status
    If Error_Status NE 0 Then Begin
        Catch, /Cancel
        errMsg=!Error_State.Msg
        Return, !Error_State.Code
    Endif

    OpenW, outUnit, OutFile, /Get_Lun

    nels = N_Elements(vals)
    PrintF, outUnit, nels

    For I=0,nels-1 Do Begin
       PrintF, outUnit, vals[i], Format='(F10.2)'
    EndFor

    Free_Lun, outUnit
Return, 0
End
