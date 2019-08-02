;+
;-
Function read_ref_values, vals, errMsg=errMsg, dbg=dbg, RefFile=RefFile

    If N_Elements(RefFile) eq 0 then RefFile="lbciaRefValues.dat"

    Catch, Error_Status
    If Error_Status NE 0 Then Begin
        Catch, /Cancel
        errMsg=!Error_State.Msg
        Return, !Error_State.Code
    Endif

    OpenR, outUnit, RefFile, /Get_Lun

    ReadF, outUnit, nels
    If nels LT 1 Then Begin
       errMsg = 'read_ref_values: cannot get number of values to read from '+RefFile
       Return, -1
    EndIf

    vals = FltArr(nels)
    For I=0,nels-1 Do Begin
       ReadF, outUnit, val
       vals[i] = val
    EndFor

    Free_Lun, outUnit
Return, 0
End
