;+
; Writes output file to be sent to TCS for applying corrections.
;
; @param coeffs {in}{required}{type=float} Array of coefficients
; @param coeffVars {in}{required}{type=float} Array of estimated errors
;    on coefficients
; @param errNo {in}{required} Error number
; @param strErr {in}{required}{type=string} Error string
; @keyword dbg {in}{optional}{type=boolean} If set, some debugging
;    messages are printed at IDL log output.
; @keyword OutFile {in}{optional} Optional output filename if not set,
;    default is "lbciaCoeffs.dat"
;
; @author Andrea Baruffolo, INAF - OAPd
;-
Pro WriteOutFile, coeffs, coeffVars, errNo, strErr, dbg=dbg,OutFile=OutFile

    if N_elements(OutFile) eq 0 then OutFile="lbciaCoeffs.dat"

    Catch, Error_Status
    If Error_Status NE 0 Then Begin
        Catch, /Cancel
        Print, "writeOutFile: Error in saving fitted coefficients"
        Print, !Error_State.Msg
        Message, !Error_State.Msg ; Throw exception again to be catched at higher level
    Endif

    Q = Size(coeffs)
    If Q[0] EQ 0 Then Begin
        Msg = "lbciaWriteOutFile: array of coefficients is UNDEFINED!"
        Message, Msg
    Endif
    NumCoeffs = Q[1]

    Q = Size(coeffVars)
    If Q[0] EQ 0 Then Begin
        Msg = "lbciaWriteOutFile: array of coefficient variances is UNDEFINED!"
        Message, Msg
    EndIf
    NumVars = Q[1]

    If NumCoeffs NE NumVars Then Begin
        Msg = "lbciaWriteOutFile: coefficients and variances array size do not match!"
        Message, Msg
    Endif

    If N_Elements( errNo ) EQ 0 Then errNo = 0
    If N_Elements( strErr ) EQ 0 Then strErr = ''

    OpenW, outUnit, OutFile, /Get_Lun

    PrintF, outUnit, errNo, " # Error code"
    PrintF, outUnit, strErr, ' # Error string'
    PrintF, outUnit, NumCoeffs, ' # Number of fitted coefficients'
    For I=0,NumCoeffs-1 Do Begin
        PrintF, outUnit, I, coeffs[I], coeffVars[I], Format='(I3,2X,F10.2,2X,F6.2)'
        Msg = String( I, coeffs[I], coeffVars[I], Format='(I3,2X,F10.2,2X,F6.2)' )
    Endfor

    Free_Lun, outUnit
Return
End
