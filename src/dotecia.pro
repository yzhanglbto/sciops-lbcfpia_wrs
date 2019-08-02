;+
; DOTECIA - Perform Image Analysis on LBC technical chips
;
; This procedure is assumed to be invoked by the LBC observing
; software, at the end of a science exposure.
; It builds a list of files containing images acquired with
; tech chip during the exposure and passes this list to
; lbcfpia for analysis.
; Results of analysis are then written to a file and send to
; LBT TCS by LBC control software.
;
; doRef: !=0 means measured parameters shall be taken as reference
; channel is 'r' or 'b'
; expTime in seconds
;-
Pro DoTecIA, doRef, channel, expTime, $
             DataDir=DataDir, $
             today=today, cur_time=cur_time, $
             Dbg=Dbg, fileList=fileList
SetLogLevel, 4
;
;
;
InfoLog, "-----------------------------------------------------------------"
InfoLog, "----------------------- DoTecIA: starting -----------------------"
;
; set data directory
;
If Not KeyWord_Set(DataDir) Then DataDir = 'D:\'
;
; Read configuration parameters from file
;
InfoLog, "Reading cfg"
cfg = lbcfpia_read_cfg(ErrCode=ErrCode,ErrMsg=ErrMsg,/Tec)
If ErrCode NE 0 Then Begin
   ErrorLog, ErrMsg
   Message, ErrMsg
EndIf
;
;
;
InfoLog, "Channel: "+strtrim(channel, 2)
Case channel OF
   'r': Begin
      RefFileName = "lbciaRefValuesRed.dat"
      chan = 'red'
      TecZ4RefCrazy = cfg.RedTecZ4RefCrazy
      TecZ4Crazy = cfg.RedTecZ4Crazy
      Red=1
      End
   'b': Begin
      RefFileName = "lbciaRefValuesBlue.dat"
      chan = 'blue'
      TecZ4RefCrazy = cfg.BlueTecZ4RefCrazy
      TecZ4Crazy = cfg.BlueTecZ4Crazy
      Red=0
      End
   Else: Begin
      ErrorLog, 'DoTecIA: channel is neither red nor blue'
      Return
      End
Endcase
;
; Place an error handler here, so that if any .pro fails
; do not send any correction
;
Catch, Error_Status
If Error_Status NE 0 Then Begin
   ErrorLog, 'DoTecIA: some unhandled error '
   ErrorLog, 'DoTecIA: error message "'+!Error_State.Msg+'"'
   aberr = FltArr(22)
   writeOutFile, aberr, abs(aberr*0.0), 99, 'Internal Error', OutFile='lbciaCoeffs_'+chan+'.dat'
   Return
EndIf

;
; Check if reference file is there
;
FInfo = File_Info(RefFileName)
If doRef EQ 0 Then Begin
   If FInfo.Exists EQ 0 Then Begin
      ErrorLog, "doRef is 0 but reference file not found, setting doRef to 1"
      doRef = 1
   EndIf
Endif Else Begin
   Catch, Error_Status
   If Error_Status NE 0 Then Begin
      Catch, /Cancel
      ErrorLog, !Error_State.Msg
      Return
   EndIf
   If FInfo.Exists EQ 1 Then File_Delete, RefFileName
EndElse
;
; Build list of files to be processed
;
If Not KeyWord_Set(fileList) Then Begin
   numFiles = Get_Last_Tech_Files(channel, expTime, fileList, $
                                  DataDir=DataDir, today=today, cur_time=cur_time, dbg=dbg, errMsg=errMsg)

   If NumFiles GT 0 Then Begin
      DebugLog, 'Num Files Found: '+ToS(NumFiles)
      DebugLog, String(fileList)
   Endif Else Begin
      DebugLog, 'Get_Last_Tech_Files returned '+ToS(numFiles)
      DebugLog, 'errMsg = '+errMsg
      Return
   Endelse
Endif Else Begin
   fileList = File_BaseName(fileList)
   DebugLog, String(fileList)
EndElse


;
; Perform actual IA and do not write results to file
;
InfoLog, "Now calling LBCFPIA"
LBCFPIA, File=fileList, /DontWrite, $
         DataDir=DataDir, ires=ires, $
         /Tec, Red=Red, /DontSend, cfg=cfg, Dbg=Dbg

If doRef NE 0 Then Begin
    ;
    ; We're supposed to produce a reference file
    ; in any case write zero to all coefficients (since LBC
    ; SW sends them to TCS)
    ;
   aberr = FltArr(22)
   writeOutFile, aberr, abs(aberr*0.0), 0, 'Computing ref aberrations, no corrections required', $
                 OutFile='lbciaCoeffs_'+chan+'.dat'
    If N_Elements(ires) GT 0 Then Begin
       If Abs(ires[3]) LT TecZ4RefCrazy Then Begin
          InfoLog, "Writing reference values: "+StrJoin(StrTrim(ires,2), ' ')
          res = write_ref_values(ires, OutFile=RefFileName, errMsg=errMsg)
          If res NE 0 Then ErrorLog, errMsg
       Endif Else Begin
          WarningLog, "Analysis of reference image yield too large Z4 = "+String(ires[3], Format='(F0)')
          WarningLog, "Limit (TecZ4RefCrazy) is "+String(TecZ4RefCrazy, Format='(F0)')
       EndElse
    Endif Else Begin
       WarningLog, "Pupil analysis failed, will NOT write reference file"
    Endelse
Endif Else Begin
    If N_Elements(ires) GT 0 Then Begin
       InfoLog, "Reading reference values "
        res = read_ref_values(ref, RefFile=RefFileName, errMsg=errMsg)
        If res NE 0 Then Message, errMsg
        InfoLog, "Reference values: "+StrJoin(strtrim(ref,2), ' ')

        LogAllWays, "ref[3] = "+strtrim(ref[3],2)
        LogAllWays, "ires[3] = "+strtrim(ires[3],2)

        defoc = ires[3]-ref[3]
        ;comax = ires[6]-ref[6]
        ;comax = ires[7]-ref[7]

        LogAllWays, "defoc = "+StrJoin(strtrim(defoc,2), ' ')

        aberr = ires*0.0

        If Abs(defoc) LT TecZ4Crazy Then aberr[3] = defoc Else LogAllWays, 'Z4 too large = ' + $
           StrTrim(defoc, 2)
        ; If Abs(comax) GT cfg.MinTecComaX Then aberr[6] = comax
        ; If Abs(comay) GT cfg.MinTecComaY Then aberr[7] = comay
        InfoLog, "Aberrs: "+StrJoin(strtrim(aberr,2), ' ')
        writeOutFile, aberr, abs(aberr*0.0), OutFile='lbciaCoeffs_'+chan+'.dat'
    Endif Else Begin
        aberr = FltArr(22)
        writeOutFile, aberr, abs(aberr*0.0), 99, 'NO good pupils found', OutFile='lbciaCoeffs_'+chan+'.dat'
    EndElse
EndElse
InfoLog, "----------------------- DoTecIA: ending -------------------------"
InfoLog, "-----------------------------------------------------------------"

Return
End
