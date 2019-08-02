Pro SendSph3, sph3, Red=Red, Sim=Sim, Interactive=Interactive, Logfile=Logfile, WRSMode=WRSMode

  get_date,dte
  print,dte

  If Not KeyWord_Set(Red) Then Begin
     chan = 'blue'
     side = 'left'
  Endif Else Begin
     chan = 'red'
     side = 'right'
  EndElse

  If Not KeyWord_Set(WRSMode) Then Begin
     logfile = '/home/LBCeng/FPIAlogs/'+chan+dte+'.Log'
     ; Take home directory (path to executable for TCSSendWavefront) from env var
     HomeDir = GetEnv("LBCFPIA_HOME")
     Print, 'SendSph3: LBCFPIA mode homedir is '+HomeDir
  Endif Else Begin
     logfile = '/home/LBCeng/WRSlogs/'+chan+dte+'.Log'
     HomeDir = GetEnv("WRS_HOME")
     Print, 'SendSph3: WRS mode homedir is '+HomeDir
  EndElse
  OutFile = 'lbcia_Sph3_'+chan+'.dat'

  res = FltArr(22)

; Z11 
  res[10] = Float(Sph3)

; Z4 compensation for Z11
;  res[3] = 0.9 * res[10] ; wrong value from 20090320
;  res[3] = -1.9 * res[10] ; 20090324
;  res[3] = -1.4 * res[10] ; 20110301 (changed from -1.0)
   res[3] = -1.75 * res[10] 
; 20110307 (changed from -1.4 to -1.75, to match Blue M12 matrix element)

  ires = Fix(res)

  writeOutFile, res, res*0.0, OutFile=OutFile

  If KeyWord_Set(Interactive) Then Begin
    print, 'Do you REALLY want to send '+String(Sph3,Format='(F7.2)')+$
      'nm sph3 (y/n)?'
    response = ''
    read, response
  EndIf Else Begin
    response='y'
  EndElse
  If response eq 'y' then Begin
      If HomeDir EQ "" Then HomeDir="."
      PrgName = HomeDir+Path_Sep()+"TCSSendWavefront"+Path_Sep() $
        +"TCSSendWavefront " + OutFile + " " + side
      If Not KeyWord_Set(Sim) Then Spawn, PrgName Else $
        Print, 'SIM: '+PrgName
  EndIf

; Open the logfile
; check whether logfile already exists, if not open & change permissions to
; insure that all potential FPIA users can write to it
  If (file_test(logfile) EQ 0) Then Begin
        ; Open the logfile
        OpenW, outUnit, logfile, /Get_Lun , /append
       file_chmod,logfile,/a_write,/a_read
  EndIf else begin
        OpenW, outUnit, logfile, /Get_Lun , /append
  EndElse
;
; write to a log file   (format adjusted to add seeing by JMH 20080129)
  Printf,outunit,ires[3],ires[4],ires[5],ires[6],ires[7],ires[10],ires[21],response,0.0,0.0,"dofpia_sph3_adjustment             ",$
         Format="(I6,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,I5,2X,A3,2X,F4.2,2X,F4.2,2X,A54)"

  free_lun, OutUnit

Return
End
