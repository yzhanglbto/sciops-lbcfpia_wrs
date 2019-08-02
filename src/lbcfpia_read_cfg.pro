Function lbcfpia_read_cfg, FileName=FileName, dbg=dbg, ErrCode=ErrCode, ErrMsg=ErrMsg, Tec=Tec, WRSMode=WRSMode
  Common lbcfpia_cfg_common, lbcfpia_cfg 

  lbcfpia_cfg_defaults = {LBCFPIA_CFG, $
    RedLimit:           400., $
    BlueLimit:          400., $
    FirstRedInitial:   1100., $
    RedInitial:         500., $
    FirstBlueInitial:  1200., $
    BlueInitial:        600., $
    DOFPIAMaxIter:       99S, $
    RedFinalZ11Zero:    -75., $
    RedFinalZ11Scale:  -350., $
    BlueFinalZ11Zero:  -300., $
    BlueFinalZ11Scale: -350., $
    SphGain:             1.0, $
    PupilRadius:        33.0, $
    MinPupRadius:       10.0, $
    MaxCandidates:        0L, $
    BackGndNSigma:       1.0, $
    ImgSec:             [2403,4450,3201,4608], $
    LBCChip:              2S,$
    z4Limit:           6000., $
    z4Crazy:          60000., $
    z11Limit:          1000., $
    z11Crazy:          3000., $
    z22Lower:            50., $
    z22Upper:           150., $
    MinBlobSize:         10S, $
    MaxBlobSize:        200S, $
    SeeingZero:       -1.564, $
    SeeingSlope:       0.953, $
    AstigScale:         220., $
    ComaScale:          200., $
    SphScale:           220., $
    SphZero:            10.6, $
    SphSlope:           55.0, $
    SphOffset:          88.0, $
    SphRedCorrection:  225.0, $
    FocusAlphaRed:      78.0, $
    FocusAlphaBlue:     78.0, $
    FocOffsTecRed:   14855.9, $
    FocOffsTecBlue:  16281.0, $
    FocusScale:      26385.2, $
    FocusGain:           1.0, $
    Z4Z11Factor:        -1.9, $
    BlueZ22Zero:         1.9, $
    BlueZ22Slope:      300.0, $
    BlueZ22Gain:         0.7, $
    RedZ22Zero:          2.55, $
    RedZ22Slope:       230.0, $
    RedZ22Gain:          0.7, $
    BlueZIMatrix:    Float([[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]), $
    RedZIMatrix:     Float([[1.0,0.0,0.0],[0.0,1.0,0.0],[0.0,0.0,1.0]]), $
    BadSeeingLimit:      2.0, $
    LargeHoleLimit:     0.25, $
    SmallHoleLimit:      1.5, $
    SmallHoleSph:      500.0, $
    MinTecDefoc:       400.0, $
    MinTecComaX:       400.0, $
    MinTecComaY:       400.0, $
    RedTecZ4Crazy:     800.0, $
    RedTecZ4RefCrazy: 2500.0, $
    BlueTecZ4Crazy:    800.0, $
    BlueTecZ4RefCrazy: 2500.0 }

  lbcfpia_cfg = lbcfpia_cfg_defaults

  ps     = Path_sep()
  If Not KeyWord_Set(WRSMode) Then Begin
     HomeDir = GetEnv("LBCFPIA_HOME")
     defcfg = 'lbcfpia.cfg'
  Endif Else Begin
     HomeDir = GetEnv("WRS_HOME")
     defcfg = 'lbcwrs.cfg'
  EndElse

  If KeyWord_Set(Tec) Then defcfg = 'lbcfpia_tec.cfg'
  ;
  ; Cfg file search order:
  ;   1. FileName parameter (CandidateFiles[0])
  ;   2. lbcfpia.cfg or lbcwrs.cfg in current directory (CandidateFiles[1])
  ;   3. $LBCFPIA_HOME/src/lbcfpia.cfg or $WRS_HOME/src/lbcwrs.cfg (CandidateFiles[2])
  ;
  If KeyWord_Set(FileName) Then CandidateFiles = [FileName,'',''] $
     Else CandidateFiles = ['', '', '']
  CandidateFiles[1] = '.'+ps+defcfg

  If HomeDir NE "" Then CandidateFiles[2] = HomeDir+ps+'src'+ps+defcfg

  For I=0,2 Do Begin
     candidate = CandidateFiles[I]
     If candidate NE '' Then Begin
        FInfo = File_Info(candidate)
        If (FInfo.Exists NE 0) And (FInfo.Read NE 0) Then Begin
           FileName = candidate
           Print, strtrim(I)+"cfg file = "+FileName
           Break
        EndIf
     EndIf
  EndFor
  ErrCode = 0
  
  Read_cfg, FileName, $
           ValueProc='lbcfpia_cfg_value', SectionProc='lbcfpia_cfg_section', $
           ErrCode=ErrCode, ErrMsg=ErrMsg, dbg=dbg

  If KeyWord_Set(dbg) Then Print, 'lbcfpia_read_cfg, errcode is '+ToS(errcode)
  If ErrCode NE 0 Then Begin
    lbcfpia_cfg = lbcfpia_cfg_defaults
    Print, ErrMsg
    ErrorLog, 'lbcfpia_read_cfg, cannot read configuration file "'+FileName+'", because:' 
    ErrorLog, ErrMsg
  EndIf 

Return, lbcfpia_cfg
End
