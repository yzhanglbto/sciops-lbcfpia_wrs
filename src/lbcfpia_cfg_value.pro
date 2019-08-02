Pro lbcfpia_cfg_value, name, value, ErrCode=ErrCode, ErrMsg=ErrMsg
  Common lbcfpia_cfg_common, lbcfpia_cfg
  
  ErrCode = 0
  Catch, Err_Status
  If Err_Status NE 0 Then Begin
    Catch, /Cancel
    ErrCode = Err_Status
    ErrMsg  = !Error_State.Msg 
    Return
  EndIf

  On_IOError, bad_io
   
  ;;; Print, 'lbcfpia_cfg_value: '+name+' = '+value
  
  Name = StrUpCase(Name)
  
  Case Name Of
    'REDLIMIT':          lbcfpia_cfg.RedLimit          = Float(value)
    'BLUELIMIT':         lbcfpia_cfg.BlueLimit         = Float(value)
    'FIRSTREDINITIAL':   lbcfpia_cfg.FirstRedInitial   = Float(value)
    'REDINITIAL':        lbcfpia_cfg.RedInitial        = Float(value)
    'FIRSTBLUEINITIAL':  lbcfpia_cfg.FirstBlueInitial  = Float(value)
    'BLUEINITIAL':       lbcfpia_cfg.BlueInitial       = Float(value)
    'DOFPIAMAXITER':     lbcfpia_cfg.DOFPIAMaxIter     = Fix(value)
    'REDFINALZ11ZERO':   lbcfpia_cfg.RedFinalZ11Zero   = Float(value)
    'REDFINALZ11SCALE':  lbcfpia_cfg.RedFinalZ11Scale  = Float(value)
    'BLUEFINALZ11ZERO':  lbcfpia_cfg.BlueFinalZ11Zero  = Float(value)
    'BLUEFINALZ11SCALE': lbcfpia_cfg.BlueFinalZ11Scale = Float(value)
    'SPHGAIN':           lbcfpia_cfg.SphGain           = Float(value)
    'AVGPUPRADIUS':      lbcfpia_cfg.PupilRadius       = Float(value)
    'MINPUPRADIUS':      lbcfpia_cfg.MinPupRadius      = Float(value)
    'MAXCANDIDATES':     lbcfpia_cfg.MaxCandidates     = Fix(value)
    'BACKGNDNSIGMA':     lbcfpia_cfg.BackGndNSigma     = Float(value)
    'LBCCHIP':           lbcfpia_cfg.LBCChip           = Fix(value)
    'Z4LIMIT':           lbcfpia_cfg.z4Limit           = Float(value)
    'Z4CRAZY':           lbcfpia_cfg.z4Crazy           = Float(value)
    'Z11LIMIT':          lbcfpia_cfg.z11Limit          = Float(value)
    'Z11CRAZY':          lbcfpia_cfg.z11Crazy          = Float(value)
    'Z22LOWER':          lbcfpia_cfg.z22Lower          = Float(value)
    'Z22UPPER':          lbcfpia_cfg.z22Upper          = Float(value)
    'MINBLOBSIZE':       lbcfpia_cfg.MinBlobSize       = Fix(value)
    'MAXBLOBSIZE':       lbcfpia_cfg.MaxBlobSize       = Fix(value)
    'SEEINGZERO':        lbcfpia_cfg.SeeingZero        = Float(value)
    'SEEINGSLOPE':       lbcfpia_cfg.SeeingSlope       = Float(value)
    'ASTIGSCALE':        lbcfpia_cfg.AstigScale        = Float(value)
    'COMASCALE':         lbcfpia_cfg.ComaScale         = Float(value)
    'SPHSCALE':          lbcfpia_cfg.SphScale          = Float(value)
    'SPHZERO':           lbcfpia_cfg.SphZero           = Float(value)
    'SPHSLOPE':          lbcfpia_cfg.SphSlope          = Float(value)
    'SPHOFFSET':         lbcfpia_cfg.SphOffset         = Float(value)
    'SPHREDCORRECTION':  lbcfpia_cfg.SphRedCorrection  = Float(value)
    'FOCUSALPHARED':     lbcfpia_cfg.FocusAlphaRed     = Float(value)
    'FOCUSALPHABLUE':    lbcfpia_cfg.FocusAlphaBlue    = Float(value)
    'FOCOFFSTECRED':     lbcfpia_cfg.FocOffsTecRed     = Float(value)
    'FOCOFFSTECBLUE':    lbcfpia_cfg.FocOffsTecBlue    = Float(value)
    'FOCUSSCALE':        lbcfpia_cfg.FocusScale        = Float(value)
    'FOCUSGAIN':         lbcfpia_cfg.FocusGain         = Float(value)
    'Z4Z11FACTOR':       lbcfpia_cfg.Z4Z11Factor       = Float(value)
    'BLUEZ22ZERO':       lbcfpia_cfg.BlueZ22Zero       = Float(value)
    'BLUEZ22SLOPE':      lbcfpia_cfg.BlueZ22Slope      = Float(value)
    'BLUEZ22GAIN':       lbcfpia_cfg.BlueZ22Gain       = Float(value)
    'REDZ22ZERO':        lbcfpia_cfg.RedZ22Zero        = Float(value)
    'REDZ22SLOPE':       lbcfpia_cfg.RedZ22Slope       = Float(value)
    'REDZ22GAIN':        lbcfpia_cfg.RedZ22Gain        = Float(value)
    'BADSEEINGLIMIT':    lbcfpia_cfg.BadSeeingLimit    = Float(value)
    'LARGEHOLELIMIT':    lbcfpia_cfg.LargeHoleLimit    = Float(value)
    'SMALLHOLELIMIT':    lbcfpia_cfg.SmallHoleLimit    = Float(value)
    'SMALLHOLESPH':      lbcfpia_cfg.SmallHoleSph      = Float(value)
    'MINTECDEFOC':       lbcfpia_cfg.MinTecDefoc       = Float(value)
    'MINTECCOMAX':       lbcfpia_cfg.MinTecComaX       = Float(value)
    'MINTECCOMAY':       lbcfpia_cfg.MinTecComaY       = Float(value)
    'REDTECZ4CRAZY':     lbcfpia_cfg.RedTecZ4Crazy     = Float(value)
    'REDTECZ4REFCRAZY':  lbcfpia_cfg.RedTecZ4RefCrazy  = Float(value)
    'BLUETECZ4CRAZY':    lbcfpia_cfg.RedTecZ4Crazy     = Float(value)
    'BLUETECZ4REFCRAZY': lbcfpia_cfg.RedTecZ4RefCrazy  = Float(value)
    'IMGSEC':  Begin
       Res = Execute('lbcfpia_cfg.ImgSec = '+value,1,1)
       If Res EQ 0 Then GoTo, bad_io
       lbcfpia_cfg.ImgSec = Fix(lbcfpia_cfg.ImgSec)
    End
    'BLUEZIMATRIX': Begin
       Res = Execute('lbcfpia_cfg.BlueZIMatrix = '+value,1,1)
       If Res EQ 0 Then GoTo, bad_io
       lbcfpia_cfg.BlueZIMatrix = Float(lbcfpia_cfg.BlueZIMatrix)
    End
    'REDZIMATRIX': Begin
       Res = Execute('lbcfpia_cfg.RedZIMatrix = '+value,1,1)
       If Res EQ 0 Then GoTo, bad_io
       lbcfpia_cfg.RedZIMatrix = Float(lbcfpia_cfg.RedZIMatrix)
    End
    Else: Break
  EndCase

Return
bad_io:
ErrCode = !Error_State.Code
ErrMsg  = !Error_State.Msg 
Return
End
