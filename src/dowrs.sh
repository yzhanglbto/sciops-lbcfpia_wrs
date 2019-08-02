#!/bin/sh 
export LBCFPIA_HOME="/home/lbceng/WRS"
export WRS_HOME="/home/lbceng/WRS"
export WRS_CFG_HOME="/home/lbceng/WRS/src"
export IDL_PATH="$LBCFPIA_HOME/src:$LBCFPIA_HOME/lib/mpfit:$LBCFPIA_HOME/lib/astron/pro:<IDL_DEFAULT>"
export LBCFPIA_DATADIR="/newdata"
export WRS_DATADIR="/newdata"
idl <<!here
@make_wrs
.r dowrs
dowrs
!here
