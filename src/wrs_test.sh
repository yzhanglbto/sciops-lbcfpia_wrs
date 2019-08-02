#!/bin/sh 
idl <<!here
@make_wrs
.r test_wrs
.r ztools
test_wrs
!here


