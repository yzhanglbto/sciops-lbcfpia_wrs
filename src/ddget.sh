#!/bin/bash

# This shell script queries parameters from the LBT Data Dictionary
# Up to 15 parameters can be specified on the command line
# The default list of parameters is generated internally in this script.
# The output parameter values come back on STDOUT (all on one line)
# J. M. Hill, LBT Observatory 20130913
#
# example usage: ddget.sh L_FocalStation r_FocalStation




Newpath="/tmp/"  # path for temporary file
Newscript="ddget"
Newname="params.dat"
PID=$$  # PID of this process to make the name more unique
Newfile=$Newpath$Newscript$PID$Newname # temporary file name

#echo "Creating a list of parameters $Newfile $#"


# write a file of parameters using "here documents"
if [ $# == 0 ]  # check number of arguments
then  # default parameters hardwired
cat > $Newfile <<End-of-message
L_FocalStation
L_Instrument
L_EffectiveMode
L_IIFOnSource
R_FocalStation
R_Instrument
R_EffectiveMode
R_IIFOnSource
IIFAllOnSource
End-of-message
else  # parameters from script arguments (limted to 15 at the moment)
cat > $Newfile <<End-of-message
$1
$2
$3
$4
$5
$6
$7
$8
$9
${10}
${11}
${12}
${13}
${14}
${15}
End-of-message
fi

#echo "Printing the file contents"
#cat $Newfile

#echo "Requesting Parameter from Data Dictionary"

# make the request and filter out the default verbose output
# this puts all the values on one line
IRC GetParameter $Newfile | awk 'BEGIN{ ORS=" " }{ if ( NR > 1 ) {print $3} }'

rm $Newfile  # clean up

#echo "Finished"

exit 0 # exit the script
