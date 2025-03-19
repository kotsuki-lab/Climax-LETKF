#!/bin/bash
#=======================================================================
# ensfcst.sh
#   This script runs the CLIMAX model with subdirectory $NODE
#=======================================================================
set -e
### input for this shell
IYMDH=$1
TYMDH=$2
MEMBER=$3
###
MEM_list=$( seq 1 $MEMBER ) 
for MEM in $MEM_list ; do
  MEM=$( printf %06i $MEM )
  cp -pv ./anal/$MEM/$IYMDH.grd ./gues/$MEM/$TYMDH.grd
done
exit 0
