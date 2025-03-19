#!/bin/bash
#=======================================================================
# ensfcst.sh
#   This script runs the SPEEDY model with subdirectory $NODE
#=======================================================================
set -e
### input for this shell
YMDH=$1
SETMEM=$2
NODE=$3
###
if test 5$3 -eq 5 ; then
  echo "ERROR in ensfcst.sh"
  exit
fi
### run

echo $SETMEM > MEMINFO
FORT2=1111
echo $FORT2 > fort.2
echo $YMDH | cut -c1-4  >> fort.2
echo $YMDH | cut -c5-6  >> fort.2
echo $YMDH | cut -c7-8  >> fort.2
echo $YMDH | cut -c9-10 >> fort.2
echo "'./anal/'"        >> fort.2
echo "'./gues/'"        >> fort.2

#mpirun -np $NODE ./imp.exe
#SINGULARITY#$MPIRUN -np $NODE ./imp.exe
singularity run --bind $CDIR oneapi-hpckit_latest.sif $MPIRUN -np $NODE ./imp.exe
#fipp -C -Ihwm -d prof mpiexec -np $NODE ./imp.exe

exit 0
