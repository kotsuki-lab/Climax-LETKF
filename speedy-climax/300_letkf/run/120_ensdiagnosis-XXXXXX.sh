#!/bin/bash
set -e
### input for this shell
CYMDH=$1
SETMEM=$2
BASDIR=$3
TRUDIR=$4
LOGIC_MSPR=$5
LOGIC_VERIFY=$6
ENSDG=$7

### check
#echo "  CYMDH        ::" $CYMDH
#echo "  SETMEM       ::" $SETMEM
#echo "  BASDIR       ::" $BASDIR
#echo "  TRUDIR       ::" $TRUDIR
#echo "  LOGIC_VERIFY ::" ${LOGIC_VERIFY}

### setting
if [ -e ensdiag.cnf ] ; then rm ensdiag.cnf ; fi
cat >> ensdiag.cnf << EOF
&ensdiag_param
  nbv      = ${SETMEM},
  ymdh     = $CYMDH,
  basedir  = "$BASDIR/",
  valddir  = "$BASDIR/vald",
  truedir  = "$TRUDIR" 
  logic_mspr = ${LOGIC_MSPR},
  logic_verf = ${LOGIC_VERIFY},
/
EOF
#DEBUG#mv $OUTPUT/$FGAN/mean/${CYMDH}.grd $OUTPUT/$FGAN/mean/${CYMDH}.grd.old 
#DEBUG#mv $OUTPUT/$FGAN/sprd/${CYMDH}.grd $OUTPUT/$FGAN/sprd/${CYMDH}.grd.old 
singularity run --bind $CDIR oneapi-hpckit_latest.sif $MPIRUN -np $NODE ./$ENSDG


exit
