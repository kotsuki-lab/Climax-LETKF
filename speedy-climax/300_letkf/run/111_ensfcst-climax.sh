#!/bin/bash
#=======================================================================
# ensfcst.sh
#   This script runs the CLIMAX model with subdirectory $NODE
#=======================================================================
set -e

CWD=`pwd`

### input for this shell
IYMDH=$1
SETMEM=$2
EXTFCST_STEP=$3
NOSAVEENS=$4
TRUDIR=$5
LOGIC_VERIFY=$6
ENSDG=$7

### (1) six hour forecasts for DA cycle
echo "  hello from 111_ensfcst-climax.sh, (1) fcst for DA cycle" 
MEM=$( printf %06i ${SETMEM} )
singularity run --bind $CWD climax.sif python3 116_predict_climax_efcs.py \
  --anal_dir ./anal/ --gues_dir ./gues/ --member_ids 000001-${MEM} --date_time $IYMDH \
  --lead_time 6 --model_dir models/${CLIMAXVER} --loop 1

### (2) extended forecasts
echo "  hello from 111_ensfcst-climax.sh, (2) extended forecasts for steps" $EXTFCST_STEP
if [ ${EXTFCST_STEP} -ge 1 ] ; then

  # 2.1 preparation
  BASEDIR=./fcst/init${IYMDH}
  if [ ! -d $BASEDIR/mean ] ; then mkdir -p $BASEDIR/mean ; fi
  if [ ! -d $BASEDIR/sprd ] ; then mkdir -p $BASEDIR/sprd ; fi
  if [ ! -d $BASEDIR/kldv ] ; then mkdir -p $BASEDIR/kldv ; fi
  if [ ! -d $BASEDIR/crsp ] ; then mkdir -p $BASEDIR/crsp ; fi
  if [ ! -d $BASEDIR/vald ] ; then mkdir -p $BASEDIR/vald ; fi
  for MEM in $MEM_list ; do
    MEM=$( printf %06i $MEM )
    if [ ! -d $BASEDIR/$MEM ] ; then mkdir -p $BASEDIR/$MEM ; fi
  done

  cp -p ./anal/mean/${IYMDH}.grd  $BASEDIR/mean/${IYMDH}.grd
  cp -p ./anal/sprd/${IYMDH}.grd  $BASEDIR/sprd/${IYMDH}.grd
  #cp -p ./anal/kldv/${IYMDH}.grd  $BASEDIR/sprd/${IYMDH}.grd # 20241003 not computed for analysis
  #cp -p ./anal/crsp/${IYMDH}.grd  $BASEDIR/crsp/${IYMDH}.grd # 20250127 not computed for cycles
  cp -p ./anal/vald/${IYMDH}*     $BASEDIR/vald/
  if  [ ${NOSAVEENS} -eq 0 ]; then
    for MEM in $MEM_list ; do
      MEM=$( printf %06i $MEM )
      cp -p ./anal/$MEM/${IYMDH}.grd  $BASEDIR/$MEM/${IYMDH}.grd 
    done
  fi  

  # 2.2 forecast
  singularity run --bind $CWD climax.sif python3 116_predict_climax_efcs.py \
    --anal_dir ./anal/ --gues_dir $BASEDIR --member_ids 000001-${MEM} --date_time $IYMDH \
    --lead_time 6 --model_dir models/${CLIMAXVER} --loop ${EXTFCST_STEP}

  # 2.3 ensdiagnosis
  STP_list=$( seq 1 $EXTFCST_STEP )
  for STP in ${STP_list} ; do
    YMD=`echo $IYMDH | cut -c1-8` ; HH=`echo $IYMDH | cut -c9-10`
    TYMDH=`date -d "$YMD $HH:00 6 hour" +%Y%m%d%H`

    LOGIC_MSPR=".true." # necessary for forecast
    sh 120_ensdiagnosis-XXXXXX.sh $TYMDH $SETMEM $BASEDIR $TRUDIR $LOGIC_MSPR $LOGIC_VERIFY $ENSDG

    if  [ ${NOSAVEENS} -eq 1 ]; then
      rm -f $BASEDIR/??????/${TYMDH}.grd
    fi
    IYMDH=$TYMDH
  done
fi

exit 0
