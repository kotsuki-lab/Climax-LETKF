#!/bin/bash
#=======================================================================
# ensfcst.sh
#   This script runs the SPEEDY model with subdirectory $NODE
#=======================================================================
set -e
### input for this shell
IYMDH=$1
SETMEM=$2
NODE=$3
EXTFCST_STEP=$4
NOSAVEENS=$5
TRUDIR=$6
LOGIC_VERIFY=$7
ENSDG=$8

MEM_list=$( seq 1 $SETMEM       )
STP_list=$( seq 0 $EXTFCST_STEP )

### for check (can be commented out)
echo "  CYMDH        ::" $CYMDH
echo "  SETMEM       ::" $SETMEM
echo "  EXTFCST_STEP ::" $EXTFCST_STEP
echo "  NOSAVEENS    ::" $NOSAVEENS
echo "  TRUDIR       ::" $TRUDIR
echo "  LOGIC_VERIFY ::" ${LOGIC_VERIFY}
echo "  ENSDG        ::" $ENSDG

### setting
CYMDH=$IYMDH
BASEDIR=./fcst/init${CYMDH}
if [ ! -d $BASEDIR/mean ] ; then mkdir -p $BASEDIR/mean ; fi
if [ ! -d $BASEDIR/sprd ] ; then mkdir -p $BASEDIR/sprd ; fi
if [ ! -d $BASEDIR/kldv ] ; then mkdir -p $BASEDIR/kldv ; fi
if [ ! -d $BASEDIR/crsp ] ; then mkdir -p $BASEDIR/crsp ; fi
if [ ! -d $BASEDIR/vald ] ; then mkdir -p $BASEDIR/vald ; fi
for MEM in $MEM_list ; do
  MEM=$( printf %06i $MEM )
  if [ ! -d $BASEDIR/$MEM ] ; then mkdir -p $BASEDIR/$MEM ; fi
done

### setting
for STP in ${STP_list} ; do
  echo $STP $CYMDH
  ### 0: analysis
  if    [ ${STP} -eq 0 ]; then 
    cp -p ./anal/mean/${CYMDH}.grd  $BASEDIR/mean/${CYMDH}.grd
    cp -p ./anal/sprd/${CYMDH}.grd  $BASEDIR/sprd/${CYMDH}.grd
    cp -p ./anal/kldv/${CYMDH}.grd  $BASEDIR/sprd/${CYMDH}.grd
    cp -p ./anal/crsp/${CYMDH}.grd  $BASEDIR/crsp/${CYMDH}.grd
    cp -p ./anal/vald/${CYMDH}*     $BASEDIR/vald/
    if  [ ${NOSAVEENS} -eq 0 ]; then
      for MEM in $MEM_list ; do
        MEM=$( printf %06i $MEM )
        cp -p ./anal/$MEM/${CYMDH}.grd  $BASEDIR/$MEM/${CYMDH}.grd 
      done
    fi

  ### 1: fcst (already forecasted)
  elif [ ${STP} -eq 1 ]; then
    YMD=`echo $CYMDH | cut -c1-8` ; HH=`echo $CYMDH | cut -c9-10`
    TYMDH=`date -d "$YMD $HH:00 6 hour" +%Y%m%d%H`
    
    for MEM in $MEM_list ; do
      MEM=$( printf %06i $MEM )
      cp -p ./gues/$MEM/${TYMDH}.grd  $BASEDIR/$MEM/${TYMDH}.grd 
    done
    CYMDH=$TYMDH
    LOGIC_MSPR=".true."
    sh 120_ensdiagnosis-XXXXXX.sh $TYMDH $SETMEM $BASEDIR $TRUDIR $LOGIC_MSPR $LOGIC_VERIFY $ENSDG

  ### 2: fcst (already forecasted)
  else
    YMD=`echo $CYMDH | cut -c1-8` ; HH=`echo $CYMDH | cut -c9-10`
    TYMDH=`date -d "$YMD $HH:00 6 hour" +%Y%m%d%H`

    echo $SETMEM > MEMINFO
    FORT2=1111
    echo $FORT2 > fort.2
    echo $CYMDH | cut -c1-4  >> fort.2
    echo $CYMDH | cut -c5-6  >> fort.2
    echo $CYMDH | cut -c7-8  >> fort.2
    echo $CYMDH | cut -c9-10 >> fort.2
    echo "'"$BASEDIR"'"      >> fort.2
    echo "'"$BASEDIR"'"      >> fort.2

    #head -20 fort.2
    echo "=======>> hello from 102_extfcstMPI-speedy.sh, employing fcst from :: " $BASEDIR/$CYMDH

    #mpirun -np $NODE ./imp.exe
    singularity run --bind $CDIR oneapi-hpckit_latest.sif $MPIRUN -np $NODE ./imp.exe
    LOGIC_MSPR=".true."
    sh 120_ensdiagnosis-XXXXXX.sh $TYMDH $SETMEM $BASEDIR $TRUDIR $LOGIC_MSPR $LOGIC_VERIFY $ENSDG

    CYMDH=$TYMDH
  fi
done

########################################### when remove ensemble data
if test ${NOSAVEENS} -eq 1 ; then
  CYMDH=$IYMDH
  STP_list=$( seq 1 $EXTFCST_STEP )
  for STP in ${STP_list} ; do
    YMD=`echo $CYMDH | cut -c1-8` ; HH=`echo $CYMDH | cut -c9-10`
    TYMDH=`date -d "$YMD $HH:00 6 hour" +%Y%m%d%H`

    rm -f $BASEDIR/??????/${TYMDH}.grd
    rm -f $BASEDIR/??????/${TYMDH}_p.grd

    CYMDH=$TYMDH
  done
fi # NOSAVEENS


exit 0
