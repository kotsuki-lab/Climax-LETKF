#!/bin/sh
#=======================================================================
# init.sh
#   This script prepares for new LETKF cycle-run experiment
#=======================================================================
set -e
#-----------------------------------------------------------------------
# Modify below according to your environment
#-----------------------------------------------------------------------
MEMBER=128
M=$( printf %06i ${MEMBER} )
#OBS=raob
#EXP=M${M}L500IADP

### directory settings
cd ../..
SPEEDY=`pwd`
NATURE=$SPEEDY/DATA/nature-speedy     # nature run
#OUTPUT=$SPEEDY/DATA/$OBS/$EXP  # directory for new experiment
OUTPUT=$SPEEDY/DATA/iniensemble-speedy
### initial date setting
IYYYY=1982
IMM=01
IDD=01
IHH=00
#-----------------------------------------------------------------------
# Usually do not modify below
#-----------------------------------------------------------------------
#SINGULARITY#source $SPEEDY/../common/timeinc.sh
### clean
rm -rf $OUTPUT
### mkdir
mkdir -p $OUTPUT/log
mkdir -p $OUTPUT/infl_mul          ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/infl_mul/
mkdir -p $OUTPUT/peff_letkf        ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/peff_letkf/
mkdir -p $OUTPUT/peff_lpf          ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/peff_lpf/
mkdir -p $OUTPUT/rsmp_lpf          ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/rsmp_lpf/
mkdir -p $OUTPUT/asis_lpf          ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/asis_lpf/
mkdir -p $OUTPUT/gues/kldv         ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/gues/kldv/
mkdir -p $OUTPUT/anal/kldv         ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/anal/kldv/
mkdir -p $OUTPUT/gues/crsp         ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/gues/crsp/
mkdir -p $OUTPUT/anal/crsp         ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/anal/crsp/
mkdir -p $OUTPUT/gptb              ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/gptb/
mkdir -p $OUTPUT/nobs              ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/nobs/
mkdir -p $OUTPUT/infl_add/000001   ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/infl_add/000001/
mkdir -p $OUTPUT/infl_add/sprd     ;  cp -p $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/infl_add/sprd/

MEM=1
while test $MEM -le $MEMBER
do
#if test $MEM -lt 100
#then
#MEM=0$MEM
#fi
#if test $MEM -lt 10
#then
#MEM=0$MEM
#fi
MEM=$( printf %06i ${MEM} )
mkdir -p $OUTPUT/anal/$MEM
mkdir -p $OUTPUT/anal/$MEM/vald
mkdir -p $OUTPUT/anal_f/$MEM
mkdir -p $OUTPUT/gues/$MEM
mkdir -p $OUTPUT/gues/$MEM/vald
mkdir -p $OUTPUT/gptb/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/anal/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/anal_f/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/gues/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/gptb/$MEM
  echo "end MKDIR MEMBER::" $MEM
MEM=`expr $MEM + 1`
done

for MEM in mean sprd
do
mkdir -p $OUTPUT/anal/$MEM
mkdir -p $OUTPUT/anal_f/$MEM
mkdir -p $OUTPUT/gues/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/anal/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/anal_f/$MEM
cp $SPEEDY/100_common-speedy/yyyymmddhh*.ctl $OUTPUT/gues/$MEM
done
### copy initial conditions
TY=1982
TM=02
TD=01
TH=00
MEM=1
while test $MEM -le $MEMBER
do
#if test $MEM -lt 100
#then
#MEM=0$MEM
#fi
#if test $MEM -lt 10
#then
#MEM=0$MEM
#fi
MEM=$( printf %06i ${MEM} )
cp $NATURE/$TY$TM$TD$TH.grd $OUTPUT/gues/$MEM/$IYYYY$IMM$IDD$IHH.grd
#SINGULARITY#UY=`timeinc6hr $TY $TM $TD $TH | cut -c1-4`
#SINGULARITY#UM=`timeinc6hr $TY $TM $TD $TH | cut -c5-6`
#SINGULARITY#UD=`timeinc6hr $TY $TM $TD $TH | cut -c7-8`
#SINGULARITY#UH=`timeinc6hr $TY $TM $TD $TH | cut -c9-10`
#SINGULARITY#TY=`timeinc6hr $UY $UM $UD $UH | cut -c1-4`
#SINGULARITY#TM=`timeinc6hr $UY $UM $UD $UH | cut -c5-6`
#SINGULARITY#TD=`timeinc6hr $UY $UM $UD $UH | cut -c7-8`
#SINGULARITY#TH=`timeinc6hr $UY $UM $UD $UH | cut -c9-10`

UY=`$SPEEDY/../common/timeinc.bash $TY $TM $TD $TH | cut -c1-4`
UM=`$SPEEDY/../common/timeinc.bash $TY $TM $TD $TH | cut -c5-6`
UD=`$SPEEDY/../common/timeinc.bash $TY $TM $TD $TH | cut -c7-8`
UH=`$SPEEDY/../common/timeinc.bash $TY $TM $TD $TH | cut -c9-10`
TY=`$SPEEDY/../common/timeinc.bash $UY $UM $UD $UH | cut -c1-4`
TM=`$SPEEDY/../common/timeinc.bash $UY $UM $UD $UH | cut -c5-6`
TD=`$SPEEDY/../common/timeinc.bash $UY $UM $UD $UH | cut -c7-8`
TH=`$SPEEDY/../common/timeinc.bash $UY $UM $UD $UH | cut -c9-10`

  echo "END CP MEM :: " $MEM "by" $NATURE/$TY$TM$TD$TH.grd
MEM=`expr $MEM + 1`
done

echo "NORMAL END"
