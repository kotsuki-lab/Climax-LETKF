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
CLIMAX=`pwd`
NATURE=$CLIMAX/DATA/weatherbc     # nature run
#OUTPUT=$CLIMAX/DATA/$OBS/$EXP  # directory for new experiment
OUTPUT=$CLIMAX/DATA/iniensemble-climax
### initial date setting
IYYYY=2016
IMM=01
IDD=01
IHH=00
#-----------------------------------------------------------------------
# Usually do not modify below
#-----------------------------------------------------------------------
#SINGULARITY#source $CLIMAX/../common/timeinc.sh
### clean
rm -rf $OUTPUT
### mkdir
mkdir -p $OUTPUT/log
mkdir -p $OUTPUT/infl_mul          ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/infl_mul/
mkdir -p $OUTPUT/peff_letkf        ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/peff_letkf/
mkdir -p $OUTPUT/peff_lpf          ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/peff_lpf/
mkdir -p $OUTPUT/rsmp_lpf          ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/rsmp_lpf/
mkdir -p $OUTPUT/asis_lpf          ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/asis_lpf/
mkdir -p $OUTPUT/gues/kldv         ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/gues/kldv/
mkdir -p $OUTPUT/anal/kldv         ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/anal/kldv/
mkdir -p $OUTPUT/gues/crsp         ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/gues/crsp/
mkdir -p $OUTPUT/anal/crsp         ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/anal/crsp/
mkdir -p $OUTPUT/gptb              ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/gptb/
mkdir -p $OUTPUT/nobs              ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/nobs/
mkdir -p $OUTPUT/infl_add/000001   ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/infl_add/000001/
mkdir -p $OUTPUT/infl_add/sprd     ;  cp -p $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/infl_add/sprd/

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
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/anal/$MEM
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/anal_f/$MEM
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/gues/$MEM
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/gptb/$MEM
  echo "end MKDIR MEMBER::" $MEM
MEM=`expr $MEM + 1`
done

for MEM in mean sprd
do
mkdir -p $OUTPUT/anal/$MEM
mkdir -p $OUTPUT/anal_f/$MEM
mkdir -p $OUTPUT/gues/$MEM
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/anal/$MEM
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/anal_f/$MEM
cp $CLIMAX/200_common-climax/yyyymmddhh*.ctl $OUTPUT/gues/$MEM
done
### copy initial conditions
TY=2007
TM=01
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
#SINGULARITYUY=`timeinc6hr $TY $TM $TD $TH | cut -c1-4`
#SINGULARITYUM=`timeinc6hr $TY $TM $TD $TH | cut -c5-6`
#SINGULARITYUD=`timeinc6hr $TY $TM $TD $TH | cut -c7-8`
#SINGULARITYUH=`timeinc6hr $TY $TM $TD $TH | cut -c9-10`
#SINGULARITYTY=`timeinc6hr $UY $UM $UD $UH | cut -c1-4`
#SINGULARITYTM=`timeinc6hr $UY $UM $UD $UH | cut -c5-6`
#SINGULARITYTD=`timeinc6hr $UY $UM $UD $UH | cut -c7-8`
#SINGULARITYTH=`timeinc6hr $UY $UM $UD $UH | cut -c9-10`

UY=`$CLIMAX/../common/timeinc.bash $TY $TM $TD $TH | cut -c1-4`
UM=`$CLIMAX/../common/timeinc.bash $TY $TM $TD $TH | cut -c5-6`
UD=`$CLIMAX/../common/timeinc.bash $TY $TM $TD $TH | cut -c7-8`
UH=`$CLIMAX/../common/timeinc.bash $TY $TM $TD $TH | cut -c9-10`
TY=`$CLIMAX/../common/timeinc.bash $UY $UM $UD $UH | cut -c1-4`
TM=`$CLIMAX/../common/timeinc.bash $UY $UM $UD $UH | cut -c5-6`
TD=`$CLIMAX/../common/timeinc.bash $UY $UM $UD $UH | cut -c7-8`
TH=`$CLIMAX/../common/timeinc.bash $UY $UM $UD $UH | cut -c9-10`

  echo "END CP MEM :: " $MEM "by" $NATURE/$TY$TM$TD$TH.grd
MEM=`expr $MEM + 1`
done

echo "NORMAL END"
