#!/bin/bash
#=======================================================================
# environment::
#   singularity run --bind /data02,/data10 ../share/oneapi-hpckit_latest.sif
#     - bind directories (e.g., data02 data10) should be modified by users
#=======================================================================
CDIR=`pwd`
cd ..
CLIMAX=`pwd`
#for EXP in "raob_cl" "raob_wb" ; do
for EXP in "raob_wb" ; do
    ##
cd $CDIR
ln -fs station_${EXP}_climax.tbl station.tbl
cd ../
##

if   [ $EXP = "raob_cl" ]; then
  TRUEDIR=$CLIMAX/DATA/nature-climax
elif [ $EXP = "raob_wb" ]; then
  TRUEDIR=$CLIMAX/DATA/weatherbc
else
  echo "error, nature is not defined for " $EXP
  exit
fi

OBSDIR=$CLIMAX/DATA/$EXP/obs
PGM=obsmake.s01
#SINGULARITY#source $CLIMAX/../common/timeinc.sh
# Initial date
IY=2017
IM=01
ID=01
IH=00
# Final date
EY=2018
EM=12
ED=31
EH=18
# start
cd $CDIR
mkdir -p $OBSDIR
#ln -fs $CLIMAX/common/orography_t30.dat fort.21
ln -fs $CLIMAX/200_common-climax/orography_x64y32.grd orography_x64y32.grd
# main loop
while test $IY$IM$ID$IH -le $EY$EM$ED$EH
do
ln -sf $TRUEDIR/$IY$IM$ID$IH.grd true.grd

./$PGM

mv obs.dat $OBSDIR/$IY$IM$ID$IH.dat
echo "  obs data made on :: " $OBSDIR/$IY$IM$ID$IH.dat " for " $EXP " nature is " $TRUEDIR
rm true.grd

#SINGULARITY#TY=`timeinc6hr $IY $IM $ID $IH | cut -c1-4`
#SINGULARITY#TM=`timeinc6hr $IY $IM $ID $IH | cut -c5-6`
#SINGULARITY#TD=`timeinc6hr $IY $IM $ID $IH | cut -c7-8`
#SINGULARITY#TH=`timeinc6hr $IY $IM $ID $IH | cut -c9-10`
TY=`$CLIMAX/../common/timeinc.bash $IY $IM $ID $IH | cut -c1-4`
TM=`$CLIMAX/../common/timeinc.bash $IY $IM $ID $IH | cut -c5-6`
TD=`$CLIMAX/../common/timeinc.bash $IY $IM $ID $IH | cut -c7-8`
TH=`$CLIMAX/../common/timeinc.bash $IY $IM $ID $IH | cut -c9-10`


IY=$TY
IM=$TM
ID=$TD
IH=$TH
done
rm fort.21
rm station.tbl
echo "NORMAL END"
done
