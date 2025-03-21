#!/bin/bash
# memo :: should be exex by bash
#NONEED## memo :: singularity run --bind /data02,/data10 /export/home/imageshare/oneapi-hpckit_latest.sif
#=========================================================================================== CASE LOOP START
###export CASTBL="RAOBLETKF"
export CASTBL="PREPLETKF"

for CAS in $CASTBL ; do  
  if [ $CAS == "RAOBLETKF" ] ; then OBSTBL="raob_wb"  ; DASTBL="0LETKF" ; fi
  if [ $CAS == "PREPLETKF" ] ; then OBSTBL="prepbufr" ; DASTBL="0LETKF" ; fi
#==================================================#
set -e
CWD=`pwd`
cd ../ ; CLIMAX=`pwd`
cd $CWD
if [ ! -d ./grddata ] ; then mkdir -p ./grddata ; fi

CATDIR=$CWD/gnu_cat ; if [ ! -d $CATDIR ] ; then mkdir -p $CATDIR ; fi
OUTDIR=$CWD/gnu_001 ; if [ ! -d $OUTDIR ] ; then mkdir -p $OUTDIR ; fi


# ========>>> preparation           <<<============#
#NONEED#if [ -e prg_calc-rmse.exe ] ; then rm prg_calc-rmse.exe ; fi
#NONEED#
#NONEED#FOPT='-convert big_endian -assume byterecl -O3'
#NONEED#ifort $FOPT prg_calc-rmse.f90 -o prg_calc-rmse.exe
#NONEED####FOPT='-fconvert=big-endian -frecord-marker=4'
#NONEED####gfortran $FOPT prg_calc-rmse.f90 -o prg_calc-rmse.exe

# ========>>> parameter for spin-up <<<============#
NRUNME=28        # DALAYED MEAN
ADATE=2016010100 # INITIAL TIME

# ========>>> parameter for exps    <<<============#
BDATE=2016010100 # Calc. STAR TIME ::SET FOR SPIN-UP
SDATE=2017010100 # SET FOR EXPERIMENT (STR)
EDATE=2017011018 # SET FOR EXPERIMENT (END)
EDATE=2017013118 # SET FOR EXPERIMENT (END)
# ========>>> parameter for basics  <<<============#
MEMTBL="20"
###VERTBL="def"
VERTBL="int"
RGNTBL="glb"     # glb nph, trp, sph 
EXTFCST_STEP=20  # forecast lead time

# ========>>> parameter for FIXED   <<<============#
SYS="V999999_" # parameter sweeep

# ========>>> parameter for LETKF   <<<============#
if [ $DASTBL == "0LETKF" ] ; then
#  if [ $OBSTBL == "adpu" ] ; then HSGTBL="400 500 600 700 800 900 1000" ; fi
#  if [ $OBSTBL == "raob_wb" ] ; then HSGTBL="500 600 700 800" ; fi
#  if [ $OBSTBL == "raob_wb" ] ; then HSGTBL="600" ; fi
  if [ $OBSTBL == "prepbufr" ] ; then HSGTBL="600" ; fi
  RTPTBL="RTPP" ; ALPTBL="0.90"
  RTPTBL="RTPS" ; ALPTBL="1.20"
  INFTBL="1.00"
  ADDTBL="0.00"
  POWTBL="0.00"
  FACTBL="2.5"
  FGTTBL="0.00"
  RSMTBL="infKK"
  NEFTBL="40"
  NRMTBL="nrmPF"

  #WSMTBL="woWSM"

fi

LOGFILE=$CWD/log.unsubmitted
RUNFILE=$CWD/log.unfinished
if [ -e $LOGFILE ] ; then rm $LOGFILE ; fi ; touch $LOGFILE
if [ -e $RUNFILE ] ; then rm $RUNFILE ; fi ; touch $RUNFILE
############################################################################ (1) BASIC LOOPS
for MEMBER in $MEMTBL ; do
for OBS    in $OBSTBL ; do
  M=$( printf %06i ${MEMBER} )
for RGN in $RGNTBL ; do
  ILINE=0
  if    [ $RGN == "glb" ] ; then ILINE=1
  elif  [ $RGN == "nph" ] ; then ILINE=2
  elif  [ $RGN == "trp" ] ; then ILINE=3
  elif  [ $RGN == "sph" ] ; then ILINE=4
  fi
############################################################################ (2) EXPERIMENTAL LOOPS (DAS)

for VER in $VERTBL ; do
for DAS in $DASTBL ; do
for RTP in $RTPTBL ; do
for INF in $INFTBL ; do
for ADD in $ADDTBL ; do
for POW in $POWTBL ; do
#for WSM in $WSMTBL ; do
############################################################################ (3) EXPERIMENTAL LOOPS (LPF)
for FAC in $FACTBL ; do
for FGT in $FGTTBL ; do
for RSM in $RSMTBL ; do
for NEF in $NEFTBL ; do
for NRM in $NRMTBL ; do
for ALP in $ALPTBL ; do
  N=$( printf %03i ${NEF} )
for HSG    in $HSGTBL ; do
  H=$( printf %04i ${HSG} )

  CATNAME=CLIMAX${VER}-${DAS}_M${M}L${H}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}_${RSM}_${NRM}_fac${FAC}_fgt${FGT}_rsmp${N}
  if [ $DAS = "1LPFck" -o $DAS = "0LETKF" ]; then
  CATNAME=CLIMAX${VER}-${DAS}_M${M}L${H}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}  ; fi
  CATFILE_ME=${CATDIR}/$tave_${OBS}_${CATNAME}_SMP${SDATE}-${EDATE}_fcst_${RGN}_RMSE.txt
  CATFILE_SP=${CATDIR}/$tave_${OBS}_${CATNAME}_SMP${SDATE}-${EDATE}_fcst_${RGN}_SPRD.txt
  CATFILE_CK=${CATDIR}/$tave_${OBS}_${CATNAME}_SMP${SDATE}-${EDATE}_fcst_${RGN}_CRSK.txt
  CATFILE_CP=${CATDIR}/$tave_${OBS}_${CATNAME}_SMP${SDATE}-${EDATE}_fcst_${RGN}_CRSP.txt

  if [ -e ${CATFILE_ME} ] ; then rm ${CATFILE_ME} ; fi ; touch ${CATFILE_ME}
  if [ -e ${CATFILE_SP} ] ; then rm ${CATFILE_SP} ; fi ; touch ${CATFILE_SP}
  if [ -e ${CATFILE_CK} ] ; then rm ${CATFILE_CK} ; fi ; touch ${CATFILE_CK}
  if [ -e ${CATFILE_CP} ] ; then rm ${CATFILE_CP} ; fi ; touch ${CATFILE_CP}



  EXPNAME=CLIMAX${VER}-${DAS}_M${M}L${H}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}_${RSM}_${NRM}_fac${FAC}_fgt${FGT}_rsmp${N}
  if [ $DAS = "1LPFck" -o $DAS = "0LETKF" ];  then
  EXPNAME=CLIMAX${VER}-${DAS}_M${M}L${H}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW} ; fi
  EXPDIR=$CLIMAX/DATA/$OBS/$SYS$EXPNAME/fcst

  #==> (1) loop for forecast step
  STP_list=$( seq 0 $EXTFCST_STEP )

  for STP in ${STP_list} ; do
    FT=$( printf %03i ${STP} )
    OUTFT_ME=${OUTDIR}/fcst_rmse_FT${FT}_${OBS}_${EXPNAME}_${RGN}_SMP${SDATE}-${EDATE}.txt
    OUTFT_SP=${OUTDIR}/fcst_sprd_FT${FT}_${OBS}_${EXPNAME}_${RGN}_SMP${SDATE}-${EDATE}.txt
    OUTFT_CK=${OUTDIR}/fcst_crsk_FT${FT}_${OBS}_${EXPNAME}_${RGN}_SMP${SDATE}-${EDATE}.txt
    OUTFT_CP=${OUTDIR}/fcst_crsp_FT${FT}_${OBS}_${EXPNAME}_${RGN}_SMP${SDATE}-${EDATE}.txt

    echo "now processing FT$FT for period $SDATE-$EDATE, EXP: $EXPNAME"


    if [ -e ${OUTFT_ME} ]; then rm ${OUTFT_ME} ; fi ; touch ${OUTFT_ME}
    if [ -e ${OUTFT_SP} ]; then rm ${OUTFT_SP} ; fi ; touch ${OUTFT_SP}
    if [ -e ${OUTFT_CK} ]; then rm ${OUTFT_CK} ; fi ; touch ${OUTFT_CK}
    if [ -e ${OUTFT_CP} ]; then rm ${OUTFT_CP} ; fi ; touch ${OUTFT_CP}

    #==> (2) loop for initialization time    
    IDATE=$SDATE
    while test $IDATE -le $EDATE ; do
      YMD=`echo $IDATE | cut -c1-8` ; HH=`echo $IDATE | cut -c9-10`
      TDATE=`date -d "$YMD $HH:00 6 hour" +%Y%m%d%H`

      #==> (3) update time upto FT
      CYMDH=$IDATE
      if [ $STP -ne 0 ];then
        YMD=`echo $CYMDH | cut -c1-8` ; HH=`echo $CYMDH | cut -c9-10`
        HOURS=$((STP * 6))

        CYMDH=`date -d "$YMD $HH:00 $HOURS hour" +%Y%m%d%H`
      fi
      #echo $IDATE $FT $CYMDH

      #==> (3) get data      
      BASDIR=$EXPDIR/init${IDATE}
      if [ -e $BASDIR/vald/${CYMDH}_rmse.txt ]; then
        head -$ILINE $BASDIR/vald/${CYMDH}_rmse.txt       >> ${OUTFT_ME}
        head -$ILINE $BASDIR/vald/${CYMDH}_sprd.txt       >> ${OUTFT_SP}
        head -$ILINE $BASDIR/vald/${CYMDH}_crps-skill.txt >> ${OUTFT_CK}
        head -$ILINE $BASDIR/vald/${CYMDH}_crps-spred.txt >> ${OUTFT_CP}
      else
        echo "error, no such file :: " $BASDIR/vald/${CYMDH}_rmse.txt
        exit
      fi
      IDATE=$TDATE
    done # (2) end for date

    #==> main part, average
    cd $CWD

    awk -f ./src/calc_average.awk "${OUTFT_ME}" | awk -v num="$FT" '{print num, $0}' >> ${CATFILE_ME}
    awk -f ./src/calc_average.awk "${OUTFT_SP}" | awk -v num="$FT" '{print num, $0}' >> ${CATFILE_SP}
    awk -f ./src/calc_average.awk "${OUTFT_CK}" | awk -v num="$FT" '{print num, $0}' >> ${CATFILE_CK}
    awk -f ./src/calc_average.awk "${OUTFT_CP}" | awk -v num="$FT" '{print num, $0}' >> ${CATFILE_CP}
  done # (1) end for step

  # end main loop, for next experiments
done                                           # ALP
done ; done ; done ; done ; done               # FAC, FGT, RSM, NEF, NRM
done ; done ; done ; done ; done ; done ; done # RGN, VER, DAS, RTP, ADD, INF, POW
done ; done ; done                             # MEM, OBS, HSG
#=========================================================================================== CASE LOOP END
done                              # CAS
echo "end 001_calc-forecast.bash"
echo "============================================================================"
echo "  UNSUBMITTED EXPs :: " $LOGFILE
echo "============================================================================"
exit
