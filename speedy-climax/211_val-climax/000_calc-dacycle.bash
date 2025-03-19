#!/bin/bash
# memo :: should be exex by bash
#NONEED## memo :: singularity run --bind /data02,/data10 /export/home/imageshare/oneapi-hpckit_latest.sif
#=========================================================================================== CASE LOOP START
###export GAXTBL="anal gues"
export GAXTBL="anal"
###export CASTBL="RAOBLETKF"
export CASTBL="PREPLETKF"

for GAX in $GAXTBL ; do 
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
OUTDIR=$CWD/gnu_000 ; if [ ! -d $OUTDIR ] ; then mkdir -p $OUTDIR ; fi


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
SDATE=2016010100 # SET FOR EXPERIMENT (STR)
EDATE=2017123118 # SET FOR EXPERIMENT (END)
PDATE=2017010100 # SET FOR AVE RMSE   (STR)
QDATE=$EDATE     # SET FOP AVE RMSE   (END)

# ========>>> parameter for basics  <<<============#
MEMTBL="20"
###VERTBL="def"
VERTBL="int"
RGNTBL="glb"     # glb nph, trp, sph 
# ========>>> parameter for FIXED   <<<============#
SYS="V250122_" # parameter sweeep

# ========>>> parameter for LETKF   <<<============#
if [ $DASTBL == "0LETKF" ] ; then
#  if [ $OBSTBL == "adpu" ] ; then HSGTBL="400 500 600 700 800 900 1000" ; fi
#  if [ $OBSTBL == "raob_wb" ] ; then HSGTBL="600" ; fi
  if [ $OBSTBL == "prepbufr" ] ; then HSGTBL="600" ; fi
  RTPTBL="RTPP" ; ALPTBL="0.70 0.75 0.80 0.85 0.90 0.95 1.00"
#  RTPTBL="RTPS" ; ALPTBL="0.80 0.90 1.00 1.10 1.20 1.30 1.40"
  RTPTBL="RTPS" ; ALPTBL="1.10"


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
  CATNAME=CLIMAX${VER}-${DAS}_M${M}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}_${RSM}_${NRM}_fac${FAC}_fgt${FGT}_rsmp${N}
  if [ $DAS = "1LPFck" -o $DAS = "0LETKF" ]; then
  CATNAME=CLIMAX${VER}-${DAS}_M${M}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}  ; fi
  CATFILE_ME=${CATDIR}/$tave_${OBS}_${CATNAME}_SMP${PDATE}-${QDATE}_${GAX}_${RGN}_RMSE.txt
  CATFILE_SP=${CATDIR}/$tave_${OBS}_${CATNAME}_SMP${PDATE}-${QDATE}_${GAX}_${RGN}_SPRD.txt

  if [ -e ${CATFILE_ME} ] ; then rm ${CATFILE_ME} ; fi ; touch ${CATFILE_ME}
  if [ -e ${CATFILE_SP} ] ; then rm ${CATFILE_SP} ; fi ; touch ${CATFILE_SP}

for HSG    in $HSGTBL ; do
  H=$( printf %04i ${HSG} )


  EXPNAME=CLIMAX${VER}-${DAS}_M${M}L${H}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW}_${RSM}_${NRM}_fac${FAC}_fgt${FGT}_rsmp${N}
  if [ $DAS = "1LPFck" -o $DAS = "0LETKF" ];  then
  EXPNAME=CLIMAX${VER}-${DAS}_M${M}L${H}_inf${INF}_${RTP}${ALP}_ad${ADD}_po${POW} ; fi

  EXPDIR=$CLIMAX/DATA/$OBS/$SYS$EXPNAME/$GAX
  TXTDIR=$CLIMAX/DATA/$OBS/$SYS$EXPNAME/out

  OUTTIME_ME=${TXTDIR}/${GAX}_rmse_time_${OBS}_${EXPNAME}_${RGN}.txt
  OUTTIME_SP=${TXTDIR}/${GAX}_sprd_time_${OBS}_${EXPNAME}_${RGN}.txt
  OUTTRUN_ME=${TXTDIR}/${GAX}_rmse_trun_${OBS}_${EXPNAME}_${RGN}.txt
  OUTTRUN_SP=${TXTDIR}/${GAX}_sprd_trun_${OBS}_${EXPNAME}_${RGN}.txt
  OUTTAVE_ME=${TXTDIR}/${GAX}_rmse_tave_${OBS}_${EXPNAME}_${RGN}_SMP${PDATE}-${QDATE}.txt
  OUTTAVE_SP=${TXTDIR}/${GAX}_sprd_tave_${OBS}_${EXPNAME}_${RGN}_SMP${PDATE}-${QDATE}.txt

cd $CWD
#NONEED#if [ -e climax.cnf ] ; then rm climax.cnf ; fi
#NONEED#cat >> climax.cnf << EOF
#NONEED#&climax_param
#NONEED#  adate         = $ADATE,
#NONEED#  bdate         = $BDATE,
#NONEED#  sdate         = $SDATE,
#NONEED#  edate         = $EDATE,
#NONEED#  pdate         = $PDATE,
#NONEED#  qdate         = $QDATE,
#NONEED#  nbv           = $MEMBER,
#NONEED#  nrunme        = $NRUNME, 
#NONEED#  natdir        = '$CLIMAX/DATA/weatherbc/'
#NONEED#  expdir        = '${EXPDIR}',
#NONEED#  outtime_me    = '${OUTTIME_ME}',
#NONEED#  outtime_sp    = '${OUTTIME_SP}',
#NONEED#  outtrun_me    = '${OUTTRUN_ME}',
#NONEED#  outtrun_sp    = '${OUTTRUN_SP}',
#NONEED#  outtave_me    = '${OUTTAVE_ME}',
#NONEED#  outtave_sp    = '${OUTTAVE_SP}',
#NONEED#  expname       = '${EXPNAME}',
#NONEED#  obsname       = '${OBS}'
#NONEED#  cloc          = '$H',
#NONEED#/
#NONEED#EOF

STAMP=0
#NONEED#if [ -e ${OUTTIME_ME} -a -e ${OUTTAVE_ME} ] ; then 
#NONEED#    echo "  already done    & skipped  :: " $EXPDIR
#NONEED#    STAMP=1
#NONEED#    cd $CWD/gnu_000/
#NONEED#    ln -fs ${OUTTIME_ME} ./
#NONEED#    ln -fs ${OUTTIME_SP} ./
#NONEED#    ln -fs ${OUTTRUN_ME} ./
#NONEED#    ln -fs ${OUTTRUN_SP} ./
#NONEED#    cd $CWD
#NONEED#else
  echo "   Processing..." $EXPDIR
  if [ -e $EXPDIR/mean/$EDATE.grd ] ; then
    if [ ! -d $TXTDIR ] ; then mkdir -p $TXTDIR ; fi
#NONEED#    ./prg_calc-rmse.exe
  
    if [ -e ${OUTTIME_ME} ]; then rm ${OUTTIME_ME} ; fi ; touch ${OUTTIME_ME}
    if [ -e ${OUTTIME_SP} ]; then rm ${OUTTIME_SP} ; fi ; touch ${OUTTIME_SP}
    if [ -e ${OUTTRUN_ME} ]; then rm ${OUTTRUN_ME} ; fi ; touch ${OUTTRUN_ME}
    if [ -e ${OUTTRUN_SP} ]; then rm ${OUTTRUN_SP} ; fi ; touch ${OUTTRUN_SP}
    if [ -e ${OUTTAVE_ME} ]; then rm ${OUTTAVE_ME} ; fi ; touch ${OUTTAVE_ME}
    if [ -e ${OUTTAVE_SP} ]; then rm ${OUTTAVE_SP} ; fi ; touch ${OUTTAVE_SP}

    # loop start    
    CDATE=$SDATE
    while test $CDATE -le $EDATE ; do
      YMD=`echo $CDATE | cut -c1-8` ; HH=`echo $CDATE | cut -c9-10`
      TDATE=`date -d "$YMD $HH:00 6 hour" +%Y%m%d%H`

        head -$ILINE $EXPDIR/vald/${CDATE}_rmse.txt >> ${OUTTIME_ME}
        head -$ILINE $EXPDIR/vald/${CDATE}_sprd.txt >> ${OUTTIME_SP}
      if [ $PDATE -le $CDATE -a $CDATE -le $QDATE ]; then
        head -$ILINE $EXPDIR/vald/${CDATE}_rmse.txt >> ${OUTTAVE_ME}
        head -$ILINE $EXPDIR/vald/${CDATE}_sprd.txt >> ${OUTTAVE_SP}
      fi

      CDATE=$TDATE
    done

    nl -w6 -s' ' ${OUTTIME_ME} > ${OUTTIME_ME}_tmp && mv ${OUTTIME_ME}_tmp ${OUTTIME_ME}
    nl -w6 -s' ' ${OUTTIME_SP} > ${OUTTIME_SP}_tmp && mv ${OUTTIME_SP}_tmp ${OUTTIME_SP}
    nl -w6 -s' ' ${OUTTAVE_ME} > ${OUTTAVE_ME}_tmp && mv ${OUTTAVE_ME}_tmp ${OUTTAVE_ME}
    nl -w6 -s' ' ${OUTTAVE_SP} > ${OUTTAVE_SP}_tmp && mv ${OUTTAVE_SP}_tmp ${OUTTAVE_SP}

    #==> average
    awk -f ./src/calc_average.awk "${OUTTAVE_ME}" > "${OUTTAVE_ME}_tmp" && mv ${OUTTAVE_ME}_tmp ${OUTTAVE_ME}
    awk -f ./src/calc_average.awk "${OUTTAVE_SP}" > "${OUTTAVE_SP}_tmp" && mv ${OUTTAVE_SP}_tmp ${OUTTAVE_SP}
    sed -i "s/^/$H /" "${OUTTAVE_ME}"
    sed -i "s/^/$H /" "${OUTTAVE_SP}"

    #==> runnning mean
    NDATA=$(wc -l < "${OUTTIME_ME}")
    if [ $NDATA -ge $NRUNME ]; then
      IDATA=$NRUNME
      while test $IDATA -le $NDATA ; do
        #echo $IDATA
        head -$IDATA ${OUTTIME_ME} | tail -$NRUNME > me.tmp
        head -$IDATA ${OUTTIME_SP} | tail -$NRUNME > sp.tmp
        awk -f ./src/calc_average.awk "me.tmp" >> "${OUTTRUN_ME}"
        awk -f ./src/calc_average.awk "sp.tmp" >> "${OUTTRUN_SP}"
        IDATA=`expr $IDATA + 1`
      done 
    fi

    STAMP=1
    cd $CWD/gnu_000/
    ln -fs ${OUTTIME_ME} ./
    ln -fs ${OUTTIME_SP} ./
    ln -fs ${OUTTRUN_ME} ./
    ln -fs ${OUTTRUN_SP} ./
    cd $CWD
  else
  	if [ ! -d $EXPDIR ] ; then
      echo "  exp not started & skipped  :: " $EXPDIR
      echo "  exp not started & skipped  :: " $EXPDIR >> $LOGFILE
    else
      echo "  exp not finised & skipped  :: " $EXPDIR
      echo "  exp not finised & skipped  :: " $EXPDIR >> $RUNFILE
    fi
  fi
#NONEED#fi
if [ $STAMP -eq 1 ] ; then cat ${OUTTAVE_ME} >> ${CATFILE_ME} ; fi
if [ $STAMP -eq 1 ] ; then cat ${OUTTAVE_SP} >> ${CATFILE_SP} ; fi

done                                           # ALP
done ; done ; done ; done ; done               # FAC, FGT, RSM, NEF, NRM
done ; done ; done ; done ; done ; done ; done # RGN, VER, DAS, RTP, ADD, INF, POW
done ; done ; done                             # MEM, OBS, HSG
#=========================================================================================== CASE LOOP END
done                                    # CAS
done                                    # GAX
echo "end 000_rmse-gues.sh"
echo "============================================================================"
echo "  UNSUBMITTED EXPs :: " $LOGFILE
echo "============================================================================"
exit
