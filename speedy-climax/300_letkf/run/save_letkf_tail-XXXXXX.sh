############################## parameter needed to be editted (end) ##############################
NNODE=1
PPN=$NODE
PE=$NODE
export MPI_XPMEM_ENABLED=disabled
#------------------------------------------------------------------------------------------------#
NOSAVEENS=1 #0:save all members, 1:save only mean/sprd 
date +"%Y/%m/%d %H:%M:%S"
set_START(){
        local dummy
        read START dummy < /proc/uptime
}

get_ELAPS() {
        local dummy
        read END dummy < /proc/uptime
        ELAPS=$(echo "$END - $START")
        ELAPS=`echo "scale=8; ${ELAPS}*0.001" | bc`
#        let ELAPS=${END/./}0-${START/./}0
#        ELAPS=`echo "scale=8; ${ELAPS}*0.001" | bc`
}
################################################################################## (0) exp. name def.
M=$( printf %06i ${SETMEM} )
H=$( printf %04i ${SETHSG} )

if [ ${VERIFY__MODE} -eq 0 ] ; then
  LOGIC_VERIFY=".false."
else
  LOGIC_VERIFY=".true."
fi

#===> weight interpolation
CWINSMP=$( printf %02i ${WINSMP} )
if [ "$WINSMP" = "1" ]; then
  LOGIC_WININT=".false."
else 
  LOGIC_WININT=".true."
  #===> LUT-BASED WEIGHT INTERPOLATION
  if [ $WINLUT = "BLLUT" ] ; then LUTNAM=$LUTDIR/interp_bilin${CWINSMP}.bin ; fi
  if [ $WINLUT = "ADJST" ] ; then LUTNAM=$LUTDIR/interp_adjst${CWINSMP}.bin ; fi
  if [ $WINLUT = "ADREV" ] ; then LUTNAM=$LUTDIR/interp_adjst${CWINSMP}.bin ; fi
  if [ $WINLUT = "REGUL" ] ; then LUTNAM=$LUTDIR/interp_regul${CWINSMP}.bin ; fi
  if [ $WINLUT = "RGREV" ] ; then LUTNAM=$LUTDIR/interp_regul${CWINSMP}.bin ; fi
  if [ $WINLUT = "WCUNI" ] ; then LUTNAM=$LUTDIR/interp_wcuni${CWINSMP}_${SETOBS}H${H}.bin ; fi
  if [ $WINLUT = "WCREG" ] ; then LUTNAM=$LUTDIR/interp_wcreg${CWINSMP}_${SETOBS}H${H}.bin ; fi

  if [ ! -e $WINLUT ]; then
    echo "  error, LUTFILE NOT FOUND :: " $WINLUT
    exit
  fi  
fi      

#===> weight treatment
if [ "$WINSTH" = "0" ]; then LOGIC_WINSTH=".false." ; fi
if [ "$WINSTH" = "1" ]; then LOGIC_WINSTH=".true."  ; fi
#==========================================>>>>> fixed descriptions  (str)
MEM_list=$( seq 1 $M )

### directory settings
cd $CDIR
cd ../..
BASDIR=`pwd`
OUTPUT=$BASDIR/DATA/$SETOBS/$EXP # data directory
OBSDIR=$BASDIR/DATA/$SETOBS/obs  # obs data directory
TMPDIR=$BASDIR/DATA/tmp_${SETOBS}_${EXP}/letkf     # work directory
if   test $DISK -eq 0 ; then
  TMPDIR=$BASDIR/DATA/tmp_${SETOBS}_${EXP}/letkf   # work directory
elif test $DISK -eq 1 ; then
  TMPDIR=/dev/shm/kotsuki_${SETOBS}_${EXP}/letkf   # work directory (specify RAM disc)
fi

if [   $SETNWP = "SPEEDY" ]; then
  LETKF=letkf020-speedy.m01 ; OBSOPE=obsopeMPI-speedy.s01 ; ENSDG=ensdiag020-speedy.m01
  TRUDIR=$BASDIR/DATA/nature-speedy  # not used in DA experiments, used for ens diagnosis
  INTDIR=$BASDIR/DATA/iniensemble-speedy
  if [   $RESTART_MODE -eq 9 -a $SETMEM -eq 40 -a $SETOBS = "raob_sp" ]; then 
    INTDIR=$BASDIR/DATA/raob_sp/init_ensemble_M000040L1200IADP.1983010100
  elif [ $RESTART_MODE -eq 9 -a $SETMEM -eq 40 -a $SETOBS = "reg2_sp" ]; then
    INTDIR=$BASDIR/DATA/reg2_sp/init_ensemble_M000040L0600IADP.1983010100
  fi

  echo $SETOBS $INTDIR
elif [ $SETNWP = "CLIMAX" ]; then
  LETKF=letkf020-climax.m01 ; OBSOPE=obsopeMPI-climax.s01 ; ENSDG=ensdiag020-climax.m01
  TRUDIR=$BASDIR/DATA/weatherbc      # not used in DA experiments, used for ens diagnosis
  INTDIR=$BASDIR/DATA/iniensemble-climax
else
  echo "  error, specified model is not supported :: " $SETNWP
  exit
fi
echo "=========================================================================================="
echo " MONITOR OBS & INTDIR :: " $SETOBS $INTDIR
echo "=========================================================================================="


#if [ -d $OUTPUT ] ; then rm -r $OUTPUT ; fi
if [ ${RESTART_MODE} -eq 0 -o ${RESTART_MODE} -eq 9 ] ; then
  if [ -d $OUTPUT ] ; then 
    echo "  process STOPED!!! not to overwrite results on :: " $OUTPUT
    exit
  fi
fi

###if [ ! -d $OUTPUT ] ; then cp -ar $INTDIR $OUTPUT ; fi
if [ ${RESTART_MODE} -eq 0 -o ${RESTART_MODE} -eq 9 ] ; then
mkdir -p $OUTPUT/gues 
mkdir -p $OUTPUT/fcst
mkdir -p $OUTPUT/gues/vald
mkdir -p $OUTPUT/anal/vald
mkdir -p $OUTPUT/wvec # SK
cd $OUTPUT
  cp -ar $INTDIR/anal        ./
  cp -ar $INTDIR/anal_f      ./
  cp -ar $INTDIR/nobs        ./
  cp -ar $INTDIR/infl_mul    ./
  cp -ar $INTDIR/infl_add    ./
  cp -ar $INTDIR/peff_letkf  ./
  cp -ar $INTDIR/peff_lpf    ./
  cp -ar $INTDIR/rsmp_lpf    ./
  cp -ar $INTDIR/asis_lpf    ./
  cp -ar $INTDIR/log         ./
  cp -ar $INTDIR/gues/mean   ./gues/
  cp -ar $INTDIR/gues/sprd   ./gues/
  cp -ar $INTDIR/gues/kldv   ./gues/
  cp -ar $INTDIR/gues/crsp   ./gues/
  cd $OUTPUT/gues
    for MEM in $MEM_list ; do
      MEM=$( printf %06i $MEM )
      cp -ar   $INTDIR/gues/$MEM ./
    done
fi

### directory settings
TIMELOG="$CDIR/stdout_${LOGNAM}" ; echo $EXP with Specified_DISK > $TIMELOG
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "Job started at $CTIME" >> $TIMELOG

cd $CDIR
### NODE info files
if test -e nodefile/$EXP ; then rm -fr nodefile/$EXP ; fi
mkdir -p nodefile/$EXP
# for LETKF
NODE_info=nodefile/$EXP/nodefile_letkf
rm -f $NODE_info ; touch $NODE_info
for n in $NODE_list ; do
  p=1
  while test $p -le $PPN ; do
    echo $n >> $NODE_info
    p=`expr $p + 1`
  done
done

#-----------------------------------------------------------------------
# Usually do not modify below
#-----------------------------------------------------------------------
#SINGULARITY#source $CLIMAX/../common/timeinc.sh
  IYYYY=$SYYYY  ;  IMM=$SMM  ;  IDD=$SDD  ;  IHH=$SHH
#
# Work directory
#
rm -rf $TMPDIR
mkdir -p $TMPDIR
  cd $TMPDIR
  # (1) common
  cp $BASDIR/300_letkf/$LETKF                             ./
  cp $BASDIR/300_letkf/$OBSOPE                            ./
  cp $BASDIR/300_letkf/$ENSDG                             ./
  cp $CDIR/nodefile/$EXP/nodefile_letkf                   ./
  cp $CDIR/120_ensdiagnosis-XXXXXX.sh                     ./
  ln -fs $BASDIR/share/oneapi-hpckit_latest.sif           ./

  # (2) SPEEDY/CLIMAX
  if [   $SETNWP = "SPEEDY" ]; then
    cp $CDIR/101_ensfcstMPI-speedy.sh                     ./
    cp $CDIR/102_extfcstMPI-speedy.sh                     ./
    SB=$BASDIR/101_model-speedy/data/bc/t30/clim
    SC=$BASDIR/101_model-speedy/data/bc/t30/anom
    cp $BASDIR/101_model-speedy/run/imp_MPI_rank2.exe     ./imp.exe
    cp $BASDIR/100_common-speedy/orography_t30.dat        ./orography_t30.dat
    cp $SB/orog_lsm_alb.t30.grd                           ./fort.20
    cp $SB/sst_8190clim.t30.sea.grd                       ./fort.21
    cp $SB/seaice_8190clim.t30.sea.grd                    ./fort.22
    cp $SB/skt_8190clim.t30.land.grd                      ./fort.23
    cp $SB/sndep_8190clim.t30.land.grd                    ./fort.24
    cp $SB/veget.t30.land.grd                             ./fort.25
    cp $SB/soilw_8190clim.t30.land.grd                    ./fort.26
    cp $SC/sst_anom_7990.t30.grd                          ./fort.30
  elif [ $SETNWP = "CLIMAX" ]; then
    cp $CDIR/111_ensfcst-climax.sh                        ./
    cp $CDIR/119_enspersist-climax.sh                     ./
    cp $CDIR/115_predict_climax_sngl.py                   ./
    cp $CDIR/116_predict_climax_efcs.py                   ./
    cp $BASDIR/200_common-climax/orography_x64y32.grd     ./orography_x64y32.grd
    cp $BASDIR/200_common-climax/orography_x64y32.ctl     ./orography_x64y32.ctl
    ln -fs $CLIMAXDIR/climax                              ./
    ln -fs $CLIMAXDIR/climax.sif                          ./
    ln -fs $CLIMAXDIR/models                              ./
    ln -fs $CLIMAXDIR/normalize_mean.npz                  ./
    ln -fs $CLIMAXDIR/normalize_std.npz                   ./
    ln -fs $CLIMAXDIR/constants.grd                       ./
  fi

### inputs
set_START
if test $DISK -eq 0 ; then
  ln -s $OUTPUT/anal       anal
  ln -s $OUTPUT/anal_f     anal_f
  ln -s $OUTPUT/gues       gues
  ln -s $OUTPUT/fcst       fcst
  ln -s $OUTPUT/nobs       nobs
  ln -s $OUTPUT/infl_mul   infl_mul
  ln -s $OUTPUT/infl_add   infl_add
  ln -s $OUTPUT/peff_lpf   peff_lpf
  ln -s $OUTPUT/peff_letkf peff_letkf
  ln -s $OUTPUT/rsmp_lpf   rsmp_lpf
  ln -s $OUTPUT/log        log
  ln -s $OBSDIR            obs
  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "copy ended: $CTIME" >> $TIMELOG

  #==> SK
  ln -s $LUTNAM           BILINLUT.bin
  ln -s $OUTPUT/wvec      wvec
  ln -s $OUTPUT/asis_lpf  asis_lpf

#NeedToBeModified4RAM#elif test $DISK -eq 1 ; then
#NeedToBeModified4RAM#  echo ">>> Compress files for STGIN"
#NeedToBeModified4RAM#  if test -e tmp_stgin ; then rm -rf tmp_stgin ; fi
#NeedToBeModified4RAM#  mkdir tmp_stgin
#NeedToBeModified4RAM#  cd tmp_stgin
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#    # Compress initial
#NeedToBeModified4RAM#    for MEM in $MEM_list ; do
#NeedToBeModified4RAM#      tMEM=$((MEM - 1))
#NeedToBeModified4RAM#      MEM=$( printf %06i $MEM )
#NeedToBeModified4RAM#  #   check=`expr $tMEM / $PPN`
#NeedToBeModified4RAM#  #   NODE_NUM=`expr $check % $NNODE + 1`
#NeedToBeModified4RAM#      check=$((tMEM / PPN))
#NeedToBeModified4RAM#      NODE_NUM=$((check % NNODE + 1))
#NeedToBeModified4RAM#      n=`echo $NODE_list | cut -d ' ' -f $NODE_NUM`
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#      mkdir -p $n/anal/$MEM
#NeedToBeModified4RAM#      mkdir -p $n/anal_f/$MEM
#NeedToBeModified4RAM#      mkdir -p $n/gues/$MEM
#NeedToBeModified4RAM#      echo " >> MEMBER:$MEM for $n ($NODE_NUM)"
#NeedToBeModified4RAM#      cp $OUTPUT/gues/$MEM/$SYYYY$SMM$SDD$SHH.grd $n/gues/$MEM
#NeedToBeModified4RAM#    done
#NeedToBeModified4RAM#  for n in $NODE_list ; do
#NeedToBeModified4RAM#    cd $n
#NeedToBeModified4RAM#    tar cf ../gues_$n.tar anal anal_f gues
#NeedToBeModified4RAM#    cd ../
#NeedToBeModified4RAM#  done
#NeedToBeModified4RAM#  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "  initial ended:     $CTIME" >> $TIMELOG
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#  # Compress inputs
#NeedToBeModified4RAM#  echo " >> INPUTs"
#NeedToBeModified4RAM#  cp $CLIMAX/common/orography_x64y32.grd orography_x64y32.grd
#NeedToBeModified4RAM#  cp $CLIMAX/common/orography_x64y32.ctl orography_x64y32.ctl
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/orog_lsm_alb.t30.grd         fort.20
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/sst_8190clim.t30.sea.grd     fort.21
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/seaice_8190clim.t30.sea.grd  fort.22
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/skt_8190clim.t30.land.grd    fort.23
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/sndep_8190clim.t30.land.grd  fort.24
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/veget.t30.land.grd           fort.25
#NeedToBeModified4RAM#  #CLIMAX# cp $SB/soilw_8190clim.t30.land.grd  fort.26
#NeedToBeModified4RAM#  #CLIMAX# cp $SC/sst_anom_7990.t30.grd        fort.30
#NeedToBeModified4RAM#  tar cf other.tar orography_x64y32.grd  fort.??
#NeedToBeModified4RAM#  pwd
#NeedToBeModified4RAM#  echo $LUTNAM
#NeedToBeModified4RAM#  if [ "$WINSMP" -ne "1" ]; then
#NeedToBeModified4RAM#    echo "copying???"
#NeedToBeModified4RAM#    cp $LUTNAM                        BILINLUT.bin
#NeedToBeModified4RAM#    tar rf other.tar                  BILINLUT.bin
#NeedToBeModified4RAM#  fi
#NeedToBeModified4RAM#  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "  copy ended:        $CTIME" >> $TIMELOG
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#  # Compress obs
#NeedToBeModified4RAM#  echo " >> OBSERVATION"
#NeedToBeModified4RAM#  mkdir obs
#NeedToBeModified4RAM#  IYMD=$SYYYY$SMM$SDD
#NeedToBeModified4RAM#  while test $IYMD -le $EYYYY$EMM$EDD ; do
#NeedToBeModified4RAM#    for n in $NODE_list ; do
#NeedToBeModified4RAM#      cp $CLIMAX/DATA/$SETOBS/obs/$IYMD??.dat obs
#NeedToBeModified4RAM#    done
#NeedToBeModified4RAM#    IYMD=`date -d "$IYMD 1 day" +%Y%m%d`
#NeedToBeModified4RAM#  done
#NeedToBeModified4RAM#  tar rf other.tar obs
#NeedToBeModified4RAM#  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "  observation ended:$CTIME" >> $TIMELOG
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#  # Compress exe
#NeedToBeModified4RAM#  echo " >> EXE_files"
#NeedToBeModified4RAM#  cp ../*.exe          .
#NeedToBeModified4RAM#  cp ../obsope_MPI.s01 .
#NeedToBeModified4RAM#  cp ../nodefile_letkf .
#NeedToBeModified4RAM#  tar rf other.tar *.exe obsope_MPI.s01 nodefile_letkf
#NeedToBeModified4RAM#  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "  exe files ended:  $CTIME" >> $TIMELOG
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#  # STAGE IN
#NeedToBeModified4RAM#  echo '>>>'
#NeedToBeModified4RAM#  echo '>>> STAGE IN'
#NeedToBeModified4RAM#  IYMD=$SYYYY$SMM$SDD
#NeedToBeModified4RAM#  PYMDH=`date -d "$IYMD $IHH:00 6 hour ago" +%Y%m%d%H`
#NeedToBeModified4RAM#  NODE_NUM=1
#NeedToBeModified4RAM#  for n in $NODE_list ; do
#NeedToBeModified4RAM#    n=`echo $NODE_list | cut -d ' ' -f $NODE_NUM`
#NeedToBeModified4RAM#    echo " >> STGIN for $n"
#NeedToBeModified4RAM#    ssh $n "mkdir -p $TMPDIR"
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#    # mkdir
#NeedToBeModified4RAM#    if test $NODE_NUM -eq 1 ; then
#NeedToBeModified4RAM#      ssh $n "mkdir -p $TMPDIR/infl_mul"
#NeedToBeModified4RAM#      ssh $n "mkdir -p $TMPDIR/peff_lpf"
#NeedToBeModified4RAM#      ssh $n "mkdir -p $TMPDIR/rsmp_lpf"
#NeedToBeModified4RAM#      ssh $n "mkdir -p $TMPDIR/log"
#NeedToBeModified4RAM#      ssh $n "mkdir -p $TMPDIR/wvec" # SK
#NeedToBeModified4RAM#      for MEM in mean sprd ; do
#NeedToBeModified4RAM#        ssh $n "mkdir -p $TMPDIR/anal/$MEM"
#NeedToBeModified4RAM#        ssh $n "mkdir -p $TMPDIR/anal_f/$MEM"
#NeedToBeModified4RAM#        ssh $n "mkdir -p $TMPDIR/gues/$MEM"
#NeedToBeModified4RAM#      done
#NeedToBeModified4RAM#      ssh $n "mkdir -p $TMPDIR/gues/kld"
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#      # Inflation
#NeedToBeModified4RAM#      if test -e $OUTPUT/infl_mul/$PYMDH.grd ; then
#NeedToBeModified4RAM#        echo "  > INFLATION"
#NeedToBeModified4RAM#        scp $OUTPUT/infl_mul/$PYMDH.grd  $n:$TMPDIR/infl_mul
#NeedToBeModified4RAM#      fi
#NeedToBeModified4RAM#      # Local PF
#NeedToBeModified4RAM#      if test -e $OUTPUT/peff_lpf/$PYMDH.grd ; then
#NeedToBeModified4RAM#        scp $OUTPUT/peff_lpf/$PYMDH.grd  $n:$TMPDIR/peff_lpf
#NeedToBeModified4RAM#      fi
#NeedToBeModified4RAM#      if test -e $OUTPUT/rsmp_lpf/$PYMDH.grd ; then
#NeedToBeModified4RAM#        scp $OUTPUT/rsmp_lpf/$PYMDH.grd  $n:$TMPDIR/rsmp_lpf
#NeedToBeModified4RAM#      fi
#NeedToBeModified4RAM#    fi
#NeedToBeModified4RAM#    # gues & inputs
#NeedToBeModified4RAM#    scp $TMPDIR/tmp_stgin/gues_$n.tar $n:$TMPDIR
#NeedToBeModified4RAM#    scp $TMPDIR/tmp_stgin/other.tar   $n:$TMPDIR
#NeedToBeModified4RAM#    ssh $n "cd $TMPDIR ; tar xf gues_$n.tar ; tar xf other.tar ; rm *.tar"
#NeedToBeModified4RAM#    CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "  STGIN($n) ended: $CTIME" >> $TIMELOG
#NeedToBeModified4RAM#
#NeedToBeModified4RAM#    NODE_NUM=$((NODE_NUM + 1))
#NeedToBeModified4RAM#  done
#NeedToBeModified4RAM#  cd $TMPDIR
fi
get_ELAPS
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "Settings ended: $CTIME, $ELAPS (s)" >> $TIMELOG

#
# Cycle run ### MAIN LOOP ========================================>>> (str) 
#
PBS_NODEFILE="./nodefile_letkf"
while test $IYYYY$IMM$IDD$IHH -le $EYYYY$EMM$EDD$EHH ; do
echo '>>>'
echo ">>> BEGIN COMPUTATION OF $IYYYY/$IMM/$IDD/$IHH"
echo '>>>'
  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "DATE of Current run : $IYYYY/$IMM/$IDD ${IHH}:00 at $CTIME" >> $TIMELOG
  CYMDH=$IYYYY$IMM$IDD$IHH
  TYMDH=`date -d "$IYYYY$IMM$IDD $IHH:00 6 hour" +%Y%m%d%H`
  PYMDH=`date -d "$IYYYY$IMM$IDD $IHH:00 6 hour ago" +%Y%m%d%H`
  TY=`echo $TYMDH | cut -c1-4` ; TM=`echo $TYMDH | cut -c5-6`
  TD=`echo $TYMDH | cut -c7-8` ; TH=`echo $TYMDH | cut -c9-10`

########################## (0) prep. configure file
if [ -e letkf.cnf ] ; then rm letkf.cnf ; fi
cat >> letkf.cnf << EOF
&letkf_param
  sigma_obs    = ${SETHSG}.0d3,
  nbv          = ${SETMEM},
  pymdh        = $PYMDH,
  ymdh         = $IYYYY$IMM$IDD$IHH,
  logic_wout   = .false.,
  logic_wsth   = ${LOGIC_WINSTH},
  logic_wint   = ${LOGIC_WININT},
  dastype      = $SETDAS,
  cov_infl_mul = $SETINF,
  s_additive   = $SETADD,
  hpo_weight   = $SETPOW,
  resample_m   = $LPFPEF,
  type_wnorm   = $LGMNRM,
  type_pfmtx   = $LPFRSM,
  type_relax   = $SETRTP,
  alph_relax   = $SETALP,
  fgt_factor   = $LPFFGT,
  tau_nmlize   = $LPFTAU,
  snk_lambda   = ${LPFLMD}d0,
  gamma_gmpf   = ${LGMFAC}d0,  
/
EOF

########################## (1) Obs Operator
echo "  > (1) OBSOPE"
set_START
#./$OBSOPE > obsope.log
#SINGULARITY#$MPIRUN -np $NODE ./$OBSOPE
singularity run --bind $CDIR oneapi-hpckit_latest.sif $MPIRUN -np $NODE ./$OBSOPE
get_ELAPS
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "OBSOPE : $CTIME, $ELAPS (s)" >> $TIMELOG

########################## (2) LETKF
echo "  > (2) LETKF"
set_START
#mpirun -np $NODE ./$LETKF #< /dev/null
#SINGULARITY#$MPIRUN -np $NODE ./$LETKF
singularity run --bind $CDIR oneapi-hpckit_latest.sif $MPIRUN -np $NODE ./$LETKF
tail -n 26 NOUT-000000
mv NOUT-000000 $TMPDIR/log/$IYYYY$IMM$IDD$IHH.log
get_ELAPS
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "LETKF  : $CTIME, $ELAPS (s)" >> $TIMELOG

########################## (3) Ensemble Diagnosis
echo "  > (3) Ensemble Diagnosis"
set_START
for FGAN in "gues" "anal" ; do
  LOGIC_MSPR=".false." # already done by letkf
  sh 120_ensdiagnosis-XXXXXX.sh $CYMDH $SETMEM $FGAN $TRUDIR $LOGIC_MSPR $LOGIC_VERIFY $ENSDG
done
get_ELAPS
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "ENSdiag: $CTIME, $ELAPS (s)" >> $TIMELOG

########################## (4) Ensemble Forecasts
echo "  > (4) Ensemble Forecasts by $SETNWP with $NODE procs"
set_START
if [   $SETNWP = "SPEEDY" ]; then
    sh 101_ensfcstMPI-speedy.sh $CYMDH $SETMEM $NODE
  if [ ${EXTFCST_STEP} -ge 1 ] ; then
    sh 102_extfcstMPI-speedy.sh $CYMDH $SETMEM $NODE $EXTFCST_STEP $NOSAVEENS $TRUDIR ${LOGIC_VERIFY} $ENSDG 
  fi
elif [ $SETNWP = "CLIMAX" ]; then
  sh 111_ensfcst-climax.sh      $CYMDH $SETMEM       $EXTFCST_STEP $NOSAVEENS $TRUDIR ${LOGIC_VERIFY} $ENSDG
    # extended fcst is employed in the script
fi
get_ELAPS
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "FCST   : $CTIME, $ELAPS (s)" >> $TIMELOG

#
# Clean up
#
########################## (4) Clean up
if test $NOSAVEENS -eq 1 ; then
  if test `echo $PYMDH | cut -c7-10` != "0100" ; then
    echo "  going  to delete ensemble for :: " $PYMDH
    rm -f $TMPDIR/gues/??????/${PYMDH}.grd
    rm -f $TMPDIR/anal/??????/${PYMDH}.grd
    if [   $SETNWP = "SPEEDY" ]; then 
      rm -f $TMPDIR/gues/??????/${PYMDH}_p.grd
      rm -f $TMPDIR/anal_f/??????/${PYMDH}.grd
      rm -f $TMPDIR/anal_f/??????/${PYMDH}_p.grd
    fi
  fi
fi # NOSAVEENS
#
# Date change ### MAIN LOOP END ###
#
IYYYY=$TY ; IMM=$TM ; IDD=$TD ; IHH=$TH
done
wait
#
# Cycle run ### MAIN LOOP ========================================>>> (end) 
#
# STAGE OUT
#
if test $DISK -eq 1 ; then
  get_ELAPS
  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "STGOUTs: $CTIME, $ELAPS (s)" >> $TIMELOG

  echo '>>>'
  echo '>>> STAGE OUT'
  NODE_NUM=1
  for n in $NODE_list ; do
    DIRLIST_STGOUT="gues anal anal_f infl_mul log wvec peff_lpf rsmp_lpf"
    echo " >> STGOUT for $n"
    if test $NODE_NUM -eq 1 ; then
#      ssh $n "cd $TMPDIR ; tar cf stgout.tar gues anal anal_f infl_mul log wvec"
#      scp $n:$TMPDIR/stgout.tar $OUTPUT/stgout_$n.tar
      for dirname in ${DIRLIST_STGOUT} ; do
        echo " >> STGOUT for $n $dirname"
        ssh $n "cd $TMPDIR ; tar cf stgout_${dirname}.tar ${dirname}"
        scp $n:$TMPDIR/stgout_${dirname}.tar $OUTPUT/stgout_${dirname}_$n.tar
        rm stgout_${dirname}.tar
      done
#    else
#      ssh $n "cd $TMPDIR ; tar cf stgout.tar gues anal anal_f infl_mul log wvec"
#      scp $n:$TMPDIR/stgout.tar $OUTPUT/stgout_$n.tar
    fi
    NODE_NUM=$((NODE_NUM + 1))
  done

  get_ELAPS
  CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "TAROUTs: $CTIME, $ELAPS (s)" >> $TIMELOG
  cd $OUTPUT
  for n in $NODE_list ; do
#    tar xf stgout_$n.tar &
    for dirname in ${DIRLIST_STGOUT} ; do
      tar xf stgout_${dirname}_$n.tar &
    done
  done

# CLEAR MEMORY SPACE on COMPUTATINAL NODES
  echo '>>>'
  echo '>>> CLEAR MEMORY SPACE'
  for n in $NODE_list ; do
    ssh $n "rm -fr $TMPDIR" &
  done
  wait

  for n in $NODE_list ; do
    rm -f stgout_$n.tar &
  done
fi

if test $DISK -eq 0 ; then
  #rm -fr $CLIMAX/DATA/tmp_${SETOBS}_$EXP
  echo "skip to remove tmpdir"
elif test $DISK -eq 1 ; then
  rm -fr /dev/shm/kotsuki_${SETOBS}_${EXP}
fi
wait


rm -fr $CDIR/nodefile/$EXP
date +"%Y/%m/%d %H:%M:%S"
CTIME=$(date +"%Y/%m/%d %H:%M:%S") ; echo "Job ended at $CTIME" >> $TIMELOG
echo "NORMAL END :: OBS & EXP :: " $SETOBS $EXP

####
echo "end all in jobs"
exit
