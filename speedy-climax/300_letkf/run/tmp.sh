#! /bin/bash -x

SYS="SYS20240725_" # SRUN (merged)
SYS="SYS20240727_" # SRUN (merged, options)
SYS="SYS20240802_" # EnsMean & Verification
SYS="SYS20240805_" # EnsMean & Verification (CRPS)

#===> CLIMAX ENVIRONMENT
CLIMAXDIR=/data10/kotsuki/climax-letkf/climax/v20240807
CLIMAXVER=def # 1. def, 2. 6x4, 3: res
CLIMAXVER=res # 1. def, 2. 6x4, 3: res
WBCLIMDIR=/data10/kotsuki/climax-letkf/climax/climate/climate/2006-2015

#===> DISC ENVIRONMENT (RAM is not supported, as of 20240730)
DISK=0             # 0:HDD/SSD, 1:RAM

#===> DA  OPTION (fixed)
RESTART_MODE="0"      # 0: start from initens (default)
                      # 1: start from existing directory
EXTFCST_STEP="20"      # 0: no extended ensemble forecast  (default)
                      # N: do extended ensemble forecast N steps
VERIFY__MODE="1"      # 0: no verification 
                      # 1: w/ verification in ensdiagnosis (vs. nature or weather bench) (default)

#===> DA  OPTION (fixed)
#=> SPEEDY
SETNWP_MASTER="SPEEDY" # CLIMAX or SPEEDY
SETOBS_MASTER="raob_sp"
SETMEM_MASTER="10" ; SETHSG_MASTER="500"   # Kotsuki and Bishop (2022; MWR)
SETMEM_MASTER="16" ; SETHSG_MASTER="700"   # Kotsuki +          (2020; QJRMS)
SETMEM_MASTER="20" ; SETHSG_MASTER="900"   # Kotsuki and Bishop (2022; MWR)
SETMEM_MASTER="40" ; SETHSG_MASTER="1200"  # Kotsuki and Bishop (2022; MWR)
SDATE=1982010100 ### initial date setting
EDATE=1982070100 ### final   date setting
#> an example for speedy
RESTART_MODE="0" ; SETMEM_MASTER="20" ; SETHSG_MASTER="900"  ; SDATE=1982010100 ; EDATE=1982070100
RESTART_MODE="0" ; SETMEM_MASTER="20" ; SETHSG_MASTER="900"  ; SDATE=1982010100 ; EDATE=1982010106 
RESTART_MODE="1" ; SETMEM_MASTER="20" ; SETHSG_MASTER="900"  ; SDATE=1982070100 ; EDATE=1982070118  

#=> CLIMAX
SETNWP_MASTER="CLIMAX" # CLIMAX or SPEEDY
SETOBS_MASTER="raob_wb"
SETMEM_MASTER="20" ; SETHSG_MASTER="500 600 700 800 900 1000"
SDATE=2017010100 ### initial date setting
EDATE=2017070100 ### final   date setting
#> an example for climax
RESTART_MODE="0" ; SETMEM_MASTER="20" ; SETHSG_MASTER="400 500 600 700 800 900" ; SDATE=2017010100 ; EDATE=2018010100
RESTART_MODE="0" ; SETMEM_MASTER="20" ; SETHSG_MASTER="600" ; SDATE=2017010100 ; EDATE=2017010100

#===> DATA ASSIMILATION OPTIONS (tunable)
SETDAS_MASTER="0" 
  # 0 :: LETKF    ;                    mean update :: LETKF,   ptb update :: LETKF (default)
  # 1 :: DEBUG    ;                    mean update :: LPF,     ptb update :: LETKF (just for debug)
  # 2 :: LAPF     ;                    mean update :: LPF  ,   ptb update :: LAPF      resampling
  # 3 :: LPFGM1   ; LETKF_particle  +  mean update :: N/A  ,   ptb update :: LPFGM w   resampling

SETRTP_MASTER="0"    # 0: no relaxation (def. for letkf) ;  1: RTPP  ;  2:RTPS (def. for lpfs)
SETALP_MASTER="0.00" # relaxation parameter alpha

#===> WI OPTIONS for the LETKF (fixed, currently not supported)
WINSTH_MASTER="0"       # 0: no weight smoothing (default), 1: weight "smoothing"
WINSMP_MASTER="1"       # sampling interval for weight "interpolation"
WINLUT_MASTER="BLLUT"   # Look up table     for weight "interpolation"

#===> LPF OPTIONS (usually fixed)
LPFRSM_MASTER="1"    # 0: shared resampling noise (Potthast+2019; MWR)
                     # 1: Monte-Carlo Resampling (repeat M-times) , Kotsuki+2023; GMD)
                     #   (This parameter is currently used only for SETDAS 2 or 3)
LPFPEF_MASTER="40"   # effective particle size for resampling (SIR)
                     # when=SETMEM --> always resampling, when=0 --> no resampling
LPFFGT_MASTER="0.00" # 0.00: no forget, 1.00: perfect forget (default for LPFGM of raob)
LGMFAC_MASTER=2.5    # (usually fixed) factor of background error covariance (only for SETDAS=3 or 4)
LGMNRM_MASTER="0"    # 0: (R      )-1 :: classical PF
                     # 1: (R+HPfHT)-1 :: Gaussian Mixture (Hoteit+2008, Stordal+2011)

####################################################### examples of specific experiental settings
##(1) REG2 & RAOB EXP FOR LPFGM##
SETDAS_MASTER="3"
LGMNRM_MASTER="0 1"
LGMFAC_MASTER="2.5 1.5 0.5"
LPFFGT_MASTER="1.00 0.00"      # 0.00: no forget, 1.00: perfect forget (default for LPFGM of raob)
LPFPEF_MASTER="2 5 10 40"      # effective particle size for resampling (SIR)
SETALP_MASTER="0.00 0.20 0.40 0.60 0.80 1.00" # RTPP ; Relaxation to Prior
                          # RTPP ==> RTPS
                          # 0: no relaxation   1: perfect relaxation i.e., dXa=dXb
                          # forced to be 0.00 if
LPFFGT_MASTER="1.00 0.00"
SETALP_MASTER="0.00 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00"

LGMNRM_MASTER="1"
LPFFGT_MASTER="1.00 0.00"
LPFPEF_MASTER="40"

###2LAPF w/o forgetting##
#SETDAS_MASTER="2"
#LGMNRM_MASTER="0"
#LGMFAC_MASTER="2.5"
##SETHSG_MASTER="600 500 400 300" # reg2
#SETHSG_MASTER="900 800 700 600"  # raob
#LPFPEF_MASTER="40"
#LPFFGT_MASTER="0.00"
#SETALP_MASTER="0.80 0.85 0.90 0.95 1.00 1.05 1.10 1.15 1.20"

##3LAPF w/o forgetting##
#3RD#LGMNRM_MASTER="0"
#3RD#SETDAS_MASTER="3"
#3RD#SETHSG_MASTER="800"
#3RD#LPFPEF_MASTER="40"
#3RD#LPFFGT_MASTER="0.00"
#3RD#LGMFAC_MASTER="2.5 1.5 1.0"
#3RD#SETALP_MASTER="0.80 0.85 0.90 0.95 1.00 1.05 1.10 1.15 1.20"

##LETKF w/o forgetting##
LGMNRM_MASTER="0"
SETDAS_MASTER="0"
#SETHSG_MASTER="400 500 600 700 800 900 1000 1100 1200 1300" # raob
LPFPEF_MASTER="20"
LPFFGT_MASTER="0.00"
LGMFAC_MASTER="2.5"
SETALP_MASTER="0.00"
SETRTP_MASTER="0"

#DEBUG#
#DBG#SETDAS_MASTER="3" 
#DBG#LGMNRM_MASTER="1"
#DBG#LPFFGT_MASTER="0.00"
#DBG#LPFPEF_MASTER="40"
#DBG#SETALP_MASTER="1.00"

#------------------------------------------------------------------------------------------------# no need to change
#------------------------------------------------------------------------------------------------# general
for SETDAS   in ${SETDAS_MASTER}  ; do
for SETNWP   in ${SETNWP_MASTER}  ; do
for SETMEM   in ${SETMEM_MASTER}  ; do
for SETOBS   in ${SETOBS_MASTER}  ; do
for SETHSG   in ${SETHSG_MASTER}  ; do
for SETRTP   in ${SETRTP_MASTER}  ; do
for SETALP   in ${SETALP_MASTER}  ; do
#------------------------------------------------------------------------------------------------# weight interpolation
for WINSMP   in ${WINSMP_MASTER}  ; do
for WINLUT   in ${WINLUT_MASTER}  ; do
for WINSTH   in ${WINSTH_MASTER}  ; do
#------------------------------------------------------------------------------------------------# particle filters
for LPFRSM   in ${LPFRSM_MASTER}  ; do
for LPFFGT   in ${LPFFGT_MASTER}  ; do
for LPFPEF   in ${LPFPEF_MASTER}  ; do
for LGMFAC   in ${LGMFAC_MASTER}  ; do
for LGMNRM   in ${LGMNRM_MASTER}  ; do

#------------------------------------------------------------------------------------------------# computational time
  ET="08"
  if [ $LGMNRM -eq 0 ] ; then ET="04" ; fi
  if [ $LGMNRM -eq 1 ] ; then ET="09" ; fi
  ET=12

  if [ $SETMEM -eq 10 ] ; then NODE=10  ; fi
  if [ $SETMEM -eq 16 ] ; then NODE=16  ; fi
  if [ $SETMEM -eq 20 ] ; then NODE=20  ; fi
  if [ $SETMEM -eq 32 ] ; then NODE=32  ; fi
  if [ $SETMEM -eq 40 ] ; then NODE=40  ; fi
  if [ $SETMEM -eq 64 ] ; then NODE=64  ; fi
  if [ $SETMEM -eq 80 ] ; then NODE=80  ; fi

#------------------------------------------------------------------------------------------------# experimental name 
M=$( printf %06i ${SETMEM} )
H=$( printf %04i ${SETHSG} )

#===> DAS NAME
                            DASNAME="NULL___"
if [ $SETDAS -eq 0 ] ; then DASNAME="0LETKF_" ; fi
if [ $SETDAS -eq 1 ] ; then DASNAME="1LPFck_" ; fi
if [ $SETDAS -eq 2 ] ; then DASNAME="2LAPF__" ; fi
if [ $SETDAS -eq 3 ] ; then DASNAME="3LPFGM_" ; fi

#===> weight interpolation
CWINSMP=$( printf %02i ${WINSMP} )
if [ "$WINSMP" = "1" ]; then
  CURRENT_TEST=''
else 
  CURRENT_TEST='_'$WINLUT$CWINSMP
fi

#===> relaxation
if [ $SETRTP = "0" ] ; then CURRENT_TEST=${CURRENT_TEST}'_RTPx'${SETALP} ; fi
if [ $SETRTP = "1" ] ; then CURRENT_TEST=${CURRENT_TEST}'_RTPP'${SETALP} ; fi
if [ $SETRTP = "2" ] ; then CURRENT_TEST=${CURRENT_TEST}'_RTPS'${SETALP} ; fi  

#===> weigt smoother
if [ "$WINSTH" = "0" ]; then CURRENT_TEST=${CURRENT_TEST}'_woWSM' ; fi
if [ "$WINSTH" = "1" ]; then CURRENT_TEST=${CURRENT_TEST}'_wwWSM' ; fi

#===> LPF 
if [ $SETDAS -ge 2 ]; then
  LPFPEF=$( printf %03i ${LPFPEF} ) # may be unnecessary (SK 20240730)
  if [ "$LPFRSM" = "0" ]; then CURRENT_TEST=${CURRENT_TEST}'_infRP' ; fi # resampling
  if [ "$LPFRSM" = "1" ]; then CURRENT_TEST=${CURRENT_TEST}'_infKK' ; fi # resampling
  if [ "$LPFRSM" = "2" ]; then CURRENT_TEST=${CURRENT_TEST}'_infMP' ; fi # resampling
  if [ "$LGMNRM" = "0" ]; then CURRENT_TEST=${CURRENT_TEST}'_nrmPF' ; fi # norm of GM
  if [ "$LGMNRM" = "1" ]; then CURRENT_TEST=${CURRENT_TEST}'_nrmGM' ; fi # norm of GM
  CURRENT_TEST=${CURRENT_TEST}'_fac'${LGMFAC}                            # gamma of LPFGM
  CURRENT_TEST=${CURRENT_TEST}'_fgt'${LPFFGT}                            # forgetting factor      
  CURRENT_TEST=${CURRENT_TEST}'_rsmp'${LPFPEF}                           # resampling parameter
fi

if [   $SETNWP = "SPEEDY" ]; then
  EXP=${SYS}${SETNWP}def"-"${DASNAME}M${M}L${H}IADP${CURRENT_TEST}
elif [ $SETNWP = "CLIMAX" ]; then
  EXP=${SYS}${SETNWP}${CLIMAXVER}"-"${DASNAME}M${M}L${H}IADP${CURRENT_TEST}
fi

#===> 
if test $DISK -eq 0 ; then
  LOGNAM="${SETOBS}_${EXP}_NODE${NODE}_HDD"
elif test $DISK -eq 1 ; then
  LOGNAM="${SETOBS}_${EXP}_NODE${NODE}_RAM"
fi

#================================================================================================================================>>> STR LOOP
if [ -e tmp_body.sh ] ; then rm tmp_body.sh ; fi
touch tmp_body.sh
echo "#! /bin/bash -x"                                                             >> tmp_body.sh
#SAVE4JSS3#echo "#"                                                                           >> tmp_body.sh
#SAVE4JSS3#echo "# for FX100(JSS SORA)"                                                       >> tmp_body.sh
#SAVE4JSS3#echo "#"                                                                           >> tmp_body.sh
#SAVE4JSS3#echo "#JX --bizcode R0201"                                                         >> tmp_body.sh
#SAVE4JSS3##echo "#JX --usecode SP"                                                            >> tmp_body.sh
#SAVE4JSS3#echo "#JX -L rscunit=SORA"                                                           >> tmp_body.sh
#SAVE4JSS3#echo "#JX -L node=10"                                                              >> tmp_body.sh
#SAVE4JSS3#echo "#JX --mpi proc=40"                                                           >> tmp_body.sh
#SAVE4JSS3#echo "#JX -L node-mem=24Gi"                                                        >> tmp_body.sh
#SAVE4JSS3##echo "#JX -L 'elapse=08:00:00'"                                                    >> tmp_body.sh
#SAVE4JSS3##echo "#JX -L 'elapse=00:10:00'"                                                    >> tmp_body.sh
#SAVE4JSS3#echo "#JX -L 'elapse=${ET}:00:00'"                                                    >> tmp_body.sh
#SAVE4JSS3#echo "#JX -j"                                                                      >> tmp_body.sh
#SAVE4JSS3#echo "#JX -s"                                                                      >> tmp_body.sh
echo "export MPIRUN=mpiexec"                                                       >> tmp_body.sh
echo ""                                                                            >> tmp_body.sh
#SAVE4JSS3#echo "PARALLEL=8 ;export PARALLEL"                                                 >> tmp_body.sh
#SAVE4JSS3#echo "export OMP_NUM_THREADS=8"                                                    >> tmp_body.sh
#SAVE4JSS3#echo "export XOS_MMM_L_ARENA_FREE=2"                                               >> tmp_body.sh
#SAVE4JSS3#echo "export FORT90L='-Wl,-T'"                                                     >> tmp_body.sh
echo ""                                                                            >> tmp_body.sh
echo "#----------------------INITIALIZE PARAMETERS-----------------------------#"  >> tmp_body.sh
echo "set -e"                                                                      >> tmp_body.sh
echo "export NODE_list='n01'"                                                      >> tmp_body.sh
echo "export NODE="$NODE "                  #Nodes for parallel computing"         >> tmp_body.sh
echo "export CDIR=`pwd`"                                                           >> tmp_body.sh
echo "LUTDIR='$CDIR/../../900_LTERPLUT-speedy/output/'"                            >> tmp_body.sh
echo "export CLIMAXDIR='$CLIMAXDIR'"                                               >> tmp_body.sh
echo "export CLIMAXVER='$CLIMAXVER'"                                               >> tmp_body.sh
echo "export WBCLIMDIR='$WBCLIMDIR'"                                               >> tmp_body.sh
echo "export WBCLIMDIR='$WBCLIMDIR'"                                               >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh
echo "DISK="$DISK                                                                  >> tmp_body.sh
echo "SYS='"$SYS"'"                                                                >> tmp_body.sh
echo "EXP='"$EXP"'"                                                                >> tmp_body.sh
echo "LOGNAM='"$LOGNAM"'"                                                          >> tmp_body.sh
echo "RESTART_MODE="${RESTART_MODE}                                                >> tmp_body.sh
echo "VERIFY__MODE="${VERIFY__MODE}                                                >> tmp_body.sh
echo "EXTFCST_STEP="${EXTFCST_STEP}                                                >> tmp_body.sh
echo "SETDAS='"$SETDAS"'"                                                          >> tmp_body.sh
echo "SETNWP='"$SETNWP"'"                                                          >> tmp_body.sh
echo "SETMEM='"$SETMEM"'"                                                          >> tmp_body.sh
echo "SETOBS='"$SETOBS"'"                                                          >> tmp_body.sh
echo "SETHSG='"$SETHSG"'"                                                          >> tmp_body.sh
echo "SETRTP='"$SETRTP"'"                                                          >> tmp_body.sh
echo "SETALP='"$SETALP"'"                                                          >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh
echo "WINSMP='"$WINSMP"'"                                                          >> tmp_body.sh
echo "WINLUT='"$WINLUT"'"                                                          >> tmp_body.sh
echo "WINSTH='"$WINSTH"'"                                                          >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh
echo "LPFRSM='"$LPFRSM"'"                                                          >> tmp_body.sh
echo "LPFFGT='"$LPFFGT"'"                                                          >> tmp_body.sh
echo "LPFPEF="$LPFPEF                                                              >> tmp_body.sh
echo "LGMFAC="$LGMFAC                                                              >> tmp_body.sh
echo "LGMNRM='"$LGMNRM"'"                                                          >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh
echo "SYYYY="`echo $SDATE | cut -c1-4`                                             >> tmp_body.sh
echo "SMM="`echo   $SDATE | cut -c5-6`                                             >> tmp_body.sh
echo "SDD="`echo   $SDATE | cut -c7-8`                                             >> tmp_body.sh
echo "SHH="`echo   $SDATE | cut -c9-10`                                            >> tmp_body.sh
echo "EYYYY="`echo $EDATE | cut -c1-4`                                             >> tmp_body.sh
echo "EMM="`echo   $EDATE | cut -c5-6`                                             >> tmp_body.sh
echo "EDD="`echo   $EDATE | cut -c7-8`                                             >> tmp_body.sh
echo "EHH="`echo   $EDATE | cut -c9-10`                                            >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh

echo "SUBMIT JOB "$LOGNAM
WDATE=`date +"%Y%m%d%H%M%S"`

RUNSCRIPT="333_run_"$LOGNAM".sh"

if [ $SETDAS -eq 2 -a $LGMNRM -eq 1 ] ; then 
  echo " skip since this norm is in appropriate for classical PF (SETDAS=2)"
else
  echo "do experiments"
  cat tmp_body.sh save_letkf_tail-XXXXXX.sh > $RUNSCRIPT
  #JSS3  #jxsub $RUNSCRIPT
  #DIRECT#sh $RUNSCRIPT

  # chiba-u. lab. setting
  chmod u+x $RUNSCRIPT
  #srun --chdir `pwd` -c $NODE --mem=127G -o J${LOGNAM}_${WDATE} -e J${LOGNAM}_${WDATE} ./$RUNSCRIPT &
  srun --chdir `pwd` -c $NODE --mem=63G -o J${LOGNAM}_${WDATE}.o -e J${LOGNAM}_${WDATE}.e ./$RUNSCRIPT &
  # mem :: climax efcst requires 8-32 GB for memory

  sleep 1
fi


done # LGMNRM
done # LGMFAC
done # LPFPEF
done # LPFFGT
done # LPFRSM
done # WINSTH
done # WINLUT
done # WINSMP
done # SETALP
done # SETRTP
done # SETHSG
done # SETOBS
done # SETMEM
done # SETNWP
done # SETDAS
#================================================================================================================================>>> END LOOP
echo "end submit jobs"
exit
