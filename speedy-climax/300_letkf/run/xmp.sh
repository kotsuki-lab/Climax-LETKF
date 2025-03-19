#! /bin/bash -x

SYS="V240927_" # baseline of v1.08
SYS="V240930_" # additive inflation
SYS="V250120_" # prepbufr implemented
SYS="V250122_" # prepbufr implemented'

#===> CLIMAX ENVIRONMENT
CLIMAXDIR=/data10/kotsuki/climax-letkf/climax/v20240807
CLIMAXVER=def # 1. def, 2. 6x4, 3: res, 4: int
#CLIMAXVER=int # 1. def, 2. 6x4, 3: res, 4: int
WBCLIMDIR=/data10/kotsuki/climax-letkf/climax/climate/climate/2006-2015

#===> DISC ENVIRONMENT (RAM is not supported, as of 20240730)
DISK=0             # 0:HDD/SSD, 1:RAM

#===> DATA ASSIMILATION OPTIONS (tunable)
SETDAS_MASTER="0" 
  # 0 :: LETKF    ;                    mean update :: LETKF,   ptb update :: LETKF (default)
  # 1 :: DEBUG    ;                    mean update :: LPF,     ptb update :: LETKF (just for debug)
  # 2 :: LAPF     ;                    mean update :: LPF  ,   ptb update :: LAPF      resampling
  # 3 :: LPFGM    ; LETKF_particle  +  mean update :: N/A  ,   ptb update :: LPFGM w/  resampling

SETPOW_MASTER="0.00"  # weight parameter of PO-LETKF (Tsuyuki 2024 JMSJ) w=0.0->LETKF, w=1.0-->PO
SETINF_MASTER="-1.01" # multiplicative inlfation factor (+: constant, -: adaptive)
                      # e.g.  1.00 : no inflation
                      #       1.01 : globally constant inflation
                      #      -1.01 : adaptive multiplicative inflation (Miyohsi 2011) w/ initial factor 1.01
SETRTP_MASTER="0"     # 0: no relaxation (def. for letkf)
                      # 1: Relaxation to Prior Perturbation (RTPP; Zhang et al. 2004)
                      # 2: Relaxation to Prior Spread       (RTPS; Whitaker and Hamill 2004) ; 
                      # 3: Adjustment to Analysis LETKF     (ATAL; for ensemble mean and ptb)
                      # 4: Adjustment to Analysis S-EnKF    (ATAE; for ensemble mean and ptb) 
                      # 5: Relaxation to Posterior LETKF    (RTPL; for ensemble ptb)
                      # 6: Relaxation to Posterior EnKF     (RTPE; for ensemble ptb) 
SETALP_MASTER="0.00"  # relaxation parameter alpha
SETADD_MASTER="0.00"  # PO-based additive inflation factor

#===> WI OPTIONS for the LETKF (fixed, currently not supported)
WINSTH_MASTER="0"       # 0: no weight smoothing (default), 1: weight "smoothing"
WINSMP_MASTER="1"       # sampling interval for weight "interpolation"
WINLUT_MASTER="BLLUT"   # Look up table     for weight "interpolation"

#===> LPF OPTIONS (usually fixed)
LPFRSM_MASTER="3"    # 0: Multinomial Resampling w/ shared noise   ; Potthast+2019 (MWR)
                     # 1: Monte-Carlo Resampling (repeat M-times)  ; Kotsuki+2022 (GMD)
                     # 2: Marginal particle filter (from textbook) ; not supported as of 20240904
                     # 3: Sinkhorn :: obs   space cost             ; Oishi and Kotsuki 2023 (SOLA) 
                     # 4: Sinkhorn :: state space cost             ; Oishi and Kotsuki 2023 (SOLA) 
                     #   (This parameter is currently used only for SETDAS 2 or 3)
LPFPEF_MASTER="40"   # effective particle size for resampling (SIR)
                     # when=SETMEM --> always resampling, when=0 --> no resampling
LPFTAU_MASTER="0.00" # tau inflation    :: 0.00->no infl. , 1.00: all weights=1/m
LPFFGT_MASTER="1.00" # forgetting factor:: 0.00->no forget, 1.00: perfect forget  (1.00 is default for LPFGM)
LPFLMD_MASTER="40"   # lambda of sinkhorn parameter (defalut=40, Oishi and Kotsuki 2023; SOLA)
                     #   note: too large lambda can cause numerical instability because of K=exp(-C*lambda)
LGMFAC_MASTER=1.5    # (usually fixed) factor of background error covariance (only for SETDAS=3 or 4)
LGMNRM_MASTER="0"    # 0: (R      )-1 :: classical PF     (default due to computational cost)
                     # 1: (R+HPfHT)-1 :: Gaussian Mixture (Hoteit+2008, Stordal+2011)

#######################################################################################################
#===> DA  OPTION (fixed)
RESTART_MODE="0"      # 0: start from initens (default)
                      # 1: start from existing directory
                      # 9: start from specified directory
EXTFCST_STEP="0"      # 0: no extended ensemble forecast  (default)
                      # N: do extended ensemble forecast N steps
VERIFY__MODE="1"      # 0: no verification 
                      # 1: w/ verification in ensdiagnosis (vs. nature or weather bench) (default)

#=> SPEEDY
######################################################################### LETKF's horizontal loc. scales
### MEMBER----RAOB----REG2----REG4 ####### Kotsuki+ 2020 (QJRMS)
#       16   700km   500km   600km
#       32   800km   500km   700km
#       64  1200km   600km   700km
### MEMBER----RAOB----REG2----REG4 ####### Kotsuki and Bishop 2022 (MWR)
#       10   500km
#       20   900km
#       40  1200km
### MEMBER----RAOB----REG2----REG4 ####### Kotsuki+2022 (GMD)
#       40  1200km   600km
######################################################################### 
SETNWP_MASTER="SPEEDY" # CLIMAX or SPEEDY
#SETOBS_MASTER="reg2_sp" ; SETMEM_MASTER="40" ; SETHSG_MASTER="600"   # Kotsuki+           (2022; GMD)
#SETOBS_MASTER="raob_sp" ; SETMEM_MASTER="20" ; SETHSG_MASTER="900"   # Kotsuki and Bishop (2022; MWR)
#SETOBS_MASTER="raob_sp" ; SETMEM_MASTER="40" ; SETHSG_MASTER="1200"  # Kotsuki and Bishop (2022; MWR)

#SDATE=1982010100 ### initial date setting
#EDATE=1982070100 ### final   date setting

#> an example for speedy
#RESTART_MODE="9"

######################## REG2
#SETOBS_MASTER="reg2_sp" ; SETMEM_MASTER="40" ; SETHSG_MASTER="600" ; SETADD_MASTER="0.00"
#SETDAS_MASTER="0" ; SDATE=1983010100 ; EDATE=1984010100 # LETKF
#SETDAS_MASTER="2" ; SDATE=1983010100 ; EDATE=1983070100 ; LPFLMD_MASTER="40" ; LPFTAU_MASTER="0.00" ; SETRTP_MASTER="0" ; SETALP_MASTER="0.00" ; LPFPEF_MASTER="40" ; LPFRSM_MASTER="3" ; SETADD_MASTER="0.25 0.75 1.25 1.75 2.25 2.75 3.25 3.50 3.75"

######################## RAOB
#SETOBS_MASTER="raob_sp" ; SETMEM_MASTER="40" ; SETHSG_MASTER="1200" ; SETADD_MASTER="0.00" # fixed
#RESTART_MODE="9" ; SETDAS_MASTER="0" ; SDATE=1983010100 ; EDATE=1984010100 #LETKF
#RESTART_MODE="9" ; SETDAS_MASTER="0" ; SDATE=1983010100 ; EDATE=1983083118 ; SETADD_MASTER="0.00 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00" # additive

#RESTART_MODE="1" ; SETDAS_MASTER="0" ; SDATE=1983090100 ; EDATE=1984033118 ; SETADD_MASTER="0.00 0.10 0.20 0.30 0.40 0.50 0.60 0.70 0.80 0.90 1.00" # additive

#LPFGM-KK
#RESTART_MODE="9" ; LPFLMD_MASTER="40" ; LPFTAU_MASTER="0.00"
#SETDAS_MASTER="3" ; SDATE=1983010100 ; EDATE=1984010100 ;  SETRTP_MASTER="2" ; SETALP_MASTER="0.60" ; LPFPEF_MASTER="0 2 5 10" ; LPFRSM_MASTER="1 3" ; SETADD_MASTER="0.00 0.20 0.40 0.60 0.80 1.00"
#SETDAS_MASTER="3" ; SDATE=1983010100 ; EDATE=1984010100 ;  SETRTP_MASTER="0" ; SETALP_MASTER="0.00" ; LPFPEF_MASTER="0 2 5 10" ; LPFRSM_MASTER="3" ; SETADD_MASTER="0.00 0.50 1.00 1.50 2.00 2.50"
#SETDAS_MASTER="3" ; SDATE=1983010100 ; EDATE=1984010100 ;  SETRTP_MASTER="2" ; SETALP_MASTER="0.60" ; LPFPEF_MASTER="0 2 5 10" ; LPFRSM_MASTER="3" ; SETADD_MASTER="0.00 0.50 1.00 1.50 2.00 2.50"

#===> DA  OPTION (fixed)
#=> CLIMAX
SETNWP_MASTER="CLIMAX" # CLIMAX or SPEEDY
SETOBS_MASTER="raob_wb"
SETOBS_MASTER="prepbufr_dnst1_sfc1_Tv12_n0"
SETMEM_MASTER="20" ; SETHSG_MASTER="500"
SDATE=2017010100 ### initial date setting
EDATE=2017070100 ### final   date setting
SDATE=2016010100 ### initial date setting
EDATE=2016070100 ### final   date setting
##> an example for climax
SETHSG_MASTER="600"
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" 
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40" 
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" 
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40" 
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2016070100 ; SETDAS_MASTER="3" ; SETMEM_MASTER="40" ; LPFRSM_MASTER="1"    ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40" ; LPFPEF_MASTER="0 2 5"
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2016070100 ; SETDAS_MASTER="3" ; SETMEM_MASTER="40" ; LPFRSM_MASTER="1"    ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" ; LPFPEF_MASTER="0 2 5"

#RESTART_MODE="1" ; SDATE=2017070100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00"
#RESTART_MODE="1" ; SDATE=2017070100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40"
#RESTART_MODE="1" ; SDATE=2017010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00"
#RESTART_MODE="1" ; SDATE=2017070100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00"
#RESTART_MODE="1" ; SDATE=2017110100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.80"
#RESTART_MODE="1" ; SDATE=2017070100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.90"
#RESTART_MODE="1" ; SDATE=2017050100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.75 0.85"
#RESTART_MODE="1" ; SDATE=2016060100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40"
#RESTART_MODE="1" ; SDATE=2016120100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40"
#RESTART_MODE="1" ; SDATE=2017030100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40"
RESTART_MODE="1" ; SDATE=2017090100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="40" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40"



#RESTART_MODE="1" ; SDATE=2016020100 ; EDATE=2016040100 ; SETDAS_MASTER="3" ; SETMEM_MASTER="40" ; LPFRSM_MASTER="1"    ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40" ; LPFPEF_MASTER="0"
#RESTART_MODE="1" ; SDATE=2016020100 ; EDATE=2016040100 ; SETDAS_MASTER="3" ; SETMEM_MASTER="40" ; LPFRSM_MASTER="1"    ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" ; LPFPEF_MASTER="0"

CLIMAXVER=int
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" 
#RESTART_MODE="0" ; SDATE=2016010100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40"
#RESTART_MODE="1" ; SDATE=2016090100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" 
#RESTART_MODE="1" ; SDATE=2017040100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="1" ; SETALP_MASTER="0.70 0.75 0.80 0.85 0.90 0.95 1.00" 
#RESTART_MODE="1" ; SDATE=2017040100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="0.80 0.90 1.00 1.10 1.20 1.30 1.40" 

RESTART_MODE="1" ; SDATE=2017110100 ; EDATE=2018010100 ; SETDAS_MASTER="0" ; SETMEM_MASTER="20" ; SETINF_MASTER="1.00" ; SETRTP_MASTER="2" ; SETALP_MASTER="1.10"

#------------------------------------------------------------------------------------------------# no need to change
#------------------------------------------------------------------------------------------------# general
for SETDAS   in ${SETDAS_MASTER}  ; do
for SETNWP   in ${SETNWP_MASTER}  ; do
for SETMEM   in ${SETMEM_MASTER}  ; do
for SETOBS   in ${SETOBS_MASTER}  ; do
for SETHSG   in ${SETHSG_MASTER}  ; do
for SETPOW   in ${SETPOW_MASTER}  ; do  
for SETRTP   in ${SETRTP_MASTER}  ; do
for SETALP   in ${SETALP_MASTER}  ; do
for SETADD   in ${SETADD_MASTER}  ; do
for SETINF   in ${SETINF_MASTER}  ; do
#------------------------------------------------------------------------------------------------# weight interpolation
for WINSMP   in ${WINSMP_MASTER}  ; do
for WINLUT   in ${WINLUT_MASTER}  ; do
for WINSTH   in ${WINSTH_MASTER}  ; do
#------------------------------------------------------------------------------------------------# particle filters
for LPFRSM   in ${LPFRSM_MASTER}  ; do
for LPFTAU   in ${LPFTAU_MASTER}  ; do
for LPFFGT   in ${LPFFGT_MASTER}  ; do
for LPFPEF   in ${LPFPEF_MASTER}  ; do
for LPFLMD   in ${LPFLMD_MASTER}  ; do
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
#  if [ $SETMEM -eq 20 ] ; then NODE=10  ; fi
  if [ $SETMEM -eq 32 ] ; then NODE=32  ; fi
  if [ $SETMEM -eq 40 ] ; then NODE=40  ; fi
#  if [ $SETMEM -eq 40 ] ; then NODE=10  ; fi # temporal
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

DASNAME=${DASNAME}M${M}L${H}

#===> multiplicative inlfation
DASNAME=${DASNAME}'_inf'${SETINF}

#===> relaxation
if [ $SETRTP = "0" ] ; then DASNAME=${DASNAME}'_RTPx'${SETALP} ; fi
if [ $SETRTP = "1" ] ; then DASNAME=${DASNAME}'_RTPP'${SETALP} ; fi
if [ $SETRTP = "2" ] ; then DASNAME=${DASNAME}'_RTPS'${SETALP} ; fi  
if [ $SETRTP = "3" ] ; then DASNAME=${DASNAME}'_ATAL'${SETALP} ; fi  
if [ $SETRTP = "4" ] ; then DASNAME=${DASNAME}'_ATAE'${SETALP} ; fi    
if [ $SETRTP = "5" ] ; then DASNAME=${DASNAME}'_RTPL'${SETALP} ; fi  
if [ $SETRTP = "6" ] ; then DASNAME=${DASNAME}'_RTPE'${SETALP} ; fi    

#===> additive inflation
DASNAME=${DASNAME}'_ad'${SETADD}

#===> hybrid parameter (PO-ETKF)
DASNAME=${DASNAME}'_po'${SETPOW}

#===> weight interpolation
#CWINSMP=$( printf %02i ${WINSMP} )
#if [ "$WINSMP" = "1" ]; then
#  CURRENT_TEST=''
#else 
#  CURRENT_TEST='_'$WINLUT$CWINSMP
#fi

#===> weigt smoother
#if [ "$WINSTH" = "0" ]; then CURRENT_TEST=${CURRENT_TEST}'_woWSM' ; fi
#if [ "$WINSTH" = "1" ]; then CURRENT_TEST=${CURRENT_TEST}'_wwWSM' ; fi

#===> LPF 
if [ $SETDAS -ge 2 ]; then
  LPFPEF_I3=$( printf %03i ${LPFPEF} )
  LPFLMD_I3=$( printf %03i ${LPFLMD} )
  
  if [ "$LPFRSM" = "0" ]; then DASNAME=${DASNAME}'_rmxRP' ; fi # resampling matrix
  if [ "$LPFRSM" = "1" ]; then DASNAME=${DASNAME}'_rmxKK' ; fi # resampling matrix
  if [ "$LPFRSM" = "2" ]; then DASNAME=${DASNAME}'_rmxMP' ; fi # resampling matrix
  if [ "$LPFRSM" = "3" ]; then DASNAME=${DASNAME}'_rmxSO' ; fi # resampling matrix
  if [ "$LPFRSM" = "4" ]; then DASNAME=${DASNAME}'_rmxSS' ; fi # resampling matrix

  if [ "$LGMNRM" = "0" ]; then DASNAME=${DASNAME}'_nrmPF' ; fi # norm of LPFGM
  if [ "$LGMNRM" = "1" ]; then DASNAME=${DASNAME}'_nrmGM' ; fi # norm of LPFGM

  DASNAME=${DASNAME}'_gma'${LGMFAC}                            # gamma of LPFGM
  DASNAME=${DASNAME}'_fgt'${LPFFGT}                            # forgetting factor      
  DASNAME=${DASNAME}'_tau'${LPFTAU}                            # forgetting factor  
  DASNAME=${DASNAME}'_nzo'${LPFPEF_I3}                         # resampling parameter (N0)
  DASNAME=${DASNAME}'_lmd'${LPFLMD_I3}                         # resampling parameter (N0)
fi

if [   $SETNWP = "SPEEDY" ]; then
  EXP=${SYS}${SETNWP}def"-"${DASNAME}
elif [ $SETNWP = "CLIMAX" ]; then
  EXP=${SYS}${SETNWP}${CLIMAXVER}"-"${DASNAME}
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
echo "SETPOW='"$SETPOW"'"                                                          >> tmp_body.sh
echo "SETRTP='"$SETRTP"'"                                                          >> tmp_body.sh
echo "SETALP='"$SETALP"'"                                                          >> tmp_body.sh
echo "SETADD='"$SETADD"'"                                                          >> tmp_body.sh
echo "SETINF='"$SETINF"'"                                                          >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh
echo "WINSMP='"$WINSMP"'"                                                          >> tmp_body.sh
echo "WINLUT='"$WINLUT"'"                                                          >> tmp_body.sh
echo "WINSTH='"$WINSTH"'"                                                          >> tmp_body.sh
echo "#==========================================================================" >> tmp_body.sh
echo "LPFRSM='"$LPFRSM"'"                                                          >> tmp_body.sh
echo "LPFTAU='"$LPFTAU"'"                                                          >> tmp_body.sh
echo "LPFFGT='"$LPFFGT"'"                                                          >> tmp_body.sh
echo "LPFPEF="$LPFPEF                                                              >> tmp_body.sh
echo "LPFLMD="$LPFLMD                                                              >> tmp_body.sh
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


WDATE=`date +"%Y%m%d%H%M%S"`
RUNSCRIPT="333_run_"$LOGNAM".sh"
echo "SUBMIT JOB :: " $RUNSCRIPT

if [ $SETDAS -eq 2 -a $LGMNRM -eq 1 ] ; then 
  echo " skip since this norm is in appropriate for classical PF (SETDAS=2)"
else
  echo "do experiments"
  cat tmp_body.sh save_letkf_tail-XXXXXX.sh > $RUNSCRIPT
  #JSS3  #jxsub $RUNSCRIPT
  #DIRECT#sh $RUNSCRIPT


  # chiba-u. lab. setting, mem :: climax efcst requires 8-32 GB for memory
  chmod u+x $RUNSCRIPT
  #srun --chdir `pwd` -c $NODE --mem=127G -o J${LOGNAM}_${WDATE} -e J${LOGNAM}_${WDATE} ./$RUNSCRIPT &
  #srun --chdir `pwd` -c $NODE --mem=63G -o J${LOGNAM}.o${SLURM_JOB_ID} -e J${LOGNAM}.e${SLURM_JOB_ID} ./$RUNSCRIPT & # ${SLURM_JOB_ID} does not work
  srun --chdir `pwd` -c $NODE --mem=63G -o J${LOGNAM}_${WDATE}.o -e J${LOGNAM}_${WDATE}.e ./$RUNSCRIPT &

  sleep 1
fi


done # LGMNRM
done # LGMFAC
done # LPFLMD
done # LPFPEF
done # LPFFGT
done # LPFTAU
done # LPFRSM
done # WINSTH
done # WINLUT
done # WINSMP
done # SETINF
done # SETADD
done # SETALP
done # SETRTP
done # SETPOW
done # SETHSG
done # SETOBS
done # SETMEM
done # SETNWP
done # SETDAS
#================================================================================================================================>>> END LOOP
echo "end submit jobs"
exit
