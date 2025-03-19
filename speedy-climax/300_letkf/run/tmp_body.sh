#! /bin/bash -x
export MPIRUN=mpiexec


#----------------------INITIALIZE PARAMETERS-----------------------------#
set -e
export NODE_list='n01'
export NODE=20                   #Nodes for parallel computing
export CDIR=/export/home/kotsuki/climax-letkf/SYS20250121_SPEEDY-ClimaX_LETKFv1.13/speedy-climax/300_letkf/run
LUTDIR='/../../900_LTERPLUT-speedy/output/'
export CLIMAXDIR='/data10/kotsuki/climax-letkf/climax/v20240807'
export CLIMAXVER='int'
export WBCLIMDIR='/data10/kotsuki/climax-letkf/climax/climate/climate/2006-2015'
export WBCLIMDIR='/data10/kotsuki/climax-letkf/climax/climate/climate/2006-2015'
#==========================================================================
DISK=0
SYS='V999999_'
EXP='V999999_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00'
LOGNAM='prepbufr_dnst1_sfc1_Tv12_n0_V999999_CLIMAXint-0LETKF_M000020L0600_inf1.00_RTPS1.20_ad0.00_po0.00_NODE20_HDD'
RESTART_MODE=1
VERIFY__MODE=1
EXTFCST_STEP=20
SETDAS='0'
SETNWP='CLIMAX'
SETMEM='20'
SETOBS='prepbufr_dnst1_sfc1_Tv12_n0'
SETHSG='600'
SETPOW='0.00'
SETRTP='2'
SETALP='1.20'
SETADD='0.00'
SETINF='1.00'
#==========================================================================
WINSMP='1'
WINLUT='BLLUT'
WINSTH='0'
#==========================================================================
LPFRSM='3'
LPFTAU='0.00'
LPFFGT='1.00'
LPFPEF=40
LPFLMD=40
LGMFAC=1.5
LGMNRM='0'
#==========================================================================
SYYYY=2017
SMM=02
SDD=01
SHH=00
EYYYY=2017
EMM=02
EDD=15
EHH=18
#==========================================================================
