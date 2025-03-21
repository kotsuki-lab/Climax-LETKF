#!/bin/sh -f
set -e
#############################################################
VER="v1.14"
CTRLDIR="/export/home/kotsuki/climax-letkf/SYS20240830_SPEEDY-ClimaX_LETKFv1.05_base"
WBCHDIR="/data10/kotsuki/climax-letkf/weatherbench/v20240703/"
PREPDIR="/data10/atakeshima/PREPBUFR/PREPBUFR_ClimaX/v02/share/thinning/dnst1_sfc1_Tv01_n0"
#############################################################
TAG=`date '+%Y%m%d'`
BASE=`pwd`
DIR="SYS${TAG}_SPEEDY-ClimaX_LETKF${VER}"
CP="cp -arv"
LN="ln -fs"
#############################################################

mkdir $DIR ; cd $DIR
${CP} ../archive.sh         ./
${CP} ../common/            ./

### SPEEDY / CLIMAX
mkdir speedy-climax
mkdir speedy-climax/DATA
  # common
  ${CP} ../speedy-climax/share                   ./speedy-climax/
  ${CP} ../speedy-climax/000_common-share        ./speedy-climax/
  ${CP} ../speedy-climax/300_letkf               ./speedy-climax/
  ${CP} ../speedy-climax/DATA/*.sh               ./speedy-climax/DATA/
    # memo jaxa's data remove scripts will be added
  mkdir -p speedy-climax/DATA/reg2_sp/finished
  mkdir -p speedy-climax/DATA/raob_sp/finished
  mkdir -p speedy-climax/DATA/raob_wb/finished
  mkdir -p speedy-climax/DATA/prepbufr/finished

  # speedy
  ${CP} ../speedy-climax/100_common-speedy       ./speedy-climax/
  ${CP} ../speedy-climax/101_model-speedy        ./speedy-climax/
  ${CP} ../speedy-climax/102_obs-speedy          ./speedy-climax/
  ${CP} ../speedy-climax/110_grads-speedy        ./speedy-climax/
  ${CP} ../speedy-climax/111_val-speedy          ./speedy-climax/
  ${CP} ../speedy-climax/109_LTERPLUT-speedy     ./speedy-climax/

  # climax
  ${CP} ../speedy-climax/200_common-climax       ./speedy-climax/
  ${CP} ../speedy-climax/202_obs-climax          ./speedy-climax/
  ${CP} ../speedy-climax/210_grads-climax        ./speedy-climax/
  ${CP} ../speedy-climax/211_val-climax          ./speedy-climax/

  # link from base / obs
  DATADIR="$BASE/$DIR/speedy-climax/DATA"
  cd $DATADIR           ; ${LN} $WBCHDIR                                                                      ./weatherbc
                          ${LN} $CTRLDIR/speedy-climax/DATA/nature-speedy                                     ./
  cd $DATADIR/raob_wb/  ; ${LN} $CTRLDIR/speedy-climax/DATA/raob_wb/obs                                       ./
  cd $DATADIR/raob_sp/  ; ${LN} $CTRLDIR/speedy-climax/DATA/raob_sp/obs                                       ./ 
                          ${LN} $CTRLDIR/speedy-climax/DATA/raob_sp/init_ensemble_M000040L1200IADP.1983010100 ./
  cd $DATADIR/reg2_sp/  ; ${LN} $CTRLDIR/speedy-climax/DATA/reg2_sp/obs                                       ./
                          ${LN} $CTRLDIR/speedy-climax/DATA/reg2_sp/init_ensemble_M000040L0600IADP.1983010100 ./
  cd $DATADIR/prepbufr/ ; ${LN} $PREPDIR/obs                                                                  ./ 
			 
echo "============================================================================"
echo "                         Archived   Successfully                            "
echo "============================================================================"

### developmental history
# 20241011 v1.xx additive inlfation (random, not yet)

# 20250123 v1.14 CB's ETKF, implemented
# 20250121 v1.13 effective particle size implemented for LETKF
# 20250120 v1.12 assimilation of prepbufr implemented (A. Takeshima)
# 20250111 v1.11 bug fixed (multiplicative inflation & RTPS)
# 20241030 v1.10 adaptive multiplicative inflation (namelist)
# 20240928 v1.09 additive inflation monitor
# 20240928 v1.08 additive inflation updated (adter relaxation)
# 20240905 v1.07 additive inflation
# 20240902 v1.06 PO method (Tsuyuki 2024; JMSJ), Sinkhorn Resampling implemented
# 20240830 v1.05 weight interpolation disabled (to reduce memory usage)
# 20240830 v1.04 added obs type
# 20240806 v1.03 ensemble diagnosis, extended forecasts, minor modification on speedy forecasts
# 20240802 v1.02 ensemble prediction modes
# 20240730 v1.02 merged speedy-climax & letkf
