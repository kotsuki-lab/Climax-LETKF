#!/bin/sh
#=======================================================================
# environment::
#   singularity run --bind /data02,/data10 ../share/oneapi-hpckit_latest.sif
#     - bind directories (e.g., data02 data10) should be modified by users
#=======================================================================
set -ex
PGM=obsmake.s01
#F90=pgf90
F90=ifort
#F90=gfortran
OMP=
#F90OPT='-byteswapio -tp p7-64 -fast -O3'
#INLINE="-Minline"
F90OPT='-convert big_endian -assume byterecl -O3'
#F90OPT='-fconvert=big-endian -frecord-marker=4'
INLINE=""

sh ulnkcommon.sh
sh lnkcommon.sh
rm -f *.mod
rm -f *.o

$F90 $OMP $F90OPT $INLINE -c SFMT.f90
$F90 $OMP $F90OPT $INLINE -c common.f90
$F90 $OMP $F90OPT $INLINE -c common_climax.f90
$F90 $OMP $F90OPT $INLINE -c common_share.f90
$F90 $OMP $F90OPT         -c common_obs_XXXXXX.f90
$F90 $OMP $F90OPT         -c obsmake.f90
$F90 $OMP $F90OPT         -o ${PGM} *.o $F90LIB

rm -f *.mod
rm -f *.o
sh ulnkcommon.sh

echo "NORMAL END"
