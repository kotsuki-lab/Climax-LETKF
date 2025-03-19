#!/bin/sh
#=======================================================================
# environment::
#   singularity run --bind /data02,/data10 ../share/oneapi-hpckit_latest.sif
#     - bind directories (e.g., data02 data10) should be modified by users
#=======================================================================
set -ex
PGM=ensdiag020-climax.m01
F90=mpiifort
F77=mpif77
CPP=mpiFCCpx
OMP=
#F90OPT='-byteswapio -tp p7-64 -fast -O3'
#INLINE="-Minline"
F90OPT='-convert big_endian -assume byterecl -O3' # -pg'
F90OPT='-convert big_endian -assume byterecl -O3 -mcmodel=large -heap-arrays -xHost'
#F90OPT='-convert big_endian -assume byterecl -O3 -check bounds -mcmodel=large -heap-arrays'

CPPOPT="-O3"
INLINE=""
BLAS=0 #0: no blas 1: using blas

sh 000_unlnkcommon-climax.sh
sh 000_lnkcommon-climax.sh
rm -f *.mod
rm -f *.o

cat netlib.f > netlib2.f
if test $BLAS -eq 1
then
LBLAS="-mkl"
cat netlibblas.f >> netlib2.f #remove soon
else
cat netlibblas.f >> netlib2.f
LBLAS=""
fi

$F90 $OMP $F90OPT $INLINE -c SFMT.f90
$F90 $OMP $F90OPT $INLINE -c common.f90
$F90 $OMP $F90OPT         -c common_mpi.f90
$F90 $OMP $F90OPT         -c common_time.f90
$F90 $OMP $F90OPT $INLINE -c common_mtx.f90
$F90 $OMP $F90OPT $INLINE -c netlib2.f
$F90 $OMP $F90OPT         -c common_letkf.f90
$F90 $OMP $F90OPT         -c common_lpf.f90
$F90 $OMP $F90OPT $INLINE -c common_climax.f90
$F90 $OMP $F90OPT $INLINE -c common_share.f90
$F90 $OMP $F90OPT         -c common_mpi_XXXXXX.f90
$F90 $OMP $F90OPT         -c ensdiagnosis.f90
#CLIMAX$F90 $OMP $F90OPT -o ${PGM} *.o $LBLAS -lc++
$F90 $OMP $F90OPT         -o ${PGM} *.o

#$CPP *.o -lgfortran



rm -f *.mod
rm -f *.o
rm -f netlib2.f
sh 000_unlnkcommon-climax.sh

echo "NORMAL END"
