#!/bin/sh
#=======================================================================
# environment::
#   singularity run --bind /data02,/data10 ../share/oneapi-hpckit_latest.sif
#     - bind directories (e.g., data02 data10) should be modified by users
#=======================================================================
set -ex
PGM=obsopeMPI-speedy.s01
F90=mpiifort
OMP=
#F90OPT='-byteswapio -tp sandybridge-64 -fast -O3'
F90OPT='-convert big_endian -assume byterecl -O3' # -traceback -check all' # -pg'
#INLINE="-Minline"

#RURI
F90=mpiifort
F90OPT='-convert big_endian -assume byterecl -O3'
BLAS=1 #0: no blas 1: using blas

sh 000_unlnkcommon-speedy.sh
sh 000_lnkcommon-speedy.sh
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
$F90 $OMP $F90OPT         -c common_mtx.f90
$F90 $OMP $F90OPT $INLINE -c netlib2.f
$F90 $OMP $F90OPT         -c common_letkf.f90
$F90 $OMP $F90OPT         -c common_lpf.f90
$F90 $OMP $F90OPT $INLINE -c common_speedy.f90
$F90 $OMP $F90OPT $INLINE -c common_share.f90
$F90 $OMP $F90OPT         -c common_obs_XXXXXX.f90
$F90 $OMP $F90OPT         -c obsope_MPI.f90
$F90 $OMP $F90OPT         -o ${PGM} *.o

rm -f *.mod
rm -f *.o
sh 000_unlnkcommon-speedy.sh

echo "NORMAL END"
