#!/bin/sh
# for making link of common sources
set -e
COMMONDIR=../../common
ln -fs $COMMONDIR/SFMT.f90                 ./
ln -fs $COMMONDIR/common.f90               ./
ln -fs $COMMONDIR/common_mpi.f90           ./
ln -fs $COMMONDIR/common_mtx.f90           ./
ln -fs $COMMONDIR/common_letkf.f90         ./
ln -fs $COMMONDIR/common_lpf.f90           ./
ln -fs $COMMONDIR/netlib.f                 ./
ln -fs $COMMONDIR/netlibblas.f             ./
ln -fs $COMMONDIR/common_time.f90          ./
#==> OT (oldver:: optimal-transport-solver.20210213)

##ln -fs $COMMONDIR/optimal-transport-solver/CImg.h                 ./
#ln -fs $COMMONDIR/optimal-transport-solver/ot.cpp                 ./
#ln -fs $COMMONDIR/optimal-transport-solver/ot.hpp                 ./
#ln -fs $COMMONDIR/optimal-transport-solver/ot-resampling.cpp      ./
#ln -fs $COMMONDIR/optimal-transport-solver/ot-resampling-mod.f90  ./
#ln -fs $COMMONDIR/optimal-transport-solver/timer.hpp              ./

ln -fs ../000_common-share/common_share.f90       ./
ln -fs ../000_common-share/common_mpi_XXXXXX.f90  ./
ln -fs ../000_common-share/common_obs_XXXXXX.f90  ./
ln -fs ../200_common-climax/common_climax.f90     ./

## weight smoother :: following P. Andrew 
#PARMDIR='../model/source'
#ln -fs $PARMDIR/spe_subfft_fftpack.f  ./
#ln -fs $PARMDIR/spe_spectral.f        ./
#ln -fs $PARMDIR/spe_subfft_fftpack.f  ./

#PARMDIR='../model/tmp'
#ln -fs $PARMDIR/atparam.h ./
#ln -fs $PARMDIR/atparam1.h ./
#ln -fs $PARMDIR/com_spectral.h ./


