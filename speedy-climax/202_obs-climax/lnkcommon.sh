#!/bin/sh
# for making link of common sources
set -e
COMMONDIR=../../common
ln -fs $COMMONDIR/SFMT.f90 ./
ln -fs $COMMONDIR/common.f90 ./
ln -fs $COMMONDIR/common_mpi.f90 ./
ln -fs $COMMONDIR/common_obs.f90 ./
ln -fs $COMMONDIR/common_mtx.f90 ./
ln -fs $COMMONDIR/common_letkf.f90 ./
ln -fs $COMMONDIR/netlib.f ./

ln -fs ../000_common-share/common_share.f90       ./
ln -fs ../000_common-share/common_mpi_XXXXXX.f90  ./
ln -fs ../000_common-share/common_obs_XXXXXX.f90  ./
ln -fs ../200_common-climax/common_climax.f90     ./


