#! /bin/bash -x
#
# for FX100(JSS SORA)
#
#JX --bizcode R0201
#JX --usecode SP
#JX -L rscunit=MA
#JX -L node=10
#JX --mpi proc=40
#JX -L node-mem=24Gi
#JX -L "elapse=00:10:00"
#JX -j
#JX -s
export MPIRUN=mpiexec

PARALLEL=8 ;export PARALLEL
export OMP_NUM_THREADS=8
export XOS_MMM_L_ARENA_FREE=2

#-----------------------INITIALIZE PARAMETERS------------------------------#
set -e
export NODE_list="n01"
export NODE=40                        #Nodes for parallel computing
CDIR=`pwd`
LUTDIR="$CDIR/../../src_LTERPLUT/output/"
############################## parameter needed to be editted (str) ############################## 
