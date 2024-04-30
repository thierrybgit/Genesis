#!/bin/bash

current=$PWD

if [ 1 -eq 1 ] ; then
   cp p8.gpu08.mpi08.omp14.sh genesis-lumi/benchmarks/npt/vver/apoa1/.
   cp select-gpu.sh genesis-lumi/benchmarks/npt/vver/apoa1/.
   cp gpu-affinity.sh genesis-lumi/benchmarks/npt/vver/apoa1/.
   cd genesis-lumi/benchmarks/npt/vver/apoa1
   rm -rf *.out
   sbatch p8.gpu08.mpi08.omp14.sh
   cd $current
fi

if [ 1 -eq 1 ] ; then
   cp p8.gpu08.mpi08.omp14.sh genesis-lumi/benchmarks/npt/vres/apoa1/.
   cp select-gpu.sh genesis-lumi/benchmarks/npt/vres/apoa1/.
   cp gpu-affinity.sh genesis-lumi/benchmarks/npt/vres/apoa1/.
   cd genesis-lumi/benchmarks/npt/vres/apoa1
   rm -rf *.out
   sbatch p8.gpu08.mpi08.omp14.sh
   cd $current
fi
