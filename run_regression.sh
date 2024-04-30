#!/bin/bash

current=$PWD

if [ 1 -eq 1 ] ; then
   cp lumi.job.sh genesis-lumi/tests/regression_test/.
   cd genesis-lumi/tests/regression_test
   rm -rf slurm*.out
   sbatch lumi.job.sh
   cd $current
fi
