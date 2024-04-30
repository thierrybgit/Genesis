#!/bin/bash

source env_genesis.sh

current=$PWD

if [ 1 -eq 1 ] ; then
   cp Makefile.lumi.cray.spdyn.gpu genesis-lumi/src/spdyn/.
   cd genesis-lumi/src/spdyn
   make -f Makefile.lumi.cray.spdyn.gpu clean
   make -f Makefile.lumi.cray.spdyn.gpu
   cd $current
fi
