#!/bin/bash

module load cray-python

current=$PWD

if [ 1 -eq 1 ] ; then
   python chechout_regression.py
   cd $current
fi
