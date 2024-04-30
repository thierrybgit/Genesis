#!/bin/bash
#SBATCH --job-name=test
#SBATCH --account=project_462000031
#SBATCH --time=02:00:00
#SBATCH --partition=small-g
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --cpus-per-task=1
#SBATCH --gpus-per-node=8

GENESIS_HOME="$(pwd)/../../"
GENESIS_PATH="${GENESIS_HOME}/src/spdyn/"
GENESIS_TEST="${GENESIS_HOME}/tests/regression_test/"

source ${GENESIS_HOME}/../env_genesis.sh
module load cray-python

export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK

python3 "${GENESIS_TEST}/test.py" "${GENESIS_PATH}/spdyn" "lumi" "gpu"
