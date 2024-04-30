#!/bin/bash -e
#SBATCH --job-name=p008_node002
#SBATCH --account=project_462000031
#SBATCH --time=00:30:00
#SBATCH --partition=standard-g
#SBATCH --mem=0
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=8
#SBATCH --gpus-per-node=8
#SBATCH --exclusive
#SBATCH -o %x-%j.out

source ../../../../../env_genesis.sh
module sw perftools-base/23.09.0 perftools-base/23.03.0
module load perftools

export PMI_NO_PREINITIALIZE=y
export OMP_NUM_THREADS=7
export OMP_PROC_BIND=true
export OMP_PLACES=cores

rm -rf spdyn+pat*
cp $(pwd)/../../../../src/spdyn/spdyn .
pat_build -u -g mpi,hip spdyn

export PAT_RT_SUMMARY=0

srun -c $OMP_NUM_THREADS spdyn+pat p008.inp
pat_report -T -o myrep.trace.rpt spdyn+pat+*/

