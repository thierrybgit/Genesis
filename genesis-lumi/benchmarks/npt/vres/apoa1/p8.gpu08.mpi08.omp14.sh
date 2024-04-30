#!/bin/bash -e
#SBATCH --job-name=apo_p8
#SBATCH --account=project_462000031
#SBATCH --time=02:00:00
#SBATCH --partition=standard-g
#SBATCH --mem=0
#SBATCH --nodes=1
#SBATCH --gpus-per-node=8
#SBATCH --ntasks-per-node=8
#SBATCH --exclusive

ml rocm
ml LUMI/23.09 partition/G
ml lumi-CPEtools

export PMI_NO_PREINITIALIZE=y
export OMP_NUM_THREADS=7
export OMP_PROC_BIND=true
export OMP_PLACES=cores

srun -c $OMP_NUM_THREADS ./gpu-affinity.sh gpu_check 2>&1 | tee p8.gpu08.mpi08.omp15.id000.out

module sw cpeGNU/23.09 PrgEnv-cray/8.4.0
source ../../../../../env_genesis.sh
 
export PMI_NO_PREINITIALIZE=y
export OMP_NUM_THREADS=7
export OMP_PROC_BIND=true
export OMP_PLACES=cores
#export SLURM_CPU_BIND="mask_cpu:0xfe000000000000,0xfe00000000000000,0xfe0000,0xfe000000,0xfe,0xfe00,0xfe00000000,0xfe0000000000"
srun -c $OMP_NUM_THREADS ./gpu-affinity.sh "$(pwd)/../../../../src/spdyn/spdyn" p008.inp 2>&1 | tee p8.gpu08.mpi08.omp15.id000.out
srun -c $OMP_NUM_THREADS ./gpu-affinity.sh "$(pwd)/../../../../src/spdyn/spdyn" p008.inp 2>&1 | tee p8.gpu08.mpi08.omp15.id001.out
srun -c $OMP_NUM_THREADS ./gpu-affinity.sh "$(pwd)/../../../../src/spdyn/spdyn" p008.inp 2>&1 | tee p8.gpu08.mpi08.omp15.id002.out
