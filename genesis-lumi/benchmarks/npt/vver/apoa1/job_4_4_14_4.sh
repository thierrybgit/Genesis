#!/bin/bash -e
#SBATCH --job-name=p008_node002
#SBATCH --account=project_462000031
#SBATCH --time=00:30:00
#SBATCH --partition=standard-g
#SBATCH --mem=0
#SBATCH --nodes=4
#SBATCH --ntasks-per-node=4
#SBATCH --gpus-per-node=4
#SBATCH --exclusive
#SBATCH -o %x-%j.out

ml rocm
ml LUMI/23.09 partition/G
ml lumi-CPEtools

export PMI_NO_PREINITIALIZE=y
export OMP_NUM_THREADS=14
export OMP_PROC_BIND=true
export OMP_PLACES=cores

srun -c $OMP_NUM_THREADS ./gpu-affinity.sh gpu_check 2>&1 | tee p8.gpu08.mpi08.omp15.id000.out

module sw cpeGNU/23.09 PrgEnv-cray/8.4.0
source ../../../../../env_genesis.sh

# Warm up
srun -c $OMP_NUM_THREADS "$(pwd)/../../../../src/spdyn/spdyn" p016.inp

# Benchmark
srun -c $OMP_NUM_THREADS "$(pwd)/../../../../src/spdyn/spdyn" p016.inp 2>&1 | tee p016.node002.mpi04.omp14.gpu04.gnu.id000.out
srun -c $OMP_NUM_THREADS "$(pwd)/../../../../src/spdyn/spdyn" p016.inp 2>&1 | tee p016.node002.mpi04.omp14.gpu04.gnu.id001.out
srun -c $OMP_NUM_THREADS "$(pwd)/../../../../src/spdyn/spdyn" p016.inp 2>&1 | tee p016.node002.mpi04.omp14.gpu04.gnu.id002.out
srun -c $OMP_NUM_THREADS "$(pwd)/../../../../src/spdyn/spdyn" p016.inp 2>&1 | tee p016.node002.mpi04.omp14.gpu04.gnu.id003.out
srun -c $OMP_NUM_THREADS "$(pwd)/../../../../src/spdyn/spdyn" p016.inp 2>&1 | tee p016.node002.mpi04.omp14.gpu04.gnu.id004.out
