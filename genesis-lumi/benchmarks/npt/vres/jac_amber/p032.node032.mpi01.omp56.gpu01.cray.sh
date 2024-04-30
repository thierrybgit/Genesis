#!/bin/bash -e
#SBATCH --job-name=p032_node032
#SBATCH --account=Project_462000123
#SBATCH --time=00:30:00
#SBATCH --partition=standard-g
#SBATCH --mem=0
#SBATCH --nodes=32
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=56
#SBATCH --gpus-per-node=1
#SBATCH --exclusive
#SBATCH -o %x-%j.out
export PMI_NO_PREINITIALIZE=y
module load PrgEnv-cray
module load craype-x86-trento
module load craype-accel-amd-gfx90a
module load rocm
module load cray-libsci
export OMP_NUM_THREADS=$SLURM_CPUS_PER_TASK
export OMP_PROC_BIND=true
export OMP_PLACES=cores
CPU_BIND="mask_cpu:"
CPU_BIND="${CPU_BIND}0xfefefefefefefefe"
export SLURM_CPU_BIND="${CPU_BIND}"

# Warm up
srun "$(pwd)/../../../../src/spdyn/spdyn" p032.inp

# Benchmark
srun "$(pwd)/../../../../src/spdyn/spdyn" p032.inp 2>&1 | tee p032.node032.mpi01.omp56.gpu01.cray.id000.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p032.inp 2>&1 | tee p032.node032.mpi01.omp56.gpu01.cray.id001.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p032.inp 2>&1 | tee p032.node032.mpi01.omp56.gpu01.cray.id002.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p032.inp 2>&1 | tee p032.node032.mpi01.omp56.gpu01.cray.id003.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p032.inp 2>&1 | tee p032.node032.mpi01.omp56.gpu01.cray.id004.out
