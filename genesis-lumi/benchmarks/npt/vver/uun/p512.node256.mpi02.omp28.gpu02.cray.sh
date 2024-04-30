#!/bin/bash -e
#SBATCH --job-name=p512_node256
#SBATCH --account=Project_462000123
#SBATCH --time=02:00:00
#SBATCH --partition=standard-g
#SBATCH --mem=0
#SBATCH --nodes=256
#SBATCH --ntasks-per-node=2
#SBATCH --cpus-per-task=28
#SBATCH --gpus-per-node=2
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
CPU_BIND="${CPU_BIND}0xfefe0000fefe0000,"
CPU_BIND="${CPU_BIND}0x0000fefe0000fefe"
export SLURM_CPU_BIND="${CPU_BIND}"

# Warm up
srun "$(pwd)/../../../../src/spdyn/spdyn" p512.inp

# Benchmark
srun "$(pwd)/../../../../src/spdyn/spdyn" p512.inp 2>&1 | tee p512.node256.mpi02.omp28.gpu02.cray.id000.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p512.inp 2>&1 | tee p512.node256.mpi02.omp28.gpu02.cray.id001.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p512.inp 2>&1 | tee p512.node256.mpi02.omp28.gpu02.cray.id002.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p512.inp 2>&1 | tee p512.node256.mpi02.omp28.gpu02.cray.id003.out
srun "$(pwd)/../../../../src/spdyn/spdyn" p512.inp 2>&1 | tee p512.node256.mpi02.omp28.gpu02.cray.id004.out
