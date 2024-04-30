#!/bin/bash
################################################################################
#                                                                              #
#                               --- WARNING ---                                #
#                                                                              #
#     This work contains trade secrets of Hewlett Packard Enterprise (HPE).    #
#     Any unauthorized use or disclosure of the work, or any part thereof,     #
#     is strictly prohibited.  Copyright in this work is the property of       #
#     HPE. All Rights Reserved. In the event of publication, the following     #
#     notice shall apply: Copyright 2023, Hewlett Packard Enterprise.          #
#                                                                              #
####----[ FULL USAGE ]------------------------------------------------------####
#                                                                              #
#     gpu-affinity.sh <application> [app parameters... ]                       #
#                                                                              #
####----[ DESCRIPTION ]-----------------------------------------------------####
#                                                                              #
#     This script is designed to assign proper MI250X GPU (CRAY EX node) to    #
#     an application to maximize NUMA and NUIOA locality. It retrieves the     #
#     current core assigned to run the script to pick the corresponding GPU    #
#     with the same core affinity.                                             #
#                                                                              #
####----[ INFORMATION ]-----------------------------------------------------####
#-    version         1.0                                                      #
#-    authors         Jean-Yves VET <vet@hpe.com>                              #
#-    created         September 25, 2023                                       #
#-    copyright       Copyright (c) 2023 Hewlett Packard Enterprise            #
#-    license         MIT                                                      #
##################################HEADER_END####################################
# Ensure the script is running on proper node type. Checking CPU is AMD Trento
lscpu | grep -q "7A53" ||
{
    if [ "$SLURM_LOCALID" -eq 0 ]; then
        echo "$(basename "$0") error: This is not a GPU node. Exit."
    fi
    exit 1
}
# AMD EPYC 7A53 (Trento) core count
CORE_COUNT=64
# GPU PCIe BUS affinity mapped by CPU core ID
GPU_AFFINITY=(D1 D1 D1 D1 D1 D1 D1 D1
              D6 D6 D6 D6 D6 D6 D6 D6
              C9 C9 C9 C9 C9 C9 C9 C9
              CE CE CE CE CE CE CE CE
              D9 D9 D9 D9 D9 D9 D9 D9
              C1 C1 C1 C1 C1 C1 C1 C1
              DE DE DE DE DE DE DE DE
              C6 C6 C6 C6 C6 C6 C6 C6)
# Fetch core ID used by the current process
CORE_ID=$(ps -o psr --no-headers $$ | xargs)
# Find best GPU, modulo to take SMTs into account
BUS=${GPU_AFFINITY[$CORE_ID % $CORE_COUNT]}
# Check rocm-smi can be used
command -v rocm-smi >/dev/null 2>&1 ||
{
    msg="$(basename "$0") error: rocm-smi (requiered by the wrapper) not found. Make sure proper modules are loaded. Exit."
    if [ "$SLURM_LOCALID" -eq 0 ]; then
        echo $msg
    fi
    exit 1
}
# Get ROCM GPU mapping
GPU_MAP=$(rocm-smi --showbus)
# Ensure the GPU is accessible
echo "$GPU_MAP" | grep -q ":$BUS:" ||
{
    echo "$(basename "$0") error: GPU 0x$BUS is not accessible from SLURM_LOCALID=$SLURM_LOCALID. Exit"
    exit 1
}
# Retrieve GPU ID based on PCIe Address
GPU_ID=$(echo "$GPU_MAP" | grep ":$BUS:" | sed -rn 's/.*GPU\[([0-9]+)\].*/\1/p')
#echo "CORE: $CORE_ID \t BUS: $BUS \t GPU_ID: $GPU_ID"
# Modify visible GPU
export ROCR_VISIBLE_DEVICES=$GPU_ID
# Launch the application
exec $*
