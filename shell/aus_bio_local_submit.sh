#!/usr/bin/env bash
set -euo pipefail

#W use "GIT_BRANCH=tagname WORKERS=2 ./aus_bio_pbs_local.sh"
export ROOT_STORE_DIR="/para/resources/qris_sandbox" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE="/para/resources/hpc_sandbox"
export COPY_MODULES=0 #don't copy HPC module files locally
export GIT_BRANCH=${GIT_BRANCH:-"develop"} #to submit a particular tag
export WORKERS=${WORKERS:-2} # 2 workers by default,
export FUTURE_WORKERS=${FUTURE_WORKERS:-8}
export SCHEDULER="multiprocess" #note, this is clustermq multiprocess (callr) not future multiprocess (deprecated, gives sequential behaviour, use multisession)
export R_FUTURE_GLOBALS_MAXSIZE=${R_FUTURE_GLOBALS_MAXSIZE:-100000000000}
export TF_FORCE_GPU_ALLOW_GROWTH="true"
export TENSOR_CPU_MEM_MAX=${TENSOR_CPU_MEM_MAX:-12000000000} #Certain operations that do bulk operations over matricies will batch to keep RAM usage within this amount (in bytes)
export TENSOR_GPU_MEM_MAX=${TENSOR_GPU_MEM_MAX:-1500000000} #Certain operations that do bulk operations over matricies will batch to keep GPU memory usage within this amount (in bytes)
export TENSOR_DEVICE=${TENSOR_DEVICE:-CUDA} # Set to CUDA to attempt to use nvidia graphics card, any other value will use CPU
export CUDA_MODULE_LOADING=LAZY
echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building with [${WORKERS}] Workers under [${SCHEDULER}]"
source ./aus_bio_content.sh
