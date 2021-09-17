#!/usr/bin/env bash
set -euo pipefail

#W use "GIT_BRANCH=tagname WORKERS=2 ./aus_bio_pbs_local.sh"
export ROOT_STORE_DIR="/vmshare/PARA/resources/qris_sandbox" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE="/vmshare/PARA/resources/hpc_sandbox"
export COPY_MODULES=0 #don't copy HPC module files locally
export GIT_BRANCH=${GIT_BRANCH:-"develop"} #to submit a particular tag
export WORKERS=${WORKERS:-2} # 2 workers by default,
export SCHEDULER="multiprocess"
echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building with [${WORKERS}] Workers under [${SCHEDULER}]"
source ./aus_bio_content.sh