#!/usr/bin/env bash
set -euo pipefail

ROOT_STORE_DIR="/vmshare/PARA/resources/qris_sandbox" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
TMPDIR_SHARE="/vmshare/PARA/resources/hpc_sandbox"
COPY_MODULES=0 #don't copy HPC module files locally
GIT_BRANCH="patch_fewer_calcs"

source ./aus_bio_content.sh
