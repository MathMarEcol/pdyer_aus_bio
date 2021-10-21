#!/usr/bin/env bash
set -euo pipefail


export ROOT_STORE_DIR="/vmshare/PARA/resources/qris_sandbox" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE=${1:-"/vmshare/PARA/resources/hpc_sandbox"}
echo "Rescuing cache and outputs from local run"
source ./rescue_cache_content.sh
