#!/usr/bin/env bash
set -euo pipefail

export ROOT_STORE_DIR="/data/uqpdyer/resources/QRISdata/" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE=${1?"Must provide a path to TMPDIR used by the job"}
echo "Rescuing cache and outputs from getafix run"
source ./rescue_cache_content.sh
