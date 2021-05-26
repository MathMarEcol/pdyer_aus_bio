#!/usr/bin/env bash
set -euo pipefail

if [ $# -ne 2 ]; then
    echo Usage: ./aus_bio_recover_cache.sh path-to-long-term-store path-to-scratch
    exit 1
fi

#Recover the cache in the event of a failed run
ROOT_STORE_DIR=$1
TMPDIR_SHARE=$2

TMP_DATA_DIR=$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/data
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
git_hash=$(git rev-parse --short HEAD)
date_run=$(date +%Y-%m-%d_%H-%M-%S)

#Store the drake cache
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
7za u -mx=0 $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/targets_cache.7z  ./_targets
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/targets_cache.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs

#Store the outputs
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
7za a "$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/${date_run}_${GIT_BRANCH}_${git_hash}_outputs_recovered.7z"  ./outputs
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/*_outputs_recovered.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/Q1215/
