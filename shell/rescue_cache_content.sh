#!/usr/bin/env bash
set -euo pipefail

cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
export GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
export git_hash=$(git rev-parse --short HEAD)
export date_run=$(date +%Y-%m-%d_%H-%M-%S)

#Store the drake cache
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
7za u -mx=0 $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/targets_cache.7z  ./_targets
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/targets_cache.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs

#Store the outputs
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
7za a "$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/${date_run}_${GIT_BRANCH}_${git_hash}_failed_outputs.7z"  ./outputs
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/*_outputs.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs
#cp "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z" $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z

#The downloaded variables from bioORACLE are also worth saving
export TMP_DATA_DIR=$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/data
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/Q1215/

#clean up TMPDIR_SHARE
chmod 770 -R $TMPDIR_SHARE
rm -r $TMPDIR_SHARE/*
#done
