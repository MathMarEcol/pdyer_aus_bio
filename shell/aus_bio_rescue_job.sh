#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=16G
#SBATCH --job-name=aus_bio_cache_recover
#SBATCH --time=4:00:00
#SBATCH -o aus_bio_output_%j
#SBATCH -e aus_bio_error_%j


set -euo pipefail

#This script uses 7zip to pack up results.
#7zip recently released a native version, but many distros will still be using the unofficial p7zip
#This is only a problem here because p7zip uses "7za" as the running command, but official 7zip for linux uses "7zz"
7z_cmd(){
   if type 7zz; then
      7zz "$@"
   elif type 7za; then
      7za "$@"
   else
      echo "No 7zip (7za or 7zz) command found"
      exit 127
   fi
}

cd $TMPDIR_SHARE/code
export GIT_BRANCH=$(git rev-parse --abbrev-ref HEAD)
export git_hash=$(git rev-parse --short HEAD)
export date_run=$(date +%Y-%m-%d_%H-%M-%S)

#Store the targets cache
cd $TMPDIR_SHARE/code/R
7z_cmd u -mx=0 $TMPDIR_SHARE/code/R/targets_cache.7z  $TMPDIR_SHARE/code/R/_targets
mkdir -p $ROOT_STORE_DIR/aus_bio_outputs
rsync -irc $TMPDIR_SHARE/code/R/targets_cache.* $ROOT_STORE_DIR/aus_bio_outputs

#Store the outputs
cd $TMPDIR_SHARE
7z_cmd a "$TMPDIR_SHARE/${date_run}_${GIT_BRANCH}_${git_hash}_failed_outputs.7z"  $TMPDIR_SHARE/outputs/*
rsync -irc $TMPDIR_SHARE/*_outputs.* $ROOT_STORE_DIR/aus_bio_outputs
#cp "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z" $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z

#The downloaded variables from bioORACLE are also worth saving
export TMP_DATA_DIR=$TMPDIR_SHARE/code/R/data
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/data/

#clean up TMPDIR_SHARE
chmod 770 -R $TMPDIR_SHARE/*
rm -r $TMPDIR_SHARE/*
#done
