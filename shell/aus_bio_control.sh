#!/usr/bin/env bash

# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only

set -euo pipefail

## Set up an R shell
#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases

export R_SHELL_REV=e14ac6840c7cb813b3c086e7526435aef613f050

alias R='nix develop github:PhDyellow/nix_r_dev_shell/${R_SHELL_REV}#devShells."x86_64-linux".r-shell -c $NIX_GL_PREFIX R'
alias Rscript='nix develop github:PhDyellow/nix_r_dev_shell/${R_SHELL_REV}#devShells."x86_64-linux".r-shell -c $NIX_GL_PREFIX Rscript'
alias mksquashfs='nix develop github:PhDyellow/nix_r_dev_shell/${R_SHELL_REV}#devShells."x86_64-linux".r-shell -c mksquashfs'
alias unsquashfs='nix develop github:PhDyellow/nix_r_dev_shell/${R_SHELL_REV}#devShells."x86_64-linux".r-shell -c unsquashfs'


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

## Most work is done in TMPDIR of control worker, because it usually does not have quota limits
## Assume for now that workers have access to shared filesystem
export TMPDIR_CONTROL=$TMPDIR

mkdir -p $ROOT_STORE_DIR/aus_bio_outputs

mkdir -p $TMPDIR_CONTROL/outputs

cd $TMPDIR_CONTROL
git clone -b $GIT_BRANCH --single-branch https://github.com/MathMarEcol/pdyer_aus_bio.git ./code

#capture current git hash for use later
cd $TMPDIR_CONTROL/code
export git_hash=$(git rev-parse --short HEAD)
export date_run=$(date +%Y-%m-%d_%H-%M-%S)

#For playing nice on the hpc, put all data into a cluster network disk, don't leave it on UQ RDM
export TMP_DATA_DIR=$TMPDIR_CONTROL/code/R/data

#All of the datasets are in smallish blocks of files. no more than a few dozen files per dataset, most are less than 5
mkdir -p $TMP_DATA_DIR/aus_microbiome/marine_bacteria
7z_cmd x $ROOT_STORE_DIR/data/aus_microbiome/marine_bacteria_AustralianMicrobiome-2019-07-03T093815-csv.zip -o$TMP_DATA_DIR/aus_microbiome/marine_bacteria
mkdir -p $TMP_DATA_DIR/bioORACLE
rsync -irc $ROOT_STORE_DIR/data/bioORACLE $TMP_DATA_DIR/
mkdir -p $TMP_DATA_DIR/AusCPR
rsync -irc $ROOT_STORE_DIR/data/AusCPR/ $TMP_DATA_DIR/AusCPR
mkdir -p $TMP_DATA_DIR/ShapeFiles/World_EEZ_v8
rsync -irc $ROOT_STORE_DIR/data/ShapeFiles/World_EEZ_v8 $TMP_DATA_DIR/ShapeFiles/
mkdir -p $TMP_DATA_DIR/Watson_Fisheries_Catch_Data/Version5/Output
rsync -irc $ROOT_STORE_DIR/data/Watson_Fisheries_Catch_Data/Version5/Output/Annual_TotalCatchSpecies $TMP_DATA_DIR/Watson_Fisheries_Catch_Data/Version5/Output
rsync -irc $ROOT_STORE_DIR/data/Watson_Fisheries_Catch_Data/Version5/Output/TaxonomicData.rds $TMP_DATA_DIR/Watson_Fisheries_Catch_Data/Version5/Output
mkdir -p $TMP_DATA_DIR/mpa_poly_june_2023
rsync -irc $ROOT_STORE_DIR/data/mpa_poly_june_2023/ $TMP_DATA_DIR/mpa_poly_june_2023

#Set up the output directory
if [[ -f "$ROOT_STORE_DIR/aus_bio_outputs/current_output.squashfs" ]]; then
		unsquashfs -q -dest $TMPDIR_CONTROL/outputs $ROOT_STORE_DIR/aus_bio_outputs/current_output.squashfs
fi

#Load in the cache if it exists
if [[ -f  "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache.squashfs" ]]; then
		unsquashfs -q -dest $TMPDIR_CONTROL/code/R $ROOT_STORE_DIR/aus_bio_outputs/targets_cache.squashfs
fi

## Files are ready, set up env


cd $TMPDIR_CONTROL/code/R/

## Put logs on scratch
mkdir -p $SCRATCH_PIPELINE_DIR/logs
export LOGDIR=$SCRATCH_PIPELINE_DIR/logs



## Run Targets


## Some Slurm variables need to be unset so workers do not get conflicting env
## Only env vars interpreted as resource requests need to be cleared
## Keep SLURM_JOB_ACCOUNT

unset ${!SBATCH*}
export TMP_EXPORT_ENV=$SLURM_EXPORT_ENV
if [[ -v  SLURM_JOB_ACCOUNT ]]; then
		export SBATCH_ACCOUNT=$SLURM_JOB_ACCOUNT
fi
unset ${!SLURM*}
export SLURM_EXPORT_ENV=$TMP_EXPORT_ENV
export LC_ALL=C
export SBATCH_EXPORT=LC_ALL,R_FUTURE_GLOBALS_MAXSIZE,date,HOME,LANG,MKL_THREADING_LAYER,MKL_INTERFACE_LAYER,NIX_SSL_CERT_FILE,NIX_GL_PREFIX,TMPDIR_CONTROL

## Attempt to build pipeline
## On success or failure, clean up rather than killing the pipeline
set +euo pipefail
## timeout: If pipeline takes too long, stop and clean up
timeout 315h  nix develop github:PhDyellow/nix_r_dev_shell/${R_SHELL_REV}#devShells."x86_64-linux".r-shell -c $NIX_GL_PREFIX R --vanilla -e "targets::tar_make(reporter = 'verbose_positives')"

set -euo pipefail

## Clean up

## Store the logs
mkdir -p $ROOT_STORE_DIR/aus_bio_logs
mksquashfs $SCRATCH_PIPELINE_DIR/logs "${ROOT_STORE_DIR}/aus_bio_logs/${date_run}_${GIT_BRANCH}_${git_hash}_logs.squashfs" -comp zstd -Xcompression-level 1 -quiet -no-duplicates -noappend  -mem 25G

#Store the cache
if [[ -f  "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache.squashfs" ]]; then
		mv "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache.squashfs" "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache_old.squashfs"
fi
cd $TMPDIR_CONTROL/code/R/
mksquashfs _targets ${ROOT_STORE_DIR}/aus_bio_outputs/targets_cache.squashfs -comp zstd -Xcompression-level 1 -b 1M -quiet -no-duplicates -noappend -no-strip -mem 25G
if [[ -f  "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache_old.squashfs" ]]; then
		rm "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache_old.squashfs"
fi

#Store the outputs
mksquashfs $TMPDIR_CONTROL/outputs "${ROOT_STORE_DIR}/aus_bio_outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.squashfs" -comp zstd -Xcompression-level 12 -b 512k -quiet -no-duplicates -noappend  -mem 25G
if [[ -f "$ROOT_STORE_DIR/aus_bio_outputs/current_output.squashfs" ]]; then
		unlink $ROOT_STORE_DIR/aus_bio_outputs/current_output.squashfs
fi
ln -s "$ROOT_STORE_DIR/aus_bio_outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.squashfs" $ROOT_STORE_DIR/aus_bio_outputs/current_output.squashfs

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/data/

#clean up SCRATCH_PIPELINE_DIR
chmod 770 -R $SCRATCH_PIPELINE_DIR
rm -r $SCRATCH_PIPELINE_DIR
#done
