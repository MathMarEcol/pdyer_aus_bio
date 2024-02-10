#!/usr/bin/env bash
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
echo "Running2"


mkdir -p $ROOT_STORE_DIR/aus_bio_outputs

mkdir -p $TMPDIR_SHARE/outputs

cd $TMPDIR_SHARE
git clone -b $GIT_BRANCH --single-branch https://github.com/MathMarEcol/pdyer_aus_bio.git ./code

#capture current git hash for use later
cd $TMPDIR_SHARE/code
export git_hash=$(git rev-parse --short HEAD)
export date_run=$(date +%Y-%m-%d_%H-%M-%S)

#For playing nice on the hpc, put all data into a cluster network disk, don't leave it on UQ RDM
export TMP_DATA_DIR=$TMPDIR_SHARE/code/R/data

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
if [[ -f "$ROOT_STORE_DIR/aus_bio_outputs/current_output.7z" ]]; then
		7z_cmd x $ROOT_STORE_DIR/aus_bio_outputs/current_output.7z -o$TMPDIR_SHARE/outputs
fi

#Load in the cache if it exists
if [[ -f  "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache.7z" ]]; then
	 7z_cmd x $ROOT_STORE_DIR/aus_bio_outputs/targets_cache.7z -o$TMPDIR_SHARE/code/R
fi

## Files are ready, set up env


cd $TMPDIR_SHARE/code/R/

if [ ! -v TMPDIR ]; then
		mkdir -p $TMPDIR_SHARE/tmp
		export TMPDIR=$TMPDIR_SHARE/tmp
fi

mkdir -p $TMPDIR_SHARE/logs
export LOGDIR=$TMPDIR_SHARE/logs

## Set up an R shell
#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases

alias R='srun nix develop github:PhDyellow/nix_r_dev_shell#devShells."x86_64-linux".r-shell -c R'
alias Rscript='srun nix develop github:PhDyellow/nix_r_dev_shell#devShells."x86_64-linux".r-shell -c Rscript'

## Run Targets

Rscript --vanilla -e "targets::tar_make()"

## Clean up

#Store the cache
7z_cmd u -mx=0 $TMPDIR_SHARE/code/R/targets_cache.7z  $TMPDIR_SHARE/code/R/_targets
rsync -irc $TMPDIR_SHARE/code/R/targets_cache.* $ROOT_STORE_DIR/aus_bio_outputs

#Store the outputs
7z_cmd a "$TMPDIR_SHARE/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z"  $TMPDIR_SHARE/outputs/*
rsync -irc $TMPDIR_SHARE/*_outputs.* $ROOT_STORE_DIR/aus_bio_outputs
if [[ -f "$ROOT_STORE_DIR/aus_bio_outputs/current_output.7z" ]]; then
		unlink $ROOT_STORE_DIR/aus_bio_outputs/current_output.7z
fi
ln -s "$ROOT_STORE_DIR/aus_bio_outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z" $ROOT_STORE_DIR/aus_bio_outputs/current_output.7z

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/data/

#clean up TMPDIR_SHARE
chmod 770 -R $TMPDIR_SHARE
rm -r $TMPDIR_SHARE
#done
