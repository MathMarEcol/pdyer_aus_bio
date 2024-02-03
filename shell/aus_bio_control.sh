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
		7z_cmd x $ROOT_STORE_DIR/aus_bio_outputs/current_output.7z -o$TMPDIR_SHARE/outputs/
fi

#Load in the cache if it exists
if [[ -f  "$ROOT_STORE_DIR/aus_bio_outputs/targets_cache.7z" ]]; then
	 7z_cmd x $ROOT_STORE_DIR/aus_bio_outputs/targets_cache.7z -o$TMPDIR_SHARE/code/R
fi

## Files are ready, set up env

## Prepare Apptainer
mkdir -p $TMPDIR_SHARE/sif_images
rsync -irc $APPTAINER_SIF_DIR/$APPTAINER_SIF_FILE $TMPDIR_SHARE/sif_images
export APPTAINER_SIF_RUN=$TMPDIR_SHARE/sif_images/$APPTAINER_SIF_FILE


#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases


#Because this is a apptainer, it can only see directories specified in "APPTAINER_BIND" env var.
export TMPDIR_REAL=$(realpath $TMPDIR_SHARE)
export APPTAINER_BIND=${TMPDIR_REAL:-""}
## Needed for cases when sbatch needs to be wrapped when the control process cannot launch jobs from a compute node.
## Use with caution, allows any executable to be overriden by files in home dir
export APPTAINERENV_APPEND_PATH="/home/${USER}/bin"
## Add env vars into container needed by code
export APPTAINERENV_TMPDIR_SHARE=$TMPDIR_SHARE
mkdir -p $TMPDIR_SHARE/apptainer_cache
mkdir -p $TMPDIR_SHARE/apptainer_tmp
export APPTAINER_CACHEDIR=$TMPDIR_SHARE/apptainer_cache
export APPTAINER_TMPDIR=$TMPDIR_SHARE/apptainer_tmp

## Debugging, assume I don't need modules for now
if [ $COPY_WRAPPERS -gt 0 ]
then
#Always use the latest module, so I don't need to remember to copy it manually
#but only if I really am on the HPC
mkdir -p ~/privatemodules
#Also make sure I have a wrapped version of qsub and qstat for the containers
mkdir -p ~/bin
   if [ $SCHEDULER == "pbs" ]
   then
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/aus_bio_module_pbs.lua \
         ~/privatemodules/aus_bio_module.lua
   module load use.own
   module load aus_bio_module.lua
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/qsub_wrap.sh \
      ~/bin/qsub
   chmod u+x ~/bin/qsub
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/qdel_wrap.sh \
   ~/bin/qdel
   chmod u+x ~/bin/qdel
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/qstat_wrap.sh \
      ~/bin/qstat
   chmod u+x ~/bin/qstat
   fi
   if [ $SCHEDULER == "slurm" ]
   then
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/aus_bio_module_slurm.tcl \
         ~/privatemodules/aus_bio_module.tcl
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/sbatch_wrap.sh \
      ~/bin/sbatch
   chmod u+x ~/bin/sbatch
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/scancel_wrap.sh \
   ~/bin/scancel
   chmod u+x ~/bin/scancel
   cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/squeue_wrap.sh \
      ~/bin/squeue
   chmod u+x ~/bin/squeue
   fi
fi




## Run Targets

cd $TMPDIR_SHARE/code/R/

if [! -v TMPDIR ]; then
		mkdir -p $TMPDIR_SHARE/tmp
		export TMPDIR=$TMPDIR_SHARE/tmp
fi

mkdir -p $TMPDIR_SHARE/logs
export LOGDIR=$TMPDIR_SHARE/logs

## Some env vars are leaking into container and breaking
## downloads.
## Unset if present
if [ -v NIX_SSL_CERT_FILE ]; then
		export TMP_NSCF=$NIX_SSL_CERT_FILE
		unset -v NIX_SSL_CERT_FILE
fi

apptainer exec $APPTAINER_SIF_RUN  Rscript --vanilla -e "targets::tar_make()"

if [ -v TMP_NSCF ]; then
		export NIX_SSL_CERT_FILE=$TMP_NSCF
fi


## Clean up

#Store the cache
7z_cmd u -mx=0 $TMPDIR_SHARE/code/R/targets_cache.7z  $TMPDIR_SHARE/code/R/_targets
rsync -irc $TMPDIR_SHARE/code/R/targets_cache.* $ROOT_STORE_DIR/aus_bio_outputs

#Store the outputs
7z_cmd a "$TMPDIR_SHARE/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z"  $TMPDIR_SHARE/outputs
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
