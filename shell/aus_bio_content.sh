#!/usr/bin/env bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=4:mem=60GB
#PBS -l walltime=25:00:00

set -euo pipefail
#This script originally used $TMPDIR, a node local disk, for best performance.
#Drake and clustermq allowed me to use more nodes, but clustermq does not set up each node with the same
#local disk configuration.
#Instead, this script uses cluster drives, such as /30days/ to run the job so that all nodes can see
#the same data and folders, and cooperate in generating the results.


#by default, PBS begins in the home dir, but the env var $PBS_O_WORKDIR contains the path to this script.
#Assuming that this job was called from /???30days???/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code
#I no longer assume anything about the calling directory, but depend on the calling script to set the important variables

cd $ROOT_STORE_DIR

#Essential Code

#mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
git clone -b $GIT_BRANCH --single-branch https://github.com/MathMarEcol/pdyer_aus_bio.git ./code

#capture current git hash for use later
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
export git_hash=$(git rev-parse --short HEAD)
export date_run=$(date +%Y-%m-%d_%H-%M-%S)

#For playing nice on the hpc, put all data into a cluster network disk, don't leave it on UQ RDM
export TMP_DATA_DIR=$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/data

#Have to carefully make sure all files make it over
#"Input" files are stored in 90days
#sync root directories up front, either from QRIS or 90 days
#I think I should manually sync between 90days and QRIS
# and automatically between 90days and $TMPDIR_SHARE
#which means I don't need $PBS_O_WORKDIR
#Inputs

#All of the datasets are in smallish blocks of files. no more than a few dozen files per dataset, most are less than 5
mkdir -p $TMP_DATA_DIR/aus_microbiome/marine_bacteria
7za x $ROOT_STORE_DIR/Q1215/aus_microbiome/marine_bacteria_AustralianMicrobiome-2019-07-03T093815-csv.zip -o$TMP_DATA_DIR/aus_microbiome/marine_bacteria
mkdir -p $TMP_DATA_DIR/bioORACLE
rsync -irc $ROOT_STORE_DIR/Q1215/bioORACLE $TMP_DATA_DIR/
mkdir -p $TMP_DATA_DIR/AusCPR
rsync -irc $ROOT_STORE_DIR/Q1215/AusCPR/ $TMP_DATA_DIR/AusCPR
mkdir -p $TMP_DATA_DIR/ShapeFiles/World_EEZ_v8
rsync -irc $ROOT_STORE_DIR/Q1215/ShapeFiles/World_EEZ_v8 $TMP_DATA_DIR/ShapeFiles/
mkdir -p $TMP_DATA_DIR/Watson_Fisheries_Catch_Data/Version5/Output
rsync -irc $ROOT_STORE_DIR/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/Annual_TotalCatchSpecies $TMP_DATA_DIR/Watson_Fisheries_Catch_Data/Version5/Output
rsync -irc $ROOT_STORE_DIR/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/TaxonomicData.rds $TMP_DATA_DIR/Watson_Fisheries_Catch_Data/Version5/Output

#Set up the output directory
#I put in current outputs, in order to avoid replotting. Update, I want to replot. Update, I don't want to replot
mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
if [[ -f "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z" ]]; then
   7za -x $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z -o$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/
else
   mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs
fi

if [[ -f  "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/targets_cache.7z" ]]
then
   rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/targets_cache.7z $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
   cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
   7za x $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/targets_cache.7z
fi

#Then run from the local disk
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/

#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases

if [ $COPY_MODULES -gt 0 ]
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
cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/qdel_wrap.sh \
~/bin/qdel
cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/qstat_wrap.sh \
   ~/bin/qstat
fi
if [ $SCHEDULER == "slurm" ]
then
cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/aus_bio_module_slurm.tcl \
      ~/privatemodules/aus_bio_module.tcl
module load use.own
module load aus_bio_module.tcl
cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/sbatch_wrap.sh \
   ~/bin/sbatch
cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/scancel_wrap.sh \
~/bin/scancel
cp $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/shell/squeue_wrap.sh \
   ~/bin/squeue
fi
fi

echo "d"
#Because this is a singularity, it can only see directories specified in aus_bio_module's "SINGULARITY_BIND" env var.
export TMPDIR_REAL=$(realpath $TMPDIR_SHARE)
cd $TMPDIR_REAL/Q1216/pdyer/pdyer_aus_bio/code/R

Rscript -e "targets::tar_make_clustermq(workers = ${WORKERS}, log_worker = TRUE)"

echo "f"
#Store the drake cache
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
7za u -mx=0 $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/targets_cache.7z  ./_targets
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/targets_cache.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs

#Store the outputs
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
7za a "$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z"  ./outputs
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/*_outputs.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs
cp "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z" $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/Q1215/

#clean up TMPDIR_SHARE
chmod 770 -R $TMPDIR_SHARE
rm -r $TMPDIR_SHARE/*
#done
