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

cd $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio


#For playing nice on the hpc, put all data into a cluster network disk, don't leave it on UQ RDM
mkdir -p $TMPDIR_SHARE/Q1215
mkdir -p $TMPDIR_SHARE/Q1216

#Have to carefully make sure all files make it over
#"Input" files are stored in 90days
#sync root directories up front, either from QRIS or 90 days
#I think I should manually sync between 90days and QRIS
# and automatically between 90days and $TMPDIR_SHARE
#which means I don't need $PBS_O_WORKDIR
#Inputs

#All of the datasets are in smallish blocks of files. no more than a few dozen files per dataset, most are less than 5
mkdir -p $TMPDIR_SHARE/Q1215/aus_microbiome/marine_bacteria
rsync -irc $ROOT_STORE_DIR/Q1215/aus_microbiome/marine_bacteria/Bacteria.csv $TMPDIR_SHARE/Q1215/aus_microbiome/marine_bacteria/
rsync -irc $ROOT_STORE_DIR/Q1215/aus_microbiome/marine_bacteria/contextual.csv $TMPDIR_SHARE/Q1215/aus_microbiome/marine_bacteria/
mkdir -p $TMPDIR_SHARE/Q1215/bioORACLE
rsync -irc $ROOT_STORE_DIR/Q1215/bioORACLE $TMPDIR_SHARE/Q1215/
mkdir -p $TMPDIR_SHARE/Q1215/AusCPR
rsync -irc $ROOT_STORE_DIR/Q1215/AusCPR/ $TMPDIR_SHARE/Q1215/AusCPR
mkdir -p $TMPDIR_SHARE/Q1215/ShapeFiles/World_EEZ_v8
rsync -irc $ROOT_STORE_DIR/Q1215/ShapeFiles/World_EEZ_v8 $TMPDIR_SHARE/Q1215/ShapeFiles/
mkdir -p $TMPDIR_SHARE/Q1215/Watson_Fisheries_Catch_Data/Version5/Output
rsync -irc $ROOT_STORE_DIR/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/Annual_TotalCatchSpecies $TMPDIR_SHARE/Q1215/Watson_Fisheries_Catch_Data/Version5/Output
rsync -irc $ROOT_STORE_DIR/Q1215/Watson_Fisheries_Catch_Data/Version5/Output/TaxonomicData.rds $TMPDIR_SHARE/Q1215/Watson_Fisheries_Catch_Data/Version5/Output

#Essential Code

#mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
mkdir -p $TMPDIR_SHARE/Q1216/pdyer
cd $TMPDIR_SHARE/Q1216/pdyer
git clone -b $GIT_BRANCH --single-branch https://github.com/MathMarEcol/pdyer_aus_bio.git

#capture current git hash for use later
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
git_hash=$(git rev-parse --short HEAD)
date_run=$(date +%Y-%m-%d_%H-%M-%S)
#rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/drake_plan.R $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
#The drake cache contains previous results, and is needed to avoid recaclulating stuff.

if [[ -f  "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/drake_cache.7z" ]]
then
   rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/drake_cache.7z $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
   cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
   7za x $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/drake_cache.7z
fi
#rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/pbs_clustermq.tmpl $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
#
#Set up the output directory
#I put in current outputs, in order to avoid replotting. Update, I want to replot
mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs

#Then run from the local disk
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code

#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases

if [ $COPY_MODULES -gt 0 ]
then
#Always use the latest module, so I don't need to remember to copy it manually
#but only if I really am on the HPC
cp $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/aus_bio_module.lua \
      ~/privatemodules/aus_bio_module.lua
#Also make sure I have a wrapped version of qsub and qstat for the containers
cp $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/qsub_wrap.sh \
   ~/bin/qsub
cp $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/qdel_wrap.sh \
~/bin/qdel
cp $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/qstat_wrap.sh \
   ~/bin/qstat
module load use.own
module load aus_bio_module
fi

#Because this is a singularity, it can only see directories specified in aus_bio_module's "SINGULARITY_BIND" env var.
TMPDIR_REAL=$(realpath $TMPDIR_SHARE)
cd $TMPDIR_REAL/Q1216/pdyer/pdyer_aus_bio

Rscript code/drake_plan.R

#Store the drake cache
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
tar -czf $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/drake_cache.tar.gz  ./drake_cache
7za u -mx=0 $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/drake_cache.7z  ./drake_cache
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/drake_cache.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio
#Store the outputs

tar -czf "$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/${date_run}_$GIT_BRANCH_${git_hash}_outputs.tar.gz"  ./outputs
7za a "$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/${date_run}_$GIT_BRANCH_${git_hash}_outputs.7z"  ./outputs
rsync -irc ""$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/*_outputs.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs_history

# #copy outputs to an archive
# mkdir -p $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs_history
# for file in $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs/* ; do
#     cp -r "$file" "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs_history/${date_run}_${git_hash}_${file##*/}"
# done

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMPDIR_SHARE/Q1215/bioORACLE $ROOT_STORE_DIR/Q1215/

#clean up TMPDIR_SHARE
rm -r $TMPDIR_SHARE/*
#done
