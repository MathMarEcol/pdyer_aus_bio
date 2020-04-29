#!/bin/bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=4:mem=60GB
#PBS -l walltime=25:00:00



#by default, PBS begins in the home dir, but the env var $PBS_O_WORKDIR contains the path to this script.
#Assuming that this job was called from /???30days???/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code
ROOT_STORE_DIR="/90days/uqpdyer/rdm_mirror" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
TMPDIR_SHARE="/30days/uqpdyer/pbs.$PBS_JOBID"
cd $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio
#capture current git hash for use later
git_hash=$(git rev-parse --short HEAD)
date_run=$(date +%Y-%m-%d_%H-%M-%S)


#For best performance, copy all needed files to the node local disk. This may break when drake submits jobs for me.
mkdir -p $TMPDIR_SHARE/Q1215
mkdir -p $TMPDIR_SHARE/Q1216

#Have to carefully make sure all files make it over
#"Input" files are stored in 90days
#sync root directories up front, either from QRIS or 90 days
#I think I should manually sync between 90days and QRIS
# and automatically between 90days and $TMPDIR_SHARE
#which means I don't need $PBS_O_WORKDIR
#Inputs

mkdir -p $TMPDIR_SHARE/Q1215/bioORACLE
rsync -irc $ROOT_STORE_DIR/Q1215/bioORACLE $TMPDIR_SHARE/Q1215/
mkdir -p $TMPDIR_SHARE/Q1215/AusCPR
rsync -irc $ROOT_STORE_DIR/Q1215/AusCPR/combined_copeped_jul19.csv $TMPDIR_SHARE/Q1215/AusCPR
mkdir -p $TMPDIR_SHARE/Q1215/ShapeFiles/World_EEZ_v8
rsync -irc $ROOT_STORE_DIR/Q1215/ShapeFiles/World_EEZ_v8 $TMPDIR_SHARE/Q1215/ShapeFiles/

#Essential Code

mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/drake_plan.R $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code
#The drake cache contains previous results, and is needed to avoid recaclulating stuff.
rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/drake_cache $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/pbs_clustermq.tmpl $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code

#Set up the .here file for the "here" package.
#here() expects to find a git repo or a Rproj file, but I want to minimise folder copying,
#so nothing above /code has been included
#The .here file is found by here() and used as the root folder in R scripts.
touch $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/.here

#Set up the output directory
#I put in current outputs, in order to avoid replotting. Update, I want to replot
mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs
# rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/ \
#           $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs

#Then run from the local disk
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code

#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases

#Always use the latest module, so I don't need to remember to copy it manually
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
#Because this is a singularity, it can only see directories specified in aus_bio_module's "SINGULARITY_BIND" env var.
Rscript $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/drake_plan.R


#Store the drake cache
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/drake_cache $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio
#Store the outputs
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs/ $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs
#copy outputs to an archive
mkdir -p $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs_history
for file in $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs/* ; do
    cp -r "$file" "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs_history/${date_run}_${git_hash}_${file##*/}"
done

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMPDIR_SHARE/Q1215/bioORACLE $ROOT_STORE_DIR/Q1215/
# $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/code/rdm_up.sh
