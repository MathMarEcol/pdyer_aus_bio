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

#by default, PBS begins in the home dir, but the env var $PBS_O_WORKDIR contains the path to this script.
#Assuming that this job was called from /???30days???/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code
#I no longer assume anything about the calling directory, but depend on the calling script to set the important variables

cd $ROOT_STORE_DIR
mkdir -p $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs

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
7z_cmd x $ROOT_STORE_DIR/Q1215/aus_microbiome/marine_bacteria_AustralianMicrobiome-2019-07-03T093815-csv.zip -o$TMP_DATA_DIR/aus_microbiome/marine_bacteria
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
   7z_cmd x $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z -o$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/
else
   mkdir -p $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/outputs
fi

if [[ -f  "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/targets_cache.7z" ]]
then
   rsync -irc $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/targets_cache.7z $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
   cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
   7z_cmd x $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/targets_cache.7z
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

#Because this is a singularity, it can only see directories specified in aus_bio_module's "SINGULARITY_BIND" env var.
export TMPDIR_REAL=$(realpath $TMPDIR_SHARE)
cd $TMPDIR_REAL/Q1216/pdyer/pdyer_aus_bio/code/R

if [ $SCHEDULER == "slurm" ]
then
   export aus_bio_sif="/data/uqpdyer/resources/hpc_scratch/r-singularity-aus-bio.img"
   export bindpath=${TMPDIR:-""}
   export bindpath=${bindpath},/data
   export SINGULARITY_BIND=$bindpath
   export SINGULARITYENV_APPEND_PATH="/home/${USER}/bin"
   ## Breaking the pipeline up into stages. see aus_bio_getafix_submit.sh for details
      ## S1
      export WORKER_MEM=$WORKER_MEM_S1
      export WORKER_CORES=$WORKER_CORES_S1
      singularity exec  $aus_bio_sif  Rscript --vanilla -e "targets::tar_make_clustermq(${TARGETS_S1}, workers = ${WORKER_N_S1}, log_worker = TRUE)"

      ## S2
      export WORKER_MEM=$WORKER_MEM_S2
      export WORKER_CORES=$WORKER_CORES_S2
      singularity exec  $aus_bio_sif  Rscript --vanilla -e "targets::tar_make_clustermq(${TARGETS_S2}, workers = ${WORKER_N_S2}, log_worker = TRUE)"

      ## S3
      export WORKER_MEM=$WORKER_MEM_S3
      export WORKER_CORES=$WORKER_CORES_S3
      singularity exec  $aus_bio_sif  Rscript --vanilla -e "targets::tar_make_clustermq(${TARGETS_S3}, workers = ${WORKER_N_S3}, log_worker = TRUE)"

      ## S4
      export WORKER_MEM=$WORKER_MEM_S4
      export WORKER_CORES=$WORKER_CORES_S4
      singularity exec  $aus_bio_sif  Rscript --vanilla -e "targets::tar_make_clustermq(${TARGETS_S4}, workers = ${WORKER_N_S4}, log_worker = TRUE)"
else
    ## Breaking the pipeline up into stages
    ## Most target branches use one core and a modest amount of
    ## memory. They can easily be run in parallel using many R target
    ## workers.
    ## A few targets use linear algebra libraries that use either
    ## the GPU or all available cores within a single target branch.
    ## These must be run sequentially on a single machine, or extreme
    ## slowdowns and crashes occur.
    ## currently these are:
    ## gfbootstrap_predicted, cluster_env_extrapolate_present
    ## Each stage is a set of targets that have the same set of
    ## resource needs, and don't depend on any targets in later stages.

    ## S1 - parallel workers
    ## Everything up to gfbootstrap_combined
    ## This might miss a few "leaf" branches, that is ok
    Rscript -e "targets::tar_make_clustermq(gfbootstrap_combined, workers = ${WORKERS}, log_worker = TRUE)"

    ## S2 - single worker GPU or lots of RAM/CORES
    ## gfbootstrap_predicted and extrapolation targets.
		## Assigning sites to clusters is also memory heavy
    Rscript -e "targets::tar_make_clustermq(c(gfbootstrap_predicted, starts_with('cluster_env_extrapolate_'), starts_with('cluster_env_assign_cluster_')), workers = 1, log_worker = TRUE)"

    ## S3 - All remaining targets
   Rscript -e "targets::tar_make_clustermq(workers = ${WORKERS}, log_worker = TRUE)"
fi

#Store the drake cache
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R
7z_cmd u -mx=0 $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/targets_cache.7z  ./_targets
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/code/R/targets_cache.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs

#Store the outputs
cd $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio
7z_cmd a "$TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z"  ./outputs
rsync -irc $TMPDIR_SHARE/Q1216/pdyer/pdyer_aus_bio/*_outputs.* $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs
cp "$ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/${date_run}_${GIT_BRANCH}_${git_hash}_outputs.7z" $ROOT_STORE_DIR/Q1216/pdyer/pdyer_aus_bio/outputs/current_output.7z

#The downloaded variables from bioORACLE are also worth saving
rsync -irc $TMP_DATA_DIR/bioORACLE $ROOT_STORE_DIR/Q1215/

#clean up TMPDIR_SHARE
chmod 770 -R $TMPDIR_SHARE/*
rm -r $TMPDIR_SHARE/*
#done
