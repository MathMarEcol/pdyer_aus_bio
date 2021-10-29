#!/usr/bin/env bash
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1
#SBATCH --mem-per-cpu=80GB  # on getafix, singularity will only be available if >16GB is requested
#SBATCH --time=7-00:00
#SBATCH -o slurm.%j.out  # STDOUT
#SBATCH -e slurm.%j.err  # STDERR

# use "-v GIT_BRANCH=tagname -v WORKER_N=2" during qsub
# eg qsub -v GIT_BRANCH=tagname -v WORKER_N=2 ./aus_bio_pbs_submit.sh
# Example use:
# GIT_BRANCH=f_targets_debug WORKER_N_S1=20 WORKER_MEM_S1=20GB WORKER_N_S2=1 WORKER_MEM_S2=100GB WORKER_RUNTIME=7-00:00 sbatch aus_bio_getafix_submit.sh
set -euo pipefail
export ROOT_STORE_DIR="/data/uqpdyer/resources/QRISdata/" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE="/data/uqpdyer/resources/hpc_scratch/slurm_${SLURM_JOB_ID}/"
export COPY_MODULES=1 #copy HPC modules
export GIT_BRANCH=${GIT_BRANCH:-"develop"} #to submit a particular tag, use -v "GIT_BRANCH=tagname" during qsub
export WORKER_N=${WORKER_N:-2} # 2 workers by default,
export WORKER_MEM=${WORKER_MEM:-20GB} #20GB per worker by default
export WORKER_CORES=${WORKER_CORES:-1} # 1 core by default,

# On the Slurm scheduler, I can use resources more efficiently
# by running tar_make_clustermq() in stages, with each
# stage defined by a change in worker profiles
# (N workers, ram and cpus per worker)

# S1: lots of small jobs
export WORKER_N_S1=${WORKER_N_S1:-20}
export WORKER_MEM_S1=${WORKER_MEM_S1:-20GB}
export WORKER_CORES_S1=${WORKER_CORES_S1:-1} # 1 core by default,
TARGETS_S1='c('
TARGETS_S1+='bac_site_csv,'
TARGETS_S1+='bac_otu_csv,'
TARGETS_S1+='marine_map,'
TARGETS_S1+='mapfile_location,'
TARGETS_S1+='env_poly,'
TARGETS_S1+='env_extent,'
TARGETS_S1+='env_domain,'
TARGETS_S1+='zoo_data_dir,'
TARGETS_S1+='zoo_load_script,'
TARGETS_S1+='fish_taxon_file,'
TARGETS_S1+='fish_data_dir,'
TARGETS_S1+='phy_data_dir,'
TARGETS_S1+='phy_load_script,'
TARGETS_S1+='biooracle_folder,'
TARGETS_S1+='fish_long,'
TARGETS_S1+='bac_long,'
TARGETS_S1+='zoo_long,'
TARGETS_S1+='phy_long,'
TARGETS_S1+='all_bio_env,'
TARGETS_S1+='all_bio_long,'
TARGETS_S1+='gf_survey,'
TARGETS_S1+='gfbootstrap_survey'
TARGETS_S1+=')'
export TARGETS_S1
# S2: Combining gfbootstrap models
export WORKER_N_S2=${WORKER_N_S2:-1}
export WORKER_MEM_S2=${WORKER_MEM_S2:-100GB}
export WORKER_CORES_S2=${WORKER_CORES_S2:-1} # 1 core by default,
TARGETS_S2='c('
TARGETS_S2+='gfbootstrap_combined_tmp'
TARGETS_S2+=')'
export TARGETS_S2
# S3: operating on gfbootstrap objects
export WORKER_N_S3=${WORKER_N_S3:-20}
export WORKER_MEM_S3=${WORKER_MEM_S3:-60GB}
export WORKER_CORES_S3=${WORKER_CORES_S3:-1} # 1 core by default,
TARGETS_S3='c('
TARGETS_S3+='gfbootstrap_combined,'
TARGETS_S3+='gfbootstrap_predicted,'
TARGETS_S3+='gfbootstrap_caster'
TARGETS_S3+=')'
export TARGETS_S3
# S4: plotting
export WORKER_N_S4=${WORKER_N_S4:-20}
export WORKER_MEM_S4=${WORKER_MEM_S4:-20GB}
export WORKER_CORES_S4=${WORKER_CORES_S4:-1} # 1 core by default,
TARGETS_S4='c('
TARGETS_S4+='gfbootstrap_plotted,'
TARGETS_S4+='gfbootstrap_coverage'
TARGETS_S4+=')'
export TARGETS_S4

export SCHEDULER="slurm"
echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building with [${WORKER_N}] Workers under [${SCHEDULER}]"

## Needed for compute nodes on Getafix
if [ ${SLURM_CLUSTER_NAME} == faculty-cluster ]
then
    export https_proxy="http://its-ri-proxy01.hpc.dc.uq.edu.au:3128"
fi

source $SLURM_SUBMIT_DIR/aus_bio_content.sh
