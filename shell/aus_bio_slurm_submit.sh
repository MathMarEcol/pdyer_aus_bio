#!/usr/bin/env bash
#SBATCH ntasks=1
#SBATCH cpus-per-task=1
#SBATCH --mem-per-cpu=4GB
#SBATCH --time=7-00:00
#SBATCH -o slurm.%N.%j.%a.out  # STDOUT
#SBATCH -e slurm.%N.%j.%a.err  # STDERR

# use "-v GIT_BRANCH=tagname -v WORKERS=2" during qsub
# eg qsub -v GIT_BRANCH=tagname -v WORKERS=2 ./aus_bio_pbs_submit.sh
set -euo pipefail
export ROOT_STORE_DIR="/data/uqpdyer/resources/QRISdata/" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE="/data/uqpdyer/resources/hpc_scratch/slurm_${SLURM_JOB_ID}/"
export COPY_MODULES=1 #copy HPC modules
export GIT_BRANCH=${GIT_BRANCH:-"develop"} #to submit a particular tag, use -v "GIT_BRANCH=tagname" during qsub
export WORKERS=${WORKERS:-2} # 2 workers by default,
export SCHEDULER="slurm"
echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building with [${WORKERS}] Workers under [${SCHEDULER}]"

## Needed for compute nodes on Getafix
if [ ${SLURM_CLUSTER_NAME} == faculty-cluster]
then
    export https_proxy="http://its-ri-proxy01.hpc.dc.uq.edu.au:3128"
fi

source $SLURM_SUBMIT_DIR/aus_bio_content.sh
