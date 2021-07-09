#!/usr/bin/env bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=1:mem=3GB
#PBS -l walltime=167:00:00
#PBS -q Single
#
#

# use "-v GIT_BRANCH=tagname -v WORKERS=2" during qsub
# eg qsub -v GIT_BRANCH=tagname -v WORKERS=2 ./aus_bio_pbs_submit.sh
set -euo pipefail
export ROOT_STORE_DIR="/QRISdata/" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
export TMPDIR_SHARE="/scratch/uqpdyer/pbs.$PBS_JOBID"
export COPY_MODULES=1 #copy HPC modules
export GIT_BRANCH=${GIT_BRANCH:-"develop"} #to submit a particular tag, use -v "GIT_BRANCH=tagname" during qsub
export WORKERS=${WORKERS:-2} # 2 workers by default,
export SCHEDULER="pbs"
echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building with [${WORKERS}] Workers under [${SCHEDULER}]"
source $PBS_O_WORKDIR/aus_bio_content.sh
