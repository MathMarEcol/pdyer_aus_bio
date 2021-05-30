#!/usr/bin/env bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=2:mem=60GB
#PBS -l walltime=12:00:00
#PBS -q Single
#
#

# use "-v GIT_BRANCH=tagname -v WORKERS=2" during qsub
# eg qsub -v GIT_BRANCH=tagname -v WORKERS=2 ./aus_bio_pbs_submit.sh
set -euo pipefail
ROOT_STORE_DIR="/QRISdata/" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
TMPDIR_SHARE="/scratch/uqpdyer/pbs.$PBS_JOBID"
COPY_MODULES=1 #copy HPC modules
GIT_BRANCH=${GIT_BRANCH:-"develop"} #to submit a particular tag, use -v "GIT_BRANCH=tagname" during qsub
WORKERS=${WORKERS:-2} # 2 workers by default,
echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building with [${WORKERS}] Workers"
source $PBS_O_WORKDIR/aus_bio_content.sh
