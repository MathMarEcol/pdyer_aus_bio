#!/usr/bin/env bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=4:mem=60GB
#PBS -l walltime=25:00:00
#
#

set -euo pipefail
ROOT_STORE_DIR="/90days/uqpdyer/rdm_mirror" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
TMPDIR_SHARE="/30days/uqpdyer/pbs.$PBS_JOBID"
COPY_MODULES=1 #copy HPC modules

source $PBS_O_WORKDIR/aus_bio_pbs_content.sh
