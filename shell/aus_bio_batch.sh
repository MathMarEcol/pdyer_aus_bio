#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --job-name=aus_bio

## Contains job submission parameters for control job
cd $TMPDIR
srun $SCRATCH_PIPELINE_DIR/aus_bio_control.sh
