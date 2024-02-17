#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --job-name=aus_bio
#SBATCH -o logs/aus_bio_output_%j
#SBATCH -e logs/aus_bio_error_%j


## Contains job submission parameters for control job
cd $TMPDIR_SHARE
srun aus_bio_control.sh
