#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --job-name=aus_bio
#SBATCH --time=320:00:00
#SBATCH --partition=general
#SBATCH -o aus_bio_output_%j
#SBATCH -e aus_bio_error_%j


## Contains job submission parameters for control job

export SLURM_EXPORT_ENV=ALL
cd $TPMDIR_SHARE
srun aus_bio_control.sh
