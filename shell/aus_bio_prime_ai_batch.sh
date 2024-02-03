#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --job-name=aus_bio
#SBATCH --time=168:00:00
#SBATCH --partition=cpu
#SBATCH --account=$1
#SBATCH -o aus_bio_output_%j
#SBATCH -e aus_bio_error_%j


## Contains job submission parameters for control job
echo "Running"
srun aus_bio_control.sh
