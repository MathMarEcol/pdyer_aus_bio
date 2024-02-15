#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=40G
#SBATCH --job-name=aus_bio
#SBATCH --time=12:00:00
#SBATCH -o setup_output_%j
#SBATCH -e setup_error_%j



srun nix build --no-link --max-jobs ${SLURM_CPUS_PER_TASK} --print-out-paths github:PhDyellow/nix_r_dev_shell#devShells.x86_64-linux.r-shell
