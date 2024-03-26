#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=55G
#SBATCH --job-name=aus_bio
#SBATCH --time=12:00:00
#SBATCH -o setup_output_%j
#SBATCH -e setup_error_%j

# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only

env
srun nix build --no-link --cores $SLURM_CPUS_PER_TASK --max-jobs $SLURM_CPUS_PER_TASK --print-out-paths github:PhDyellow/nix_r_dev_shell/dc0d948b1fd6c49bd4ba4c61e86ce90b19b37e30#devShells.x86_64-linux.r-shell
