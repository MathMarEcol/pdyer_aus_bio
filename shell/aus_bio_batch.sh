#!/usr/bin/env bash
#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --cpus-per-task=1
#SBATCH --mem=4G
#SBATCH --job-name=aus_bio

# Copyright 2017-2024 Philip Dyer
# SPDX-License-Identifier: GPL-3.0-only

## Contains job submission parameters for control job
cd $TMPDIR
srun $SCRATCH_PIPELINE_DIR/aus_bio_control.sh
