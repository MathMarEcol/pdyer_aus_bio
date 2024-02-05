#!/usr/bin/env sh

ssh -i ~/.ssh/selfkey ${SLURM_SUBMIT_HOST:-localhost} sbatch "$@"
