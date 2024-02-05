#!/bin/sh

ssh -i ~/.ssh/selfkey ${SLURM_SUBMIT_HOST:-localhost} squeue "$@"
