#!/bin/sh

ssh -i /home/uqpdyer/.ssh/selfkey ${SLURM_SUBMIT_HOST:-"no_host_not_in_a_slurm_job"} squeue "$@"
