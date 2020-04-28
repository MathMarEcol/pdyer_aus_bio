#!/bin/sh

ssh -i /home/uqpdyer/.ssh/selfkey ${PBS_O_HOST:-"no_host_not_in_a_pbs_job"} qdel "$@"
