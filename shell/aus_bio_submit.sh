#!/usr/bin/env bash
set -euo pipefail


## Script to set up and submit the targets pipeline
## usage: ./aus_bio_submit.sh branchname acctstring qrisrefQxxxx
## Generally expects to be on a node that can submit jobs
## Is not a job

## Uses hostnames to decide appropriate pathways
HName=$(hostname -s)


## Common parameters

export GIT_BRANCH=${1:-"develop"} #to submit a particular tag

## This must get to workers TODO
export R_FUTURE_GLOBALS_MAXSIZE=${R_FUTURE_GLOBALS_MAXSIZE:-100000000000}

printf -v date '%(%Y%m%dT%H%M%S)T' -1

echo "Checking out git branch: ${GIT_BRANCH}"
echo "Building on ${HName}"

## Submit a control job that
## 1. sets up the temp files and env
## 2. Starts R targets
## 3. Cleans up temp files
## Master job has minimal compute requirements, but needs a long walltime




case $HName in
		bunya*)
				export ROOT_STORE_DIR="/QRISdata/$3" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE="/scratch/user/$(whoami)/aus_bio_scratch_${date}"
				export APPTAINER_SIF_DIR=$ROOT_STORE_DIR/sif_images
				export APPTAINER_SIF_FILE=aus_bio_apptainer_r.sif
				export COPY_WRAPPERS=0 #don't copy HPC module files locally
				## tell BLAS that we are using gnu openmp
				export MKL_THREADING_LAYER="GNU"
				export MKL_INTERFACE_LAYER="GNU,LP64"
				## Control job goes to "general" partition
				sbatch aus_bio_bunya_batch.sh $2 general
				;;

		prime-ai*)
				export ROOT_STORE_DIR="/para/resources/qris_sandbox/$3" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE="/para/resources/hpc_sandbox/scratch/user/$(whoami)/aus_bio_scratch_${date}"
				export APPTAINER_SIF_DIR=$ROOT_STORE_DIR/sif_images
				export APPTAINER_SIF_FILE=aus_bio_apptainer_r.sif
				export COPY_WRAPPERS=0 #don't copy HPC module files locally
				## tell BLAS that we are using gnu openmp
				export MKL_THREADING_LAYER="GNU"
				export MKL_INTERFACE_LAYER="GNU,LP64"

				## Control job goes to "cpu" partition
        sbatch aus_bio_prime_ai_batch.sh -A $2
		;;

		*)
				echo "Hostname not recognised"
				exit 1
		;;
esac






