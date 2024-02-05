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
		bun*)
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
				## Root directories
				export ROOT_STORE_DIR="/para/resources/qris_sandbox/$3" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE="/para/resources/hpc_sandbox/scratch/user/$(whoami)/aus_bio_scratch_${date}"
				## Container location. Will be moved to scratch before execution
				export APPTAINER_SIF_DIR=$ROOT_STORE_DIR/sif_images
				export APPTAINER_SIF_FILE=aus_bio_apptainer_r.sif
				## Mount slurm into container
				## Nix is horrible for this
				export APPTAINERENV_SLURM_CONF=/nix/store/cjmalw9cc69vh0b3sl8v2af6pj1iahym-etc-slurm/slurm.conf
				export APPTAINERENV_APPEND_PATH=/nix/store/lp79sxc2j3r69finmb9c0wr6f04h4m3m-slurm-23.02.7.1/bin
				export APPTAINERENV_APPEND_PATH=${APPTAINERENV_APPEND_PATH}:/nix/store/9zw2qhrnsfrj5n1682f0qbqdv7kdwl4q-munge-0.5.15/bin

				# export APPTAINER_BIND=/nix/store/fqpibzgmziwzs6ypfxwz67528b5h4qby-wrappedSlurm/bin
				export APPTAINER_BIND=/nix/store/cjmalw9cc69vh0b3sl8v2af6pj1iahym-etc-slurm
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/lp79sxc2j3r69finmb9c0wr6f04h4m3m-slurm-23.02.7.1
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/9zw2qhrnsfrj5n1682f0qbqdv7kdwl4q-munge-0.5.15
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/9y8pmvk8gdwwznmkzxa6pwyah52xy3nk-glibc-2.38-27
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/bi51rzf2g7jlfqccqv4c3yswzy4ah80f-slurm.conf
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/9558j3vg39rznx9n318cqyg4bvyvhvnf-plugstack.conf
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/6d38l2007fk6x5mzns0ccw0ackg5d79l-cgroup.conf
				export APPTAINER_BIND=${APPTAINER_BIND},/nix/store/vwpki5nihab5yssssd54kd8f5c32sg7f-lz4-1.9.4

				export APPTAINER_BIND=${APPTAINER_BIND},/var/run/munge
				export APPTAINER_BIND=${APPTAINER_BIND},/run/munge
				export APPTAINER_BIND=${APPTAINER_BIND},/etc/passwd,/etc/group



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






