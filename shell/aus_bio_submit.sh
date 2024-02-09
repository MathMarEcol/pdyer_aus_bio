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
				## Moving into aus_bio_bunya_batch
				## Root directories
				export ROOT_STORE_DIR="/QRISdata/$3" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE="/scratch/user/$(whoami)/aus_bio_scratch_${date}"
				mkdir -p $TMPDIR_SHARE
				## Set up the nix dir.
				## put nix into ~/bin
				# curl -L https://hydra.nixos.org/job/nix/maintenance-2.20/buildStatic.x86_64-linux/latest/download-by-type/file/binary-dist > ~/bin/nix


				# cat 'export NIX_CONFIG="use-xdg-base-directories = true
# ssl-cert-file = /etc/ssl/certs/ca-bundle.crt
# store = ~/nix_store
# extra-experimental-features = flakes nix-command recursive-nix
# max-jobs = 1
# cores = 1
# "' > ~/nix_env_aus_bio.sh
				# source ~/nix_env_aus_bio.sh
				export NIX_CONFIG="use-xdg-base-directories = true
ssl-cert-file = /etc/ssl/certs/ca-bundle.crt
store = ~/nix_store
extra-experimental-features = flakes nix-command recursive-nix
max-jobs = 1
cores = 1"
				export NIX_SSL_CERT_FILE=/etc/ssl/certs/ca-bundle.crt

				## tell BLAS that we are using gnu openmp
				export MKL_THREADING_LAYER="GNU"
				export MKL_INTERFACE_LAYER="GNU,LP64"

				export SLURM_EXPORT_ENV=ROOT_STORE_DIR,TMPDIR_SHARE,GIT_BRANCH,R_FUTURE_GLOBALS_MAXSIZE,date,HOME,LANG,NIX_CONFIG,NIX_SSL_CERT_FILE,MKL_THREADING_LAYER,MKL_INTERFACE_LAYER

				## Control job goes to "general" partition
				cd $TMPDIR_SHARE
				sbatch aus_bio_bunya_batch.sh -A $2
				;;

		prime-ai*)
				## Root directories
				export ROOT_STORE_DIR="/para/resources/qris_sandbox/$3" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE="/para/resources/hpc_sandbox/scratch/user/$(whoami)/aus_bio_scratch_${date}"

				## tell BLAS that we are using gnu openmp
				export MKL_THREADING_LAYER="GNU"
				export MKL_INTERFACE_LAYER="GNU,LP64"

				## Control job goes to "cpu" partition
				export SLURM_EXPORT_ENV=ROOT_STORE_DIR,TMPDIR_SHARE,GIT_BRANCH,R_FUTURE_GLOBALS_MAXSIZE,date,HOME,LANG,NIX_BUILD_CORES,MKL_THREADING_LAYER,MKL_INTERFACE_LAYER
				sbatch aus_bio_prime_ai_batch.sh -A $2
		;;

		*)
				echo "Hostname not recognised"
				exit 1
		;;
esac






