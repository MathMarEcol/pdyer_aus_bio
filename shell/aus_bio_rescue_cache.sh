#!/usr/bin/env bash
set -euo pipefail


## Script to set up and submit the targets pipeline
## usage: ./aus_bio_rescue_cache.sh tmpdir qrisrefQxxxx acctstring
## Generally expects to be on a node that can submit jobs
## Is not a job

## Uses hostnames to decide appropriate pathways
HName=$(hostname -s)


## Common parameters

echo "rescuing cache at $1"

## Submit a control job that
## 1. sets up the temp files and env
## 2. Starts R targets
## 3. Cleans up temp files
## Master job has minimal compute requirements, but needs a long walltime




case $HName in
		bun*)
				## Moving into aus_bio_bunya_batch
				## Root directories
				export ROOT_STORE_DIR="/QRISdata/$2" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE=$1
				# cat 'export NIX_CONFIG="use-xdg-base-directories = true
# ssl-cert-file = /etc/ssl/certs/ca-bundle.crt
# store = ~/nix_store
# extra-experimental-features = flakes nix-command recursive-nix
# max-jobs = 1
# cores = 1
# "' > ~/nix_env_aus_bio.sh
				# source ~/nix_env_aus_bio.sh

				export SBATCH_ACCOUNT=$3
				export SBATCH_PARTITION=general

				export SLURM_EXPORT_ENV=ROOT_STORE_DIR,TMPDIR_SHARE,HOME,LANG,SBATCH_ACCOUNT,SBATCH_PARTITION

				## Control job goes to "general" partition
				;;

		prime-ai*)
				## Root directories
				export ROOT_STORE_DIR="/para/resources/qris_sandbox/$2" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE=$1

				export SBATCH_PARTITION=cpu

				## Control job goes to "cpu" partition
				export SLURM_EXPORT_ENV=ROOT_STORE_DIR,TMPDIR_SHARE,date,HOME,LANG,NIX_BUILD_CORES,SBATCH_PARTITION
		;;

		*)
				echo "Hostname not recognised"
				exit 1
		;;
esac

cp aus_bio_rescue_job.sh $TMPDIR_SHARE
cd $TMPDIR_SHARE
sbatch aus_bio_rescue_job.sh
