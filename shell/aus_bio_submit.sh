#!/usr/bin/env bash
set -euo pipefail


## Script to set up and submit the targets pipeline
## usage: ./aus_bio_submit.sh branchname acctstring qrisrefQxxxx
## Generally expects to be on a node that can submit jobs
## Is not a job

## Uses hostnames to decide appropriate pathways
HName=$(hostname -s)


## Common parameters

export GIT_BRANCH=$1 #to submit a particular tag

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
				mkdir -p $TMPDIR_SHARE/logs
				cp ./aus_bio_batch.sh $TMPDIR_SHARE
				cp ./aus_bio_control.sh $TMPDIR_SHARE

				## Requires Nix
				## put nix into ~/bin
				# curl -L https://hydra.nixos.org/job/nix/maintenance-2.20/buildStatic.x86_64-linux/latest/download-by-type/file/binary-dist > ~/bin/nix
				## Once nix is on path and configured properly, use
				## ./setup_nix_shell_submit.sh
				## to pre-build the nix shell
				if ! command -v nix &> /dev/null
				then
						echo "Nix package manager is not found on the path.
For unprivileged users download static binary from https://hydra.nixos.org/job/nix/maintenance-2.20/buildStatic.x86_64-linux/latest/download-by-type/file/binary-dist
then create and edit ~/.config/nix/nix.conf"
						exit 1
				fi

				## tell BLAS that we are using gnu openmp
				export MKL_THREADING_LAYER="GNU"
				export MKL_INTERFACE_LAYER="GNU,LP64"

				## Connect nix binaries to Nvidia GPUs
				export NIX_GL_PREFIX="nixglhost -- "

				export SBATCH_ACCOUNT=$2
				## Control job goes to "general" partition
				export SBATCH_PARTITION=general
				export SBATCH_TIMELIMIT=320:00:00
				export SBATCH_MEM_PER_NODE=16G

				export SBATCH_EXPORT=ROOT_STORE_DIR,TMPDIR_SHARE,GIT_BRANCH,R_FUTURE_GLOBALS_MAXSIZE,date,HOME,LANG,NIX_CONFIG,NIX_SSL_CERT_FILE,MKL_THREADING_LAYER,MKL_INTERFACE_LAYER,NIX_GL_PREFIX,SBATCH_ACCOUNT,SBATCH_EXPORT

				cd $TMPDIR_SHARE
				sbatch aus_bio_batch.sh
				;;

		prime-ai*)
				## Root directories
				export ROOT_STORE_DIR="/para/resources/qris_sandbox/${3}" #directory with same structure as /QRISdata/. May even be /QRISdata, but probably shouldn't be
				export TMPDIR_SHARE="/para/resources/hpc_sandbox/scratch/user/$(whoami)/aus_bio_scratch_${date}"
				mkdir -p $TMPDIR_SHARE
				mkdir -p $TMPDIR_SHARE/logs
				cp ./aus_bio_batch.sh $TMPDIR_SHARE
				cp ./aus_bio_control.sh $TMPDIR_SHARE

				## Connect nix binaries to Nvidia GPUs
				## NixOS does not need modifications
				export NIX_GL_PREFIX=""

				## tell BLAS that we are using gnu openmp
				export MKL_THREADING_LAYER="GNU"
				export MKL_INTERFACE_LAYER="GNU,LP64"

				## Control job goes to "cpu" partition
				export SBATCH_PARTITION=cpu
				export SBATCH_TIMELIMIT=168:00:00

				export SBATCH_EXPORT=ROOT_STORE_DIR,TMPDIR_SHARE,GIT_BRANCH,R_FUTURE_GLOBALS_MAXSIZE,date,HOME,LANG,NIX_BUILD_CORES,MKL_THREADING_LAYER,MKL_INTERFACE_LAYER,NIX_GL_PREFIX
				cd $TMPDIR_SHARE
				sbatch aus_bio_batch.sh
		;;

		*)
				echo "Hostname not recognised"
				exit 1
		;;
esac






