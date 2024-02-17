#!/usr/bin/env bash



if [ ! -v SBATCH_PARTITION ]; then
	 echo "SBATCH_PARTITION not defined, please set it and rerun script"
	 exit 1
fi

if [ ! -v SBATCH_ACCOUNT ]; then
	 echo "SBATCH_ACCOUNT not defined, please set it and rerun script"
	 exit 1
fi


if [ -z "$1"  ]; then
		echo "Please specify the number of cores to request"
		exit 1
fi

if ! command -v nix &> /dev/null
then
		echo "Nix package manager is not found on the path.
For unprivileged users download static binary from https://hydra.nixos.org/job/nix/maintenance-2.20/buildStatic.x86_64-linux/latest/download-by-type/file/binary-dist
then create and edit ~/.config/nix/nix.conf"
		exit 1
fi

sbatch --cpus-per-task $1 setup_nix_shell_batch.sh
