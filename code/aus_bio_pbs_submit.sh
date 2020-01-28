#!/bin/bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=1:mem=4GB
#PBS -l walltime=00:01:00


#by default, PBS begins in the home dir, but the env var $PBS_O_WORKDIR contains the path to this script.
#Assuming that this job was called from /???30days???/uqpdyer/Q1216/pdyer/pdyer_aus_bio/code
cd $PBS_O_WORKDIR

#For best performance, copy all needed files to the node local disk. This may break when drake submits jobs for me.
mkdir -p $TMPDIR/Q1215
mkdir -p $TMPDIR/Q1216

#Have to carefully make sure all files make it over
mkdir -p $TMPDIR/Q1215/AusCPR
rsync -irc ../../../../Q1215/AusCPR/combined_copeped_jul19.csv $TMPDIR/Q1215/AusCPR
mkdir -p $TMPDIR/Q1216/pdyer/pdyer_aus_bio/code
rsync -irc ./drake_plan.R $TMPDIR/Q1216/pdyer/pdyer_aus_bio/code



#Then run from the local disk
cd $TMPDIR/Q1216/pdyer/pdyer_aus_bio/code

#Bash builtin that allows module aliases to work in non-interactive jobs (shopt: SHell OPTions)
#-s means set, expand_aliases is only default for interactive shells, not non-interactive
shopt -s expand_aliases

module load use.own
module load aus_bio_module
#Because this is a singularity, it can only see directories specified in aus_bio_module's "SINGULARITY_BIND" env var.
Rscript drake_plan.R


#Recover the outputs
rsync -irc $TMPDIR/Q1216/pdyer/pdyer_aus_bio/code/ $PBS_O_WORKDIR
