#!/bin/bash
#PBS -A UQ-SCI-SMP
#PBS -l select=1:ncpus=2:mem=4GB
#PBS -l walltime=00:10:00


module load use.own
module load aus_bio_module.lua

Rscript drake_plan.R
