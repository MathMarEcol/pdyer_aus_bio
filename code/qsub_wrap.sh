#!/bin/sh

ssh -i /home/uqpdyer/.ssh/selfkey $PBS_O_HOST qsub "$@"
