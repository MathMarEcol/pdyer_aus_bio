#!/bin/sh

ssh -i ~/.ssh/selfkey ${PBS_O_HOST:-localhost} qstat "$@"
