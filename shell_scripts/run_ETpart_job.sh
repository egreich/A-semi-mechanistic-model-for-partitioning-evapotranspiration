#!/bin/bash
# analysis.sh - an analysis program
# $1 (input) and $2 (output) are the first and second arguments to this script

# strip off the directory paths to get just the filename
# BASE=`basename $1`
chain=$1
site=$2
seed=$3

echo 
echo "run('$1');"
date

#R --no-save < $1
./scripts/02_run_ETpart_HPC.R $chain $site $seed
echo "run('$1'); done"
date