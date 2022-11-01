#!/bin/bash
#SBATCH --job-name=ETpart_ses
##SBATCH --workdir
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --mem=150000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

chmod +x ./scripts/02b_run_ETpart_ses.R
srun ./scripts/02b_run_ETpart_ses.R
