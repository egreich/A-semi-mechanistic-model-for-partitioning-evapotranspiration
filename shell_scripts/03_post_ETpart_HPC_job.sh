#!/bin/bash
#SBATCH --job-name=post_ETpart
##SBATCH --workdir
#SBATCH --cpus-per-task=8
#SBATCH --time=10:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

chmod +x ./scripts/03_post_ETpart_HPC.R
srun ./scripts/03_post_ETpart_HPC.R
