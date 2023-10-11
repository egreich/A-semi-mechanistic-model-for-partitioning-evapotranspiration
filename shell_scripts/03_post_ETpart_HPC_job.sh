#!/bin/bash
#SBATCH --job-name=post_ETpart
##SBATCH --workdir
#SBATCH --output=/scratch/egr65/ETpart/log/postmod_%A.log
#SBATCH --cpus-per-task=1
#SBATCH --time=20:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

module load R/4.1.2 # load a specific R version

chmod +x ./scripts/03_post_ETpart_HPC.R
srun ./scripts/03_post_ETpart_HPC.R
