#!/bin/bash
#SBATCH --job-name=ETpart_mpj
##SBATCH --workdir
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --mem=150000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

chmod +x ./scripts/02d_run_ETpart_mpj.R
srun ./scripts/02d_run_ETpart_mpj.R
