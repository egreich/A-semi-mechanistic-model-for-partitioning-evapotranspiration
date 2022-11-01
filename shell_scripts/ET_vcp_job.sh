#!/bin/bash
#SBATCH --job-name=ETpart_vcp
##SBATCH --workdir
#SBATCH --cpus-per-task=8
#SBATCH --time=24:00:00
#SBATCH --mem=150000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

chmod +x ./scripts/02e_run_ETpart_vcp.R
srun ./scripts/02e_run_ETpart_vcp.R
