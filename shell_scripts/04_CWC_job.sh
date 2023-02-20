#!/bin/bash
#SBATCH --job-name=CWC_graphs
##SBATCH --workdir
#SBATCH --cpus-per-task=8
#SBATCH --time=35:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu


chmod +x 04_CWC_analysis.R
srun 04_CWC_analysis.R
