#!/bin/bash
#SBATCH --job-name=CWC_graphs
##SBATCH --workdir
#SBATCH --cpus-per-task=1
#SBATCH --time=35:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

module load R/4.1.2 # load a specific R version

chmod +x ./scripts/04_CWC_analysis.R
srun ./scripts/04_CWC_analysis.R
