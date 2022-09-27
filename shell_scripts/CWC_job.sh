#!/bin/bash
#SBATCH --job-name=CWC_graphs
##SBATCH --workdir
#SBATCH --cpus-per-task=8
#SBATCH --time=35:00:00
#SBATCH --mem=100000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu

#module load R/4.0.2
#module load jags
#export PKG_CONFIG_PATH=/packages/jags/4.3.0/lib/pkgconfig
#pkg-config ––modversion jags

chmod +x 03_CWC_analysis.R
srun 03_CWC_analysis.R
