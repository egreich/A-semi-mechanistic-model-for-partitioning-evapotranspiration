#!/bin/bash
#SBATCH --job-name=ETpart_array
##SBATCH --workdir
#SBATCH --output=/scratch/egr65/ETpart/log/AllSites_%A_%a.log
#SBATCH --cpus-per-task=1
#SBATCH --time=10:00:00
#SBATCH --mem=70000
#SBATCH --mail-type=all
#SBATCH --mail-user=egr65@nau.edu
#SBATCH --array=1-24

### %A is monsoon job number %a is interior array index

module load R/4.1.2 # load a specific R version

chmod +x ./shell_scripts/run_ETpart_job.sh # for permissions
chmod +x ./scripts/02_run_ETpart_HPC.R # for permissions

chain=$(sed -n "$SLURM_ARRAY_TASK_ID"p chainEND)
site=$(sed -n "$SLURM_ARRAY_TASK_ID"p siteEND)
seed=$(sed -n "$SLURM_ARRAY_TASK_ID"p seedEND)

srun ./shell_scripts/run_ETpart_job.sh $chain $site $seed
