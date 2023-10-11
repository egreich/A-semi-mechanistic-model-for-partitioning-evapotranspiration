# A new semi-mechanistic model to partition evapotranspiration in drylands

This repository contains the code and models for the paper *A new semi-mechanistic model to partition evapotranspiration in drylands*. An explanation of the file structure is below:

1. models
  - inits
  - DEPART_model.R
    - A version of the depart model that uses ECOSTRESS WUE to help constrain WUE
  - DEPART_model_noECO.R
    - this is the main model used in the manuscript, which models WUE autoregressively
  - DEPART_model_split.R
    - the same as DEPART_model.R, but splits the model to account for a large data gap at US-Vcp
  - DEPART_model_noECO_split.R
    - the same as DEPART_model_noECO.R, but splits the model to account for a large data gap at US-Vcp

2. scripts
  - contains R scripts numbered in order from 00-07. Helper functions that are called by the numbered scripts are also included and designated with the word "function" in the script name.

3. shell_scripts
  - shell scripts that were used to run the models on a HPC
  - the files outside the folders (Slurm_jbs_3chains.csv, chainEND, seedEND, and siteEND) are called by the shell scripts to parallelize the model runs on the HPC.


## This repo relies on the following suppressed folders, which contain files that are too large to include here. To run the code in this repo or to look at the output, please download these folders from the Zenodo repository 10.5281/zenodo.8213094
  - input_data/
  - clean_data/
  - output_CWC/
  - output_coda/
  - output_dfs/
  - plots/
