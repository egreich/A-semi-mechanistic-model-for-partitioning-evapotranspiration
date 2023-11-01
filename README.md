# A semi-mechanistic model for partitioning evapotranspiration reveals demand and supply moisture controls over plant water-use efficiency are temporally distinct

This repository contains the code and models for the paper *A semi-mechanistic model for partitioning evapotranspiration reveals a shift to soil moisture-controlled water-use efficiency along an aridity gradient*. An explanation of the file structure is below:

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
  - contains R scripts numbered in order from 00-06
    - 00_data_clean.R
      - This script reads in all necessary data and formats the information consistently across sites
      - This includes filling small data gaps
      - and calculate parameters to feed into the soil evaporation equations used in DEPART
    - 01_model_prep.R
      - This script assigns "block lengths" (i.e., the window length over which we calculate WUE)
      - and manually assigns season time periods for easy summarizing.
      - This script results in three dataframes for each site, one for daily data, one for weekly data (for WUE calculations), and one for yearly data (for easy summarizing).
    - 02_run_ETpart.R
      - Runs the DEPART model
    - 03_post_ETpart_HPC.R
      - If the chains in the model were run separately, this script recombines the chains for each coda object
    - 04_CWC_analysis.R
      - Runs the cross wavelet coherence analysis for all sites
    - 05_model_comp.R
      - Compares the DEPART model output with the Perez-Priego model output and sapflow data
    - 06_Scott_partition.R
      - Compares all T estimates from 05_model_comp.R with the Scott and Beiderman partitioning method
  - Helper functions that are called by the numbered scripts are also included and designated with the word "function" in the script name.

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
