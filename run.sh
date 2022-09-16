#!/bin/bash
#SBATCH --job-name='carbon_master'
#SBATCH -c 10  # Number of Cores per Task
#SBATCH -p cpu
#SBATCH --mem=15000 #Requested memory
#SBATCH -t 24:00:00  # Job time limit 1 day
#SBATCH -o out_master.txt  # %j = job ID
#SBATCH -e err_master.txt

Rscript src/run_targets.R
