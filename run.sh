#!/bin/bash
#SBATCH --job-name='carbon_master'
#SBATCH -c 48  # Number of Cores per Task
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=cbrinkerhoff@umass.edu
#SBATCH -p ceewater_cjgleason-cpu
#SBATCH --mem=128000 #Requested memory
#SBATCH -t 24:00:00  # hrs
#SBATCH -o out_master.txt  # %j = job ID
#SBATCH -e err_master.txt

Rscript src/run_targets.R
