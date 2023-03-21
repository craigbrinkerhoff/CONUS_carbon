#!/bin/bash
#SBATCH --job-name='carbon_master'
#SBATCH -c 30  # Number of Cores per Task
#SBATCH --mail-type=END,FAIL
#SBATCH --mail-user=cbrinkerhoff@umass.edu
#SBATCH -p ceewater_cjgleason-cpu
#SBATCH --mem=30000 #Requested memory
#SBATCH -t 4800:00:00  # Job time limit 200 days (some huge number)
#SBATCH -o out_master.txt  # %j = job ID
#SBATCH -e err_master.txt
#SBATCH --exclude=ceewater-cpu[001-007]

Rscript src/run_targets.R
