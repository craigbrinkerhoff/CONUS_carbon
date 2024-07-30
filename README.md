# CO2 transport modeling at continental scales

## To Run

```
conda activate env-CONUS_carbon
sbatch run.sh
```

This code is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. those that are not up/downstream of one another.

The pipeline is written entirely in `R` and is facilitated by [`targets`](https://books.ropensci.org/targets/) and [`future.batchtools`](https://future.batchtools.futureverse.org/). You'd need familiarity with these tools to run with confidence! We use a SLURM scheduler to submit jobs. This code is setup for the specific architecture of our partition on our HPC, experiences will certainly vary (and scheduler settings will need to be changed for another HPC).

## Notes

- The actual call to run the pipeline is done in *src/run_targets.R*. This is where one specifies whether they are running in serial or parallel.

- We use a conda environment to run the entire project. The environment recipe is available in *environment.yml*. Note that you need to manually install `dataRetrieval` package (v. 2.7.12) as its not available on conda-forge at the time of download. You also need to manually install `ggsn` package (v. 0.5.0).

- Input data are stored in another repo and not provided here as they're publicly available. See `path_to_data` in *_targets.R*. The *src/data/* folder contains lookup tables and an auxillary dataset. While the necessary hardcoded file paths should only be in *_targets.R*, there is an assumed structure for the `path_to_data` repo. Check the functions in *src/analysis.R* to see how those folders are named and structured. Note that there is one hardcoded file path (to the hydraulic geometry models- see Brinkerhoff et al. 2022 in the manuscript).

- In calibrating the model, we sometimes experienced dropped connections between master and slave jobs, meaning that the calibration continued but could not communicate with the master job at the pipeline would terminate. Luckily in those cases, `future.batchtools` saves the job output in a cached folder. You will see in *_targets.R* that we sometimes manual access the cached calibration results with R variables named "*_bugFix". This may or may not happen to you and appears to be a product of `future.batchtools`, which has been superseded in more recent versions of `targets`, but either way the pipeline may need tweaking.