# CO2 transport modeling at continental scales

## To Run
```
conda activate CONUS_carbon-env
sbatch run.sh
```
This code is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. those that are not up/downstream of one another.

## Notes
- The pipeline is written entirely in `R` and is facilitated by [`targets`](https://books.ropensci.org/targets/) and [`future.batchtools`](https://future.batchtools.futureverse.org/). You need familiarity with these tools to run with confidence! We use a SLURM scheduler to submit jobs. This code is setup for the specific architecture of our partition on our HPC, your experience will certainly vary (and scheduler settings will need to be changed).

- The actual call to run the pipeline is done in *src/run_targets.R*. This is where one specifies whether they are running in serial or parallel.

- We use a conda environment to run the entire project. The environment recipe is available in *environment.yml*. Note that you need to manually install `dataRetrieval` package (v. 2.7.12) as its not available on conda-forge. You also need to manually install `ggsn` package (v. 0.5.0).

- Input data are stored in another repo and not provided here as they're publicly available (and huge). See `path_to_data` in *_targets.R*. The *src/data/* folder contains lookup tables and an auxillary dataset. While the necessary hardcoded file paths should only be in *_targets.R*, there is an assumed structure for the `path_to_data` repo. Check the functions in *src/analysis.R* to see how those folders are named and structured. Note that there is one hardcoded file path (to the hydraulic geometry models- see Brinkerhoff et al. 2022 in the paper).

- In calibrating the model, we sometimes experienced dropped connections between master and slave jobs- you will see that we manual access some of cached results in *_targets.R*. This may or may not happen to you, but either way the file will need tweaking.