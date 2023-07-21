# CO2 transport modeling at continental scales

## To Run
```
conda activate CONUS_carbon-env
sbatch run.sh
```
This code is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. not up/downstream of one another.

## Notes
- The pipeline is written entirely in `R` and is facilitated by [`targets`](https://books.ropensci.org/targets/) and [`future.batchtools`](https://future.batchtools.futureverse.org/). You need familiarity with these tools to run with confidence! We use a SLURM scheduler to submit jobs. This code is setup for the specific architecture of our partition on our HPC, your experience will certainly vary (and scheduler settings will need to be changed).

- The actual call to run the pipeline is done in *src/run_targets.R* so make sure you check there as well for different commands to run in serial or parallel (and for the latter, the number of workers requested).

- We use a conda environment to run the entire project. The environment recipe is available in *environment.yml*. Note that you need to manually install `dataRetrieval` package (v. 2.7.12) as its not available on conda-forge. You also need to manually install `ggsn` package (v. 0.5.0) as it's not available on conda-forge.

- While the necessary hardcoded file paths *should* only be in *_targets.R*, there is an assumed data repo structure. Check functions in *src/analysis.R* for examples. There is one hardcoded file path (to the hydraulic geometry models- see Brinkerhoff et al. 2022 in the paper).

- Input data are stored in another repo and not provided here. See `path_to_data` in *_targets.R*. Those data should all be publically available online. *src/data/* contains mostly lookup tables.

- Note that we sometimes experienced dropped connections between master and slave jobs- you will see that we manual access some of cached results in the pipeline file. This may or may not happen to you.

- There is an .RProfile file for automatically setting up an interactive R session should you launch one from terminal.

- We use 'lumped' and 'upscaling' interchangeably throughout the code. So these are referring to the same things.... Same goes for 'distributed' and 'transport'- these are the same models.