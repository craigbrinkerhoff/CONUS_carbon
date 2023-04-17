# CO2 transport modeling at continental scales

## To Run
```
conda activate CONUS_carbon-env
sbatch run.sh
```

- This code is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. not up/downstream of one another.

- This pipeline is facilitated by [`targets`](https://books.ropensci.org/targets/) and [`future.batchtools`](https://future.batchtools.futureverse.org/), so you need familiarity with these tools to run with confidence! We use a SLURM scheduler to submit jobs, your HPC mileage may vary.

- We used a conda environment to run the entire project. The specifics of the environment are available in *environment.yml*. Note that you need to manually install `dataRetrieval` package as its not available in conda-forge.

- While the necessary hardcoded file paths *should* only be in *_targets.R*, there is an assumed data repo structure. Check functions in *src/analysis.R* for examples.

- Input data are stored in another repo, see `path_to_data` in *_targets.R*

## To export the all finished calibrated parameters:
```
R

built <- tar_built(starts_with('calibratedParameters'))
skipped <- tar_skipped(starts_with('calibratedParameters'))

tar_load(starts_with(built))
tar_load(starts_with(skipped))

calibratedParameters <- mget(ls(pattern='calibratedParameters_'))

readr::write_rds(calibratedParameters, 'cache/calibratedParameters.rds')
```


```
install.packages('gittargets')
```

```
git add --all -- ':!path/to/file1'
```