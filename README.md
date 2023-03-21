# CO2 reactive transport modeling at continental scales

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


# OLD
## Batchtools patch
`batchtools` kills master jobs if SQUEUE throws an error for a hot second. This means the model calibrations keep running while the master process is down, and so we never recieve the results from the workers...

For whatever reason, our in development cluster will periodically throw errors when running SQUEUE. So, we had to patch `batchtools`, removing this error flag. Presumbaly would apply to non-SLURM clusters too.

```
#download package
wget https://cran.r-project.org/src/contrib/batchtools_0.9.15.tar.gz #this is the version installed in our conda env so it should work
tar -xvzf batchtools_0.9.15.tar.gz

####comment out lines 88-89 in clusterFunctionsSlurm.R #####

#re-build from source
cd ..
R CMD build batchtools/ --no-build-vignettes #package version issues kept this from building the vignettes correctly so I just didn't
R CMD INSTALL batchtools_0.9.15.tar.gz -l '/work/cbrinkerhoff_umass_edu/.conda/envs/CONUS_carbon-env/lib/R/library' #override within the conda env package library

#double check that package was overwritten within conda
R
trace('makeClusterFunctionsSlurm', 'listJobs', edit=TRUE)
:q!
```