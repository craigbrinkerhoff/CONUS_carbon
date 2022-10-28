# CO2 reactive transport modeling at continental scales

## To Run
This code (entirely in R) is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. not up/downstream of one another. Realistically this code will not run on a local machine (and requires a pretty hefty HPC at that).

Use run.sh, slurm_future.tmpl, and src/runTargets.R to execute the complete job non-interactively. This pipeline is facilitated by [`targets`](https://books.ropensci.org/targets/) and [`future.batchtools`](https://future.batchtools.futureverse.org/), so you need familiarity with these tools to run with confidence! For other HPC setups, you might need a non-SLURM template FYI. One could also execute individual basins in an interactive session, but that will require a good understanding of how [`targets`](https://books.ropensci.org/targets/) works.

## Notes
- We used a conda environment to run the entire project. The specifics of the environment are available in `environment.yml`.
- While the necessary hardcoded file paths *should* only be in _targets.R, there is an assumed data repo structure. Check functions in src/analysis.R for examples.
- None of the input data or model results are included in this repo as they are way too large.

## To export the all finished calibrated parameters:
```
tar_load(starts_with('calibratedParameters'))
calibratedParameters <- mget(ls(pattern='calibratedParameters_'))
```
