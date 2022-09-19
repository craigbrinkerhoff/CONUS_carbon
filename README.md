# Process-based transport modeling of CO2 at continental scales

## To Run
This code (entirely in R) is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. not up/downstream of one another. Realistically this code will not run on a local machine (and requires a pretty hefty HPC at that).

Use run.sh, slurm_future.tmpl, and src/runTargets.R to execute the complete job non-interactively. This pipeline is facilitated by [`targets`](https://books.ropensci.org/targets/) and [`future.batchtools`](https://future.batchtools.futureverse.org/), so you need familiarity with these tools to run with confidence! For other HPC setups, you might need a non-SLURM template FYI. One could also execute individual basins in an interactive session, but that will require a good understanding of how [`targets`](https://books.ropensci.org/targets/) works.

We used a conda environment to run the entire project. The specifics of the environment are available in `environment.yml`.
