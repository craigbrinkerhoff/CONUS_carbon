# Process-based transport modeling of CO2 at continental scales

## To Run
This code is setup for an HPC cluster using a nested parallelization scheme that enables parallel calibration across all basins at the same "processing level", i.e. not up/downstream of one another. Realistically this code will not run on a local machine (and requires a pretty hefty HPC at that).

Use `run.sh`, `slurm_future.tmpl`, and `src/runTargets.R` to execute the complete job non-interactively. This pipeline is facilitated by the [`targets`](https://books.ropensci.org/targets/) package in R and a slurm scheduler, so you need familiarity with these tools to run with confidence! One could also execute individual basins in an interactive session, but that will require a good understanding of how `targets` works.

We used a conda environment to run the entire project. The specifics of the environment are available in `environment.yml`.
