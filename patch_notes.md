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