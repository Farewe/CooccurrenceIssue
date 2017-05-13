# Simulation of virtual species for the co-occurrence issue
Repository for the simulations of virtual species for the co-occurrence issue paper


The script used to run simulations is "Species generation script". It is based on various other scripts on other scripts included in the repository, as well as three R packages: [`virtualspecies`](https://github.com/farewe/virtualspecies), [`foreach`](https://cran.r-project.org/web/packages/foreach/index.html) and [`doParallel`](https://cran.r-project.org/web/packages/doParallel/index.html).


It is **necessary** to use the latest version of virtualspecies, which can be found on GitHub @ https://github.com/farewe/virtualspecies. Fastest way is to install the package `devtools` and run the function `install_github('virtualspecies')`.


The important scripts in this repository are:

* [`Species generation script.R`](scripts/Species%20generation%20script.R): scripts used to run the simulations
* [`initialisation.R`](scripts/initialisation.R): contains the initialisation steps (e.g. downloading data from WorldClim)
* [`patch_generation_version4.1.R`](scripts/functions/patch_generation_version4.1.R): function used to generate uncohesive distribution ranges
* [`growDistribution.R`](scripts/functions/growDistribution.R): function used to generate cohesive distribution ranges
* [`Species generation function.R`](scripts/Species%20generation%20function.R): function to generate a full simulation for a given number of species, a given temperature-richness scenario, and other parameters 

