library(virtualspecies)
library(foreach)
library(doParallel)

source("./scripts/initialisation.R")
source("./scripts/functions/patch_generation_version4.1.R")
source("./scripts/functions/growDistribution.R")
source("./scripts/functions/resamp.R")
source("./scripts/Species generation function.R")


registerDoParallel(cores=12)
generateAllSpecies(nb.sim = 10, nb.sp = 5000)
