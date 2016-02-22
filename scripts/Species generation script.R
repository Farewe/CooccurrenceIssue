library(virtualspecies)
library(foreach)
library(doParallel)

source("./scripts/initialisation.R")
source("./scripts/functions/patch_generation_version4.1.R")
source("./scripts/functions/growDistribution.R")
source("./scripts/functions/resamp.R")
source("./scripts/Species generation function.R")

registerDoParallel(cores = 12)
generateAllSpecies(simulation = "100patches", nb.sp = 2000, nb.patches = 100, patchsize = 1, scenario = "neutral",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "250patches", nb.sp = 2000, nb.patches = 250, patchsize = 1, scenario = "neutral",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "500patches", nb.sp = 2000, nb.patches = 500, patchsize = 1, scenario = "neutral",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "1000patches", nb.sp = 2000, nb.patches = 1000, patchsize = 1, scenario = "neutral",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "2500patches", nb.sp = 2000, nb.patches = 2500, patchsize = 1, scenario = "neutral",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "5000patches", nb.sp = 2000, nb.patches = 5000, patchsize = 1, scenario = "neutral",
                   temperature.gradient = temperature.gradient)

generateAllSpecies(simulation = "100patches", nb.sp = 2000, nb.patches = 100, patchsize = 1, scenario = "temp.gradient",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "250patches", nb.sp = 2000, nb.patches = 250, patchsize = 1, scenario = "temp.gradient",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "500patches", nb.sp = 2000, nb.patches = 500, patchsize = 1, scenario = "temp.gradient",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "1000patches", nb.sp = 2000, nb.patches = 1000, patchsize = 1, scenario = "temp.gradient",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "2500patches", nb.sp = 2000, nb.patches = 2500, patchsize = 1, scenario = "temp.gradient",
                   temperature.gradient = temperature.gradient)
generateAllSpecies(simulation = "5000patches", nb.sp = 2000, nb.patches = 5000, patchsize = 1, scenario = "temp.gradient",
                   temperature.gradient = temperature.gradient)
