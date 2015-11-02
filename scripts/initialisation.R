# Initialisation script
library(virtualspecies)
library(reshape2)
library(ggplot2)
library(rworldmap)
source("./scripts/functions/patch_generation.R")
worldclim <- getData("worldclim", var = "bio", res = 10)
worldclim <- worldclim[["bio1"]]

# Downloading worldclim data
worldclim <- getData("worldclim", var = "bio", res = 10)
worldclim <- worldclim[["bio1"]]

# Resampling to the resolution used in the paper
worldclim <- aggregate(worldclim, fact = 3)

# We will crop to North America
worldmap<- getMap()
northam <- extent(-180, -15, 5, 85)
bio1 <- crop(worldclim, northam)
bio1 <- mask(bio1, worldmap[which(worldmap@data$SUBUNIT %in% 
                                    c("United States of America", "Canada", "Greenland", 
                                      "Mexico", "Panama", "Costa Rica", "Guatemala", 
                                      "Honduras", "Nicaragua")), ])


temperature.gradient <- seq(worldclim[["bio1"]]@data@min - 100,
                            worldclim[["bio1"]]@data@max, 
                            length = 1000)
rm(worldclim)

gauss.resp <- function(x., mean., diff., prob.)
{
  custnorm(x = x., mean = mean., diff = diff., prob = prob.) /
    custnorm(x = mean., mean = mean., diff = diff., prob = prob.)
}