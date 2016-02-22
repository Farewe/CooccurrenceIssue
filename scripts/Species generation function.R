### Correct generation method, scenario 1 (even temperature gradient)

generateAllSpecies <- function(nb.sim = 1, nb.sp = 20, nb.patches = 50, scenario = "neutral",
                               temperature.gradient, patchsize = 10)

{
  library(virtualspecies)
  richness.stack <- stack()
  richness.patch.stack <- stack()
  richness.cohesive.stack <- stack()

  if(scenario == "neutral")
  {
    probi <- NULL
  } else if(scenario == "temp.gradient")
  {
    probi <- logisticFun(x = temperature.gradient,
                         alpha = -50, beta = 0.5)
  }

  for (j in simulation)
  {
    
    sp.traits <- data.frame(T.optimum = sample(temperature.gradient,

                                               nb.sp, replace = T,
                                               prob = probi),
                            T.tolerance = sample(seq(50, 450, 
                                                     length = 1000), 
                                                 nb.sp, replace = T))
    save(sp.traits, file = paste0("./outputs/", scenario, "_sim", j, "_traits"))
    
    
    species <- foreach(i = 1:nb.sp, 
                       .packages = "virtualspecies",
                       .export = c("bio1", "generate.patches",
                                   "expand", "growDistribution",
                                   "resamp", "nb.patches")) %dopar% 
    {
      sup <- custnorm(x = sp.traits[i, "T.optimum"],
                      mean = sp.traits[i, "T.optimum"],
                      diff = sp.traits[i, "T.tolerance"],
                      prob = 0.99)
      
      cur.sp <- generateSpFromFun(raster.stack = bio1,
                                               list(bio1 = list(fun = "custnorm", 
                                                                args = list(mean = sp.traits[i, "T.optimum"],
                                                                            diff = sp.traits[i, "T.tolerance"],
                                                                            prob = 0.99))),
                                  rescale.each.response = FALSE,
                                  rescale = FALSE)
      cur.sp$suitab.raster <- cur.sp$suitab.raster / sup
      
      cur.sp <- convertToPA(cur.sp, # PA.method = "threshold",
                            beta = 0.7, alpha = -0.05, 
                            plot = FALSE)
      
      # Step 2.5: generate habitat patches
      # 2.5.1 Uncohesive
      patches <- generate.patches(bio1, n.patches = nb.patches, patch.size = patchsize)
      cur.sp$patched.pa.raster <- overlay(cur.sp$pa.raster,
                                          patches,
                                          fun = function(x, y) x * y)
      active.cells <- Which(cur.sp$patched.pa.raster == 1, cells = T)
      
      # 2.5.2 Cohesive
      if(length(active.cells) > 0)
      {
        cur.sp$cohesive.pa.raster <- growDistribution(x = cur.sp$pa.raster, size.max = length(active.cells))
      } else
      {
        cur.sp$cohesive.pa.raster <- cur.sp$patched.pa.raster
      }
      return(cur.sp)
    }

    # We may try rasterengine here to calculate richness
    # Creating range size stack
    range.stack <- stack(sapply(species, FUN = function(x) return(x$pa.raster)))
    # Writing range size stack
    writeRaster(range.stack, paste0("./outputs/", scenario, "_sim", j, "_rangestacks"), 
                overwrite = T)
    # Calculating richness
    richness <- sum(range.stack)
    # Writing richness
    writeRaster(richness, paste0("./outputs/", scenario, "_sim", j, "_richness"), 
                overwrite = T)
    # Creating species - site matrix
    species.sites.matrix <- getValues(range.stack)
    rm(range.stack)
    species.sites.matrix <- species.sites.matrix[-which(is.na(species.sites.matrix[, 1])), ]
    # Writing species - site matrix
    save(species.sites.matrix, 
         file = paste0("./outputs/", scenario, "_sim", j, "_sp_site_matrix"), compress = "gzip")
    rm(species.sites.matrix)
    
    # Creating patched range size stack
    range.patch.stack <- stack(sapply(species, FUN = function(x) return(x$patched.pa.raster)))
    # Writing range size stack
    writeRaster(range.patch.stack, paste0("./outputs/", scenario, "_sim", j, "_rangestacks_patch"), 
                overwrite = T)
    # Calculating richness
    richness.patch <- sum(range.patch.stack)
    # Writing richness
    writeRaster(richness.patch, paste0("./outputs/", scenario, "_sim", j, "_richness_patch"), 
                overwrite = T)
    # Creating species - site matrix
    speciespatch.sites.matrix <- getValues(range.patch.stack)
    rm(range.patch.stack)
    speciespatch.sites.matrix <- speciespatch.sites.matrix[-which(is.na(speciespatch.sites.matrix[, 1])), ]
    # Writing species - site matrix
    save(speciespatch.sites.matrix, 
         file = paste0("./outputs/", scenario, "_sim", j, "_sp_patch_site_matrix"), compress = "gzip")
    rm(speciespatch.sites.matrix) 

    # Creating cohesive range size stack 
    range.cohesive.stack <- stack(sapply(species, FUN = function(x) return(x$cohesive.pa.raster)))
    # Writing range size stack
    writeRaster(range.cohesive.stack, paste0("./outputs/", scenario, "_sim", j, "_rangestacks_cohesive"), 
                overwrite = T)
    # Calculating richness
    richness.cohesive <- sum(range.cohesive.stack)
    # Writing richness
    writeRaster(richness.cohesive, paste0("./outputs/", scenario, "_sim", j, "_richness_cohesive"), 
                overwrite = T)
    # Creating species - site matrix
    speciescohesive.sites.matrix <- getValues(range.cohesive.stack)
    rm(range.cohesive.stack)
    speciescohesive.sites.matrix <- speciescohesive.sites.matrix[-which(is.na(speciescohesive.sites.matrix[, 1])), ]
    # Writing species - site matrix
    save(speciescohesive.sites.matrix, 
         file = paste0("./outputs/", scenario, "_sim", j, "_sp_cohesive_site_matrix"), compress = "gzip")
    rm(speciescohesive.sites.matrix)
    
    png(paste0("./outputs/", scenario, "_sim", j, "_richnessmap.png"))
    tmp <- stack(richness,
                 richness.patch,
                 richness.cohesive)
    names(tmp) <- c("Initial.richness", "Uncohesive.richness", "Cohesive.richness")
    plot(tmp)
    dev.off()
    rm(tmp)
    
    rich <- data.frame(Initial.richness = getValues(richness),
                       Uncohesive.richness = getValues(richness.patch),
                       Cohesive.richness = getValues(richness.cohesive),
                       Temperature = getValues(bio1))
    rich <- rich[-which(is.na(rich[, 1])), ]
    # rich <- melt(rich, id = 'Temperature', value.name = "Richness")
    png(paste0("./outputs/", scenario, "_sim", j, "_rich_temp.png"))
    
    par(mfrow = c(1, 3))

    plot(Initial.richness ~ Temperature, data = rich, cex = .5, 
         main = "Initial", ylab = 'Richness',
         ylim = c(0, max(rich$Initial.richness)))
    plot(Uncohesive.richness ~ Temperature, data = rich, cex = .5, 
         main = "Uncohesive", ylab = 'Richness',
         ylim = c(0, max(rich$Uncohesive.richness)))
    plot(Cohesive.richness ~ Temperature, data = rich, cex = .5, 
         main = "Cohesive", ylab = 'Richness',
         ylim = c(0, max(rich$Cohesive.richness)))
    dev.off()
    
    
    # Saving the files
    save(species, file = paste0("./outputs/", scenario, "_sim", j, "_species"), compress = "gzip")
    
    
    rm(list = c("rich", "species", "sp.traits"))
    richness.stack <- addLayer(richness.stack, richness)
    richness.patch.stack <- addLayer(richness.patch.stack, richness.patch)
    richness.cohesive.stack <- addLayer(richness.cohesive.stack, richness.cohesive)
    message(Sys.time(), " - Simulation ", j, "complete\n")
  }
  
}
