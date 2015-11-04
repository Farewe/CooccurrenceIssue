generatePatchySpecies <-
function(nb.sim = 1, nb.sp = 20)
{
  richness <- stack()
  richness.patch <- stack()
  for (j in 1:nb.sim)
  {
    sp.traits <- data.frame(T.optimum = sample(temperature.gradient,
                                               nb.sp, replace = T),
                            T.tolerance = sample(seq(50, 100, 
                                                     length = 1000), 
                                                 nb.sp, replace = T))
    species <- list() # All the species will be stored in this list
    range.stack <- stack() # Species ranges will be stored in this stack
    range.patch.stack <- stack() # Species ranges with patched distributions will be stored in this stack
    
    for (i in 1:nb.sp)
    {
      # Step 1: generating the species environmental suitability
      # (i.e., transforming from E-space to G-space)
      species[[i]] <- generateSpFromFun(bio1,
                                        list(bio1 = list(fun = "gauss.resp", 
                                                         args = list(mean. = sp.traits[i, "T.optimum"],
                                                                     diff. = sp.traits[i, "T.tolerance"],
                                                                     prob. = 0.99))),
                                        rescale.each.response = FALSE,
                                        rescale = FALSE)
      # Step 2: converting environmental suitability to presence-absence,
      # using the probabilistic conversion
      species[[i]] <- convertToPA(species[[i]], # PA.method = "threshold",
                                  beta = 0.7, alpha = -0.05, 
                                  plot = FALSE)
      
      # Step 2.5: generate habitat patches
      patches <- generate.patches(bio1, n.patches = 50, patch.size = 10)
      species[[i]]$patched.pa.raster <- overlay(species[[i]]$pa.raster,
                                                patches,
                                                fun = function(x, y){return(x * y)})
      
      
      # Step 3: we store ranges to calculate richness par cell
      range.stack <- addLayer(range.stack,
                              species[[i]]$pa.raster)
      range.patch.stack <- addLayer(range.patch.stack,
                                    species[[i]]$patched.pa.raster)
    }
    
    
    # Richness calculation
    richness <- addLayer(richness,
                         sum(range.stack))
    richness.patch <- addLayer(richness.patch,
                               sum(range.patch.stack))
    
    # Saving the files
    save(species, file = paste0("./data/S1_sim", j, "_species"))
    save(sp.traits, file = paste0("./data/S1_sim", j, "_traits"))
    writeRaster(range.stack, paste0("./data/S1_sim", j, "_rangestacks"), 
                overwrite = T)
    writeRaster(richness, paste0("./data/S1_sim", j, "_richness"), 
                overwrite = T)
    writeRaster(range.patch.stack, paste0("./data/S1_sim", j, "_rangestacks_patch"), 
                overwrite = T)
    writeRaster(richness.patch, paste0("./data/S1_sim", j, "_richness_patch"), 
                overwrite = T)
  }
}
