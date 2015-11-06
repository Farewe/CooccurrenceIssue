### Test code correct but slow version
generatePatchySpecies <- function(nb.sim = 1, nb.sp = 20)
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
    species <- foreach(i = 1:nb.sp, .export = c("bio1", "generate.patches",
                                                "expand")) %dopar% 
    {
      sup <- custnorm(x = sp.traits[i, "T.optimum"],
                      mean = sp.traits[i, "T.optimum"],
                      diff = sp.traits[i, "T.tolerance"],
                      prob = 0.99)
      
      cur.sp <- generateSpFromFun(raster.stack = bio1,
                                               list(bio1 = list(fun = "custnorm", 
                                                                args = list(mean = sp.traits[i, "T.optimum"],
                                                                            diff = sp.traits[i, "T.tolerance"],
                                                                            prob = 0.99))))
      cur.sp$suitab.raster <- cur.sp$suitab.raster / sup
      
      cur.sp <- convertToPA(cur.sp, # PA.method = "threshold",
                            beta = 0.7, alpha = -0.05, 
                            plot = FALSE)
      
      # Step 2.5: generate habitat patches
      patches <- generate.patches(bio1, n.patches = 50, patch.size = 10)
      cur.sp$patched.pa.raster <- overlay(cur.sp$pa.raster,
                                          patches,
                                          fun = function(x, y) x * y)
      
      return(cur.sp)
    }

    # Richness calculation
    range.stack <- stack(sapply(species, FUN = function(x) return(x$pa.raster)))
    range.patch.stack <- stack(sapply(species, FUN = function(x) return(x$patched.pa.raster)))
    # Try rasterengine here
    richness <- sum(range.stack)
    richness.patch <- sum(range.patch.stack)
    
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
