generate.patches <-
function(environment.raster, n.patches = 50, patch.size = 10)
{
  dummy.raster <- environment.raster
  dummy.raster[!is.na(dummy.raster)] <- 1
  n.cols <- ncol(dummy.raster)
  n.rows <- nrow(dummy.raster)
  
  # Get the environment matrix
  env.m <- getValues(dummy.raster, format = "matrix")
  
  # Select the number of patches
  # n.patches <- 50
  # Define the starting points for each patch
  starts <- sample(x = which(!is.na(env.m)), size = n.patches, replace = FALSE)
  # Select patch size
  # patch.size <- sample(5:20, n.patches, replace = T)
  patch.s <- patch.size
  test <- lapply(starts, FUN = function(s1, x1, patch.s1)
  {
    expand(start = s1, x = x1, n.size = patch.s1)
  }, x1 = env.m, patch.s1 = patch.s)
  test2 <- rowSums(matrix(unlist(test), ncol = length(test), byrow = FALSE), na.rm = T)
  ids <- which(test2 == 2)
  
  # Attribute all patch values in the environment matrix (NA = sea, 0 = no patch, 1 = patch)
  env.m[!(is.na(env.m))] <- 0
  env.m[ids] <- 1
  
  patch.raster <- setValues(dummy.raster,
                            env.m)
  
  return(patch.raster)
}
