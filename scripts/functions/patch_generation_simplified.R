generate.patches <- function(environment.raster, n.patches = 50, patch.size = 10, starts = NULL)
{
  dummy.raster <- environment.raster
  dummy.raster[!is.na(dummy.raster)] <- 1
  n.cols <- ncol(dummy.raster)
  n.rows <- nrow(dummy.raster)
  
  # Get the environment matrix
  env.m <- t(getValues(dummy.raster, format = "matrix"))
  
  # Select the number of patches
  # n.patches <- 50
  # Define the starting points for each patch
  if(is.null(starts))
  {
    starts <- sample(x = which(!is.na(env.m)), size = n.patches, replace = FALSE)
  }

  # Select patch size
  # patch.size <- sample(5:20, n.patches, replace = T)
  patch.s <- patch.size
  ids <- NULL
  for (run in 1:n.patches)
  {
    s <- starts[run]
    # patch.s <- patch.size[run]
    # Create the patch (same matrix as env.m, but with values of 2 where the patch exist)
    patch <- expand(x = env.m, n.size = patch.s, start = s)
    # Get patch cell ids
    ids <- c(ids, which(patch == 2))
  }
  
  # Attribute all patch values in the environment matrix (NA = sea, 0 = no patch, 1 = patch)
  env.m[!(is.na(env.m))] <- 0
  env.m[ids] <- 1
  
  patch.raster <- setValues(dummy.raster,
                            t(env.m))
  
  return(patch.raster)
}
expand <- function(x, n.size, start) {
  if (x[start] != 1) stop("Attempting to begin on an unoccupied cell")
  n.rows <- dim(x)[1]
  n.cols <- dim(x)[2]
  nbrhood <- matrix(c(-1,-1, -1,0, -1,1, 0,-1, 0,1, 1,-1, 1,0, 1,1), nrow=2)
  #
  # Adjoin one more random cell and update `state`, which records
  # (1) the immediately available cells and (2) already occupied cells.
  #
  grow <- function(state) {
    #
    # Find all available neighbors that lie within the extent of `x` and
    # are unoccupied.
    #
    neighbors <- function(i) {
      n <- c((i-1)%%n.rows+1, floor((i-1)/n.rows+1)) + nbrhood
      n <- n[, n[1,] >= 1 & n[2,] >= 1 & n[1,] <= n.rows & n[2,] <= n.cols,
             drop=FALSE]             # Remain inside the extent of `x`.
      n <- n[1,] + (n[2,]-1)*n.rows  # Convert to *vector* indexes into `x`.
      n <- n[x[n]==1]                # Stick to valid cells in `x`.
      n <- setdiff(n, state$occupied)# Remove any occupied cells.
      return (n)
    }
    #
    # Select one available cell uniformly at random.
    # Return an updated state.
    #
    j <- ceiling(runif(1) * length(state$available))
    i <- state$available[j]
    return(list(index=i,
                available = union(state$available[-j], neighbors(i)),
                occupied = c(state$occupied, i)))
  }
  #
  # Initialize the state.
  # (If `start` is missing, choose a value at random.)
  #
  if(missing(start)) {
    indexes <- 1:(n.rows * n.cols)
    indexes <- indexes[x[indexes]==1]
    start <- sample(indexes, 1)
  }
  if(length(start)==2) start <- start[1] + (start[2]-1)*n.rows
  state <- list(available=start, occupied=c())
  #
  # Grow for as long as possible and as long as needed.
  #
  i <- 1
  indices <- c(NA, n.size)
  while(length(state$available) > 0 && i <= n.size) {
    state <- grow(state)
    indices[i] <- state$index
    i <- i+1
  }
  #
  # Return a grid of generation numbers from 1, 2, ... through n.size.
  #
  indices <- indices[!is.na(indices)]
  y <- matrix(NA, n.rows, n.cols)
  y[indices] <- 2
  return(y)
}
