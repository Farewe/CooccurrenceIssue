growDistribution <- function(x, size.max = length(Which(x == 1, cells = T)), init.cell = resamp(Which(x == 1, cells = T), 1))
{
  all.cells <- NULL
  nb <- 5
  while(length(all.cells) < size.max)
  {
    if(length(init.cell) > length(all.cells) & length(all.cells) > 0)
    {
      stop("Uh oh.")
    }
    if(length(init.cell) == length(all.cells))
    {
      x2 <- x
      x2[all.cells] <- 0
      cur.square <- NULL
      while(length(cur.square) == 0)# & nb < 500)
      {
        n.matrix <- matrix(c(rep(1, ceiling(nb^2 / 2) - 1), 0,
                             rep(1, ceiling(nb^2 / 2) - 1)),
                           nr = nb, nc = nb)
        cur.square <- adjacent(x2, cells = init.cell[1],
                               pairs = F, directions = n.matrix,
                               target = Which(x2 == 1, cells = T),
                               include = T)
        
        # cat(" -- nb", nb, "\n")
        nb <- nb + 2
        if(length(cur.square) > 1)
        {
          for (s in cur.square)
          {
            all.cells <- unique(c(all.cells,
                                  adjacent(x2, cells = s,
                                           pairs = F, directions = 8,
                                           target = Which(x2 == 1, cells = T),
                                           include = T)))
            
            # cat(" ------- ", s, "\n")
          }
          init.cell <- unique(c(init.cell, cur.square,
                                all.cells[!(all.cells %in% init.cell)][sample.int(ifelse(length(all.cells[!(all.cells %in% init.cell)]) > 0,
                                                                                         1,
                                                                                         0))]))
        } else
        {
          init.cell <- unique(c(init.cell, cur.square))
        }
        
      } 
    }
    all.cells <- unique(c(all.cells,
                          adjacent(x, cells = init.cell[length(init.cell)],
                                   pairs = F, directions = 8,
                                   target = Which(x == 1, cells = T),
                                   include = T)))
    init.cell <- c(init.cell, 
                   all.cells[!(all.cells %in% init.cell)][sample.int(ifelse(length(all.cells[!(all.cells %in% init.cell)]) > 0,
                                                                            1,
                                                                            0))])
    
    # cat(length(all.cells), "\n")
  }
  all.cells <- all.cells[1:size.max]
  x[x > 0] <- 0
  x[all.cells] <- 1
  return(x)
}
