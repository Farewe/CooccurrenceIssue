#### PAM export ####

birdrfsd <- read.table("./data/bird range szi.txt", h = T)

simulation <- c("250patches", "500patches", "1000patches", "2500patches", "5000patches")
pdf('./graphs/RSFD.pdf')
for (s in simulation)
{
  for(k in c("neutral", "temp.gradient"))
  {
    load(paste0("./outputs/", k, "_sim", s, "_sp_patch_site_matrix"))
    load(paste0("./outputs/", k, "_sim", s, "_sp_cohesive_site_matrix"))
    range.size.p <- colSums(speciespatch.sites.matrix)
    range.size.p <- range.size.p / nrow(speciespatch.sites.matrix)
    range.size.c <- colSums(speciespatch.sites.matrix)
    range.size.c <- range.size.c / nrow(speciespatch.sites.matrix)
    par(mfrow = c(3, 1))
    hist(birdrfsd$ranges, breaks = 100, xlim = c(0, 0.5), xlab = "", ylab = "", main = "Bird RSFD")
    hist(range.size.p, breaks = 100, xlim = c(0, 0.5), xlab = "", ylab = "", main = paste0(k, " ", s, " - patch"))
    hist(range.size.c, breaks = 100, xlim = c(0, 0.5), xlab = "", ylab = "", main = paste0(k, " ", s, " - cohesive"))
  }
}
dev.off()

dir("C:/Users/BorisMNHN/Google Drive/recherche/publis/Co-occurrence problem/example data")

neutral <- simulation
gradient <- simulation

for(i in neutral)
{
  load(paste0("./outputs/neutral_sim", i, "_sp_patch_site_matrix"))
  load(paste0("./outputs/neutral_sim", i, "_sp_cohesive_site_matrix"))
  write.csv(speciespatch.sites.matrix, paste0("./data/examples bradford2/nogradient_uncohesive_", i, ".csv"))
  write.csv(speciescohesive.sites.matrix, paste0("./data/examples bradford2/nogradient_cohesive_", i, ".csv"))
}

for(i in gradient)
{
  load(paste0("./outputs/temp.gradient_sim", i, "_sp_patch_site_matrix"))
  load(paste0("./outputs/temp.gradient_sim", i, "_sp_cohesive_site_matrix"))
  write.csv(speciespatch.sites.matrix, paste0("./data/examples bradford2/gradient_uncohesive", i, ".csv"))
  write.csv(speciescohesive.sites.matrix, paste0("./data/examples bradford2/gradient_cohesive", i, ".csv"))
}

for(i in neutral)
{
  uncohesive <- raster(paste0("./outputs/neutral_sim", i, "_richness_patch"))
  cohesive <- raster(paste0("./outputs/neutral_sim", i, "_richness_cohesive"))
  writeRaster(uncohesive, paste0("./data/examples bradford2/nogradient_uncohesive_richness", i, ".asc"), format = "ascii", overwrite = T)
  writeRaster(cohesive, paste0("./data/examples bradford2/nogradient_cohesive_richness", i, ".asc"), format = "ascii", overwrite = T)
  uncohesive <- getValues(uncohesive)
  uncohesive <- uncohesive[-which(is.na(uncohesive))]
  cohesive <- getValues(cohesive)
  cohesive <- cohesive[-which(is.na(cohesive))]
  write.csv(uncohesive, paste0("./data/examples bradford2/nogradient_uncohesive_richness_", i, ".csv"))
  write.csv(cohesive, paste0("./data/examples bradford2/nogradient_cohesive_richness_", i, ".csv"))
}

for(i in gradient)
{
  uncohesive <- raster(paste0("./outputs/temp.gradient_sim", i, "_richness_patch"))
  cohesive <- raster(paste0("./outputs/temp.gradient_sim", i, "_richness_cohesive"))
  writeRaster(uncohesive, paste0("./data/examples bradford2/gradient_uncohesive_richness", i, ".asc"), format = "ascii", overwrite = T)
  writeRaster(cohesive, paste0("./data/examples bradford2/gradient_cohesive_richness", i, ".asc"), format = "ascii", overwrite = T)
  uncohesive <- getValues(uncohesive)
  uncohesive <- uncohesive[-which(is.na(uncohesive))]
  cohesive <- getValues(cohesive)
  cohesive <- cohesive[-which(is.na(cohesive))]
  write.csv(uncohesive, paste0("./data/examples bradford2/gradient_uncohesive_richness_", i, ".csv"))
  write.csv(cohesive, paste0("./data/examples bradford2/gradient_cohesive_richness_", i, ".csv"))
}


#### mean range sizes ####

simulation <- c("100patches", "250patches", "500patches", "1000patches", "2500patches", "5000patches")
results <- data.frame()
for (s in simulation)
{
  for(k in c("neutral", "temp.gradient"))
  {
    load(paste0("./outputs/", k, "_sim", s, "_sp_patch_site_matrix"))
    load(paste0("./outputs/", k, "_sim", s, "_sp_cohesive_site_matrix"))
    nsites <- nrow(speciespatch.sites.matrix)
    if(any(rowSums(speciespatch.sites.matrix) <= 3))
    {
      speciespatch.sites.matrix <- speciespatch.sites.matrix[-which(rowSums(speciespatch.sites.matrix) <= 3), ]
    }
    if(any(colSums(speciespatch.sites.matrix) == 0))
    {
      speciespatch.sites.matrix <- speciespatch.sites.matrix[, -which(colSums(speciespatch.sites.matrix) == 0)]
    }
    if(any(rowSums(speciescohesive.sites.matrix) <= 3))
    {
      speciescohesive.sites.matrix <- speciescohesive.sites.matrix[-which(rowSums(speciescohesive.sites.matrix) <= 3), ]
    }
    if(any(colSums(speciescohesive.sites.matrix) == 0))
    {
      speciescohesive.sites.matrix <- speciescohesive.sites.matrix[, -which(colSums(speciescohesive.sites.matrix) == 0)]
    }
    range.size.p <- colSums(speciespatch.sites.matrix)
    range.size.c <- colSums(speciescohesive.sites.matrix)
    range.size.ptot <- mean(range.size.p) / nsites
    range.size.prel <- mean(range.size.p) / nrow(speciespatch.sites.matrix)
    
    results <- rbind.data.frame(results,
                                data.frame(simulation = s,
                                           scenario = k,
                                           type = "patch",
                                           number.of.cells = nsites,
                                           number.of.cells.sup.3 = nrow(speciespatch.sites.matrix),
                                           range.size.relative = mean(range.size.p) / nrow(speciespatch.sites.matrix),
                                           range.size.total = mean(range.size.p) / nsites),
                                data.frame(simulation = s,
                                           scenario = k,
                                           type = "cohesive",
                                           number.of.cells = nsites,
                                           number.of.cells.sup.3 = nrow(speciescohesive.sites.matrix),
                                           range.size.relative = mean(range.size.c) / nrow(speciescohesive.sites.matrix),
                                           range.size.total = mean(range.size.c) / nsites))
  }
}

write.table(results, "./data/meanrangesize.txt", sep = "\t")

