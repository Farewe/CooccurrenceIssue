birdrfsd <- read.table("./data/bird range szi.txt", h = T)


load("./data/S1_sim1_sp_patch_site_matrix")
load("./data/S1_sim1_sp_cohesive_site_matrix")

range.size.p <- colSums(speciespatch.sites.matrix)
range.size.p <- range.size.p / nrow(speciespatch.sites.matrix)
range.size.c <- colSums(speciespatch.sites.matrix)
range.size.c <- range.size.c / nrow(speciespatch.sites.matrix)


par(mfrow = c(3, 1))
hist(birdrfsd$ranges, breaks = 100, xlim = c(0, 0.5))
hist(range.size.p, breaks = 100, xlim = c(0, 0.5))
hist(range.size.c, breaks = 100, xlim = c(0, 0.5))



simulation <- c("100patches", "250patches", "500patches", "1000patches", "2500patches", "5000patches")
pdf('./graphs/RSFD.pdf')
for (s in simulation)
{
  load(paste0("./data/S1_sim", s, "_sp_patch_site_matrix"))
  load(paste0("./data/S1_sim", s, "_sp_cohesive_site_matrix"))
  range.size.p <- colSums(speciespatch.sites.matrix)
  range.size.p <- range.size.p / nrow(speciespatch.sites.matrix)
  range.size.c <- colSums(speciespatch.sites.matrix)
  range.size.c <- range.size.c / nrow(speciespatch.sites.matrix)
  par(mfrow = c(3, 1))
  hist(birdrfsd$ranges, breaks = 100, xlim = c(0, 0.5), xlab = "", ylab = "")
  hist(range.size.p, breaks = 100, xlim = c(0, 0.5), xlab = "", ylab = "")
  hist(range.size.c, breaks = 100, xlim = c(0, 0.5), xlab = "", ylab = "")
}
dev.off()