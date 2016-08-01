source("./scripts/initialisation.R")

cf <- 2

bio1 <- bio1 / 10
temperature.gradient <- temperature.gradient / 10
bio1 <- setMinMax(bio1)

png("./graphs/schema/s01.png", h = 600, w = 840)
plot(bio1)
dev.off()


png("./graphs/schema/s02.png", h = 600, w = 840)
par(mar = c(4.1, 6.6, 4.1, 2.1))
plot(1, 1, type = "n", xlab = "Temperature", ylab = "", yaxt = "n",
     ylim = c(0, 1), bty = "n", las = 1,
     xlim = c(min(temperature.gradient), max(temperature.gradient)))
axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = par()$usr[4], y1 = par()$usr[4], angle = 20, code = 3,
       length = 0.12, xpd = NA)
text(x = (bio1@data@min + bio1@data@max) / 2, y = par()$usr[4] + 0.06 * diff(par()$usr[3:4]), "North America\ntemperature range", xpd = NA)

dev.off()


png("./graphs/schema/s03.png", h = 600, w = 870)
par(mar = c(4.1, 7.6, 4.1, 2.1))
temp <- temperature.gradient * 10
plot((logisticFun(x = temp,
                        alpha = -50, beta = 0.5) / sum(logisticFun(x = temp,
                                                                   alpha = -50, beta = 0.5))
           ~ temperature.gradient), 
     type = "l", xlab = "Optimum temperature",
     bty = "l", lwd = 2,
     xlim = c(min(temperature.gradient), max(temperature.gradient)),
     ylab = "Sampling probability\n", cex.lab = 2, cex.axis = 2)

axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2, lwd = 2)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = par()$usr[4], y1 = par()$usr[4], angle = 20, code = 3,
       length = 0.12, xpd = NA, lwd = 2)
text(x = (bio1@data@min + bio1@data@max) / 2, y = par()$usr[4] + 0.06 * diff(par()$usr[3:4]), 
     "North America\ntemperature range", xpd = NA, cex = 2)
dev.off()

png("./graphs/schema/s04.png", h = 600, w = 840)
par(mar = c(4.1, 6.6, 4.1, 2.1))
plot(rep(1 / length(temperature.gradient), length(temperature.gradient)) ~ temperature.gradient,
     type = "l", xlab = "Optimum temperature", ylim = c(0, 1 / length(temperature.gradient) + 0.2 * 1 / length(temperature.gradient)),
     bty = "l", lwd = cf, cex.lab = cf, cex.axis = cf,
     xlim = c(min(temperature.gradient), max(temperature.gradient)),
     ylab = "Probability of sampling\n")
axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2, lwd = cf)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = par()$usr[4], y1 = par()$usr[4], angle = 20, code = 3,
       length = 0.12, xpd = NA, lwd = cf)
text(x = (bio1@data@min + bio1@data@max) / 2, y = par()$usr[4] + 0.06 * diff(par()$usr[3:4]),
     "North America\ntemperature range", xpd = NA, cex = cf)
dev.off()

probi <- logisticFun(x = temp,
                     alpha = -50, beta = 0.5)
sp.traits <- data.frame(T.optimum = c(20, 40, 5),
                        T.tolerance =  c(10, 20, 30))
library(RColorBrewer)


png("./graphs/schema/s05.png", h = 600, w = 840)
par(mar = c(4.1, 6.6, 4.1, 2.1))
plot(gauss.resp(x. = temperature.gradient, mean. = sp.traits[1, 1],
                diff. = sp.traits[1, 2], prob. = .99) ~ temperature.gradient,
     type = "l", lty = 1, col = brewer.pal(3, "Dark2")[1],
     bty = "l", xlab = "Temperature", ylab = "Probability of occurrence",
     lwd = cf, cex.lab = cf, cex.axis = cf)
lines(gauss.resp(x. = temperature.gradient, mean. = sp.traits[2, 1],
                 diff. = sp.traits[2, 2], prob. = .99) ~ temperature.gradient,
      col = brewer.pal(3, "Dark2")[2], lwd = cf)
lines(gauss.resp(x. = temperature.gradient, mean. = sp.traits[3, 1],
                 diff. = sp.traits[3, 2], prob. = .99) ~ temperature.gradient,
      col = brewer.pal(3, "Dark2")[3], lwd = cf)
axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2, lwd = cf)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = par()$usr[4], y1 = par()$usr[4], angle = 20, code = 3,
       length = 0.12, xpd = NA, lwd = cf)
text(x = (bio1@data@min + bio1@data@max) / 2, y = par()$usr[4] + 0.06 * diff(par()$usr[3:4]), 
     "North America\ntemperature range", xpd = NA, cex = 2)
dev.off()


sp.traits <- data.frame(T.optimum = c(-19, -5, 25),
                        T.tolerance = c(10, 20, 30))
library(RColorBrewer)


png("./graphs/schema/s06.png", h = 600, w = 840)
par(mar = c(4.1, 6.6, 4.1, 2.1))
plot(gauss.resp(x. = temperature.gradient, mean. = sp.traits[1, 1],
                diff. = sp.traits[1, 2], prob. = .99) ~ temperature.gradient,
     type = "l", lty = 1, col = brewer.pal(3, "Dark2")[1],
     bty = "l", xlab = "Temperature", ylab = "Probability of occurrence",
     lwd = cf, cex.lab = cf, cex.axis = cf)
lines(gauss.resp(x. = temperature.gradient, mean. = sp.traits[2, 1],
                 diff. = sp.traits[2, 2], prob. = .99) ~ temperature.gradient,
      col = brewer.pal(3, "Dark2")[2], lwd = cf)
lines(gauss.resp(x. = temperature.gradient, mean. = sp.traits[3, 1],
                 diff. = sp.traits[3, 2], prob. = .99) ~ temperature.gradient,
      col = brewer.pal(3, "Dark2")[3], lwd = cf)
axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2, lwd = cf)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = par()$usr[4], y1 = par()$usr[4], angle = 20, code = 3,
       length = 0.12, xpd = NA, lwd = cf)
text(x = (bio1@data@min + bio1@data@max) / 2, y = par()$usr[4] + 0.06 * diff(par()$usr[3:4]), 
     "North America\ntemperature range", xpd = NA, cex = cf)
dev.off()


library(virtualspecies)
sup <- custnorm(x = 20,
                mean = 20,
                diff = 20,
                prob = 0.99)

sp1 <- generateSpFromFun(raster.stack = bio1,
                         list(bio1 = list(fun = "custnorm", 
                                          args = list(mean = 20,
                                                      diff = 20,
                                                      prob = 0.99))),
                         rescale.each.response = FALSE,
                         rescale = FALSE,
                         plot = FALSE)
sp1$suitab.raster <- sp1$suitab.raster / sup

png("./graphs/schema/s07.png", h = 600, w = 840)
plot(sp1$suitab.raster)
dev.off()

sp1 <- convertToPA(sp1, # PA.method = "threshold",
                   beta = 0.7, alpha = -0.05, 
                   plot = T)
png("./graphs/schema/s08.png", h = 600, w = 840)
plot(sp1$pa.raster)
dev.off()


source("./scripts/functions/patch_generation_version4.1.R")
source("./scripts/functions/growDistribution.R")
source("./scripts/functions/resamp.R")
patches <- generate.patches(bio1, n.patches = 1000, patch.size = 1)
sp1$patched.pa.raster <- overlay(sp1$pa.raster,
                                 patches,
                                 fun = function(x, y) x * y)
png("./graphs/schema/s09.png", h = 600, w = 840)
plot(sp1$patched.pa.raster)
dev.off()



active.cells <- Which(sp1$patched.pa.raster == 1, cells = T)

sp1$cohesive.pa.raster <- growDistribution(x = sp1$pa.raster, size.max = length(active.cells))

png("./graphs/schema/s10.png", h = 600, w = 840)
plot(sp1$cohesive.pa.raster)
dev.off()


