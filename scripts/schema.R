source("./scripts/initialisation.R")

bio1 <- bio1 / 10
temperature.gradient <- temperature.gradient / 10
bio1 <- setMinMax(bio1)

png("./graphs/schema/s01.png", h = 600, w = 840)
plot(bio1)
dev.off()


png("./graphs/schema/s02.png", h = 600, w = 840)
plot(1, 1, type = "n", xlab = "", ylab = "", yaxt = "n",
     ylim = c(0, 1), bty = "n", las = 1,
     xlim = c(min(temperature.gradient), max(temperature.gradient)))
axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = 1, y1 = 1, angle = 20, code = 3,
       length = 0.12)
text(x = (bio1@data@min + bio1@data@max) / 2, y = .9, "North America\ntemperature range")
dev.off()


png("./graphs/schema/s03.png", h = 600, w = 840)
plot(1, 1, type = "n", xlab = "", ylab = "", yaxt = "n",
     ylim = c(0, 1), bty = "n", las = 1,
     xlim = c(min(temperature.gradient), max(temperature.gradient)))
axis(1, at = c(-100, 100))
abline(v = c(bio1@data@min, bio1@data@max), lty = 2)
arrows(x0 = bio1@data@min, x1 = bio1@data@max,
       y0 = 1, y1 = 1, angle = 20, code = 3,
       length = 0.12)
text(x = (bio1@data@min + bio1@data@max) / 2, y = .9, "North America\ntemperature range")
dev.off()