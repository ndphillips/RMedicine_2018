
library(yarrr)

png(filename = "images/simulation_speed_1.png", units = "in", width = 6, height = 5, res = 400)
par(mar = c(5, 5, 3, 1))

data.name <- c("arrhythmia", "audiology", "breast", "bridges", "cmc", "credit", "dermatology", "heart", "occupancy", "yeast")
mcu <- c(1.85, 1.73, 1.39, 2.40, 2.06, 1.9, 1.69, 1.72, 1.92, 1.63)
pci <- c(.99, .98, .86, .76, .79, .88, .95, .88, .68, .82)

plot(mcu, pci, xlim = c(1, 10), ylim = c(0, 1), xlab = "", 
     ylab = "", pch = 21, col = "white", 
     bg = yarrr::piratepal("basel", trans = .2), cex = 2, main = "", xaxt = "n", yaxt = "n", type = "n")
grid()
axis(1, 1:10)
axis(2, seq(0, 1, .2), las = 1)

mtext("Speed: Information Used", side = 1, cex = 1.5, line = 3)
mtext("Frugality: Information Ignored", side = 2, cex = 1.5, line = 3)
rect(7, .05, 10, .95, col = yarrr::transparent("white", .2), border = gray(.5, .5))
mtext("FFTrees Speed and Frugality", side = 3, cex = 1.5, line = 1.5)

text(rep(8, length(data.name)), seq(.1, .9, length.out = length(data.name)), labels = data.name, adj = 0)
points(rep(7.5, length(data.name)), seq(.1, .9, length.out = length(data.name)), pch = 21, col = "white", bg = yarrr::piratepal("basel", trans = .2), cex = 2)


dev.off()


png(filename = "images/simulation_speed_2.png", units = "in", width = 6, height = 5, res = 400)

data.name <- c("arrhythmia", "audiology", "breast", "bridges", "cmc", "credit", "dermatology", "heart", "occupancy", "yeast")
mcu <- c(1.85, 1.73, 1.39, 2.40, 2.06, 1.9, 1.69, 1.72, 1.92, 1.63)
pci <- c(.99, .98, .86, .76, .79, .88, .95, .88, .68, .82)

par(mar = c(5, 5, 3, 1))
plot(mcu, pci, xlim = c(1, 10), ylim = c(0, 1), xlab = "", 
     ylab = "", pch = 21, col = "white", 
     bg = yarrr::piratepal("basel", trans = .2), cex = 2, main = "", xaxt = "n", yaxt = "n")
grid()
axis(1, 1:10)
axis(2, seq(0, 1, .2), las = 1)

mtext("Speed: Information Used", side = 1, cex = 1.5, line = 3)
mtext("Frugality: Information Ignored", side = 2, cex = 1.5, line = 3)
mtext("FFTrees Speed and Frugality", side = 3, cex = 1.5, line = 1.5)

rect(7, .05, 10, .95, col = yarrr::transparent("white", .2), border = gray(.5, .5))

text(rep(8, length(data.name)), seq(.1, .9, length.out = length(data.name)), labels = data.name, adj = 0)
points(rep(7.5, length(data.name)), seq(.1, .9, length.out = length(data.name)), pch = 21, col = "white", bg = yarrr::piratepal("basel", trans = .7), cex = 2)

segments(mcu, pci, rep(7.5, length(data.name)), seq(.1, .9, length.out = length(data.name)), col = gray(.5, .2))

dev.off()


png(filename = "images/simulation_speed_3.png", units = "in", width = 6, height = 5, res = 400)
par(mar = c(5, 5, 3, 1))

data.name <- c("arrhythmia", "audiology", "breast", "bridges", "cmc", "credit", "dermatology", "heart", "occupancy", "yeast")
mcu <- c(1.85, 1.73, 1.39, 2.40, 2.06, 1.9, 1.69, 1.72, 1.92, 1.63)
pci <- c(.99, .98, .86, .76, .79, .88, .95, .88, .68, .82)

plot(mcu, pci, xlim = c(1, 10), ylim = c(0, 1), xlab = "", 
     ylab = "", pch = 21, col = "white", 
     bg = yarrr::piratepal("basel", trans = .6), cex = 2, main = "", xaxt = "n", yaxt = "n")
grid()
axis(1, 1:10)
axis(2, seq(0, 1, .2), las = 1)

mtext("FFTrees Speed and Frugality", side = 3, cex = 1.5, line = 1.5)

mtext("Speed: Information Used", side = 1, cex = 1.5, line = 3)
mtext("Frugality: Information Ignored", side = 2, cex = 1.5, line = 3)

rect(7, .05, 10, .95, col = yarrr::transparent("white", .2), border = gray(.5, .5))

text(rep(8, length(data.name)), seq(.1, .9, length.out = length(data.name)), labels = data.name, adj = 0)
points(rep(7.5, length(data.name)), seq(.1, .9, length.out = length(data.name)), pch = 21, col = "white", bg = yarrr::piratepal("basel", trans = .7), cex = 2)

segments(mcu, pci, rep(7.5, length(data.name)), seq(.1, .9, length.out = length(data.name)), col = gray(.5, .2))

points(mean(mcu), mean(pci), cex = 5, lwd = 3, bg = transparent("white", .3), pch = 21)

text(x = mean(mcu) + .5, y = mean(pci), labels = "(1.83, 86%)", adj = 0, cex = 1.5)

dev.off()

