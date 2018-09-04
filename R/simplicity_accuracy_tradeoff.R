line.fun <- function(alpha, 
                     x, 
                     x1, 
                     y1, 
                     x2, 
                     y2) {
  
  w <- (x - x1) / (x2 - x1)
  
  w <- w ^ alpha
  
  
  output <- y1 * (1 - w) + y2 * w
  
  return(output)
  
  
}

library(yarrr)

linear <- function(x) {line.fun(alpha = 1, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}
slow <- function(x) {line.fun(alpha = .5, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}
medium <- function(x) {line.fun(alpha = .75, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}

fast <- function(x) {line.fun(alpha = .1, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}


fft.loc <- c(20, 65)
rf.loc1 <- c(88, 75)
rf.loc2 <- c(88, 79)
cart.loc <- c(27, 72)

linear <- function(x) {line.fun(alpha = 1, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}
slow <- function(x) {line.fun(alpha = .5, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}
medium <- function(x) {line.fun(alpha = .1, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}
fast <- function(x) {line.fun(alpha = .1, x = x, x1 = 0, y1 = 0, x2 = 100, y2 = 80)}



# Blank

pdf(file = "images/world_blank.pdf", width = 7, height = 5)

par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()


mtext(side = 2, text = "Value", line = 3, cex = 1.5)

mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()

## 3

## 2

pdf(file = "images/world_slowgains_1.pdf", width = 7, height = 5)

par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()

text(30, 52, srt = 18, labels = "Accuracy", cex = 2, col = yarrr::transparent("blue", .1), font = 2)
mtext(side = 2, text = "Value", line = 3, cex = 1.5)


mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

curve(slow, from = 0, to = 100, ylim = c(0, 100), xlim = c(0, 100), add = TRUE,lwd= 5, 
      col = yarrr::transparent("blue", .1))

points(80, 70, cex = 10, pch = 22, bg = gray(.1), lwd = 5)
text(80, 70, labels = "Black\nBox", col = "white", cex = 1.2, font = 2)

points(10, 22, cex = 10, pch = 21, bg = "white", lwd = 3)
text(10, 22, labels = "Simple", cex = 1.25)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()



## 4

pdf(file = "images/world_slowgains_2.pdf", width = 7, height = 5)

par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()
mtext(side = 2, text = "Value", line = 3, cex = 1.5)

text(30, 50, srt = 15, labels = "Accuracy", cex = 1, col = yarrr::transparent("blue", .7), font = 2)
text(30, 5, srt = 0, labels = "Cost", cex = 2, col = yarrr::transparent("red", .1), font = 2)

mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

curve(slow, from = 0, to = 100, ylim = c(0, 100), xlim = c(0, 100), add = TRUE,lwd= 5, 
      col = yarrr::transparent("blue", .7))

cost.curve <- function(x) {sqrt(x) * .1}

curve(cost.curve, from = 0, to = 100, add = TRUE, 
      col = yarrr::transparent("red", .1), lwd = 7)

points(80, 4, cex = 10, pch = 22, bg = gray(.1), lwd = 5)
text(80, 4, labels = "Black\nBox", col = "white", cex = 1.2, font = 2)

points(10, 4, cex = 10, pch = 21, bg = "white", lwd = 3)
text(10, 4, labels = "Simple", cex = 1.25)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()



## 5

pdf(file = "images/world_slowgains_3.pdf", width = 7, height = 5)

par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()
mtext(side = 2, text = "Value", line = 3, cex = 1.5)


text(30, 50, srt = 15, labels = "Accuracy", cex = 1, col = yarrr::transparent("blue", .7), font = 1)
text(30, 5, srt = 0, labels = "Cost", cex = 1, col = yarrr::transparent("red", .7), font = 1)

text(30, 50, srt = 10, labels = "Value", cex = 2, col = yarrr::transparent("darkgreen", .1), font = 2)

mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

curve(slow, from = 0, to = 100, ylim = c(0, 100), xlim = c(0, 100), add = TRUE,lwd= 5, 
      col = yarrr::transparent("blue", .7))

cost.curve <- function(x) {sqrt(x) * .1}

curve(cost.curve, from = 0, to = 100, add = TRUE, 
      col = yarrr::transparent("red", .7), lwd = 5)

net.curve <- function(x) {slow(x) - cost.curve(x)}
curve(net.curve, from = 0, to = 100, add = TRUE, 
      col = yarrr::transparent("forestgreen", .3), lwd = 7)

points(80, 78, cex = 10, pch = 22, bg = gray(.1), lwd = 5)
text(80, 78, labels = "Black\nBox", col = "white", cex = 1.2, font = 2)

points(10, 25, cex = 10, pch = 21, bg = "white", lwd = 3)
text(10, 25, labels = "Simple", cex = 1.25)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()



pdf(file = "images/world_fastgains_1.pdf", width = 7, height = 5)


par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()
mtext(side = 2, text = "Value", line = 3, cex = 1.5)


text(30, 78, srt = 7, labels = "Accuracy", cex = 2, col = yarrr::transparent("blue", .1), font = 2)

mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

curve(medium, from = 0, to = 100, ylim = c(0, 100), xlim = c(0, 100), add = TRUE,lwd= 5, 
      col = yarrr::transparent("blue", .1))


points(80, 80, cex = 10, pch = 22, bg = gray(.1), lwd = 5)
text(80, 80, labels = "Black\nBox", col = "white", cex = 1.2, font = 2)

points(10, 62, cex = 10, pch = 21, bg = "white", lwd = 3)
text(10, 62, labels = "Simple", cex = 1.15)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()

## 4

pdf(file = "images/world_fastgains_2.pdf", width = 7, height = 5)

par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()
mtext(side = 2, text = "Value", line = 3, cex = 1.5)

text(30, 78, srt = 7, labels = "Accuracy", cex = 1, col = yarrr::transparent("blue", .7), font = 2)
text(25, 28, srt = 10, labels = "Cost", cex = 2, col = yarrr::transparent("red", .1), font = 2)

text(40, 35, labels = "Transparency")
text(60, 20, labels = "Insight")
text(30, 10, labels = "Efficiency")
text(55, 36, labels = "Clarity")

mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

curve(medium, from = 0, to = 100, ylim = c(0, 100), xlim = c(0, 100), add = TRUE,lwd= 5, 
      col = yarrr::transparent("blue", .7))

cost.curve <- function(x) {sqrt(x) * 4}

curve(cost.curve, from = 0, to = 100, add = TRUE, 
      col = yarrr::transparent("red", .1), lwd = 7)

points(80, 38, cex = 10, pch = 22, bg = gray(.1), lwd = 5)
text(80, 38, labels = "Black\nBox", col = "white", cex = 1.2, font = 2)

points(10, 12, cex = 10, pch = 21, bg = "white", lwd = 3)
text(10, 12, labels = "Simple", cex = 1.25)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()



## 5

pdf(file = "images/world_fastgains_3.pdf", width = 7, height = 5)

par(mar = c(5, 5, 4, 1))
plot(1, xlim = c(0, 100), ylim = c(0, 100), type = "n", 
     xlab = "", yaxt = "n", ylab = "", main = "", bty = "n", cex.main = 2)
grid()
mtext(side = 2, text = "Value", line = 3, cex = 1.5)

text(40, 35, labels = "Transparency", col = gray(.5))
text(60, 20, labels = "Insight", col = gray(.5))
text(30, 10, labels = "Efficiency", col = gray(.5))
text(55, 36, labels = "Clarity", col = gray(.5))

text(30, 78, srt = 7, labels = "Accuracy", cex = 1, col = yarrr::transparent("blue", .7), font = 1)
text(25, 28, srt = 10, labels = "Cost", cex = 1, col = yarrr::transparent("red", .7), font = 1)

text(30, 57, srt = 355, labels = "Value", cex = 2, col = yarrr::transparent("darkgreen", .1), font = 2)

mtext(side = 1, text = "Complexity", line = 3, cex = 1.5)

curve(medium, from = 0, to = 100, ylim = c(0, 100), xlim = c(0, 100), add = TRUE,lwd= 5, 
      col = yarrr::transparent("blue", .7))

cost.curve <- function(x) {sqrt(x) * 4}

curve(cost.curve, from = 0, to = 100, add = TRUE, 
      col = yarrr::transparent("red", .7), lwd = 5)

net.curve <- function(x) {medium(x) - cost.curve(x)}
curve(net.curve, from = 0, to = 100, add = TRUE, 
      col = yarrr::transparent("forestgreen", .3), lwd = 7)

points(80, 42, cex = 10, pch = 22, bg = gray(.1), lwd = 5)
text(80, 42, labels = "Black\nBox", col = "white", cex = 1.2, font = 2)

points(10, 50, cex = 10, pch = 21, bg = "white", lwd = 3)
text(10, 50, labels = "Simple", cex = 1.25)

axis(2, at = seq(0, 100, 20), las = 1)

dev.off()


