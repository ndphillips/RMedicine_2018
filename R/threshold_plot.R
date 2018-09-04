library(FFTrees)
library(tidyverse)



png("images/threshold.png", width = 7, height = 5, units="in", res = 400)

set.seed(100)

A_x <- rnorm(100, mean = 40, sd = 10)
A_y <- rnorm(100, mean = 40, sd = 5) / 100+ rnorm(100, mean = 0, sd = 1) / 100

B_x <- rnorm(100, mean = 60, sd = 10)
B_y <- rnorm(100, mean = 60, sd = 5) / 100 + rnorm(100, mean = 0, sd = 1)/ 100


layout(matrix(1:2, nrow = 2, ncol = 1), widths = 4, heights = c(2, 2))

par(mar = c(0, 1, 0, 1))

plot(1, xlim = c(0, 100), ylim = c(.35, 1), type = "n", 
     ylab = "", xlab = "X", xaxt = "n", yaxt = "n", bty = "n")

text(2, .9, "Balanced Accuracy", adj = 0)


abline(h = .35)


bacc.v <- sapply(seq(10, 90, 1), FUN = function(x) {
  
  hr <- mean(A_x < x)
  far <- mean(B_x < x)
  
  return((hr + (1 - far)) / 2)
  
  
})


target.x <- c(20, 30,  40, 50, 60, 70, 80)
target.vals <- bacc.v[which(seq(10, 90, 1) %in% target.x)]

segments(x0 = target.x, 
         y0 = rep(0, length(target.x)), x1 = target.x, 
         y1 = target.vals, lty = 2)


points(target.x, target.vals, cex = 3)

lines(x = seq(10, 90, 1), y = bacc.v)

text(target.x, target.vals + .02, labels = round(target.vals, 2), pos = 3)

points(x = target.x - 2, 
       y = rep(.45, length(target.x)), 
       pch = c(1), 
       col = c("red"))

points(x = target.x + 2, 
       y = rep(.45, length(target.x)), 
       pch = c(2), 
       col = c("blue"))


arrows(x0 = target.x - 1, 
       y0 = rep(.41, length(target.x)),
       x1 = target.x - 4, 
       y1 = rep(.41, length(target.x)), length = .1, col = gray(.3)
       )


par(mar = c(5, 1, 0, 1))


plot(1, xlim = c(0, 100), ylim = c(.2, .8), type = "n", 
     ylab = "", xlab = "Feature X", yaxt = "n", bty = 'n', xaxt = "n")

axis(1, at = seq(0, 100, 10))


points(1, .7, pch = 1, col = "red")
text(3, .7, "Class 0", adj = 0)

points(1, .6, pch = 2, col = "blue")

text(3, .6, "Class 1", adj = 0)



points(A_x, A_y, col = "red")

points(B_x, B_y, col = "blue", pch = 2)
segments(x0 = target.x, 
         y0 = 0, x1 = target.x, 
         y1 = 100, lty = 2, col = gray(.5))


dev.off()
