

library("FFTrees")

heart_FFT <- FFTrees(diagnosis ~.,
                     data = heart.train,
                     data.test = heart.test,
                     main = "Heart Disease",
                     decision.labels = c("Healthy", "Diseased"))



set.seed(100)

heart.test_ex <- heart.test
heart.test_ex$cp <- "aa"
heart.test_ex$diagnosis[heart.test_ex$cp == "aa"] <- sample(c(TRUE, FALSE), 
                                                        size = sum(heart.test_ex$cp == "aa"),
                                                        prob = c(.9, .1), replace = TRUE)

heart_FFT_ex <- FFTrees(diagnosis ~.,
                        data = heart.train,
                        data.test = heart.test_ex,
                        main = "Heart Disease",
                        decision.labels = c("Healthy", "Diseased"))


png(filename = "images/heart_fft_nostats.png", units = "in", width = 12, height = 6, res = 400)

plot(heart_FFT, stats = FALSE)

dev.off()

png(filename = "images/heart_fft_train.png", units = "in", width = 8, height = 7, res = 400)

plot(heart_FFT)

dev.off()

png(filename = "images/heart_fft_test.png", units = "in", width = 8, height = 7, res = 400)

plot(heart_FFT, data = "test")

dev.off()


pdf(file =  "images/heart_fft_train.pdf", width = 8, height = 7)

plot(heart_FFT, data = "train")

dev.off()


pdf(file =  "images/heart_fft_test.pdf", width = 8, height = 7)

plot(heart_FFT, data = "test")

dev.off()



png(filename = "images/heart_fft_test_tree6.png", units = "in", width = 8, height = 7, res = 400)

plot(heart_FFT, data = "test", tree = 6)

dev.off()


png(filename = "images/heart_fft_test_tree7.png", units = "in", width = 8, height = 7, res = 400)

plot(heart_FFT, data = "test", tree = 7)

dev.off()

png(filename = "images/heart_fft_test_tree6.png", units = "in", width = 8, height = 7, res = 400)

plot(heart_FFT, data = "test", tree = 6)

dev.off()



png(filename = "images/heart_fft_ex_train.png", units = "in", width = 7.5, height = 6.5, res = 400)

plot(heart_FFT_ex, data = "train", label.tree = "Heart FFT", main = "2017 Data")

dev.off()



png(filename = "images/heart_fft_ex_test.png", units = "in", width = 7.5, height = 6.5, res = 400)

plot(heart_FFT_ex, data = "test", label.tree = "Heart FFT", main = "2018 Data")

dev.off()

