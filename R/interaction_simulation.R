
# Interaction simulation


coin_flip <- function(probs) {
  
  sapply(probs, FUN = function(probi) {
    
    sample(x = c(0, 1), prob = c(1-probi, probi), size = 1)}
    
  )
  
}

nsim <- 50
ss <- 1000

design_mtx <- expand.grid(interaction = c(TRUE, FALSE),
                          sim = 1:nsim,
                          rpart_train_acc = NA,
                          fftrees_train_acc = NA,
                          rpart_test_acc = NA,
                          fftrees_test_acc = NA
)

for(i in 1:nrow(design_mtx)) {
  
  data <- tibble(group = sample(c("A", "B"), size = ss, replace = TRUE),
                 x = rnorm(ss, mean = 0, sd = 1),
                 y = rnorm(ss, mean = 0, sd = 1),
                 z = rnorm(ss, mean = 0, sd = 1),
                 noise1 = rnorm(ss, mean = 0, sd = 1),
                 noise2  = rnorm(ss, mean = 0, sd = 1) 
  )
  
  if(design_mtx$interaction[i]) {
    
    data <- data %>% mutate(
      y_prob = case_when(group == "A" ~ 1 / (1 + exp(-(1 * x + .5 * y + .25 * z))),
                         group == "B" ~ 1 / (1 + exp(-(-1 * x+ .5 * y+ .25 * z)))))
    
  } else {
    
    data <- data %>% mutate(
      y_prob = case_when(group == "A" ~ 1 / (1 + exp(-(1 * x+ .5 * y+ .25 * z))),
                         group == "B" ~ 1 / (1 + exp(-(1 * x+ .5 * y+ .25 * z)))))
  }
  
  data <- data %>% mutate(y = coin_flip(y_prob)
  ) %>%
    select(-y_prob)
  
  data_train <- data %>% slice(1:(ss / 2))
  data_test <- data %>% slice((ss / 2 + 1):ss)
  
  rpart_mod <- rpart::rpart(y ~ ., data = data_train, method = "class")
  fftrees_mod <- FFTrees::FFTrees(y ~ ., data = data_train)
  
  rpart_train_acc <- mean((predict(rpart_mod, data_train)[,2] >= .5) == data_train$y)
  fftrees_train_acc <- mean(predict(fftrees_mod, data_train) == data_train$y)
  
  rpart_test_acc <- mean((predict(rpart_mod, data_test)[,2] >= .5) == data_test$y)
  fftrees_test_acc <- mean(predict(fftrees_mod, data_test) == data_test$y)
  
  design_mtx$rpart_train_acc[i] <- rpart_train_acc
  design_mtx$rpart_test_acc[i] <- rpart_test_acc
  design_mtx$fftrees_train_acc[i] <- fftrees_train_acc
  design_mtx$fftrees_test_acc[i] <- fftrees_test_acc
  
  design_mtx$rvf_train_acc[i] <- rpart_train_acc - fftrees_train_acc
  design_mtx$rvf_test_acc[i] <- rpart_test_acc - fftrees_test_acc
  
}


sim_gg <- ggplot(design_mtx, aes(x = interaction, 
                                 y = rvf_test_acc)) + 
  geom_violin() + 
  geom_jitter(width = .1, alpha = .3) +
  stat_summary(col = "red", fun.data = "mean_cl_boot", size = 1.5) +
  geom_hline(aes(yintercept = 0)) +
  labs(y = "CART vs FFTrees\nPositive better for CART", x = "Interaction", title = "When does CART outperform FFTrees?", subtitle = "Answer: When there are strong interactions") + 
  theme_minimal()


ggsave(filename = "images/sim_gg.png", plot = sim_gg, device = "png")

