

library(speff2trial)
library(FFTrees)
library(tidyverse)

ACTG175 <- ACTG175[complete.cases(ACTG175), c("age", "wtkg", "drugs", "race", "gender", "cd40", "days")]

ACTG175$days_bin <- ACTG175$days <= 730
set.seed(100)

train.samp <- sample(nrow(ACTG175), size = round(nrow(ACTG175) / 2, 0))
test.samp <- setdiff(1:nrow(ACTG175), train.samp)

ACTG175_train <- ACTG175[train.samp, names(ACTG175) %in% c("days", "cens") == FALSE]
ACTG175_test <- ACTG175[test.samp, names(ACTG175) %in% c("days", "cens") == FALSE]

ACTG175_test_B <- ACTG175_test %>% mutate(
  cd40 = rnorm(nrow(ACTG175_test), mean = 300)
)
  



x <- FFTrees(days_bin ~ ., 
             data = ACTG175_train,
             data.test = ACTG175_test,
             train.p = .5)
