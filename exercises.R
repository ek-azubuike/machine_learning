library(dslabs)
library(tidyverse)
library(lubridate)
data(reported_heights)

dat <- mutate(reported_heights, date_time = ymd_hms(time_stamp)) %>%
  filter(date_time >= make_date(2016, 01, 25) & date_time < make_date(2016, 02, 1)) %>%
  mutate(type = ifelse(day(date_time) == 25 & hour(date_time) == 8 & between(minute(date_time), 15, 30), "inclass","online")) %>%
  select(sex, type)

y <- factor(dat$sex, c("Female", "Male"))
x <- dat$type

# What proportion of the inclass group is female? 
dat %>% 
  filter(type == "inclass") %>% 
  group_by(sex) %>% 
  summarise(per = n() / nrow(.))

# What proportion of the online group is female?
dat %>% 
  filter(type == "online") %>% 
  group_by(sex) %>% 
  summarise(per = n() / nrow(.))

# Report the accuracy of your prediction of sex based on type.
y.hat <- ifelse(dat$type == "online", "Male", "Female")
y.hat <- factor(y.hat, c("Female", "Male"))
accuracy <- mean(y.hat == y)
accuracy

# Write a line of code using the table() function to show the confusion matrix
# between y_hat and y.
cm <- table(y.hat, y)
cm

# What is the sensitivity of this prediction?
sensitivity(cm)

# What is the specificity of this prediction? 
specificity(cm)

# What is the prevalence (% of females) in the dat dataset defined above?
dat %>% 
  group_by(sex) %>% 
  summarize(per = n() / nrow(.))

library(caret)
data(iris)
iris <- iris[-which(iris$Species=='setosa'),]
y <- iris$Species

set.seed(76)
test_index <- createDataPartition(y, 
                                  times = 1, 
                                  p = 0.5, 
                                  list = F)
test <- iris[test_index,]
train <- iris[-test_index,]

cutoffs <- sapply(train[,-5], function(x){
  low <- min(x)
  high <- max(x)
  cutoff <- seq(from = low, to = high, by = 0.1)
  })

sl <- sapply(cutoffs$Sepal.Length, function(x) {
  y.hat <- ifelse(train$Sepal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y.hat == train$Species)
}) %>% 
  cbind(cutoffs$Sepal.Length)

sw <- sapply(cutoffs$Sepal.Width, function(x) {
  y.hat <- ifelse(train$Sepal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y.hat == train$Species)
}) %>% 
  cbind(cutoffs$Sepal.Width)

pl <- sapply(cutoffs$Petal.Length, function(x) {
  y.hat <- ifelse(train$Petal.Length > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y.hat == train$Species)
}) %>% 
  cbind(cutoffs$Petal.Length)

pw <- sapply(cutoffs$Petal.Width, function(x) {
  y.hat <- ifelse(train$Petal.Width > x, "virginica", "versicolor") %>% 
    factor(levels = levels(train$Species))
  mean(y.hat == train$Species)
}) %>% 
  cbind(cutoffs$Petal.Width)

# best feature 
max(sl[,1])
max(sw[,1])
max(pl[,1])
max(pw[,1])

# best cutoff
pw[which.max(pw[,1]),2]

# test accuracy
y.hat <- map_chr(test$Petal.Width, ~{ifelse(. > 1.5, "virginica", "versicolor")}) %>% 
  factor(levels = levels(test$Species))
mean(y.hat == test$Species)

plot(iris, pch=21, bg=iris$Species)

# best two cuttoffs
plc <- pl[which.max(pl[,1]),2]
pwc <- pw[which.max(pw[,1]),2]

# two factor accuracy
y.hat <- ifelse(test$Petal.Length > plc & test$Petal.Width > pwc,
                "virginica", "versicolor") %>% 
  factor(levels = levels(test$Species))
mean(y.hat == test$Species)

# The test is positive 85% of the time when tested on a patient with the 
# disease (high sensitivity):
# The test is negative 90% of the time when tested on a healthy patient 
# (high specificity):
# The disease is prevalent in about 2% of the community:
# Using Bayes' theorem, calculate the probability that you have the disease 
# if the test is positive. 

p.disease <- 0.02
p.healthy <- 1 - p.disease
p.healthy <- 1 - p.disease
p.pos.disease <- 0.85
p.neg.healthy <- 0.9
p.pos.healthy <- 1 - p.neg.healthy
p.pos <- (p.pos.disease * p.disease) + (p.pos.healthy * p.healthy)

p.disease.pos <- p.pos.disease * (p.disease / p.pos)

set.seed(1) 
disease <- sample(c(0,1), size=1e6, 
                  replace=TRUE, 
                  prob=c(0.98,0.02)) # vector of patients
test <- rep(NA, 1e6) # vector of test results
test[disease==0] <- sample(c(0,1),
                           size=sum(disease==0),
                           replace=TRUE, 
                           prob=c(0.90,0.10))
test[disease==1] <- sample(c(0,1), 
                           size=sum(disease==1), 
                           replace=TRUE, 
                           prob=c(0.15, 0.85))

# What is the probability that a test is positive?
sum(test==1) / length(test)

# What is the probability that an individual has the disease if the 
# test is negative?
sum(disease[disease == 1 & test == 0]) / length(test)

# What is the probability that you have the disease if the test is positive?
sum(disease[disease == 1 & test == 1]) / sum(test == 1)

# Compare the prevalence of disease in people who test positive to the 
# overall prevalence of disease.
# If a patient's test is positive, by how many times does that increase 
# their risk of having the disease?
0.1471762 / p.disease

library(dslabs)
data("heights")

heights %>% 
  mutate(height = round(height)) %>% 
  group_by(height) %>% 
  summarize(p = mean(sex == "Male")) %>% 
  qplot(height, p, data =.)

# In the plot we just made in Q6 we see high variability for low values of 
# height. This is because we have few data points. This time use the quantile 
# and the cut() function to assure each group has the same number of points. 
# Note that for any numeric vector x, you can create groups based on quantiles 
# like this: cut(x, quantile(x, seq(0, 1, 0.1)), include.lowest = TRUE). 
ps <- seq(0, 1, 0.1)

heights %>% 
  mutate(g = cut(height, quantile(height, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(p = mean(sex == "Male"), height = mean(height)) %>%
  qplot(height, p, data =.)

# You can generate data from a bivariate normal distrubution using the MASS 
# package using the following code: 
Sigma <- 9*matrix(c(1,0.5,0.5,1), 2, 2)
dat <- MASS::mvrnorm(n = 10000, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

plot(dat)

# estimate the conditional expectations and make a plot.
ps <- seq(0, 1, 0.1)
dat %>% 
  mutate(g = cut(x, quantile(x, ps), include.lowest = TRUE)) %>%
  group_by(g) %>%
  summarize(y = mean(y), x = mean(x)) %>%
  qplot(x, y, data =.)

library(tidyverse)
library(caret)

# We will build 100 linear models using the data above and calculate the mean 
# and standard deviation of the combined models. First, set the seed to 1 
# again. Then, within a replicate() loop, (1) partition the dataset into test 
# and training sets with p = 0.5 and using dat$y to generate your indices, 
# (2) train a linear model predicting y from x, (3) generate predictions on 
# the test set, and (4) calculate the RMSE of that model. Then, report the 
# mean and standard deviation (SD) of the RMSEs from all 100 models.

set.seed(1, sample.kind = "Rounding")
n <- 100
Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))


sims <- replicate(n, {
  ind <- createDataPartition(dat$y, times = 1, p = 0.5)$Resample1
  train <- dat[ind,]
  test <- dat[-ind,]
  fit <- lm(y ~ x, data = train)
  preds <- predict(fit, test, type = "response")
  errors <- RMSE(preds, test$y)
  errors
})

mean(sims)
sd(sims)

# Write a function that takes a size n, then (1) builds a dataset using the 
# code provided at the top of Q1 but with n observations instead of 100 and 
# without the set.seed(1), (2) runs the replicate() loop that you wrote to 
# answer Q1, which builds 100 linear models and returns a vector of RMSEs, 
# and (3) calculates the mean and standard deviation of the 100 RMSEs.

#Set the seed to 1 and then use sapply() or map() to apply your new function 
# to n <- c(100, 500, 1000, 5000, 10000).

sim_n <- function(n) {
  set.seed(1, sample.kind = "Rounding")
  
  Sigma <- 9*matrix(c(1.0, 0.5, 0.5, 1.0), 2, 2)
  dat <- MASS::mvrnorm(n = n, c(69, 69), Sigma) %>%
    data.frame() %>% setNames(c("x", "y"))
  
  sims <- replicate(100, {
    ind <- createDataPartition(dat$y,
                               times = 1,
                               p = 0.5,
                               list = F)
    train <- dat[ind,]
    test <- dat[-ind,]
    fit <- lm(y ~ x, data = train)
    preds <- predict(fit, test, type = "response")
    errors <- RMSE(preds, test$y)
    errors
  })
  
  c(mean(sims), sd(sims))
}

n <- c(100, 500, 1000, 5000, 10000)

results <- sapply(n, sim_n)
results

# Now repeat the exercise from Q1, this time making the correlation between 
# x and y larger, as in the following code:
set.seed(1)
n <- 100
Sigma <- 9*matrix(c(1.0, 0.95, 0.95, 1.0), 2, 2)
dat <- MASS::mvrnorm(n = 100, c(69, 69), Sigma) %>%
  data.frame() %>% setNames(c("x", "y"))

sims <- replicate(100, {
  ind <- createDataPartition(dat$y, 
                             times = 1, 
                             p = 0.5, 
                             list = F)
  train <- dat[ind,]
  test <- dat[-ind,]
  fit <- lm(y ~ x, data = train)
  preds <- predict(fit, test, type = "response")
  RMSE(preds, test$y)
})

c(mean(sims), sd(sims))

set.seed(1)
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.25, 0.75, 0.25, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))
# Note that y is correlated with both x_1 and x_2 but the two predictors are 
# independent of each other, as seen by cor(dat).

# Set the seed to 1, then use the caret package to partition into test and 
# training sets with p = 0.5. Compare the RMSE when using just x_1, just x_2 
# and both x_1 and x_2. Train a single linear model for each (not 100 like in 
# the previous questions).

ind <- createDataPartition(dat$y, list = F)
train <- dat[ind,]
test <- dat[-ind,]

x1.fit <- lm(y ~ x_1, data = train)
x2.fit <- lm(y ~ x_2, data = train)
x1.x2.fit <- lm(y ~ x_1 + x_2, data = train)
fits <- list(x1.fit, x2.fit, x1.x2.fit)

results <- lapply(1:length(fits), function(x){
  preds <- predict(fits[[x]], test, type = "response")
  RMSE(preds, test$y)
})

results

# Repeat the exercise from Q6 but now create an example in which x_1 and x_2 
# are highly correlated.
Sigma <- matrix(c(1.0, 0.75, 0.75, 0.75, 1.0, 0.95, 0.75, 0.95, 1.0), 3, 3)
dat <- MASS::mvrnorm(n = 100, c(0, 0, 0), Sigma) %>%
  data.frame() %>% setNames(c("y", "x_1", "x_2"))

ind <- createDataPartition(dat$y, list = F)
train <- dat[ind,]
test <- dat[-ind,]

x1.fit <- lm(y ~ x_1, data = train)
x2.fit <- lm(y ~ x_2, data = train)
x1.x2.fit <- lm(y ~ x_1 + x_2, data = train)
fits <- list(x1.fit, x2.fit, x1.x2.fit)

results <- lapply(1:length(fits), function(x){
  preds <- predict(fits[[x]], test, type = "response")
  RMSE(preds, test$y)
})

results

# Smoothing
library(tidyverse)
library(lubridate)
library(purrr)
library(pdftools)

fn <- system.file("extdata", "RD-Mortality-Report_2015-18-180531.pdf", package="dslabs")
dat <- map_df(str_split(pdf_text(fn), "\n"), function(s){
  s <- str_trim(s)
  header_index <- str_which(s, "2015")[1]
  tmp <- str_split(s[header_index], "\\s+", simplify = TRUE)
  month <- tmp[1]
  header <- tmp[-1]
  tail_index  <- str_which(s, "Total")
  n <- str_count(s, "\\d+")
  out <- c(1:header_index, which(n==1), which(n>=28), tail_index:length(s))
  s[-out] %>%
    str_remove_all("[^\\d\\s]") %>%
    str_trim() %>%
    str_split_fixed("\\s+", n = 6) %>%
    .[,1:5] %>%
    as_tibble() %>% 
    setNames(c("day", header)) %>%
    mutate(month = month,
           day = as.numeric(day)) %>%
    gather(year, deaths, -c(day, month)) %>%
    mutate(deaths = as.numeric(deaths))
}) %>%
  mutate(month = recode(month, "JAN" = 1, "FEB" = 2, "MAR" = 3, "APR" = 4, "MAY" = 5, "JUN" = 6, 
                        "JUL" = 7, "AGO" = 8, "SEP" = 9, "OCT" = 10, "NOV" = 11, "DEC" = 12)) %>%
  mutate(date = make_date(year, month, day)) %>%
  dplyr::filter(date <= "2018-05-01")

str(dat)
n.days <- as.numeric(diff(range(dat$date)))
span <- 60 / n.days
fit <- loess(deaths ~ as.numeric(date), 
             span = span, 
             degree = 1,
             data = dat)

dat %>% 
  mutate(smoothed = predict(fit, as.numeric(date))) %>% 
  ggplot(aes(x = date, y = deaths)) +
  geom_point(alpha = 0.5) +
  geom_line(aes(x = date, 
                y = smoothed), 
            color = "red",
            size = 1)
# Work with the same data as in Q1 to plot smooth estimates against day of the 
# year, all on the same plot, but with different colors for each year.
dat %>% 
  mutate(smoothed = predict(fit, as.numeric(date)),
         day = yday(date)) %>% 
  ggplot(aes(x = day,
             y = smoothed,
             color = year)) +
  geom_line(lwd = 2)

# Suppose we want to predict 2s and 7s in the mnist_27 dataset with just the 
# second covariate. Can we do this? On first inspection it appears the data 
# does not have much predictive power.
# In fact, if we fit a regular logistic regression the coefficient for x_2 is 
# not significant!
library(broom)
mnist_27$train %>% 
  glm(y ~ x_2,
      family = "binomial", # fit logistic regression
      data = .) %>% 
  tidy()

# Fit a loess line on the train set to predict the 2s and 7s with just the 
# second covariate and degree=1. What is the accuracy of the prediction in the
# test set?
fit <- mnist_27$train %>% 
  mutate(y = ifelse(y == 7, 1, 0)) %>% # convert y to binary factor
  loess(y ~ x_2, degree = 1, data = .)

p.hat <- predict(fit, mnist_27$test, type = "response")
preds <- factor(ifelse(p.hat > 0.5, 7, 2)) # convert probabilities back to preds
confusionMatrix(mnist_27$test$y, preds)

# KNN
library(caret)
library(tidyverse)
library(dslabs)
data("heights")
set.seed(1)
ind <- createDataPartition(heights$height, list = F)
train <- heights[ind,]
test <- heights[-ind,]
k <- seq(1, 101, 3)

fits <- sapply(k, function(x){
  fit <- knn3(sex ~ height, 
              data = train, 
              k = x)
  preds <- predict(fit, 
                   test, 
                   type = "class")
  F_meas(data = preds, reference = test$sex)
})

max(fits)

cbind(k, fits)[which.max(fits),]

# First, set the seed to 1 and split the data into training and test sets with 
# p = 0.5. Then, report the overall accuracy you obtain from predicting tissue 
# type using KNN with k = seq(1, 11, 2) using sapply() or map_df(). 
# Note: use the createDataPartition() function outside of sapply() or map_df(). 
data("tissue_gene_expression")

set.seed(1)
ind <- createDataPartition(tissue_gene_expression$y, list = F)
train.x <- tissue_gene_expression$x[ind,] %>% 
  data.frame()
train.y <- tissue_gene_expression$y[ind]
training <- cbind(train.x, GENE = train.y)

test.x <- tissue_gene_expression$x[-ind,] %>% 
  data.frame()
test.y <- tissue_gene_expression$y[-ind]
testing <- cbind(test.x, GENE = test.y)

k <- seq(1, 11, 2)
acc <- sapply(k, function(x){
  fit <- knn3(GENE ~ ., data = training, k = x)
  preds <- predict(fit, testing, type = "class")
  cm <- confusionMatrix(preds, testing$GENE)
  cm$overall["Accuracy"]
})

str(acc)
acc

fit <- knn3(GENE ~ ., data = training, k = 9)
preds <- predict(fit, testing, type = "class")
cm <- confusionMatrix(preds, testing$GENE)
cm$overall["Accuracy"]

# Bootstrap
library(dslabs)
library(caret)

data(mnist_27)

set.seed(1995)
indexes <- createResample(mnist_27$train$y, 10)

# How many times do 3, 4, and 7 appear in the first resampled index?
indexes$Resample01[indexes$Resample01 == 3]
indexes$Resample01[indexes$Resample01 == 4]
indexes$Resample01[indexes$Resample01 == 7]

# What is the total number of times that 3 appears in all of the 
# resampled indexes?
total <- 0

for (i in 1:10){
  total <- total + sum(indexes[[i]] == 3)
}

#  Now, set the seed to 1 and perform a Monte Carlo simulation with 10,000 
# repetitions, generating the random dataset and estimating the 75th quantile 
# each time. What is the expected value and standard error of the 75th quantile?
set.seed(1)
B <- 10E3

sims <- replicate(B, {
  y <- rnorm(100, 0, 1)
  quantile(y, 0.75)
})
mean(sims)
sd(sims)

# Set the seed to 1 again after generating y and use 10,000 bootstrap samples 
# to estimate the expected value and standard error of the 75th quantile.
set.seed(1)
y <- rnorm(100, 0, 1)

sims <- replicate(B, {
  mc <- sample(y, 100, replace = T)
  quantile(mc, 0.75)
})

mean(sims)
sd(sims)

# Cross-Validation
library(tidyverse)
library(caret)

set.seed(1996)
n <- 1000
p <- 10000
x <- matrix(rnorm(n*p), n, p)
colnames(x) <- paste("x", 1:ncol(x), sep = "_")
y <- rbinom(n, 1, 0.5) %>% factor()

x_subset <- x[ ,sample(p, 100)]

# running cross-validation using logistic regression to fit the model.
fit <- train(x_subset, y, method = "glm")
fit$results

# Now, instead of using a random selection of predictors, we are going to 
# search for those that are most predictive of the outcome. We can do this by 
# comparing the values for the group to those in the group, for each predictor, 
# using a t-test.

pvals <- rep(0, ncol(x))
for (i in 1:ncol(x)) {
  pvals[i] <- t.test(x[,i][y==0], x[,i][y==1], var.equal=TRUE)$p.value
}

ind <- pvals <= 0.01
str(x[, ind])

x.sig <- x[, ind]

set.seed(1)
fit <- train(x.sig, y, method = "glm")
fit$results

set.seed(1)
fit <- train(x.sig,
             y, 
             method = "knn", 
             tuneGrid = data.frame(k = seq(101, 301, 25)))
ggplot(fit)

# Trees and Random Forests
# Create a simple dataset where the outcome grows 0.75 units on average for 
# every increase in a predictor
library(rpart)
n <- 1000
sigma <- 0.25
set.seed(1)
x <- rnorm(n, 0, 1)
y <- 0.75 * x + rnorm(n, 0, sigma)
dat <- data.frame(x = x, y = y)

fit <- rpart(y ~ ., data = dat) 
plot(fit, margin = 0.1)
text(fit, cex = 0.75)

dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), color = "red")

#Now run Random Forests instead of a regression tree using randomForest() from 
# the randomForest package, and remake the scatterplot with the prediction line.
library(randomForest)
fit <- randomForest(y ~ ., data = dat)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

# Use the plot() function to see if the Random Forest from Q4 has converged 
# or if we need more trees.
plot(fit)

# It seems that the default values for the Random Forest result in an estimate 
# that is too flexible (unsmooth). Re-run the Random Forest but this time with 
# a node size of 50 and a maximum of 25 nodes. Remake the plot.
library(caret)
library(tidyverse)
library(randomForest)
library(dslabs)
library(rpart)
data("tissue_gene_expression")

fit <- randomForest(y ~ .,
                    data = dat,
                    nodesize = 50,
                    maxnodes = 25)
dat %>% 
  mutate(y_hat = predict(fit)) %>% 
  ggplot() +
  geom_point(aes(x, y)) +
  geom_step(aes(x, y_hat), col = "red")

plot(fit)

# Load the rpart package and then use the caret::train() function with
# method = "rpart" to fit a classification tree to the tissue_gene_expression
# dataset. Try out cp values of seq(0, 0.1, 0.01). Plot the accuracies to 
# report the results of the best model. Set the seed to 1991.
set.seed(1991)
ind <- createDataPartition(tissue_gene_expression$y, p = 0.8, list = F)
train.x <- tissue_gene_expression$x[ind,]
train.y <- tissue_gene_expression$y[ind]
test.x <- data.frame(tissue_gene_expression$x[-ind,])
test.y <- tissue_gene_expression$y[-ind]

dat <- data.frame(train.x, train.y) %>% 
  rename(GENE = train.y)

fit <- train(GENE ~ .,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
             data = dat)
y.hat <- predict(fit, test.x)
confusionMatrix(y.hat, test.y)
fit$bestTune
plot(fit)

# Note that there are only 6 placentas in the dataset. By default, rpart 
# requires 20 observations before splitting a node. That means that it is 
# difficult to have a node in which placentas are the majority. Rerun the 
# analysis you did in Q1 with caret::train(), but this time with 
# method = "rpart" and allow it to split any node by using the argument 
# control = rpart.control(minsplit = 0). Look at the confusion matrix again 
# to determine whether the accuracy increases.
set.seed(1991)
fit <- train(GENE ~ .,
             method = "rpart",
             tuneGrid = data.frame(cp = seq(0, 0.1, 0.01)),
             control = rpart.control(minsplit = 0),
             data = dat)
y.hat <- predict(fit, test.x)
confusionMatrix(y.hat, test.y)

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel)

# We can see that with just seven genes, we are able to predict the tissue 
# type. Now let's see if we can predict the tissue type with even fewer genes 
# using a Random Forest. Use the train() function and the rf method to train 
# a Random Forest model and save it to an object called fit. Try out values 
# of mtry ranging from seq(50, 200, 25) (you can also explore other values on
# your own). What mtry value maximizes accuracy? To permit small nodesize to
# grow as we did with the classification trees, use the following 
# argument: nodesize = 1.

set.seed(1991)
fit <- train(GENE ~ .,
             method = "rf",
             tuneGrid = data.frame(mtry = seq(50, 200, 25)),
             nodesize = 1,
             data = dat)
fit$bestTune
plot(fit$finalModel)
plot(fit)
fit$results

# Titanic Exercises
library(titanic)
library(caret)
library(tidyverse)
library(rpart)

options(digits = 3)

# clean the data - `titanic_train` is loaded with the titanic package
titanic_clean <- titanic_train %>%
  mutate(Survived = factor(Survived),
         Embarked = factor(Embarked),
         Age = ifelse(is.na(Age), median(Age, na.rm = TRUE), Age), # NA age to median age
         FamilySize = SibSp + Parch + 1) %>%    # count family members
  select(Survived,  Sex, Pclass, Age, Fare, SibSp, Parch, FamilySize, Embarked)
# 20% partition for testing
set.seed(42)
ind <- createDataPartition(y = titanic_clean$Survived,
                           p = 0.2, 
                           list = F)
test.titanic <- titanic_clean[ind,]
train.titanic <- titanic_clean[-ind,]
dim(train.titanic)
dim(test.titanic)

sum(train.titanic$Survived == 1) / nrow(train.titanic)

set.seed(3)

y.hat <- sample(c(0,1),
                length(test.titanic$Survived),
                replace = T) %>% 
  factor(levels = levels(test.titanic$Survived))

confusionMatrix(y.hat, test.titanic$Survived)


train.titanic %>% 
  group_by(Sex, Survived) %>% 
  summarize(n = n()) %>% 
  mutate(per = n / sum(n))

sum(train.titanic$Sex == "female" & train.titanic$Survived == 1) / sum(train.titanic$Sex == "female")

sex.based <- factor(ifelse(test.titanic$Sex == "male", 0, 1),
                    levels = levels(test.titanic$Survived))
sex.cm <- confusionMatrix(sex.based, test.titanic$Survived)

train.titanic %>% 
  group_by(Pclass, Survived) %>% 
  summarize(n = n()) %>% 
  mutate(per = n / sum(n))

class.based <- factor(ifelse(test.titanic$Pclass == 1, 1, 0),
                      levels = levels(test.titanic$Survived))
class.cm <- confusionMatrix(class.based, test.titanic$Survived)

train.titanic %>% 
  group_by(Sex, Pclass, Survived) %>% 
  summarize(n = n()) %>% 
  mutate(per = n / sum(n))

sex.class <- factor(ifelse(test.titanic$Sex == "female" & test.titanic$Pclass %in% c(1, 2), 1, 0),
                    levels = levels(test.titanic$Survived))
sex.class.cm <- confusionMatrix(sex.class, test.titanic$Survived)

# compare balanced accuracy of predictions
sex.cm$byClass[["Balanced Accuracy"]]
class.cm$byClass[["Balanced Accuracy"]]
sex.class.cm$byClass[["Balanced Accuracy"]]

F_meas(sex.class, test.titanic$Survived)
F_meas(sex.based, test.titanic$Survived)
F_meas(class.based, test.titanic$Survived)

# Set the seed to 1. Train a model using Loess with the caret gamLoess
# method using fare as the only predictor.
set.seed(1)

fit <- train(Survived ~ Fare,
             method = "gamLoess",
             data = train.titanic)

p.hat <- predict(fit, test.titanic, type = "prob")
y.hat <- ifelse(p.hat$`1` > 0.5, 1, 0) %>% 
  factor(levels = levels(train.titanic$Survived))
confusionMatrix(y.hat, test.titanic$Survived)

# Set the seed to 1. Train a logistic regression model with the caret glm 
# method using age as the only predictor. 
set.seed(1)
fit <- train(Survived ~ Age,
             data = train.titanic,
             method = "glm")
y.hat <- predict(fit, test.titanic, type = "raw")
age.cm <- confusionMatrix(y.hat, test.titanic$Survived)

set.seed(1)
fit <- train(Survived ~ Sex + Pclass + Fare + Age,
             data = train.titanic,
             method = "glm")
sex.class.fare.age.cm <- confusionMatrix(predict(fit, test.titanic), test.titanic$Survived)

set.seed(1)
fit <- train(Survived ~ .,
             data = train.titanic,
             method = "glm")
confusionMatrix(predict(fit, test.titanic), test.titanic$Survived)

# Q9 kNN
set.seed(6)
ks <- seq(3, 51, 2)

fit <- train(Survived ~ .,
             data = train.titanic,
             method = "knn",
             tuneGrid = data.frame(k = ks))
fit$bestTune

plot(fit)

# Of these values of , which yields the highest accuracy?
fit$results %>%
  filter(k %in% c(7, 11, 15, 21, 23)) %>% 
  arrange(desc(Accuracy))

# What is the accuracy of the kNN model on the test set?
confusionMatrix(predict(fit, test.titanic), test.titanic$Survived)

set.seed(8)
controls <- trainControl(method = "cv", number = 10, p = 0.1)
fit <- train(Survived ~ .,
             data = train.titanic,
             method = "knn",
             trControl = controls,
             tuneGrid = data.frame(k = ks))
fit$bestTune
confusionMatrix(predict(fit, test.titanic), test.titanic$Survived)

# Set the seed to 10. Use caret to train a decision tree with the rpart method. 
# Tune the complexity parameter with cp = seq(0, 0.05, 0.002).
set.seed(10)
cps <- seq(0, 0.05, 0.002)
fit <- train(Survived ~ .,
             data = train.titanic,
             method = "rpart",
             tuneGrid = data.frame(cp = cps))
fit$bestTune
confusionMatrix(predict(fit, test.titanic), test.titanic$Survived)

plot(fit$finalModel, margin = 0.1)
text(fit$finalModel, cex = 0.75)

# Set the seed to 14. Use the caret train() function with the rf method 
# to train a random forest. Test values of mtry = seq(1:7). Set ntree to 100.
set.seed(14)

fit <- train(Survived ~ .,
             data = train.titanic,
             method = "rf",
             tuneGrid = data.frame(mtry = seq(1:7)),
             ntree = 100)
fit$bestTune
plot(fit)

confusionMatrix(predict(fit, test.titanic), test.titanic$Survived)

# Ensembles
models <- c("glm", "lda", "naive_bayes", "knn",
            "gamLoess", "qda", "rf")

library(caret)
library(dslabs)
library(tidyverse)
set.seed(1)
data("mnist_27")

fits <- lapply(models, function(model){ 
  print(model)
  train(y ~ ., method = model, data = mnist_27$train)
}) 

names(fits) <- models

fits

# Now that you have all the trained models in a list, use sapply() or map() 
# to create a matrix of predictions for the test set. You should end up with 
# a matrix with length(mnist_27$test$y) rows and length(models) columns.
preds <- sapply(fits, function(model){
  y.hat <- predict(model, mnist_27$test)
}) %>% 
  data.frame()

str(preds)

# Now compute accuracy for each model on the test set.
acc <- sapply(preds, function(x){
  x <- factor(x, levels = levels(mnist_27$test$y))
  cm <- confusionMatrix(x, mnist_27$test$y)
  cm$overall["Accuracy"]
})

acc
mean(acc)

# Next, build an ensemble prediction by majority vote and compute the accuracy 
# of the ensemble. Vote 7 if more than 50% of the models are predicting a 7, and 2 otherwise.
preds$votes <- rowSums(preds[,1:7] == 7)
preds$ens <- ifelse(preds$votes > 3, 7, 2)
preds$ens <- factor(preds$ens, levels = levels(mnist_27$test$y))

confusionMatrix(preds$ens, mnist_27$test$y)

# use the minimum accuracy estimates obtained from cross validation with the 
# training data for each model from fit$results$Accuracy. Obtain these 
# estimates and save them in an object. Report the mean of these training 
# set accuracy estimates.
min.acc <- sapply(fits, function(model){
  min(model$results$Accuracy)
})

mean(min.acc)

min.acc.preds <- preds %>% select(naive_bayes, gamLoess, qda)
min.acc.preds$votes <- rowSums(min.acc.preds[,1:3] == 7)
min.acc.preds$ens <- ifelse(min.acc.preds$votes > 1, 7, 2)
min.acc.preds$ens <- factor(min.acc.preds$ens, levels = levels(mnist_27$test$y))

confusionMatrix(min.acc.preds$ens, mnist_27$test$y)

# Recommendation Systems
library(tidyverse)
library(lubridate)
library(dslabs)
data("movielens")

movielens %>% 
  group_by(movieId, year) %>% 
  summarise(n = n()) %>% 
  ggplot(aes(x = year, y = sqrt(n), group = year)) +
  geom_boxplot()

movielens %>% 
  group_by(year) %>% 
  summarize(median = median(n())) %>% 
  arrange(desc(median))

movielens %>% 
  filter(str_detect(title, "Shawshank")) %>% 
  pull(movieId)

movielens %>% 
  filter(str_detect(title, "Forrest Gump")) %>% 
  pull(movieId)

movielens %>% 
  filter(year >= 1993) %>% 
  group_by(movieId, year) %>% 
  reframe(count = n(),
          per.year.ratings = count / (2018 - year),
          avg.rating = mean(rating)) %>% 
  distinct() %>% 
  arrange(desc(per.year.ratings)) %>%
  slice(1:25)

# stratify the post-1993 movies by ratings per year and compute 
# their average ratings
movielens %>% 
  filter(year >= 1993) %>% 
  group_by(movieId, year) %>% 
  reframe(count = n(),
          per.year.ratings = round(count / (2018 - year)),
          avg.rating = mean(rating)) %>% 
  distinct() %>% 
  arrange(desc(per.year.ratings)) %>% 
  ggplot(aes(x = factor(per.year.ratings),
             y = avg.rating)) +
  geom_boxplot()

movielens <- movielens %>% 
  mutate(date = as_datetime(timestamp))

movielens %>% 
  mutate(week = round_date(date, "week")) %>% 
  group_by(week) %>% 
  summarize(avg.rating = mean(rating)) %>% 
  ggplot(aes(x = week, y = avg.rating)) +
  geom_line()

movielens %>% 
  group_by(genres) %>% 
  summarize(n = n(), avg = mean(rating), sd = sd(rating)) %>% 
  filter(n > 1E3) %>% 
  ggplot(aes(x = avg, y = genres)) +
  geom_point() +
  geom_errorbar(aes(xmin = avg - sd, xmax = avg + sd))

# Regularization
library(tidyverse)
options(digits = 7)

# simulate 1000 schools of various sizes
set.seed(1986)
n <- round(2^rnorm(1000, 8, 1))

# generate true quality for each school
set.seed(1)
mu <- round(80 + 2*rt(1000, 5))
range(mu)
schools <- data.frame(id = paste("PS", 1:1000),
                      size = n,
                      quality = mu,
                      rank = rank(-mu))
schools %>% 
  top_n(10, quality) %>% 
  arrange(desc(quality))
  
# generate normally distributed test scores based on school quality

scores <- sapply(1:nrow(schools), function(i){
  scores <- rnorm(schools$size[i], schools$quality[i], 30)
  scores
})
schools <- schools %>% 
  mutate(score = sapply(scores, mean))

schools %>% 
  top_n(10, score) %>% 
  arrange(desc(score))

schools %>% 
  summarize(median(size))

schools %>% 
  top_n(10, score) %>% 
  summarise(median(size))

schools %>% 
  slice_min(n = 10, score) %>% 
  arrange(score) %>% 
  summarise(median(size))

schools %>% 
  arrange(desc(quality))

library(gghighlight)
schools %>% 
  ggplot(aes(x = size, y = score)) +
  geom_point(alpha = 0.5) +
  gghighlight(quality >= 86)

# calculate average score across schools
overall <- mean(sapply(scores, mean))
overall

schools$reg.score <- sapply(scores, function(class){
  overall + sum(class - overall) / (length(class) + 25)
})

schools %>% 
  arrange(desc(reg.score))

# Using values of from 10 to 250, find the that minimizes the RMSE.
alpha <- seq(10:250)

RMSE(schools$quality, schools$reg.score)
RMSE(schools$quality, schools$score)

results <- list(length = length(alpha))

# calculate predictions for each alpha
for (i in 1:length(alpha)) {
  results[[i]] <- sapply(scores, function(class){
    overall + sum(class - overall) / (length(class) + alpha[i])
  })
}

# calculate RMSE for each prediction set
errors <- sapply(results, function(scores){
  RMSE(schools$quality, scores)
})

# find optimal alpha
alpha[which.min(errors)]

# Rank the schools based on the average obtained with the best alpha from Q6
schools$optimal <- results[[135]]
schools %>% 
  arrange(desc(optimal))

# Matrix Decomposition
# construct a dataset that represents grade scores for 100 students in 24 
# different subjects. The overall average has been removed so this data 
# represents the percentage point each student received above or below the 
# average test score. So a 0 represents an average grade (C), a 25 is a high 
# grade (A+), and a -25 represents a low grade (F)
set.seed(1987)
n <- 100
k <- 8
Sigma <- 64  * matrix(c(1, .75, .5, .75, 1, .5, .5, .5, 1), 3, 3) 
m <- MASS::mvrnorm(n, rep(0, 3), Sigma)
m <- m[order(rowMeans(m), decreasing = TRUE),]
y <- m %x% matrix(rep(1, k), nrow = 1) + matrix(rnorm(matrix(n*k*3)), n, k*3)
colnames(y) <- c(paste(rep("Math",k), 1:k, sep="_"),
                 paste(rep("Science",k), 1:k, sep="_"),
                 paste(rep("Arts",k), 1:k, sep="_"))

my_image <- function(x, zlim = range(x), ...){
  colors = rev(RColorBrewer::brewer.pal(9, "RdBu"))
  cols <- 1:ncol(x)
  rows <- 1:nrow(x)
  image(cols, rows, t(x[rev(rows),,drop=FALSE]), xaxt = "n", yaxt = "n",
        xlab="", ylab="",  col = colors, zlim = zlim, ...)
  abline(h=rows + 0.5, v = cols + 0.5)
  axis(side = 1, cols, colnames(x), las = 2)
}

my_image(y)

# You can examine the correlation between the test scores directly like this:
my_image(cor(y), zlim = c(-1,1))
range(cor(y))
axis(side = 2, 1:ncol(y), rev(colnames(y)), las = 2)

# Use the function svd() to compute the SVD of y. This function will return 
# U, V, and the diagonal entries of D.
s <- svd(y)
names(s)

# check that SVD is working
y_svd <- s$u %*% diag(s$d) %*% t(s$v)
max(abs(y - y_svd))

# Compute the sum of squares of the columns of Y and store them in ss_y. 
# Then compute the sum of squares of columns of the transformed YV and store 
# them in ss_yv
ss_y <- sum(y^2)
ss_yv <- sum(y_svd^2)

ggplot(data = ss_y, aes(x = ss_y, y = seq(1:ncol(ss_y)))) +
  geom_point()
