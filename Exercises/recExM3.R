### Module 3: Recommended Exercises

#===========#
# Problem 1 #
#===========#

library(tidyverse)
library(ISLR)

Auto = subset(Auto, select = -name)
summary(Auto)
# Need to convert origin to factor, since this is a qualitative predictor
Auto$origin = factor(Auto$origin)

# a)
library(GGally)
ggpairs(Auto, lower = list(continuous = wrap("points", size = 0.1))) + # change point size
  theme(text = element_text(size = 7)) # change text size


# b) Compute the correlation matrix:
reduced.auto = subset(Auto, select = -origin)
cor(reduced.auto)

# c
fit <- lm(data = Auto, mpg ~ .)
summary(fit)

## i) Yes, we can see that there are multiple relationships present, and the low p-value for the F-
##    statistic indicate that the model has significant explanatory power.
## ii) From the coefficients, a 1000 kg increase in weight would result in a decrease of 6.71
##     in mpg. The low p-value speaks for the fact that this relationship is valid/exists.
## iii) year suggests that every 10th year, mpg increases with 7.770

# d)
# We have to do conduct the test origin2 = origin3 = 0. This requires an F.test, which
# is readily available with

anova(fit)
# The small p-value is strong evidence that origin has an influence on the response mpg.

# e)
library(ggfortify)
autoplot(fit, smooth.colour = NA)
# There is some evidence of non-linearity in the Residuals vs Fitted plot

# f)
set.seed(2332)
n = 100

par(mfrow = c(2, 3))
for (i in 1:6) {
  sim = rnorm(n)
  qqnorm(sim, pch = 1, frame = FALSE)
  qqline(sim, col = "blue", lwd = 1)
}

# g)

fit2 <- lm(mpg ~ displacement + weight + year + origin + year:origin, Auto)
summary(fit2)
anova(fit2)
# The p-value of the F-test provides evidence that the interaction effect is relevant for the response.
# The interpretation is that the slope of year is somewhat larger for the categories origin 2/3.

# h)

fit3 <- lm(log(mpg) ~ ., Auto)
summary(fit3)
autoplot(fit3)
fit4 <- lm(sqrt(mpg) ~., Auto)
autoplot(fit4)

#===========#
# Problem 2 #
#===========#

# b)
# The interpretation of a 95% confidence interval is that if we repeat the same experiment
# a very large amount of times, we expect the relevant parameter to lie in the confidence
# interval for 95% of the experiments. Example:

beta0 = 1
beta1 = 3
true_beta = c(beta0, beta1)  # vector of model coefficients
true_sd = 1  # choosing true sd
X = runif(100, 0, 1)  # simulate the predictor variable X
Xmat = model.matrix(~X, data = data.frame(X))  # create design matrix


ci_int = ci_x = 0  # Counts how many times the true value is within the confidence interval
nsim = 1000
for (i in 1:nsim) {
  y = rnorm(n = 100, mean = Xmat %*% true_beta, sd = rep(true_sd, 100))
  mod = lm(y ~ x, data = data.frame(y = y, x = X))
  ci = confint(mod)
  ci_int[i] = ifelse(beta0 >= ci[1,1] && beta0 <= ci[1,2], 1, 0)  # if true value of beta0 is within the CI then 1 else 0
  ci_x[i] = ifelse(beta1 >= ci[2,1] && beta1 <= ci[2,2], 1, 0)  # if true value of beta_1 is within the CI then 1 else 0
}

c(mean(ci_int), mean(ci_x))

# c)
# The interpretation of a e.g. 95 % prediction interval is that if you repeat an experiment on Y
# for a given x0 a large amount of times, we expect the resulting Y-value to lie in the pred.int.
# 95 % of the time. Example:

beta0 = 1
beta1 = 3
true_beta = c(beta0, beta1)  # vector of model coefficients
true_sd = 1  # choosing true sd
X = runif(100, 0, 1)  # simulate the predictor variable X
Xmat = model.matrix(~X, data = data.frame(X))  # create design matrix

x0 = c(1, 0.4)
pi_y0 = 0 # Counts how many times the true value is within the PI
nsim = 1000
for (i in 1:nsim) {
  y = rnorm(n = 100, mean = Xmat %*% true_beta, sd = rep(true_sd, 100))
  mod = lm(y ~ x, data = data.frame(y = y, x = X))
  y0 = rnorm(n = 1, mean = x0 %*% true_beta, sd = true_sd)
  pi = predict(mod, newdata = data.frame(x = x0[2]), interval = "predict")[2:3]
  pi_y0[i] = ifelse(y0 >= pi[1] && y0 <= pi[2], 1, 0)
}

mean(pi_y0)

# d)

# e)
# The error is the theoretical, irreducible error, while a residual is the distance from the
# estimated regression plane to a data point.



#===================#
# Lab (3.6) in ISLR #
#===================#

library(ISLR)
library(MASS)


lm.fit <- lm(medv ~ lstat, Boston)
summary(lm.fit)
names(lm.fit)

# Plot the fit and data together
attach(Boston)
plot(lstat,medv)
abline(lm.fit)

# Diagnostic plots:
par(mfrow = (c(2, 2))) # View all plots at same time
plot(lm.fit)

# Leverage plot:
plot(hatvalues(lm.fit))
which.max(hatvalues(lm.fit)) # Find index of largest element

# Multiple linear regression
lm.fit <- lm(medv ~ ., data = Boston)
library(car)
vif(lm.fit)

# Use all except age:
lm.fit1 <- update(lm.fit, ~.-age)

# Non-linear transformation:
lm.fit2 <- lm(medv ~ lstat + I(lstat^2))
plot(lm.fit2)

# For higher degrees:
lm.fit5 <- lm(medv ~ poly(lstat, 5))

# Qualitative predictors
rm(list = ls())
library(ISLR)
lm.fit=lm(Sales~.+Income:Advertising+Price:Age, data=Carseats)
contrasts(Carseats$ShelveLoc)


