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


# b) Compute the covariance matrix:
reducedAuto = subset(Auto, select = -origin)
cor(reducedAuto)

# c
fit <- lm(data = Auto, mpg ~ .)
summary(fit)

## i) Yes, we can see that there are multiple relationships present.
## ii) From the coefficients, a 1000 kg increase in weight would result in a decrease of 6.71
##     in mpg. The low p-value speaks for the fact that this relationship is valid/exists.
## iii) year suggests that every 10th year, mpg increases with 7.770

# d)

