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
# We have to do conduxt the test origin2 = origin3 = 0. This requires an F.test, which
# is readily available with

anova(fit)
# The small p-value is trong evidence that origin has an influence on the response mpg.

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
# The interpretation is that the slope of year is somewhat larger for the categories origin2/3.

# h)

fit3 <- lm(mpg ~ log(displacement), Auto)
summary(fit3)

#DO THE LAB OF CHAPTER 3!!

