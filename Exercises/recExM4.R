### Module 4: Recommended Exercises

#===========#
# Problem 1 #
#===========#

## c)
nG = nF = 500
muG = c(214.97, 141.52)
muF = c(214.82, 139.45)

covG = matrix(c(0.1502, 0.0055, 0.0055, 0.1998), ncol = 2, byrow = T)
covF = matrix(c(0.1240, 0.0116, 0.0116, 0.3112), ncol = 2, byrow = T)

pool = ( (nG - 1) * covG + (nF - 1) * covF ) / (nG + nF - 2)
pool

discriminant <- function(x, sigma, mu, pi) {
  inv = solve(sigma)
  return(t(x) %*% inv %*% mu - 0.5 * t(mu) %*% inv %*% mu + log(pi))
}

x = c(214.0, 140.4)

dG <- discriminant(x, pool, muG, 0.5)
dG
dF <- discriminant(x, pool, muF, 0.5)
dF
# Since dF > dG, we classify the bank note as a fake.

## d)
dQ <- function(x, sigma, mu, pi) {
  inv = solve(sigma)
  return(-0.5 * t(x - mu) %*% inv %*% (x - mu) - 0.5 * log( det(sigma) ) + log(pi))
}

dG <- dQ(x, covG, muG, 0.5)
dG
dF <- dQ(x, covF, muF, 0.5)
dF
# Since dF > dG, we again classify the bank note as a fake.

#===========#
# Problem 6 #
#===========#

library(MASS)
library(stats)
library(class)
library(pROC)
library(ISLR)
library(tidyverse)
library(GGally)


## a)
data("Weekly")
?Weekly
head(Weekly)
glimpse(Weekly)
summary(Weekly)

ggpairs(Weekly, lower = list(continuous = wrap("points", size = 0.1))) + # change point size
  theme(text = element_text(size = 7)) # change text size
# Exponential relationship between years and volume?

## b)

logfit <- glm(Direction ~ . -Year -Today, family = "binomial", data = Weekly)
summary(logfit)
# Lag2 is the only predictor with relatively low p-value.

## c)
glm.probs_Weekly = predict(logfit, type = "response")
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
table(glm.preds_Weekly, Weekly$Direction)

## d)
Weekly_trainID = (Weekly$Year < 2009)
Weekly_train = Weekly[Weekly_trainID, ]
Weekly_test = Weekly[!Weekly_trainID, ]

lfit <- glm(Direction ~ Lag2, family = "binomial", Weekly_train)

glm.probs_Weekly <- predict(lfit, newdata = Weekly_test, type = "response")
glm.preds_Weekly = ifelse(glm.probs_Weekly > 0.5, "Up", "Down")
conf <- table(glm.preds_Weekly, Weekly_test$Direction)
conf

# Correctly classified:
(conf[1,1] + conf[2,2]) / (sum(conf[2,]) + sum(conf[1,]))

## e)

?lda()
lda.Weekly <-  lda(Direction ~ Lag2, data = Weekly_train)
summary(lda.Weekly)
lda.Weekly_pred <- predict(lda.Weekly, newdata = Weekly_test)$class
lda.Weekly_prob = predict(lda.Weekly, newdata = Weekly_test)$posterior
table(lda.Weekly_pred, Weekly_test$Direction)


