### Module 4: Recommended Exercises

#===========#
# Problem 2 #
#===========#

## c)
nG = nF = 500
muG = c(214.97, 141.52)
muF = c(214.82, 139.45)

covG = matrix(c(0.1502, 0.0055, 0.0055, 0.1998), ncol = 2, byrow = T)
covF = matrix(c(0.1240, 0.0116, 0.0116, 0.3112), ncol = 2, byrow = T)

pool = ( (nG - 1) * covG + (nF - 1) * covF ) / (nG + nF - 2)


discriminant <- function(x, sigma, mu, pi) {
  inv = solve(sigma)
  return(t(x) %*% inv %*% mu - 0.5 * t(mu) %*% inv %*% mu + log(pi))
}

x <-  c(214.0, 140.4)

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
# Problem 3 #
#===========#

## a)
odds <- 0.37 # = p/(1 - p)
p <-  0.37/(1+0.37)
p

## b)
p <- 0.16
odds <- p/(1 - p)
odds


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
lda.Weekly_prob <-  predict(lda.Weekly, newdata = Weekly_test)$posterior
lda.conf <- table(lda.Weekly_pred, Weekly_test$Direction)
lda.conf
(lda.conf[1,1] + lda.conf[2,2]) / (sum(lda.conf[1,]) + sum(lda.conf[2,]))

## f)

qda.Weekly <-  qda(Direction ~ Lag2, data = Weekly_train)
summary(qda.Weekly)
qda.Weekly_pred <- predict(qda.Weekly, newdata = Weekly_test)$class
qda.Weekly_prob <-  predict(qda.Weekly, newdata = Weekly_test)$posterior
qda.conf <- table(qda.Weekly_pred, Weekly_test$Direction)
(qda.conf[1,1] + qda.conf[2,2]) / (sum(qda.conf[1,]) + sum(qda.conf[2,]))

## g)
library(class)
knn.train = as.matrix(Weekly_train$Lag2)
knn.test = as.matrix(Weekly_test$Lag2)

set.seed(1233)
yourKNNmodel <-  knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction,
                   k = 4, prob = T)
knn.conf <- table(yourKNNmodel, Weekly_test$Direction)
(knn.conf[1,1] + knn.conf[2,2]) / (sum(knn.conf[1,]) + sum(knn.conf[2,]))

## h)
# knn error:
K = 30
knn.error = rep(NA, K)

set.seed(321)
for (k in 1:K) {
  knn.pred = knn(train = knn.train, test = knn.test, cl = Weekly_train$Direction,
                 k = k)
  knn.error[k] = mean(knn.pred != Weekly_test$Direction)
}
which.min(knn.error)
knn.error.df = data.frame(k = 1:K, error = knn.error)
ggplot(knn.error.df, aes(x = k, y = error)) + geom_point(col = "blue") + geom_line(linetype = "dotted")
# k = 12 yields the lowest error

## i)
# lda seems to be best, or logistic regression

## j)
# get the probabilities for the classified class
knn.Weekly_prob = attributes(yourKNNmodel)$prob

# since we want the probability for Up, we need to take 1-p for the elements
# that gives probability for Down
down = which(yourKNNmodel == "Down")
knn.Weekly_prob[down] = 1 - knn.Weekly_prob[down]

library(pROC)
library(plotROC)


glmroc = roc(response = Weekly_test$Direction, predictor = glm.probs_Weekly,
             direction = "<")
ldaroc = roc(response = Weekly_test$Direction, predictor = lda.Weekly_prob[,
                                                                           2], direction = "<")
qdaroc = roc(response = Weekly_test$Direction, predictor = qda.Weekly_prob[,
                                                                           2], direction = "<")
knnroc = roc(response = Weekly_test$Direction, predictor = knn.Weekly_prob,
             direction = "<")
auc(glmroc)
# you can use this function for all your methods and plot them using
# plot(yourRoc)

# or use ggplot2
dat = data.frame(Direction = Weekly_test$Direction, glm = glm.probs_Weekly, lda = lda.Weekly_prob[,
                                                                                           2], qda = qda.Weekly_prob[, 2], knn = knn.Weekly_prob)
dat_long = melt_roc(dat, "Direction", c("glm", "lda", "qda", "knn"))
ggplot(dat_long, aes(d = D, m = M, color = name)) + geom_roc(n.cuts = F) + xlab("1-Specificity") +
  ylab("Sensitivity")
# glm is very similar to lda, so the roc-curve for glm is not shown.


# AUC: yourAUC = auc(yourRoc)


#================#
# Lab: Chapter 4 #
#================#

library(ISLR)
names(Smarket)
attach(Smarket)
plot(Volume)

# Logistic reg.
logfit <- glm(Direction~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket, family = binomial)
summary(logfit)  
contrasts(Direction)  
# With test set
train <- (Year < 2005)
Smarket.train <- Smarket[train, ]
Smarket.test <- Smarket[!train, ]
logfit <- glm(Direction~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume, data = Smarket.train, family = binomial)
logfit.prob <- predict(logfit, Smarket.test, type = "response")
logfit.pred <- rep("Down", length(logfit.prob))
logfit.pred[logfit.prob > 0.5] <- "Up"
table(logfit.pred, Smarket.test$Direction)
mean(logfit.pred == Smarket.test$Direction)

# LDA:
library(MASS)
lda.fit <- lda(Direction~ Lag1 + Lag2, data = Smarket.train)
lda.pred <- predict(lda.fit, Smarket.test)
lda.class <- lda.pred$class
table(lda.class, Smarket.test$Direction)
mean(lda.class == Smarket.test$Direction)

# QDA:
qda.fit <- qda(Direction~ Lag1 + Lag2, data = Smarket.train)
qda.pred <- predict(qda.fit, Smarket.test)
qda.class <- qda.pred$class
table(qda.class, Smarket.test$Direction)
mean(qda.class == Smarket.test$Direction)

# KNN:
library(class)
train.X <- cbind(Lag1, Lag2)[train, ]
train.y <- Direction[train]
test.X <- cbind(Lag1, Lag2)[!train, ]
set.seed(1)
knn.pred <- knn(train.X, test.X, train.y, k = 3)
table(knn.pred, Direction[!train])
mean(knn.pred == Direction[!train])
