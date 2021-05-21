## Problem 2

# a)
library(ISLR)
data("Carseats")
set.seed(4268)
n = nrow(Carseats)
train = sample(1:n, 0.7 * nrow(Carseats), replace = F)
test = (1:n)[-train]
Carseats.train = Carseats[train, ]
Carseats.test = Carseats[test, ]

# b)
library(tree)
tree.mod = tree(Sales ~ ., data = Carseats.train)
summary(tree.mod)

plot(tree.mod)
text(tree.mod, pretty = 0)

yhat = predict(tree.mod, newdata = Carseats.test)
mse = mean((yhat - Carseats.test$Sales)^2)
mse

# c)
set.seed(4268)
cv.Carseats = cv.tree(tree.mod)
cv.Carseats
tree.min = which.min(cv.Carseats$dev)
best = cv.Carseats$size[tree.min]
plot(cv.Carseats$size, cv.Carseats$dev, type = "b")
points(cv.Carseats$size[tree.min], cv.Carseats$dev[tree.min], col = "red", pch = 20)

pr.tree = prune.tree(tree.mod, best = 11)
plot(pr.tree)
text(pr.tree, pretty = 0)

yhat = predict(pr.tree, newdata = Carseats.test)
mse = mean((yhat - Carseats.test$Sales)^2)
mse

# d)
library(randomForest)
dim(Carseats)
set.seed(4268)
bag.Carseats = randomForest(Sales ~ ., Carseats.train, mtry = ncol(Carseats) - 1, ntree = 500, importance = TRUE)

yhat = predict(bag.Carseats, newdata = Carseats.test)
mse = mean((yhat - Carseats.test$Sales)^2)
mse

importance(bag.Carseats)
varImpPlot(bag.Carseats)

# e)
rf.Carseats = randomForest(Sales ~ ., Carseats.train, mtry = 3, ntree = 500, importance = TRUE)

yhat = predict(rf.Carseats, newdata = Carseats.test)
mse = mean((yhat - Carseats.test$Sales)^2)
mse

importance(rf.Carseats)
varImpPlot(rf.Carseats)

# f)
library(gbm)
r.boost = gbm(Sales ~ ., Carseats.train, distribution = "gaussian", n.trees = 500, interaction.depth = 4,
              shrinkage = 0.1)

yhat = predict(r.boost, newdata = Carseats.test)
mse = mean((yhat - Carseats.test$Sales)^2)
mse

# g)
train.predictors = Carseats.train[, -1]
test.predictors = Carseats.test[, -1]
Y.train = Carseats.train[, 1]
Y.test = Carseats.test[, 1]

bag.Car = randomForest(train.predictors, y = Y.train, xtest = test.predictors, ytest = Y.test,
                       mtry = 10, ntree = 500)
rf.Car = randomForest(train.predictors, y = Y.train, xtest = test.predictors, ytest = Y.test,
                      mtry = 3, ntree = 500)
plot(1:500, bag.Car$test$mse, col = "blue", type = "l", xlab = "Number of Trees",
     ylab = "Test MSE", ylim = c(2, 2.8))
lines(1:500, rf.Car$test$mse, col = "green")
legend("topright", c("m = p", "m = p/3"), col = c("blue", "green"), cex = 1, lty = 1)

## Problem 3

# a)
library(kernlab)
data(spam)
?spam
head(spam)

# b)
n = nrow(spam)
train = sample(1:n, 0.70*n)
test = (1:n)[-train]
spam.train = spam[train, ]
spam.test = spam[test, ]

# c)
library(tree)
tree.mod = tree(type ~ ., data = spam)
summary(tree.mod)

plot(tree.mod)
text(tree.mod, pretty = 0)

# d)
yhat = predict(tree.mod, newdata = spam.test, type = "class")
response.test = spam.test$type

# Misclassification error
misclass = table(yhat, response.test)
misclass
1 - sum(diag(misclass))/sum(misclass)

# e)
cv.spam = cv.tree(tree.mod, FUN = prune.misclass)
plot(cv.spam$size, cv.spam$dev, type = "b")

# Choose size = 6
prune.spam = prune.misclass(tree.mod, best = 6)
plot(prune.spam)
text(prune.spam, pretty = 0)



yhat = predict(prune.spam, newdata = spam.test, type = "class")
response.test = spam.test$type

# Misclassification rate
misclass = table(yhat, response.test)
misclass
1 - sum(diag(misclass))/sum(misclass)

# f)
bag.spam = randomForest(type ~ ., spam.train, mtry = ncol(spam.train) - 1, ntree = 500)

yhat = predict(bag.spam, newdata = spam.test, type = "class")
response.test = spam.test$type

# Misclassification rate
misclass = table(yhat, response.test)
misclass
1 - sum(diag(misclass))/sum(misclass)

# g)
rf.spam = randomForest(type ~ ., spam.train, mtry = 8, ntree = 500, importance = T)

importance(rf.spam)
varImpPlot(rf.spam)

yhat = predict(rf.spam, newdata = spam.test, type = "class")
response.test = spam.test$type

# Misclassification rate
misclass = table(yhat, response.test)
misclass
1 - sum(diag(misclass))/sum(misclass)

# h)
library(gbm)
boost.spam = gbm(type ~ ., spam.train, distribution = "gaussian", n.trees = 5000, interaction.depth = 3,
              shrinkage = 0.001)

yhat = predict(rf.spam, newdata = spam.test, type = "class")
response.test = spam.test$type

# Misclassification rate
misclass = table(yhat, response.test)
misclass
1 - sum(diag(misclass))/sum(misclass)

#================#
# Lab: Chapter 8 #
#================#
library(tree)
library(ISLR)
df <- Carseats

# Classification trees:
High <- ifelse(df$Sales <=8,"No","Yes")
df <- data.frame(df ,High)

factor(Carseats$High)
tree.carseats <- tree(High~., data = df[, !(names(df) %in% c("Sales"))]) #MYSTERIOUS ERROR?! :(
summary(tree.carseats)

# Regression trees
library(MASS)
set.seed(1)
train <- sample(1:nrow(Boston), nrow(Boston)/2)
tree.boston <- tree(medv~., Boston[train, ])
summary(tree.boston)
plot(tree.boston)
text(tree.boston, pretty = 0, cex = 0.5)

# CV:
cv.boston=cv.tree(tree.boston)
plot(cv.boston$size ,cv.boston$dev ,type='b')

# Pruning:
prune.boston <- prune.tree(tree.boston ,best=5)
plot(prune.boston)
text(prune.boston ,pretty=0, cex = 0.7)

# Prediction:
yhat <- predict(tree.boston, newdata = Boston[-train, ])
y <- Boston[-train, "medv"]
plot(yhat,y)
abline (0 ,1)
mse <- mean((y-yhat)^2)
mse

# Random forests ( and bagging)
library(randomForest)
set.seed(1)
bag.boston=randomForest(medv~.,data=Boston,subset=train, mtry=13,importance =TRUE)
bag.boston

yhat.bag = predict(bag.boston ,newdata=Boston[-train ,])
plot(yhat.bag, y)
abline (0 ,1)
mean((yhat.bag-y)^2)
varImpPlot(bag.boston)

# Boosting:
library(gbm)
boost.boston <- gbm(medv~., data = Boston[train, ], distribution = "gaussian", 
                    n.trees = 5000, interaction.depth = 4, shrinkage = 0.2)
par(mfrow=c(1,2)) 
plot(boost.boston ,i="rm") 
plot(boost.boston ,i="lstat")

yhat.boost <- predict(boost.boston, newdata = Boston[-train, ], n.trees = 5000)
mean((yhat.boost - y)^2)
