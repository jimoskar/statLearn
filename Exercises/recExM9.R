#===========#
# Problem 1 #
#===========#

## 9.6 from ISLR
library(e1071)

## Generating data
set.seed(1)
x=matrix(rnorm(20*2), ncol=2)
y=c(rep(-1,10), rep(1,10))
x[y==1,]=x[y==1,] + 1
plot(x, col=(3-y))

dat=data.frame(x=x, y=as.factor(y))
svmfit=svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
plot(svmfit, dat)
summary(svmfit)

## Cross-validation
set.seed(1)
tune.out <- tune(svm,y~.,data=dat,kernel="linear", ranges=list(cost=c(0.001, 0.01, 0.1, 1,5,10,100)))
summary(tune.out)
bestmod <- tune.out$best.model
summary(bestmod)

## Prediction
xtest=matrix(rnorm(20*2), ncol=2)
ytest=sample(c(-1,1), 20, rep=TRUE)
xtest[ytest==1,]=xtest[ytest==1,] + 1
testdat=data.frame(x=xtest, y=as.factor(ytest))

ypred=predict(bestmod ,testdat)
table(predict=ypred, truth=testdat$y)

## Support Vector Machine

# Generate data
set.seed (1)
x=matrix(rnorm(200*2), ncol=2)
x[1:100,]=x[1:100,]+2
x[101:150,]=x[101:150,]-2
y=c(rep(1,150),rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
plot(x, col = y)

## SVM with radial kernel

train <- sample(200,100)
svmfit <- svm(y~., data=dat[train,], kernel="radial", gamma=1, cost =1)
plot(svmfit , dat[train ,])
summary(svmfit)

set.seed (1)
tune.out=tune(svm, y~., data=dat[train,], kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000), gamma=c(0.5,1,2,3,4)))
summary(tune.out)

table(true=dat[-train,"y"], pred=predict(tune.out$best.model,
                                         newdata=dat[-train ,]))
## ROC Curves
library(ROCR)

rocplot=function(pred, truth, ...){
  predob = prediction(pred, truth)
  perf = performance(predob , "tpr", "fpr")
  plot(perf ,...)}

svmfit.opt=svm(y~., data=dat[train,], kernel="radial",
               gamma=2, cost=1,decision.values=T)
fitted=attributes(predict(svmfit.opt,dat[train,],decision.values=TRUE))$decision.values

par(mfrow=c(1,2))
rocplot(fitted ,dat[train ,"y"],main="Training Data")

svmfit.flex=svm(y~., data=dat[train,], kernel="radial", gamma=50, cost=1, decision.values=T)
fitted=attributes(predict(svmfit.flex,dat[train,],decision.values=T))$decision.values
rocplot(fitted ,dat[train ,"y"],add=T,col="red")

# On test data:
fitted=attributes(predict(svmfit.opt,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],main="Test Data")
fitted=attributes(predict(svmfit.flex,dat[-train,],decision.values=T))$decision.values
rocplot(fitted,dat[-train,"y"],add=T,col="red")


# SVM with multiple classes

set.seed (1)
x=rbind(x, matrix(rnorm(50*2), ncol=2))
y=c(y, rep(0,50))
x[y==0,2]=x[y==0,2]+2
dat=data.frame(x=x, y=as.factor(y))
par(mfrow=c(1,1))
plot(x,col=(y+1))

svmfit=svm(y~., data=dat, kernel="radial", cost=10, gamma=1)
plot(svmfit , dat)

# Application to Gene Expression Data

library(ISLR)
dat=data.frame(x=Khan$xtrain , y=as.factor(Khan$ytrain ))
out=svm(y~., data=dat, kernel="linear",cost=10)
summary(out)

# On test data:
dat.te=data.frame(x=Khan$xtest , y=as.factor(Khan$ytest ))
pred.te=predict(out, newdata=dat.te)
table(pred.te, dat.te$y)

#===========#
# Problem 3 #
#===========#

# code taken from video by Trevor Hastie
set.seed(10111)
x <- matrix(rnorm(40), 20, 2)
y <- rep(c(-1, 1), c(10, 10))
x[y == 1, ] <- x[y == 1, ] + 1
plot(x, col = y + 3, pch = 19, xlab = expression(X[1]), ylab = expression(X[2]))
dat = data.frame(x, y = as.factor(y))

# a)
svmfit <- svm(y~., data = dat, kernel  = "linear", cost = 10, scale = F)

make.grid = function(x, n = 75) {
  # takes as input the data matrix x and number of grid points n in each direction
  # the default value will generate a 75x75 grid
  grange = apply(x, 2, range) # range for x1 and x2
  x1 = seq(from = grange[1, 1], to = grange[2, 1], length.out = n) 
  x2 = seq(from = grange[1, 2], to = grange[2, 2], length.out = n)  
  expand.grid(X1 = x1, X2 = x2) #create a uniform grid according to x1 and x2 values
}

x = as.matrix(dat[, c("X1", "X2")])
xgrid = make.grid(x)
ygrid = predict(svmfit, xgrid)
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2)

# b)
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)

# c)
beta = drop(t(svmfit$coefs) %*% x[svmfit$index, ])
beta0 = svmfit$rho
plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = 0.2) 
points(x, col = y + 3, pch = 19)
points(x[svmfit$index, ], pch = 5, cex = 2)
abline(beta0/beta[2], -beta[1]/beta[2], lwd = 2) #class boundary 
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2) #class boundary-margin 
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2) #class boundary+margin


#===========#
# Problem 4 #
#===========#

load(url("https://web.stanford.edu/~hastie/ElemStatLearn/datasets/ESL.mixture.rda")) # names(ESL.mixture)
rm(x, y)
attach(ESL.mixture)
plot(x, col = y + 1, pch = 19, xlab = expression(X[1]), ylab = expression(X[2]))
dat = data.frame(y = factor(y), x)
r.cv <- tune(svm, factor(y) ~ ., data = dat, kernel = "radial", 
             ranges = list(cost = c(0.01, 0.1, 1, 5, 10, 100, 1000), gamma = c(0.01, 0.1, 1, 10, 100)))
summary(r.cv)
fit <- r.cv$best.model

xgrid = make.grid(x)
ygrid = predict(fit, xgrid)
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2) 
points(x, col = y + 1, pch = 19)

# decision boundary
func = predict(fit, xgrid, decision.values = TRUE)
func = attributes(func)$decision
contour(unique(xgrid[, 1]), unique(xgrid[, 2]), matrix(func, 75, 75), level = 0,
        add = TRUE) #svm boundary

#===========#
# Problem 5 #
#===========#

# a)
library(ISLR)
train <- sample(1:nrow(OJ), 800, replace = F)
test <- (1:nrow(OJ))[-train]
oj.train <- OJ[train, ]
oj.test <- OJ[test, ]

# b)
svm.oj <- svm(Purchase~., data = oj.train, kernel = "linear", cost = 0.01)
summary(svm.oj)

# c)
pred.train <- predict(svm.oj, oj.train)
(table.train <- table(oj.train$Purchase, pred.train))
(msrate.train <- 1 - sum(diag(table.train))/sum(table.train))

pred.test <- predict(svm.oj, oj.test)
(table.test <- table(oj.test$Purchase, pred.test))
(msrate.test <- 1 - sum(diag(table.test))/sum(table.test))

# d)
set.seed(1)
cost.vals <- 10^seq(-2, 1, by = 0.25)
tune.cost <- tune(svm, Purchase ~., data = oj.train, kernel = "linear", ranges = list(cost = cost.vals))
summary(tune.cost)

# e)
svm.linear <- svm(Purchase~., data = oj.train, kernel = "linear", cost = tune.cost$best.parameters$cost)
pred.train <- predict(svm.linear, oj.train)
(table.train <- table(oj.train$Purchase, pred.train))
(msrate.train <- 1 - sum(diag(table.train))/sum(table.train))

pred.test <- predict(svm.linear, oj.test)
(table.test <- table(oj.test$Purchase, pred.test))
(msrate.test <- 1 - sum(diag(table.test))/sum(table.test))

# f)
svm.radial <- svm(Purchase~., data = oj.train, kernel = "radial")
pred.train <- predict(svm.radial, oj.train)
(table.train <- table(oj.train$Purchase, pred.train))
(msrate.train <- 1 - sum(diag(table.train))/sum(table.train))

pred.test <- predict(svm.radial, oj.test)
(table.test <- table(oj.test$Purchase, pred.test))
(msrate.test <- 1 - sum(diag(table.test))/sum(table.test))

# g)
svm.poly <- svm(Purchase~., data = oj.train, kernel = "poly", degree = 2)
pred.train <- predict(svm.poly, oj.train)
(table.train <- table(oj.train$Purchase, pred.train))
(msrate.train <- 1 - sum(diag(table.train))/sum(table.train))

pred.test <- predict(svm.poly, oj.test)
(table.test <- table(oj.test$Purchase, pred.test))
(msrate.test <- 1 - sum(diag(table.test))/sum(table.test))

