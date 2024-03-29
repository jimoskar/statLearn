#===========#
# Problem 2 #
#===========#
library(ISLR)
library(tidyverse)
library(GGally)

data(Credit)

names(Credit)
head(Credit)
pairwise_scatter_data <- Credit[,c("Balance", "Age", "Cards", "Education", "Income", "Limit", "Rating")]

# Simplest possible pairwise scatter plot
pairs(pairwise_scatter_data)

# More interesting but slower pairwise plot from package GGally
library(GGally)
ggpairs(data=pairwise_scatter_data)


#===========#
# Problem 3 #
#===========#

# Exclude 'ID' column
credit_data <- subset(Credit, select=-c(ID))
# Counting the dummy variables as well
credit_data_number_predictors <- 11

# Create train and test set indexes
set.seed(1)
train_perc <- 0.75
credit_data_train_index <- sample(1:nrow(credit_data), nrow(credit_data)*train_perc)
credit_data_test_index <- (-credit_data_train_index)
# Create train and test set
credit_data_training <- credit_data[credit_data_train_index, ]
credit_data_testing <- credit_data[credit_data_test_index, ]

library(leaps)
# Perform best subset selection using all the predictors and the training data
best_subset_method=regsubsets(Balance~.,credit_data_training,nvmax=credit_data_number_predictors)

# Save summary obj
best_subset_method_summary=summary(best_subset_method)

# Plot RSS, Adjusted Rˆ2, C_p and BIC
par(mfrow=c(2,2))
plot(best_subset_method_summary$rss,xlab="Number of Variables",ylab="RSS",type="l")
plot(best_subset_method_summary$adjr2,xlab="Number of Variables",ylab="Adjusted RSq",type= "l")
bsm_best_adjr2 = which.max(best_subset_method_summary$adjr2)

points(bsm_best_adjr2,best_subset_method_summary$adjr2[bsm_best_adjr2], col="red",cex=2,pch = 20)
plot(best_subset_method_summary$cp,xlab="Number of Variables",ylab="Cp",type='l')
bsm_best_cp=which.min(best_subset_method_summary$cp)

points(bsm_best_cp,best_subset_method_summary$cp[bsm_best_cp],col="red",cex=2,pch=20)
bsm_best_bic=which.min(best_subset_method_summary$bic)

plot(best_subset_method_summary$bic,xlab="Number of Variables",ylab="BIC",type='l')
points(bsm_best_bic,best_subset_method_summary$bic[bsm_best_bic],col="red",cex=2,pch=20)

# Create a prediction function to make predictions
# for regsubsets with id predictors included
predict.regsubsets=function(object,newdata,id,...){
form=as.formula(object$call[[2]])
mat=model.matrix(form,newdata)
coefi=coef(object,id=id)
xvars=names(coefi)
mat[,xvars]%*%coefi
}

# Create indexes to divide the data between folds
k=10
set.seed(1)
folds=sample(1:k,nrow(credit_data_training),replace=TRUE)
cv.errors=matrix(NA,k,credit_data_number_predictors,
                 dimnames=list(NULL, paste(1:credit_data_number_predictors)))
# Perform CV
for(j in 1:k){
  best_subset_method=regsubsets(Balance~.,data=credit_data_training[folds!=j,],nvmax=credit_data_number_predictors)
  for(i in 1:credit_data_number_predictors){
  pred=predict(best_subset_method,credit_data_training[folds==j,],id=i)
  cv.errors[j,i]=mean( (credit_data_training$Balance[folds==j]-pred)^2)
  }
}

# Compute mean cv errors for each model size
mean.cv.errors=apply(cv.errors,2,mean)
mean.cv.errors
# Plot the mean cv errors
par(mfrow=c(1,1))
plot(mean.cv.errors,type='b')

# Fit the selected model using the whole training data
# and compute test error
# models selected
number_predictors_selected <- 4

# Create info for lm call
variables <- names(coef(best_subset_method,id=number_predictors_selected))
variables <- variables[!variables %in% "(Intercept)"]
bsm_formula <- as.formula(best_subset_method$call[[2]])
bsm_design_matrix <- model.matrix(bsm_formula,credit_data_training)[, variables]
bsm_data_train <- data.frame(Balance = credit_data_training$Balance, bsm_design_matrix)
# Fit a standard linear model using only the selected
# predictors on the training data
model_best_subset_method <- lm(formula = bsm_formula, bsm_data_train)
summary(model_best_subset_method)

# Make predictions on the test set
bsm_design_matrix_test <- model.matrix(bsm_formula,credit_data_testing)[, variables]
bsm_predictions <- predict(object = model_best_subset_method, newdata = as.data.frame(bsm_design_matrix_test))

# Compute test squared errors
bsm_squared_errors <- (credit_data_testing$Balance-bsm_predictions)^2
squared_errors <- data.frame(bsm_squared_errors=bsm_squared_errors)
# test MSE
mean(bsm_squared_errors)


#===========#
# Problem 5 #
#===========#

library(glmnet)
# Package Lasso and Elastic-Net Regularized
# Generalized Linear Models

x_train <- model.matrix(Balance~.,credit_data_training)[,-1]
y_train <- credit_data_training$Balance

x_test <- model.matrix(Balance~.,credit_data_testing)[,-1]
y_test <- credit_data_testing$Balance

ridge_mod <- glmnet(x_train,y_train,alpha=0)

set.seed(1)
cv.out=cv.glmnet(x_train, y_train,alpha=0)
plot(cv.out)

ridge_mod <- glmnet(x_train,y_train,alpha=0)
best_lambda_ridge <- cv.out$lambda.min
best_lambda_ridge

ridge_predictions = predict(ridge_mod,s=best_lambda_ridge,newx=x_test)
ridge_square_errors <- as.numeric((ridge_predictions-y_test)^2)
squared_errors <- data.frame(ridge_square_errors = ridge_square_errors, squared_errors)

#===========#
# Problem 6 #
#===========#

lasso_mod <- glmnet(x_train,y_train,alpha=1)

set.seed(1)
cv.out=cv.glmnet(x_train, y_train,alpha=1)
plot(cv.out)

best_lambda_lasso <- cv.out$lambda.min
best_lambda_lasso

lasso_predictions = predict(lasso_mod,s=best_lambda_lasso,newx=x_test)
lasso_square_errors <- as.numeric((lasso_predictions-y_test)^2)
squared_errors <- data.frame(lasso_square_errors = lasso_square_errors, squared_errors)

#===========#
# Problem 7 #
#===========#
x <- model.matrix(Balance~.,credit_data)[,-1]
credit_pca <- prcomp(x, center = TRUE, scale. = TRUE)
print(credit_pca)
plot(credit_pca, type = "l")
summary(credit_pca)

#===========#
# Problem 8 #
#===========#

library(pls)
set.seed(1)
pcr_model <- pcr(Balance~., data=credit_data_training,scale=TRUE, validation="CV")
validationplot(pcr_model,val.type="MSEP")

pcr_predictions = predict(pcr_model,credit_data_testing,ncomp=10)
pcr_square_errors <- as.numeric((pcr_predictions-credit_data_testing$Balance)^2)
squared_errors <- data.frame(pcr_square_errors = pcr_square_errors, squared_errors)
mean(pcr_square_errors)

library(ggplot2)
library(reshape2)
ggplot(melt(squared_errors)) + geom_boxplot(aes(variable, value))

#===========#
# Problem 9 #
#===========#

set.seed(1)
plsr_model <- plsr(Balance~., data=credit_data_training,scale=TRUE, validation="CV")
validationplot(plsr_model,val.type="MSEP")

plsr_predictions = predict(plsr_model,credit_data_testing,ncomp=3)
plsr_square_errors <- as.numeric((plsr_predictions-credit_data_testing$Balance)^2)
squared_errors <- data.frame(plsr_square_errors = plsr_square_errors, squared_errors)
mean(plsr_square_errors)

ggplot(melt(squared_errors)) + geom_boxplot(aes(variable, value))

colMeans(squared_errors)

#================#
# Lab: Chapter 6 #
#================#

## Best Subset Selection
library(ISLR)
library(leaps)

data <- na.omit(Hitters)
regfit.full <- regsubsets(Salary~., data, nvmax = 19)
reg.sum <- summary(regfit.full)
reg.sum
reg.sum$rsq

par(mfrow = c(2,2))
# RSS
plot(reg.sum$rss, xlab="Number of Variables ",ylab="RSS", type="l")

# AdjRsq
plot(reg.sum$adjr2 ,xlab="Number of Variables ", ylab="Adjusted RSq",type="l")
which.max(reg.sum$adjr2)
points(which.max(reg.sum$adjr2),max(reg.sum$adjr2), col="red",cex=2,pch=20)

# Cp
plot(reg.sum$cp, xlab="Number of Variables ", ylab="Cp", type = 'l')
points(which.min(reg.sum$cp), min(reg.sum$cp), col = "red", cex = 2, pch = 20)

# BIC
plot(reg.sum$bic, xlab="Number of Variables ", ylab="BIC", type = 'l')
points(which.min(reg.sum$bic), min(reg.sum$bic), col = "red", cex = 2, pch = 20)

# Built-in plot-func.
plot(regfit.full,scale="adjr2")

## Forward and Backward Stepwise Selection
regfit.fwd <- regsubsets(Salary~., data, nvmax=19, method = "forward")
summary(regfit.fwd)
regfit.bwd <- regsubsets(Salary~., data,nvmax=19, method = "backward")
summary(regfit.bwd)

## Validation Set Approach and CV
set.seed(100)
train <- sample(c(T,F), nrow(data), rep = T)
test <- (!train)
regfit.best <- regsubsets(Salary~., data = data[train, ], nvmax = 19)

test.mat <- model.matrix(Salary~., data[test, ])

val.errors=rep(NA,19)
for(i in 1:19) { 
  coefi=coef(regfit.best,id=i)
  pred <- test.mat[, names(coefi)] %*% coefi
  val.errors[i] <- mean((data$Salary[test] - pred)^2)
}
val.errors
which.min(val.errors)

# Using CV: See book

## Ridge Regression and Lasso
library(glmnet)

x <- model.matrix(Salary~.,data)[,-1]
y <- data$Salary

grid <- 10^seq(10, -2, length=100)
ridge.mod <- glmnet(x, y, alpha=0, lambda = grid)

set.seed(1)
train <- sample(1:nrow(x), nrow(x)/2)
test <- (-train)

ridge.mod <- glmnet(x[train, ], y[train], alpha = 0, lambda = grid)
pred <- predict(ridge.mod, s = 4, newx = x[test, ])
mean((pred - y[test])^2)

# CV:
cv.out <- cv.glmnet(x[train,], y[train], alpha = 0)
bestlam <- cv.out$lambda.min
pred <- predict(ridge.mod, s = bestlam, newx = x[test, ])
mean((pred - y[test])^2)

lasso.mod <- glmnet(x[train, ], y[train], alpha = 1, lambda  = grid)
cv.out <- cv.glmnet(x[train,], y[train], alpha = 1)
pred <- predict(lasso.mod, s = cv.out$lambda.min, newx = x[test, ])
mean((pred - y[test])^2)

# PCR and PLS Regression
library(pls)
set.seed(2)

pcr.fit <- pcr(Salary~., data = Hitters, subset = train, scale =  T, validation = "CV")
summary(pcr.fit)
validationplot(pcr.fit, val.type = "MSEP")
pred.pcr <- predict(pcr.fit, x[test, ], ncomp = 6)
(mse.pcr <- mean((pred.pcr - y[test])^2))

pls.fit <- plsr(Salary~., data = Hitters, subset = train, scale = T, validation = "CV")
summary(pls.fit)
validationplot(pls.fit, val.type = "MSEP")
pred.pls <- predict(pls.fit, x[test, ], ncomp = 7)
(mse.pls <- mean((pred.pls - y[test])^2))
