id <- "1X_8OKcoYbng1XvYFDirxjEWr7LtpNr1m" # google file ID
values <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
X = values$X
dim(X)
x0 = values$x0
dim(x0)
beta=values$beta
dim(beta)
sigma=values$sigma
sigma

library(ggplot2)
bias = function(lambda,X,x0,beta)
{
  p = ncol(X)

  #value = mean(t(x0) %*% solve((1+lambda)*diag(p)) %*% beta - t(x0) %*% beta )^2
  #value = mean(t(x0) %*% solve(diag(p) + lambda*t(X) %*% X) %*% beta - t(x0) %*% beta )^2
  #value = mean(t(x0) %*% solve(t(X) %*% X + lambda*diag(p)) %*% t(X) %*% X %*% beta - t(x0) %*%   #beta)^2

  v1 <- t(x0) %*% solve(t(X) %*% X + lambda * diag(p)) %*% t(X) %*% X %*% beta - t(x0) %*% beta
  v2 <- sigma^2 * t(x0) %*% (solve(t(X) %*% X + lambda * diag(p)) %*% t(X) %*% X %*% t(solve(t(X) %*% X + lambda * diag(p)))) %*% x0
  v3 <- sigma^2
  value <-  (t(x0) %*% solve(t(X) %*% X + lambda * diag(p)) %*% t(X) %*% X %*% beta - t(x0) %*% beta)^2
  # Tror dette skal være rett algebraisk, men synes resultatet er merkelig kanskje!
  # Kanskje oppgaven ovenfor burde fullføres først?
  #value = mean(t(x0) %*% (diag(p) + 1/lambda*t(X)%*%X)%*%beta - t(x0) %*% beta)^2

  # All of these give different plots!
  return(value)
}
lambdas = seq(0, 2, length.out = 500)
BIAS = rep(NA,length(lambdas))
for (i in 1:length(lambdas)) BIAS[i] = bias(lambdas[i], X, x0, beta)
dfBias = data.frame(lambdas = lambdas, bias = BIAS)
ggplot(dfBias, aes(x = lambdas, y = bias)) +
  geom_line(color = "red")+
  xlab(expression(lambda))+
  ylab(expression(bias^2))

variance = function(lambda, X, x0, sigma)
{
  p = ncol(X)
  inv = solve(t(X)%*%X+lambda*diag(p))
  value = sigma*t(x0) %*% (inv%*%t(X)%*%X%*%t(inv)) %*% x0
  return(value)
}
lambdas = seq(0, 2, length.out = 500)
VAR=rep(NA,length(lambdas))
for (i in 1:length(lambdas)) VAR[i]=variance(lambdas[i], X, x0, sigma)
dfVar = data.frame(lambdas = lambdas, var = VAR)
ggplot(dfVar, aes(x = lambdas, y = var))+
  geom_line(color = "green4")+
  xlab(expression(lambda))+
  ylab("variance")

exp_mse = BIAS + VAR + sigma^2
lambdas[which.min(exp_mse)]

dfAll = data.frame(lambda = lambdas, bias = BIAS, var = VAR, exp_mse = exp_mse)
ggplot(dfAll)+
  geom_line(aes(x = lambda, y = exp_mse), color = "blue")+
  geom_line(aes(x = lambda, y = bias), color = "red")+
  geom_line(aes(x = lambda, y = var), color = "green4")+
  xlab(expression(lambda))+
  ylab(expression(E(MSE)))


id <- "1yYlEl5gYY3BEtJ4d7KWaFGIOEweJIn__" # google file ID
d.corona <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),header=T)

#d.corona$sex = factor(d.corona$sex)
#d.corona$country = factor(d.corona$country)
lm.fit <- lm(deceased ~ ., data = d.corona) # perhaps a linear model is not the correct model to use?
summary(lm.fit)

newdata = data.frame(sex = "male", age = 75, country = "Korea")
predict(lm.fit, newdata = newdata, type = "response")

l
glm.fit <- glm(deceased ~ ., family = "binomial", data = d.corona)
x0 = data.frame(sex = "male", age = 75, country = "Korea")
predict(glm.fit, newdata = x0, type = "response")


glm.fit <- glm(deceased ~ . + sex:age, family = "binomial", data = d.corona)
summary(glm.fit)

head(d.corona)
sum(d.corona[,1] == 1) /nrow(d.corona)

library(MASS)
library(stats)
library(class)
library(pROC)
library(ISLR)
lda.fit <- lda(deceased ~ ., data = d.corona )
summary(lda.fit)

# 2.d




# 3

id <- "1i1cQPeoLLC_FyAH0nnqCnnrSBpn05_hO" # Google file ID.
diab <- dget(sprintf("https://docs.google.com/uc?id=%s&export=download", id))
t = MASS::Pima.tr2
train = diab$ctrain
test = diab$ctest

lda.diabetes <- lda(diabetes~., data = train)
qda.diabetes <- qda(diabetes~., data = train)

lda.diabetes.probs <- predict(lda.diabetes, newdata = test)$posterior[,2]
lda.preds <- ifelse(lda.diabetes.probs > 0.5, 1, 0)
lda.diabetes.probs
length(test$diabetes)
conf.table.lda.diabetes <- table(predicted = lda.preds, true = test$diabetes)
conf.table.lda.diabetes

qda.diabetes <- qda(diabetes~., data = train)
qda.diabetes.probs <- predict(qda.diabetes, newdata = test)$posterior[, 2]
qda.preds <- ifelse(qda.diabetes.probs > 0.5, 1, 0)
conf.table.qda.diabetes <- table(predicted = qda.preds, true = test$diabetes)
conf.table.qda.diabetes

# 3.d

library(pROC)
library(class)
library(ggplot2)

set.seed(123) # For reproducibility.
knn.diabetes <- knn(train = train, test = test, cl = train$diabetes, k=25, prob=T)
knn.probs <- ifelse(knn.diabetes == 0, 1 - attributes(knn.diabetes)$prob, attributes(knn.diabetes)$prob)

ldaroc = roc(response = test$diabetes, predictor = lda.diabetes.probs, direction = "<")
qdaroc = roc(response = test$diabetes, predictor = qda.diabetes.probs, direction = "<")
knnroc = roc(response = test$diabetes, predictor = knn.probs, direction = "<")


ggroc(list(LDA  = ldaroc, QDA = qdaroc, KNN = knnroc))
auc(ldaroc)
auc(qdaroc)
auc(knnroc)


# 5

id <- "19auu8YlUJJJUsZY8JZfsCTWzDm6doE7C" # google file ID
d.bodyfat <- read.csv(sprintf("https://docs.google.com/uc?id=%s&export=download", id),header=T)

set.seed(123)
boot.fn <-  function(data, index) {
  return(summary(lm(bodyfat ~ age + weight + bmi, data = data, subset = index))$r.squared)
}

B <- 1000
r.squared <- rep(NA, B)
for (b in 1:B){
  r.squared[b] <- boot.fn(d.bodyfat, sample(nrow(d.bodyfat), nrow(d.bodyfat), replace = T))
}

df = data.frame(r.squared = r.squared, norm_den = dnorm(r.squared, mean(r.squared),
                                                        sd(r.squared)))
ggplot(df) + geom_histogram(aes(x = r.squared, y = ..density..), fill = "grey80",color = "black") +
   geom_line(aes(x = r.squared, y = norm_den), color = "red") +
   theme_minimal()

sd(r.squared)
quantile(r.squared, c(0.025, 0.975))


