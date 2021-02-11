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

library(tidyverse)

table <- summarise(d.corona, deceased = sum(d.corona$deceased), non_deceased = nrow(d.corona) - deceased)
table

group_by(d.corona, deceased)
summarise(d.corona, deceased = sum(d))

d.france <- filter(d.corona, country == "France")
head(d.france)

table <- table(d.corona$country, d.corona$deceased)
table
colnames(table) = c("hei", "på")
table






