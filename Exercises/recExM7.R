
## Problem 1

library(ISLR)
# extract only the two variables from Auto
ds = Auto[c("horsepower", "mpg")]
n = nrow(ds)
# which degrees we will look at
deg = 1:4
set.seed(1)
# training ids for training set
tr = sample.int(n, n/2)
# plot of training data
plot(ds[tr, ], col = "darkgrey", main = "Polynomial regression")

# which colors we will plot the lines with
co = rainbow(length(deg))
# iterate over all degrees (1:4) - could also use a for-loop here
MSE = sapply(deg, function(d) {
  # fit model with this degree
  mod = lm(mpg ~ poly(horsepower, d), ds[tr, ])
  # add lines to the plot - use fitted values (for mpg) and horsepower from
  # training set
  lines(cbind(ds[tr, 1], mod$fit)[order(ds[tr, 1]), ], col = co[d])
  # calculate mean MSE - this is returned in the MSE variable
  mean((predict(mod, ds[-tr, ]) - ds[-tr, 2])^2)
})
# add legend to see which color corresponds to which line
legend("topright", legend = paste("d =", deg), lty = 1, col = co)

# plot MSE
plot(MSE, type = "o", pch = 16, xlab = "degree", main = "Test error")


# solution with ggplot
library(ISLR)
library(ggplot2)
# extract only the two variables from Auto
ds = Auto[c("horsepower", "mpg")]
n = nrow(ds)
# which degrees we will look at
deg = 1:4
set.seed(1)
# training ids for training set
tr = sample.int(n, n/2)
# plot of training data
ggplot(data = ds[tr, ], aes(x = horsepower, y = mpg)) + geom_point(color = "darkgrey") +
  labs(title = "Polynomial regression")

# iterate over all degrees (1:4) - could also use a for-loop here
dat = c()  #make a empty variable to store predicted values
for (d in deg) {
  # fit model with this degree
  mod = lm(mpg ~ poly(horsepower, d), ds[tr, ])
  # dataframe of predicted values - use fitted values (for mpg) and horsepower
  # from training set and add column (factor) for degree
  dat = rbind(dat, data.frame(horsepower = ds[tr, 1], mpg = mod$fit, degree = as.factor(rep(d,
                                                                                            length(mod$fit)))))
  # calculate mean MSE - this is returned in the MSE variable
  MSE[d] = mean((predict(mod, ds[-tr, ]) - ds[-tr, 2])^2)
}
# plot fitted values for different degrees
ggplot(data = ds[tr, ], aes(x = horsepower, y = mpg)) + geom_point(color = "darkgrey") +
  labs(title = "Polynomial regression") + geom_line(data = dat, aes(x = horsepower,
                                                                    y = mpg, color = degree))
# plot MSE
MSEdata = data.frame(MSE = MSE, degree = 1:4)
ggplot(data = MSEdata, aes(x = degree, y = MSE)) + geom_line() + geom_point() +
  labs(title = "Test error")

## Problem 2

attach(Auto)

fit <- lm(mpg ~ factor(origin))
# make a new dataset of the origins to predict the mpg for the different
# origins
new = data.frame(origin = as.factor(sort(unique(origin))))
# predicted values and standard errors
pred = predict(fit, new, se = T)
# dataframe including CI (z_alpha/2 = 1.96)
dat = data.frame(origin = new, mpg = pred$fit, lwr = pred$fit - 1.96 * pred$se.fit,
                 upr = pred$fit + 1.96 * pred$se.fit)
# plot the fitted/predicted values and CI
ggplot(dat, aes(x = origin, y = mpg)) + geom_point() + geom_segment(aes(x = origin,
                                                                        y = lwr, xend = origin, yend = upr)) + scale_x_discrete(labels = c(`1` = "1.American",
                                                                                                                                           `2` = "2.European", `3` = "3.Japanese"))
## Problem 4

library(ISLR)
attach(Wage)
# install.packages('gam')
library(gam)

# Write a couple of functions first, which will be used to produce the
# components of the design matrix We write separate functions to generate
# X_1, X_2 and X_3 (the three components of the model) X_1: The function
# mybs() generates basis functions for the cubic spline
mybs = function(x, knots) cbind(x, x^2, x^3, sapply(knots, function(y) pmax(0,
                                                                            x - y)^3))

# X_2: The function myns() generates basis functions for the natural cubic
# spline; d() is a helper function
d = function(c, cK, x) (pmax(0, x - c)^3 - pmax(0, x - cK)^3)/(cK - c)
myns = function(x, knots) {
  kn = c(min(x), knots, max(x))
  K = length(kn)
  sub = d(kn[K - 1], kn[K], x)
  cbind(x, sapply(kn[1:(K - 2)], d, kn[K], x) - sub)
}

# X_3: The function myfactor() generates the dummy-basis functions for a
# factor covariate, building on the R-function model.matrix()
myfactor = function(x) model.matrix(~x)[, -1]

# Once these functions are prepared, we can generate the model matrix X =
# (1,X_1, X_2, X_3) as a one-liner
X = cbind(1, mybs(age, c(40, 60)), myns(year, 2006), myfactor(education))

# fitted model with our X
myhat = lm(wage ~ X - 1)$fit
# fitted model with gam
yhat = gam(wage ~ bs(age, knots = c(40, 60)) + ns(year, knots = 2006) + education)$fit
# are they equal?
all.equal(myhat, yhat)

## Problem 5

attach(Auto)
origin = as.factor(origin)
am <- gam(mpg ~ bs(displacement, knots = c(290)) + poly(horsepower, 2) + weight +
            s(acceleration, df = 3) + origin)
# plot covariates
par(mfrow = c(2, 3))
plot(am, se = TRUE, col = "blue")
# summary of fitted model
summary(am)
