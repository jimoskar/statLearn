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
