#http://www.inside-r.org/packages/cran/rockchalk/docs/mvrnorm
library(MASS)
library(rockchalk)
library(fifer)

set.seed(12345)
X0 <- MASS::mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
## create a smaller data set, starting at same position
set.seed(12345)
X1 <- MASS::mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
## Create a larger data set
set.seed(12345)
X2 <- MASS::mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
## The first 5 rows in X0, X1, and X2 are not the same
head(round(X0,2))
head(round(X1,2))
head(round(X2,2))
set.seed(12345)
Y0 <- mvrnorm(n=10, mu = c(0,0,0), Sigma = diag(3))
set.seed(12345)
Y1 <- mvrnorm(n=5, mu = c(0,0,0), Sigma = diag(3))
set.seed(12345)
Y2 <- mvrnorm(n=15, mu = c(0,0,0), Sigma = diag(3))
# note results are the same in the first 5 rows:
head(round(Y0,2))
head(round(Y1,2))
head(round(Y2,2))
identical(Y0[1:5, ], Y1[1:5, ])
identical(Y1[1:5, ], Y2[1:5, ])

myR <- lazyCor(X = 0.3, d = 5)
mySD <- c(0.5, 0.5, 0.5, 1.5, 1.5)
myCov <- lazyCov(Rho = myR, Sd = mySD)

set.seed(12345)
X0 <- MASS::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
## create a smaller data set, starting at same position
set.seed(12345)
X1 <- MASS::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
head(round(X0,2))
head(round(X0,2))
##' set.seed(12345)
Y0 <- rockchalk::mvrnorm(n=10, mu = rep(0, 5), Sigma = myCov)
## create a smaller data set, starting at same position
set.seed(12345)
Y1 <- rockchalk::mvrnorm(n=5, mu = rep(0, 5), Sigma = myCov)
head(round(Y0,2))
head(round(Y1,2))

set.seed(2)
## generate data with correlation of .6
d = mv.rnorm(n=1000, Sigma=matrix(c(1, .6, .6, 1), 2), names=c("x", "y"))
head(d); cor(d)
## generate data with a random correlation
d = mv.rnorm(n=1000, vars=4, names=letters[1:4])
head(d); cor(d)
## generate non-scaled data
ms = c(100, 10, 5, 0) ### specify means
Sigma = matrix(c(1, .6, .5, .4,
                 .6, 1, .3, .2,
                 .5, .3, 1, .1,
                 .4, .2, .1, 1), 4)
## convert Sigma to covariance matrix
Sigma = cor2cov(Sigma, sd=c(15, 3, 2, 1))
## generate the data
d = mv.rnorm(n=1000, mu=ms, Sigma=Sigma, names=letters[1:4])
head(d); cor(d)


require(graphics)

dnorm(0) == 1/sqrt(2*pi)
dnorm(1) == exp(-1/2)/sqrt(2*pi)
dnorm(1) == 1/sqrt(2*pi*exp(1))

## Using "log = TRUE" for an extended range :
par(mfrow = c(2,1))
plot(function(x) 2*dnorm(x, log = TRUE), -60, 50,
     main = "log { Normal density }")
curve(log(dnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("dnorm(x, log=TRUE)", adj = 0)
mtext("log(dnorm(x))", col = "red", adj = 1)

plot(function(x) 2*pnorm(x, log.p = TRUE), -50, 10,
     main = "log { Normal Cumulative }")
curve(log(pnorm(x)), add = TRUE, col = "red", lwd = 2)
mtext("pnorm(x, log=TRUE)", adj = 0)
mtext("log(pnorm(x))", col = "red", adj = 1)

## if you want the so-called 'error function'
erf <- function(x) 2 * pnorm(x * sqrt(2)) - 1
## (see Abramowitz and Stegun 29.2.29)
## and the so-called 'complementary error function'
erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE)
## and the inverses
erfinv <- function (x) qnorm((1 + x)/2)/sqrt(2)
erfcinv <- function (x) qnorm(x/2, lower = FALSE)/sqrt(2)
