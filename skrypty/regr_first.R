#http://www.r-bloggers.com/linear-regression-by-gradient-descent/
##
## Liniowa regresja na bazie najszybszego spadku
## na podstawie
## https://www.coursera.org/learn/machine-learning/lecture/kCvQc/gradient-descent-for-linear-regression
## http://cs229.stanford.edu/notes/cs229-notes1.pdf
## http://machinelearningmastery.com/convex-optimization-in-r/


#Gradient Descent
# define a 2D basin function, optima is at (0,0)
basin <- function(x) {
  x[1]^2 + x[2]^2
}

# define the derivative for a 2D basin function
derivative <- function(x) {
  c(2*x[1], 2*x[2])
}

# definition of the gradient descent method in 2D
gradient_descent <- function(func, derv, start, step=0.05, tol=1e-8) {
  pt1 <- start
  grdnt <- derv(pt1)
  pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
  while (abs(func(pt1)-func(pt2)) > tol) {
    pt1 <- pt2
    grdnt <- derv(pt1)
    pt2 <- c(pt1[1] - step*grdnt[1], pt1[2] - step*grdnt[2])
    print(func(pt2)) # print progress
  }
  pt2 # return the last point
}

# locate the minimum of the function using the Gradient Descent method
result <- gradient_descent(
  basin, # the function to optimize
  derivative, # the gradient of the function
  c(runif(1,-3,3), runif(1,-3,3)), # start point of the search 
  0.05, # step size (alpha)
  1e-8) # relative tolerance for one step

# display a summary of the results
print(result) # coordinate of fucntion minimum
print(basin(result)) # response of fucntion minimum

# display the function as a contour plot
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- basin(expand.grid(x, y))
contour(x, y, matrix(z, length(x)), xlab="x",ylab="y")
# draw the optima as a point
points(result[1], result[2], col="red", pch=19)
# draw a square around the optima to highlight it
rect(result[1]-0.2, result[2]-0.2, result[1]+0.2, result[2]+0.2, lwd=2)
Sys.sleep(2)                             #pauza na 2 sekundy


#REGRESSION
# generate random data in which y is a noisy function of x
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

# fit a linear model
res <- lm( y ~ x )

# plot the data and the model
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression')
abline(res, col='blue')

# squared error cost function
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

# learning rate and iteration limit
alpha <- 0.01
num_iters <- 1000

# keep history
cost_history <- double(num_iters)
theta_history <- list(num_iters)

# initialize coefficients
theta <- matrix(c(0,0), nrow=2)

# add a column of 1's for the intercept coefficient
X <- cbind(1, matrix(x))

# gradient descent
for (i in 1:num_iters) {
  error <- (X %*% theta - y)
  delta <- t(X) %*% error / length(y)
  theta <- theta - alpha * delta
  cost_history[i] <- cost(X, y, theta)
  theta_history[[i]] <- theta
}

# plot data and converging fit
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Linear regression by gradient descent')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col="blue")

# check out the trajectory of the cost function
cost_history[seq(1,num_iters, by=100)]
plot(cost_history, type='l', col='blue', lwd=2, main='Cost function', ylab='cost', xlab='Iterations')

