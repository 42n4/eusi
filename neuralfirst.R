pkglist <- c(
  "clusterGeneration",
  "corrplot",
  "nnet",
  "neuralnet",
  "RSNNS",
  "reshape",
  "rockchalk",
  "fifer"
)
pkgcheck <- pkglist %in% row.names(installed.packages())
for (i in pkglist[!pkgcheck]) {
  install.packages(i, depend = TRUE)
}


library(clusterGeneration)
library(corrplot)

seed.val <- 2
set.seed(seed.val)

num.vars <- 8
num.obs <- 1000

#input variables
#cov.mat<-genPositiveDefMat(num.vars,covMethod=c("unifcorrmat"))$Sigma
#cov.mat<-diag(8)
cov.mat <- genPositiveDefMat(num.vars, covMethod = c("eigen"))$Sigma

#http://www.inside-r.org/packages/cran/rockchalk/docs/mvrnorm
rand.vars <- mvrnorm(num.obs, rep(0, num.vars), Sigma = cov.mat)
cortest <- cor(rand.vars)
#head(round(M,2))
corrplot(cortest, method = "circle")

#output variables
parms <- runif(num.vars, -10, 10)
y1 <- rand.vars %*% matrix(parms) + rnorm(num.obs, sd = 20)
parms2 <- runif(num.vars, -10, 10)
y2 <- rand.vars %*% matrix(parms2) + rnorm(num.obs, sd = 20)

#final datasets
rand.vars <- data.frame(rand.vars)
resp <- data.frame(y1, y2)
names(resp) <- c('Y1', 'Y2')
dat.in <- data.frame(resp, rand.vars)

#nnet function from nnet package
library(nnet)
set.seed(seed.val)
mod1 <- nnet(rand.vars,
             resp,
             data = dat.in,
             size = 10,
             linout = T)

#neuralnet function from neuralnet package, notice use of only one response
library(neuralnet)
form.in <- as.formula('Y1~X1+X2+X3+X4+X5+X6+X7+X8')
set.seed(seed.val)
mod2 <- neuralnet(form.in, data = dat.in, hidden = 10)

#mlp function from RSNNS package
library(RSNNS)
set.seed(seed.val)
mod3 <- mlp(rand.vars, resp, size = 10, linOut = T)

#import the function from Github
library(devtools)
source_url(
  'https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r'
)

#plot each model
plot.nnet(mod1)
plot.nnet(mod2)
plot.nnet(mod3)
