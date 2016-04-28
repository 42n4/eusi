# zbiór funkcji do zajęć EU SI testy wariancji bptest oraz levene.test
# TODO: test, testować
# Licence LGPL
# Author: Piotr Wąsiewicz
########################################################################################################

help(bptest)
## generate a regressor
x <- rep(c(-1,1), 50)
## generate heteroskedastic and homoskedastic disturbances
err1 <- rnorm(100, sd=rep(c(1,2), 50))
err2 <- rnorm(100)
## generate a linear relationship
y1 <- 1 + x + err1
y2 <- 1 + x + err2
## perform Breusch-Pagan test
bptest(y1 ~ x)
bptest(y2 ~ x)
mbp1<-lm(y1~x)
mbp2<-lm(y2~x)
plot(mbp1$residuals,mbp1$fitted.values)
plot(mbp2$residuals,mbp2$fitted.values)
plot(x,y1)
plot(x,y2)
mintervals1<-cut(mbp1$fitted.values,4)
mintervals2<-cut(mbp2$fitted.values,4)
levene.test(mbp1$residuals,factor(mintervals1))
levene.test(mbp2$residuals,factor(mintervals2))
bptest(y2 ~ x)
mintervals2<-cut(mbp2$fitted.values,10)
levene.test(mbp2$residuals,factor(mintervals2))
levene.test(mbp1$residuals,factor(mintervals1))
mintervals1<-cut(mbp1$fitted.values,10)
levene.test(mbp1$residuals,factor(mintervals1))

