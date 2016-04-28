# zbiór funkcji do zajęć EU SI ordinary least squares 
# TODO: test, testować
# Licence LGPL 
# Author: Piotr Wąsiewicz
########################################################################################################
#Książka po polsku: Daniel Larose "Metody i modele eksploracji danych" PWN 2006
#po angielsku: http://cran.r-project.org/doc/contrib/Faraway-PRA.pdf
#Regresja liniowa: zasada KISS - Keep It Simple, Stupid
#y~x <=> y=B0+B1x+e
#zakłócenia e mają
#rozkład normalny
#są niezależne
#mają średnie 0
#mają wariancję niezależną od x


ols <- function( y, x ){ 
size <- length(x) # number of observations 
xmean <- mean(x) 
ymean <- mean(y) 
Sxx <- sum( (x-xmean)^2 ) 
b <- sum( (x-xmean)*(y-ymean) )/Sxx # coefficient 
a <- ymean - b*xmean # interception 
e <- y - a - b*x # residuals 
# SSE (error sum of squares) 
SSE <- sum( e^2 ) 
# SST (total sum of squares) 
SST <- sum( (y-ymean)^2 ) 
# SSR (regression sum of squares) 
SSR <- b^2 * Sxx 
# Coefficient of determination 
r2 <- SSR / SST 
# unbiased estimator of sigma^2 
s.square <- sum(e^2)/(size - 2) 
# standard error for b 
std.error.b <- sqrt( s.square/Sxx ) 
# standard error for intercept 
std.error.a <- sqrt( s.square*(1/size + xmean^2/Sxx) ) 
standard.errors <- list( intercept=std.error.a, coeficient=std.error.b ) 
coefficients <- list( intercept=a, coefficient=b ) 
# create row names for data.frame 
rownames <- c("Intercept", "X")
# create data.frame 
z <- data.frame( row.names=rownames, cbind(coefficients,standard.errors) )
#F-test of y~1 (no slopes) and y~x
#error of y~1
e1<-y-mean(y)
#SSE of y~1
SSE1<-sum(e1^2)
#degree of freedom of least squares for sum(e)=0
df<-length(y)-2
#degree of freedom of x-mean(x) for sum(e1)=0
df1<-length(y)-1
#degree of NUM and DEN from F-test
df.num<-df1-df
df.den<-df
#F-test the same as lm F-statistic
F<-((SSE1-SSE)/df.num)/(SSE/df.den)
#p.value the same as in summary(lm(y~x))
p<-1-pf(F, df.num, df.den)
return (list(z,r2,F,p))
} 

x <- c(-3, -1.5, 2, 5, 7, 8)
y <- c(2, 4, 5, 6, 6, 9)
o<-ols(y,x)
my<-lm(y~x)
#the same values of 
o
summary(my)
smy<-summary(my)
names(smy)
fmy<-summary(my)["fstatistic"]
1-pf(fmy$fstatistic[["value"]],fmy$fstatistic[["numdf"]],fmy$fstatistic[["dendf"]])
an<-anova(lm(y~1),my)
#F statistic value
an$F[2]
#degrees of freedom
my$df
1-pf(an$F[2],1,my$df)
qf(0.95,1,my$df)

