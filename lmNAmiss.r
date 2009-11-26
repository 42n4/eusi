# zbiór funkcji do zajêæ ISO WIT lm with NA 
# TODO: test, testowaæ
# Licence LGPL 
# Author: Piotr W±siewicz
########################################################################################################

x=rnorm(100,0,1)
zee=c(rep(0,50),rep(1,50))
z=c(zee[0:80],rep(NA,20))
e=rnorm(100,0,2)

y=x+2*zee+e

reg1=lm(y~x+z)
summary(reg1)

z.miss=ifelse(is.na(z)==T,1,0)
z2=ifelse(is.na(z)==T,0,z)

data.frame(y,x,z,z2,z.miss)

reg2=lm(y~x+z2+z.miss)
summary(reg2) 
