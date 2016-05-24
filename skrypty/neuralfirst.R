#R sieć neuronowa
#https://en.wikipedia.org/wiki/Backpropagation
#https://aimatters.wordpress.com/2015/12/19/a-simple-neural-network-in-octave-part-1/
#Zamieszczone przykłady dadzą się uruchomić jeśli zainstalujecie pakiety R 
#(w linuxie na roocie w konsoli R, żeby nie instalować na lokalnym koncie):
pkglist<-c("clusterGeneration","corrplot","nnet","neuralnet","RSNNS","reshape","rockchalk","fifer","ade4","sqldf","plyr","dplyr")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#funkcja progowa sigmoidalna
sigmoid <- function(x) {
  1.0 / (1.0 + exp(-x))
}
#generuje x od -100 do 100 i dzielę przez 10, aby mieć od -10 do 10 z gęstszym upakowaniem 
x <- (c(1:200)-100)/10
plot(x,sigmoid(x))

#ustawiamy początek pseudolosowego generatora, aby zawsze mieć te same wyniki
set.seed(1235)

A1=c(1,0,0)
#dwa sposoby na generację macierzy dwuwymiarowej
# w tym przypadku wszystkich połaczeń między wejściem X1 i X2 oraz 3 neuronami warstwy środkowej
nkolumn=3
mwierszy=2 
THETA1<-t(replicate(mwierszy, runif(nkolumn,-1,1)))
THETA1<-matrix(runif(mwierszy*nkolumn), ncol=nkolumn)

# w tym przypadku wszystkich połaczeń między wyjściem z sieci neuronowej oraz neuronem na wyjściu
nkolumn=3
mwierszy=1 
THETA2<-matrix(runif(mwierszy*nkolumn), ncol=nkolumn)

Z2 <- THETA1 %*% A1
A2 <- c(1, sigmoid(Z2))
Z3 <- c(THETA2 %*% A2)
h <- sigmoid(Z3)
h

alfa<-1
y <- 0
J <- ((y * log(h)) + ((1 - y) * log(1 - h))) * -1
delta3 = h - y
#pochodna sigmoid(Z) równa się sigmoid(Z)*(1-sigmoid(Z))
delta2<-(t(THETA2) %*% delta3 * A2 * (1 - A2))[-1]
THETA2<-THETA2-alfa*delta3%*%t(A2)
THETA1<-THETA1-alfa*delta2%*%t(A1)

Z2 <- THETA1 %*% A1
A2 <- c(1, sigmoid(Z2))
Z3 <- c(THETA2 %*% A2)
h <- sigmoid(Z3)
h


