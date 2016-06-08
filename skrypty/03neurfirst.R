#########################################################################################################################
#R sieć neuronowa
#https://en.wikipedia.org/wiki/Backpropagation
#https://aimatters.wordpress.com/2015/12/19/a-simple-neural-network-in-octave-part-1/
#Zamieszczone przykłady dadzą się uruchomić jeśli zainstalujecie pakiety R 
#(w linuxie na roocie w konsoli R, żeby nie instalować na lokalnym koncie):

pkglist<-c("clusterGeneration","corrplot","nnet","neuralnet","RSNNS","reshape","rockchalk","fifer","ade4","sqldf","plyr","dplyr")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}

#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
seed.val <- 32487893
set.seed(seed.val)

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
mwierszy2=1 
THETA2<-matrix(runif(mwierszy2*nkolumn), ncol=nkolumn)

Z2 <- THETA1 %*% A1
A2 <- c(1, sigmoid(Z2))
Z3 <- c(THETA2 %*% A2)
h  <- sigmoid(Z3)
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

#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
set.seed(seed.val)
#########################################################################################################################
# funkcja ucząca sieć neuronową funkcji XOR
xor_nn <-
  function(XOR,
           THETA1,
           THETA2,
           init_w = 0,
           learn  = 0,
           alpha  = 0.01) {
    # sprawdź, czy to inicjalizacja sieci
    if (init_w == 1) {
      THETA1 <- matrix(runif(mwierszy * nkolumn), ncol = nkolumn)
      THETA2 <- matrix(runif(mwierszy2 * nkolumn), ncol = nkolumn)
    }
    # sumatory korekcji wag z całego zbioru trenującego
    T1_DELTA = array(0L, dim(THETA1))
    T2_DELTA = array(0L, dim(THETA2))
    # przejdź przez cały zbiór trenujący
    m <- 0
    # funkcja kosztu
    J <- 0.0
    #nauczone_xor
    wynik<-c()
    #disp('NN output ');
    for (i in 1:nrow(XOR)) {
      # propagacja sygnału do przodu ku wyjściu i=1
      A1 = c(1, XOR[i, 1:2])
      Z2 <- THETA1 %*% A1
      A2 <- c(1, sigmoid(Z2))
      Z3 <- THETA2 %*% A2
      h <- sigmoid(Z3)
      J <- J + (XOR[i, 3] * log(h)) + ((1 - XOR[i, 3]) * log(1 - h))
      m <- m + 1
      
      # liczymy korekcję t2_delta i t1_delta, aby skorygować błąd
      if (learn == 1) {
        delta3 = h - XOR[i, 3]
        #pochodna sigmoid(Z) równa się sigmoid(Z)*(1-sigmoid(Z))
        delta2 <- (t(THETA2) %*% delta3 * A2 * (1 - A2))[-1]
        # sumuj korekcje dla każdego elementu zbioru uczącego
        T2_DELTA <- T2_DELTA + delta3 %*% t(A2)
        T1_DELTA <- T1_DELTA + delta2 %*% t(A1)
      }
      else{
        cat('Prognoza XOR dla ', XOR[i, 1:2], 'wynosi ', h, '\n')
      }
      wynik<-c(wynik,h)
    }
    J <- J / -m
    #cat('delta3: ', delta3, '\n')
    if (learn == 1) {
      THETA2 <- THETA2 - alfa * (T2_DELTA / m)
      THETA1 <- THETA1 - alfa * (T1_DELTA / m)
      #cat(THETA2,'\n');
      #cat(THETA1,'\n');
    }
    else{
      cat('J: ', J, '\n')
    }
    list(THETA1,THETA2,wynik)
  }

XOR <- rbind(c(0, 0, 0), c(0, 1, 1), c(1, 0, 1), c(1, 1, 0))

list <- structure(NA, class = "result")
"[<-.result" <- function(x, ..., value) {
  args <- as.list(match.call())
  args <- args[-c(1:2, length(args))]
  length(value) <- length(args)
  for (i in seq(along = args)) {
    a <- args[[i]]
    if (!missing(a))
      eval.parent(substitute(a <- v, list(a = a, v = value[[i]])))
  }
  x
}

#wywołanie z inicjalizacją i uczeniem
list[THETA1, THETA2,] <- xor_nn(XOR, THETA1, THETA2, 1, 1, 0.05)

for (i in 1:100000) {
  #wywołanie bez inicjalizacji i z uczeniem
  list[THETA1, THETA2,] <- xor_nn(XOR, THETA1, THETA2, 0, 1, 0.05)
  if (i %% 1000 == 0) {
    cat('Iteracja : ', i, '\n')
    #wywołanie bez inicjalizacji i bez uczenia, zwykła wyuczona odpowiedź sieci neuronowej
    list[THETA1, THETA2,nauczone_xor] <- xor_nn(XOR, THETA1, THETA2)
  }
}
#powinno być 
for (i in 1:nrow(XOR)) {
  cat('Wartość XOR dla ', XOR[i, 1:2], 'wynosi ', XOR[i, 3], '\n')
}
(XOR[, 3] - nauczone_xor) ^ 2                #różnice do kwadratu między założonymi XOR, a uzyskanymi wynikami
sum((XOR[, 3] - nauczone_xor) ^ 2)           #suma kwadratów różnic
#pierwiastek z sumy - końcowy błąd sieci neuronowej
pierwkwadsumkwadrozn <- sqrt(sum((XOR[, 3] - nauczone_xor) ^ 2))
cat('Błąd uczenia się funkcji XOR przez moją sieć neuronową', pierwkwadsumkwadrozn,'\n')
Sys.sleep(2)                                 # pauza na 2 sekund


#########################################################################################################################
library(clusterGeneration)
library(corrplot)
#importuj funkcję wizualizacji sieci neuronowej z Githuba
library(devtools)
source_url(
  'https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r'
)
#biblioteka nnet
library(nnet)

rand.vars <- data.frame(XOR[, 1:2])
names(rand.vars) <- c('X1','X2')
resp <- data.frame(XOR[, 3])
names(resp) <- c('Y1')
dat.in <- data.frame(resp, rand.vars)
dat.in

#nauka sieci neuronowej z 3 neuronami (minimalna liczba - 3 neurony) 
#i liniowym wyjściem z neurona
mod1 <- nnet(rand.vars,
             resp,
             data = dat.in,
             size = 3,
             linout = T)
mod1
par(mfrow = c(3, 1))
#pokaż nauczoną sieć, szare to minusowe, czarne to dodatnie wagi połączeń
plot.nnet(mod1)
#przewidujemy wartości funkcji XOR
nauczone_xor <- predict(mod1, cbind(XOR[, 1:2]))
nauczone_xor
#powinno być 
cbind(XOR[, 3])                              
(cbind(XOR[, 3]) - nauczone_xor) ^ 2         #różnice do kwadratu między założonymi XOR, a uzyskanymi wynikami nauczonymi
sum((cbind(XOR[, 3]) - nauczone_xor) ^ 2)    #suma kwadratów różnic
#pierwiastek z sumy - końcowy błąd sieci neuronowej
pierwkwadsumkwadrozn <- sqrt(sum((cbind(XOR[, 3]) - nauczone_xor) ^ 2))
cat('Błąd uczenia się funkcji XOR przez sieć neuronową nnet', pierwkwadsumkwadrozn,'\n')
Sys.sleep(2)                                 # pauza na 2 sekund

