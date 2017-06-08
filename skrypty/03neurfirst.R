#########################################################################################################################
#R sieć neuronowa - najprostsza forma backpropagation na macierzach - nauka funkcji XOR
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
#generuje x od -10 do 10 co 0.1
x <- seq(-10,10,0.1)
plot(x,sigmoid(x))

#ustawiamy początek pseudolosowego generatora, aby zawsze mieć te same wyniki
set.seed(1235)

#wejście do sieci neuronowej BIAS=1, X1=0, X2=0, gdzie bias dodatkowa zmienna równa 1 tylko jej waga się zmienia
A1=c(1,0,0)
#zakładane wyjśćie y - tego dla X=(0,0) będziemy uczyć sieć neuronową
y <- 0
#dwa sposoby na generację macierzy dwuwymiarowej
# w tym przypadku wszystkich połaczeń między wejściem X1, X2 i b oraz 3 neuronami warstwy środkowej
nkolumn=3
mwierszy=3 
W1<-t(replicate(mwierszy, runif(nkolumn,-1,1)))
W1<-matrix(runif(mwierszy*nkolumn), ncol=nkolumn)

# w tym przypadku wszystkich połaczeń między wyjściem z sieci neuronowej plus b oraz neuronem na wyjściu
nkolumn2=4
mwierszy2=1 
W2<-matrix(runif(mwierszy2*nkolumn2), ncol=nkolumn2)

#THE FIRST LOOP - FORWARD PROPAGATION OF A
N2 <- W1 %*% A1
A2 <- c(1, sigmoid(N2))
N3 <- c(W2 %*% A2)
h <- A3 <- sigmoid(N3)
h

#THE NEXT LOOPS
#ERROR BACKPROPAGATION
alfa<-100
J <- ((y * log(h)) + ((1 - y) * log(1 - h))) * -1
delta3 = (h - y)h(1-h)
#pochodna sigmoid(Z) równa się sigmoid(Z)*(1-sigmoid(Z))
delta2<-(t(W2) %*% delta3 * A2 * (1 - A2))[-1]
W2<-W2-alfa*delta3%*%t(A2)
W1<-W1-alfa*delta2%*%t(A1)
#FORWARD PROPAGATION OF A
N2 <- W1 %*% A1
A2 <- c(1, sigmoid(N2))
N3 <- c(W2 %*% A2)
h <- A3 <- sigmoid(N3)
h

#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
set.seed(seed.val)
#########################################################################################################################
# APROKSYMACJA FUNKCJI XOR W 4 PUNKTACH ##############################################################################
#########################################################################################################################
# funkcja ucząca sieć neuronową funkcji XOR
init_w = 1; learn  = 1; alpha  = 0.01
xor_nn <- function(XOR,
           W1,
           W2,
           init_w = 0,
           learn  = 0,
           alpha  = 0.01) {
    # sprawdź, czy to inicjalizacja sieci
    if (init_w == 1) {
      W1 <- matrix(runif(mwierszy * nkolumn), ncol = nkolumn)
      W2 <- matrix(runif(mwierszy2 * nkolumn2), ncol = nkolumn)
    }
    # sumatory korekcji wag z całego zbioru trenującego
    T1_DELTA = array(0L, dim(W1))
    T2_DELTA = array(0L, dim(W2))
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
      N2 <- W1 %*% A1
      A2 <- c(1, sigmoid(N2))
      N3 <- W2 %*% A2
      h <- sigmoid(N3)
      J <- J + (XOR[i, 3] * log(h)) + ((1 - XOR[i, 3]) * log(1 - h))
      m <- m + 1
      
      # liczymy korekcję t2_delta i t1_delta, aby skorygować błąd
      if (learn == 1) {
        delta3 = (h - XOR[i, 3])*h*(1-h)
        #pochodna sigmoid(Z) równa się sigmoid(Z)*(1-sigmoid(Z))
        delta2 <- (t(W2) %*% delta3 * A2 * (1 - A2))[-1]
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
      W2 <- W2 - alfa * (T2_DELTA / m)
      W1 <- W1 - alfa * (T1_DELTA / m)
      #cat(W2,'\n');
      #cat(W1,'\n');
    }
    else{
      cat('J: ', J, '\n')
    }
    list(W1,W2,wynik)
}

#funkcja XOR do nauki dwa pierwsze to wejścia, a trzeci to wyjście
XOR <- rbind(c(0, 0, 0), c(0, 1, 1), c(1, 0, 1), c(1, 1, 0))

#http://stackoverflow.com/questions/1826519/function-returning-more-than-one-value
#usprawnienie pobierania listy obiektów zwracanych przez funkcję
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
list[W1, W2,] <- xor_nn(XOR, W1, W2, 1, 1, 0.01)

for (i in 1:50000) {
  #wywołanie bez inicjalizacji i z uczeniem
  list[W1, W2,] <- xor_nn(XOR, W1, W2, 0, 1, 0.05)
  if (i %% 1000 == 0) {
    cat('Iteracja : ', i, '\n')
    #wywołanie bez inicjalizacji i bez uczenia, zwykła wyuczona odpowiedź sieci neuronowej
    list[W1, W2,nauczone_xor] <- xor_nn(XOR, W1, W2)
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
#R sieć neuronowa z pakietu nnet - nauka funkcji XOR na oficjalnej sieci neuronowej z R
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
Sys.sleep(7)                                 # pauza na 7 sekund


#########################################################################################################################
# APROKSYMACJA FUNKCJI SINUS W 20 PUNKTACH ##############################################################################
#########################################################################################################################
#R sieć neuronowa z pakietu nnet - nauka funkcji sinus w 20 punktach i aproksymacja reszty zakresu
library(clusterGeneration)
library(corrplot)
#importuj funkcję wizualizacji sieci neuronowej z Githuba
library(devtools)
source_url(
  'https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r'
)
#biblioteka nnet
library(nnet)

#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
seed.val <- 86644
set.seed(seed.val)

#ilość punktów z x do nauki
num.obs <- 20
#ilość neuronów w pierwszej i jedynej warstwie sieci
max.neurons <- 200

#do nauki sieci 20 punktów co jeden
x1 <- seq(1, num.obs, 1)
#gęstsze próbkowanie do sprawdzenia działania sieci, jej aproksymacji między punktami uczenia
xx1 <- seq(1, num.obs, 0.3)

#dane do nauki, na końcowym wykresie czerwone punkty
y1 <- sin(x1)
#tak powinna działać sieć aproksymować ten wykres yy1, na końcowym wykresie zielona ciągła linia
yy1 <- sin(xx1)
plot(x1, y1, col = "red")
lines(xx1, yy1, col = "green")

#dane pakowane w ramki danych specjalnie dla funkcji sieci neuronowej: X1 - wejście,  Y1 - wyjście do nauki
rand.vars <- data.frame(x1)
names(rand.vars) <- c('X1')
resp <- data.frame(y1)
names(resp) <- c('Y1')
dat.in <- data.frame(resp, rand.vars)
dat.in

#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
set.seed(seed.val)
#nauka sieci neuronowej z 20 neuronami i liniowym wyjściem z neurona
mod1 <- nnet(rand.vars,
             resp,
             data = dat.in,
             size = 20,
             linout = T)

par(mfrow = c(3, 1))
#pokaż nauczoną sieć, szare to minusowe, czarne to dodatnie wagi połączeń
plot.nnet(mod1)


#sprawdzenie działania sieci na gęstszej próbce xx1
x1
xx1
ypred <- predict(mod1, cbind(xx1))
plot(xx1, ypred)
#kwadrat różnic między założonymi yy1, a uzyskanymi wynikami ypred
kwadroznicy <- (yy1 - ypred) ^ 2
#suma kwadratów różnic
sumkwadrozn <- sum((yy1 - ypred) ^ 2)
#pierwiastek z sumy - końcowy błąd sieci neuronowej
error1 <- sqrt(sumkwadrozn)
error1
Sys.sleep(2)                                 # pauza na 2 sekund


#########################################################################################################################
#R sieć neuronowa nnet - badanie wpływu ilości neuronów warstwy ukrytej
errorlist <- list()                          #pusta lista
#przeprowadź naukę sieci od 4 do max.neurons np. 100 neuronów w jedynej warstwie
for (i in 4:max.neurons) {
  #ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
  set.seed(seed.val)
  #nauka sieci neuronowej z i (z pętli for) neuronami i liniowym wyjściem z neurona
  mod1 <- nnet(
    rand.vars,
    resp,
    data = dat.in,
    size = i,
    linout = T,
    trace = FALSE
  )
  #sprawdzenie działania sieci na gęstszej próbce xx1
  ypred <- predict(mod1, cbind(xx1))
  #policzenie błędu z pierwiastka sumy kwadratów różnic
  error <- sqrt(sum((yy1 - ypred) ^ 2))
  # i dodanie do listy w której indeks+3 oznacza liczbę neuronów w sieci
  errorlist <- c(errorlist, error)
}
#przetworzenie listy do wektora
errorvector <- rapply(errorlist, c)
#wyrysowanie wektora na wykresie - nie widać tendencji malejących lub rosnących
plot(errorvector)
#minimalny błąd
minerror <- min(errorvector)
minerror
#optimise<-which(errorvector %in% c(min(errorvector)))
#i jego indeks czyli liczba neuronów zmniejszona o 3 gdyż pętla for zaczynała od liczby neuronów 4
optimsize <- match(min(errorvector), errorvector)
optimsize
Sys.sleep(2)                                 # pauza na 2 sekund

#########################################################################################################################
#nnet z optymalną liczbą neuronów
#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
set.seed(seed.val)
#ponowna nauka sieci z idealną liczbą neuronów (dającą najmniejszy błąd)
mod1 <-
  nnet(
    rand.vars,
    resp,
    data = dat.in,
    size = optimsize + 3,
    linout = T,
    trace = FALSE
  )
#sprawdzenie działania sieci na gęstszej próbce xx1
ypred <- predict(mod1, cbind(xx1)) #uwaga xx1 , a nie x1
error2 <- sqrt(sum((yy1 - ypred) ^ 2))
#powinien być ten sam błąd co dla indeksu optimise minerror
error2

#końcowy wykres
#dane do nauki, na końcowym wykresie czerwone punkty
#yy1 - zadana funkcja na gęstszej próbie xx1, na końcowym wykresie zielona ciągła linia
#czarna linia to końcowa aproksymacja sieci na gęstszej próbie niż była uczona 
#zmuszanie sieci do "wymyślania" nowych punktów, które tworzą czarną linię
par(mfrow = c(3, 1))
#plot each model
plot.nnet(mod1)
plot(x1, y1, col = "red")       # czerwone punkty nauki funkcji sinus
lines(xx1, yy1, col = "green")  # zielona prawdziwa funkcja sinus 
lines(xx1, ypred)               # czarna aproksymowana przez nauczoną sieć funkcja sinus
plot(errorvector)

