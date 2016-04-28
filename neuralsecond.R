pkglist <- c(
  "clusterGeneration",
  "corrplot",
  "nnet",
  "neuralnet",
  "RSNNS",
  "reshape",
  "rockchalk",
  "fifer",
  "reshape",
  "ade4",
  "sqldf",
  "plyr",
  "dplyr"
)
pkgcheck <- pkglist %in% row.names(installed.packages())
#for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}

library(clusterGeneration)
library(corrplot)
#import the function from Github
library(devtools)
source_url(
  'https://gist.github.com/fawda123/7471137/raw/cd6e6a0b0bdb4e065c597e52165e5ac887f5fe95/nnet_plot_update.r'
)
#nnet function from nnet package
library(nnet)

#ustawienie początkowego stanu generatora losowego, aby wyniki za każdym razem były te same
seed.val <- 86644
set.seed(seed.val)

#co który element z 200 pomiarów w dziedzinie od 0 do 20 przekazać do nauki sieci
div <- 3
#ilość punktów z x do nauki
num.obs <- 200 / div
#ilość neuronów w pierwszej i jedynej warstwie sieci
max.neurons <- 100

#do nauki sieci
x1 <- seq(1, num.obs, div) / 10 * div
#gęstsze próbkowanie do sprawdzenia działania sieci, jej aproksymacji między punktami uczenia
xx1 <- seq(1, num.obs) / 10 * div

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
pierwkwadsumkwadrozn <- sqrt(sumkwadrozn)
pierwkwadsumkwadrozn

#pusta lista
errorlist <- list()
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
#wyrysowanie wektora na wykresie
plot(errorvector)
#minimalny błąd
minerror <- min(errorvector)
minerror
#optimise<-which(errorvector %in% c(min(errorvector)))
#i jego indeks czyli liczba neuronów zmniejszona o 3 gdyż pętla for zaczynała od liczby neuronów 4
optimsize <- match(min(errorvector), errorvector)
optimsize

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
error <- sqrt(sum((yy1 - ypred) ^ 2))
#powinien być ten sam błąd co dla indeksu optimise minerror
error

#końcowy wykres
#dane do nauki, na końcowym wykresie czerwone punkty
#yy1 - zadana funkcja na gęstszej próbie xx1, na końcowym wykresie zielona ciągła linia
#czarna linia to końcowa aproksymacja sieci na gęstszej próbie niż była uczona 
#zmuszanie sieci do "wymyślania" nowych punktów, które tworzą czarną linię
par(mfrow = c(3, 1))
#plot each model
plot.nnet(mod1)
plot(x1, y1, col = "red")
lines(xx1, yy1, col = "green")
lines(xx1, ypred)
plot(errorvector)
