#http://www.r-bloggers.com/linear-regression-by-gradient-descent/
##
## Liniowa regresja na bazie najszybszego spadku
## na podstawie
## https://www.coursera.org/learn/machine-learning/lecture/kCvQc/gradient-descent-for-linear-regression
## http://cs229.stanford.edu/notes/cs229-notes1.pdf
## http://machinelearningmastery.com/convex-optimization-in-r/


################################################################################
# SPADEK GRADIENTU - w 1D pochodnej - dla funkcji 1D

funkcja <- function(x) {                     # zdefiniuj funkcję 1D z optimum w punkcie (0,0)
  x^2
}

pochodna <- function(x) {                    # określ pochodną funkcji 1D
  2*x
}

# metoda spadku gradientu w 1D
spadek_gradientu <- function(func, pochodna, start, alfa=0.05, tol=1e-8) {
  pt1 <- start                               # start
  grdnt <- pochodna(pt1)                     # pochodna funkcji 
  pt2 <- pt1 - alfa*grdnt                    # odejmujemy pochodną razy alfa od wejściowego parametru
  while (abs(func(pt1)-func(pt2)) > tol) {   # przy minimum małe wartości pochodnej, jeśli mniejsze od tol przerwij pętle
    pt1 <- pt2                               # w przypadku spadku funkcji dodajemy do x
    grdnt <- pochodna(pt1)                   # w przypadku wzrostu funkcji odejmujemy do x
    pt2 <- pt1 - alfa*grdnt                  # aby "stoczyć się" do minimum
    print(func(pt2))                         # wypisuj postęp schodzenia do minimum
  }                                          
  pt2                                        # zwróć ostatni punkt optymalizacji
}

result <- spadek_gradientu(                  # znajdź minimum używając funkcji spadku gradientu
  funkcja,                                   # funkcja do optymalizacji
  pochodna,                                  # gradient funkcji 
  c(runif(1,-3,3)),                          # punkt startu
  0.05,                                      # krok optymalizacji
  1e-8)                                      # dopuszczalny błąd

# wyswietl rezultat optymalizacji
print(result)                                # współrzędne minimum
print(funkcja(result))                       # wartość funkcji w minimum
x <- seq(-3, 3, length.out=100)
y <- sapply(x, funkcja)                      # inaczej y <- funkcja(x)
plot(x, y, xlab="x",ylab="y")                # wyswietl funkcję 1D na wykresie
# wskaż czerwonym punktem znalezione optimum
points(result, funkcja(result), col="red", pch=19)
Sys.sleep(2)                                 #pauza na 2 sekundy


################################################################################
# SPADEK GRADIENTU dla funkcji 2D

funkcja <- function(x) {                     # zdefiniuj funkcję 2D z optimum w punkcie (0,0)
  x[1]^2 + x[2]^2
}

gradient <- function(x) {                    # określ pochodną funkcji 2D
  c(2*x[1], 2*x[2])
}

# metoda spadku gradientu w 2D
spadek_gradientu <- function(func, gradient, start, alfa=0.05, tol=1e-8) {
  pt1 <- start                               # start
  grdnt <- gradient(pt1)                     # gradient funkcji po przecieciu wzdłuż x i y
  pt2 <- c(pt1[1] - alfa*grdnt[1], pt1[2] - alfa*grdnt[2])
  while (abs(func(pt1)-func(pt2)) > tol) {   # przy minimum małe wartości gradientu, jeśli mniejsze od tol przerwij pętle
    pt1 <- pt2                               # w przypadku spadku funkcji przecięcia (część gradientu) dodajemy do odpowiedniego wymiaru
    grdnt <- gradient(pt1)                   # w przypadku wzrostu funkcji przecięcia odejmujemy od odpowiedniego wymiaru
    pt2 <- c(pt1[1] - alfa*grdnt[1], pt1[2] - alfa*grdnt[2])
    print(func(pt2))                         # wypisuj postęp schodzenia do minimum
  }
  pt2                                        # zwróć ostatni punkt optymalizacji
}

result <- spadek_gradientu(                  # znajdź minimum używając funkcji spadku gradientu
  funkcja,                                   # funkcja do optymalizacji
  gradient,                                  # gradient funkcji 
  c(runif(1,-3,3), runif(1,-3,3)),           # punkt startu
  0.05,                                      # krok optymalizacji
  1e-8)                                      # dopuszczalny błąd

# wyswietl rezultat optymalizacji
print(result)                                # współrzędne minimum
print(funkcja(result))                       # wartość funkcji w minimum
x <- seq(-3, 3, length.out=100)
y <- seq(-3, 3, length.out=100)
z <- funkcja(expand.grid(x, y))              # wyswietl funkcję 2D jako rysunek konturów
contour(x, y, matrix(z, length(x)), xlab="x",ylab="y")
# wskaż czerwonym punktem znalezione optimum
points(result[1], result[2], col="red", pch=19)
# obrysuj ten punkt kwadratem dla lepszego rozeznania
rect(result[1]-0.2, result[2]-0.2, result[1]+0.2, result[2]+0.2, lwd=2)
Sys.sleep(2)                                 #pauza na 2 sekundy



################################################################################
# REGRESJA LINIOWA
# wygeneruj losowe dane, gdzie y jest funkcją losową x też losowego (inne rozkłady)
x <- runif(1000, -5, 5)
y <- x + rnorm(1000) + 3

res <- lm( y ~ x )                           # wygeneruj model regresji liniowej

# rysunek danych i modelu
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Regresja liniowa')
abline(res, col='blue')
Sys.sleep(2)                                 #pauza na 2 sekundy

################################################################################
# Funkcja kosztów - suma kwadratów błędów między y, a krzywą regresji: y - (Ax+B)
cost <- function(X, y, theta) {
  sum( (X %*% theta - y)^2 ) / (2*length(y))
}

alfa <- 0.01                                 # współczynnik uczenia alfa
num_iters <- 1000                            # liczba iteracji
cost_history <- double(num_iters)            # historia
theta_history <- list(num_iters)
theta <- matrix(c(0,0), nrow=2)              # inicjalizuj współczynniki
X <- cbind(1, matrix(x))                     # dodaj jedynki dla B z Ax+B wzoru prostej

for (i in 1:num_iters) {                     # spadek gradientu dla sumy kwadratów różnic po osi y
  error <- (X %*% theta - y)                 # między punktami (x,y) a linią regresji A*X+B 
  delta <- t(X) %*% error / length(y)        # gdzie Theta to wektor [B,A]
  theta <- theta - alfa * delta              # zwiększamy lub zmniejszamy theta 
  cost_history[i] <- cost(X, y, theta)       # w zależności od wartości gradientu w delta  
  theta_history[[i]] <- theta                # notujemy historię kosztów i theta
}

# rysunek danych i próby optymalizacji 
plot(x,y, col=rgb(0.2,0.4,0.6,0.4), main='Regresja liniowa przez spadek gradientu')
for (i in c(1,3,6,10,14,seq(20,num_iters,by=10))) {
  abline(coef=theta_history[[i]], col=rgb(0.8,0,0,0.3))
}
abline(coef=theta, col="blue")
Sys.sleep(2)                                 #pauza na 2 sekundy

cost_history[seq(1,num_iters, by=100)]       # wykres kosztu funkcji w czasie
plot(cost_history, type='l', col='blue', lwd=2, main='Funkcja kosztu', ylab='koszt', xlab='Iteracja')
Sys.sleep(2)                                 #pauza na 2 sekundy


