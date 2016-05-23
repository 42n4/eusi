# Zasady ogólne:
# indeksy w R zaczynają się od 1, a nie od 0
# wielkość liter ma znaczenie
# w rstudio kursor myszy na nazwie funkcji i F1 wywołują opis funkcji
# funkcja pomocy w R 
help(kmeans) # opisuje funkcje kmeans
help(pi) # opisuje stałą pi
# podaj aktualny roboczy katalog
getwd()
# ustaw roboczy katalog
setwd()
# utwórz katalog
dir.create(foldername)
# uruchom skrypt R
source('file.R')
# użyj print() do wypisania czegokolwiek na konsolę
a <- 2
print(a)
# wypisz wszystkie komendy także na konsolę
source('file.R', echo = TRUE)
# wypisz litery i zmienne
i <- 10
cat(i, "th interation...\n", sep = "") # sep="" means that there is no space between the input parameters
# sprawdź strukturę danych w tym przypadku wektor
vec <- c(1:10)
str(vec)
# podaj pierwszych 6 i ostatnich 6 elementów
head(vec)
tail(vec)
no <- c(1:3)
grade <- c(89, 95, 100)
data <- data.frame(no, grade)
head(data)
tail(data)
# podaj typ zmiennych
a <- c(1, 2)
class(a)
# usuń zmienne z pamięci przestrzeni roboczej
x <- c(1, 2)
rm(x)
# usuń wszystkie zmienne z pamięci przestrzeni roboczej
# rm(list=ls(all=T))
rm(list = ls(pattern = '^tmp')) # usuń zmienne z nazwami zaczynającymi się na 'tmp'
# wyjdź z rstudia
# quit() # zapyta czy zapisać przestrzeń roboczą

# Struktury danych
# WEKTORY mają tylko jeden typ danych
# indeksy w R zaczynają się od 1, a nie od 0

a <- c(1, 2, 5, 3, 6,-2, 4)
a[3]                  # 5
a[c(1, 3, 5)]         # 1 5 6
a[2:6]                # 2 5 3 6 -2
b <- replicate(10, 2) # generuje wektor z długością 10, wszystkie elementy to 2
b <- rep(2, 10)       # generuje wektor z długością 10, wszystkie elementy to 2
b <- 1:10             # b równa się 1, 2, 3, 4, 5, 6, 7, 8, 9 10
b <- seq(1, 10)       # b równa się 1, 2, 3, 4, 5, 6, 7, 8, 9 10
b <- seq(1, 10, 2)    # b równa się 1, 3, 5, 7, 9
b <- seq(0, 1, 0.1)   # b równa się 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
b <- seq(from=0, to=1, by=0.1) # b równa się 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
# dodaj element do wektora: metoda 1
e <- 10
a <- c(a, e)
# dodaj element do wektora: metoda 2
a <- append(a, e)
a <- append(a, e, 2)  #dodaj e na pozycji 2+1 czyli 3
a <- append(a, e, 0)  #dodaj e na pozycji 0+1 czyli 1
# dodaj element do wektora: metoda 3
a[length(a) + 1] <- e
# usuń element z wektora
index <- 2
a <- a[-index]        # usuń drugi element z wektora
a[-1]                 # wypisz wektor bez pierwszego elementu
a[-length(a)]         # wypisz wektor bez ostatniego elementu
a[-c(2, 5)]           # usuń drugi i piąty element z wektora
a                     #c( 10,  1,    2,     10,   5,   3,     6,   -2,    4,    10,   10,   10)
a > 3                 # TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE      
a[a>3]                # wypisz elementy wektora większe od 3
a[a==10]              # wypisz elementy wektora równe 10
a[a>1 & a < length(a)]# wypisz elementy wektora większe od 1, a mniejsze od długości a
# wektor z nazwanymi elementami
s<-c(jeden=1,dwa=2,trzy=3)
names(s)     #nazwy elementów
s[["trzy"]]  #wybiera element o nazwie "trzy"
# znajdź indeks pierwszego pasującego elementu
# dla przykładu, znajdź indeks 10-tki w wektorze vec <- c(1, 10, 2, 10).
# match(10, vec) zwróci 2, następny indeks dla 10 nie będzie zwracany
vec <- c(10, 2:10)
e  <- 10
e2 <- c(10, 5)
match(e, vec)     #podaje na którym miejscu znajduje się pierwsze wystąpienie e  w wektorze vec
which(vec %in% e) #podaje na którym miejscu znajduje się e w wektorze vec we wszystkich wystąpieniach
match(e2, vec)    #podaje na którym miejscu znajduje się pierwsze wystąpienie e2 w wektorze vec
which(vec %in% e2)#podaje na którym miejscu znajduje się e2 w wektorze vec we wszystkich wystąpieniach
# sprawdź, czy element znajduje się w wektorze
e3 <- 1
!is.na(match(e3, vec)) #czy element e3 znajduje się w wektorze vec
e3 %in% vec            #czy element e3 znajduje się w wektorze vec 

# elementy z wektoru c1 nie znajdujące się w c2
c1 <- c(1, 2, 3)
c2 <- c(2, 3, 5)
c1[!(c1 %in% c2)] # 1
setdiff(c1, c2)   # 1
# elementy z wektoru c2 nie znajdujące się w c1
c2[!(c2 %in% c1)] # 5
setdiff(c2, c1)   # 5

# oblicz ile jest nie powtarzających się numerów
vec <- c(1, 2, 3, 2)
nlevels(factor(vec)) # zwraca 3
length(unique(vec))  # zwraca 3

# operatory są dostosowane do wektorów - element z elementem na tym samym miejscu
c(1,3,5) + c(5,3,1)   #-> 6,6,6
c(1,3,5) - c(5,3,1)   #-> -4,0,4
c(1,3,5) * c(5,3,1)   #-> 5,9,5
c(2)     * c(5,3,1)   #-> 10,6,2
c(1,3,5) / c(5,3,1)   #-> 0.2,1.0,5.0
c(1,3,5)%/%c(5,3,1)   #-> 0,1,5  dzielenie całkowite
c(1,3,5) %%c(5,3,1)   #-> 1,0,0  mod - reszta całkowita z dzielenia
c(1,3,5) ^ c(5,3,1)   #-> 1,27,5 podnoszenie do potęgi
c(1,3,5) **c(5,3,1)   #-> 1,27,5 podnoszenie do potęgi
c(1,3,5)%in%c(5,3,1)  #-> TRUE,TRUE,TRUE
# obliczenia na wektorach - element * element i ich suma
c(1,3,5) %*% c(5,3,1) #-> 19

#sortowanie elementów wektora
sort(a)
sort(a, decreasing = TRUE) 

# liczba znaków w ciągu (łańcuchu znaków)
x <- 'abc'
numc <- nchar(x)
numc

# znajdź pozycję znaku w ciągu
# \" jest pojedyńczym znakiem, loc jest listą
loc <- gregexpr(pattern = '\"', "abc\"defg") 
cat('Pozycja znaku: ', loc[[1]][1], '\n')

# konwersja łańcucha znaków do całkowitej
# 1 sposób
x <- 123
x <- paste(x)  # x równa się "123"
x <- strtoi(x) # x równa się 123
# 2 sposób
x <- 123
x <- as.character(x)  # x równa się "123"
x <- as.integer(x)    # x równa się 123, także as.numeric(x) jako zmiennoprzecinkowa

#uwaga! 
c(5,'a')  # się konwertuje domyślnie na c('5','a')
e<-c(5)
e[2]<-'a' # się konwertuje domyślnie na c('5','a')
e 
typeof(1:2) == typeof(c(1,2))     # FALSE pierwszy typ integer - drugie double
for( i in 1:length(c()))        print(i)# 1 0 niby pusty wektor, a może coś wypisać 
for( i in seq_len(length(c()))) print(i)# poprawna forma pętli odporna na błąd pustego wektora
for( i in seq_along(c()))       print(i)# poprawna forma pętli odporna na błąd pustego wektora


#MACIERZ: dwuwymiarowa tablica tego samego typu zmiennych
# użyj indeksów macierzy 
x <- matrix(1:16, nrow = 4)
x
x[2, ]        #drugi wiersz
x[, 2]        #druga kolumna
x[1, 4]       #pole z 1 wiersza i 4 kolumny
x[1, c(3, 4)] #pola z 1 wiersza i 3 oraz 4 kolumny
nrow(x)       #liczba wierszy macierzy
ncol(x)       #liczba kolumn macierzy
dim(x)        #wymiary macierzy c(nrow(x),ncol(x))
length(x)     #nrow(x)*ncol(x)
rowMeans(x)   #średnie liczone po wierszach
colMeans(x)   #średnie liczone po kolumnach
rowSums(x)    #sumy liczone po wierszach
colSums(x)    #sumy liczone po kolumnach
t(x)          #transponowana macierz x
det(x)        #wyznacznik macierzy w tym przypadku 0 - macierz sosbliwa
#tworzenie kolumny 4x1 z wektora i nazwanej macierzy 2x2
cells <- c(1, 6, 4, 8)
cells
matrix(cells) # kolumnowy, pionowy wektor
t(t(cells))   # transponowany dwa razy wektor to to samo co kolumnowy wektor
rnames <- c('R1', 'R2')
cnames <- c('C1', 'C2')
# wypełnij macierz po kolumnach, to domyślne ustawienie
colmatrix<-matrix(
  cells,
  nrow = 2,
  ncol = 2,
  byrow = FALSE,
  dimnames = list(rnames, cnames)
)
colmatrix
rownames(colmatrix) #nazwy wierszy
colnames(colmatrix) #nazwy kolumn
c(colmatrix)        #konwersja do wektora spowrotem do oryginalnej postaci wektora cells
# wypełnij macierz po wierszach 
rowmatrix<-matrix(
  cells,
  nrow = 2,
  ncol = 2,
  byrow = TRUE,
  dimnames = list(rnames, cnames)
)
rowmatrix
c(rowmatrix)        #konwersja do wektora, ale innego niż początkowy wektor cells
# macierzowe mnożenie np.: iloczyn pierwszego wiersza i kolumny pierwszej 
# to wymnożenie ich elementów i suma iloczynów
# efektem jest element c(1,1) nowej wynikowej macierzy, dla 1 wiersza i 2 kolumny - c(1,2)
colmatrix %*% rowmatrix #mnożenie macierzy: wierszy pierwszej przez kolumny drugiej
colmatrix *   rowmatrix #mnożenie element colmatrix[i,j] z odpowiadającym elementem rowmatrix[i,j]
crossprod(colmatrix,rowmatrix)   #mnożenie macierzy transponowanej przez zwykłą t(colmatrix) %*% rowmatrix
tcrossprod(colmatrix,rowmatrix)  #mnożenie macierzy zwykłej przez transponowaną colmatrix %*% t(rowmatrix)
solve(colmatrix)                 #odwrócona macierz
solve(colmatrix,rowmatrix)       #rozwiązanie równania colmatrix %*% X = rowmatrix
solve(rowmatrix,colmatrix)       #rozwiązanie równania rowmatrix %*% X = colmatrix
solve(rowmatrix,rowmatrix)       #przekształcenie na tą samą macierz poprzez macierz diagonalną identycznościowa X=diag(2)
diag(2)                          #macierz diagonalna identycznościowa
det(colmatrix %*% rowmatrix)     #det(A · B) = detA · detB
det(colmatrix) %*% det(rowmatrix)#det(A · B) = detA · detB


#TABLICA ARRAY: podobna do macierzy matrix, ale może mieć więcej wymiarów
dim1 <- c('A1', 'A2')
dim2 <- c('B1', 'B2', 'B3')
dim3 <- c('C1', 'C2', 'C3', 'C4')
z <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
z
z[1, 2, 3] #wartość komórki z jednego pola trójwymiarowej macierzy


#RAMKA DANYCH: kolumny mogą mieć różne typy
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c('Type1', 'Type2', 'Type1', 'Type1')
status <- c('Poor', 'Improved', 'Excellent', 'Poor')
patientdata <- data.frame(patientID, age, diabetes, status)
x <- nrow(patientdata)
y <- ncol(patientdata)
size <-
  dim(patientdata) # rozmiar to wektor z liczbą wierszy i kolumn
# podaj ij-ty element ramki danych
i <- 1
j <- 2
patientdata[i, j]
patientdata[[j]][i]
patientdata[, j][i]
as.integer(patientdata[i,][j]) # patientdata[i, ][j] i-ty wiersz jta kolumna jako integer
patientdata$age[i]
patientdata[i, 'age']  # i-ty wiersz kolumny age

# podaj wiersz i kolumnę ramki danych
patientdata[2,] # drugi wiersz jako ramka danych
patientdata[, 2] # druga kolumna jako wektor
patientdata[, 'age'] # kolumna age jako wektor
patientdata$age # kolumna age jako wektor
patientdata[['age']] # kolumna age jako wektor
patientdata['age'] # kolumna age jako ramka danych

patientdata[1:2] # pierwsze dwie kolumny jako ramka danych
patientdata[c('diabetes', 'status')]
patientdata$age
table(patientdata$diabetes, patientdata$status)
# dodaj kolumnę do ramki danych: metoda 1
patientdata$new_col <- c(2:5)
# usuń kolumnę z ramki danych: metoda 1
patientdata$new_col <- NULL
# dodaj kolumnę do ramki danych: metoda 2
patientdata <- transform(patientdata, new_col = c(2:5))
# usuń kolumnę z ramki danych: metoda 2
patientdata <- transform(patientdata, new_col = NULL)
# dodaj wiersz do ramki danych
new_row <-
  data.frame(
    patientID = 5,
    age = 10,
    diabetes = 'Type3',
    status = 'Good'
  )
patientdata <- rbind(patientdata, new_row)
# usuń wiersz z ramki danych
index <- 2
patientdata <- patientdata[-index,]
#
# attach, detach, with
# attach, detach nie pracują na tych samych nazwach zmiennych, użyj "with"
summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)
attach(mtcars) # dodaj zbiór danych do ścieżki R wyszukiwania
summary(mpg)
plot(mpg, disp)
plot(mpg, wt)
detach(mtcars) # usuń zbiór danych do ścieżki R wyszukiwania

#Faktor zmienna jakościowa, czynnikowa: dyskretne lub porządkowe dane

# mapa wektorów dyskretnych wartości [1...k]
diabetes <- c('Type1', 'Type2', 'Type1', 'Type1')
diabetes <- factor(diabetes) # Levels: Type1 Type2
status <- c('Poor', 'Improved', 'Excellent', 'Poor')
status <-
  factor(status, ordered = TRUE) # Excellent-1 Improved-2 Poor-3
status
status2 <-
  factor(status,
         ordered = TRUE,
         levels = c('Poor', 'Improved', 'Excellent')) # Excellent-3 Improved-2 Poor-1
status2

# przykład pokazujący faktory zmienne jakościowe
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", 'Type1', 'Type1')
status <- c('Poor', 'Improved', 'Excellent', 'Poor')
diabetes <- factor(diabetes)
status <- factor(status, ordered = TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

#Lista: uporządkowany zbiór danych

g <- 'My First List'
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c('one', 'two', 'three')
mylist <- list(title = g, ages = h, j, k)
mylist
mylist[[2]]
mylist[['ages']]
mylist[[2]][1] # 25, pierwszy element drugiego argumentu z listy

#wstawianie danych z pliku csv
# exportuj dane do pliku csv
write.table(
  data,
  'data.csv',
  row.names = F,
  col.names = F,
  sep = ','
)

# czytaj plik csv
signal <- read.table('data.csv', header = FALSE, sep = ',')

#zapisz rysunki do plików

# pierwszy rysunek
pdf('graph1.pdf')
x2plot = seq(1:20)
plot(sin(x2plot))
dev.off()

# drugi rysunek
pdf('graph2.pdf')
plot(cos(x2plot))
dev.off()

#dwa wykresy na jednym rysunku
x <- c(1:5)
y1 <- 2 * x
y2 <- 3 * x
plot(
  x,
  y1,
  type = 'l',
  col = 'red',
  xlab = 'day',
  ylab = 'Net Value'
)
lines(x, y2, type = 'l', col = 'green')

# etykietuj linie z pomocą legend(), lwd - grubość linii, bty='n' - brak ramki wokół
legend(
  'topleft',
  legend = c('line 1', 'line 2'),
  col = c('red', 'green'),
  lwd = 1,
  bty = 'n'
)

# rysuj funkcję
f <- function(x) {
  x * sin(x)
}
plot(f,-20 * pi, 20 * pi)

#narysuj wiele rysunków w tym samym czasie

# pierwszy rysunek
plot(f)
# drugi rysunek
# otwórz nowy rysunek
dev.new()
# przelączanie strzałkami z klawiatury
plot(f(x2plot / 20))
dev.off()

#Zarządzanie, manipulacja strukturami danych

#dodawanie nowych zmiennych do ramki danych

mydata <- data.frame(x1 = c(2, 2, 6, 4),
                     x2 = c(3, 4, 2, 8))

# dodawanie nowych zmiennych do ramki danych
# mamy trzy metody

# metoda 1
mydata$sumx <-  mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2) / 2

rm(x1)
# metoda 2
attach(mydata)
mydata$sumx <-  x1 + x2
mydata$meanx <- (x1 + x2) / 2
detach(mydata)

# metoda 3
mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1 + x2) / 2)

#zapisywanie zmiennych

mydata <- data.frame(x1 = c(2, 2, 6, 4),
                     x2 = c(3, 4, 2, 8))

# metoda 1
mydata$age[mydata$x1 == 2] <- 1
mydata$tmp <- mydata$x1 * mydata$x2
mydata
mydata$tmp <- NULL

mydata$age <- NULL

# metoda 2
mydata <- within(mydata, {
  age <-
    NA # utwórz nową zmienną age i zainicjalizuj ją NA (nullem z R)
  age[x1 == 2] <- 1
  tmp <- x1 * x2
})
mydata

#zmiana nazw

#rename(dataframe, c(oldname1="newname1", oldname2="newname2",...))
library(plyr)
rename(mydata, c(tmp = "temp"))
#names(dataframe) zwraca wektor nazw zmiennych
names(mydata)[3] <- "tmp"
# lub użyj fix(dataframe) aby zmienić nazwy w gui

#manipulacja NA

mydata$age[mydata$age == 1] <- NA
mydata
mydata$age[is.na(mydata$age)] <- 55
mydata
x <- c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4] # y równa się NA
z <- sum(x) # y równa się NA
z <-
  sum(x, na.rm = TRUE) # na.rm=TRUE usuwa brakujące wartości czyli NA
mydata$age[mydata$tmp == 6] <- NA
newdata <- na.omit(mydata) # na.omit() usuwa wiersze z NA
newdata

#zapisz datę jako zmienną
# x jest datą w formie tekstu
# format daty to %Y-%m-%d
mydate <- as.Date('2016-04-28', '%Y-%m-%d')
mydate
class(mydate)

#zapisz datę do formatu
# x jest datą w formie tekstu
# format daty to %Y-%m-%d
datechar <- format(mydate, format = '%Y-%m-%d')
datechar
class(datechar)

#policz okres czasu w sekundach, minutach, godzinach, dniach lub tygodniach
# dataX and dataY są zmiennymi dat
interval <- difftime('2016-04-01', '2016-04-28', units = 'weeks')
interval

#Sys.Date() zwraca dzisiejszą datę w postaci obiektu Date, date() zwraca datę i czas w postaci łańcucha znaków

# zapisz czas programu
t1 <- proc.time()
for (i in 1:1000) {
  cat('.')
}
t2 <- proc.time()
time_elapsed <- (t2 - t1)[[3]] # okres czasu
time_elapsed
time_elapsed <- as.numeric((t2 - t1)[3]) # okres czasu
time_elapsed

# typ konwersji
#is.datatype() #zwraca TRUE or FALSE, gdzie datatype np. integer tzn. is.integer
#as.datatype() #konwertuje do typu danych, gdzie datatype np. integer tzn. is.integer

#sortuje ramkę danych, domyślnie rosnąco
# sortuj wiersze od najmłodszych do najstarszych
newdata <- patientdata[order(patientdata$age), ]

# sortuj wiersze osobno dla kobiet i mężczyzn, od najmłodszych do najstarszych
attach(patientdata)
newdata <- patientdata[order(diabetes, age),]
detach(patientdata)
newdata

# sortuj wiersze osobno dla kobiet i mężczyzn, od najstarszych do najmłodszych
attach(patientdata)
newdata <- patientdata[order(diabetes,-age),]
detach(patientdata)
newdata


#Łączenie danych: dodawanie kolumn
# połącz kolumnami patientdata i newdata po ID
total <- merge(patientdata, newdata, by = "patientID")
total
# połącz kolumnami patientdata i newdata po ID i Country
total <- merge(patientdata, newdata, by = c('patientID', 'diabetes'))
total
# połącz kolumnami patientdata i newdata muszą mieć tą samą ilość wierszy
total <- cbind(patientdata, newdata)
total

newdata$patientID <- newdata$patientID + 10
#Łączenie danych: dodawanie wierszy
# połącz dwie ramki z tą samą liczbą kolumn
total <- rbind(patientdata, newdata)
total

#wyodrębnianie podzbiorów
c1 <- c(1, 2, 3)
c2 <- c(4, 5, 6)
c3 <- c(7, 8, 9)
data <- data.frame(c1, c2, c3)
data[1]
data['c1']
x <- c('c1', 'c2')
data[x]
data[, 1]
data[, 1:2]
data[, c(1:2)]
data[1, ]
data[1:2, ]

#wyodrębnianie zmiennych z ramki (kolumn)
# usuń zmienne age, diabetes
myvars <- names(total) %in% c('age', 'diabetes')
myvars
newdata <- total[!myvars]
newdata
# usuń 2 i 3-ą kolumnę
newdata <- total[c(-2,-3)]
newdata
# usuń zmienne age, diabetes
total$age <- total$diabetes <- NULL
total

#wybieranie podzbiorów
patientdata[1:3, ]
patientdata[which(patientdata$status == 'Poor' &
                    patientdata$age < 30), ]
attach(patientdata)
patientdata[which(status == 'Poor' & age < 30), ]
detach(patientdata)

patientdata$date <- Sys.Date()
patientdata
patientdata$date <- as.Date(patientdata$date, '%m/%d/%y')
patientdata
startdate <- as.Date('2009-01-01')
enddate <- as.Date('2017-10-31')
patientdata[which(patientdata$date >= startdate &
                    patientdata$date <= enddate), ]

# use package dplyr (install first)
library(dplyr)
newdata <- filter(patientdata, status == 'Poor' & age < 30)
newdata

# subset()
subset(patientdata, age >= 35 | age < 24, select = c(age, status))
subset(patientdata, status == 'Poor' &
         age < 30, select = patientID:date)

#losowa próba sample
sample(1:nrow(patientdata), 3, replace = FALSE)
patientdata[sample(1:nrow(patientdata), 3, replace = FALSE),]

#używanie sqla
library(sqldf)
head(mtcars)
sqldf('select * from mtcars where carb=1 order by mpg', row.names = TRUE)
sqldf(
  'select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from mtcars where cyl in (4, 6) group by gear'
)

#konwersja pomiędzy łancuchem znaków i zmienną
# łańcuch do zmiennej
assign('test', 10) # taki sam efekt jak po: test <- 10
x <- 'test'
assign(x, 5) # taki sam efekt jak po: test <- 5

# nazwa zmiennej do łańcucha
x <- 5
var.name <- deparse(substitute(x)) # var.name równa się "x"

#Zaawansowana obsługa danych
#matematyczne funkcje: sqrt(x), floor(x), log(x), exp(x)
#statystyczne funkcje: mean(x), median(x), sd(x), var(x), range(x), sum(x), scale(x, center=TRUE, scale=TRUE)
#funkcje probabilistyczne:
#[dpqr]distribution_abbrebiation (d=density, p=distribution function, q=quantile function, r=random generation)
#runif()- próba o równomiernym rozkładzie
#set.seed(5): ustaw seed na jakąś jedną wartość np. 5, aby uzyskać te same wyniki
#fukcje znakowe:
#nchar(x), nzchar(x), substr(x, start, stop), grep(pattern, x, ignore.case=FALSE, fixed=FALSE)
#sub(pattern, replacement, x, ignore.case=FALSE, fixed=FALSE), strsplit(x, split, fixed=FALSE)
#paste(..., sep=""), toupper(x), tolower(x)
#inne funkcje:
#length(x), seq(from, to, by), rep(x, n), cut(x, n), pretty(x, n), cat(.., file='myfile', append=FALSE)
#uruchom funckje na macierzach i ramkach
#apply(x, MARGIN, FUN, ...)
#x to dane, MARGIN indeks rozmiaru, 1 oznacza wiersze, 2 oznacza kolumny, FUN jest funkcją, a  ... jej dodatkowymi argumentami

#kontrola przepływu danych
#if/else, ifelse, switch

score <- 0.6
if (score > 0.5) {
  outcome2 <- 'passed'
} else {
  outcome2 <- 'not passed'
}
outcome <- ifelse(score > 0.5, 'passed', 'not passed')
#switch(expr, ...)


#for, while
#for (var in seq) statement
#while (cond) statement

for (i in 1:10) {
  print('hello world')
}
# wypisze od 1 do 10
for (i in 1:10) {
  print(i)
}

# wypisze 10 razy 'witaj'
i <- 10
while (i >= 0) {
  print('witaj')
  i <- i - 1
}

#funkcje użytkownika
myfunction <- function(arg1, arg2, ...) {
  statements
  return(object)
}

mystats <- function(x,
                    parametric = TRUE,
                    print = FALSE) {
  if (parametric) {
    center <- mean(x)
    spread <- sd(x)
  } else {
    center <- median(x)
    spread <- mad(x)
  }
  if (print & parametric) {
    cat('Mean=', center, '\n', 'SD=', spread, '\n')
  } else if (print & !parametric) {
    cat('Median=', center, '\n', 'MAD=', spread, '\n')
  }
  result <- list(center = center, spread = spread)
  return(result)
}

#agregacja i zmiana struktur
#aggregate(x, by, FUN)
#x to obiekt danych, by - lista zmiennych, i FUN  funkcja agregacyjna
#reshape
#install.packages('reshape', depend=T)
library(reshape)
md <- melt(patientdata, id = (c('diabetes', 'status')))
md
cast(md, status ~ variable, mean)

dev.off()
#klasteryzacja w R np. K-means
mdata <- mtcars[c('disp', 'hp')]
kmeans.res <- kmeans(mdata, 3) # 3 zbiory odrębnych danych
plot(
  mdata,
  xaxt = 'n',
  yaxt = 'n',
  xlab = "X",
  ylab = "Y"
)
axis(1, pos = 0)
axis(2, pos = 0)
abline(v = 0, h = 0)
kmeans.cluster <- factor(kmeans.res$cluster)
# zainstaluj 'ade4', aby zwizualizować zbiory
library(ade4)
s.class(
  mdata,
  fac = kmeans.cluster,
  add.plot = TRUE,
  col = seq(1, nlevels(kmeans.cluster), 1)
)



#krok pierwszy
fn <- function (x) {
  ifelse(x > 46 & x < 52, 1, 0)
}
res <- fn(40:60)

fn <- function (x, y) {
  ifelse(x > 46 & x < 52 & y < 12, 1, 0)
}
datagrid <- expand.grid(i = 40:60, j = 0:20)
res <- fn(datagrid$i, datagrid$j)

#funkcje w różnych odmianach apply

fn <- function (x) {
  ifelse(x > 46 & x < 52, 1, 0)
}
res <- sapply(40:60, fn)

fn <- function (x, y) {
  ifelse(x > 46 & x < 52 & y < 12, 1, 0)
}
datagrid <- expand.grid(i = 40:60, j = 0:20)
res <- apply(datagrid, 1, function(z) {
  fn(z["i"], z["j"])
})

#zagnieżdżona pętla

res <- NULL
for (i in 40:60) {
  for (j in 0:20) {
    res <- c(res, fn(i, j))
  }
}
