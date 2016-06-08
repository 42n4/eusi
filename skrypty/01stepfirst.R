#########################################################################################################################
# MINI KURS R w pigułce (podstawy R, struktury danych, grupowania, klasyfikacje):
#########################################################################################################################
# Udostępnił i opracował na podstawie źródeł z Internetu: Piotr Wąsiewicz 
# Zasady ogólne:
# indeksy w R zaczynają się od 1, a nie od 0
# wielkość liter ma znaczenie
# w rstudio kursor myszy na nazwie funkcji i F1 wywołują opis funkcji
# Wszystkie przykłady trzeba wykonać po kolei, gdyż wykorzystują czasami dane utworzone wcześniej przez poprzednie przykłady.
# Na windowsie samo się instaluje, tylko trzeba zainstalować R for windows server:
# https://mran.microsoft.com/install/mro4mrs/8.0.0/MRO-3.2.2-for-RRE-8.0.0-Windows.exe
# ze strony https://mran.microsoft.com/download/mro-for-mrs/
# oraz rstudio:
# https://download1.rstudio.org/RStudio-0.99.902.exe
# ze strony https://www.rstudio.com/products/rstudio/download/
# Zamieszczone przykłady dadzą się uruchomić pod ms windowsem jeśli zainstalujecie wymienione wyżej programy
# (pod linuxem pakiety R najlepiej zainstalować w linuxie na roocie w konsoli R, żeby nie instalować na lokalnym koncie):
pkglist<-c("reshape","ade4","sqldf","plyr","dplyr")
pkglist<-c(pkglist,"party","rgl","scatterplot3d","fpc","pvclust","dendextend")
pkglist<-c(pkglist,"nFactors","FactoMineR","RRF","mclust","foreach","doParallel")
pkglist<-c(pkglist,"rpart","ipred","gbm","mda","klaR","kernlab","caret")
pkglist<-c(pkglist,"tseries","fUnitRoots","forecast","sets","TTR")
#pkglist<-c(pkglist,"MASS","RWeka")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#ZAKOMENTUJ jeśli chcesz zainstalować biblioteki pod linuxem w konsoli tekstowej R na koncie root
for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
#wczytanie wszystkich bibliotek - to konieczne, aby uniknąć błędów
for(i in pkglist) library(i, character.only = TRUE);
#########################################################################################################################
# Podstawowe informacje
# funkcja pomocy w R 
#help(kmeans)                       # opisuje funkcje kmeans
#?kmeans                            # opisuje funkcje kmeans
#help(pi)                           # opisuje stałą pi
getwd()                             # podaj aktualny roboczy katalog
setwd()                             # ustaw roboczy katalog
dir.create(foldername)              # utwórz katalog
source('file.R')                    # uruchom skrypt R
a <- 2; print(a)                    # użyj print() do wypisania czegokolwiek na konsolę
source('file.R', echo = TRUE)       # wypisz wszystkie komendy także na konsolę
i <- 10                             # wypisz litery i zmienne 
cat(i, "ta zmienna...\n", sep = "") # sep="" oznacza brak spacji miedzy argumentami w cat
#konwersja pomiędzy łancuchem znaków i zmienną
assign('test', 10)                  # taki sam efekt jak po: test <- 10
x <- 'test'                         # łańcuch do zmiennej
assign(x, 5)                        # taki sam efekt jak po: test <- 5
x <- 5                              # nazwa zmiennej do łańcucha poprzez deparse substitute
var.name <- deparse(substitute(x))  # var.name równa się "x"
vec <- c(1:10)                      # sprawdź strukturę danych w tym przypadku wektor
str(vec)                            # struktura zmiennej
head(vec)                           # podaj pierwszych 6 elementów
tail(vec)                           # podaj ostatnich 6 elementów
no <- c(1:3)
grade <- c(89, 95, 100)
data <- data.frame(no, grade)       # ramka danych złożona z wektorów
head(data)                          # pokaż 6 pierwszych wierszy
tail(data)                          # pokaż 6 ostatnich wierszy
a <- c(1, 2);class(a)               # typ zmiennej
# typ konwersji
#is.datatype()                      # zwraca TRUE or FALSE, gdzie datatype np. integer tzn. is.integer
#as.datatype()                      # konwertuje do typu danych, gdzie datatype np. integer tzn. is.integer
x <- c(1, 2); rm(x)                 # usuń zmienne z pamięci przestrzeni roboczej

# rm(list=ls(all=T))                # usuń wszystkie zmienne z pamięci przestrzeni roboczej
rm(list = ls(pattern = '^tmp'))     # usuń zmienne z nazwami zaczynającymi się na 'tmp'
# quit()                            # wyjdź z rstudia (zapyta czy zapisać przestrzeń roboczą)


#########################################################################################################################
# STRUKTURY DANYCH
#########################################################################################################################
# WEKTORY mają tylko jeden typ danych
# indeksy w R zaczynają się od 1, a nie od 0
a <- c(1, 2, 5, 3, 6,-2, 4)
a[3]                   # 5
a[c(1, 3, 5)]          # 1 5 6
a[2:6]                 # 2 5 3 6 -2
b <- replicate(10, 2)  # generuje wektor z długością 10, wszystkie elementy to 2
b <- rep(2, 10)        # generuje wektor z długością 10, wszystkie elementy to 2
b <- 1:10              # b równa się 1, 2, 3, 4, 5, 6, 7, 8, 9 10
b <- seq(1, 10)        # b równa się 1, 2, 3, 4, 5, 6, 7, 8, 9 10
b <- seq(1, 10, 2)     # b równa się 1, 3, 5, 7, 9
b <- seq(0, 1, 0.1)    # b równa się 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
b <- seq(from=0, to=1, by=0.1) # b równa się 0.0 0.1 0.2 0.3 0.4 0.5 0.6 0.7 0.8 0.9 1.0
e <- 10
a <- c(a, e)           # dodaj element e do wektora: metoda 1
a <- append(a, e)      # dodaj element do wektora: metoda 2  
a <- append(a, e, 2)   # dodaj e na pozycji 2+1 czyli 3
a <- append(a, e, 0)   # dodaj e na pozycji 0+1 czyli 1
a[length(a) + 1] <- e  # dodaj element do wektora: metoda 3
index <- 2             # usuń element z wektora, inicjalizacja indeksu
a <- a[-index]         # usuń drugi element z wektora
a[-1]                  # wypisz wektor bez pierwszego elementu
a[-length(a)]          # wypisz wektor bez ostatniego elementu
a[-c(2, 5)]            # usuń drugi i piąty element z wektora
a                      #c( 10,  1,    2,     10,   5,   3,     6,   -2,    4,    10,   10,   10)
a > 3                  # TRUE FALSE FALSE  TRUE  TRUE FALSE  TRUE FALSE  TRUE  TRUE  TRUE  TRUE      
a[a>3]                 # wypisz elementy wektora większe od 3
a[a==10]               # wypisz elementy wektora równe 10
a[a>1 & a < length(a)] # wypisz elementy wektora większe od 1, a mniejsze od długości a
s<-c(jeden=1,dwa=2,trzy=3)# wektor z nazwanymi elementami
names(s)               #nazwy elementów
s[["trzy"]]            #wybiera element o nazwie "trzy"
# znajdź indeks pierwszego pasującego elementu
# dla przykładu, znajdź indeks 10-tki w wektorze vec <- c(1, 10, 2, 10).
# match(10, vec) zwróci 2, następny indeks dla 10 nie będzie zwracany
vec <- c(10, 2:10)
e  <- 10
e2 <- c(10, 5)
match(e, vec)          #podaje na którym miejscu znajduje się pierwsze wystąpienie e  w wektorze vec
which(vec %in% e)      #podaje na którym miejscu znajduje się e w wektorze vec we wszystkich wystąpieniach
match(e2, vec)         #podaje na którym miejscu znajduje się pierwsze wystąpienie e2 w wektorze vec
which(vec %in% e2)     #podaje na którym miejscu znajduje się e2 w wektorze vec we wszystkich wystąpieniach
e3 <- 1                # sprawdź, czy element znajduje się w wektorze
!is.na(match(e3, vec)) #czy element e3 znajduje się w wektorze vec
e3 %in% vec            #czy element e3 znajduje się w wektorze vec 
c1 <- c(1, 2, 3)
c2 <- c(2, 3, 5)
c1[!(c1 %in% c2)]      # 1 - elementy z wektoru c1 nie znajdujące się w c2
setdiff(c1, c2)        # 1 - druga metoda
c2[!(c2 %in% c1)]      # 5 - elementy z wektoru c2 nie znajdujące się w c1
setdiff(c2, c1)        # 5
vec <- c(1, 2, 3, 2)   # oblicz ile jest nie powtarzających się numerów
nlevels(factor(vec))   # zwraca 3
length(unique(vec))    # zwraca 3
# operatory są dostosowane do wektorów - element z elementem na tym samym miejscu
c(1,3,5) + c(5,3,1)    #-> 6,6,6
c(1,3,5) - c(5,3,1)    #-> -4,0,4
c(1,3,5) * c(5,3,1)    #-> 5,9,5
c(2)     * c(5,3,1)    #-> 10,6,2
c(1,3,5) / c(5,3,1)    #-> 0.2,1.0,5.0
c(1,3,5)%/%c(5,3,1)    #-> 0,1,5  dzielenie całkowite
c(1,3,5) %%c(5,3,1)    #-> 1,0,0  mod - reszta całkowita z dzielenia
c(1,3,5) ^ c(5,3,1)    #-> 1,27,5 podnoszenie do potęgi
c(1,3,5) **c(5,3,1)    #-> 1,27,5 podnoszenie do potęgi
c(1,3,5)%in%c(5,3,1)   #-> TRUE,TRUE,TRUE
# obliczenia na wektorach - element * element i ich suma
c(1,3,5) %*% c(5,3,1)  #-> 19
sort(a)                #sortowanie elementów wektora
sort(a, decreasing = TRUE) 
x <- 'abc'
numc <- nchar(x)       # liczba znaków w ciągu (łańcuchu znaków)
numc
# znajdź pozycję znaku w ciągu
# \" jest pojedyńczym znakiem, loc jest listą
loc <- gregexpr(pattern = '\"', "abc\"defg") 
cat('Pozycja znaku: ', loc[[1]][1], '\n')
# konwersja łańcucha znaków do całkowitej
# 1 sposób
x <- 123
x <- paste(x)          # x równa się "123"
x <- strtoi(x)         # x równa się 123
# 2 sposób
x <- 123
x <- as.character(x)   # x równa się "123"
x <- as.integer(x)     # x równa się 123, także as.numeric(x) jako zmiennoprzecinkowa
#uwaga! 
c(5,'a'); e<-c(5)      # się konwertuje domyślnie na c('5','a')
e[2]<-'a'; e           # się konwertuje domyślnie na c('5','a')
typeof(1:2) == typeof(c(1,2))           # FALSE pierwszy typ integer - drugie double
for( i in 1:length(c()))        print(i)# 1 0 niby pusty wektor, a może coś wypisać 
for( i in seq_len(length(c()))) print(i)# poprawna forma pętli odporna na błąd pustego wektora
for( i in seq_along(c()))       print(i)# poprawna forma pętli odporna na błąd pustego wektora


#########################################################################################################################
#MACIERZ: dwuwymiarowa tablica tego samego typu zmiennych
# użyj indeksów macierzy 
x <- matrix(1:16, nrow = 4)
x
x[2, ]                 #drugi wiersz
x[, 3]                 #trzecia kolumna
x[1, 4]                #pole z 1 wiersza i 4 kolumny
x[1, c(3, 4)]          #pola z 1 wiersza i 3 oraz 4 kolumny
nrow(x)                #liczba wierszy macierzy
ncol(x)                #liczba kolumn macierzy
dim(x)                 #wymiary macierzy c(nrow(x),ncol(x))
length(x)              #nrow(x)*ncol(x)
rowMeans(x)            #średnie liczone po wierszach
colMeans(x)            #średnie liczone po kolumnach
rowSums(x)             #sumy liczone po wierszach
colSums(x)             #sumy liczone po kolumnach
t(x)                   #transponowana macierz x
det(x)                 #wyznacznik macierzy w tym przypadku 0 - macierz sosbliwa
#tworzenie kolumny 4x1 z wektora i nazwanej macierzy 2x2
cells <- c(1, 6, 4, 8)
cells
matrix(cells)          # kolumnowy, pionowy wektor
t(t(cells))            # transponowany dwa razy wektor to to samo co kolumnowy wektor
rnames <- c('R1', 'R2')
cnames <- c('C1', 'C2')
colmatrix<-matrix(     # wypełnij macierz po kolumnach, to domyślne ustawienie
  cells,
  nrow = 2,
  ncol = 2,
  byrow = FALSE,
  dimnames = list(rnames, cnames)
)
colmatrix
rownames(colmatrix)    #nazwy wierszy
colnames(colmatrix)    #nazwy kolumn
c(colmatrix)           #konwersja do wektora spowrotem do oryginalnej postaci wektora cells
rowmatrix<-matrix(     # wypełnij macierz po wierszach 
  cells,
  nrow = 2,
  ncol = 2,
  byrow = TRUE,
  dimnames = list(rnames, cnames)
)
rowmatrix              #wychodzi to samo co t(colmatrix) - transponowana macierz colmatrix
c(rowmatrix)           #konwersja do wektora, ale innego niż początkowy wektor cells
# macierzowe mnożenie np.: iloczyn pierwszego wiersza i kolumny pierwszej 
# to wymnożenie ich elementów i suma iloczynów
# efektem jest element c(1,1) nowej wynikowej macierzy, dla 1 wiersza i 2 kolumny - c(1,2)
colmatrix %*% rowmatrix #mnożenie macierzy: wierszy pierwszej przez kolumny drugiej 
#wynik to np. macierz M o rozmiarach 2x2, jeśli wejściowe są też 2x2
#M[1,1]=colmatrix[1,] %*% rowmatrix[,1] M[1,2]=colmatrix[1,] %*% rowmatrix[,2]
#M[2,1]=colmatrix[2,] %*% rowmatrix[,1] M[2,2]=colmatrix[2,] %*% rowmatrix[,2] 
colmatrix *   rowmatrix #mnożenie element colmatrix[i,j] z odpowiadającym elementem rowmatrix[i,j]
crossprod(colmatrix,rowmatrix)   #mnożenie macierzy transponowanej przez zwykłą t(colmatrix) %*% rowmatrix
tcrossprod(colmatrix,rowmatrix)  #mnożenie macierzy zwykłej przez transponowaną colmatrix %*% t(rowmatrix)
solve(colmatrix)                 #odwrócona macierz
solve(colmatrix,rowmatrix)       #rozwiązanie X równania colmatrix %*% X = rowmatrix
solve(rowmatrix,colmatrix)       #rozwiązanie X równania rowmatrix %*% X = colmatrix
solve(rowmatrix,rowmatrix)       #przekształcenie na tą samą macierz poprzez macierz diagonalną identycznościowa X=diag(2)
diag(2)                          #macierz diagonalna identycznościowa
det(colmatrix %*% rowmatrix)     #det(A · B) = detA · detB
det(colmatrix) %*% det(rowmatrix)#det(A · B) = detA · detB


#########################################################################################################################
#TABLICA ARRAY: podobna do macierzy matrix, ale może mieć więcej wymiarów
dim1 <- c('A1', 'A2')
dim2 <- c('B1', 'B2', 'B3')
dim3 <- c('C1', 'C2', 'C3', 'C4')
z <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
z
z[1, 2, 3] #wartość komórki z jednego pola trójwymiarowej macierzy


#########################################################################################################################
#LISTA: uporządkowany zbiór obiektów o możliwych różnych typach
g <- 'Moja pierwsza lista'
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c('jeden', 'dwa', 'trzy')
mlist <- list(tytul = g, wiek = h, j, k)
mlist
mlist[[2]]             #drugi element listy wiek jako wektor
mlist[['wiek']]        #element wiek, w tym przypadku wektor
mlist$wiek             #element wiek, w tym przypadku wektor
mlist['wiek']          #element wiek, w tym przypadku lista (nie używać, kiedy potrzebny wektor)
mlist[2]               #element wiek, w tym przypadku lista (nie używać, kiedy potrzebny wektor)
typeof(mlist[['wiek']])#typ double  
typeof(mlist['wiek'])  #typ lista
mlist[[2]][[1]]        #25, pierwszy element drugiego argumentu wektora wiek z listy mlist
as.list(h)             #konwersja do listy
nlist<-as.list(h)       
list(mlist,nlist)      #lista dwóch list
plist<-list(mlist,nlist)
print(plist)           #wypisz listę dwóch list
str(plist)             #struktura listy dwóch list
dput(plist)            #kod w R listy dwóch list
class(plist)           #typ struktury danych - ista
#cat(plist)            uwaga! cat z listą nie działa!
#nadpisuje wartości dwóch elementów listy o nazwach tytul i wiek
mlist[names(mlist) %in% c('tytul','wiek')]<-c('Nadpisany element listy',list(c(2,4,6,7)))
lapply(mlist,FUN=length)#zastosuj funkcję length do każdego elementu listy mlist i zwróć LISTĘ długości argumentów
sapply(mlist,FUN=length)#zastosuj funkcję length do każdego elementu listy mlist i zwróć WEKTOR długości argumentów
ylist<-list(a=1, b=3, c=5, d=6)
sapply(ylist, FUN=function(x,p) x^p, p=2)   #wynik potęgi do p=2 wynik to wektor 1,9,25,36
sapply(ylist, FUN=function(x,p) x^p, p=2:3) #wynik potęgi do p=2 wynik to macierz 2 wierszowa 1,9,25,36 i 1,27,125,216


#########################################################################################################################
#RAMKA DANYCH: kolumny z różnymi typami, odpowiednik tabeli w bazie lub arkusza w excelu
pacjent_id <- c(1, 2, 3, 4)
wiek <- c(25, 34, 28, 52)
typ <- c('Typ1', 'Typ2', 'Typ1', 'Typ1')
stan <- c('Kiepski', 'Poprawa', 'Wybitny', 'Kiepski')
pacjenci <- data.frame(pacjent_id, wiek, typ, stan)
nrow(pacjenci)         # ilość wierszy
ncol(pacjenci)         # ilość kolumn
dim(pacjenci)          # rozmiar to wektor z liczbą wierszy i kolumn c(nrow(pacjenci),ncol(pacjenci))
rownames(pacjenci) <- seq_len(nrow(pacjenci)) #na wszelki wypadek nazywa wiersze ich indeksami
# podaj wiersz lub kolumnę ramki danych
i <- 1; j <- 2
pacjenci[i,]           # i-ty wiersz jako ramka danych
pacjenci[, j]          # j-ta kolumna jako wektor
pacjenci[, 'wiek']     # kolumna 'wiek' jako wektor
pacjenci[['wiek']]     # kolumna 'wiek' jako wektor
pacjenci$wiek          # kolumna 'wiek' jako wektor
pacjenci[j]            # j-ta kolumna jako ramka danych
pacjenci['wiek']       # kolumna 'wiek' jako ramka danych
pacjenci[1:2]          # pierwsze dwie kolumny jako ramka danych
pacjenci[c('typ', 'stan')]
# podaj ij-ty element ramki danych
as.integer(pacjenci[i,][j])# pacjenci[i, ][j] i-ty wiersz jta kolumna jako integer
pacjenci[i, j]         # komórka z i tego wiersza i j-tej kolumny
pacjenci[[i, j]]       # komórka z i tego wiersza i j-tej kolumny
pacjenci[[j]][i]       # komórka z i tego wiersza i j-tej kolumny
pacjenci[, j][i]       # komórka z i tego wiersza i j-tej kolumny
pacjenci$wiek[i]       # komórka z i tego wiersza i kolumny wiek 
pacjenci[i, 'wiek']    # i-ty wiersz kolumny wiek
pacjenci[i, i:j]       # dwie komórki z i tego wiersza i oraz i-tej i j-tej kolumny, to NIE działa na [[i, i:j]]
# usuń wiersz wskazany indeksem
index <- 2
pacjenci[-index,]      # usuń 2 wiersz z ramki danych
#Wybieranie podzbiorów
pacjenci[1:3, ]        # trzy pierwsze wiersze - pacjenci
pacjenci[which(pacjenci$stan == 'Kiepski' & pacjenci$wiek < 30), ] #pacjenci stan kiepski i wiek poniżej 30
library(plyr); library(dplyr) # use package dplyr (install first)
filter(pacjenci, stan == 'Kiepski' & wiek < 30) # subset()
subset(pacjenci, wiek >= 35 | wiek < 24, select = c(wiek, stan))
subset(pacjenci, stan == 'Kiepski' & wiek < 30, select = pacjent_id:date)
#Sortowanie
pacjenci[order(pacjenci$wiek), ]             # sortuj wiersze od najmłodszych do najstarszych, domyślnie rosnąco
attach(pacjenci)
spacjenci <- pacjenci[rev(order(typ, wiek)),]# sortuj wiersze w porządku malejącym 'rev' po typie, od najstarszych do najmłodszych
detach(pacjenci)
spacjenci
attach(pacjenci)
spacjenci <- pacjenci[order(typ,-wiek),]     # sortuj wiersze po typie, od najstarszych do najmłodszych
detach(pacjenci)
spacjenci
#Łączenie danych: dodawanie wierszy
new_row <-                                   # nowy wiersz
  data.frame(
    pacjent_id = 5,
    wiek = 10,
    typ = 'Typ3',
    stan = 'Zdrowy'
  )
pacjenci <- rbind(pacjenci, new_row)         # RBIND dodaje nowy wiersz do ramki danych
spacjenci<- rbind(spacjenci, new_row)        # to samo z kopią posortowanych - spacjenci
spacjenci$pacjent_id <- spacjenci$pacjent_id + 10#inne identyfikatory w spacjenci
rbind(pacjenci, spacjenci)                   # połącz dwie ramki z tą samą liczbą kolumn
#Łączenie danych: dodawanie kolumn 
pacjenci$new_col <- c(2:6)                   # dodaj kolumnę do ramki danych: metoda 1
pacjenci$new_col <- NULL                     # usuń kolumnę z ramki danych: metoda 1
pacjenci <- transform(pacjenci, new_col = c(2:6))# dodaj kolumnę do ramki danych: metoda 2
pacjenci <- within(pacjenci, {new_col = NULL})# usuń kolumnę z ramki danych: metoda 3
merge(pacjenci, spacjenci, by = "pacjent_id")# połącz kolumnami pacjenci i spacjenci po ID
merge(pacjenci, spacjenci, 
      by = c('pacjent_id', 'typ'))           # połącz kolumnami pacjenci i spacjenci po ID i Country
cbind(pacjenci, spacjenci)                   # CBIND połącz kolumnami pacjenci i spacjenci muszą mieć tą samą ilość wierszy
#Usuwanie kolumn
myvars <- names(spacjenci) %in% c('wiek', 'typ')# wyodrębnianie zmiennych (kolumn) wiek, typ z ramki 
myvars
spacjenci[!myvars]                           # usuń zmienne wiek, typ
spacjenci$wiek <- spacjenci$typ <- NULL      # usuń zmienne wiek, typ
spacjenci[c(-2,-3)]                          # usuń 2 i 3-ą kolumnę
#Dodawanie dat
#Sys.Date() zwraca dzisiejszą datę w postaci obiektu Date, date() zwraca datę i czas w postaci łańcucha znaków
pacjenci$date <- Sys.Date()
pacjenci
startdate <- as.Date('2009-01-01')
enddate <- as.Date('2017-10-31')
pacjenci[which(pacjenci$date >= startdate & pacjenci$date <= enddate), ]
subset(pacjenci,date >= startdate & date <= enddate)
#Losowa próba sample
sample(1:nrow(pacjenci), 3, replace = FALSE)
pacjenci[sample(1:nrow(pacjenci), 3, replace = FALSE),]
# Dodawanie nowych zmiennych do ramki danych
# mamy trzy metody
df <- data.frame(x1 = c(2, 2, 6, 4), x2 = c(3, 4, 2, 8))
# metoda 1
df$sumx <-  df$x1 + df$x2
df$meanx <- (df$x1 + df$x2) / 2
# metoda 2
attach(df)
df$sumx <-  x1 + x2
df$meanx <- (x1 + x2) / 2
df
detach(df)
# metoda 3
transform(df, sumx = x1 + x2, meanx = (x1 + x2) / 2)
# metoda 4
with(df, {                                   # 'with' nic nie zwraca
wiek[x1 == 2] <- 1                           # reszta jest wypełniana NA (brakiem danych z R)
})
df
df$wiek <- NULL                              # usuń wiek
# metoda 5
df <- within(df, {                           # 'within' zwraca df
  wiek <- NA                                 # utwórz nową zmienną wiek i zainicjalizuj ją NA (brakiem danych z R)
  wiek[x1 == 2] <- 1
})
df
#Zmiana nazw
#rename(dataframe, c(oldname1="newname1", oldname2="newname2",...))
#library(reshape)
df <- rename(df, c(sumx = "suma"))
df
names(df)[4] <- "srednia"                    # names(df) zwraca wektor nazw zmiennych lub użyj fix(df) aby zmienić nazwy w gui
#Manipulacja NA
df$wiek[df$wiek == 1] <- NA                  # zamienia 1 na NA w kolumnie wiek
df
df$wiek[is.na(df$wiek)] <- 55                # zamienia NA na 55 w kolumnie wiek
df$wiek <-
  ifelse(is.na(df$wiek), 55, df$wiek)        # też zamienia NA na 55 w kolumnie wiek
df
x <- c(1, 2, NA, 3)                          # wektor z NA
y <- x[1] + x[2] + x[3] + x[4]               # y równa się NA
z <- sum(x)                                  # y równa się NA
z <- sum(x, na.rm = TRUE)                    # na.rm=TRUE usuwa wiersze z brakującymi danymi czyli NA
df$wiek[df$suma == 6] <- NA
df
na.omit(df)                                  # na.omit() usuwa wiersze z NA
df[!is.na(df$wiek),]                         # też usuwa wiersze z NA

#########################################################################################################################
#FAKTOR - etykiety, zmienna jakościowa (niemierzalna), czynnikowa: dyskretne lub porządkowe dane
#zmienne jakościowe (niemierzalne) – np. kolor oczu, płeć, grupa krwi
#porządkowe (quasi-ilościowe) – np. klasyfikacja wzrostu: (niski, średni, wysoki)
#skokowe (dyskretne) – np. ilość posiadanych dzieci, ilość gospodarstw domowych, wiek (w rozumieniu ilości skończonych lat)
#mapa wektorów dyskretnych wartości [1...k]
#nie można faktorów dodawać, mnożyć
#nie działa operator $, używa się pojedyńczych [] z indeksem np. levels(x)[1]
typ <- c('Typ1', 'Typ2', 'Typ1', 'Typ1')
typ <- factor(typ)                           # Levels: Typ1 Typ2
typ
stan <- c('Kiepski', 'Poprawa', 'Wybitny', 'Kiepski')
stan <-
  factor(stan, ordered = TRUE)               # Wybitny-3 Poprawa-2 Kiepski-1
stan
levels(stan)                                 #pokazuje poziomy dyskretnej zmiennej stan
stan2 <-
factor(stan,
levels = c('Wybitny', 'Poprawa', 'Kiepski')) # Wybitny-1 Poprawa-2 Kiepski-3
stan2
levels(stan2)                                #pokazuje poziomy dyskretnej zmiennej stan2, odwrotnie uporządkowane niż stan
stan2 <- factor(c(as.character(stan2), 'Zalosny')) #dodaje nowy stan nie zdefiniowany
levels(stan2)                                #widać w poziomach nowy stan
#Dyskretyzacja zmiennej do poziomów
i<-1:50+rnorm(50,0,5); i                     #zmienna i
k<-cut(i,5); k                               #generuje ze zmiennej i pięć poziomów zmiennej dyskretnej k
levels(k)<-seq_len(length(levels(k)))        #zmienia nazwy poziomów na bardziej czytelne
levels(k)
# przykład pokazujący faktory - zmienne jakościowe w ramce danych
# definiują automatycznie przy tworzeniu ramki danych, jednak porządek będzie alfabetyczny
pacjenci
str(pacjenci)
summary(pacjenci)
table(pacjenci$typ, pacjenci$stan)           #wygeneruj statystyki przecięcia dwóch kolumn
#Liczenie średnich po kolumnach i ich złączeniach
#library(reshape)
#melt i cast lub w jednym recast, ddply, aggregate czyli 4 METODY agregacji
library(reshape)
pacjenci$date <- NULL
# dla każdej wartosci pary typ, stan wypisz w jednej kolumnie variable inne kolumny i ich wartości
md <- melt(pacjenci, id = (c('typ', 'stan'))) 
md
cast(md, stan ~ variable, mean)              #policz srednie dla stanów po wartościach z variable
cast(md, typ ~ variable, mean)               #policz srednie dla typ po wartościach z variable
cast(md, typ + stan ~ variable, mean)        #policz srednie dla typ i stan po wartościach z variable
recast(pacjenci, typ + stan ~ variable, 
       mean, id.var = c('typ', 'stan'))      #policz srednie dla typ i stan po wartościach z variable w jednym kroku
ddply(pacjenci, ~typ + stan, 
      summarise, N=length(wiek), 
      sredniaid=mean(pacjent_id),
      sredniawiek=mean(wiek))                #policz srednie dla typ i stan po innych parametrach w jednym kroku
ddply(pacjenci, .(typ,stan), 
      summarise, N=length(wiek), 
      sredniaid=mean(pacjent_id),
      sredniawiek=mean(wiek))                #policz srednie dla typ i stan po innych parametrach w jednym kroku
aggregate(.~stan+typ,data=pacjenci,mean)     #policz srednie dla typ i stan po innych parametrach w jednym kroku


#########################################################################################################################
#RYSUNEK FUNKCJI
f <- function(x) {
  x * sin(x)
}
plot(f,-20 * pi, 20 * pi)
plot(f,-20 * pi, 20 * pi)
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
#narysuj wiele rysunków w tym samym czasie
plot(f)                                      # pierwszy rysunek
dev.new()                                    # otwórz drugi rysunek
plot(f(x2plot/20))                           # przelączanie strzałkami z klawiatury
Sys.sleep(2)                                 #pauza na 2 sekundy
dev.off()                                    # zamknij drugi rysunek
# attach, detach
# attach, detach nie pracują na tych samych nazwach zmiennych, użyj "with"
summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)
attach(mtcars)                               # dodaj zbiór danych do ścieżki R wyszukiwania
summary(mpg)
plot(mpg, disp)
plot(mpg, wt)
detach(mtcars)                               # usuń zbiór danych do ścieżki R wyszukiwania
Sys.sleep(2)                                 #pauza na 2 sekundy

#używanie sqla
library(sqldf)
head(mtcars)
sqldf('select * from mtcars where carb=1 order by mpg', row.names = TRUE)
sqldf(
  'select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from mtcars where cyl in (4, 6) group by gear'
)


#########################################################################################################################
#ZAPIS DO PLIKÓW
#exportuj dane do pliku csv
write.table(
  data,
  'data.csv',
  row.names = F,
  col.names = F,
  sep = ','
)
#wstawianie danych z pliku csv
signal <- read.table('data.csv', header = FALSE, sep = ',')
#RYSUNKI DO PLIKÓW
# pierwszy rysunek plik
pdf('plotgraph1.pdf')
x2plot = seq(1:20)
plot(sin(x2plot))
dev.off()
# drugi rysunek plik
pdf('plotgraph2.pdf')
plot(cos(x2plot))
dev.off()


#DATA I CZAS
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
# zapisz czas programu
t1 <- proc.time()
for (i in 1:1000) {
  cat('.')
}
t2 <- proc.time()
time_elapsed <- (t2 - t1)[[3]]               # okres czasu
time_elapsed
time_elapsed <- as.numeric((t2 - t1)[3])     #okres czasu
time_elapsed

#########################################################################################################################
#PROGRAMOWANIE, PĘTLE, FUNKCJE
#kontrola przepływu danych
#if/else, ifelse, switch
score <- 0.6
if (score > 0.5) {
  outcome2 <- 'passed'
} else {
  outcome2 <- 'not passed'
}
outcome <- ifelse(score > 0.5, 'passed', 'not passed')
#FOR, WHILE, SAPPLY
for (i in 1:10) {                            #for (var in seq) statement
  print('witaj')                             #wypisze 10 razy 'witaj'
}
sapply(1:10, function(i) {                   #sapply - alternatywa dla for - wypisze 10 razy 'witaj'
  print('witaj')                             #wypisze 10 razy 'witaj'
})
for (i in 1:10) {                            #pętla for wypisze od 1 do 10
  print(i)
}
sapply(1:10,function(i){print(i)})           #sapply - alternatywa dla for - wypisze od 1 do 10
i <- 10                                      #while (cond) statement - zaczyna od i=10
while (i >= 0) {                             #wypisze 10 razy 'witaj'
  print('witaj')
  i <- i - 1
}
for(i in seq_len(nrow(pacjenci)))            #odporna na pustą ramkę pętla z seq_len(nrow)
  print(pacjenci$wiek[i])
#FUNKCJE
#mojafunkcja <- function(arg1, arg2, ...) {
#  instrukcje
#  return(obiekt)
#  lub wystarczy do zwrócenia wartości
#  obiekt
#}
#bardziej zaawansowana funkcja
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
    cat('Mean=', center, '\n', 'SD= ', spread, '\n')
  } else if (print & !parametric) {
    cat('Median=', center, '\n', 'MAD = ', spread, '\n')
  }
  result <- list(center = center, spread = spread)
  return(result)
}
mystats(pacjenci$wiek,TRUE,TRUE)
mystats(pacjenci$wiek,FALSE,TRUE)
#SAPPLY, APPLY - funkcje argumenty w różnych odmianach apply
fn <- function (x) {                         #funkcja z jednym argumentem
  ifelse(x > 46 & x < 52, 1, 0)
}
fn(40:60)                                    #wystarczy do funkcji wrzucić wektor
sapply(40:60, fn)                            #to samo co wywołanie funkcji z wektorem na wejściu
fn <- function (x, y) {                      #funkcja z dwoma argumentami
  ifelse(x > 46 & x < 52 & y < 12, 1, 0)     
}
datagrid <- expand.grid(i = 40:60, j = 0:20) #utwórz wszystkie kombinacje wartości i, j; najpierw łączymy j=0 z wszystkimi wartościami i 
#fn(datagrid$i, datagrid$j)                  #TO NIE DZIAŁA w przypadku dwóch argumentów
apply(datagrid, 1, function(z) {             #odpowiednik dwóch pętli pierwszej z j, drugiej z i
  fn(z["i"], z["j"])                         #apply działa na macierzy, dla 1 (drugi argument) na wierszach, dla 2 na kolumnach
})
apply(datagrid,1,function(z){fn(z[1], z[2])})#skrócona wersja z indeksami, 1 oznacza pobór wierszy do funkcji fn
res <- NULL                                  #zagnieżdżona pętla w pętli - odpowiednik apply na datagrid
for (j in 0:20) {
  for (i in 40:60) {                         #utwórz wszystkie kombinacje wartości i, j w zagnieżdżonej pętli w pętli
    res <- c(res, fn(i, j))                  #expand.grid(i = 40:60, j = 0:20) najpierw łączymy j=0 z wszystkimi wartościami i 
  }
}
res

#Zaawansowane przetwarzanie danych
#matematyczne funkcje: sqrt(x), floor(x), log(x), exp(x)
#statystyczne funkcje: mean(x), median(x), sd(x), var(x), range(x), sum(x), scale(x, center=TRUE, scale=TRUE)
#funkcje probabilistyczne:
#[dpqr]distribution_abbrebiation (d=density, p=distribution function, q=quantile function, r=random generation)
#runif()- próba o równomiernym rozkładzie (random generation)
#set.seed(5): ustaw seed na jakąś jedną wartość np. 5, aby uzyskać te same wyniki
#fukcje znakowe:
#nchar(x), nzchar(x), substr(x, start, stop), grep(pattern, x, ignore.case=FALSE, fixed=FALSE)
#sub(pattern, replacement, x, ignore.case=FALSE, fixed=FALSE), strsplit(x, split, fixed=FALSE)
#paste(..., sep=""), toupper(x), tolower(x)
#inne funkcje:
#length(x), seq(from, to, by), rep(x, n), cut(x, n), pretty(x, n), cat(.., file='myfile', append=FALSE)


#########################################################################################################################
#UCZENIE SIĘ MASZYN - ODZYSKIWANIE WIEDZY
#########################################################################################################################
#https://en.wikibooks.org/wiki/Data_Mining_Algorithms_In_R
#########################################################################################################################

set.seed(12459);                             # początkowa wartość random seed dla takich samych wyników za każdym razem
dev.off()                                    # na wszelki wypadek wyłączamy drugi rysunek

#########################################################################################################################
#ANALIZA DANYCH - korelacje i podobieństwa
#Wybór danych do grupowania i klasyfikacji brakujących etykiet lub testowych zbiorów
#tu wybieramy inne dane dla lepszych i bardziej powtarzalnych wyników
#najpierw wypisujemy dostępne przykładowe dane poleceniem:
#data()
#potem odpytujemy pomoc poleceniem
#?nazwa_danych
#wybieramy:
library(RRF)
ndata <- "imports85"
data(list=ndata)
dane<-get(ndata)
dane<-dane[sapply(dane, is.numeric)]         # pobierz tylko dane liczbowe
dane$normalizedLosses<-NULL                  # usuń nieprzydatne wiersze
dane$symbolic<-NULL
dane <- na.omit(dane)                        # usuń niepełne wiersze z NA
rownames(dane) <- seq_len(nrow(dane))        # nazywa wiersze ich indeksami
#scale(x) oznacza (x - mean(x)) / sd(x) ważne kiedy dane w kolumnach mają różne bardzo dziedziny w rodzaju (0,1) oraz (0,10000)
dane_scaled <- dane
dane_scaled[sapply(dane, is.numeric)] <- scale(dane_scaled[sapply(dane, is.numeric)])

dane$grupa<-NULL
cor(dane)                                    # korelacje między zmiennymi
round(cor(dane), 2)                          # sprawdzamy korelację wybranych kolumn, widać dużą korelację
                                             # tzn. wybrane parametry razem się zmniejszają lub zwiększają
                                             # jeśli korelacja jest DODATNIA
                                             # jeśli jest UJEMNA, to przy zwiększaniu jednej, druga maleje
#image(cor(dane))
#heatmap z odległości między wierszami
distMatrix <- as.matrix(dist(dane_scaled))
heatmap(distMatrix)
Sys.sleep(2)                                 #pauza na 2 sekundy
#heatmap z korelacji
#https://planspacedotorg.wordpress.com/2013/07/24/clustered-r-squared-heat-maps-in-r/
dissimilarity <- 1 - cor(dane)^2             #miara niepodobnych 1 - korelacja do kwadratu
clustering <- hclust(as.dist(dissimilarity), method="ward.D2")
plot(clustering)                             #grupowanie po niepodobieństwach
order <- clustering$order
oldpar <- par(no.readonly=TRUE); par(mar=c(0,0,0,0))
image(dissimilarity[order, rev(order)], axes=FALSE)
par(oldpar)
clusterRsquared <- function(dataframe) {     #funkcja z miar niepodobieństw
  dissimilarity <- 1 - cor(dataframe)^2
  clustering <- hclust(as.dist(dissimilarity))
  order <- clustering$order
  oldpar <- par(no.readonly=TRUE); par(mar=c(0,0,0,0))
  image(dissimilarity[order, rev(order)], axes=FALSE)
  par(oldpar)
  return(1 - dissimilarity[order, order])
}
round(clusterRsquared(dane),2)
#round(clusterRsquared(dane3kol),2)
#Sys.sleep(2)                                #pauza na 2 sekundy


#########################################################################################################################
#GRUPOWANIE czyli KLASTERYZACJA
#https://cran.r-project.org/web/views/Cluster.html
#Grupowanie klasteryzacja w R np. K-means w dwóch wymiarach
dane2kol <- dane[c('engineSize', 'horsepower')]#wybieramy 2 parametry mtcars pojemność silnika i konie mechaniczne
round(cor(dane2kol), 2)                      #sprawdzamy korelację wybranych kolumn, widać dużą korelację 
grupowanie_kmeans <- kmeans(dane2kol, 3)     #3 zbiory odrębnych danych
plot(                                        #wizualizacja w 2D z plot, abline, ade4 s.class
  dane2kol,
  xaxt = 'n',
  yaxt = 'n',
  xlab = "X",
  ylab = "Y"
)
axis(1, pos = 0)
axis(2, pos = 0)
abline(v = 0, h = 0)
grupowanie_kmeans_cluster <- factor(grupowanie_kmeans$cluster)
# zainstaluj 'ade4', aby zwizualizować zbiory
library(ade4)
s.class(
  dane2kol,
  fac = grupowanie_kmeans_cluster,
  add.plot = TRUE,
  col = seq(1, nlevels(grupowanie_kmeans_cluster), 1)
)
aggregate(dane2kol,by=list(grupowanie_kmeans$cluster),FUN=mean) # średnie w grupach widać zróżnicowanie
groupk2 <- data.frame(dane2kol, grupowanie_kmeans$cluster) 
Sys.sleep(2)                                 #pauza na 2 sekundy
library(cluster)
clusplot(dane2kol, grupowanie_kmeans$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
Sys.sleep(2)                                 #pauza na 2 sekundy
library(fpc)
plotcluster(dane2kol, grupowanie_kmeans$cluster) 
#library(fpc)                                # porównanie dwóch grupowań
#cluster.stats(dane, fit$cluster, fit2$cluster) 
Sys.sleep(2)                                 #pauza na 2 sekundy

#Grupowanie klasteryzacja w R : określanie ilości grup
#http://www.statmethods.net/advstats/cluster.html
wss <- (nrow(dane)-1)*sum(apply(dane,2,var)) #oceń liczbę grup (klasterów)
for (i in 2:15) wss[i] <- sum(kmeans(dane,centers=i)$withinss)
#$betweenss: suma kwadratów odległośći między clusterami. 
#To jest średnia dystansów pomiędzy centrami klasterów
#Jeśli chcemy osobno leżące klustery, wartość betweenss musi być jak największa.
#$withinss: to jest suma odległości pomiędzy węzłami w klastrze. 
#Jeśli chcemy osobno leżące klustery, wartość withinss musi być jak najmniejsza.
#$tot.withinss = sum ( $withinss )
#$totss = $tot.withinss + $betweenss
plot(1:15, wss, type="b", xlab="Liczba grup", ylab="Suma wss")
library(fpc)
pamk(dane_scaled)$nc                         # liczba grup (klasterów) policzona automatycznie
pamk(dane)$nc                                # liczba grup (klasterów) policzona automatycznie
Sys.sleep(2)                                 # pauza na 2 sekundy

#Grupowanie klasteryzacja w R np. K-means w trzech wymiarach
dane3kol <- dane[c('cityMpg','engineSize', 'horsepower')]      # wybieramy 3 parametry z mtcars ilość przejechanych mil na galon paliwa,
dane[c('cityMpg','engineSize', 'horsepower')]                  # a także pojemność silnika i konie mechaniczne (miary z USA)
grupowanie3_kmeans <- kmeans(dane3kol, 3)    # 3 zbiory odrębnych danych
grupowanie3_kmeans_cluster <- factor(grupowanie3_kmeans$cluster)
library(scatterplot3d)
scatterplot3d(dane3kol,color=grupowanie3_kmeans_cluster,pch=19) #wizualizacja w 3D 
library(rgl)
#http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
r3dDefaults$windowRect <- c(0,50, 800, 800) 
plot3d(dane3kol, col=grupowanie3_kmeans_cluster, size = 10)     #wizualizacja w 3D interaktywna
Sys.sleep(2)                                 # pauza na 2 sekundy

#Grupowanie klasteryzacja w R np. K-means w trzech obliczonych wymiarach z PCA  
#http://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
library(nFactors)
ev <- eigen(cor(dane)) # get eigenvalues
ap <- parallel(subject=nrow(dane),var=ncol(dane),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 
Sys.sleep(2)                                 # pauza na 2 sekundy 
library(FactoMineR)                          # PCA Variable Factor Map
result <- PCA(dane)                          # graphs generated automatically 
plot(result)
Sys.sleep(2)                                 # pauza na 2 sekundy
pc <- princomp(dane, cor=TRUE, scores=TRUE)  # PCA obliczamy sztuczne 3 wymiary
summary(pc)
biplot(pc)
plot(pc,type="lines")
mdatapc<-pc$scores[,1:3]
str(mdatapc)
class(mdatapc)
grupowanie_pca_kmeans <- kmeans(mdatapc, 4)  # 4 zbiory odrębnych danych
grupowanie_pca_kmeans_cluster <- factor(grupowanie_pca_kmeans$cluster)
scatterplot3d(mdatapc,color=grupowanie_pca_kmeans_cluster,pch=19)       #wizualizacja w 3D 
r3dDefaults$windowRect <- c(0,50, 800, 800) 
plot3d(mdatapc, col=grupowanie_pca_kmeans_cluster, size = 10)           #wizualizacja w 3D interaktywna
text3d(pc$scores[,1:3],texts=rownames(dane)) # dodajemy parametry z badanych danych
text3d(pc$loadings[,1:3], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)
Sys.sleep(2)                                 # pauza na 2 sekundy

#Grupowanie klasteryzacja w R np. hierarchiczne grupowanie
nc <- 3                                      # liczba grup
di <- dist(dane, method="euclidean")
grupowanie_hclust <- hclust(di, method="ward.D2")
hcluster <-                                  # potnij drzewo na nc grup i przerotuj numery
    as.factor((cutree(grupowanie_hclust,k=nc)-2)%%nc+1)
plot(grupowanie_hclust, xlab="")
rect.hclust(grupowanie_hclust, k=nc, border="red")
groupy <- cutree(grupowanie_hclust, k=nc)    # potnij drzewo na nc grup
Sys.sleep(2)                                 # pauza na 2 sekundy 

#Grupowanie Mclust
#https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
#dane <- dane_scaled
#library(mclust)
#grupowanie_mclust <- Mclust(dane)
#plot(grupowanie_mclust, what = "BIC") 
#summary(grupowanie_mclust) 
#Sys.sleep(2)                                # pauza na 2 sekundy


#Grupowanie klasteryzacja w R: pvclust hierarchiczne grupowanie
#wyniki wykorzystane w poniżej wyjaśnionych drzewach decyzyjnych i klasyfikatorach
#dane <- dane_scaled
#http://www.sigmath.es.osaka-u.ac.jp/shimo-lab/prog/pvclust/
library("pvclust")
grupowanie_pvclust <- pvclust(t(dane), method.dist="euclidean", method.hclust="average", nboot=30)
plot(grupowanie_pvclust)
pvrect(grupowanie_pvclust, alpha=0.95) 
pvgroup <- pvpick(grupowanie_pvclust, alpha=0.95) 
#dane$grupa <- NA
#for( i in seq_len(length(pvgroup$clusters))) dane[pvgroup$clusters[[i]],]$grupa = i
#dane$grupa


###################################################################################################
#DRZEWA DECYZYJNE 
#dodajemy etykietę grupy z grupowania hclust oraz uczymy klasyfikatory na części danych
#a testujemy na pozostałej pozbawionej etykiet 
#dodawanie etykiety
nc <- 3                                      # stosuje dane z hclust do generowania grup
grupy <- cutree(grupowanie_hclust, k=nc)     # potnij drzewo na nc grup
dane$grupa <- as.numeric(grupy)
dane$grupa <- factor(dane$grupa)             
# twórz drzewo na zbiorze trenującym dane_trenujace z etykietami grupa
# potem je użyj na zbiorze dane_testujace do określenia brakujących etykiet grupa
# wybór zbiorów trenujących i testujących
library(dplyr)
dane_trenujace <- sample_n(dane,180)         # uczę na przykładzie wybranych wierszy
dane_testujace <- dane[-as.numeric(rownames(dane_trenujace)),]
etykiety_z_grupowania <-dane_testujace$grupa #zapamiętuję grupę elementów testowych
dane_testujace$grupa <- NULL                 # i usuwam ją do testu
rownames(dane_trenujace)<-seq_len(nrow(dane_trenujace))# nazywa wiersze ich indeksami
rownames(dane_testujace)<-seq_len(nrow(dane_testujace))# nazywa wiersze ich indeksami 

#Drzewa decyzyjne rpart
#http://www.statmethods.net/advstats/cart.html
#http://machinelearningmastery.com/non-linear-regression-in-r-with-decision-trees/
library(rpart)
#?rpart.control                              # ustaw parametry rpart
klasyfikator_rpart <- rpart(grupa ~ ., method="class", data=dane_trenujace, minsplit=2)
#klasyfikator_rpart <- rpart(grupa ~ ., method="anova", data=dane)
printcp(klasyfikator_rpart)                  # wyswietlam rezultaty
plotcp(klasyfikator_rpart)                   # vizualizuję krossvalidację
summary(klasyfikator_rpart)                  # podsumowanie
# utwórz rysunek
par(mar=c(0,5,3,5))
plot(klasyfikator_rpart, uniform=TRUE,
     main="Decyzyjne drzewo dla mtcars z etykietą fit (pvclust)")
text(klasyfikator_rpart, use.n=TRUE, all=TRUE, cex=.8)
#zapisz rysunek do pliku
post(klasyfikator_rpart, file = "treerpart1.pdf",
     title = "Decyzyjne drzewo dla mtcars z etykietą fit (pvclust)")
pred_etykiety1 <- predict(klasyfikator_rpart, dane_testujace)   #przewiduj etykietę zbioru z fit=NA
pred_etykiety1

#Drzewa Decyzyjne ctree
library(party)
klasyfikator_ctree <- ctree(grupa ~ ., data=dane_trenujace, controls = 
    ctree_control(mincriterion = 0,minbucket = 0,minsplit = 0,maxdepth = 100,savesplitstats = TRUE))
plot(klasyfikator_ctree)
pdf('treec.pdf')
plot(klasyfikator_ctree)
dev.off()
#plot(klasyfikator_ctree, type="simple")
pred_etykiety2 <- predict(klasyfikator_ctree, dane_testujace)    #przewiduj etykietę zbioru z fit=NA
pred_etykiety2


#Drzewa Decyzyjne randomForest - wiele drzew i głosowanie
#zmienna liczba atrybutów dla każdej próby tworzenia drzewa
library(RRF)
klasyfikator_rrf <- RRF(grupa ~ ., data=dane_trenujace)
print(klasyfikator_rrf)                      # zobacz rezultaty
#summary(klasyfikator_rrf)
importance(klasyfikator_rrf)                 # importance of each predictor 
pred_etykiety3 <- predict(klasyfikator_rrf, dane_testujace)#przewiduj etykietę zbioru z fit=NA
pred_etykiety3


#Mixture Discriminant Analysis
library(mda)
klasyfikator_mda <- mda(grupa ~ ., data=dane_trenujace)
print(klasyfikator_mda)                      # zobacz rezultaty
summary(klasyfikator_mda)
pred_etykiety4 <- predict(klasyfikator_mda, dane_testujace)#przewiduj etykietę zbioru z fit=NA
pred_etykiety4


#Regularized Discriminant Analysis
library(klaR)
klasyfikator_rda <- rda(grupa ~ ., data=dane_trenujace)
print(klasyfikator_rda)                      # zobacz rezultaty
summary(klasyfikator_rda)
pred_etykiety5 <- predict(klasyfikator_rda, dane_testujace)   #przewiduj etykietę zbioru z fit=NA
pred_etykiety5


#Gradient Boosted Machine 
#http://www.listendata.com/2015/07/gbm-boosted-models-tuning-parameters.html
library(gbm)
klasyfikator_gbm <- gbm(grupa ~ ., data=dane_trenujace, distribution="gaussian", 
              bag.fraction = 0.5, n.trees = 1000, interaction.depth =6, 
              shrinkage = 0.1, n.minobsinnode = 1)
print(klasyfikator_gbm)                      # zobacz rezultaty
summary(klasyfikator_gbm)
pred_etykiety6 <- predict(klasyfikator_gbm, dane_testujace,n.trees = 10)   #przewiduj etykietę zbioru z fit=NA
round(pred_etykiety6)



#Niesprawdzone w tym skrypcie funkcje funkcja(pakiet) m.in:
#fda(mda), kernlab(ksvm), knn3(caret), naiveBayes(e1071), nnet(nnet), qda(MASS)
#J48(RWeka), PART(RWeka), C5.0(C50), vglm(VGAM), lda(MASS), plsda(caret)
#earth(earth), knnreg(caret), glmnet(glmnet), lars(lars), glmnet(glmnet)
#pcr(pls), plsr(pls)
#i wiele innych: http://topepo.github.io/caret/modelList.html
#http://machinelearningmastery.com/how-to-get-started-with-machine-learning-algorithms-in-r/
#R webinars
#https://cran.r-project.org/web/packages/RSelenium/vignettes/OCRUG-webinar.html
#https://vimeo.com/89562453


#UWAGA!!!! Można zauważyć, że wyniki etykietowania zbioru z nieznaną wartością fit z pvclust
# dla różnych etykiety_osobne_pakiety* czasami są takie same, czasami różne
# zbieram je do jednej ramki danych
fit1=c();for(i in 1:nrow(pred_etykiety1))fit1=c(fit1,which(pred_etykiety1[i,]==1))
etykiety_osobne_pakiety<-t(data.frame(rpart=as.numeric(fit1)))
etykiety_osobne_pakiety<-rbind(etykiety_osobne_pakiety,ctree=as.numeric(pred_etykiety2))
etykiety_osobne_pakiety<-rbind(etykiety_osobne_pakiety,rrf=as.numeric(pred_etykiety3))
etykiety_osobne_pakiety<-rbind(etykiety_osobne_pakiety,mda=as.numeric(pred_etykiety4))
etykiety_osobne_pakiety<-rbind(etykiety_osobne_pakiety,rda=as.numeric(pred_etykiety5$class))
etykiety_osobne_pakiety<-rbind(etykiety_osobne_pakiety,gbm=round(pred_etykiety6))
etykiety_osobne_pakiety

#########################################################################################################################
#CARET - Klasyfikator ogólny wrapper dla około 170 metod
#https://cran.r-project.org/web/packages/caret/vignettes/caret.pdf
#lista około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
#http://topepo.github.io/caret/bytag.html
#przyładowy model
#http://topepo.github.io/caret/training.html
#https://www.youtube.com/watch?v=7Jbb2ItbTC4 #caretwebinar
#http://www.r-bloggers.com/caret-webinar-materials/
library(caret)

#Drzewa decyzyjne rpart opakowane pakietem caret
#z listy około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
#http://www.statmethods.net/advstats/cart.html
#http://machinelearningmastery.com/non-linear-regression-in-r-with-decision-trees/
library(rpart)
#klasyfikator_rpart <- rpart(grupa ~ ., method="class", data=dane_trenujace, minsplit=2)
#zastępujemy rpart funkcją train z pakietu caret, możemy tylko zmieniać parametr cp
#reszta jest "zgadywana" przez caret - pełna wygoda
klasyfikator <- train(grupa ~ ., data=dane_trenujace,method = "rpart")
print(klasyfikator)                          # zobacz rezultaty
#summary(klasyfikator)   
etykiety <- predict(klasyfikator, dane_testujace)#przewiduj etykietę zbioru z fit=NA
etykiety_caret<-t(data.frame(rpart=as.numeric(etykiety)))


#Drzewa Decyzyjne ctree opakowane pakietem caret
#z listy około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
library(party)
#ctree <- ctree(grupa ~ ., data=dane_trenujace, controls = 
#                 ctree_control(mincriterion = 0,minbucket = 0,minsplit = 0,maxdepth = 100,savesplitstats = TRUE))
#zastępujemy ctree funkcją train z pakietu caret, możemy tylko zmieniać parametr mincriterion
#reszta jest "zgadywana" przez caret - pełna wygoda
klasyfikator <- train(grupa ~ ., data=dane_trenujace,method = "ctree")
plot(klasyfikator)
etykiety <- predict(klasyfikator, dane_testujace)     #przewiduj etykietę zbioru z fit=NA
etykiety_caret<-rbind(etykiety_caret,t(data.frame(ctree=as.numeric(etykiety))))


#Drzewa Decyzyjne randomForest RRF (wiele drzew i głosowanie -
#zmienna liczba atrybutów dla każdej próby tworzenia drzewa) opakowane pakietem caret
#z listy około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
#klasyfikator_rrf <- RRF(grupa ~ ., data=dane_trenujace)
#zastępujemy RRF funkcją train z pakietu caret, możemy tylko zmieniać parametry mtry, coefReg, coefImp
#reszta jest "zgadywana" przez caret - pełna wygoda
klasyfikator <- train(grupa ~ ., data=dane_trenujace,method = "RRF")
print(klasyfikator)                          # zobacz rezultaty
#summary(klasyfikator)
etykiety <- predict(klasyfikator, dane_testujace)  #przewiduj etykietę zbioru z fit=NA
etykiety_caret<-rbind(etykiety_caret,t(data.frame(rrf=as.numeric(etykiety))))



#Mixture Discriminant Analysis opakowane pakietem caret
#z listy około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
library(mda)
#klasyfikator_mda <- mda(grupa ~ ., data=dane_trenujace)
#zastępujemy mda funkcją train z pakietu caret, możemy tylko zmieniać parametr subclasses
#reszta jest "zgadywana" przez caret - pełna wygoda
klasyfikator <- train(grupa ~ ., data=dane_trenujace,method = "mda")
print(klasyfikator)                          #zobacz rezultaty
summary(klasyfikator)
etykiety <- predict(klasyfikator, dane_testujace)#przewiduj etykietę zbioru z fit=NA
etykiety_caret<-rbind(etykiety_caret,t(data.frame(mda=as.numeric(etykiety))))


#Regularized Discriminant Analysis opakowane pakietem caret
#z listy około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
library(klaR)
#klasyfikator_rda <- rda(grupa ~ ., data=dane_trenujace)
#zastępujemy rda funkcją train z pakietu caret, możemy tylko zmieniać parametry gamma, lambda
#reszta jest "zgadywana" przez caret - pełna wygoda
klasyfikator <- train(grupa ~ ., data=dane_trenujace,method = "rda")
print(klasyfikator)                          # zobacz rezultaty
summary(klasyfikator)
etykiety <- predict(klasyfikator, dane_testujace)   #przewiduj etykietę zbioru z fit=NA
etykiety_caret<-rbind(etykiety_caret,t(data.frame(rda=as.numeric(etykiety))))


#Gradient Boosted Machine opakowane pakietem caret
#z listy około 170 klasyfikatorów i regresji
#http://topepo.github.io/caret/modelList.html
#http://www.listendata.com/2015/07/gbm-boosted-models-tuning-parameters.html
#ZA POMOCĄ trainControl możemy dodać k-krotną walidację
#https://pl.wikipedia.org/wiki/Sprawdzian_krzy%C5%BCowy#K-krotna_walidacja
#3-krotna walidacja (z uśrednianiem) powtórzona dwa razy
ctrl <- trainControl(method = "repeatedcv", number = 3, repeats = 2, 
                     classProbs = FALSE)

#odrzucamy hipotezę zerową gdy jest ona prawdziwa - błąd I rodzaju (zdrowy jako chory)
#przyjmujemy hipotezę zerową gdy jest ona fałszywa - błąd II rodzaju.
#błąd trzeciego rodzaju to zbytnie dopasowanie do modelu (udzielenie właściwej odpowiedzi na niewłaściwe pytanie)

gbmGrid <-  expand.grid(interaction.depth = 9, 
                        n.trees = 100, shrinkage = 0.1, n.minobsinnode = 2)
klasyfikator <- train(grupa ~ ., data=dane_trenujace,
                  method = "gbm",
                  verbose = FALSE,
                  trControl = ctrl,
                  tuneGrid = gbmGrid)
print(klasyfikator)                          # zobacz rezultaty
summary(klasyfikator)
etykiety <- predict(klasyfikator, dane_testujace,n.trees = 10) #przewiduj etykietę zbioru z fit=NA
etykiety_caret<-rbind(etykiety_caret,t(data.frame(gbm=as.numeric(etykiety))))
#trellis.par.set(caretTheme())
#plot(traingbm)
#ggplot(traingbm)

#WYNIKI KLASYFIKACJI ZBIORU TESTOWEGO w którym usunięto etykiety, ale ich wartości są pamiętane w etykiety_z_grupowania
#różnice wynikają z tego że algorytmy korzystają z pseudolosowych generatorów random i
#generują za każdym razem szczególnie dla małego zbioru różne wyniki
#wyniki z osobnych klasyfikatorów
etykiety_osobne_pakiety
#wyniki z tych samych klasyfikatorów, ale opakowanych w pakiecie caret i 
#czasami już tam automatycznie tuningowanych (też stąd inne wyniki)
etykiety_caret
#poprzednie_wyniki z grupowania (TAKIE POWINNY BYĆ WYNIKI Z KLASYFIKATORÓW)
t(data.frame(spr=as.numeric(etykiety_z_grupowania)))
#ZAWODY KLASYFIKATORÓW WYGRYWA dla dużych zbiorów testujących przeważnie GBM!
#########################################################################################################################
Sys.sleep(5)                                 #pauza na 5 sekund


#########################################################################################################################
#TIME SERIES - przewiduje trendy w szeregach czasowych
#forecast to prognoza, predykcja to także dopasowanie modelu do bieżących danych
#http://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/
#http://a-little-book-of-r-for-time-series.readthedocs.io/en/latest/src/timeseries.html
#https://www.otexts.org/fpp/8/9
ndata <- "AirPassengers"                     #dane liczby pasażerów w miesiącu w okresie kilku lat
data(list=ndata)
dane<-get(ndata)
dane                                         # badane dane
start(dane)                                  # początkowa data
end(dane)                                    # końcowa data
frequency(dane)                              # liczba powtórzeń w okresie czasu (wierszu)
time(dane)                                   # wspólny wektor wszystkich powtórzeń
cycle(dane)                                  # cykl - numeracja powtórzeń w obrębie czasu
summary(dane)                                # średnie w podsumowaniu
plot(as.vector(time(dane)), as.vector(dane), type = "l")#wykres danych
plot(dane)                          
plot(log(dane))
abline(reg=lm(dane~time(dane)))              # krzywa regresji
plot(aggregate(dane,FUN=mean))               # średnie na rok
boxplot(dane~cycle(dane))                    # sezonowy efekt
dane_decomp<-decompose(dane)                 # dekompozycja na regularne i nieregularne trendy
str(dane_decomp)
plot(dane_decomp)                            # widać trend, regularności i nieregularności
plot(dane - dane_decomp[['seasonal']])       # dane bez sezonowego efektu (regularności)

library(tseries)
adf.test(dane, "stationary", k=0)            #Augmented Dickey-Fuller Test na stacjonarność
adf.test(log(dane), "stationary", k=0)       #nie sprawdza się
adf.test(diff(dane), "stationary", k=0)      #trzeba wykonać testy z pakietu fUnitRoots
adf.test(diff(log(dane)), "stationary", k=0) #TE NIE DZIAŁAJĄ!

library(fUnitRoots);
adfTest(dane);                               # ADF Test dla p<0.01 wskazuje na odpowiednie dane 
adfTest(log(dane));                          # do modelu arima
adfTest(diff(dane));                         # dla różnicy 
adfTest(diff(log(dane)));                    # różnica z logarytmu danych - najlepszy wynik    
acf(dane)                                    # Total Correlation Chart (Auto – correlation Function / ACF) 
                                             # dla kolejnych korelacji  x(t) z  x(t-1) , x(t-2)
pacf(dane)                                   # partial correlation function (PACF) poniżej linii AR
acf(log(dane))                               # acf z log
pacf(log(dane))                              # pacf z log
acf(diff(dane))                              # acf z różnicy
pacf(diff(dane))                             # pacf z różnicy
acf(diff(log(dane)))                         # acf z różnicy z logarytmu
pacf(diff(log(dane)))                        # pacf z różnicy z logarytmu
acf(diff(dane,diff=2))                       # acf z różnicy podwójnej
pacf(diff(dane,diff=2))                      # pacf z różnicy podwójnej
acf(diff(log(dane),diff=2))                  # acf z różnicy podwójnej z logarytmu
pacf(diff(log(dane),diff=2))                 # pacf z różnicy podwójnej z logarytmu

library(forecast)
tsdisplay(dane)                              # tsdisplay pokazuje acf i pacf jednocześnie 
tsdisplay(log(dane));                        # wystające piki poza niebieskie linie 
tsdisplay(diff(dane,12));                    # wskazują w PACF elementy AR komponenty 
tsdisplay(diff(log(dane,12)));               # w ACF MA komponenty MA(1) ostatni wystający pik 
                                             # na 1 miejscu licząc od początku

#dla Auto-Regressive (AR) PACF się zmniejsza po różnicy czasów na osi x - dla wpływów x(t) na x(t+N) N>>0
#dla Moving Average (MA) ACF się zmniejsza - to dla nieregularnych losowych - dla wpływów x(t) na x(t+N) N~1
#c(p,d,q) p związane z AR oraz q związane z MA, d to stopień diff zastosowany
#piki na okresowej krotności pozycji są do opcji seasonal
#najlepiej tsdisplay(forecast(auto.arima(dane),20)$residuals) z pakietu forecast sprawdzić i dodać AR i MV 
library(forecast)
model_autoarima<-auto.arima(dane)            # ARIMA model 
model_autoarima                              # parametry z ACF i PACF uzyskane automatycznie
prognoza<-forecast(model_autoarima,h=20)
plot(prognoza)                               # wykres na 20 miesięcy w przód, widać zwiększający się margines błędu
Sys.sleep(2)                                 # pauza na 2 sekund
tsdisplay(prognoza$residuals)                # pokaż analizę błędów widzimy w ACF pik na 23 pozycji dodajemy MA(11)
# to zmienia model z ARIMA(0,1,1)(0,1,0)[12] na ARIMA(0,1,1)(0,1,11)[12]
Box.test(prognoza$residuals, lag=12, type="Ljung-Box")
predict(model_autoarima,20)                  # na 20 okresów w przód
#w modelu minimalizujemy aic 
model_arima <- arima(log(dane), c(0, 1, 1), seasonal = list(order = c(0, 1, 11), period = 12)) # 2 podwójna różnica
predyktor <- 
  predict(model_arima, n.ahead = 10*12)      # na 10 lat wprzód przewidzieć trend
ts.plot(dane,2.718^predyktor$pred, log = "y", lty = c(1,3))#krzywa predykcji 
prognoza<-forecast.Arima(model_arima, h=24)  # prognoza z arimy na 24 okresów
plot(prognoza)                               # rysunek
acf(prognoza$residuals, lag.max=20)          # sprawdzanie prognozy, czy można ją poprawić (zbyt duże korelacje błedów)
tsdisplay(prognoza$residuals)
# w Box.test lag = 2 * period lub prognozy horyzont
Box.test(prognoza$residuals, lag=24, type="Ljung-Box")# dla p<<0 są autokorelacje zły predyktor dla MA i diff lepszy
Sys.sleep(2)                                 # pauza na 2 sekund


#model_predykcji <- HoltWinters(dane, beta=FALSE, gamma=FALSE)# gamma = FALSE nie okresowy model
#model_predykcji <- HoltWinters(dane, beta=FALSE)  # beta=FALSE wygładanie exponens
#model_predykcji <- HoltWinters(dane)        # 
model_predykcji <- HoltWinters(dane, seasonal = "mult")# okresowy "multiplicative", domyślny additive
plot(model_predykcji)                        # linia czerwona to predykcja 
model_predykcji$SSE                          # jakość predykcji
prognoza<-forecast.HoltWinters(model_predykcji,h=8)# predykcja prognoza na 8 okresów
plot(prognoza)
acf(prognoza$residuals, lag.max=20)          # sprawdzanie prognozy, czy można ją poprawić (zbyt duże korelacje błedów)
Box.test(prognoza$residuals, lag=24, type="Ljung-Box")# dla p<<0 są autokorelacje zły predyktor
plot.ts(prognoza$residuals)                  # wykres błedów badamy jego wariancję
plotForecastErrors<-function(forecasterrors) # funkcja wpasowująca rozkład błedów w rozkład normalny
{
  # histogram błedów prognozy
  mybinsize <- IQR(forecasterrors)/4
  mysd   <- sd(forecasterrors)
  mymin  <- min(forecasterrors) - mysd*5
  mymax  <- max(forecasterrors) + mysd*3
  # normalny rozkład z średnią 0 i odchylemiem standardowym mysd
  mynorm <- rnorm(10000, mean=0, sd=mysd)
  mymin2 <- min(mynorm)
  mymax2 <- max(mynorm)
  if (mymin2 < mymin) { mymin <- mymin2 }
  if (mymax2 > mymax) { mymax <- mymax2 }
  # czerwony hostogram błedów z linią rozkładu normalnego
  mybins <- seq(mymin, mymax, mybinsize)
  hist(forecasterrors, col="red", freq=FALSE, breaks=mybins)
  # dla freq=FALSE pole pod histogramem = 1
  # normalny rozkład z średnią 0 i odchylemiem standardowym mysd
  myhist <- hist(mynorm, plot=FALSE, breaks=mybins)
  # niebieska linia rozkładu normalnego
  points(myhist$mids, myhist$density, type="l", col="blue", lwd=2)
}
plotForecastErrors(prognoza$residuals)       # czy jest zbliżony do normalnego rozkład błedów

library(forecast)
model_predykcji=ets(dane)                    # w pełni automatyczny model predykcji
plot(forecast(model_predykcji))              # wykres z marginesami niepewności w predykcji
Sys.sleep(2)                                 # pauza na 2 sekund
predict(model_predykcji,10)                        # na 10 okresów w przód

#Sieci neuronowe w predykcji i prognozie
model_predykcji <- nnetar(dane)              # specjalne sieci neuronowe do prognozowania trendów
prognoza <- forecast(model_predykcji)        # prognoza  
plot(prognoza)                               # rysunek na niebiesko prognoza
Sys.sleep(2)                                 # pauza na 2 sekund
tsdisplay(prognoza$residuals)                # pokaż analizę błędów 
Box.test(prognoza$residuals, lag=12, type="Ljung-Box")

#ciekawostki z time series
#http://www.r-bloggers.com/additive-modelling-global-temperature-time-series-revisited/
#http://www.r-bloggers.com/additive-modelling-and-the-hadcrut3v-global-mean-temperature-series-2/
#########################################################################################################################

