# Zasady ogólne:
# indeksy w R zaczynają się od 1, a nie od 0
# wielkość liter ma znaczenie
# w rstudio kursor myszy na nazwie funkcji i F1 wywołują opis funkcji
#Zamieszczone przykłady dadzą się uruchomić jeśli zainstalujecie wymienione pakiety R 
#(w linuxie na roocie w konsoli R, żeby nie instalować na lokalnym koncie):
pkglist<-c("reshape","ade4","sqldf","plyr","dplyr")
pkglist<-c(pkglist,"party","rgl","scatterplot3d","fpc","pvclust","dendextend")
pkglist<-c(pkglist,"nFactors","FactoMineR","randomForest","mclust")
pkglist<-c(pkglist,"rpart","ipred","gbm","mda","klaR","kernlab")
#pkglist<-c(pkglist,"MASS","RWeka")
pkgcheck <- pkglist %in% row.names(installed.packages())
pkglist[!pkgcheck]
#ODKOMENTUJ jak chcesz zainstalować biblioteki najlepiej w konsoli tekstowej R na koncie root w Linuxie
#for(i in pkglist[!pkgcheck]){install.packages(i,depend=TRUE)}
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



# STRUKTURY DANYCH
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


#MACIERZ: dwuwymiarowa tablica tego samego typu zmiennych
# użyj indeksów macierzy 
x <- matrix(1:16, nrow = 4)
x
x[2, ]        #drugi wiersz
x[, 3]        #trzecia kolumna
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
rowmatrix           #wychodzi to samo co t(colmatrix) - transponowana macierz colmatrix
c(rowmatrix)        #konwersja do wektora, ale innego niż początkowy wektor cells
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

#TABLICA ARRAY: podobna do macierzy matrix, ale może mieć więcej wymiarów
dim1 <- c('A1', 'A2')
dim2 <- c('B1', 'B2', 'B3')
dim3 <- c('C1', 'C2', 'C3', 'C4')
z <- array(1:24, c(2, 3, 4), dimnames = list(dim1, dim2, dim3))
z
z[1, 2, 3] #wartość komórki z jednego pola trójwymiarowej macierzy


#LISTA: uporządkowany zbiór obiektów o możliwych różnych typach
g <- 'Moja pierwsza lista'
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow = 5)
k <- c('jeden', 'dwa', 'trzy')
mlist <- list(tytul = g, wiek = h, j, k)
mlist
mlist[[2]]              #drugi element listy wiek jako wektor
mlist[['wiek']]         #element wiek, w tym przypadku wektor
mlist$wiek              #element wiek, w tym przypadku wektor
mlist['wiek']           #element wiek, w tym przypadku lista (nie używać, kiedy potrzebny wektor)
mlist[2]                #element wiek, w tym przypadku lista (nie używać, kiedy potrzebny wektor)
typeof(mlist[['wiek']]) #typ double  
typeof(mlist['wiek'])   #typ lista
mlist[[2]][[1]]         #25, pierwszy element drugiego argumentu wektora wiek z listy mlist
as.list(h)              #konwersja do listy
nlist<-as.list(h)       
list(mlist,nlist)       #lista dwóch list
plist<-list(mlist,nlist)
print(plist)            #wypisz listę dwóch list
str(plist)              #struktura listy dwóch list
dput(plist)             #kod w R listy dwóch list
class(plist)            #typ struktury danych - ista
#cat(plist)             uwaga! cat z listą nie działa!
#nadpisuje wartości dwóch elementów listy o nazwach tytul i wiek
mlist[names(mlist) %in% c('tytul','wiek')]<-c('Nadpisany element listy',list(c(2,4,6,7)))
lapply(mlist,FUN=length)#zastosuj funkcję length do każdego elementu listy mlist i zwróć LISTĘ długości argumentów
sapply(mlist,FUN=length)#zastosuj funkcję length do każdego elementu listy mlist i zwróć WEKTOR długości argumentów
ylist<-list(a=1, b=3, c=5, d=6)
sapply(ylist, FUN=function(x,p) x^p, p=2)   #wynik potęgi do p=2 wynik to wektor 1,9,25,36
sapply(ylist, FUN=function(x,p) x^p, p=2:3) #wynik potęgi do p=2 wynik to macierz 2 wierszowa 1,9,25,36 i 1,27,125,216



#RAMKA DANYCH: kolumny z różnymi typami, odpowiednik tabeli w bazie lub arkusza w excelu
pacjent_id <- c(1, 2, 3, 4)
wiek <- c(25, 34, 28, 52)
cukrzyca <- c('Typ1', 'Typ2', 'Typ1', 'Typ1')
stan <- c('Kiepski', 'Poprawa', 'Wybitny', 'Kiepski')
pacjenci <- data.frame(pacjent_id, wiek, cukrzyca, stan)
nrow(pacjenci)        # ilość wierszy
ncol(pacjenci)        # ilość kolumn
dim(pacjenci)         # rozmiar to wektor z liczbą wierszy i kolumn c(nrow(pacjenci),ncol(pacjenci))
rownames(pacjenci) <- seq_len(nrow(pacjenci)) #na wszelki wypadek nazywa wiersze ich indeksami
# podaj wiersz lub kolumnę ramki danych
i <- 1; j <- 2
pacjenci[i,]               # i-ty wiersz jako ramka danych
pacjenci[, j]              # j-ta kolumna jako wektor
pacjenci[, 'wiek']         # kolumna 'wiek' jako wektor
pacjenci[['wiek']]         # kolumna 'wiek' jako wektor
pacjenci$wiek              # kolumna 'wiek' jako wektor
pacjenci[j]                # j-ta kolumna jako ramka danych
pacjenci['wiek']           # kolumna 'wiek' jako ramka danych
# podaj ij-ty element ramki danych
as.integer(pacjenci[i,][j])# pacjenci[i, ][j] i-ty wiersz jta kolumna jako integer
pacjenci[i, j]             # komórka z i tego wiersza i j-tej kolumny
pacjenci[[i, j]]           # komórka z i tego wiersza i j-tej kolumny
pacjenci[[j]][i]           # komórka z i tego wiersza i j-tej kolumny
pacjenci[, j][i]           # komórka z i tego wiersza i j-tej kolumny
pacjenci$wiek[i]           # komórka z i tego wiersza i kolumny wiek 
pacjenci[i, 'wiek']        # i-ty wiersz kolumny wiek
pacjenci[i, i:j]           # dwie komórki z i tego wiersza i oraz i-tej i j-tej kolumny, to NIE działa na [[i, i:j]]
pacjenci[1:2]              # pierwsze dwie kolumny jako ramka danych
pacjenci[c('cukrzyca', 'stan')]
index <- 2
pacjenci[-index,]          # usuń 2 wiersz z ramki danych
#Wybieranie podzbiorów
pacjenci[1:3, ]            # trzy pierwsze wiersze - pacjenci
pacjenci[which(pacjenci$stan == 'Kiepski' & pacjenci$wiek < 30), ] #pacjenci stan kiepski i wiek poniżej 30
library(plyr); library(dplyr) # use package dplyr (install first)
filter(pacjenci, stan == 'Kiepski' & wiek < 30) # subset()
subset(pacjenci, wiek >= 35 | wiek < 24, select = c(wiek, stan))
subset(pacjenci, stan == 'Kiepski' & wiek < 30, select = pacjent_id:date)
#Sortowanie
pacjenci[order(pacjenci$wiek), ]                  # sortuj wiersze od najmłodszych do najstarszych, domyślnie rosnąco
attach(pacjenci)
spacjenci <- pacjenci[rev(order(cukrzyca, wiek)),]# sortuj wiersze w porządku malejącym 'rev' po typie, od najstarszych do najmłodszych
detach(pacjenci)
spacjenci
attach(pacjenci)
spacjenci <- pacjenci[order(cukrzyca,-wiek),]     # sortuj wiersze po typie, od najstarszych do najmłodszych
detach(pacjenci)
spacjenci
#Łączenie danych: dodawanie wierszy
new_row <-                                        #nowy wiersz
  data.frame(
    pacjent_id = 5,
    wiek = 10,
    cukrzyca = 'Typ3',
    stan = 'Zdrowy'
  )
pacjenci <- rbind(pacjenci, new_row)             # RBIND dodaje nowy wiersz do ramki danych
spacjenci<- rbind(spacjenci, new_row)            # to samo z kopią posortowanych - spacjenci
spacjenci$pacjent_id <- spacjenci$pacjent_id + 10#inne identyfikatory w spacjenci
rbind(pacjenci, spacjenci)                       # połącz dwie ramki z tą samą liczbą kolumn
#Łączenie danych: dodawanie kolumn 
pacjenci$new_col <- c(2:6)                       # dodaj kolumnę do ramki danych: metoda 1
pacjenci$new_col <- NULL                         # usuń kolumnę z ramki danych: metoda 1
pacjenci <- transform(pacjenci, new_col = c(2:6))# dodaj kolumnę do ramki danych: metoda 2
pacjenci <- within(pacjenci, {new_col = NULL})   # usuń kolumnę z ramki danych: metoda 3
merge(pacjenci, spacjenci, by = "pacjent_id")    # połącz kolumnami pacjenci i spacjenci po ID
merge(pacjenci, spacjenci, 
      by = c('pacjent_id', 'cukrzyca'))          # połącz kolumnami pacjenci i spacjenci po ID i Country
cbind(pacjenci, spacjenci)                       # CBIND połącz kolumnami pacjenci i spacjenci muszą mieć tą samą ilość wierszy
#Usuwanie kolumn
myvars <- names(spacjenci) %in% c('wiek', 'cukrzyca')# wyodrębnianie zmiennych (kolumn) wiek, cukrzyca z ramki 
myvars
spacjenci[!myvars]                               # usuń zmienne wiek, cukrzyca
spacjenci$wiek <- spacjenci$cukrzyca <- NULL     # usuń zmienne wiek, cukrzyca
spacjenci[c(-2,-3)]                              # usuń 2 i 3-ą kolumnę
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
with(df, {               # 'with' nic nie zwraca
wiek[x1 == 2] <- 1       # reszta jest wypełniana NA (brakiem danych z R)
})
df
df$wiek <- NULL          # usuń wiek
# metoda 5
df <- within(df, {       # 'within' zwraca df
  wiek <- NA             # utwórz nową zmienną wiek i zainicjalizuj ją NA (brakiem danych z R)
  wiek[x1 == 2] <- 1
})
df
#Zmiana nazw
#rename(dataframe, c(oldname1="newname1", oldname2="newname2",...))
#library(reshape)
df <- rename(df, c(sumx = "suma"))
df
names(df)[4] <- "srednia"      # names(df) zwraca wektor nazw zmiennych lub użyj fix(df) aby zmienić nazwy w gui
#Manipulacja NA
df$wiek[df$wiek == 1] <- NA    # zamienia 1 na NA w kolumnie wiek
df
df$wiek[is.na(df$wiek)] <- 55  # zamienia NA na 55 w kolumnie wiek
df$wiek <-
  ifelse(is.na(df$wiek), 55, df$wiek)# też zamienia NA na 55 w kolumnie wiek
df
x <- c(1, 2, NA, 3)            # wektor z NA
y <- x[1] + x[2] + x[3] + x[4] # y równa się NA
z <- sum(x)                    # y równa się NA
z <- sum(x, na.rm = TRUE)      # na.rm=TRUE usuwa wiersze z brakującymi danymi czyli NA
df$wiek[df$suma == 6] <- NA
df
na.omit(df)                    # na.omit() usuwa wiersze z NA
df[!is.na(df$wiek),]           # też usuwa wiersze z NA


#FAKTOR - etykiety, zmienna jakościowa (niemierzalna), czynnikowa: dyskretne lub porządkowe dane
#zmienne jakościowe (niemierzalne) – np. kolor oczu, płeć, grupa krwi
#porządkowe (quasi-ilościowe) – np. klasyfikacja wzrostu: (niski, średni, wysoki)
#skokowe (dyskretne) – np. ilość posiadanych dzieci, ilość gospodarstw domowych, wiek (w rozumieniu ilości skończonych lat)
#mapa wektorów dyskretnych wartości [1...k]
#nie można faktorów dodawać, mnożyć
#nie działa operator $, używa się pojedyńczych [] z indeksem np. levels(x)[1]
cukrzyca <- c('Typ1', 'Typ2', 'Typ1', 'Typ1')
cukrzyca <- factor(cukrzyca)          # Levels: Typ1 Typ2
cukrzyca
stan <- c('Kiepski', 'Poprawa', 'Wybitny', 'Kiepski')
stan <-
  factor(stan, ordered = TRUE)        # Wybitny-3 Poprawa-2 Kiepski-1
stan
levels(stan)                          #pokazuje poziomy dyskretnej zmiennej stan
stan2 <-
  factor(stan,
         levels = c('Wybitny', 'Poprawa', 'Kiepski')) # Wybitny-1 Poprawa-2 Kiepski-3
stan2
levels(stan2)                         #pokazuje poziomy dyskretnej zmiennej stan2, odwrotnie uporządkowane niż stan
stan2 <- factor(c(as.character(stan2), 'Zalosny')) #dodaje nowy stan nie zdefiniowany
levels(stan2)                         #widać w poziomach nowy stan
#Dyskretyzacja zmiennej do poziomów
i<-1:50+rnorm(50,0,5); i              #zmienna i
k<-cut(i,5); k                        #generuje ze zmiennej i pięć poziomów zmiennej dyskretnej k
levels(k)<-seq_len(length(levels(k))) #zmienia nazwy poziomów na bardziej czytelne
levels(k)
# przykład pokazujący faktory - zmienne jakościowe w ramce danych
# definiują automatycznie przy tworzeniu ramki danych, jednak porządek będzie alfabetyczny
pacjenci
str(pacjenci)
summary(pacjenci)
table(pacjenci$cukrzyca, pacjenci$stan)        #wygeneruj statystyki przecięcia dwóch kolumn
#Liczenie średnich po kolumnach i ich złączeniach
#library(reshape)
#melt i cast lub w jednym recast, ddply, aggregate czyli 4 METODY agregacji
library(reshape)
pacjenci$date <- NULL
# dla każdej wartosci pary cukrzyca, stan wypisz w jednej kolumnie variable inne kolumny i ich wartości
md <- melt(pacjenci, id = (c('cukrzyca', 'stan'))) 
md
cast(md, stan ~ variable, mean)               #policz srednie dla stanów po wartościach z variable
cast(md, cukrzyca ~ variable, mean)           #policz srednie dla cukrzyca po wartościach z variable
cast(md, cukrzyca + stan ~ variable, mean)    #policz srednie dla cukrzyca i stan po wartościach z variable
recast(pacjenci, cukrzyca + stan ~ variable, 
       mean, id.var = c('cukrzyca', 'stan'))  #policz srednie dla cukrzyca i stan po wartościach z variable w jednym kroku
ddply(pacjenci, ~cukrzyca + stan, 
      summarise, N=length(wiek), 
      sredniaid=mean(pacjent_id),
      sredniawiek=mean(wiek))                 #policz srednie dla cukrzyca i stan po innych parametrach w jednym kroku
ddply(pacjenci, .(cukrzyca,stan), 
      summarise, N=length(wiek), 
      sredniaid=mean(pacjent_id),
      sredniawiek=mean(wiek))                 #policz srednie dla cukrzyca i stan po innych parametrach w jednym kroku
aggregate(.~stan+cukrzyca,data=pacjenci,mean) #policz srednie dla cukrzyca i stan po innych parametrach w jednym kroku


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
plot(f)           # pierwszy rysunek
dev.new()         # otwórz drugi rysunek
plot(f(x2plot/20))# przelączanie strzałkami z klawiatury
Sys.sleep(2)                             #pauza na 2 sekundy
dev.off()         # zamknij drugi rysunek
# attach, detach
# attach, detach nie pracują na tych samych nazwach zmiennych, użyj "with"
summary(mtcars$mpg)
plot(mtcars$mpg, mtcars$disp)
plot(mtcars$mpg, mtcars$wt)
attach(mtcars) # dodaj zbiór danych do ścieżki R wyszukiwania
summary(mpg)
plot(mpg, disp)
plot(mpg, wt)
detach(mtcars) # usuń zbiór danych do ścieżki R wyszukiwania
Sys.sleep(2)                             #pauza na 2 sekundy

#używanie sqla
library(sqldf)
head(mtcars)
sqldf('select * from mtcars where carb=1 order by mpg', row.names = TRUE)
sqldf(
  'select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from mtcars where cyl in (4, 6) group by gear'
)



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
pdf('graph1.pdf')
x2plot = seq(1:20)
plot(sin(x2plot))
dev.off()
# drugi rysunek plik
pdf('graph2.pdf')
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
time_elapsed <- (t2 - t1)[[3]] # okres czasu
time_elapsed
time_elapsed <- as.numeric((t2 - t1)[3]) # okres czasu
time_elapsed


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
for (i in 1:10) {                  #for (var in seq) statement
  print('witaj')                   #wypisze 10 razy 'witaj'
}
sapply(1:10, function(i) {         #sapply - alternatywa dla for - wypisze 10 razy 'witaj'
  print('witaj')                   #wypisze 10 razy 'witaj'
})
for (i in 1:10) {                  #pętla for wypisze od 1 do 10
  print(i)
}
sapply(1:10,function(i){print(i)}) #sapply - alternatywa dla for - wypisze od 1 do 10
i <- 10                            #while (cond) statement - zaczyna od i=10
while (i >= 0) {                   #wypisze 10 razy 'witaj'
  print('witaj')
  i <- i - 1
}
for(i in seq_len(nrow(pacjenci)))  #odporna na pustą ramkę pętla z seq_len(nrow)
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



#UCZENIE SIĘ MASZYN
set.seed(12459);                          #początkowa wartość random seed dla takich samych wyników
dev.off()                                 # na wszelki wypadek wyłączamy drugi rysunek

#Analiza Danych - korelacje i podobieństwa
mydata<-mtcars
cor(mydata)                                  #korelacje między zmiennymi
round(cor(mydata), 2)                        # sprawdzamy korelację wybranych kolumn, widać dużą korelację
                                             # tzn. wybrane parametry razem się zmniejszają lub zwiększają
                                             # jeśli korelacja jest DODATNIA
                                             # jeśli jest UJEMNA, to przy zwiększaniu jednej, druga maleje
#image(cor(mtcars))
# standaryzuj zmienne ciągłe
scaledcars <- na.omit(mtcars)             # usuń niepełne wiersze z NA
scaledcars[c('mpg','disp', 'hp','drat', 'wt','qsec' )] <- scale(scaledcars[c('mpg','disp', 'hp','drat', 'wt','qsec' )])
#heatmap z korelacji
#https://planspacedotorg.wordpress.com/2013/07/24/clustered-r-squared-heat-maps-in-r/
dissimilarity <- 1 - cor(mtcars)^2           #miara niepodobnych 1 - korelacja do kwadratu
clustering <- hclust(as.dist(dissimilarity), method="ward.D2")
plot(clustering)                             #grupowanie po niepodobieństwach
order <- clustering$order
oldpar <- par(no.readonly=TRUE); par(mar=c(0,0,0,0))
image(dissimilarity[order, rev(order)], axes=FALSE)
par(oldpar)
clusterRsquared <- function(dataframe) {         #funkcja z miar niepodobieństw
  dissimilarity <- 1 - cor(dataframe)^2
  clustering <- hclust(as.dist(dissimilarity))
  order <- clustering$order
  oldpar <- par(no.readonly=TRUE); par(mar=c(0,0,0,0))
  image(dissimilarity[order, rev(order)], axes=FALSE)
  par(oldpar)
  return(1 - dissimilarity[order, order])
}
round(clusterRsquared(mtcars),2)
Sys.sleep(2) 
#round(clusterRsquared(mdata3),2)
#Sys.sleep(2) 

#Grupowanie klasteryzacja w R np. K-means w dwóch wymiarach
mydata <- mtcars[c('disp', 'hp')]          # wybieramy 2 parametry mtcars pojemność silnika i konie mechaniczne
round(cor(mydata), 2)                      # sprawdzamy korelację wybranych kolumn, widać dużą korelację 
kmeans.res <- kmeans(mydata, 3)            # 3 zbiory odrębnych danych
plot(                                     #wizualizacja w 2D z plot, abline, ade4 s.class
  mydata,
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
  mydata,
  fac = kmeans.cluster,
  add.plot = TRUE,
  col = seq(1, nlevels(kmeans.cluster), 1)
)
aggregate(mydata,by=list(kmeans.res$cluster),FUN=mean) # średnie w grupach widać zróżnicowanie
groupk2 <- data.frame(mydata, kmeans.res$cluster) 
Sys.sleep(2)                             #pauza na 2 sekundy
library(cluster)
clusplot(mydata, kmeans.res$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0)
Sys.sleep(2)                             #pauza na 2 sekundy
library(fpc)
plotcluster(mydata, kmeans.res$cluster) 
#library(fpc)                 # porównanie dwóch grupowań
#cluster.stats(mydata, fit$cluster, fit2$cluster) 
Sys.sleep(2)                             #pauza na 2 sekundy

#Grupowanie klasteryzacja w R : określanie ilości grup
#http://www.statmethods.net/advstats/cluster.html
mydata <- mtcars
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))   #oceń liczbę grup (klasterów)
for (i in 2:15) wss[i] <- sum(kmeans(mydata,centers=i)$withinss)
#$betweenss: suma kwadratów odległośći między clusterami. 
#To jest średnia dystansów pomiędzy centrami klasterów
#Jeśli chcemy osobno leżące klustery, wartość betweenss musi być jak największa.
#$withinss: to jest suma odległości pomiędzy węzłami w klastrze. 
#Jeśli chcemy osobno leżące klustery, wartość withinss musi być jak najmniejsza.
#$tot.withinss = sum ( $withinss )
#$totss = $tot.withinss + $betweenss
plot(1:15, wss, type="b", xlab="Liczba grup", ylab="Suma wss")
library(fpc)
pamk(scaledcars)$nc   #liczba grup (klasterów) policzona automatycznie
pamk(mydata)$nc       #liczba grup (klasterów) policzona automatycznie
Sys.sleep(2)                             #pauza na 2 sekundy

#Grupowanie klasteryzacja w R np. K-means w trzech wymiarach
mdata3 <- mtcars[c('mpg','disp', 'hp')]  # wybieramy 3 parametry z mtcars ilość przejechanych mil na galon paliwa,
mtcars[c('mpg','disp', 'hp')]            # a także pojemność silnika i konie mechaniczne (miary z USA)
kmeans3.res <- kmeans(mdata3, 3)         # 3 zbiory odrębnych danych
kmeans3.cluster <- factor(kmeans3.res$cluster)
library(scatterplot3d)
scatterplot3d(mdata3,color=kmeans3.cluster,pch=19) #wizualizacja w 3D 
library(rgl)
#http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
r3dDefaults$windowRect <- c(0,50, 800, 800) 
plot3d(mdata3, col=kmeans3.cluster, size = 10)     #wizualizacja w 3D interaktywna
Sys.sleep(2)                                       #pauza na 2 sekundy

#Grupowanie klasteryzacja w R np. K-means w trzech obliczonych wymiarach z PCA  
#http://planspace.org/2013/02/03/pca-3d-visualization-and-clustering-in-r/
mydata<-mtcars
library(nFactors)
ev <- eigen(cor(mydata)) # get eigenvalues
ap <- parallel(subject=nrow(mydata),var=ncol(mydata),
               rep=100,cent=.05)
nS <- nScree(x=ev$values, aparallel=ap$eigen$qevpea)
plotnScree(nS) 
Sys.sleep(2)                             #pauza na 2 sekundy 
library(FactoMineR)                           # PCA Variable Factor Map
result <- PCA(mydata)                         # graphs generated automatically 
plot(result)
Sys.sleep(2)                             #pauza na 2 sekundy
pc <- princomp(mydata, cor=TRUE, scores=TRUE) #PCA obliczamy sztuczne 3 wymiary
summary(pc)
biplot(pc)
plot(pc,type="lines")
mdatapc<-pc$scores[,1:2]
str(mdatapc)
class(mdatapc)
kmeanspc.res <- kmeans(mdatapc, 4)                  # 4 zbiory odrębnych danych
kmeanspc.cluster <- factor(kmeanspc.res$cluster)
scatterplot3d(mdatapc,color=kmeanspc.cluster,pch=19)#wizualizacja w 3D 
r3dDefaults$windowRect <- c(0,50, 800, 800) 
plot3d(mdatapc, col=kmeanspc.cluster, size = 10)    #wizualizacja w 3D interaktywna
text3d(pc$scores[,1:2],texts=rownames(mtcars))  #dodajemy parametry z mtcars
text3d(pc$loadings[,1:2], texts=rownames(pc$loadings), col="red")
coords <- NULL
for (i in 1:nrow(pc$loadings)) {
  coords <- rbind(coords, rbind(c(0,0,0),pc$loadings[i,1:3]))
}
lines3d(coords, col="red", lwd=4)
table(kmeans.cluster, kmeanspc.cluster)
Sys.sleep(2)                                        #pauza na 2 sekundy

#Grupowanie klasteryzacja w R np. hierarchiczne grupowanie
nc <- 5
di <- dist(mtcars, method="euclidean")
tree <- hclust(di, method="ward.D2")
hcluster <- as.factor((cutree(tree, k=nc)-2) %% nc +1)
groups <- cutree(fit, k=nc)             # potnij drzewo na nc grup
plot(tree, xlab="")
rect.hclust(tree, k=nc, border="red")
table(hcluster, kmeanspc.cluster)
Sys.sleep(2)                             #pauza na 2 sekundy 

#Grupowanie Mclust
#https://cran.r-project.org/web/packages/mclust/vignettes/mclust.html
mydata <- scaledcars
library(mclust)
fit <- Mclust(mydata)
plot(fit, what = "BIC") 
summary(fit) 
Sys.sleep(2)                             #pauza na 2 sekundy

#Grupowanie klasteryzacja w R: pvclust hierarchiczne grupowanie
#wyniki wykorzystane w poniżej wyjaśnionych drzewach decyzyjnych i klasyfikatorach
mydata <- scaledcars
#http://www.sigmath.es.osaka-u.ac.jp/shimo-lab/prog/pvclust/
library(pvclust)
fitpv <- pvclust(t(mydata), method.dist="euclidean", method.hclust="ward.D2", nboot=1000)
plot(fitpv)
pvrect(fitpv, alpha=.9) 
pvgroup <- pvpick(fitpv, alpha=0.9) 
mydata$fit <- NA
for( i in seq_len(length(pvgroup$clusters))) mydata[pvgroup$clusters[[i]],]$fit = i
mydata$fit
Sys.sleep(2)                             #pauza na 2 sekundy
#http://www.sthda.com/english/wiki/beautiful-dendrogram-visualizations-in-r-5-must-known-methods-unsupervised-machine-learning
#pvclust and dendextend
#plot(fit)
#fit %>% as.dendrogram %>% 
#  set("branches_k_color", k = 2, value = c("purple", "orange")) %>%
#  plot
#fit %>% text
#fit %>% pvrect

#Drzewa decyzyjne rpart
#http://www.statmethods.net/advstats/cart.html
#http://machinelearningmastery.com/non-linear-regression-in-r-with-decision-trees/
library(rpart)
# hoduj drzewo na zbiorze trenującym mydatatr z etykietami fit 
# potem je użyj na zbiorze mydatana do określenia brakujących etykiet fit
mydata$fit <- factor(mydata$fit)        #stosuje mydata z pvclust poprzedni przykład
mydatana <- mydata[is.na(mydata$fit),]  #zbiór z fit=NA z pvclust przycięcia
mydatatr <- na.omit(mydata)             #uczę na przykładzie z etykietą fit != NA
#?rpart.control                         #ustaw parametry rpart
fitree <- rpart(fit ~ ., method="class", data=mydatatr, minsplit=2)
#fitree <- rpart(fit ~ ., method="anova", data=mydata)
printcp(fitree)                         # wyswietlam rezultaty
plotcp(fitree)                          # vizualizuję krossvalidację
summary(fitree)                         # podsumowanie
# utwórz rysunek
par(mar=c(0,5,3,5))
plot(fitree, uniform=TRUE,
     main="Decyzyjne drzewo dla mtcars z etykietą fit (pvclust)")
text(fitree, use.n=TRUE, all=TRUE, cex=.8)
#zapisz rysunek do pliku
post(fitree, file = "treerpart1.pdf",
     title = "Decyzyjne drzewo dla mtcars z etykietą fit (pvclust)")
Sys.sleep(2)                            #pauza na 2 sekundy
# przytnij drzewo
pfitree<- prune(fitree, cp=   fit$cptable[which.min(fitree$cptable[,"xerror"]),"CP"])
# utwórz rysunek
plot(pfitree, uniform=TRUE,
     main="Przycięte decyzyjne drzewo dla mtcars z etykietą fit (pvclust)")
text(pfitree, use.n=TRUE, all=TRUE, cex=.8)
#zapisz rysunek do pliku
post(pfitree, file = "treerpart2.pdf",
     title = "Przycięte decyzyjne drzewo dla mtcars z etykietą fit (pvclust)")
predrest1 <- predict(fitree, mydatana)   #przewiduj etykietę zbioru z fit=NA
predrest1
Sys.sleep(2)                             #pauza na 2 sekundy

#Drzewa Decyzyjne ctree
library(party)
ctree <- ctree(fit ~ ., data=mydatatr, controls = 
    ctree_control(mincriterion = 0,minbucket = 0,minsplit = 0,maxdepth = 100,savesplitstats = TRUE))
plot(ctree)
pdf('treec.pdf')
plot(ctree)
dev.off()
#plot(ctree, type="simple")
predrest2 <- predict(ctree, mydatana)    #przewiduj etykietę zbioru z fit=NA
predrest2
Sys.sleep(2)                             #pauza na 2 sekundy

#Drzewa Decyzyjne randomForest - wiele drzew i głosowanie
#zmienna liczba atrybutów dla każdej próby tworzenia drzewa
library(randomForest)
fitforest <- randomForest(fit ~ ., data=mydatatr)
print(fitforest)                         # zobacz rezultaty
#summary(fitforest)
importance(fitforest)                    # importance of each predictor 
predrest3 <- predict(fitforest, mydatana)#przewiduj etykietę zbioru z fit=NA
predrest3
Sys.sleep(2)                             #pauza na 2 sekundy

#Bootstrapped Aggregation (Bagging) drzewa na różnych próbkach i głosowanie
library(ipred)
fitbag <- bagging(fit~., data=mydatatr, control=rpart.control(minsplit=5))
print(fitbag)                            #zobacz rezultaty
#summary(fitbag)                         #zbyt duży zbiór to zakomentowane
predrest4 <- predict(fitbag, mydatana)   #przewiduj etykietę zbioru z fit=NA
predrest4
Sys.sleep(2)                             #pauza na 2 sekundy

#Mixture Discriminant Analysis
library(mda)
fitmda <- mda(fit~., data=mydatatr)
print(fitmda)                            #zobacz rezultaty
summary(fitmda)
predrest5 <- predict(fitmda, mydatana)#przewiduj etykietę zbioru z fit=NA
predrest5
Sys.sleep(2)                             #pauza na 2 sekundy

#Regularized Discriminant Analysis
library(klaR)
fitrda <- rda(fit~., data=mydatatr)
print(fitrda)                            #zobacz rezultaty
summary(fitrda)
predrest6 <- predict(fitrda, mydatana)   #przewiduj etykietę zbioru z fit=NA
predrest6
Sys.sleep(2)                             #pauza na 2 sekundy

#UWAGA!!!! Można zauważyć, że wyniki etykietowania zbioru z nieznaną wartością fit z pvclust
#predrest1, predrest2, predrest3, predrest4, predrest6 są takie same

#Gradient Boosted Machine 
#http://www.listendata.com/2015/07/gbm-boosted-models-tuning-parameters.html
library(gbm)
fitgbm <- gbm(fit~., data=mydatatr, distribution="gaussian", 
              bag.fraction = 0.5, n.trees = 1000, interaction.depth =6, 
              shrinkage = 0.1, n.minobsinnode = 1)
print(fitgbm)                            #zobacz rezultaty
summary(fitgbm)
predrest7 <- predict(fitgbm, mydatana,n.trees = 10)   #przewiduj etykietę zbioru z fit=NA
round(predrest7)
Sys.sleep(2)                             #pauza na 2 sekundy
#UWAGA !!! Tutaj wynik predrest7 od poprzednich predict, podobnie jak predrest5(mda)

#Niesprawdzone w tym skrypcie funkcje funkcja(pakiet) m.in:
#fda(mda), kernlab(ksvm), knn3(caret), naiveBayes(e1071), nnet(nnet), qda(MASS)
#J48(RWeka), PART(RWeka), C5.0(C50), vglm(VGAM), lda(MASS), plsda(caret)
#earth(earth), knnreg(caret), glmnet(glmnet), lars(lars), glmnet(glmnet)
#pcr(pls), plsr(pls)
#http://machinelearningmastery.com/how-to-get-started-with-machine-learning-algorithms-in-r/
