#Zasady ogólne:
#indeksy w R zaczynają się od 1, a nie od 0
#wielkość liter ma znaczenie
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
source('file.R', echo=TRUE)
# wypisz litery i zmienne
i <- 10
cat(i, "th interation...\n", sep="") # sep="" means that there is no space between the input parameters 
# sprawdź strukturę danych
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
x <- c(1,2)
rm(x)
# usuń wszystkie zmienne z pamięci przestrzeni roboczej
rm(list=ls(all=T))
rm(list=ls(pattern='^tmp')) # usuń zmienne z nazwami zaczynającymi się na 'tmp'
# wyjdź z rstudia
quit() # zapyta czy zapisać przestrzeń roboczą

Struktury danych
#wektory mają tylko jeden typ danych

a <- c(1, 2, 5, 3, 6, -2, 4)
a[3]  # 5
a[c(1, 3, 5)] # 1 5 6
a[2:6] # 2 5 3 6 -2
b <- replicate(10, 2) # generuje wektor z długością 10, wszystkie elementy to 2
b <- rep(2, 10) # generuje wektor z długością 10, wszystkie elementy to 2
b <- seq(1,10) # b równa się 1, 2, 3, 4, 5, 6, 7, 8, 9 10
b <- seq(1, 10, 2) # b równa się 1, 3, 5, 7, 9
# dodaj element do wektora: metoda 1
e <- 10
a <- c(a, e)
# dodaj element do wektora: metoda 2
a <- append(a, e)
# dodaj element do wektora: metoda 3
a[length(a)+1] <- e
# usuń element z wektora
index <- 2
a <- a[-index] # usuń drugi element z wektora
index <- c(2, 5)
a <- a[-index] # usuń drugi i piąty element z wektora
# znajdź indeks pierwszego pasującego elementu
# dla przykładu, znajdź 10-ty indeks w wektorze vec <- c(1, 10, 2, 10). 
# match(10, vec) zwróci 2, następny indeks dla 10 nie będzie zwracany
vec <- c(2:10)
e <- 3
e2 <- c(3, 5)
index <- match(e, vec)
index2 <- match(e2, vec)
# sprawdź, czy element znajduje się w wektorze
e3 <- 1
is_in <- !is.na(match(e3, vec))
is_in <- e3 %in% vec

# elementy z wektoru c1 nie znajdujące się w c2
c1 <- c(1, 2, 3)
c2 <- c(2, 3, 5)
c1[!(c1 %in% c2)] # 1
setdiff(c1, c2) # 1
# elementy z wektoru c2 nie znajdujące się w c1
c2[!(c2 %in% c1)] # 5
setdiff(c2, c1) # 5

# oblicz ile jest nie powtarzających się numerów
vec <- c(1, 2, 3, 2)
nlevels(factor(vec)) # zwraca 3
length(unique(vec)) # zwraca 3

# liczba znaków w ciągu (łańcuchu znaków)
x <- 'abc'
num <- nchar(x)

# znajdź pozycję znaku w ciągu 
loc <- gregexpr(pattern='\"', "abc\"defg") # \" jest pojedyńczym znakiem, loc jest listą
cat('Pozycja znaku: ', loc[[1]][1], '\n')

# konwersja łańcucha znaków do całkowitej
x <- 123
x <- paste(x) # x równa się '123'
x <- strtoi(x) # x równa się 123

#Macierz: dwuwymiarowa tablica

cells <- c(1, 26, 24, 68)
rnames <- c('R1', 'R2')
cnames <- c('C1', 'C2')
# wypełnij macierz po wierszach
mymatrix <- matrix(cells, nrow=2, ncol =2, byrow=TRUE,
                   dimnames=list(rnames, cnames))
# wypełnij macierz po kolumnach, to domyślne ustawienie
mymatrix <- matrix(cells, nrow=2, ncol=2, byrow=FALSE,
                   dimnames=list(rnames, cnames))
# użyj macierzy indeksów
x <- matrix(1:10, nrow=2)
x
x[2,]
x[,2]
x[1,4]
x[1,c(4,5)]

#Tablica array: podobna do macierzy matrix, ale może mieć więcej wymiarów

dim1 <- c('A1','A2')
dim2 <- c('B1', 'B2', 'B3')
dim3 <- c('C1', 'C2', 'C3', 'C4')
z <- array(1:24, c(2,3,4), dimnames=list(dim1, dim2, dim3))
z
z[1,2,3]

#Ramka danych: kolumny mogą mieć różne typy

patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c('Type1', 'Type2', 'Type1','Type1')
status <- c('Poor', 'Improved', 'Excellent', 'Poor')
patientdata <- data.frame(patientID, age, diabetes, status)
x <- nrow(patientdata)
y <- ncol(patientdata)
size <- dim(patientdata) # rozmiar to wektor z liczbą wierszy i kolumn
# podaj ij-ty element ramki danych
i <- 1
j <- 2
patientdata[i, j]
patientdata[[j]][i]
patientdata[, j][i]
as.datatype(patientdata[i, ][j]) # patientdata[i, ] i-ty wiersz jako ramka danych
patientdata$age[i]
patientdata[i, 'age']  # i-ty wiersz kolumny age

# podaj wiersz i kolumnę ramki danych
patientdata[2, ] # drugi wiersz jako ramka danych
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
# dodaj kolumnę do ramki danych: metoda 2
patientdata <- transform(patientdata, new_col=c(2:5))
# dodaj wiersz do ramki danych
new_row <- data.frame(patientID=5, age=10, diabetes='Type3', status='Good')
patientdata <- rbind(patientdata, new_row)
# usuń kolumnę do ramki danych: metoda 1
patientdata$new_col <- NULL
# usuń kolumnę do ramki danych: metoda 2
patientdata <- transform(patientdata, new_col=NULL)
# usuń wiersz z ramki danych
index <- 2
patientdata <- patientdata[-index, ]
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
diabetes <- factor(diabetes) # Type1-1 Type2-2
status <- c('Poor', 'Improved', 'Excellent', 'Poor') 
status <- factor(status, ordered=TRUE) # Excellent-1 Improved-2 Poor-3
status2 <- factor(status, ordered=TRUE, levels=c('Poor', 'Improved', 'Excellent')) # Excellent-3 Improved-2 Poor-1

# przykład pokazujący faktory zmienne jakościowe
patientID <- c(1, 2, 3, 4)
age <- c(25, 34, 28, 52)
diabetes <- c("Type1", "Type2", 'Type1', 'Type1')
status <- c('Poor', 'Improved', 'Excellent', 'Poor')
diabetes <- factor(diabetes)
status <- factor(status, ordered=TRUE)
patientdata <- data.frame(patientID, age, diabetes, status)
str(patientdata)
summary(patientdata)

#Lista: uporządkowany zbiór danych

g <- 'My First List'
h <- c(25, 26, 18, 39)
j <- matrix(1:10, nrow=5)
k <- c('one', 'two', 'three')
mylist <- list(title=g, ages=h, j, k)
mylist
mylist[[2]]
mylist[['ages']]
mylist[[2]][1] # 25, pierwszy element drugiego argumentu z listy

#wstawianie danych z pliku csv

# czytaj plik csv
signal <- read.table(fileDir, header=FALSE, sep=',')
# exportuj dane do pliku csv
write.table(data, 'data.csv', row.names=F, col.names=F, sep=',')

#zapisz rysunki do plików

# pierwszy rysunek
pdf('graph1.pdf')
plot()
dev.off()

# drugi rysunek
pdf('graph2.pdf')
plot()
dev.off()

#dwa wykresy na jednym rysunku
x <- c(1:5)
y1 <- 2*x
y2 <- 3*x
plot(x, y1, type='l', col='red', xlab='day', ylab='Net Value')
lines(x, y2, type='l', col='green')

# etykietuj osie z pomocą legend(), lwd - grubość linii, bty='n' - brak ramki wokół
legend('topleft', legend=c('line 1', 'line 2'), col=c('red', 'green'), lwd=1, bty='n') 

# rysuj funkcję
f <- function(x) {
  x*sin(x)
}
plot(f, -20*pi, 20*pi)

#narysuj wiele rysunków w tym samym czasie

# pierwszy rysunek
plot()
# drugi rysunek
# otwórz nowy rysunek
dev.new()
# przelączanie strzałkami z klawiatury
plot() 

#Zarządzanie, manipulacja strukturami danych 

#dodawanie nowych zmiennych do ramki danych

mydata <- data.frame(x1 = c(2, 2, 6, 4),
                     x2 = c(3, 4, 2, 8))

# dodawanie nowych zmiennych do ramki danych
# mamy trzy metody

# metoda 1
mydata$sumx <-  mydata$x1 + mydata$x2
mydata$meanx <- (mydata$x1 + mydata$x2)/2

# metoda 2
attach(mydata)
mydata$sumx <-  x1 + x2
mydata$meanx <- (x1 + x2)/2
detach(mydata)

# metoda 3
mydata <- transform(mydata, sumx = x1 + x2, meanx = (x1+x2)/2)

#2 recoding variables

mydata <- data.frame(x1 = c(2, 2, 6, 4),
                     x2 = c(3, 4, 2, 8))
# recoding variables
# method 1
#mydata$age[mydata$x1==2] <- 1
#mydata$tmp <- mydata$x1 * mydata$x2

# method 2
mydata <- within(mydata, {
  age <- NA # create a new variable age for data frame mydata and intialize it as NA
  age[x1==2] <- 1
  tmp <- x1*x2
})

3 renaming variables

rename(dataframe, c(oldname1="newname1", oldname2="newname2",...))
#names(dataframe) returns the variable names of the data frame as a vector
names(dataframe)[2] <- "newname"
# or use fix(dataframe) to rename variables in GUI

4 missing values

leadership$age[leadership$age==99] <- NA
x < - c(1, 2, NA, 3)
y <- x[1] + x[2] + x[3] + x[4] # y will be NA
z <- sum(x) # z will be NA
z <- sum(x, na.rm=TRUE) # na.rm=TRUE removes the missing values
newdata <- na.omit(leadership) # na.omit() deletes any rows with missing data

5 Date values
(1) store character date as a data variable

# x is character date
# default input format is yyyy-mm-dd
mydate <- as.Date(x, 'input_format')

(2) transform date to a specific format and to extract portions of dates

# x is a date variable
# default output format is yyyy-mm-dd
mydate <- format(x, format='output_format') 

(3) calculate a time interval and express it in seconds, minutes, hours, days, or weeks,

# dataX and dataY are data variables
interval <- difftime(dateX, dateY, units='weeks')

(4) Sys.Date() returns today's date and date() returns the current date and time

# record the running time of a program
t1 <- proc.time()
code 
t2 <- proc.time()
time_elapsed <- (t2-t1)[[3]] # the elapsed time
time_elapsed <- as.numeric((t2-t1)[3]) # the elapsed time, the same as the above value

6 Type conversion (page 84)
is.datatype() return TRUE or FALSE
as.datatype() converts the argument to that type

7 Sort data frame (By default, the sorting order is ascending)

# sort the rows from youngest to oldest
newdata <- leadership[order(leadership$age),]

# sort the rows from female to male, and from youngest to oldest within each gender
attach(leadership)
newdata <- leadership[order(gender, age), ]
detach(leadership)

# sort the rows from female to male, and from oldest to youngest within each gender
attach(leadership)
newdata <- leadership[order(gender, -age), ]
detach(leadership)

8 Merge data sets
(1) add columns

# merge dataframeA and dataframeB by ID (inner join)
total <- merge(dataframeA, dataframeB, by="ID")
# merge dataframeA and dataframeB by ID and Country (inner join)
total <- merge(dataframeA, dataframeB, by=c('ID', 'Country'))
# horizontally concatenate dataframeA and dataframeB
# dataframeA and dataframeB must have the same number of rows and the row order should be the same
total <- cbind(dataframeA, dataframeB)

(2) add rows

# the two data frames must have the same variables
total <- rbind(dataframeA, dataframeB)

9 Subsetting data sets
(1) Keeping variables

c1 <- c(1, 2, 3)
c2 <- c(4, 5, 6)
c3 <- c(7, 8, 9)
data <- data.frame(c1, c2, c3)
newdata <- data[1]
newdata <- data['c1']
x <- c('c1', 'c2)
newdata <- data[x]
newdata <- data[, 1]
newdata <- data[, 1:2]
newdata <- data[, c(1:2)]
newdata <- data[1,]
newdata <- data[1:2,]

(2) Excluding variables

# delete variables q3 and q4
myvars <- names(leadership) %in% c('q3', 'q4')
newdata <- leadership[!myvars]
# delete the 8th and 9th variables
newdata <- leadership[c(-8, -9)]
# delete variables q3 and q4
leadership$q3 <- leadership$q4 <- NULL

(3) Selecting observations

newdata <- leadership[1:3,]
newdata <- leadership[which(leadership$gender=='M' & leadership$age>30),]
attach(leadership)
newdata <- leadership[which(gender=='M' & age>30),]

leadership$date <- as.Date(leadership$date, '%m/%d/%y')
startdate <- as.Date('2009-01-01')
enddate <- as.Date('2009-10-31')
newdata <- leadership[which(leadership$date>=startdate & leadership$date<=enddate),]

# use package dplyr (install first)
newdata <- filter(leadership, gender=='M' & age>30)

(4) The subset() function

newdata <- subset(leadership, age>=35 | age<24, select=c(q1, q2, q3, q4))
newdata <- subset(leadership, gender=='M' | age>25, select=gender:q4)

(5) Random samples

mysample <- leadership[sample(1:nrow(leadership), 3, replace=FALSE), ]

(6) Using SQL to manipulate data frames

library('sqldf')
newdf <- sq1df('select * from mtcars where carb=1 order by mpg', row.names=TRUE)
newdf <- sqldf('select avg(mpg) as avg_mpg, avg(disp) as avg_disp, gear from mtcars where cyl in (4, 6) group by gear')

10 Conversion between string and variable name

# string to variable name
assign('test', 10) # same as test <- 10
x <- 'test'
assign(x, 5) # same as test <- 5

# variable name to string
x <- 5
var.name <- deparse(substitute(x)) # var.name will be "x"

Advanced data management

1 Numerical and character functions

Mathematical functions
sqrt(x), floor(x), log(x), exp(x)
Statistical functions
mean(x), median(x), sd(x), var(x), range(x), sum(x), scale(x, center=TRUE, scale=TRUE)
Probability functions (page 96)
[dpqr]distribution_abbrebiation (d=density, p=distribution function, q=quantile function, r=random generation)
runif(): uniform distribution on the interval 0 to 1
set.seed(5): set seed to make the results reproducible
Character functions
nchar(x), nzchar(x), substr(x, start, stop), grep(pattern, x, ignore.case=FALSE, fixed=FALSE)
sub(pattern, replacement, x, ignore.case=FALSE, fixed=FALSE), strsplit(x, split, fixed=FALSE)
paste(..., sep=""), toupper(x), tolower(x)
Other useful functions
length(x), seq(from, to, by), rep(x, n), cut(x, n), pretty(x, n), cat(.., file='myfile', append=FALSE)
Apply functions to matrices and data frames
apply(x, MARGIN, FUN, ...)
x is the data object, MARGIN is the dimension index, 1 indicates rows, 2 indicates columns, FUN is a function you specify, and ... are any parameters you want to pass to FUN

2 Control flow

if/else, ifelse, switch

score <- 0.6
if (score > 0.5) {
  outcome2 <- 'passed' 
} else {
  outcome2 <- 'not passed'
}
outcome <- ifelse(score>0.5, 'passed', 'not passed')
switch(expr, ...)

Note:
  "... When the if statement is not in a block the else, if present, must appear on the same line as statement1. Otherwise the new line at the end of statement1 yields a syntactically complete statement that is evaluated. ..."

for, while
for (var in seq) statement
while (cond) statement

for (i in 1:10) {
  print('hello world')
}
# pay attention to the following example, it will print 1 and 0
for (i in 1:0) {
  print(i)
}

i <- 10
while(i>=0) {
  print('hello world')
  i <- i-1
}

3 User-defined functions
myfunction <- function(arg1, arg2, ...) {
  statements
  return(object)
}

mystats <- function(x, parametric=TRUE, print=FALSE) {
  if (parametric) {
    center <- mean(x)
    spread <- sd(x)
  } else {
    center <- median(x)
    spread <- mad(x)
  }
  if(print&parametric) {
    cat('Mean=', center, '\n', 'SD=', spread, '\n')
  } else if(print&!parametric){
    cat('Median=', center, '\n', 'MAD=', spread, '\n')
  }
  result <- list(center = center, spread=spread)
  return(result)
}

4 Aggregation and restructuring

aggregate(x, by, FUN)
x is the data object to be collapsed, by is a list of variables that will be crossed to form the new observations, and FUN is the scalar function used to calculate summary statistics that will make up the new observation values.
reshape
install.packages('reshape'), library('reshape')
melt(data, id=(c('id', 'time'))), cast(md, id~variable, mean)

Clustering in R

K-means clustering

signal.return <- data.frame(signal.value, return.rate)
kmeans.res <- kmeans(signal.return, 3) # 3 clusters, give each observation a label
plot(signal.return, xaxt='n', yaxt='n', xlab='Signal Value', ylab='Return')
axis(1, pos=0)
axis(2, pos=0)
abline(v=0, h=0)
kmeans.cluster <- factor(kmeans.res$cluster)
# need to install package 'ade4' to visualize the clusters
s.class(signal.return, fac=kmeans.cluster, add.plot=TRUE, col=seq(1, nlevels(kmeans.cluster), 1))



#stepfirst
fn <- function (x) {
  ifelse(x>46 & x<52, 1, 0)
}    
res <- fn(40:60)

fn <- function (x,y) {
  ifelse(x>46 & x<52 & y<12, 1, 0)
}    
datagrid <- expand.grid(i = 40:60, j = 0:20)
res <- fn(datagrid$i, datagrid$j)

#An other option is to use the functions for the apply-family

fn <- function (x) {
  ifelse(x>46 & x<52, 1, 0)
}    
res <- sapply(40:60, fn)

fn <- function (x,y) {
  ifelse(x>46 & x<52 & y<12, 1, 0)
}    
datagrid <- expand.grid(i = 40:60, j = 0:20)
res <- apply(datagrid, 1, function(z){
  fn(z["i"], z["j"])
}) 

#or you can use a nested loop

res <-NULL 
for (i in 40:60){
  for(j in 0:20){
    res <-c(res,fn(i,j))
  }
}
