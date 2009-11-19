Sys.setlocale("LC_NUMERIC","C")

#lista pakietów z CRAN-u
pkglist<-c("plyr","mlbench","boot","Hmisc","RWeka","ipred","klaR","ROCR","rpart","dprep","maptree","party","grid","lattice","latticeExtra","playwith","ada","randomForest","kknn","e1071","cluster","class","caret","fda","zoo","lattice","deal","RJDBC","cairoDevice")
pkgcheck <- pkglist %in% row.names(installed.packages())
for(i in pkglist[!pkgcheck]){
	install.packages(i,depend=TRUE)
}

#lista pakietów z bioconductora
biolist<-c("Rgraphviz")
biocheck <- biolist %in% row.names(installed.packages())
for(i in biolist[!biocheck]){
    source("http://bioconductor.org/biocLite.R")
	#update.packages(repos=biocinstallRepos(), ask=FALSE)
	biocLite(i)
}

for(i in pkglist)
{ library(i, character.only = TRUE);}

for(i in biolist)
{ library(i, character.only = TRUE);}

#############################################################################
#Funkcje podstawowe niezbêdne do normalnego odzyskiwania wiedzy
#############################################################################

#Funkcja evalwithattr wywo³uje podan± funkcjê z wyj¶ciowym atrybutem output i 
#wej¶ciowymi atrybutami parvec oraz zbiorem danych
evalwithattr<-function(oFunction,output,parvec,oData)
{
	tmp=paste(deparse(substitute(oFunction)),"(",output,"~")
	j=0
	for(i in parvec){
		j=j+1
		if(j==1) tmp<-paste(tmp,i)
		else 	 tmp<-paste(tmp,"+",i)
	}
	tmp<-paste(tmp,",data=",deparse(substitute(oData)),")",sep="")
	return(eval(parse(text=tmp)))
}

#Funkcja defactor.numeric najpierw defaktoryzuje, a potem oznacza jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi, tak na wszelki wypadek, gdyby csv ¼le siê wczyta³ (w przypadku zbiorów data() to tylko æwiczenie)
defactor.numeric<-function (DataSet, parvec)         
{
	DataSet<-unfactorto(DataSet, which(names(DataSet) %in% parvec))
	for(i in 1:ncol(DataSet)){
        	if(i%in%which(names(DataSet) %in% parvec))
	        DataSet[,i]<-as.numeric(DataSet[,i])
	}
 	return (DataSet)
}

#Funkcja zscore.for.integer zetskoruje wybrane kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi po kolumnie zdyskretyzowanej (etykiecie) integercolumnforzscore dla poszczególnych jej warto¶ci
zscore.for.integer<-function (DataSet, parvec, integercolumnforzscore)         
{
indata<-DataSet
for(i in c(sort(unique(DataSet[[integercolumnforzscore]])))){
indata[(indata[[integercolumnforzscore]]==i),]=zscore(indata[(indata[[integercolumnforzscore]]==i),],which(names(indata) %in% parvec))
}
return(indata)
}

#Funkcja discret.for.chosen dyskretyzujê atrybuty (kolumny) z parvec na levelnum poziomów
disc.for.chosen<-function (DataSet, parvec, levelnum)         
{
DataSetd<-DataSet
DataSetd[names(DataSet) %in% parvec]<-disc.ef(DataSet[names(DataSet) %in% parvec], which(names(DataSetz) %in% parvec), levelnum)
DataSetd<-factorto(DataSetd, which(names(DataSetd) %in% parvec))
return(DataSetd)
}

#Funkcja KruskelMDS generuje z daisy ró¿nice miêdzy wierszami podanego zbioru i wprowadza do isoMDS rzutuj±cego na wymiary k=dimnum (jak siê pojawi± dwa takie same wiersze to wyrzuca b³±d, dlatego na samym pocz±tku usuwa³em wiersze z Glass
KruskelMDS<-function (DataSet, parvec, dimnum)         
{
return(isoMDS(daisy(DataSet[,which(names(DataSet)%in%parvec)]),k=dimnum))
}

plotMDS.for.chosen<-function (fname, nDataSets, DataSet, parvec, wzorzec1)         
{
DataSet1<-DataSet[,which(names(DataSet)%in%parvec)]
for(i in which(names(DataSet1)%in%parvec)){
wzorzec=DataSet1[,i]
#wzorzec1=2
if(i < 10){
zapisz_pplot(nDataSets,paste(fname,"_0",i,parvec[i],sep=""),wzorzec,wzorzec1,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),3.5)
#zapisz_pplot(nDataSets,paste(fname,"_0",i,parvec[i],sep=""),wzorzec,2,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),wzorzec1)
}else{
zapisz_pplot(nDataSets,paste(fname,"_",i,parvec[i],sep=""),wzorzec,wzorzec1,c('yellow','black','green','red','blue','cyan','magenta','pink'),c(17,16,15,18,20,9,10,12),3.5)
}
}
}

#Funkcja disc2 wykonywana w funkcji disc.ef
disc2<-function (x, k) 
{
	z = x
    	n = length(x)
	f = seq(n)
	if(sum(is.na(x))!=0)
	{
		f<-f[-which(is.na(x))]
		x<-x[-which(is.na(x))]
	}
	m = length(x)
	ciclo = ceiling(m/k)
	y = x
	for (i in 1:(k - 1)) {
	    y[order(x)[((i - 1) * ciclo + 1):(i * ciclo)]] = i
	}
	y[order(x)[((k - 1) * ciclo + 1):m]] = k
	z[f]<-y; 
	return(z)
}

#Funkcja disc.ef z pakietu dprep zmieniona (pomija nulle), 
#gdy¿ w tym pakiecie prawie wszystkie funkcje wymagaj± zmian 
disc.ef<-function (indata, varcon, k)        # Nastêpuje dyskretyzacja danych
{
    indata = as.matrix(indata)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
            indata[, i] <- disc2(as.vector(indata[, i]), k)
        }
    }
    indata
}

#Funkcja zscore zeskoruje dane ca³kowite lub zmiennoprzecinkowe 
#czyli odejmuje ¶redni± i dzieli przez standardowe odchylenie
zscore<-function (indata, varcon)        # Nastêpuje dyskretyzacja danych
{
#    indata = as.matrix(indata)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
			z = indata[, i]
			x = indata[, i]
    		n = length(x)
			fs = seq(n)
			if(sum(is.na(x))!=0)
			{
				fs<-fs[-which(is.na(x))]
				x<-as.numeric(x[-which(is.na(x))])
			} 
			if (sum(x!=0) != 0) {
            x<-(x-mean(x))/sd(x)
			}
			z[fs]=x
			indata[, i]=z
        }
    }
    indata
}

#Funkcja factorto faktoryzuje dane wej¶ciowe z domy¶lnymi poziomami
factorto<-function (indata, varcon)  
{
	varcon <- as.vector(varcon)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
            indata[, i] <- factor(indata[, i])
        }
    }
    indata
}

#Funkcja unfactorto defaktoryzuje dane wej¶ciowe z ustalonymi poziomami
unfactorto<-function (indata, varcon)  
{
	varcon <- as.vector(varcon)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
			if(!is.na(as.numeric(as.character(indata[1,i])))){
            	indata[, i] <- as.numeric(as.character(indata[, i]))
			}
			if(is.na(as.numeric(as.character(indata[1,i])))){
            	indata[, i] <- as.character(indata[, i])
			}
        }
    }
    indata
}

#Funkcja "zmianana"  zamienia warto¶ci 9; 99; 99.9 na NA
zmianana<- function (kolumna_wyjatek, wartoscNA)         
{
 	x<-kolumna_wyjatek
    is.na(x)<-which(x==wartoscNA) 
 	return (x)
}

#Funkcja z4na3 zamienia przedzia³y 1,2,3,4 na 1,2,2,3
z4na3<-function (indata, varcon)
{
    indata = as.matrix(indata)
    p <- dim(indata)[2]
    f <- p
    ft <- rep(0, f)
    for (i in 1:length(varcon)) {
        ft[varcon[i]] = 1
    }
    for (i in 1:f) {
        if (ft[i] > 0) {
		 	if(sum(!is.na(indata[indata[,i]==3, i])))
            	indata[indata[,i]==3, i] = 2;
		 	if(sum(!is.na(indata[indata[,i]==4, i])))
            	indata[indata[,i]==4, i] = 3;
        }
    }
    indata
}

#Funkcja zapisz_pplot zapisuje wykres rzutu wielowymiarowego w pliku jpeg
zapisz_pplot = function (npointszds,fname,wzorzec1,wzorzec2,co,pc,scex)
{
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=scex)
	#plot(npointszds$points, type="p", col=co[wzorzec1], pch=pc[wzorzec2], cex=c(2,3.5,5,6.5,8,9.5,11))
	dev.off()
}

#Funkcja zapisz_weka zapisuje wykres drzewa uzyskanego procedurami RWeka w pliku png
zapisz_weka = function (drzewo, fname)
{
	#dotname=paste(fname,".dot",sep="")
	dotname=tempfile()
	pngname=paste(fname,".png",sep="")
	write_to_dot(drzewo,dotname)
	system(paste("dot -Tpng ",dotname," > ",pngname,sep=""))
}

#Funkcja zapisz_rpart zapisuje wykres drzewa uzyskanego procedurami rpart w pliku jpeg
zapisz_rpart = function (drzewo, fname)
{
    jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(lwd=4)
	draw.tree(drzewo, cex=3.3, pch=1.0, print.levels=TRUE)
   # plot(drzewo,uniform=T,branch=0.3,compress=T,margin=0.02)
   # text(drzewo,all=T,use.n=T, fancy=T)
    dev.off()
}

#Funkcja hier2jpg zapisuje dendogram w pliku jpeg
hier2jpg<-function(inmethod,indata,fname){
indatanum<-indata[, sapply(indata, class) == "numeric"]
cc <- cor(indatanum, use="pairwise", method=inmethod)
# Generate hierarchical cluster of variables.
hc <- hclust(dist(cc), "ave")
# Generate the dendrogram.
dn <- as.dendrogram(hc)
# Now draw the dendrogram.
#op <- par(mar = c(3, 4, 3, 2.86))
jpeg(file=paste("/media/disk/guest/obrazki/",fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
par(mar=c(9,9,9,9))
plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "black"), lab.cex = 3.75, cex.main = 1.8, cex.axis = 1.2,  lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2))
title(main=paste("Variable Correlation Clusters ",as.character(substitute(indata)),"using",inmethod),cex.main=2)
#par(op)
dev.off()
}

#Funkcja latt2jpg zapisuje lattice w pliku jpeg
latt2jpg<-function(indata, gvec, fname){
dat<-indata[, sapply(indata, class) == "numeric"]
jpeg(file=paste("/media/disk/guest/obrazki/",fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
## Assuming that the data are attached and any
## customised style settings are in place; save with
## myStyle <- trellis.par.get(); then restore with
## trellis.par.set(myStyle)
fvec=as.character(substitute(gvec))
print(marginal.plot(dat, data = dat, groups = gvec, par.settings = list(cex=2.6), auto.key = list(lines = TRUE, title = paste("Variable Plots by ",fvec[3]), cex.title = 3, columns = 2)))
opar <- trellis.par.set(list(plot.symbol = list( cex = 2.6), dot.symbol = list( cex = 2.6), par.main = list( cex = 2.6), par.sub.text = list( cex = 2.6), par.xlab.text = list( cex = 2.6), plot.line = list(), plot.polygon = list(), superpose.symbol = list(cex = 2.6), superpose.line = list(), superpose.polygon = list()))
#latticeStyleToBasePar()
on.exit(trellis.par.set(opar))
dev.off()
}

