# zbiór funkcji do zajêæ ISO WIT 
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

Sys.setlocale("LC_NUMERIC","C")

#lista pakietów z CRAN-u
#naprawiæ pakier gRain
#pkglist<-c("gRain","splines","betareg","ellipse","nlme","MASS","leaps","car","lmtest","gregmisc","foreign","plyr","mlbench","boot","Hmisc","RWeka","ipred","klaR","ROCR","rpart","dprep","maptree","party","grid","lattice","latticeExtra","playwith","ada","randomForest","kknn","e1071","cluster","class","caret","fda","zoo","lattice","deal","RJDBC","cairoDevice")
pkglist<-c("bootstrap","DAAG","ff","biglm","bigmemory","splines","betareg","ellipse","nlme","MASS","leaps","car","lmtest","gregmisc","foreign","plyr","mlbench","boot","Hmisc","RWeka","ipred","klaR","ROCR","rpart","dprep","maptree","party","grid","lattice","latticeExtra","playwith","ada","randomForest","kknn","e1071","cluster","class","caret","fda","zoo","lattice","deal","RJDBC","cairoDevice")
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
#Funkcje podstawowe zalecane do procesu odkrywania wiedzy
#############################################################################

#Funkcja prederror zwraca warto¶ci predykcji klasyfikatora, tablicê rezultatów i rzeczywistych warto¶ci, 
#oraz b³±d klasyfikatora, na wej¶ciu dane testowe, ale nie treningowe wykorzystane do konstrukcji klasyfikatora
prederror<-function(classimod,paroutputree,parvectree,DataSet,EvalString="DEFAULT"){
	if (EvalString == "LDA"){
		predvalues=predict(classimod,DataSet[,parvectree])$class
	} else if(EvalString=="DEFAULT"){
		predvalues=predict(classimod,newdata=DataSet[,parvectree],"class");
	}  else {
		predvalues=predict(classimod,DataSet[,parvectree]);
	}
	tabresults=table(predicted=predvalues, real=DataSet[,paroutputree])
	prederr<-1-sum(diag(tabresults))/sum(tabresults)
	l<-list()
	l[["pvalues"]]<-predvalues
	l[["table"]]<-tabresults
	l[["perror"]]<-prederr
	l
}


#Funkcja permbesteval liczy permutacje zmiennych niezale¿nych dla n=1 do n równego ilo¶æ wszystkich zmiennych 
#i oblicza dla nich funkcje get(nFunction) i wybiera najlepsz± dla ka¿dego n, wraca listê najlepszych obiektów z kolejnych iteracji  
#w lb<n> mamy wybierane po kolei kolejne zestawy na najlepsz± regresjê 
#w m<n> najlepszy model dla n, w sm<n> summary(m<n>), dla n=1 rysuje plot regresji
permbesteval<-function (nFunction, fname, mDataSet, moutput, parvec, nleven=-1, alpha=0.01, EvalString="DEFAULT"){
	l<-list()
	n<-length(parvec)
	varvec<-parvec
	varplus<-paste(parvec,collapse="+")
	for(numvar in 1:length(parvec)){
		#cat(" nF:",nFunction)
		if(nFunction=="lm")
			ilist<-lmwithattr(mDataSet,moutput,parvec,numvar,nleven,alpha,EvalString)
		if(nFunction=="polr")
			ilist<-polrwithattr(mDataSet,moutput,parvec,numvar,EvalString)
		i<-ilist[[1]]; 
		#cat(paste("1:",moutput,"~",varplus)," n:",numvar," length:",n," EvalString:",EvalString,"\n")
		if(numvar<n){
			perm<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
			varvec<-perm[i,]
		}
		if(numvar==n) varvec<-parvec
		if(EvalString=="SPLINE") varplus<-paste("bs(",varvec,")",sep="") else varplus<-varvec
		varplus<-paste(varplus,collapse="+")			
		m01<-try(evalwithattr(get(nFunction),moutput,varvec,mDataSet,EvalString),TRUE)
	    #print(m01)
		if(!inherits(m01, "try-error")){
			if(numvar<10)mnv<-paste("m0",numvar,sep="")else mnv<-paste("m",numvar,sep="")
			if(numvar<10)smnv<-paste("sm0",numvar,sep="")else smnv<-paste("sm",numvar,sep="")
			if(numvar<10)lbnv<-paste("lb0",numvar,sep="")else lbnv<-paste("lb",numvar,sep="")
			assign(lbnv,ilist[[2]]);
			assign(mnv,m01);
			assign(smnv,summary(get(mnv)))
			get(smnv)
			#Vif(get(mnv))
			l[[lbnv]]<-get(lbnv)
			l[[mnv]]<-get(mnv)
			l[[smnv]]<-get(smnv)
			if(nFunction=="lm"){
				if(numvar==1){
					jpeg(file=paste(fname,numvar,"_1.jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
					par(lwd=4)
					vartemp<-eval(parse(text=paste("mDataSet$",varvec,sep="")))
					rangetemp<-seq(min(vartemp),max(vartemp),length.out=213)
					plot(eval(parse(text=paste(moutput,"~",varvec))),data=mDataSet,main=paste(moutput,"~",varplus), pch=1.0, cex.lab=1.5)
					dframetemp<-as.data.frame(rangetemp)
					names(dframetemp)<-varvec
					lines(rangetemp,predict(get(mnv),newdata=dframetemp), col="red", lwd=3)
					#cat(paste("3:",moutput,"~",varvec)," n:",numvar," mnv:",mnv," varvec:",paste("mDataSet$",varvec,sep=""),"\n")
					dev.off()
				}
				jpeg(file=paste(fname,numvar,"_2.jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
				par(mfrow=c(2,2))
				plot(get(mnv),main=paste(moutput,"~",varplus), pch=1.0, cex.lab=1.5)
				dev.off()
			}
		}
	}
	return (l)
}

#Funkcja polrwithattr znajduje najlepsz± logistyczn± regresjê dla parametrów
#moutput - wyj¶cie modelu zmienna zale¿na np. "RI", 
#parvec - wektor zmiennych wej¶ciowych niezale¿nych dla modelu,
#numvar - ile zmiennych z parvec ma wzi±æ udzia³ w permutacji zmiennych niezale¿nych
polrwithattr<-function (DataSet, moutput, parvec, numvar, EvalString="DEFAULT"){
	if(numvar<length(parvec)){
		perm<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
		rowperm<-nrow(perm)
	}
	else rowperm<-1
	lb<-c(); ibest<-0; br2<-1000000000;
	for(i in 1:rowperm){
		if(numvar<length(parvec)) 	varplus<-perm[i,]
		if(numvar==length(parvec))	varplus<-parvec
		m01<-try(evalwithattr(polr,moutput,varplus,DataSet,EvalString),TRUE);
		if(!inherits(m01, "try-error")){
			sm01<-summary(m01)
			if(sm01$deviance < br2){ 
				br2<-sm01$deviance
				lb<-c(lb, i)
				ibest<-i
			}
		}
	}
	return (list(ibest,lb))
}


#Funkcja lmwithattr znajduje najlepsz± liniow± regresjê dla parametrów
#moutput - wyj¶cie modelu zmienna zale¿na np. "RI", 
#parvec - wektor zmiennych wej¶ciowych niezale¿nych dla modelu,
#numvar - ile zmiennych z parvec ma wzi±æ udzia³ w permutacji zmiennych niezale¿nych
lmwithattr<-function (DataSet, moutput, parvec, numvar, nleven=-1, alpha=0.01, EvalString="DEFAULT"){
	if(numvar<length(parvec)){
		perm<-get("combinations","package:gtools")(length(parvec),numvar,parvec)
		rowperm<-nrow(perm)
	}
	else rowperm<-1
	lb<-c(); ibest<-0; br2<-0;
	for(i in 1:rowperm){
		if(numvar<length(parvec)) 	varplus<-perm[i,]
		if(numvar==length(parvec))	varplus<-parvec;
		m01<-try(evalwithattr(lm,moutput,varplus,DataSet,EvalString),TRUE);
		if(!inherits(m01, "try-error")){
			tmp=paste("anova(",deparse(substitute(lm)),"(",moutput,"~1,DataSet),m01)",sep="")
			#an<-anova(lm(RI~1),m01);
			an<-eval(parse(text=tmp))
			sm01<-summary(m01)
			if(nleven>0){
				mintervals<-cut(m01$fitted.values,nleven)
				lt<-levene.test(m01$residuals,factor(mintervals))
			}
			if(nleven<0)
				bp<-evalwithattr(bptest,moutput,varplus,DataSet)
			#levene pominiêty dla nleven=0, dla nleven < 0 bptest
			if((nleven==0 && qf(0.99,1,m01$df)<an$F[2] && sm01$r.squared > br2) 
					|| (nleven < 0 && qf(0.99,1,m01$df)<an$F[2] && sm01$r.squared > br2 && bp$p.value > alpha)
					|| (nleven > 0 && qf(0.99,1,m01$df)<an$F[2] && sm01$r.squared > br2 && lt$"Pr(>F)"[1] > alpha && qf(1-alpha,lt$Df[1],lt$Df[2]) > lt$"F value"[1])){
				br2<-sm01$r.squared
				lb<-c(lb, i)
				ibest<-i
			}
		}
	}
	return (list(ibest,lb))
}

#Funkcja evalwithattr wywo³uje podan± funkcjê z wyj¶ciowym atrybutem output i 
#wej¶ciowymi atrybutami parvec oraz zbiorem danych
evalwithattr<-function(oFunction,output,parvec,oData,EvalString="DEFAULT")
{
	tmp=paste(deparse(substitute(oFunction)),"(",output,"~")
	if(EvalString=="SPLINE")  
		parvec<-paste("bs(",parvec,")",sep="")
	parvec<-paste(parvec,collapse="+")
	if(EvalString=="DEFAULT" || EvalString=="SPLINE") 
		tmp<-paste(tmp,parvec,",data=",deparse(substitute(oData)),")",sep="")
	else
		tmp<-paste(tmp,parvec,",data=",deparse(substitute(oData)),",",EvalString,")",sep="")
	return(eval(parse(text=tmp)))
}

#Metoda funkcja Vif okre¶la stopieñ korelacji zmiennych niezale¿nych, 
#podaje warto¶ci takie same jak vif z pakietu car, a jest prostsza i dzia³a dla jednej zmiennej (podaje 1) 
Vif <- function(object, ...)
	UseMethod("Vif")

Vif.default <- function(object, ...)
	stop("No default method for Vif.  Sorry.")

Vif.lm <- function(object, ...) {       
	V <- summary(object)$cov.unscaled
	Vi <- crossprod(model.matrix(object))
	nam <- names(coef(object))
	if(k <- match("(Intercept)", nam, nomatch = F)) {
		v1 <- diag(V)[-k]
		v2 <- (diag(Vi)[-k] - Vi[k, -k]^2/Vi[k,k])
		nam <- nam[-k]
	} else {
		v1 <- diag(V)
		v2 <- diag(Vi)
		warning("No intercept term detected.  Results may
						surprise.")
	}
	structure(v1*v2, names = nam)
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

#Funkcja scale.numeric skaluje wybrane numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi
#z paramerami CENTER=TRUE mean=0, a dla SCALE=TRUE sd=1
scale_for<-function (DataSet, parvec, CENTER, SCALE)         
{
	mtemp<-DataSet[parvec]
	mtemp<-as.data.frame(scale(mtemp,center=CENTER,scale=SCALE))
	DataSet[parvec]<-mtemp
	return (DataSet)
}


#Funkcja zscore.for.integer zetskoruje wybrane kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi po kolumnie zdyskretyzowanej (etykiecie) integercolumnforzscore dla poszczególnych jej warto¶ci
zscore.for.integer<-function (DataSet, parvec, integercolumnforzscore)         
{
	indata<-DataSet
	for(i in sort(unique(DataSet[[integercolumnforzscore]]))){
		indata[(indata[[integercolumnforzscore]]==i),]=zscore(indata[(indata[[integercolumnforzscore]]==i),],which(names(indata) %in% parvec))
	}
	return(indata)
}

#Funkcja discret.for.chosen dyskretyzujê atrybuty (kolumny) z parvec na levelnum poziomów
disc.for.chosen<-function (DataSet, parvec, levelnum)         
{
	DataSetd<-DataSet
	#DataSetd[names(DataSet) %in% parvec]<-disc.ef(DataSet[names(DataSet) %in% parvec], which(names(DataSetz) %in% parvec), levelnum)
	DataSetd[,parvec]<-disc.ef(DataSet[,parvec], which(names(DataSet[,parvec]) %in% parvec), levelnum)
	#DataSetd<-disc.ef(DataSet, which(names(DataSet) %in% parvec), levelnum)
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

#próba napisania uniwersalej funkcji write2jpg
write2jpg <- function(object, ...)
	UseMethod("write2jpg")

write2jpg.default <- function(object, ...)
	stop("No default method for write2jpg.  Sorry.")

write2jpg.lm <- function(object, ...) {       
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(lwd=4)
	
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
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
	par(mar=c(9,9,9,9))
	plot(dn, horiz = TRUE, nodePar = list(col = 3:2, cex = c(2.0, 0.75), pch = 21:22, bg=  c("light blue", "black"), lab.cex = 3.75, cex.main = 1.8, cex.axis = 1.2,  lab.col = "tomato"), edgePar = list(col = "gray", lwd = 2))
	title(main=paste("Variable Correlation Clusters ",as.character(substitute(indata)),"using",inmethod),cex.main=2)
	#par(op)
	dev.off()
}

#Funkcja latt2jpg zapisuje lattice w pliku jpeg
latt2jpg<-function(indata, gvec, fname){
	dat<-indata[, sapply(indata, class) == "numeric"]
	jpeg(file=paste(fname,".jpg",sep=""),width = 1200, height = 1000, quality = 55, bg = "white")
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

