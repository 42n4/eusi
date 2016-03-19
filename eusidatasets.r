# skrypt do zajêæ EU SI: dane testowe
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

##########################################################################################################
nData<-"glass"
#nData<-"nihills"
#nData<-"photocar"
#nData<-"meatspec"
#nData<-"skoliozaNIL1.csv"
#nData<-"narchecked.csv"

##########################################################################################################
#wczytywanie zbioru treningowego o nazwie nData
#assign(nData,read.csv(paste("file://",mypath,nData,sep=""),head=TRUE,sep=";",dec=",",na.strings=c("NA", "BD", "bd", "", "?")))

data(list=nData)
DataSet<-get(nData)

##########################################################################################################
if(nData=="glass"){
	#te atrybuty, co s± faktorami, zmiennymi jako¶ciowymi z kategoriami, etykietami
	parvecfactor=c("Type")
	#kruskal
	#w parnokruskal to co zostanie nie wys³ane do isoMDS(daisy) np. parnokruskal=c("Type")
	parnokruskal=c()
	#atrybut pojedyñczy do zescorowania po jego dyskretnych warto¶ciach
	vecfactorzesc=c("Type")
	#atrybuty w plotMDS do kolorowania powsta³ych punktów 
	parvecol<-names(DataSet)
	#zmienne drzewa
	#zmienne, które nie wchodz± do drzewa m.in. jego li¶cie, target, cel optymalizacji drzewa
	parvecnotree=c("Type")
	#li¶æ drzewa, etykieta
	paroutputree=c("Type")
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("Type","RI")
	#wybieramy zmienn± zale¿n±, target dla regresji, zwykle zmiennoprzecinkowy
	moutput<-"RI"
}

##########################################################################################################

if(nData=="nihills"){
	parvecfactor=c()
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c('time')
	paroutputree=c('time')
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("time")
	moutput<-"time"
} 

##########################################################################################################
if(nData=="photocar"){
	parvecfactor=c("group","event","tumor")
	vecfactorzesc=c("event")
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("group")
	paroutputree=c("group")
	#zmienne zale¿ne i inne zbêdne w regresji
	#parvecnolm=c("time","group","tumor","event")
	parvecnolm=c("time")
	moutput<-"time"
}
##########################################################################################################
if(nData=="meatspec"){
	parvecfactor=c("")
	vecfactorzesc=c("")
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("fat")
	paroutputree=c("fat")
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("fat")
	moutput<-"fat"
}

##########################################################################################################
if(nData=="skoliozaNIL1.csv"){
	parvecfactor=c()
	vecfactorzesc=c()
	#kruskal
	parnokruskal=c()
	parvecol<-names(DataSet)
	#zmienne drzewa
	parvecnotree=c("NI")
	paroutputree=c("NI")
	#zmienne zale¿ne i inne zbêdne w regresji
	parvecnolm=c("NI")
	moutput<-"NI"
}
