# skrypt do zajęć EU SI: Kruskal, lm, rpart
# TODO: test, testować
# Licence LGPL  
# Author: Piotr Wąsiewicz
########################################################################################################
 
#te ścieżki mypath i mypathout muszą istnieć w systemie
#to ścieżka do plików wykonywalnych z R powłoki za pomocą source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/guest/workspace/iso/"
#to ścieżka do plików graficznych uzyskiwanych za pomocą funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")
dir.create(mypathout, showWarnings = TRUE, recursive = TRUE, mode = "0755")
#Sys.chmod(paths, mode = "0755")

source(paste(mypath,"eusifunkcje.r",sep=""))

##########################################################################################################
#nData<-"Glass"
#nData<-"nihills"
#nData<-"photocar"
source(paste(mypath,"eusidatasets.r",sep=""))
source(paste(mypath,"eusidataprep.r",sep=""))

##########################################################################################################
#Kruskal dla normalnych danych
#generuję z daisy różnice między wierszami podanego zbioru i wprowadzam do isoMDS rzutującego na dwa wymiary k=2 
parveckruskal=setdiff(names(DataSet),parnokruskal)
nDataSets<-KruskelMDS(DataSet, parveckruskal, 2)

# uzyskany zbiór punktów koloruję różnymi atrybutami podstawianymi do wzorzec i uzyskuję wizualizację cech
fname<-paste(mypathout,nData,"_MDSnorm_nrmdis",sep="")
wzorzec1=DataSet$Type
#pdm=1:length(unique(DataSet$Type))
#org=sort(unique(DataSet$Type))
#wzorzec1=2
plotMDS.for.chosen(fname, nDataSets, DataSetd, parvecol, wzorzec1) 

#Kruskal dla zeskorowanych danych
parveckruskal=setdiff(names(DataSet),parnokruskal)
nDataSets<-KruskelMDS(DataSetz, parveckruskal, 2)

fname<-paste(mypathout,nData,"_MDSzesc_zscdis",sep="")
wzorzec1=DataSet$Type
plotMDS.for.chosen(fname, nDataSets, DataSetzd, parvecol, wzorzec1) 


##########################################################################################################
#generujemy drzewa rpart dla parvectree
nData<-"DataSet"
datype<-"norm"
#DataSet<-get(nData)
#nFunction<-"ipredknn"
#evalstr<-"DEFAULT"
#nFunction<-"NaiveBayes"
#evalstr<-"CLASS"
#nFunction<-"randomForest"
#evalstr<-"DEFAULT"
#nFunction<-"ctree"
#evalstr<-'controls = ctree_control(mincriterion=0.95, maxdepth=4,teststat="quad",testtype="Bonferroni")'
#nFunction<-"cforest"
#evalstr<-'controls = cforest_control(maxdepth=10)'
nFunction<-"rpart"
evalstr<-"DEFAULT"
#evalstr<-"control=rpart.control(cp=.001)"
#evalstr<-"parms=list(prior=c(0.2,0.2,0.2,0.2,0.2)),control=rpart.control(cp=.05)"
#nFunction<-"qda"
#evalstr<-"CLASS"
#nFunction<-"svm"
#evalstr<-'kernel="linear"'	
#evalstr<-'kernel="radial"'
#evalstr<-'kernel="sigmoid"'
#evalstr<-'kernel="polynomial"'	
#nFunction<-"J48"
#evalstr<-"parms=list(split='information')"
#evalstr<-'control=Weka_control(R=list("J48"))'
#evalstr<-'control=Weka_control(C=0.15)'
percent<-90
trials<-5
paroutputree=c("Type")
parvectree<-setdiff(names(DataSet),c("Type"))
mdet<-0;
#while(!mdet){
cat("\nDane ",nData," funkcja: ",nFunction," lisc: ",i,"\n")
bl<-brutoptim.klas(mypathout,nData,nFunction,paroutputree,parvectree,percent,trials,evalstr)
if(length(bl))
{
	#lres<-prederror(bl$bestclass,nFunction, paroutputree, parvectree, DataSet[-bl$betykiety,],evalstr)
	lres<-prederror(bl$bestclass,nFunction, paroutputree, parvectree, DataSet,evalstr)
	mdet<-det(matrix(lres$table, ncol=length(unique(DataSet[,paroutputree]))))
	print(bl$bestclass)
	print(lres$table)
	cat("Error:",lres$perror," Det:",mdet,"\n")
	if(nFunction=="rpart")
		zapisz_rpart(bl$bestclass,paste(mypathout,nData,"_Best_",nFunction,"_",paroutputree,sep=""))
	if(nFunction=="J48") 
		zapisz_weka(bl$bestclass,paste(mypathout,nData,"_Best_",nFunction,"_",paroutputree,sep=""))
}
#}

DataSetm<-DataSet
paroutput=c("Type")
for(parvecforout in paroutput)
{
	cat(paste("\n \n BADAMY WAŻNOŚĆ ATRYBUTÓW W IDENTYFIKACJI WYJŚCIOWEGO PARAMETRU: ",parvecforout,"\n"))
	dtree=evalwithattr("randomForest", parvecforout, parvectree,DataSetm,"DEFAULT")
	print(prederror(dtree,nFunction, parvecforout, parvectree, DataSetm,"DEFAULT")$table)
	print(prederror(dtree,nFunction, parvecforout, parvectree, DataSetm,"DEFAULT")$perror)
	print(sort(importance(dtree)[,1],decreasing = TRUE));cat("\n")
	dtree=evalwithattr("cforest", parvecforout, parvectree,DataSetm,'controls = cforest_control(mincriterion=0.65, maxdepth=20,ntree=1000)')
	print(table(predict(dtree),DataSetm[,parvecforout]))
	vimp<-varimp(dtree);
	vimpcd<-varimp(dtree,conditional=TRUE);
	print(sort(vimp,decreasing = TRUE));cat("\n")
	print(sort(vimpcd,decreasing = TRUE));cat("\n")
}
