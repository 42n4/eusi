# skrypt do zajêæ ISO WIT: Kruskal, lm, rpart
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

#te ¶cie¿ki mypath i mypathout musz± istnieæ w systemie
#to ¶cie¿ka do plików wykonywalnych z R pow³oki za pomoc± source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"
#to ¶cie¿ka do plików graficznych uzyskiwanych za pomoc± funkcji plot i innych
mypathout<-paste(mypath,"rysunki/",sep="")
dir.create(mypathout, showWarnings = TRUE, recursive = TRUE, mode = "0755")
#Sys.chmod(paths, mode = "0755")

source(paste(mypath,"isowitfunkcje.r",sep=""))

##########################################################################################################
#nData<-"Glass"
#nData<-"nihills"
#nData<-"photocar"
source(paste(mypath,"isowitdatasets.r",sep=""))

##########################################################################################################
#korelacje miêdzy atrybutami
#for(cormethod in c("pearson","kendall","spearman")){
for(cormethod in c("pearson")){
	hier2jpg(cormethod,DataSet[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_norm",sep=""))
	hier2jpg(cormethod,DataSetz[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_zesc",sep=""))
}
try(latt2jpg(DataSet[,parvec],DataSetd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_norm",sep="")),TRUE)
try(latt2jpg(DataSetz[,parvec],DataSetzd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_zesc",sep="")),TRUE)
#pairs

##########################################################################################################
#REGRESJA LOGISTYCZNA
nFunction="polr"
etykiety <- sample(1:nrow(DataSet), round(nrow(DataSet)*0.9))
parvectree=setdiff(names(DataSet),parvecnotree)
evalstr<-"Hess=TRUE"
mDataSet<-DataSetd[etykiety,]
polra_nodi<-combesteval("polr", paste(mypathout,nData,"_polra_nodi",sep=""),mDataSet, paroutputree, parvectree, nleven, alpha, evalstr)
mDataSet<-DataSetzd[etykiety,]
polra_zedi<-combesteval("polr", paste(mypathout,nData,"_polra_zedi",sep=""),mDataSet, paroutputree, parvectree, nleven, alpha, evalstr)

#prederror(regression,paroutputree,mparvec,DataSetzd[-etykiety,],evalstr)
#nFunction<-"polr"; fname<-paste(mypathout,nData,"_polra_nodi",sep=""); moutput<-paroutputree;parvec<-parvectree; EvalString<-evalstr;
#nFunction="lm";fname<-paste(mypathout,nData,"_lmbsp_zelt",sep="");mDataSet<-DataSetz



