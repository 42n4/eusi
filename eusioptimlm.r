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
#korelacje między atrybutami
#for(cormethod in c("pearson","kendall","spearman")){
for(cormethod in c("pearson")){
	hier2jpg(cormethod,DataSet[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_norm",sep=""))
	hier2jpg(cormethod,DataSetz[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_zesc",sep=""))
}
try(latt2jpg(DataSet[,parvec],DataSetd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_norm",sep="")),TRUE)
try(latt2jpg(DataSetz[,parvec],DataSetzd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_zesc",sep="")),TRUE)
#pairs

##########################################################################################################
#REGRESJA LINIOWA
nFunction="lm"
#w mparvec zmienne niezależne do regresji
mparvec=setdiff(names(DataSet),zalezne)
 
##########################################################################################################
#alpha to krytyczna wartość prawdopodobieństwa p dla statystyk bp i f 
alpha<-0.1;
##########################################################################################################
#dla nleven > 0 le vena ilość przedziałów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#bptest
nleven<--1; 
#wybieramy dane normalne
mDataSet<-DataSet
lmabc_nobp<-combesteval("lm", paste(mypathout,nData,"_lmabc_nobp",sep=""),mDataSet, zmiennaout, mparvec, nleven, alpha)
#wybieramy dane zeskorowane
#mDataSet<-DataSetz
#lmabc_zebp<-combesteval("lm", paste(mypathout,nData,"_lmabc_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskalowane i zcentrowane
#mDataSet<-DataSetm
#skalowanie likwiduje intercept współczynnik przesunięcia prostej
#lmabc_msbp<-combesteval("lm", paste(mypathout,nData,"_lmabc_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilość przedziałów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-0; 
mDataSet<-DataSet
lmabc_nono<-combesteval("lm", paste(mypathout,nData,"_lmabc_nono",sep=""),mDataSet, zmiennaout, mparvec, nleven, alpha)
#mDataSet<-DataSetz
#lmabc_zeno<-combesteval("lm", paste(mypathout,nData,"_lmabc_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#mDataSet<-DataSetm
#lmabc_msno<-combesteval("lm", paste(mypathout,nData,"_lmabc_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilość przedziałów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#leven
nleven<-5; 
mDataSet<-DataSet
lmabc_nolt<-combesteval("lm", paste(mypathout,nData,"_lmabc_nolt",sep=""),mDataSet, zmiennaout, mparvec, nleven, alpha)
#mDataSet<-DataSetz
#lmabc_zelt<-combesteval("lm", paste(mypathout,nData,"_lmabc_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#mDataSet<-DataSetm
#lmabc_mslt<-combesteval("lm", paste(mypathout,nData,"_lmabc_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#Teraz użyjemy splinów czyli y~bs(x1)+bs(x2)+...
SPLINE<-"SPLINE";
##########################################################################################################
#bptest
nleven<--1; 
mDataSet<-DataSet
lmbsp_nobp<-combesteval("lm", paste(mypathout,nData,"_lmbsp_nobp",sep=""),mDataSet, zmiennaout, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetz
#lmbsp_zebp<-combesteval("lm", paste(mypathout,nData,"_lmbsp_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_msbp<-combesteval("lm", paste(mypathout,nData,"_lmbsp_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilość przedziałów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-0; 
mDataSet<-DataSet
lmbsp_nono<-combesteval("lm", paste(mypathout,nData,"_lmbsp_nono",sep=""),mDataSet, zmiennaout, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetz
#lmbsp_zeno<-combesteval("lm", paste(mypathout,nData,"_lmbsp_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_msno<-combesteval("lm", paste(mypathout,nData,"_lmbsp_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilość przedziałów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#leven
nleven<-5; 
mDataSet<-DataSet
lmbsp_nolt<-combesteval("lm", paste(mypathout,nData,"_lmbsp_nolt",sep=""),mDataSet, zmiennaout, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetz
#lmbsp_zelt<-combesteval("lm", paste(mypathout,nData,"_lmbsp_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_mslt<-combesteval("lm", paste(mypathout,nData,"_lmbsp_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)


lmabc_nobp
lmabc_nono
lmabc_nolt
lmbsp_nobp
lmbsp_nono
lmbsp_nolt
#j=0;for( i in lmabc_nobp) {j=j+1;if(j%%4==3) print(i)}
#mDataSet<-cbind(mDataSet,log(mDataSet$NI))
#names(mDataSet)[18]<-"logNI"
#moutput<-"logNI"
#parvecnolm<-c("NI","logNI")
#mparvec=setdiff(names(DataSet),parvecnolm)
#lmlog_nobp<-combesteval("lm", paste(mypathout,nData,"_lmabc_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#moutput<-"NI"
#DataSet[!is.na(DataSet$potassium),]
