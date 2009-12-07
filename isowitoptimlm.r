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
#REGRESJA LINIOWA
nFunction="lm"
#w mparvec zmienne niezale¿ne do regresji
mparvec=setdiff(names(DataSet),parvecnolm)

##########################################################################################################
#alpha to krytyczna warto¶æ prawdopodobieñstwa p dla statystyk bp i f 
alpha<-0.1;
##########################################################################################################
#dla nleven > 0 le vena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#bptest
nleven<--1; 
#wybieramy dane normalne
mDataSet<-DataSet
lmabc_nobp<-permbesteval("lm", paste(mypathout,nData,"_lmabc_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskorowane
mDataSet<-DataSetz
#lmabc_zebp<-permbesteval("lm", paste(mypathout,nData,"_lmabc_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#wybieramy dane zeskalowane i zcentrowane
#mDataSet<-DataSetm
#skalowanie likwiduje intercept wspó³czynnik przesuniêcia prostej
#lmabc_msbp<-permbesteval("lm", paste(mypathout,nData,"_lmabc_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-0; 
mDataSet<-DataSet
lmabc_nono<-permbesteval("lm", paste(mypathout,nData,"_lmabc_nono",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetz
#lmabc_zeno<-permbesteval("lm", paste(mypathout,nData,"_lmabc_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#mDataSet<-DataSetm
#lmabc_msno<-permbesteval("lm", paste(mypathout,nData,"_lmabc_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#leven
nleven<-5; 
mDataSet<-DataSet
lmabc_nolt<-permbesteval("lm", paste(mypathout,nData,"_lmabc_nolt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
mDataSet<-DataSetz
#lmabc_zelt<-permbesteval("lm", paste(mypathout,nData,"_lmabc_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)
#mDataSet<-DataSetm
#lmabc_mslt<-permbesteval("lm", paste(mypathout,nData,"_lmabc_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha)

##########################################################################################################
#Teraz u¿yjemy splinów czyli y~bs(x1)+bs(x2)+...
SPLINE<-"SPLINE";
##########################################################################################################
#bptest
nleven<--1; 
mDataSet<-DataSet
lmbsp_nobp<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_nobp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
#lmbsp_zebp<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_zebp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_msbp<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_msbp",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#brak testów wariancji
nleven<-0; 
mDataSet<-DataSet
lmbsp_nono<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_nono",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
#lmbsp_zeno<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_zeno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_msno<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_msno",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)

##########################################################################################################
#dla nleven > 0 levena ilo¶æ przedzia³ów, dla nleven < 0 bptest, dla nleven == 0 brak testów wariancji
#leven
nleven<-5; 
mDataSet<-DataSet
lmbsp_nolt<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_nolt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
mDataSet<-DataSetz
#lmbsp_zelt<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_zelt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)
#mDataSet<-DataSetm
#lmbsp_mslt<-permbesteval("lm", paste(mypathout,nData,"_lmbsp_mslt",sep=""),mDataSet, moutput, mparvec, nleven, alpha, SPLINE)


lmabc_nobp
lmabc_nono
lmabc_nolt
lmbsp_nobp
lmbsp_nono
lmbsp_nolt
