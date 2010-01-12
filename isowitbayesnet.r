# skrypt do zajêæ ISO WIT: Kruskal, lm, rpart
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

#te ¶cie¿ki mypath i mypathout musz± istnieæ w systemie
#to ¶cie¿ka do plików wykonywalnych z R pow³oki za pomoc± source("...")
#mypath<-"/media/disk/guest/"
mypath<-"/home/pwas/workspace/iso/"
#mypath<-"/home/pwas/workspace/dataset/"
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
source(paste(mypath,"isowitdataprep.r",sep=""))

##########################################################################################################
#korelacje miêdzy atrybutami
#for(cormethod in c("pearson","kendall","spearman")){
for(cormethod in c("pearson")){
	hier2jpg(cormethod,DataSet[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_norm",sep=""))
	hier2jpg(cormethod,DataSetz[,parvec],paste(mypathout,nData,"_hiercor_",cormethod,"_zesc",sep=""))
}
try(latt2jpg(DataSet[,parvec],DataSetd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_norm",sep="")),TRUE)
try(latt2jpg(DataSetz[,parvec],DataSetzd[,vecfactorzesc],paste(mypathout,nData,"_lattcor_zesc",sep="")),TRUE)


##########################################################################################################
#Sieci Bayesa
#
fname<-paste(mypathout,nData,"_bnet_norm",sep="")
DataSetm<-DataSet
lbn_iabm<-evalbn(fname,"iamb",DataSetm)
lbn_gs<-evalbn(fname,"gs",DataSetm)
lbn_fast<-evalbn(fname,"fast.iamb",DataSetm)
lbn_inter<-evalbn(fname,"inter.iamb",DataSetm)
#fname<-paste(mypathout,nData,"_bnet_norm",sep="")
#lbn_mmhc<-evalbn("mmhc",DataSet,fname)

DataSetd<-disc.for.chosen(DataSet,parvec,4)
DataSetm<-DataSetd
fname<-paste(mypathout,nData,"_bnet_disc",sep="")
lbn_iabmd<-evalbn(fname,"iamb",DataSetm)
lbn_gsd<-evalbn(fname,"gs",DataSetm)
lbn_hcd<-evalbn(fname,"hc",DataSetm)
lbn_interd<-evalbn(fname,"inter.iamb",DataSetm)

DataSetm<-DataSetd
bn<-lbn_interd[["bn"]]
bnfit<-lbn_interd[["bnfit"]]
fname<-paste(mypathout,nData,"_bnet_nofr",sep="")
jpeg(file=paste(fname,".jpg",sep=""),width = 1600, height = 1000, quality = 55, bg = "white")
par(mfrow=c(1,1),pch=1.0,cex.lab=1.5,lwd=3)
bn.fit.qqplot(bnfit)
dev.off()
fname<-paste(mypathout,nData,"_bnet_nonr",sep="")
jpeg(file=paste(fname,".jpg",sep=""),width = 1600, height = 1000, quality = 55, bg = "white")
par(mfrow=c(1,1),pch=1.0,cex.lab=1.5,lwd=3)
bn.fit.xyplot(bnfit)
dev.off()

DataSetm<-DataSetd
bn<-lbn_interd[["bn"]]
bnfit<-lbn_interd[["bnfit"]]
bnfit<-removenullbn(bn,bnfit)

ll<-bn2gr(bn,bnfit)
lname<-ll[["lname"]]
lcpname<-ll[["lcpname"]]
for(i in 1:length(names(bn$nodes))){
assign(lname[i],eval(parse(text=lcpname[i])))
}
plname<-"plist"
tmp<-paste("gRain::compileCPT(list(",paste(lname,collapse=","),"))",sep="")
require(party);detach(package:party)
require(deal);detach(package:deal)
assign(plname,eval(parse(text=tmp)))
gnet<-grain(get(plname))
gnet
querygrain(gnet,nodes=c("RI","Type"))
gnet1  <- setFinding(gnet,nodes=c("Mg","Ba"),states=c("4","4"))
querygrain(gnet1,nodes=c("RI","Type"))
