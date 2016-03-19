# skrypt do zajêæ EU SI: wstêpne przetwarzanie danych zscore, discrete
# TODO: test, testowaæ
# Licence LGPL  
# Author: Piotr W±siewicz
########################################################################################################

#te atrybuty, które s± zmiennymi ilo¶ciowymi i nie identyfikatorami
parvec=setdiff(names(DataSet),parvecfactor)
parvec=setdiff(parvec,parnokruskal)

#Zamieniam dowolne faktory na numeryczne faktory typu 1,2,3
#DataSet<-normfactor(DataSet, parvecfactor)

#potem faktoryzujê na wszelki wypadek kolumny, które s± etykietami, maj± ju¿ zdyskretyzowane warto¶ci; ich wybór jest w wektorze parvecfactor
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))

#najpierw defaktoryzujê i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi, tak na wszelki wypadek, gdyby csv ¼le siê wczyta³ (w przypadku zbiorów data() to tylko æwiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0 i sd=1
DataSetms<-scale_for(DataSet,parvec,TRUE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do sd=1
DataSets<-scale_for(DataSet,parvec,FALSE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0
DataSetm<-scale_for(DataSet,parvec,TRUE,FALSE)

#zetskorujê wybrane kolumny z liczbami zmiennoprzecinkowymi i ca³kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,vecfactorzesc)

#dyskretyzujê kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzujê tak¿e nie zeskorowane warto¶ci zwyk³ego zbioru wczytanego na pocz±tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


