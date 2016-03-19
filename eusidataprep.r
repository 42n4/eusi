# skrypt do zaj�� EU SI: wst�pne przetwarzanie danych zscore, discrete
# TODO: test, testowa�
# Licence LGPL  
# Author: Piotr W�siewicz
########################################################################################################

#te atrybuty, kt�re s� zmiennymi ilo�ciowymi i nie identyfikatorami
parvec=setdiff(names(DataSet),parvecfactor)
parvec=setdiff(parvec,parnokruskal)

#Zamieniam dowolne faktory na numeryczne faktory typu 1,2,3
#DataSet<-normfactor(DataSet, parvecfactor)

#potem faktoryzuj� na wszelki wypadek kolumny, kt�re s� etykietami, maj� ju� zdyskretyzowane warto�ci; ich wyb�r jest w wektorze parvecfactor
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))

#najpierw defaktoryzuj� i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi, tak na wszelki wypadek, gdyby csv �le si� wczyta� (w przypadku zbior�w data() to tylko �wiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0 i sd=1
DataSetms<-scale_for(DataSet,parvec,TRUE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do sd=1
DataSets<-scale_for(DataSet,parvec,FALSE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0
DataSetm<-scale_for(DataSet,parvec,TRUE,FALSE)

#zetskoruj� wybrane kolumny z liczbami zmiennoprzecinkowymi i ca�kowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,vecfactorzesc)

#dyskretyzuj� kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzuj� tak�e nie zeskorowane warto�ci zwyk�ego zbioru wczytanego na pocz�tku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


