# skrypt do zajęć EU SI: wstępne przetwarzanie danych zscore, discrete
# TODO: test, testować
# Licence LGPL  
# Author: Piotr Wąsiewicz
########################################################################################################

#te atrybuty, które są zmiennymi ilościowymi i nie identyfikatorami
parvec=setdiff(names(DataSet),parvecfactor)
parvec=setdiff(parvec,parnokruskal)

#Zamieniam dowolne faktory na numeryczne faktory typu 1,2,3
#DataSet<-normfactor(DataSet, parvecfactor)

#potem faktoryzuję na wszelki wypadek kolumny, które są etykietami, mają już zdyskretyzowane wartości; ich wybór jest w wektorze parvecfactor
DataSet<-factorto(DataSet, which(names(DataSet) %in% parvecfactor))

#najpierw defaktoryzuję i oznaczam jako numeryczne kolumny z liczbami zmiennoprzecinkowymi i całkowitymi, tak na wszelki wypadek, gdyby csv źle się wczytał (w przypadku zbiorów data() to tylko ćwiczenie)
DataSet<-defactor.numeric(DataSet, parvec)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0 i sd=1
DataSetms<-scale_for(DataSet,parvec,TRUE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do sd=1
DataSets<-scale_for(DataSet,parvec,FALSE,TRUE)

#skaluje wszystkie zmiennoprzecinkowe atrybuty do mean=0
DataSetm<-scale_for(DataSet,parvec,TRUE,FALSE)

#zetskoruję wybrane kolumny z liczbami zmiennoprzecinkowymi i całkowitymi
DataSetz<-zscore.for.integer(DataSet,parvec,vecfactorzesc)

#dyskretyzuję kolumny poprzednio zeskorowane
DataSetzd<-disc.for.chosen(DataSetz,parvec,3)

#dyskretyzuję także nie zeskorowane wartości zwykłego zbioru wczytanego na początku
DataSetd<-disc.for.chosen(DataSet,parvec,3)


