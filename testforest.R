# TODO: Add comment
# 
# Author: pwas
###############################################################################



data(iris)
set.seed(111)
ind <- sample(2, nrow(iris), replace = TRUE, prob=c(0.8, 0.2))
iris.rf <- randomForest(Species ~ ., data=iris[ind == 1,])
iris.pred <- predict(iris.rf, iris[ind == 2,])
table(observed = iris[ind==2, "Species"], predicted = iris.pred)
## Get prediction for all trees.
predict(iris.rf, iris[ind == 2,], predict.all=TRUE)
## Proximities.
predict(iris.rf, iris[ind == 2,], proximity=TRUE)
## Nodes matrix.
str(attr(predict(iris.rf, iris[ind == 2,], nodes=TRUE), "nodes"))

set.seed(1)
iris.rf <- randomForest(iris[,-5], iris[,5], proximity=TRUE)
plot(outlier(iris.rf), type="h",
		col=c("red", "green", "blue")[as.numeric(iris$Species)])


data(iris)
iris.rf <- randomForest(Species ~ ., iris)
hist(treesize(iris.rf))

data(mtcars)
plot(randomForest(mpg ~ ., mtcars, keep.forest=FALSE, ntree=100), log="y")

data(airquality)
airquality <- na.omit(airquality)
set.seed(131)
ozone.rf <- randomForest(Ozone ~ ., airquality)
partialPlot(ozone.rf, airquality, Temp)

data(iris)
set.seed(543)
iris.rf <- randomForest(Species~., iris)
partialPlot(iris.rf, iris, Petal.Width, "versicolor")

set.seed(1)
data(iris)
iris.rf <- randomForest(Species ~ ., iris, proximity=TRUE,
		keep.forest=FALSE)
MDSplot(iris.rf, iris$Species)
## Using different symbols for the classes:
MDSplot(iris.rf, iris$Species, palette=rep(1, 3), pch=as.numeric(iris$Species))


set.seed(4543)
data(mtcars)
mtcars.rf <- randomForest(mpg ~ ., data=mtcars, ntree=1000,
		keep.forest=FALSE, importance=TRUE)
importance(mtcars.rf)
importance(mtcars.rf, type=1)


data(iris)
## Look at the third trees in the forest.
getTree(randomForest(iris[,-5], iris[,5], ntree=10), 3, labelVar=TRUE)

data(iris)
rf1 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf2 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf3 <- randomForest(Species ~ ., iris, ntree=50, norm.votes=FALSE)
rf.all <- combine(rf1, rf2, rf3)
print(rf.all)

data(iris)
iris.rf <- randomForest(iris[,-5], iris[,5], prox=TRUE)
iris.p <- classCenter(iris[,-5], iris[,5], iris.rf$prox)
plot(iris[,3], iris[,4], pch=21, xlab=names(iris)[3], ylab=names(iris)[4],
		bg=c("red", "blue", "green")[as.numeric(factor(iris$Species))],
		main="Iris Data with Prototypes")
points(iris.p[,3], iris.p[,4], pch=21, cex=2, bg=c("red", "blue", "green"))

data(fgl, package="MASS")
fgl.res <- tuneRF(fgl[,-10], fgl[,10], stepFactor=1.5)


no nosymp
CBTVS     intrables      velocity       totales         CBTLS 
-0.0035454545 -0.0034545455 -0.0022727273 -0.0003181818  0.0005000000 
respres         TBTLS         TBTVS      percontr          LESP 
0.0020909091  0.0042272727  0.0051363636  0.0122272727  0.0202272727 
intrables      velocity         CBTVS       totales         CBTLS 
-4.000000e-03 -2.954545e-03 -1.772727e-03 -5.909091e-04  9.090909e-05 
respres         TBTLS         TBTVS      percontr          LESP 
5.909091e-04  2.045455e-03  3.590909e-03  7.590909e-03  1.540909e-02 

with nosymp
CBTLS       totales         CBTVS     intrables      velocity 
-0.0020983607 -0.0016721311 -0.0015081967 -0.0012131148 -0.0008524590 
respres         TBTLS      percontr         TBTVS          LESP 
-0.0003278689  0.0012459016  0.0024590164  0.0050491803  0.0168524590 
velocity         CBTLS         CBTVS     intrables       totales 
-0.0028196721 -0.0027540984 -0.0018688525 -0.0009836066 -0.0008524590 
respres      percontr         TBTLS         TBTVS          LESP 
-0.0007540984  0.0016721311  0.0023606557  0.0028524590  0.0125901639 


set.seed(777);
#Add some nonsense attributes to iris dataset by shuffling original attributes
iris.extended<-data.frame(iris,apply(iris[,-5],2,sample));
names(iris.extended)[6:9]<-paste("Nonsense",1:4,sep="");
#Run Boruta on this data
Boruta(Species~.,data=iris.extended,doTrace=2)->Boruta.iris.extended
#Nonsense attributes should be rejected
print(Boruta.iris.extended);
##


set.seed(290875)

### regression
airq <- subset(airquality, !is.na(Ozone))
airct <- ctree(Ozone ~ ., data = airq, 
		controls = ctree_control(maxsurrogate = 3))
airct
plot(airct)
mean((airq$Ozone - predict(airct))^2)

### classification
irisct <- ctree(Species ~ .,data = iris)
irisct
plot(irisct)
table(predict(irisct), iris$Species)

### estimated class probabilities, a list
tr <- treeresponse(irisct, newdata = iris[1:10,])

### ordinal regression
mammoct <- ctree(ME ~ ., data = mammoexp) 
plot(mammoct)

### estimated class probabilities
treeresponse(mammoct, newdata = mammoexp[1:10,])

### survival analysis
if (require("ipred")) {
	data("GBSG2", package = "ipred")
	GBSG2ct <- ctree(Surv(time, cens) ~ .,data = GBSG2)
	plot(GBSG2ct)
	treeresponse(GBSG2ct, newdata = GBSG2[1:2,])        
}


browseURL(system.file("documentation/html/index.html", 
				package = "party"))
