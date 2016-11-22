# Data Science/-Mining/-Machine Learning mit R für Aktuare
# Eine kurze, praktische Einführung
# Friedrich Loser,  Okt. 2016 

# R-Skripte. für die Installation der benötigten R-Packages siehe #Anhang



#4. R-Umgebung erzeugen, R kennen lernen und ausprobieren

##R 4.1 Werden die folgenden Zeilen in RStudio nur als eine Zeile eingefügt? Tipp: Einen anderen pdf-Reader (z.B. Foxit-Reader) verwenden
dat <- data.frame(sex=c("m","f","m","f"),age=c(23,45,67,89),class=c("1","3","3","2"))
dat
ran <- c(1,3,4)
dat[ran,] # Auswahl von Datensätzen
dat[-ran,] # Komplement dazu (z.B. Testdatei)
dat[,-2] # Ohne zweites Merkmal (age)
##t (Ende R-Skript, weiter im Text. R-Skripte können über den Suchtext "##R" angesteuert werden)

##R 4.2 Tipp: Wenn sie einen guten pdf-Reader haben, können Sie das komplette Dokument markieren, in RStudio einfügen und ausschnittweise ausführen
set.seed(3) # Startwert setzen (für reproduzierbare Ergebnisse sorgen)
x=rnorm(100) # 100 standardnormalverteilte Pseudozufallszahlen erzeugen
sum(x) # Summe (Nahe Null wäre gut)
length(x) # Länge des Vektors (100)
y=x+rnorm(100,mean=50,sd=.2)
cor(x,y) # Korrelationkoeffizient
plot(x,y)
##t

##R 4.3
library(ISLR)
attach(Wage) # Nun kann Prefix Wage$ bei Variablennamen weggelassen werden
par(mfrow=c(1,3)) # drei Grafiken nebeneinander
# 1. Altersabhängigkeit mit smoothing spline
plot(age, wage, xlab="Age", ylab="Wage" ,cex=.5, col="darkgrey ")
fit=smooth.spline(age, wage ,df=7)
lines(fit, col="blue", lwd=2)
# 2. Zeitabhängigkeit mit linearem Modell (Funktion lm)
plot(year, wage, col="darkgrey ", xlab="Year", ylab="Wage")
lm.fit=lm(wage ~ year)
abline(lm.fit, lwd=3, col="blue")
# 3. Bunter Boxplot zur Ausbildungsabhängigkeit
plot(education, wage, col=c("cyan","green","yellow","darkblue","red"), varwidth =T)
detach(Wage) # verhindert spätere Fehlermeldungen "The following objects are masked from ..."
##t


#5. Titanic-Daten einlesen, visualisieren und aufbereiten

##R 5.1
test <- read.csv("D:/downloads/titanic_test.csv") # Pfad/Name anpassen. Windows:
train <- read.csv("D:/downloads/titanic_train.csv") # "\" durch "/" ersetzen.
dim(test) # Anzahl Datensätze und Merkmale anzeigen
dim(train)
str(train) # Struktur des data frame anzeigen
head(train) # Die ersten Zeilen andrucken
##t

##R 5.2
attach(train) # Nun kurze Variablennamen (z.B. Sex statt train$Sex) möglich
table(Sex, Survived) # Absolute Häufigkeiten
prop.table(table(Sex, Survived),1) # Relative Häufigkeiten (1: Zeilensumme=100%)
aggregate(Survived ~ Pclass + Sex, data=train, FUN=function(x) {sum(x)/length(x)})
##t

##R 5.3
library(vcd) # für mosaicplot
par(mfrow=c(1,3))
mosaicplot(Sex ~ Survived, main="", shade=F, color=T)
mosaicplot(Pclass~Survived,main="", shade=F, color=T)
boxplot(Age ~ Survived, main="",xlab="Survived", ylab="Age")
##t

##R 5.4
test$Survived <- NA # In der Testdatei das Leerfeld Survived anlegen
full <- rbind(train, test) # Trainings- und Testdatei aneinanderhängen
full$Name <- as.character(full$Name) # Name zu Textfeld machen
full$Name[1] # "Braund, Mr. Owen Harris" auswählen
strsplit(full$Name[1], split='[,.]') # ... seinen Namen splitten
strsplit(full$Name[1], split='[,.]')[[1]][2] # ... das 2. Namenselement abgreifen ("Mr.")
##t

##R 5.5
full$Title <- sapply(full$Name, FUN=function(x) {strsplit(x, split='[,.]')[[1]][2]})
full$Title <- sub(' ', '', full$Title) # noch 1. Leerzeichen entfernen
table(full$Sex, full$Title) # ... und das Ergebnis ansehen
##t


#6. R für den Aktuaralltag: Excel, SAS und Funktionen für Aktuare

##R 6.1
library(copula)
c1=mvdc(copula = archmCopula(family = "clayton", param = 2), margins=c("norm","norm"),
        paramMargins=list(list(mean = 0,sd = 1), list(mean=0,sd= 1)))
c2=mvdc(copula = archmCopula(family ="frank",param = 5.736), margins=c("norm","norm"),
        paramMargins=list(list(mean = 0,sd = 1), list(mean=0,sd= 1)))
c3=mvdc(copula = archmCopula(family = "gumbel", param = 2), margins=c("norm","norm"),
        paramMargins=list(list(mean = 0,sd = 1), list(mean=0,sd= 1)))
par(mfrow=c(1,3))
contour(c1,dMvdc,main="Clayton Copula",xlim = c(-3, 3), ylim = c(-3, 3))
contour(c2,dMvdc,main="Frank Copula", xlim = c(-3, 3), ylim = c(-3, 3))
contour(c3,dMvdc,main="Gumbel Copula", xlim = c(-3, 3), ylim = c(-3, 3))
##t

##R 6.2
library(ChainLadder)
W <- MackChainLadder(MW2014, est.sigma="Mack")
plot(W)
CDR(W)
##t


#7. Unüberwachtes Lernen: Clusteranalyse und Hauptkomponentenanalyse

##R 7.1 Quelle: ISLR, Ch 10.5
set.seed(2)
x=matrix(rnorm(50*2),ncol=2)
x[1:25,1]=x[1:25,1]+3 # Zweiten Cluster nach rechts verschieben
x[1:25,2]=x[1:25,2]-4 # Zweiten Cluster nach unten verschieben
# k-means-Clusteranalyse mit K=2 und 20 Läufen
km.out=kmeans(x,2,nstart =20) # Die Clusteranalyse durchführen
km.out$cluster # enthält Clusterzuordnungen
par(mfrow=c(1,3)) # drei Grafiken nebeneinander
plot(x, col=(km.out$cluster +1), main="K=2, nstart=20", xlab="", ylab="",pch=20,cex=2)
# Vergleich mit K=3 und 20 Läufen
km3a=kmeans(x,3,nstart =20)
km3a$tot.withinss # Ergebnis: 97.9793
plot(x, col=(km3a$cluster +1), main=" K=3, nstart=20", xlab="", ylab="", pch=20, cex=2)
# dazu Vergleich mit nur einem Lauf
set.seed(3) # dem Zufall nachhelfen ...
km3b=kmeans(x,3,nstart =1)
km3b$tot.withinss # Ergebnis: 104.3319
plot(x, col=(km3b$cluster +1), main="K=3, nstart=1", xlab="", ylab="", pch=20, cex=2)
##t

##R 7.2 Quelle: ISLR, Ch 10.6
library (ISLR) # Enthält die NCI60-Daten.
nci.labs=NCI60$labs # labs: Krebsart
nci.data=NCI60$data # data: Messwerte
dim(nci.data) # Anzahl Spalten und Zeilen anzeigen
table(nci.labs) # Verteilung der Krebs-Typen
pr.out=prcomp(nci.data , scale=TRUE) # Hauptkomponenten berechnen
summary(pr.out) # Std und Varianzanteil für 64 Komponenten
Cols=function(vec) { cols=rainbow(length(unique(vec)))
return(cols[as.numeric (as.factor(vec))]) }
par(mfrow=c(1,3))
plot(pr.out$x[,1:2], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z2")
plot(pr.out$x[,c(1,3)], col=Cols(nci.labs), pch=19, xlab="Z1",ylab="Z3")
pve=pr.out$sdev^2/sum(pr.out$sdev^2) # Anteil erklärter Varianz
plot(pve, type="o", ylab="PVE", xlab="Hauptkomponente", col="blue")
##t


#8. Regression: Multivariate lineare Regression, Interaktionen und Polynome

##R 8.1                                                                                                                    siehe auch: ISLR, Ch 3.6
library(MASS)
fit1=lm(medv~.,Boston)           # 1. Lineares Modell: Fit mit allen Merkmalen
summary(fit1)                       # => Adjusted R-squared = 0.7334
fit2=update(fit1,~.-age-indus)   # 2. Lineares Modell: Fit ohne die schwachen Merkmale  
summary(fit2)                       # => Adjusted R-squared = 0.7344
# 3. Fit mit qualitativem Merkmal, 2 Interaktionen und 2 Polynomen (in versch. Schreibweisen)
boston=Boston                   # Kopie anlegen (zum Schutz der Originaldatei Boston)
boston$zn2=ifelse(Boston$zn>0,'c1','c0') # Qualitatives Merkmal ableiten 
fit3=lm(medv~.-age-indus-lstat-zn+zn2*rm+zn2*nox+I(dis^2)+poly(lstat,5),boston)             
par(mfrow=c(1,4))
plot(fit3)
summary(fit3)       # => Ergebnis:
##t


#9. Klassifikation: Logistische Regression und Vergleich mit K-Nearest-Neighbors

##R 9.1 Quelle: ISLR, Ch 4.6.6
library(ISLR) # Enthält den Datensatz Caravan
dim(Caravan)
attach(Caravan)
summary(Purchase)
test=1:1000
test.Y=Purchase[test] # test.Y enthält tatsächliches Kaufverhalten
# Logistische Regression (Verallgemeinertes Lineares Modell)
glm.fit=glm(Purchase???.,data=Caravan,family=binomial,subset=-test)
test.probs=predict(glm.fit,Caravan[test,],type="response")
test.pred=rep("No",1000)
test.pred[test.probs >.25]="Yes" # test.pred enthält prognostiziertes Kaufverhalten
table(test.pred ,test.Y) # 11/(11+22)=33%
##t

##R 9.2 Achtung: Installiert automatisch die class-Bibliothek Quelle: ISLR, Ch 4.6.6
# Falls nicht vorhanden das Package class installieren
if (!"class" %in% rownames(installed.packages())) {install.packages("class")}
library("class")
std.X=scale(Caravan[,-86]) # Alle numerischen Variablen standardisieren
test=1:1000
train.X=std.X[-test,]
test.X=std.X[test,]
train.Y=Purchase[-test]
test.Y=Purchase[test]
set.seed(1)
knn.pred=knn(train.X,test.X,train.Y,k=5)
table(knn.pred,test.Y) # 4/(4+11)=27%
##t


#10. Sampling: Training und Test, Cross-Validation, Bootstrapping und Standardfehler

##R 10.1 siehe auch: ISLR, Ch 5.3.1
library(ISLR)
library(boot) # enthält cv.glm
attach(Auto)
summary(Auto) # (der mpg-Mittelwert von 23,45 miles per gallon entspricht 10L/100km)
set.seed(1)
train=sample(392,196) # Train/Test: train enthält zuf. Nummern für die Hälfte der Datensätze
par(mfrow=c(1,3))
plot(horsepower,mpg, pch=19,col=rainbow(ifelse(train>0,2,1))) # s.u. Graphik Links
mse=matrix(rep(0,10*2),2)
for (i in 1:10) {
lm.fit=lm(mpg~poly(horsepower,i),data=Auto, subset=train)
mse[1,i]=mean((mpg -predict(lm.fit ,Auto))[train ]^2) # Standardfehler: Trainingsdaten
mse[2,i]=mean((mpg -predict(lm.fit ,Auto))[-train ]^2) # Standardfehler: Testdaten
} # s.u. Graphik Mitte
plot(mse[1,],col="cyan",lwd=2,type="l",ylim=c(16,26),xlab="Poly",ylab="MSE",main="Train/Test")
lines(mse[2,] ,col="red",lwd=2)
# K-fache Cross-Validation mit cv.glm (Beschreibung auf der nächsten Seite)
set.seed(17)
cv.error=rep(0,10)
for (i in 1:10) {glm.fit=glm(mpg~poly(horsepower,i),data=Auto) # Polynom 10.ter Ordnung
cv.error[i]=cv.glm(Auto ,glm.fit ,K=10)$delta [1] } # 10-fache Cross-Validation
# Ergebnis der Kreuzvalidierung : s.u. Graphik Rechts
plot(cv.error ,col="red",lwd=2,type="l",ylim=c(16,26),xlab="Poly",ylab="MSE",main="CV")
##t

##R 10.2 Quelle: ISLR, Ch 5.3.4
set.seed(1)
boot.fn=function(data,index) return (coef(lm(mpg~horsepower,data=data,subset=index)))
boot(Auto,boot.fn,1000) # Ergebnis:
# Bootstrap Statistics :
# original bias std. error
# t1* 39.9358610 0.0269563085 0.859851825
# t2* -0.1578447 -0.0002906457 0.007402954
# Vergleich mit Standardfehlern der Regressionskoeffizienten eines linearen Modells:
summary(lm(mpg~horsepower,data=Auto))$coef # Ergebnis:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) 39.9358610 0.717498656 55.65984 1.220362e-187
# horsepower -0.1578447 0.006445501 -24.48914 7.031989e-81
par(mfrow=c(1,4))
plot((lm(mpg~horsepower,data=Auto))) # Residuenplots für den klassischen Weg:
##t


#11. Regularisierung linearer Modelle: Ridge-Regression und LASSO

##R 11.1 siehe auch: ISLR, Ch 6.6
library(ISLR)
library(glmnet)
Hitters=na.omit(Hitters) # Datensätze mit fehlenden Werten entfernen
with(Hitters,sum(is.na(Salary))) # Zähle Datensätze ohne Salary. Ergebnis: 0
dim(Hitters) # Ergebnis: Noch 263 Datensätze, 20 Merkmale
x=model.matrix(Salary~.-1,data=Hitters) # Matrix, ohne Salary. Bildet num. Dummy-Merkmale
y=Hitters$Salary
# Schrumpfen der Koeffizienten:
par(mfrow=c(1,2))
set.seed(1)
fit.ridge=glmnet(x,y,alpha=0) # Ridge-Ression fitten
plot(fit.ridge,xvar="lambda",label=TRUE) # Schrumpfen der Koeffizienten plottenfit.lasso=glmnet(x,y) # Lasso fitten. Default: alpha=1
plot(fit.lasso,xvar="lambda",label=TRUE) # Ergebnis x: log(Lamba)
##t

##R 11.2 siehe auch: ISLR, Ch 6.6
# x-Matrix und y-Vektor für glmnet anlegen. Trainings- und Testdatei erzeugen
x=model.matrix(Salary~.,Hitters)[,-1] # Matrix ohne Salary. Bildet num. Dummy-Merkmale
y=Hitters$Salary
library(glmnet)
grid=10^seq(10,-2,length=100) # Gitter von Lamba=0.01 bis 10^10 anlegen
set.seed(1) # Reproduzierbare Trainings- und Testdaten erzeugen
train=sample(1:nrow(x),nrow(x)/2)
test=(-train)
y.test=y[test]
# Ridge Regression: Elasticnet mixing parameter "alpha" = 0
ridge.mod=glmnet(x[train,],y[train],alpha=0,lambda=grid,thresh=1e-12) # Werte fürs Gitter
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min # Lambda mit dem kleinsten Standardfehler
ridge.pred=predict(ridge.mod,s=bestlam,newx=x[test,])
mean((ridge.pred-y.test)^2) # Ergebnis: 96016
out=glmnet(x,y,alpha=0) # Fit mit gesamtem Datensatz
predict(out,typ="coefficients",s=bestlam)[1:20,] # Prognose mit bestlam
# The Lasso: Elasticnet mixing parameter "alpha" = 1 (default)
lasso.mod=glmnet(x[train,],y[train],alpha=1,lambda=grid) # Für grid-Suche
set.seed(1)
cv.out=cv.glmnet(x[train,],y[train],alpha=0)
plot(cv.out)
bestlam=cv.out$lambda.min
lasso.pred=predict(lasso.mod,s=bestlam,newx=x[test,])
mean((lasso.pred-y.test)^2) # Ergebnis: 100743
out=glmnet(x,y,alpha=1,lambda=grid)
predict(out,typ="coefficients",s=bestlam)[1:20,] # CV-Ergebnis Lasso-Coefficients:
##t


#12. Support Vector Classifier und Support Vector Machine (SVM), ROC-Kurve

##R 12.1 Quelle: statLearning, ch09.Rmd
library(e1071)
set.seed(10111) # probieren geht über studieren ...
x=matrix(rnorm(40),20,2) # zweidimensionale Daten mit unscharfer Trennlinie anlegen
y=rep(c(-1,1),c(10,10))
x[y==1,]=x[y==1,]+1 # Für etwas Abstand zwischen den Clustern sorgen
par(mfrow=c(1,2))
plot(x,col=y+3,pch=19, main="simulierte Daten")
svmfit=svm(y~.,data=data.frame(x,y=as.factor(y)),kernel="linear",cost=10,scale=FALSE)
print(svmfit) # Ergebnis: 6 Support Vectors
# gepunkteten Hintergrund anlegen, Support Vectors markieren und Margins plotten
make.grid=function(x,n=75){ grange=apply(x,2,range)
x1=seq(from=grange[1,1],to=grange[2,1],length=n)
x2=seq(from=grange[1,2],to=grange[2,2],length=n)
expand.grid(X1=x1,X2=x2)}
xgrid=make.grid(x)
ygrid=predict(svmfit,xgrid)
beta=drop(t(svmfit$coefs)%*%x[svmfit$index,]) # Jetzt geht's um die Linien
beta0=svmfit$rho
plot(xgrid,main="SVs (Rauten), Margins",col=c("red","blue")[as.numeric(ygrid)],pch=20,cex=.2)
points(x,col=y+3,pch=19) # Datenpunkte einzeichnen
points(x[svmfit$index,],pch=5,cex=2) # Rauten um "Support Vectors" malen
abline(beta0/beta[2],-beta[1]/beta[2]) # Linien für "Margin" ziehen
abline((beta0-1)/beta[2],-beta[1]/beta[2],lty=2)
abline((beta0+1)/beta[2],-beta[1]/beta[2],lty=2) # Aufgehübschtes Ergebnis:
##t

##R 12.2 Quelle: ISLR, Ch 9.6
set.seed(1)
x=matrix(rnorm (200*2) , ncol=2) # 2D-Daten mit nichtlinearer Klassengrenze anlegen
x[1:100,]=x[1:100,]+2
x[101:150 ,]=x[101:150,]-2
y=c(rep(1,150) ,rep(2,50))
dat=data.frame(x=x,y=as.factor(y))
par(mfrow=c(1,2))
plot(x,col=y,main="Simulated Data",pch=20)
train=sample(200,100) # 50% Training-Sample anlegen
#Kreuzvalidierung mit tune() für Cost- und gamma-Gitter durchführen
set.seed(1)
tune.out=tune(svm, y~.,data=dat[train,],kernel="radial",ranges=list(cost=c(0.1,1,10,100,1000),gamma=c(0.5,1,2,3,4)))
summary(tune.out)
table(true=dat[-train,"y"],pred=predict(tune.out$best.model,newdata=dat[-train,])) # Test-Ergebnis: 10% Fehlklassifikation
# ROC-Kurve für bestes Modell plotten (gamma=2, cost=1)
library(ROCR)
rocplot=function(pred ,truth,...) { predob=prediction(pred,truth)
perf=performance(predob,"tpr","fpr")
plot(perf,...) }
svmfit=svm(y~.,data=dat[train,],kernel="radial",gamma=2,cost=1,decision.values=T)
pred.trn=attributes(predict(svmfit,dat[train,],decision.values=T))$decision.values
rocplot(pred.trn,dat[train,"y"],main="ROC Curve",col="blue",lwd=2)
pred.tst=attributes(predict(svmfit,dat[-train,],decision.values=T))$decision.values
rocplot(pred.tst,dat[-train,"y"],add=T,col="green",lwd=2)
abline(0,1)
##t


#13. Entscheidungsbäume: Splits, Pruning, Bagging und Random Forests
##R 13.1 siehe auch: Kap.08 und ISLR, Ch 8.5
library(MASS)
require(tree) # Synonym für library(tree)
par(mfrow=c(1,3))                         
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2) # 50%-Trainingsstichprobe definieren
test.medv=Boston[-train,"medv"]
tree.boston=tree(medv~.,Boston,subset=train) # Entscheidungsbaum rechnen
plot(tree.boston)
text(tree.boston,pretty=0)
yhat=predict(tree.boston,newdata=Boston[-train,]) # Modell auf Testdaten anwenden
plot(yhat~test.medv,main="Ergebnis: Entscheidungsbaum",xlim=c(5,50),ylim=c(5,50))
print(paste("RMSE:",sqrt(mean((yhat - test.medv)^2)))) # Ergebnis: RMSE=5.005
abline(0,1)
# Zum Vergleich: Lineares Modell (siehe Kap. 8)
fit2=lm(medv~.-age-indus,Boston,subset=train)
summary(fit2)
yhat.lm=predict(fit2,newdata=Boston[-train,])
print(paste("RMSE:",sqrt(mean((yhat.lm - test.medv)^2)))) # Ergebnis: RMSE=5.120
plot(yhat.lm~test.medv,main="Vergleich: Lineares Modell",xlim=c(5,50),ylim=c(5,50))
abline(0,1)
##t

##R 13.2 Quelle: ISLR, Ch 8.5
library(randomForest)
set.seed(1)
rf.boston=randomForest(medv~.,data=Boston,subset=train,mtry=6,importance=TRUE)
yhat.rf=predict(rf.boston,newdata=Boston[-train,])
print(paste("RMSE:",sqrt(mean((yhat.rf - test.medv)^2)))) # Ergebnis: RMSE=3.389
varImpPlot(rf.boston) # Variable Importance plotten:
##t


#14. Gradient Boosting Machines, Sparse Data und XGBOOST

##R 14.1 Quelle: ISLR, Ch 8.5
library(MASS)
set.seed(1)
train = sample(1:nrow(Boston), nrow(Boston)/2) # 50%-Trainingsstichprobe definieren
library(gbm)
set.seed(1)
boost.boston=gbm(medv~.,data=Boston[train,],distribution="gaussian",n.trees=5000,interaction.depth=4)
summary(boost.boston)
yhat.boost=predict(boost.boston,newdata=Boston[-train ,],n.trees=5000)
print(paste("RMSE:",sqrt(mean((yhat.boost - test.medv)^2)))) # Ergebnis: RMSE=3.442
par(mfrow=c(1,2)) # Ergebnis (hier für 5.000 Trees):
plot(boost.boston,i="rm") # Der mittlere Hauspreis steigt mit der Raumanzahl
plot(boost.boston,i="lstat") # und fällt mit steigendem Anteil der Sozial-Haushalte
##t

##R 14.2
library(xgboost)
data(agaricus.train, package='xgboost'); train <- agaricus.train
data(agaricus.test, package='xgboost'); test <- agaricus.test
class(train$data)[1] # Ergebnis: "dgCMatrix" - sparse matrix
class(train$label) # Ergebnis: "numeric": label=1 bedeutet "giftig".
summary(as.matrix(test$data)) # Ergebnis: u.a. min, mean, max für 126 Pilzeigenschaften
dtrain=xgb.DMatrix(data = train$data, label = train$label)
# Boosting Trees (default: booster="gbtree")
set.seed(1)
bst=xgboost(data=dtrain,max_depth=2,eta=1,nrounds=2,objective="binary:logistic")
pred <- predict(bst, test$data)
prediction <- as.numeric(pred > 0.5) # Aus Regressionsergebnis Klassifikation machen
err <- mean(as.numeric(pred > 0.5) != test$label) # Anteil Fehlklassifikationen berechnen
print(paste("test-error=", err)) # Ergebnis: "test-error= 0.0217"
table(prediction ,test$label) # Kreuztabelle ausgeben
# Regression Boosting (hier Logistische Regression)
set.seed(1)
bst=xgboost(data=dtrain,max_depth=2,booster="gblinear",nrounds=2,objective="binary:logistic")
pred <- predict(bst, test$data); prediction <- as.numeric(pred > 0.5)
err <- mean(as.numeric(pred > 0.5) != test$label) # Ergebnisse:
print(paste("test-error (lineares Boosting)=", err))
table(prediction ,test$label)
##t


#15. Künstliche Neuronale Netze (1): Einführung, einfache Regressionsmodelle

##R 15.1
library(neuralnet); set.seed(1)
x=runif(30,min=0,max=100) # 30 Zufallszahlen zwischen 0 und 100 simulieren,
y=sqrt(x) # die Quadratwurzel berechnen und beides
train=data.frame(cbind(x,y)) # in eine Trainingsdatei schreiben
# Neuronales Netz trainieren: Multi-Layer Perceptron mit 1 "hidden layer" und 2 "Neuronen"
set.seed(1)
net=neuralnet(y~x,train,hidden=2) # Default: act.fct="logistic"
print(net)
plot(net) # Grafische Ausgabe der Netzstruktur und Gewichte:
##t

##R 15.2
t=((1:14)^2) # Testdaten mit Quadratzahlen erzeugen
pred=compute(net,t); y.test=as.vector(pred$net.result); sqrt.x=sqrt(t)
test=data.frame(cbind(sqrt.x,y.test))
plot(test,pch=19,col="red",xlim=c(0,14),ylim=c(0,14))
abline(0,1) # Graphik:
##t

##R 15.3
require(MASS)
set.seed(1)
smp=sample(1:nrow(Boston),nrow(Boston)/2)
train=Boston[smp,]
test=Boston[-smp,]
mins=apply(train,2,min) # Normalisierung/Skalierung
rng=apply(train,2,max) - apply(train,2,min)
sc_train=scale(train,mins,rng)
sc_test=scale(test, mins,rng)
formula=paste("medv ~", paste(colnames(Boston[1:13]),collapse='+'))
nn=neuralnet(formula, data=sc_train, hidden=c(5,3,2), linear.output=T)
predictions=compute(nn, sc_test[,1:13])
predicted_values=(predictions$net.result * rng[14]) + mins[14]
print(paste("RMSE:",sqrt(mean((test[,14] - predicted_values)^2)))) # Ergebnis: RMSE=3.334
##t


#16. Künstliche Neuronale Netze (2): Multidimensionale Klassifikation, weitere Netztypen

##R 16.1
require(MASS)
require(neuralnet)
spec = model.matrix( ~ Species - 1, data=iris )
colnames(spec) = c("setosa", "versicolor", "virginica")
# Datensatz teilen und normalisieren
set.seed(1)
smp = sample(1:nrow(iris), 100)
d_train = iris[smp, 1:4]
d_test = iris[-smp, 1:4]
mins = apply(d_train, 2, min)
maxs = apply(d_train, 2, max)
sc_train = cbind(scale(d_train,mins,maxs-mins),spec[smp,])
sc_test = cbind(scale(d_test ,mins,maxs-mins),spec[-smp,])
summary(sc_train)
# NN Multi-Layer-Perceptron (MPL) trainieren. 1 hidden layer, voll verknüpft
set.seed(1)
n=names(iris)
f=as.formula(paste("setosa+versicolor+virginica~",paste(n[!n %in% "Species"],collapse="+")))
net=neuralnet(f, data=sc_train, hidden=c(2), linear.output=F)
plot(net) # Grafische Ausgabe der Netzstruktur und Gewichte:
##t

##R 16.2
pred = compute(net, sc_test[,1:4])
y_pred = apply(pred$net.result,1,which.max) # Höchsten Wert auswählen,
y_true = apply(sc_test[,5:7],1,which.max) # auf Testdaten anwenden und
confusion_matrix = table(y_true, y_pred) # Genauigkeit berechnen:
print(confusion_matrix)
print(paste("Accuracy:",sum(diag(confusion_matrix))/sum(confusion_matrix)))
##t


#17. Personalisierte Empfehlungen: Kollaboratives Filtern und Singulärwertzerlegung

##R 17.1
library(recommenderlab) # Die Filmbewertungen der Nutzer ansehen:
data(MovieLense) # Daten bereitstellen. Die Datei enthält 99.392 Filmbewertungen
print(MovieLense) # Matrixgröße/-art anzeigen: n=943 User (Zeilen), p=1664 Filme (Spalten)
print(table(as.vector(as(MovieLense, "matrix")))) # "Sterneverteilung" (1 bis 5) anzeigen
summary(colCounts(MovieLense)) # Filmbewertungen je Nutzer: Median 59.73
summary(rowCounts(MovieLense)) # Bewertungen je Film: Median 105.4
print(colCounts(MovieLense[,1:5])) # Anzahl der Bewertungen für die ersten 5 Filme
print(colMeans(MovieLense)[1:5]) # Durchschnittsrating (Spaltendurchschnitt) -"-
# Kollaboratives Filtern: Cosinus-Ähnlichkeiten zu Film 1 (Toy Story) berechnen
cos.film1=similarity(MovieLense[,1],MovieLense[,-1],method="cosine",which="items")
colnames(cos.film1)[which(cos.film1>0.69)] # Filme mit der höchsten Ähnlichkeit. Ergebnis:
##t

##R 17.2
library(irlba) # Mindestanzahl an Bewertungen für Filme und Nutzer stellen:
rm=MovieLense[rowCounts(MovieLense)>10,colCounts(MovieLense)>50]
rm.n=normalize(rm,row=TRUE) # Bewertungen je Nutzer standardisieren
rm.dm=as(rm.n,"matrix") # Werte in eine "normale" (dense) Matrix schreiben
rm.dm[is.na(rm.dm)]=0 # Fehlende Werte auf 0 setzen.
film<-39 # Film, hier "Star Wars", auswählen
print(paste("Choosen film:", colnames(rm.dm)[film]))
print(colCounts(rm[,film])) # (583 von 943 Nutzern haben den Film bewertet)
label=as.factor(as.numeric(rm.dm[,film]!=0)) # Antwortvektor (1: Film gesehen) erzeugen## Singulärwertzerlegung ohne den ausgewählten Film durchführen
SVD=irlba(rm.dm[,-film],nv=50,nu=50) # nv,nu: Anzahl rechte,linke Singulärvektoren
#rotation <- data.frame(movies=colnames(rm.dm[,-film]),SVD$v)
print(dim(rm.dm));print(dim(SVD$u));print(dim(SVD$v)) # Check der Matrixgrößen
print(length(SVD$d))
# Random Forest trainieren und testen, Prognosegüte bewerten
library(randomForest)
set.seed(1)
train=sample(1:length(label),500) # Trainingsstichprobe mit 500 von 943 Nutzern
u.t=as.data.frame(SVD$u[train,]) # Trainingsdaten mit den U-Komponenten (User)
rf=randomForest(label[train] ~.,data=u.t,importance=TRUE)
u.v=as.data.frame(SVD$u[-train,]) # Testdaten erzeugen
pred=predict(rf,newdata=u.v,n.trees=model$n.trees) # Testdaten scoren
c.m=table(label[-train],pred) # Confusion Matrix berechnen und ausgeben
precision=c.m[2,2]/sum(c.m[,2]);recall=c.m[2,2]/sum(c.m[2,]);print(c.m)
print(paste("Precision:",round(precision,3)," Recall:",round(recall,3))) # Ergebnis:
##t


#18. Entziffern mit Random Forest und XGBOOST, Laufzeiten, kaggle.com

##R 18.1
library(readr) # Nötig für read_csv
train=read_csv("D:/downloads/mnist_train.csv") # 42000 Images mit 784 Bildpunkten + label
test =read_csv("D:/downloads/mnist_test.csv") # 28000 Images mit 784 Bildpunkten
par( mfrow = c(1,20)) # in RStudio Plot-Fenster entsprechend anpassen. Nun Pixel aufbereiten:
for (i in 1:20){ y = as.matrix(train[i, 2:785]); dim(y) = c(28, 28)
image( y[,nrow(y):1], axes = FALSE, col = gray(255:0/255) )
text( 0.2, 0, train[i,1], cex = 4, col = 2, pos = 3 ) } # und Ziffernwert rot überblenden:
##t

##R 18.2 Achtung: Ein paar Minuten Laufzeit möglich. Für kurzen Test ntree=2 setzen
library(randomForest)
set.seed(1)
rows=sample(1:nrow(train),10000) # Liste von 10000 Zufallszahlen
labels=as.factor(train[rows,]$label) # Spalte "label" enthält Ziffer
train=train[rows,-1] # je Obs. 28*28 Bildpunkte (pixel0 bis pixel783), ohne label
tic <- proc.time() # Startzeit merken
rf=randomForest(train, labels, xtest=test, ntree=2) # ntree=250
print(proc.time() - tic) # Training-Laufzeit des Random Forest
pred=data.frame(ImageId=1:nrow(test), Label=levels(labels)[rf$test$predicted])
head(pred)
write_csv(pred,"D:/downloads/mnist_submission_rf1.csv") # auf kaggle.com submitten
##t

##R 18.3 Achtung: Ein paar Minuten Laufzeit möglich. Für kurzen Test nrounds=2 setzen
library(xgboost)
train=read.csv("D:/downloads/mnist_train.csv") # 42000 Images, 784 Bildpunkte + label
test =read.csv("D:/downloads/mnist_test.csv") # 28000 Images, 784 Bildpunkte
train.images=train[,-1] # Bilddaten und Ziffern in verschiedene Objekte schreiben
train.digits=train[,1]
# Boosting Trees (default: booster="gbtree", eta=0.3, max_depth=6)
set.seed(1)
tic <- proc.time() # Startzeit merken
mod=xgboost(data=xgb.DMatrix(model.matrix(~.,data=train.images),label=train.digits),
            num_class=10, nrounds=2, early.stop.round=3,
            params=list(objective="multi:softmax",eval_metric="merror")) #nrounds=100
print(proc.time() - tic) # Training-Laufzeit xgboost berechnen
pred=predict(mod,newdata=xgb.DMatrix(model.matrix(~.,data=test)))
write.csv(data.frame(ImageId=1:nrow(test),Label=pred),"mnist_xgb.csv",quote=F,row.names=F)
##t


#19. Handschrifterkennung mit MXNet: Deep Learning und Convolutional Neural Nets

##R 19.1 Achtung: Längere Laufzeit möglich. Für ersten Test num.nround=1 (statt 30) setzen
install.packages("drat", repos="https://cran.rstudio.com")
drat:::addRepo("dmlc")
install.packages("mxnet")
require(mxnet)
train <- read.csv('D:/downloads/kaggle-mnist_train.csv', header=TRUE)
test <- read.csv('D:/downloads/kaggle-mnist_test.csv', header=TRUE)
train <- data.matrix(train); test <- data.matrix(test)
train.x <- train[,-1] ; train.y <- train[,1]
train.x <- t(train.x/255) ; test <- t(test/255)
table(train.y)
# Konfiguration des vollständig verknüpften Netzwerks
data <- mx.symbol.Variable("data") # Input-Layer
fc1 <- mx.symbol.FullyConnected(data, name="fc1", num_hidden=128)
act1 <- mx.symbol.Activation(fc1, name="relu1", act_type="relu")
fc2 <- mx.symbol.FullyConnected(act1, name="fc2", num_hidden=64)
act2 <- mx.symbol.Activation(fc2, name="relu2", act_type="relu")
fc3 <- mx.symbol.FullyConnected(act2, name="fc3", num_hidden=10) # Output-Layer
softmax=mx.symbol.SoftmaxOutput(fc3, name="sm")
mx.set.seed(1)
model <- mx.model.FeedForward.create(softmax, X=train.x, y=train.y, ctx= mx.cpu(),
                                     num.round=1, array.batch.size=100, learning.rate=0.07, momentum=0.9,
                                     eval.metric=mx.metric.accuracy, initializer=mx.init.uniform(0.07),
                                     epoch.end.callback=mx.callback.log.train.metric(100)) # num.round=30
# Vorhersage und Submission 
preds <- predict(model, test)
pred.label <- max.col(t(preds))-1; table(pred.label)
submission <- data.frame(ImageId=1:ncol(test), Label=pred.label)
write.csv(submission, file='mnist_mx1fc.csv', row.names=FALSE, quote=FALSE)
##t

##R 19.2 Achtung: Längere Laufzeit möglich. Für ersten Test num.nround=1 (statt 20) setzen
Data= mx.symbol.Variable('data') # Definition der Faltungen:
conv1=mx.symbol.Convolution(data=data, kernel=c(5,5), num_filter=20) # first conv
tanh1=mx.symbol.Activation(data=conv1, act_type="tanh")
pool1=mx.symbol.Pooling(data=tanh1, pool_type="max", kernel=c(2,2), stride=c(2,2))
conv2=mx.symbol.Convolution(data=pool1, kernel=c(5,5), num_filter=50) # second conv
tanh2=mx.symbol.Activation(data=conv2, act_type="tanh")
pool2=mx.symbol.Pooling(data=tanh2, pool_type="max", kernel=c(2,2), stride=c(2,2))
flatten=mx.symbol.Flatten(data=pool2)
fc1 =mx.symbol.FullyConnected(data=flatten, num_hidden=500) # first full connection
tanh3=mx.symbol.Activation(data=fc1, act_type="tanh")
fc2=mx.symbol.FullyConnected(data=tanh3, num_hidden=10) # second full connection
lenet= mx.symbol.SoftmaxOutput(data=fc2) # loss
train.array=train.x; dim(train.array)=c(28, 28, 1, ncol(train.x))
test.array=test ; dim(test.array)=c(28, 28, 1, ncol(test))
mx.set.seed(0)
tic=proc.time()
model= mx.model.FeedForward.create(lenet, X=train.array, y=train.y,ctx=mx.cpu(),
                                   num.round=1, array.batch.size=100, learning.rate=0.05, momentum=0.9,
                                   wd=0.00001,eval.metric=mx.metric.accuracy,
                                   epoch.end.callback=mx.callback.log.train.metric(100)) #num.round=20
print(proc.time() - tic)
preds=predict(model, test.array)
pred.label=max.col(t(preds))-1; table(pred.label)
submission <- data.frame(ImageId=1:ncol(test), Label=pred.label)
write.csv(submission, file='mnist_mx2conv.csv', row.names=FALSE, quote=FALSE)
##t


#20. GPU-Computing mit MXNet: Graphikkarten und Software

##R 20.1 Benöigt Objekt lenet (R 19.2) und Daten aus Kap. 19
require(mxnet)
n.gpu=1
device.gpu=lapply( 0:(n.gpu-1),function(i) { mx.gpu(i) } ) mx.set.seed(0)
tic=proc.time()
model= mx.model.FeedForward.create(lenet, X=train.array, y=train.y, ctx=device.gpu,
                                   num.round=20, array.batch.size=100, learning.rate=0.05, momentum=0.9,
                                   wd=0.00001,eval.metric=mx.metric.accuracy,
                                   epoch.end.callback=mx.callback.log.train.metric(100))
print(proc.time() - tic)
preds=predict(model, test.array)
pred.label=max.col(t(preds))-1; table(pred.label)
submission <- data.frame(ImageId=1:ncol(test), Label=pred.label)
write.csv(submission, file='mnist_mx3gpu.csv', row.names=FALSE, quote=FALSE)
##t


#21. Big Data mit H2O: Java-Umgebung einrichten, neuronales Netz trainieren, R vs. FLOW

##R 21.1
# Packages, die von H2O benötigt werden, installieren
if (! ("metods" %in% rownames(installed.packages()))) { install.packages("methods") }
if (! ("statmod" %in% rownames(installed.packages()))) { install.packages("statmod") }
if (! ("stats" %in% rownames(installed.packages()))) { install.packages("stats") }
if (! ("graphics" %in% rownames(installed.packages()))) { install.packages("graphics") }
if (! ("RCurl" %in% rownames(installed.packages()))) { install.packages("RCurl") }
if (! ("jsonlite" %in% rownames(installed.packages()))) { install.packages("jsonlite") }
if (! ("tools" %in% rownames(installed.packages()))) { install.packages("tools") }
if (! ("utils" %in% rownames(installed.packages()))) { install.packages("utils") }
# Das H2O-Package herunterladen, installieren und initialisieren (Stand 15.10.2016)
install.packages("h2o",type="source",repos=(c("http://h2o-release.s3.amazonaws.com/h2o/rel-turing/8/R")))
library(h2o)
localH2O = h2o.init(nthreads=-1) # -1: alle Kerne nutzen
demo(h2o.kmeans) # Demo starten, H2O kennen lernen
##t

##R 21.2 Neuronales Netz mit H2O trainieren (Anmerkung: Keine Testdaten)
airlines.hex <- h2o.importFile(path = normalizePath("allyears2k.csv"), destination_frame = "allyears2k.hex")
summary(airlines.hex) # Überblick über die Airlines-Daten
y <- "IsDepDelayed" # Kennzeichen für Flugverspätung
x <- c("Dest","Origin","DayofMonth","Year","UniqueCarrier","DayOfWeek","Month","Distance")
dl_model <- h2o.deeplearning(x = x, y = y, training_frame = airlines.hex, distribution = "bernoulli",
                             model_id = "H20_NN_R",epochs = 100, hidden = c(200,200),
                             target_ratio_comm_to_comp = 0.02, seed = 12345, variable_importances = T)
auc2 <- h2o.auc(object = dl_model)
print(paste0("AUC of the training set : ", round(auc2, 4)))
print(h2o.varimp(dl_model))
print(h2o.scoreHistory(dl_model))
##t



#Anhang
R-Skript zur Installation der verwendeten R-Packages (ohne MXNet, H2O, doMC)
##R Anhang
if (!"ISLR" %in% rownames(installed.packages())) {install.packages("ISLR")}     # Daten 
if (!"boot" %in% rownames(installed.packages())) {install.packages("boot")}     # bootstrapping
if (!"ChainLadder" %in% rownames(installed.packages())) {install.packages("ChainLadder")}   
if (!"class" %in% rownames(installed.packages())) {install.packages("class")}   # knn
if (!"copula" %in% rownames(installed.packages())) {install.packages("copula")}   
if (!"e1071" %in% rownames(installed.packages())) {install.packages("e1071")}   # SVM
if (!"gbm" %in% rownames(installed.packages())) {install.packages("gbm")}   
if (!"glmnet" %in% rownames(installed.packages())) {install.packages("glmnet")} # LASSO, Elasticnet
if (!"irlba" %in% rownames(installed.packages())) {install.packages("irlba")}   # SVD
if (!"MASS" %in% rownames(installed.packages())) {install.packages("MASS")}     # Boston-Daten
if (!"neuralnet" %in% rownames(installed.packages())) {install.packages("neuralnet")}   
if (!"randomForest" %in% rownames(installed.packages())) {install.packages("randomForest")}   
if (!"readr" %in% rownames(installed.packages())) {install.packages("readr")}   # read_csv
if (!"recommenderlab" %in% rownames(installed.packages())) {install.packages("recommenderlab")}   
if (!"ROCR" %in% rownames(installed.packages())) {install.packages("ROCR")}     # ROC-Kurve
if (!"tree" %in% rownames(installed.packages())) {install.packages("tree")}   
if (!"vcd" %in% rownames(installed.packages())) {install.packages("vcd")}       # mosaic plots
if (!"xgboost" %in% rownames(installed.packages())) {install.packages("xgboost")}   
##t

