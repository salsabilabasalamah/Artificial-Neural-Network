#install.packages("nnet")
library(nnet)
data.titanic=read.csv("E:\\BIML\\titanic3.csv", header=T)
library(data.table)
data.titanic=data.table(data.titanic)
dim(data.titanic)
titanic.baru=na.omit(data.titanic, cols=c("survived","boat", "pclass", "sex","age"))
dim(titanic.baru)
head(titanic.baru)
faktor=cbind(titanic.baru$survived,titanic.baru$pclass,titanic.baru$sex,titanic.baru$boat)
View(faktor)

head(faktor)
Titanic<-data.frame(faktor)
View(Titanic)
target=(titanic.baru$survived)
target

#setengah dari data titanic di gunakan untuk menjadi data training
#dan setengahnya lagi di jadikan data tetsing dengan dibagi menjadi 3 sampel.
sampel <- c(sample(1:282,141), sample(283:545,146),sample(546:1046,250))
sampel

#memisahkan data train dan test
titanic.train<-Titanic[sampel,]
titanic.train
titanic.test<-Titanic[-sampel,]
titanic.test

#percobaan pertama
titanic1 <- nnet(Titanic,target, data=titanic.train, size = 3, 
                 decay = 5e-4, maxit = 600)
#bobot 16 dari haisl output
#berhenti setelah 260x iterasi
#dengan menggunakan data train sudah convergen dengan 260x iterasi

#mencoba melakuka prediksi menggunakan data tetsing

#membuat model
library(neuralnet)
model <- neuralnet(Titanic,titanic.train,
                   hidden=c(4,5,2),
                   linear.output=TRUE)
summary(model)
## plot model
plot(model)


#data baru
pclass<-2
name=c(alex)
sex<-2
age<-40
boat<-12
class(Titanic)
titanicbaru<-data.frame(pclass,name,sex,age,boat)
titanicbaru
prediksi<-predict(titanic1,titanicbaru)
prediksi

#testing model dengan data tets
prediksi<-compute(model, titanicbaru)
prediksi
