install.packages("ggplot2")
library(ggplot2)
install.packages("mlbench")
library(mlbench)
install.packages("AppliedPredictiveModeling")
library(AppliedPredictiveModeling)
installed.packages("caret")
library(caret)
install.packages("e1071")
library(e1071)


#data 

data("Glass")
show(Glass)
str(Glass)

#3.1 
#3.1(a)

#pairwise scatter plot 

pairs(Glass)

#correlation 

cor(Glass[,-10])
cor(Glass[,-10], as.numeric(Glass[,10]))

#3.1(b)
#outliers

#method#1

par(mfrow=c(1,1))
boxplot(Glass)

#method#2

par(mfcol=c(1,3))
boxplot(Glass$RI,main="RI")
boxplot(Glass$Na,main="Na")
boxplot(Glass$Mg,main="Mg")
par(mfcol=c(1,3))
boxplot(Glass$Al,main="Al")
boxplot(Glass$Si,main="Si")
boxplot(Glass$K,main="K")
par(mfcol=c(1,3))
boxplot(Glass$Ca,main="Ca")
boxplot(Glass$Ba,main="Ba")
boxplot(Glass$Fe,main="Fe")


#Variable vs Type 

par(mfrow=c(1,3)) 
boxplot( Glass$Na ~ Glass$Type,main="Na" )
boxplot( Glass$Si ~ Glass$Type ,main="Si")
boxplot( Glass$Ca ~ Glass$Type ,main="Ca")
par(mfrow=c(1,3)) 
boxplot( Glass$Mg ~ Glass$Type, main="Mg" )
boxplot( Glass$Al ~ Glass$Type, main ="Al" )
boxplot( Glass$K ~ Glass$Type, main="K" )
par(mfrow=c(1,3)) 
boxplot( Glass$RI ~ Glass$Type, main="RI" )
boxplot( Glass$Ba ~ Glass$Type, main ="Ba" )
boxplot( Glass$Fe ~ Glass$Type, main="Fe" )

#skewness

apply( Glass[,-10], 2, skewness )

#skewed predictor

par(mfrow=c(1,3))
hist( Glass$K )
hist( Glass$Ba ) 
hist( Glass$Mg )
par(mfrow=c(1,3))
hist( Glass$Na )
hist( Glass$Si ) 
hist( Glass$Ca )
par(mfrow=c(1,3))
hist( Glass$Al )
hist( Glass$Fe ) 
hist( Glass$RI )

#correlation Structure

library(corrplot)
corrplot(cor(Glass[,-10]),order="hclust")

#3.1(c)

#Relevant Transformations

#PCA
Glass_data1<-preProcess(Glass,method=c("BoxCox","center","scale","pca"))
preProcess(x = Glass, method = c("BoxCox", "center","scale", "pca"))
Glass_trans<-predict(Glass_data1,Glass)
head(Glass_trans[, 1:5])   

#BOXCOX
Glass_data <-preProcess(Glass[,-10], method="BoxCox")
Glass_train <- predict(Glass_data, Glass[,-10])
apply( Glass_train[,-10], 2, skewness )

#Transformed Skewed Predictor

par(mfrow=c(1,3))
hist( Glass_train$K )
hist( Glass_train$Ba ) 
hist( Glass_train$Mg )
par(mfrow=c(1,3))
hist( Glass_train$Na )
hist( Glass_train$Si ) 
hist( Glass_train$Ca )
par(mfrow=c(1,3))
hist( Glass_train$Al )
hist( Glass_train$Fe ) 
hist( Glass_train$RI )

#Problem 3.2 
#3.2 (a)

#data
library(mlbench)
data("Soybean")
show(Soybean)
str(Soybean)

#near_Zero_Var

nzv<-nearZeroVar(Soybean,  saveMetrics = TRUE,names = TRUE)
nzv

#removing TRUE value from the data 

nzv<-nzv[-c(19,26,28),]
nzv

soybean_data_1<-subset(Soybean,select= -c(19,26,28))
#3.2(b)

#count_data
count_data <-sapply(soybean_data_1[,-1], function(y) sum(length(which(is.na(y)))))
count_data

#missing_data
#method1
Soybean_missing = apply(soybean_data_1[,-1],1,function(s){sum(is.na(s))>0})
table(soybean_data_1[, c(1,33)])

#method2
MissingValues_Count <- function(x){sum(is.na(x))}
RowMissingIndex<-apply(Soybean, 1,MissingValues_Count)

RowMissingIndex[grep("2-4-d-injury",Soybean$Class)]
RowMissingIndex[grep("alternarialeaf-spot",Soybean$Class)]
RowMissingIndex[grep("anthracnose",Soybean$Class)]
RowMissingIndex[grep("bacterial-blight",Soybean$Class)]
RowMissingIndex[grep("bacterial-pustule",Soybean$Class)]
RowMissingIndex[grep("brown-spot",Soybean$Class)]
RowMissingIndex[grep("brown-stem-rot",Soybean$Class)]
RowMissingIndex[grep("charcoal-rot",Soybean$Class)]
RowMissingIndex[grep("cyst-nematode",Soybean$Class)]
RowMissingIndex[grep("diaporthe-pod-&-stem-blight",Soybean$Class)]
RowMissingIndex[grep("diaporthe-stem-canker",Soybean$Class)]
RowMissingIndex[grep("downy-mildew",Soybean$Class)]
RowMissingIndex[grep("frog-eye-leaf-spot",Soybean$Class)]
RowMissingIndex[grep("herbicide-injury",Soybean$Class)]
RowMissingIndex[grep("phyllosticta-leaf-spot",Soybean$Class)]
RowMissingIndex[grep("phytophthora-rot",Soybean$Class)]
RowMissingIndex[grep("powdery-mildew",Soybean$Class)]
RowMissingIndex[grep("purple-seed-stain",Soybean$Class)]
RowMissingIndex[grep("rhizoctonia-root-rot",Soybean$Class)]

