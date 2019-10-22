data= read.csv(file.choose())
setwd("C:/sai saran/project580")

library(ggplot2)
library(corrplot)

winedata<-read.csv("winequalityN.csv", sep=",")

#coorelation plot
plot_cor<-corrplot(cor(winedata[,-1]),method="circle", order="hclust",type="upper")


#scatter plots
ggplot(winedata, aes(free.sulfur.dioxide, total.sulfur.dioxide,color=type))+geom_point()+
  ggtitle("Scatter plot for Total Sulphur Dioxide Vs Free Sulphur Dioxide ")+
  xlab("Free Sulphur Dioxide")+ylab("Total Sulphur Dioxide")

#boxplot
boxplot(alcohol~quality,data=winedata,xlab="Quality", ylab="Alcohol %",main="Box plot for Quality and Alcohol %")

#regression plots
lmdata<-lm(quality~.,winedata[,-1])
plot(lmdata)

#hypothesis test
t.test(winedata[,-1],alternative="two.sided",mu=7)

#scatterplot matrix
pairs(winedata[,-1], pch = 19,  cex = 0.5,
      col = c("blue","red")[winedata$type],
      lower.panel=NULL)

#histograms
par(mfrow=c(3,3))
hist(winedata$fixed.acidity,main="Histogram of Fixed Acidity")
hist(winedata$volatile.acidity,main="Histogram of Volatile Acidity")
hist(winedata$citric.acid,main="Histogram of Citric Acid")
hist(winedata$free.sulfur.dioxide,main="Histogram of Free Sulphur Dioxide")
hist(winedata$total.sulfur.dioxide,main="Histogram of Total Sulphur Dioxide")
hist(winedata$sulphates,main="Histogram of Sulphates")
hist(winedata$density,main="Histogram of Density")
hist(winedata$pH,main="Histogram of pH")
hist(winedata$chlorides,main="Histogram of Chlorides")