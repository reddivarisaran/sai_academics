###------------------
###Regression and Clustering
###------------------

###Students Name:Reddivari SAI SARAN
###GNumber:G01142501



rm(list=ls())


library(ggplot2)

data <- read.csv(file.choose())

#a)graph for totalworking year and monthly income
scatter.smooth(data$TotalWorkingYears,data$MonthlyIncome,main="WORKINGYEARS-MONTHLYINCOME", span= 3/2, degree =1, xlab="TOTAL WORKING YEARS", ylab = "MONTHLY INCOME",  family= c("symmetric", "gaussian"),lpars = list(col= "red",lwd = 4, lty = 5))
#b)graph for Age and distancefromhome
scatter.smooth(data$Age,data$DistanceFromHome, main="AGE-DISTANCEfromHOME", span= 3/2, degree =1, xlab="Age", ylab = "Distance from Home",  family= c("symmetric", "gaussian"),lpars = list(col= "red",lwd = 4, lty = 10))
#correlation test for (a)
cor.test(data$Age,data$DistanceFromHome)
cor(data$Age,data$DistanceFromHome)
#correlation test for(b)
cor.test(data$TotalWorkingYears,data$MonthlyIncome)
cor(data$TotalWorkingYears,data$MonthlyIncome)
#linear regression
data_lr<-lm(data$TotalWorkingYears ~ data$MonthlyIncome, data = data)
print(data_lr)
summary(data_lr)
#creating data frame
data_df<-data.frame(data$HourlyRate,data$TotalWorkingYears)
data_df
#calulating clustering when k=3
datak3<-kmeans(data_df,3)
datak3
#calulating clustering when k=5
datak5<-kmeans(data_df,5)
datak5
#creating data frame for the kmean when k=3 and k=5
kmean_results <- data.frame(data$HourlyRate,data$TotalWorkingYears,datak3$cluster,datak5$cluster)
kmean_results                         
#plot for the clustering when k=3
plotk3<- ggplot(kmean_result, aes(kmean_result$data.HourlyRate,kmean_result$data.TotalWorkingYears,color=kmean_result$datak3.cluster))+geom_point()
plotk3
#plot for the clustering when k=5
plotk5<- ggplot(kmean_result, aes(kmean_result$data.HourlyRate,kmean_result$data.TotalWorkingYears,color=kmean_result$datak5.cluster))+geom_point()
plotk5



