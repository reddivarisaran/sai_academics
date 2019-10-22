###--------------------------------------
#Student Name:REDDIVARI SAI SARAN 
#GNumber:G01142501
###--------------------------------------

rm(list=ls())

data <- read.csv(file.choose())

# a. Find the number of rows and columns in the dataset (5 points)

row(data)
col(data)
nrow(data)
ncol(data)


# b. Find the maximum Age in the dataset (5 points)

max(data$Age)

 
# c. Find the minimum DailyRate in the dataset (5 points)

min(data$DailyRate)


# d. Find the average/mean MontlyIncome in the dataset (5 points)

mean(data$MonthlyIncome)


# e. How many employees rated WorkLifeBalance as 1 (5 points)

coun<-data$WorkLifeBalance
library(plyr)
count(coun,1)
    
        #or

sum(data$WorkLifeBalance == 1)

       #or

output<-length(which(data$WorkLifeBalance ==1))
output



# f. What percent of total employees have TotalWorkingYears less than equal to 5? Also calculate the percentage for TotalWorkingYears greater than 5 (5 points)

workyear1<-sum(data$TotalWorkingYears <= 5)
workyear2<-sum(data$TotalWorkingYears > 5)

percentage1<-(workyear1/1470)*100
percentage2<-(workyear2/1470)*100

percentage1
percentage2



# g. Print EmployeeNumber, Department and MaritalStatus for those employees whose Attrition is Yes and RelationshipSatisfaction is 1 and YearsSinceLastPromotion is greater than 3 (10 points)


data[(data$Attrition=='Yes') & (data$RelationshipSatisfaction==1) & (data$YearsSinceLastPromotion>3),c(10,5,18)]


# h. Find the mean, median, mode, standard deviation and frequency distribution of EnvironmentSatisfaction for males and females separately. (Hint: For frequency distribution use table() function (10 points)
 

#for data separation

males1<- subset(data, data$Gender== 'Male')
females1<-subset(data, data$Gender== 'Female')

#for mode function

mode = function(x){ unx=unique(x) 
unx[which.max(tabulate(match(x,unx)))]}


#for males

mean(males1$EnvironmentSatisfaction)
median(males1$EnvironmentSatisfaction)
mode(males1$EnvironmentSatisfaction)
sd(males1$EnvironmentSatisfaction)
summary(males1$EnvironmentSatisfaction)

#for females

mean(females1$EnvironmentSatisfaction)
median(females1$EnvironmentSatisfaction)
mode(females1$EnvironmentSatisfaction)
sd(females1$EnvironmentSatisfaction)
summary(females1$EnvironmentSatisfaction)
 
#frquency distribution for male and female

ftable(males1$EnvironmentSatisfaction)
ftable(females1$EnvironmentSatisfaction)


