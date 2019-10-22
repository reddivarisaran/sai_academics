#get the input
data1<-read.csv(file.choose())

#to view the output
data1

#assigning the data
year<-data1$Years
salary<-data1$StSalary
gender<-data1$Gender
degree<-data1$Degree

#Identify	data	types	for	each	attribute	in	the	dataset

typeof(year)
typeof(salary)
typeof(gender)
typeof(degree)

#data type of my dataset

typeof(data1)

#Produce	a	summary	statistics	for	each	attribute	in	the	dataset
summary(data1)
summary(year)
summary(salary)
summary(gender)
summary(degree)

#Produce	visualizations	for	each	attribute

hist(year, col='red', main='Histogram for year')
hist(salary, col='blue',main='Histogram for salary')
plot(gender, col='green',main='Histogram for gender')
plot(degree, col='yellow',main='Histogram for degree')


#instalization of tidyverse and ggplot2

install.packages('tidyverse')
install.packages('ggplot2')
library('ggplot2')

#Years	of	Experience	and	Starting	Salary	for	all	employees

ggplot(data1, aes(x=year,y=salary))+geom_point()+ggtitle('Years of Experience and Starting Salary for all employees')

#Years	of	Experience	and	Starting	Salary	for	each	gender

ggplot(data1, aes(year, y=salary,color=gender))+ggtitle('Years of Experience and Starting	Salary for each gender')+geom_point()


#Years	of	Experience	and	Starting	Salary	for	each	degree

ggplot(data1, aes(year,y=salary, shape=degree,color=degree))+geom_point()+ggtitle('Years of Experience and Starting Salary for each degree')

#Find	the	correlation	between	Starting	Salary	and	Years	of	Experience

cor(data1$Years, data1$StSalary,  method = "pearson", use = "complete.obs")
cor.test(data1$Years, data1$StSalary)

#a. Is	the	correlation	different	for	each	gender?	

male<-subset(data1,data1$Gender=='M')
female<-subset(data1,data1$Gender=='F')

cor(female$Years, female$StSalary,  method = "pearson", use = "complete.obs")
cor.test(female$Years, female$StSalary)

cor(male$Years, male$StSalary,  method = "pearson", use = "complete.obs")
cor.test(male$Years, male$StSalary)


#b. Is	the	correlation	different	for	each	degree?

bs<-subset(data1,data1$Degree=='BS')
ms<-subset(data1,data1$Degree=='MS')
phd<-subset(data1,data1$Degree=='PhD')

cor(bs$Years, bs$StSalary,  method = "pearson", use = "complete.obs")
cor.test(bs$Years, bs$StSalary)

cor(ms$Years, ms$StSalary,  method = "pearson", use = "complete.obs")
cor.test(ms$Years, ms$StSalary)

cor(phd$Years, phd$StSalary,  method = "pearson", use = "complete.obs")
cor.test(phd$Years, phd$StSalary)
