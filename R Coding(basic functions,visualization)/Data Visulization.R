###------------------
###Visualization
###------------------

###Students Name:REDDIVARI SAI SARAN
###GNumber:G01142501


rm(list=ls())

data <- read.csv(file.choose())


# a) Create	Histogram	for	Age	using	R

ggplot(data, aes(Age))+ggtitle("Histogram diagram for Age")+geom_bar( width=0.7, fill="blue")

# Create	Scatter	Plot	for	Age	and	Monthly	Income	using	R

ggplot(data,aes( Age, MonthlyIncome))+ggtitle("Scatter plot for Age and Monthly Income ")+geom_point(colour='orange')
