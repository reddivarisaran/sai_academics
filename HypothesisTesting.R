###------------------
###Hypothesis Testing
###------------------

###Students Name:REDDIVARI SAI SARAN
###GNumber:G01142501


rm(list=ls())

data <- read.csv(file.choose())
#a. If	the	MonthlyIncome	of	Males	is	greater	than	Females	(10	points)

male_data= which(data$Gender=='Male')
female_data= which(data$Gender=='Female')
t.test(data$MonthlyIncome[male_data],data$MonthlyIncome[female_data], alternative="greater",var.equal=T)

#b. If	the	WorkLifeBalance	of	Males	is	less	than	Females	(10	points)

t.test(data$WorkLifeBalance[male_data],data$WorkLifeBalance[female_data], alternative="less",var.equal=T)

#c. If	the	YearsAtCompany	of	Single is	less	than	Married	(10	points)

Single_data=which(data$MaritalStatus=='Single')
Married_data=which(data$MaritalStatus=='Married')
t.test(data$YearsAtCompany[Single_data],data$YearsAtCompany[Married_data],alternative = "less")

#d. If	the	EnvironmentalSatisfaction	of	Attrition=Yes	is	less	than	Attrition=No (10	points)

attrition_data=which(data$Attrition=='Yes')
attrition_data1=which(data$Attrition=='No')
t.test(data$EnvironmentSatisfaction[attrition_data],data$EnvironmentSatisfaction[attrition_data1],alternative = "less")

#e. If	the	MonthlyIncome	of	Manager	is	greater	than	Laboratory	Technician	(Hint:	Use	JobRole	to	find	Manager	and	Laboratory	Technician)	(10	points)

manager_data=which(data$JobRole=='Manager')
lab_data=which(data$JobRole=='Laboratory Technician')
t.test(data$MonthlyIncome[manager_data],data$MonthlyIncome[lab_data],alternative="greater")
