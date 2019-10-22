library(tidyverse)
library(readxl)
Data <- read_excel("Mason/Spring 2019/AIT 582/Opioid-Dashboard-Dataset-View.xlsx")
View(Data)


is.na(Data)
table(NA_Info)["TRUE"]
which(is.na(Data))
which(is.na(Step1$`Virginia Department of Emergency Management Region Case Count Display`))


unique(Data$Type)
Step1 <- subset(Data, Data$Type != "EMS Narcan" & Data$Type != "Diagnosed HIV" & Data$Type != "Reported Hepatitis C (18-30 year olds)" & Data$Type != "Neonatal Abstinance Syndrome")
unique(Step1$Type)
which(Step1=='*')
which(is.null(Step1))
which(Step1$`Case Count Display`== "*")
which(Step1$`VDH Health District Case Count Display`== "*")
which(Step1$`Virginia Hospital & Healthcare Preparedness Region Case Count Display`== "*")
which(Step1$`Community Service Board Case Count Display`== "*")
which(Step1$`Virginia State Police Division Case Count Display`== "*")

Numeric_Data <- as.numeric(Numeric_Data)

#faceted bar plot with deaths per region by opioid
region_opioid <- ggplot(Numeric, aes(y= Numeric$`VDH Health Region Case Count Display`, x= Numeric$`VDH Health Region`,  fill = Numeric$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_wrap(~Numeric$Type) + labs(title = "VDH Mortality Case Count by Health Region and Event Type", x = "VDH Health Region", y= "Deaths", fill = "VDH Health Region")
region_opioid

#All opioid deaths by year and region
all_opioids_time <- ggplot(Numeric, aes( y = Numeric$`VDH Health Region Case Count Display`, x = Numeric$`VDH Health Region`, fill= Numeric$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_grid(col = vars(Numeric$Year)) + labs(fill = "VDH Health Region", title = "Deaths Due to Overdose on Any Opioid by VDH Health Region (2011-2017)", x= "VDH Health Region", y= "Deaths Due to OD") +  theme(axis.text.x = element_blank())
#make tables with just specific drug type

Numeric_Heroin <- subset(Numeric, Numeric$Type == "ED Heroin Overdose")
Numeric_Prescription <- subset(Numeric, Numeric$Type == "Fatal Prescription Opioid Overdose")
Numeric_Fent <- subset(Numeric, Numeric$Type == "Fatal Fentanyl and/or Heroin Overdose")
Numeric_Opioid <- subset(Numeric, Numeric$Type == "ED Opioid Overdose")
#total graph
ll_time <- ggplot(Numeric, aes( y = Numeric$`VDH Health Region Case Count Display`, x = Numeric$`VDH Health Region`, fill= Numeric$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_grid(col = vars(Numeric$Year)) + labs(fill = "VDH Health Region", title = "Deaths Due to Any Opioid Overdose by VDH Health Region (2011-2017)", x= "VDH Health Region", y= "Deaths Due to OD") +  theme(axis.text.x = element_blank())
#heroin graph
heroin_time <- ggplot(Numeric_Heroin, aes( y = Numeric_Heroin$`VDH Health Region Case Count Display`, x = Numeric_Heroin$`VDH Health Region`, fill= Numeric_Heroin$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_grid(col = vars(Numeric_Heroin$Year)) + labs(fill = "VDH Health Region", title = "Deaths Due to Heroin Overdose by VDH Health Region (2011-2017)", x= "VDH Health Region", y= "Deaths Due to Heroin OD") +  theme(axis.text.x = element_blank())
#prescription opioid graph
prescription_time <- ggplot(Numeric_Prescription, aes( y = Numeric_Prescription$`VDH Health Region Case Count Display`, x = Numeric_Prescription$`VDH Health Region`, fill= Numeric_Prescription$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_grid(col = vars(Numeric_Prescription$Year)) + labs(fill = "VDH Health Region", title = "Deaths Due to Prescription Opioid Overdose by VDH Health Region (2011-2017)", x= "VDH Health Region", y= "Deaths Due to Prescription Opioid OD") +  theme(axis.text.x = element_blank())
#fentanyl and/or heroin OD
fent_time <- ggplot(Numeric_Fent, aes( y = Numeric_Fent$`VDH Health Region Case Count Display`, x = Numeric_Fent$`VDH Health Region`, fill= Numeric_Fent$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_grid(col = vars(Numeric_Fent$Year)) + labs(fill = "VDH Health Region", title = "Deaths Due to Fentanyl and/or Heroin Overdose by VDH Health Region (2011-2017)", x= "VDH Health Region", y= "Deaths Due to Fentanyl and/or Opioid OD") +  theme(axis.text.x = element_blank())
#other opioids graph
opioid_time <- ggplot(Numeric_Opioid, aes( y = Numeric_Opioid$`VDH Health Region Case Count Display`, x = Numeric_Opioid$`VDH Health Region`, fill= Numeric_Opioid$`VDH Health Region`)) + geom_bar(stat = "identity") + facet_grid(col = vars(Numeric_Opioid$Year)) + labs(fill = "VDH Health Region", title = "Deaths Due to Overdose on Other Opioids by VDH Health Region (2011-2017)", x= "VDH Health Region", y= "Deaths Due to Other Opioid OD") +  theme(axis.text.x = element_blank())
#HHPR Case Rate v Community Service Board Rate (log transform)
ggplot(data = Data, aes(x = Data$`Virginia Hospital & Healthcare Preparedness Region Case Rate`, y= Data$`Community Service Board Case Rate`, color = Data$Year)) + geom_point() + labs(x = "Hospital and Health Preparedness Region Case Rate", y= "Community Service Board Case Rate", title = "The Relationship Between Case Rates for Health Preparedness Region and Community Service Board ", color = "Year")

# --------------------------
