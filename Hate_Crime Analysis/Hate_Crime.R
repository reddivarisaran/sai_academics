#Setup
library(tidyverse)
library(micromapST)
library(ggplot2)
library(fiftystater)
library(mapproj)
library(car)   # vif() and qqPlot functions
library(splines)
library(corrgram)
library(corrplot)
library(Hmisc)
library(iplots)
library(ISLR)
library(lattice)
library(hexbin)
library(ape)
library(randomForest)
library(heuristica)

#reading csv data
hcrime<-read.csv("hate_crimes.csv")
prevnames<-colnames(hcrime)
colnames(hcrime)<-c("State","Income","Unemployed","Metro Population","HS Population","Non Citizen","White Poverty","Gini Index","Non White","Trump Voters","Hate Crimes","Avg Hate Crimes")

#data imputation
hcrime$`Non Citizen`[which(is.na(hcrime$`Non Citizen`))]=0
hcrime$`Avg Hate Crimes`[which(is.na(hcrime$`Avg Hate Crimes`))]=0
hcrime$`Hate Crimes`[which(is.na(hcrime$`Hate Crimes`))]=0

#choropleth maps
data("fifty_states")
hcrime$State<-tolower(hcrime$State)

#map for avg hate crimes
ccmap1<-ggplot(hcrime, aes(map_id = State)) + 
  geom_map(aes(fill = hcrime$`Hate Crimes`), map = fifty_states,boundary) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_gradient(low="white",high="red")+
  labs(x = "", y = "",fill="Hate Crimes per 100k in 2016") +
  theme(legend.position = "bottom",panel.background = element_blank())

#map for Income
ccmap2<-ggplot(hcrime, aes(map_id = State)) + 
  geom_map(aes(fill = hcrime$Income), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_gradient(low="white",high="darkgreen")+
  labs(x = "", y = "",fill="Median Income") +
  theme(legend.position = "right",panel.background = element_blank())

#map for high school graduates
ccmap3<-ggplot(hcrime, aes(map_id = State)) + 
  geom_map(aes(fill = hcrime$`HS Population`), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_gradient(low="white",high="blue")+
  labs(x = "", y = "",fill="High School\nGraduate Population") +
  theme(legend.position = "right",panel.background = element_blank())

#map for Gini Index
ccmap4<-ggplot(hcrime, aes(map_id = State)) + 
  geom_map(aes(fill = hcrime$`Gini Index`), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_gradient(low="white",high="green")+
  labs(x = "", y = "",fill="Gini Index") +
  theme(legend.position = "right",panel.background = element_blank())

#map for Non citizen
ccmap5<-ggplot(hcrime, aes(map_id = State)) + 
  geom_map(aes(fill = hcrime$`Non Citizen`), map = fifty_states) + 
  expand_limits(x = fifty_states$long, y = fifty_states$lat) +
  coord_map() +
  scale_x_continuous(breaks = NULL) + 
  scale_y_continuous(breaks = NULL) +
  scale_fill_gradient(low="white",high="black")+
  labs(x = "", y = "",fill="Non Citizen") +
  theme(legend.position = "right",panel.background = element_blank())


#Correlation
corrgram(hcrime,order=T,text.panel=panel.txt,diag.panel=panel.minmax)
res<-cor(hcrime[,2:12], use="complete.obs", method="pearson")
corr<-corrplot(res, type = "full", order = "hclust", 
         tl.col = "black", tl.srt = 45)
corr2<-corrplot(res,order = "hclust",add = TRUE, # add the above plot
                type = "lower", method = "number",number.font = 2,
                number.cex = .75,col = "black",
                diag = FALSE,tl.pos = "n", cl.pos = "n")

#Micromap
panelDesc <- data.frame(
  type=c('mapcum','id','dot','dot'),
  lab1=rep("",4),
  lab2=c('' ,'','Hate Crimes per 100k population\nin November 2016','Average annual hate crimes\nper 100k population\n during 2010-2015 '),
  col1 = c(NA,NA,'Hate Crimes','Avg Hate Crimes')
)
t(panelDesc)
fName = "micromapdot.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(hcrime, panelDesc,
           rowNamesCol='State',
           rowNames='full',
           sortVar='Hate Crimes',ascend=FALSE,
           title=c(" Average Hate Crimes in 2010-2015 Vs Hate Crimes in 2016"),
           ignoreNoMatches=TRUE)

dev.off()

hcrime$med<-median(hcrime$`Avg Hate Crimes`)
med<-median(hcrime$`Avg Hate Crimes`)
arrowPanelDesc <- data.frame(
  type=c('mapcum','id','arrow','arrow','arrow','arrow'),
  lab1=c('','','Average Hate\nCrimes per 100k\nPopulation','Median Household\nIncome($)',
         '%Share of\npopulation with\nHigh School Degree',
         '%Share of\npopulation who are\nNon US Citizens'),
  col1 = c(NA,NA,'med','Income','HS Population','Non Citizen'),
  col2 = c(NA,NA,'Avg Hate Crimes','Income','HS Population','Non Citizen'),
  refVals=c(NA,NA,med,NA,NA,NA))

fName = "MicromapArrows.pdf"
pdf(file=fName,width=7.5,height=10)
micromapST(hcrime,arrowPanelDesc,
           rowNamesCol='State',
           rowNames='full',
           sortVar='Avg Hate Crimes',ascend=FALSE,
           title=c("Average Annual Hate Crimes 2010-2015"),
           ignoreNoMatches=TRUE)

dev.off()

#Bar plot
plot1<-ggplot(data=hcrime, aes(x = reorder(hcrime$State,hcrime$`Avg Hate Crimes`), y = hcrime$`Avg Hate Crimes`,fill=hcrime$`Avg Hate Crimes`)) +
  geom_bar(stat="identity",show.legend = F)+
  scale_fill_gradient(low="yellow",high="red")+
  ggtitle("Average Annual Hate crimes during 2010-2015")+
  labs(x="State",y="Average Hate Crimes per 100k population")+
  coord_flip()

#Histograms
plot2<-hist(hcrime$`Metro Population`,breaks=12, col="red",ylim=c(0,10),xlab="% Share of population in Metro Areas",main="")
plot3<-hist(hcrime$Unemployed,breaks=12, col="red",xlab="% Share of population that is Unemployed",main="")
plot4<-hist(hcrime$Income,breaks=12, col="red",xlab="Median Household Income($)",main="")

#DotPlots
plot5<-ggplot(data=hcrime, aes(x = reorder(hcrime$State,hcrime$Income), y = hcrime$Income)) +
  geom_point()+
  ggtitle("Median Household Income per State")+
  labs(x="State",y="Median Household Income($)")+
  coord_flip()
plot6<-ggplot(data=hcrime, aes(x = reorder(hcrime$State,hcrime$`Gini Index`), y = hcrime$`Gini Index`)) +
  geom_point()+
  ggtitle("Gini Index per State")+
  labs(x="State",y="Gini Index")+
  coord_flip()



#Density plots
plot7<-plot(density(hcrime$`Non Citizen`), main="Kernel Density of % share of Non US Citizen",xlab="% share of Non US Citizen")
polygon(density(hcrime$`Non Citizen`), col="lightblue", border="blue")

plot8<-plot(density(hcrime$`White Poverty`), main="Kernel Density of % share of\nWhite residents living in Poverty",xlab="% share of White residents living in Poverty")
polygon(density(hcrime$`White Poverty`), col="lightblue", border="blue")

plot9<-hist(hcrime$`Trump Voters`,prob=T,col="lightblue",main="Histrogram with Normal Curve",xlab="% Share of voters who voted Trump",xlim = c(0.2,0.8),ylim=c(0,5))
curve(dnorm(x, mean(hcrime$`Trump Voters`), sd(hcrime$`Trump Voters`)), col="red", lwd=2, add=TRUE)

#Linear Regression plots
plot10<-ggplot(hcrime,aes(x = hcrime$Income,y = hcrime$`Avg Hate Crimes`)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "lm",color = "blue",fill = "cyan") +
  labs(x = "Median Household Income($)",y = "Average Annual Hate Crimes")
    
plot11<-ggplot(hcrime,aes(x = hcrime$`HS Population`,y = hcrime$`Avg Hate Crimes`)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "lm",color = "blue",fill = "cyan") +
  labs(x = "%Share of population\nwith High School degree",y = "Average Annual Hate Crimes")

plot12<-ggplot(hcrime,aes(x = hcrime$`Non Citizen`,y = hcrime$`Avg Hate Crimes`)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "lm",color = "blue",fill = "cyan") +
  labs(x = "%Share of population\nwho are Non US citizens",y = "Average Annual Hate Crimes")

#Loess Regression plots
plot13<-ggplot(hcrime,aes(x = hcrime$Income,y = hcrime$`Avg Hate Crimes`)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "loess",color = "blue",fill = "cyan") +
  labs(x = "Median Household Income($)",y = "Average Annual Hate Crimes")

plot14<-ggplot(hcrime,aes(x = hcrime$`HS Population`,y = hcrime$`Avg Hate Crimes`)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "loess",color = "blue",fill = "cyan") +
  labs(x = "%Share of population\nwith High School degree",y = "Average Annual Hate Crimes")

plot15<-ggplot(hcrime,aes(x = hcrime$`Non Citizen`,y = hcrime$`Avg Hate Crimes`)) +
  geom_point(shape = 21,fill = "red",
             color = "black",size = 2) +
  stat_smooth(method = "loess",color = "blue",fill = "cyan") +
  labs(x = "%Share of population\nwho are Non US citizens",y = "Average Annual Hate Crimes")

#Linear Regreesion Model Diagnostic Plots
lmfit1<-lm(hcrime$`Avg Hate Crimes`~hcrime$Income,data=hcrime)
plot(lmfit1)

lmfit2<-lm(hcrime$`Avg Hate Crimes`~hcrime$`HS Population`,data=hcrime)
plot(lmfit2)
summary(lmfit2)

lmfit3<-lm(hcrime$`Avg Hate Crimes`~hcrime$`Non Citizen`,data=hcrime)
plot(lmfit3)

#Scatterplot Matrix
splot1<-pairs(~., data=hcrime[,-c(1,13)])

panel.cor <- function(x, y, digits = 2, cex.cor, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  # correlation coefficient
  r <- cor(x, y)
  txt <- format(c(r, 0.123456789), digits = digits)[1]
  txt <- paste("r= ", txt, sep = "")
  text(0.5, 0.6, txt)
  
  # p-value calculation
  p <- cor.test(x, y)$p.value
  txt2 <- format(c(p, 0.123456789), digits = digits)[1]
  txt2 <- paste("p= ", txt2, sep = "")
  text(0.5, 0.3, txt2)
}
splot2<-pairs(~., data=hcrime[,-c(1,13)],
      lower.panel=panel.smooth, upper.panel=panel.cor,cex.labels=1, 
      pch=20, main=" Scatterplot Matrix")

#Dendogram
dd <- dist(scale(hcrime[,-1,13]), method = "euclidean")
hc <- hclust(dd, method = "ward.D2")
plot(hc, hang = -1, cex = 0.6,labels=hcrime$State[hc$order],xlab="State")

#Random Forest Regression Model
rf1<-randomForest(hcrime$`Avg Hate Crimes`~hcrime$Income,hcrime)
plot(rf1,main="Random Forest Regression for Income")

rf2<-randomForest(hcrime$`Avg Hate Crimes`~hcrime$`HS Population`,hcrime)
plot(rf2,main="Random Forest Regression for HS population")
summary(rf2)

rf3<-randomForest(hcrime$`Avg Hate Crimes`~hcrime$`Non Citizen`,hcrime)
plot(rf3,main="Random Forest Regression for Non US citizen")

#writing a new csv
write.csv(hcrime,"final_data.csv")