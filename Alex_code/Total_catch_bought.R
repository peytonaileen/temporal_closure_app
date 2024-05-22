######
###Tahuhu 2020
#####
library(scales)
library(tidyverse)
library(dplyr)
library(janitor)
library(ggplot2)
library(ggridges)
library(gridExtra)
library(fishmethods)
library(gdata)
library(lunar)
library(lubridate)
library(here)

###
#read and clean data
#####

## r version 3.5.3

rm(list=ls())
#setwd('C:\\Users\\User\\Dropbox\\Documents\\TNC French Polynisha\\Rcodes')

data<-read.csv(here("Alex_code", "tahuhu2020_catch.csv"))
data$Month<-as.factor(data$Month)
d<-data %>% group_by(Month) %>% summarise(total = sum(Catch), n = n())

d$Month <- factor(d$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November","December"))

ggplot2::ggplot(d, aes(Month, total))+geom_bar(stat="identity", fill="black")+theme_classic()+
  theme(text=element_text(size=18))+theme(axis.text=element_text(colour="black", size=10))+
  xlab("Month")+ylab("Total lethrinus olivaceus purchased")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))




ggplot(d, aes(Month, total))+geom_bar(stat="identity", fill="black")+theme_classic()+
  theme(text=element_text(size=18))+theme(axis.text=element_text(colour="black", size=10))+
  xlab("Month")+ylab("Total lethrinus olivaceus purchased")+ theme(axis.text.x = element_text(angle = 70))+coord_polar()

#lunar
data$Date<-mdy(data$Date)
data$illum<-lunar.illumination(data$Date)

#4 or 8 phases
data$phase4<-lunar.phase(data$Date, name=T)
data$phase8<-lunar.phase(data$Date, name=8)

# season
data$Season<-terrestrial.season(data$Date)


require(plyr)
data$Season<-revalue(data$Season, c("Winter"="Hot", "Summer"="Cool", "Autumn"="Hot", "Spring"="Hot"))
detach(package:plyr)

data %>% group_by(Season) %>% summarise(total = sum(Catch), n = n())


# filtering out outlirs
#summary(mnb <- glm.nb(Catch~Season, data = data))
#summary(glht(mnb, linfct = mcp(phase4 = "Tukey")))

lunartab<-data%>%group_by(phase4)%>%summarise(Min=min(Catch), Max=max(Catch), total=sum(Catch), mean=mean(Catch), sd=sd(Catch))
write.csv(lunartab, "lunartab.csv")
sum(data$Catch)

16094/sum(data$Catch)



# negative binomial

# filtering out outlirs
summary(mnb <- glm.nb(Catch~phase4, data = data))
summary(glht(mnb, linfct = mcp(phase4 = "Tukey")))

lunartab<-data%>%group_by(phase4)%>%summarise(Min=min(Catch), Max=max(Catch), total=sum(Catch), mean=mean(Catch), sd=sd(Catch))
write.csv(lunartab, "lunartab.csv")
sum(data$Catch)

16094/sum(data$Catch)

##taggging plot
#jpeg(filename="Figure_LEOL_moon.jpg", width = 3400, height = 3400, res=300)
ggplot(data, aes(round(illum, 2), Catch, fill=phase4))+geom_bar(stat="identity")+coord_polar()+ylab("Longnose emperor landed")+xlab("Lunar illumination")+theme_classic()+theme_classic()+theme(text=element_text(size=22))+
  theme(axis.text=element_text(colour="black"))+
  scale_fill_manual("legend", values = c("New" = "orange", "Waxing" = "grey", "Full" = "yellow", "Waning" = "blue"))+scale_x_continuous(breaks=scales::pretty_breaks(n=6))
#dev.off()




### Tahuhu data 2

Tdata<-read.csv("Tahuhu_2020_LEOL Catches.csv")

Td<-Tdata %>% gather(key = "Gear", value = "n", 4:6)
Td<-Td %>% group_by(Year, Month, Gear) %>% summarise(total = sum(n))
Td<-na.omit(Td)

Td$Month <- factor(Td$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November","December"))

#jpeg(filename="Figure_tahuhu_gear.jpg", width = 3400, height = 2400, res=300)
t<-ggplot(Td, aes(x = Month, y = total))+
  geom_col(aes(fill = Gear), width = 0.7)+ scale_fill_manual(values = c("red", "blue", "grey"))+
  theme_classic()+theme(text=element_text(size=25))+theme(axis.text=element_text(colour="black"))+
  xlab("Month")+ylab("Total LEOL purchased")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))
#dev.off()

Tahuhu_monthly2020_total <-Td %>% group_by(Year, Month) %>% summarise(total = sum(total))
Tahuhu_monthly2020 <-Tahuhu_monthly2020_total %>% group_by(Year) %>% summarise(mean = mean(total), sd = sd(total))
Tahuhu_2020_total <-Td %>% group_by(Year) %>% summarise(total = sum(total))

Td %>% group_by(Gear) %>% summarise(total = sum(total))

perhl = 11503/(11503+7624+910)
perT = 7624/(11503+7624+910)
perunk = 910/(11503+7624+910)
###
#Patrick Changus data
###

PC_data<-read.csv("PC_catch_2018-2020.csv")
PC_data$Month <- factor(PC_data$Month, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))
PC<-na.omit(PC_data) %>% group_by(Month) %>% summarise(Average = mean(Catch), n = n(), sd = sd(Catch))
#PC$Month <- factor(PC$Month, levels = c("January","February","March","April","May","June","July","August","September","October","November","December"))

jpeg(filename="Figure_PC_mean.jpg", width = 4400, height = 3400, res=300)
ggplot(PC, aes(Month, Average))+geom_bar(stat="identity", fill="black")+theme_classic()+
  theme(text=element_text(size=24))+theme(axis.text=element_text(colour="black", size=10))+
  xlab("Month")+ylab("Total lethrinus olivaceus purchased")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))+
  geom_errorbar(aes(ymin=Average, ymax=Average+sd), width=.2,
                position=position_dodge(.9)) 
dev.off()

#jpeg(filename="Figure_PC.jpg", width = 3400, height = 1400, res=300)
p<-ggplot(PC_data, aes(Month, Catch))+geom_bar(stat="identity", fill="black")+theme_classic()+
  theme(text=element_text(size=18))+theme(axis.text=element_text(colour="black", size=10))+
  xlab("Month")+ylab("Total LEOL purchased")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))+facet_wrap(~Year)+ ggtitle("Landing faciltiy B")
#dev.off()


#final plot
p<-ggplot(PC_data, aes(Month, Catch))+geom_bar(stat="identity", fill="black")+theme_classic()+
  theme(text=element_text(size=18))+theme(axis.text=element_text(colour="black", size=10))+
  xlab("Month")+ylab("Total landed")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))+facet_wrap(~Year)+ ggtitle("Faciltiy B")

t<-ggplot(Td, aes(x = Month, y = total))+
  geom_col(aes(fill = Gear), width = 0.7)+ scale_fill_manual(values = c("red", "blue", "grey"))+
  theme_classic()+theme(text=element_text(size=18))+theme(axis.text=element_text(colour="black", size = 10))+
  xlab("")+ylab("Total landed")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))+ ggtitle("Faciltiy A")




library(gridExtra)

jpeg(filename="Figure_LEOL_landed.jpg", width = 2500, height = 3000, res=300)
grid.arrange(t,p)
dev.off()

sum(na.omit(PC_data$Catch))

PC_2018to2020<-na.omit(PC_data) %>% group_by(Year) %>% summarise(Average = mean(Catch),Total=sum(Catch), n = n(), sd = sd(Catch))
PC_2018to2020<-PC_2018to2020[,c(1,2,5,3)]

Tahuhu_2020_total 
Tahuhu_monthly2020
T2020<-cbind(Tahuhu_monthly2020,Tahuhu_2020_total)
T2020<-T2020[,c(1,2,3,5)]

colnames(PC_2018to2020)[2]<-"mean"
colnames(PC_2018to2020)[4]<-"total"

total_catch<-rbind(PC_2018to2020,T2020)
total_catch$Buyer<-c("Buyer 1", "Buyer 1", "Buyer 1", "Buyer 2")
total_catch<-total_catch[,c(5,1,2,3,4)]

write.csv(total_catch, "total_catch_LEOL.csv")

total_catch %>% group_by(Buyer) %>% summarise(Average = mean(total), sd=sd(total))


p2020 <-PC_data %>% filter(Year==2020)
p2020$Month <- factor(p2020$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November","December"))

p<-with(p2020, p2020[order(Month, Buyer, Year, Catch),])

t2020 <-d
t2020$Month <- factor(t2020$Month, levels = c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October","November","December"))

t<-with(t2020, t2020[order(Month, total, n),])

dataF<-cbind(p,t)
dataF[is.na(dataF)] <- 0 
dataF$TBtotal<-dataF$Catch+dataF$total

dataF <- dataF[,c(2,3,8)]

jpeg(filename="Figure_2buyer.jpg", width = 3400, height = 2400, res=300)
ggplot(dataF, aes(Month, TBtotal))+geom_bar(stat="identity", fill="black")+theme_classic()+
  theme(text=element_text(size=26))+theme(axis.text=element_text(colour="black", size=10))+
  xlab("Month")+ylab("L. olivaceus landed")+ theme(axis.text.x = element_text(angle = 90,hjust=0.95,vjust=0.2))+facet_wrap(~Year)
dev.off()
