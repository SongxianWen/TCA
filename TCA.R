#load packages
library(tidyverse)
library(fs)
library(readxl)
library(ggplot2)
library(lubridate)
library(dplyr)

setwd("~/Test")
data = read_excel("EQ061.xlsx")
head(data)

#Setting time intervals
date1 <- as.POSIXct("2011-05-01")
date2 <- as.POSIXct("2012-05-01")
date3 <- as.POSIXct("2013-05-01")
date4 <- as.POSIXct("2014-05-01")
date5 <- as.POSIXct("2015-05-01")
date6 <- as.POSIXct("2016-05-01")
date7 <- as.POSIXct("2017-05-01")
date8 <- as.POSIXct("2018-05-01")
date9 <- as.POSIXct("2019-05-01")
date0 <- as.POSIXct("2020-05-01")
datex <- as.POSIXct("2021-05-01")

int1 <- interval(date1, date2)
int2 <- interval(date2, date3)
int3 <- interval(date3, date4)
int4 <- interval(date4, date5)
int5 <- interval(date5, date6)
int6 <- interval(date6, date7)
int7 <- interval(date7, date8)
int8 <- interval(date8, date9)
int9 <- interval(date9, date0)
int0 <- interval(date0, datex)



#Total Employment in Tech Sector

yr2012 = sum(data[data$Date %within% int1,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int1,]$`Employed part-time ('000)`)

yr2013 = sum(data[data$Date %within% int2,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int2,]$`Employed part-time ('000)`)

yr2014 = sum(data[data$Date %within% int3,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int3,]$`Employed part-time ('000)`)

yr2015 = sum(data[data$Date %within% int4,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int4,]$`Employed part-time ('000)`)

yr2016 = sum(data[data$Date %within% int5,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int5,]$`Employed part-time ('000)`)

yr2017 = sum(data[data$Date %within% int6,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int6,]$`Employed part-time ('000)`)

yr2018 = sum(data[data$Date %within% int7,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int7,]$`Employed part-time ('000)`)

yr2019 = sum(data[data$Date %within% int8,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int8,]$`Employed part-time ('000)`)

yr2020 = sum(data[data$Date %within% int9,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int9,]$`Employed part-time ('000)`)

yr2021 = sum(data[data$Date %within% int0,]$`Employed full-time ('000)`)+
  sum(data[data$Date %within% int0,]$`Employed part-time ('000)`)


# Plotting Graph
year = rep(2012:2021)
Total_Employment_in_thousands = c(yr2012,yr2013,yr2014,yr2015,yr2016,yr2017,yr2018,yr2019,yr2020,yr2021)
df = data.frame(year = year, Total_Employment_in_thousands = Total_Employment_in_thousands)

p = ggplot(df,aes(x = year, y = Total_Employment_in_thousands,  group = factor(1)))+
  geom_point(size = 3.8)+
  geom_line(size = 0.8)+
  labs(x = "Year",y = "Total_Employment_in_thousands",title = "Total Employment in Tech Sector from 2012 to 2021")+
  theme_bw()+
  geom_text(aes(label = Total_Employment_in_thousands),show.legend = FALSE)+
  theme(panel.grid = element_blank())
p+scale_x_continuous(breaks = seq(2012,2021,1))







#592 Data Processing, Web Hosting and Electronic Information Storage Services

Data_Processing = subset(data,data$`Industry group of main job` == "592 Data Processing, Web Hosting and Electronic Information Storage Services")
head(Data_Processing)

a = sum(Data_Processing[Data_Processing$Date %within% int1,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int1,]$`Employed part-time ('000)`)

b = sum(Data_Processing[Data_Processing$Date %within% int2,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int2,]$`Employed part-time ('000)`)

c = sum(Data_Processing[Data_Processing$Date %within% int3,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int3,]$`Employed part-time ('000)`)

d = sum(Data_Processing[Data_Processing$Date %within% int4,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int4,]$`Employed part-time ('000)`)

e = sum(Data_Processing[Data_Processing$Date %within% int5,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int5,]$`Employed part-time ('000)`)

f = sum(Data_Processing[Data_Processing$Date %within% int6,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int6,]$`Employed part-time ('000)`)

g = sum(Data_Processing[Data_Processing$Date %within% int7,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int7,]$`Employed part-time ('000)`)

h = sum(Data_Processing[Data_Processing$Date %within% int8,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int8,]$`Employed part-time ('000)`)

i = sum(Data_Processing[Data_Processing$Date %within% int9,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int9,]$`Employed part-time ('000)`)

j = sum(Data_Processing[Data_Processing$Date %within% int0,]$`Employed full-time ('000)`)+
  sum(Data_Processing[Data_Processing$Date %within% int0,]$`Employed part-time ('000)`)


year = rep(2012:2021)
Data_Employment_thousands = c(a,b,c,d,e,f,g,h,i,j)
df <- data.frame(year = year, Data_Employment_thousands = Data_Employment_thousands)

p = ggplot(df,aes(x = year, y = Data_Employment_thousands,  group = factor(1)))+
  geom_point(size = 3.8)+
  geom_line(size = 0.8)+
  labs(x = "Year",y = "Data_Employment_thousands",title = "Total Employment in 
       Data Processing, Web Hosting and Electronic Information Storage Services from 2012 to 2021")+
  theme_bw()+
  geom_text(aes(label = Data_Employment_thousands),show.legend = FALSE)+
  theme(panel.grid = element_blank())
p+scale_x_continuous(breaks = seq(2012,2021,1))





#580 Telecommunications Services
Telecom = subset(data,data$`Industry group of main job` == "580 Telecommunications Services")
head(Telecom)

l = sum(Telecom[Telecom$Date %within% int1,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int1,]$`Employed part-time ('000)`)

m = sum(Telecom[Telecom$Date %within% int2,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int2,]$`Employed part-time ('000)`)

n = sum(Telecom[Telecom$Date %within% int3,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int3,]$`Employed part-time ('000)`)

o = sum(Telecom[Telecom$Date %within% int4,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int4,]$`Employed part-time ('000)`)

p = sum(Telecom[Telecom$Date %within% int5,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int5,]$`Employed part-time ('000)`)

q = sum(Telecom[Telecom$Date %within% int6,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int6,]$`Employed part-time ('000)`)

r = sum(Telecom[Telecom$Date %within% int7,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int7,]$`Employed part-time ('000)`)

s = sum(Telecom[Telecom$Date %within% int8,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int8,]$`Employed part-time ('000)`)

t = sum(Telecom[Telecom$Date %within% int9,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int9,]$`Employed part-time ('000)`)

u = sum(Telecom[Telecom$Date %within% int0,]$`Employed full-time ('000)`)+
  sum(Telecom[Telecom$Date %within% int0,]$`Employed part-time ('000)`)



year = rep(2012:2021)
Tele_Employment_thousands = c(l,m,n,o,p,q,r,s,t,u)
df <- data.frame(year = year, Tele_Employment_thousands = Tele_Employment_thousands)

p = ggplot(df,aes(x = year, y = Tele_Employment_thousands,  group = factor(1)))+
  geom_point(size = 3.8)+
  geom_line(size = 0.8)+
  labs(x = "Year",y = "Tele_Employment_thousands",title = "Total Employment in Telecommunications Services from 2012 to 2021")+
  theme_bw()+
  geom_text(aes(label = Tele_Employment_thousands),show.legend = FALSE)+
  theme(panel.grid = element_blank())
p+scale_x_continuous(breaks = seq(2012,2021,1))

