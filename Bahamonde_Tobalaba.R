############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())
if (!require("pacman")) install.packages("pacman"); library(pacman) 


############################## 
# COVID
##############################


# Comunas
comunas = c("Colina",	"La Reina",	"Las Condes",	"Lo Barnechea",	"Providencia",	"Vitacura")

# Load Covid Data
p_load(rio, tidyverse)
covid.d = rio::import(file = 'https://github.com/hbahamonde/Datos-COVID19/raw/master/output/producto1/Covid-19_std.csv',which = 1)

covid.d <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[1,1] | 
                            covid.d$Comuna == data.frame(comunas)[2,1] | 
                            covid.d$Comuna == data.frame(comunas)[3,1] | 
                            covid.d$Comuna == data.frame(comunas)[4,1] | 
                            covid.d$Comuna == data.frame(comunas)[5,1] | 
                            covid.d$Comuna == data.frame(comunas)[6,1]), ]

# keep columns
p_load(dplyr)
covid.d = covid.d %>% select(Comuna, Fecha,`Casos confirmados`)

# Change column names
colnames(covid.d)[colnames(covid.d)=="Fecha"] <- "Date"
colnames(covid.d)[colnames(covid.d)=="Casos confirmados"] <- "Covid"

# Construct datasets for each municipality
covid.d.Colina <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[1,1]), ]; covid.d.Colina <- covid.d.Colina[,2:3]; colnames(covid.d.Colina)[colnames(covid.d.Colina)=="Covid"] <- "Covid.Colina";rownames(covid.d.Colina)<-NULL

covid.d.La.Reina <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[2,1]), ]; covid.d.La.Reina <- covid.d.La.Reina[,2:3]; colnames(covid.d.La.Reina)[colnames(covid.d.La.Reina)=="Covid"] <- "Covid.La.Reina";rownames(covid.d.La.Reina)<-NULL

covid.d.Las.Condes <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[3,1]), ]; covid.d.Las.Condes <- covid.d.Las.Condes[,2:3]; colnames(covid.d.Las.Condes)[colnames(covid.d.Las.Condes)=="Covid"] <- "Covid.Las.Condes";rownames(covid.d.Las.Condes)<-NULL

covid.d.Lo.Barnechea <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[4,1]), ]; covid.d.Lo.Barnechea <- covid.d.Lo.Barnechea[,2:3]; colnames(covid.d.Lo.Barnechea)[colnames(covid.d.Lo.Barnechea)=="Covid"] <- "Covid.Lo.Barnechea";rownames(covid.d.Lo.Barnechea)<-NULL

covid.d.Providencia <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[5,1]), ]; covid.d.Providencia <- covid.d.Providencia[,2:3]; colnames(covid.d.Providencia)[colnames(covid.d.Providencia)=="Covid"] <- "Covid.Providencia";rownames(covid.d.Providencia)<-NULL

covid.d.Vitacura <- covid.d[ which(covid.d$Comuna == data.frame(comunas)[6,1]), ]; covid.d.Vitacura <- covid.d.Vitacura[,2:3]; colnames(covid.d.Vitacura)[colnames(covid.d.Vitacura)=="Covid"] <- "Covid.Vitacura";rownames(covid.d.Vitacura)<-NULL

# merge
m1 = merge(covid.d.Colina,covid.d.La.Reina, by.x = "Date")
m2 = merge(covid.d.Las.Condes,covid.d.Lo.Barnechea, by.x = "Date")
m3 = merge(covid.d.Providencia,covid.d.Vitacura, by.x = "Date")
m12 = merge(m1,m2)
covid.d = merge(m12,m3)

# total covid ABC1
covid.d$Covid.tot = rowSums(covid.d[2]+covid.d[3]+covid.d[4]+covid.d[5]+covid.d[6]+covid.d[7])

# format vars
covid.d$Date = as.Date(covid.d$Date)

############################## 
# AIRPORT
##############################

# Load the airport data

# import dataset
p_load(rio, tidyverse)
dat = rio::import(file = 'https://github.com/hbahamonde/Tobalaba/raw/main/dataset.xlsx',which = 1)

# format vars
dat$Day = as.factor(dat$Day)
dat$Arrivals = as.numeric(dat$Arrivals)
dat$Departures = as.numeric(dat$Departures)
dat$Date = as.Date(dat$Date)

# difference
dat$Empty = dat$Departures - dat$Arrivals

# Merge with Covid Data
dat = merge(dat,covid.d, by.y = "Date", all=T)

# Multiple imputation for covid vector
p_load(imputeTS)
dat$Covid.tot.i = na_interpolation(dat$Covid.tot)
dat$Covid.tot.i = ifelse(dat$Date < "2020-03-30", 0, dat$Covid.tot.i) # delete prior info
dat$Covid.tot.i = round(dat$Covid.tot.i,0)

# Multiple imputation for Departures vector // TEMPORARY (remove after getting complete dataset)
p_load(imputeTS)
dat$Departures.i = na_interpolation(dat$Departures)
dat$Departures.i = round(dat$Departures.i,0)
# Variable "Open" (how open the rich parte of the city is)
# dat$Open = round(rowMeans(dat[,7:18], na.rm=TRUE),1)
# dat$Open[is.na(dat$Open)] <- 4
dat$Open = apply(dat[,7:18], 1, function(idx) which.max(tabulate(idx)))
dat$Open = ifelse(dat$Date < "2020-03-26", 4, dat$Open)

# Variable Weekend
dat$Weekend = ifelse(dat$Day=="Saturday" |  dat$Day=="Sunday" | dat$Day=="Friday" | dat$Holiday==1, 1, 0)


# Keep Weekend Data
# dat <- dat[ which(dat$Weekend==1),]


# Keep between dates
# dat = dat[ which(dat$Date<"2020-07-28"),] # Fase 1
 dat = dat[ which(dat$Date<"2020-09-14"),] # Fase 1 y 2
# Simple Plot
ggplot(dat, aes(x=Date, y=Departures)) +   geom_line(aes(color=as.factor(Open))) +  xlab("") + theme_bw() 

# RDD
dat$Cutpoint = ifelse(dat$Date<"2020-03-26",0,1) # lockdown begins
p_load(rddtools)
data <- rdd_data(dat$Departures, dat$Date, cutpoint = as.Date("2020-03-26"), covar = dat$Covid.tot.i)
data = na.omit(data)
rdd_mod <- rdd_reg_lm(rdd_object = data, slope = "same", covariates = TRUE)
rdd_mod



















# Empty (factor)
dat$Empty.f = as.factor(ifelse(dat$Empty<0, "Arrivals", "Departures"))

# Plot
weekend = data.frame(
  x.min=c(dat$Date[dat$Day=="Friday"]), 
  x.max=c(dat$Date[dat$Day=="Sunday"]), 
  y.min=c(rep(min(dat$Empty, na.rm = T), length(dat$Date[dat$Day=="Friday"]))), 
  y.max=c(rep(max(dat$Empty, na.rm = T), length(dat$Date[dat$Day=="Friday"]))))

phase.1 = data.frame(
  x.min=c(dat$Date[dat$Open==1]), 
  x.max=c(dat$Date[dat$Open==1]),
  y.min=c(rep(min(dat$Empty, na.rm = T), length(dat$Date[dat$Open==1]))), 
  y.max=c(rep(max(dat$Empty, na.rm = T), length(dat$Date[dat$Open==1]))))

phase.2 = data.frame(
  x.min=c(dat$Date[dat$Open==2]), 
  x.max=c(dat$Date[dat$Open==2]),
  y.min=c(rep(min(dat$Empty, na.rm = T), length(dat$Date[dat$Open==2]))), 
  y.max=c(rep(max(dat$Empty, na.rm = T), length(dat$Date[dat$Open==2]))))

phase.3 = data.frame(
  x.min=c(dat$Date[dat$Open==3]), 
  x.max=c(dat$Date[dat$Open==3]),
  y.min=c(rep(min(dat$Empty, na.rm = T), length(dat$Date[dat$Open==3]))), 
  y.max=c(rep(max(dat$Empty, na.rm = T), length(dat$Date[dat$Open==3]))))

phase.4 = data.frame(
  x.min=c(dat$Date[dat$Open==4]), 
  x.max=c(dat$Date[dat$Open==4]),
  y.min=c(rep(min(dat$Empty, na.rm = T), length(dat$Date[dat$Open==4]))), 
  y.max=c(rep(max(dat$Empty, na.rm = T), length(dat$Date[dat$Open==4]))))


p_load(ggplot2)
ggplot() + 
  #geom_rect(data=weekend, mapping=aes(xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max), color='grey', alpha=0.2) +
  geom_rect(data=phase.1, mapping=aes(xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max), color='red', alpha=0.00001) +
  geom_rect(data=phase.2, mapping=aes(xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max), color='orange', alpha=1) +
  geom_rect(data=phase.3, mapping=aes(xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max), color='yellow', alpha=0.00001) +
  geom_rect(data=phase.4, mapping=aes(xmin=x.min, xmax=x.max, ymin=y.min, ymax=y.max), color='blue', alpha=0.00001) +   geom_line(aes(x=Date, y=Empty), data=dat) + # color=Empty.f

  
  theme_bw() 
