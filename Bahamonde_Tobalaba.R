############################## 
# Cleaning
##############################
cat("\014")
rm(list=ls())
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# setwd("/Users/hectorbahamonde/research/Tobalaba")

############################## 
# COVID
##############################


# Load Covid Data
p_load(rio, tidyverse)
covid.d = rio::import(file = 'https://github.com/hbahamonde/Datos-COVID19/raw/master/output/producto1/Covid-19_std.csv',which = 1)


covid.d <- covid.d[ which(covid.d$Region == "Metropolitana"), ]


# keep columns
p_load(dplyr)
covid.d = covid.d %>% dplyr::select("Casos confirmados", Comuna, Fecha,"Casos confirmados", "Codigo comuna")

# Change column names
colnames(covid.d)[colnames(covid.d)=="Fecha"] <- "Date"
colnames(covid.d)[colnames(covid.d)=="Casos confirmados"] <- "Covid"
colnames(covid.d)[colnames(covid.d)=="Codigo comuna"] <- "mun.cod"

# format vars
covid.d$Date = as.Date(covid.d$Date)

# drop NA's in comuna
covid.d <- na.omit(covid.d)


############################## 
# AIRPORT
##############################
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# Load the airport data

# import dataset
p_load(rio, tidyverse)
airport = rio::import(file = 'https://github.com/hbahamonde/Tobalaba/raw/main/airport.xls',which = 1)

# colnames
colnames(airport)[colnames(airport)=="OPERACIÓN"] <- "operation"
colnames(airport)[colnames(airport)=="Matrícula"] <- "plate"
colnames(airport)[colnames(airport)=="AERONAVE"] <- "aircraft"
colnames(airport)[colnames(airport)=="FECHA"] <- "Date"
colnames(airport)[colnames(airport)=="Hora Local"] <- "time"
colnames(airport)[colnames(airport)=="Origen/Destino"] <- "place"
colnames(airport)[colnames(airport)=="Latitud"] <- "lat"
colnames(airport)[colnames(airport)=="Longitud"] <- "long"
colnames(airport)[colnames(airport)=="Código Comuna"] <- "mun.cod"

# drop NA
p_load(DataCombine)
airport = DropNA(airport, Var = c("lat", "long", "mun.cod"), message = F)

# format vars
airport$date = as.Date(airport$date)

# p_load(sp)
#
#test = airport$lat %>%
#  sub('º', "d", .) %>%
#  sub("'", "'", .) %>% 
#  sub("''", "\''", .) 
#
# as.numeric(sp::char2dms("33d 40' 36'' S", chd = "d", chm = "'", chs = "\''"))

# function to convert to long and lat
## https://stackoverflow.com/a/58646642/6079926
angle2dec <- function(angle) {
  angle <- as.character(angle)
  angle <- ifelse(grepl("S|W", angle), paste0("-", angle), angle)
  angle <- trimws(gsub("[^- +.0-9]", "", angle))
  x <- do.call(rbind, strsplit(angle, split=' '))
  x <- apply(x, 1L, function(y) {
    y <- as.numeric(y)
    (abs(y[1]) + y[2]/60 + y[3]/3600) * sign(y[1])
  })
  return(x)
}

airport$lat.2 = data.frame(unlist(t(data.frame(lapply(airport$lat, angle2dec)))))[,1]
airport$long.2 = data.frame(unlist(t(data.frame(lapply(airport$long, angle2dec)))))[,1]

# jitter
airport$lat.2 = jitter(airport$lat.2, 7)
airport$long.2 = jitter(airport$long.2, 7)



# Merge with Covid Data
dat = merge(airport,covid.d, by.y = c("Date","mun.cod"), all=T)
p_load(dplyr)
dat = dat %>% dplyr::select(-c("Pasaporte Sanitario", "lat", "long", "Nota", "time"))
colnames(dat)[colnames(dat)=="lat.2"] <- "Latitude"
colnames(dat)[colnames(dat)=="long.2"] <- "Longitude"



############################## 
# Fase base (paso)
##############################

# Load Fase Data
p_load(rio, tidyverse)
paso.d = rio::import(file = 'https://raw.githubusercontent.com/hbahamonde/Datos-COVID19/master/output/producto74/paso_a_paso_std.csv',which = 1)


# format vars
paso.d$Fecha = as.Date(paso.d$Fecha)

# Select columns
p_load(dplyr)
paso.d = paso.d %>% dplyr::select(c("codigo_comuna", "Paso", "Fecha", "zona"))
colnames(paso.d)[colnames(paso.d)=="Fecha"] <- "Date"
colnames(paso.d)[colnames(paso.d)=="codigo_comuna"] <- "mun.cod"


############################## 
# Confinamiento base
##############################

# Before "Paso a Paso" there was "Cuarentena" (total lockdown) which equals Paso == 1 in "Paso a Paso."
# https://es.wikipedia.org/wiki/Confinamiento_por_la_pandemia_de_COVID-19_en_Chile





confinamiento.Date = data.frame(Date = seq(as.Date("2020/1/1"), as.Date(min(paso.d$Date))-1, "days"))
confinamiento.mun.cod = data.frame(mun.cod = unique(covid.d$mun.cod))
confinamiento = merge(confinamiento.Date,confinamiento.mun.cod,all=T)

# p_load("openxlsx")
# write.xlsx(confinamiento, "/Users/hectorbahamonde/research/Tobalaba/confinamiento.xlsx")

data.frame(unique(data.frame(data.table(covid.d$Comuna, covid.d$mun.cod))))


#### DESDE AQUI

d1 = confinamiento %>% # Independencia
  filter(mun.cod == 13108) %>%
  filter(Date >= "2020-03-27" & Date <= "2020-04-03" | # 27 de marzo de 2020-3 de abril de 2020
         Date >= "2020-04-24" , Date <= "2020-09-21" # 24 de abril de 2020-21 de septiembre de 2020
         ) %>% 
  mutate(Paso = 1)

d2 = confinamiento %>% # Las Condes
  filter(mun.cod == 13114) %>%
  filter(Date >= "2020-03-27" & Date <= "2020-04-17" | # 27 de marzo de 2020-17 de abril de 2020
           Date >= "2020-05-16" , Date <= "2020-07-28" # 16 de mayo de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d3 = confinamiento %>% # Lo Barnechea
  filter(mun.cod == 13115) %>%
  filter(Date >= "" & Date <= "" | # FECHA AQUI WIKIPEDIA
           Date >= "" , Date <= "" # FECHA AQUI WIKIPEDIA
  ) %>% 
  mutate(Paso = 1)




#### HASTA AQUI














# Merge with Covid and Airport Data
df = merge(dat,paso.d, by.y = c("Date","mun.cod"), all=T)
#df = data.frame(na.omit(dat))
df$Paso = as.factor(df$Paso)


############################## 
# Toy map
##############################


## https://github.com/dkahle/ggmap
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap")
p_load("ggmap")


airport.centro = dat[ which(dat$Latitude <= -29 & dat$Latitude>= -36 & dat$Longitude >= -73 & dat$Longitude <= -69),]


qmplot(long.2, lat.2, 
       geom = "auto",
       zoom = 8, 
       data = airport.centro, 
       maptype = "toner-lite", 
       #darken = .4,
       alpha = I(0.2),
       color = I("red"),
       legend = "topleft",
       facets = NULL
)
























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
# save state file
p_load(foreign)
write.dta(dat, paste(getwd(),"dat.dta", sep = "/"))

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



#### Transportation Analyses

# https://spatial.blog.ryerson.ca/2019/09/03/transportation-flow-mapping-using-r/
# https://rstudio-pubs-static.s3.amazonaws.com/259095_2f8cb24b43284692a8af916bd447931d.html
# https://servicios.dgac.gob.cl/portal_consulta_aeronaves/
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
