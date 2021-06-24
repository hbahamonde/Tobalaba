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

covid.d = covid.d %>% dplyr::select(-c("Comuna"))

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
airport$Date = as.Date(airport$Date)

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
# airport$lat.2 = jitter(airport$lat.2, 20)
# airport$long.2 = jitter(airport$long.2, 20)



# Merge with Covid Data
dat = merge(airport,covid.d, by.y = c("Date","mun.cod"), all=T)
p_load(dplyr)
dat = dat %>% dplyr::select(-c("Pasaporte Sanitario", "lat", "long", "Nota", "time", "Covid"))
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
paso.d = paso.d %>% dplyr::select(c("codigo_comuna", "Paso", "Fecha"))
colnames(paso.d)[colnames(paso.d)=="Fecha"] <- "Date"
colnames(paso.d)[colnames(paso.d)=="codigo_comuna"] <- "mun.cod"

# delete duplicates
paso.d = paso.d[!duplicated(paso.d[c("Date", "mun.cod")]),]


############################## 
# Confinamiento base
##############################

# Before "Paso a Paso" there was "Cuarentena" (total lockdown) which equals Paso == 1 in "Paso a Paso."
# https://es.wikipedia.org/wiki/Confinamiento_por_la_pandemia_de_COVID-19_en_Chile

confinamiento.Date = data.frame(Date = seq(as.Date("2020/1/1"), as.Date(min(paso.d$Date))-1, "days"))
confinamiento.mun.cod = data.frame(mun.cod = unique(covid.d$mun.cod))
confinamiento = merge(confinamiento.Date,confinamiento.mun.cod,all=T)

#### Collect data prior to Paso a Paso (plan "Confinamiento").
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
  filter(Date >= "2020-03-26" & Date <= "2020-04-13" | # 26 de marzo de 2020-13 de abril de 2020
           Date >= "2020-05-15" , Date <= "2020-07-28" # 15 de mayo de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d4 = confinamiento %>% # Vitacura
  filter(mun.cod == 13132) %>%
  filter(Date >= "2020-03-26" & Date <= "2020-04-13" | # 26 de marzo de 2020-13 de abril de 2020
           Date >= "2020-05-15" , Date <= "2020-07-28" # 15 de mayo de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d5 = confinamiento %>% # Nunoa
  filter(mun.cod == 13120) %>%
  filter(Date >= "2020-03-26" & Date <= "2020-05-07" | # 26 de marzo de 2020-7 de mayo de 2020
           Date >= "2020-05-15" , Date <= "2020-07-28" # 15 de mayo de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d6 = confinamiento %>% # Providencia
  filter(mun.cod == 13123) %>%
  filter(Date >= "2020-03-26" & Date <= "2020-04-13" | # 26 de marzo de 2020-13 de abril de 2020
           Date >= "2020-05-15" , Date <= "2020-08-09" # 15 de mayo de 2020-9 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d7 = confinamiento %>% # Santiago
  filter(mun.cod == 13101) %>%
  filter(Date >= "2020-03-26" & Date <= "2020-04-13" # 26 de marzo de 2020-17 de agosto de 2020 
  ) %>% 
  mutate(Paso = 1)

d8 = confinamiento %>% # Puente Alto
  filter(mun.cod == 13201) %>%
  filter(Date >= "2020-04-09" & Date <= "2020-09-28" # 9 de abril de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d9 = confinamiento %>% # El Bosque
  filter(mun.cod == 13105) %>%
  filter(Date >= "2020-04-16" & Date <= "2020-09-21" # 16 de abril de 2020-21 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d10 = confinamiento %>% # San Bernardo
  filter(mun.cod == 13401) %>%
  filter(Date >= "2020-04-16" & Date <= "2020-09-14" # 16 de abril de 2020-14 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d11 = confinamiento %>% # Pedro Aguirre Cerda
  filter(mun.cod == 13121) %>%
  filter(Date >= "2020-04-23" & Date <= "2020-08-31" # 23 de abril de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d12 = confinamiento %>% # Quinta Normal
  filter(mun.cod == 13126) %>%
  filter(Date >= "2020-04-23" & Date <= "2020-09-28" # 23 de abril de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d13 = confinamiento %>% # Estacion Central
  filter(mun.cod == 13106) %>%
  filter(Date >= "2020-04-30" & Date <= "2020-08-17" # 30 de abril de 2020-17 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d14 = confinamiento %>% # La Pintana 
  filter(mun.cod == 13112) %>%
  filter(Date >= "2020-04-30" & Date <= "2020-09-28" # 30 de abril de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d15 = confinamiento %>% # San Ramon 
  filter(mun.cod == 13131) %>%
  filter(Date >= "2020-04-30" & Date <= "2020-09-07" # 30 de abril de 2020-7 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d16 = confinamiento %>% # Cerrillos
  filter(mun.cod == 13102) %>%
  filter(Date >= "2020-05-05" & Date <= "2020-08-31" # 5 de mayo de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d17 = confinamiento %>% # Quilicura
  filter(mun.cod == 13125) %>%
  filter(Date >= "2020-05-05" & Date <= "2020-09-14" # 5 de mayo de 2020-14 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d18 = confinamiento %>% # Recoleta
  filter(mun.cod == 13127) %>%
  filter(Date >= "2020-05-05" & Date <= "2020-09-07" # 5 de mayo de 2020-7 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d19 = confinamiento %>% # Cerro Navia
  filter(mun.cod == 13103) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-28" # 8 de mayo de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d20 = confinamiento %>% # Lo Espejo
  filter(mun.cod == 13116) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-28" # 8 de mayo de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d21 = confinamiento %>% # Conchali
  filter(mun.cod == 13104) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-28" # 8 de mayo de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d22 = confinamiento %>% # La Cisterna
  filter(mun.cod == 13109) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-07" # 	8 de mayo de 2020-7 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d23 = confinamiento %>% # La Florida
  filter(mun.cod == 13110) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-08-31" # 	8 de mayo de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d24 = confinamiento %>% # La Granja
  filter(mun.cod == 13111) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-07" # 	8 de mayo de 2020-7 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d25 = confinamiento %>% # San Joaquin
  filter(mun.cod == 13129) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-07" # 	8 de mayo de 2020-7 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d26 = confinamiento %>% # Lo Prado
  filter(mun.cod == 13117) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-28" # 		8 de mayo de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d27 = confinamiento %>% # Macul
  filter(mun.cod == 13118) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-08-31" # 	8 de mayo de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d28 = confinamiento %>% # Penalolen
  filter(mun.cod == 13122) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-08-24" # 	8 de mayo de 2020-24 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d29 = confinamiento %>% # Renca
  filter(mun.cod == 13128) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-10-05" # 	8 de mayo de 2020-5 de octubre de 2020
  ) %>% 
  mutate(Paso = 1)

d30 = confinamiento %>% # San Miguel
  filter(mun.cod == 13130) %>%
  filter(Date >= "2020-05-08" & Date <= "2020-09-07" # 	8 de mayo de 2020-7 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d31 = confinamiento %>% # Buin
  filter(mun.cod == 13402) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-09-28" # 	15 de mayo de 2020-28 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d32 = confinamiento %>% # Colina
  filter(mun.cod == 13301) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-07-28" # 	15 de mayo de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d33 = confinamiento %>% # Huechuraba
  filter(mun.cod == 13107) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-08-31" # 	15 de mayo de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d34 = confinamiento %>% # Lampa
  filter(mun.cod == 13302) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-08-09" # 	15 de mayo de 2020-9 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d35 = confinamiento %>% # La Reina
  filter(mun.cod == 13113) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-07-28" # 	15 de mayo de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d36 = confinamiento %>% # Maipu
  filter(mun.cod == 13119) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-08-31" # 15 de mayo de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d37 = confinamiento %>% # Padre Hurtado
  filter(mun.cod == 13604) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-08-24" # 15 de mayo de 2020-24 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d38 = confinamiento %>% # Pudahuel 
  filter(mun.cod == 13124) %>%
  filter(Date >= "2020-05-15" & Date <= "2020-09-21" # 15 de mayo de 2020-21 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d39 = confinamiento %>% # Curacavi 
  filter(mun.cod == 13503) %>%
  filter(Date >= "2020-06-12" & Date <= "2020-08-09" # 12 de junio de 2020-9 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d40 = confinamiento %>% # Melipilla 
  filter(mun.cod == 13501) %>%
  filter(Date >= "2020-06-12" & Date <= "2020-08-09" # 12 de junio de 2020-9 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d41 = confinamiento %>% # Penaflor 
  filter(mun.cod == 13605) %>%
  filter(Date >= "2020-06-12" & Date <= "2020-08-24" # 12 de junio de 2020-24 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d42 = confinamiento %>% # San Jose de Maipo 
  filter(mun.cod == 13203) %>%
  filter(Date >= "2020-06-12" & Date <= "2020-08-24" # 12 de junio de 2020-24 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d43 = confinamiento %>% # Tiltil 
  filter(mun.cod == 13303) %>%
  filter(Date >= "2020-06-12" & Date <= "2020-07-28" # 12 de junio de 2020-28 de julio de 2020
  ) %>% 
  mutate(Paso = 1)

d44 = confinamiento %>% # Calera de Tango 
  filter(mun.cod == 13403) %>%
  filter(Date >= "2020-06-26" & Date <= "2020-08-31" # 26 de junio de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d45 = confinamiento %>% # Talagante
  filter(mun.cod == 13601) %>%
  filter(Date >= "2020-06-26" & Date <= "2020-08-31" # 	26 de junio de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d46 = confinamiento %>% # El Monte
  filter(mun.cod == 13602) %>%
  filter(Date >= "2020-06-26" & Date <= "2020-08-31" # 	26 de junio de 2020-31 de agosto de 2020
  ) %>% 
  mutate(Paso = 1)

d47 = confinamiento %>% # Isla de Maipo
  filter(mun.cod == 13603) %>%
  filter(Date >= "2020-07-27" & Date <= "2020-09-14" # 	27 de julio de 2020-14 de septiembre de 2020
  ) %>% 
  mutate(Paso = 1)

d48 = confinamiento %>% # Paine
  filter(mun.cod == 13404) %>%
  filter(Date >= "2020-09-11" & Date <= "2020-10-03" # 	11 de septiembre de 2020-3 de octubre de 2020
  ) %>% 
  mutate(Paso = 1)


# builds "confinamiento" dataset
confinamiento = merge(confinamiento, rbind(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, d12, d13, d14, d15, d16, d17, d18, d19, d20, d21, d22, d23, d24, d25, d26, d27, d28, d29, d30, d31, d32, d33, d34, d35, d36, d37, d38, d39, d40, d41, d42, d43, d44, d45, d46, d47, d48), by = c("Date", "mun.cod"))

# duplicates?
# nrow(confinamiento[!duplicated(confinamiento[c("Date", "mun.cod")]),]) == nrow(confinamiento)

# merges "confinamiento" dataset with "paso" a paso dataset
paso.d = rbind(paso.d, confinamiento)

# Add covid data
paso.d = merge(paso.d,covid.d, by = c("mun.cod", "Date"), all = TRUE)

# Add socio-economic data
p_load(rio, tidyverse)
idc.d = rio::import(file = 'https://github.com/hbahamonde/Tobalaba/raw/main/IDC_data.csv',which = 1)
idc.d$party = as.factor(idc.d$party)
idc.d$Comuna = as.factor(idc.d$Comuna)
paso.d = merge(paso.d,idc.d, by.y = c("mun.cod"))

# Add Municipal Popuplation data (SINIM, 18yrds old and older)
p_load(rio, tidyverse)
mun.pop = rio::import(file = 'https://github.com/hbahamonde/Tobalaba/raw/main/mun_pop_rm.csv',which = 1)
paso.d = merge(paso.d,mun.pop, by.y = c("mun.cod"))
paso.d["Paso"][is.na(paso.d[c("Paso")])] <- 0 # replaces NA in paso with a 0

save(paso.d, file = "/Users/hectorbahamonde/research/Bus/paso.Rdata")

# multiple imputation
## https://r.iq.harvard.edu/docs/amelia/amelia.pdf

#install.packages("Amelia", repos="http://r.iq.harvard.edu", type = "source")
p_load(Amelia,dplyr)

paso.mi = paso.d %>% dplyr::select("Date", "mun.cod", "Covid", "Bienestar", "Economia", "Educacion", "Paso", "mun.pop") 

## "Unless the rate of missingness is very high, m = 5 (the program default) is probably adequate" p. 4

## bounds
bds <- matrix(c(3,min(paso.d$Covid, na.rm = T),max(paso.d$Covid, na.rm = T)), nrow = 1, ncol = 3) # nota that 3 is the 3rd column of the paso.mi matrix.

## MI procedure via Amelia
amelia.out = amelia(paso.mi, m = 5, 
                    ts = "Date", 
                    cs = "mun.cod", 
                    polytime = 3, # polynomial of the time index
                    p2s = 3, 
                    ords = "Covid",
                    noms = "Paso",
                    bounds = bds,
                    max.resample = 1000,
                    #logs= "l.Covid",
                    seed = 2020
)

# MI Diagnostics
# overimpute(amelia.out, var="Covid")
# summary(amelia.out)

# dev.off();dev.off();
# par(mfrow=c(2,52))
# tscsPlot(amelia.out, cs="13101", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13101"]))))
# tscsPlot(amelia.out, cs="13102", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13102"]))))
# tscsPlot(amelia.out, cs="13103", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13103"]))))
# tscsPlot(amelia.out, cs="13104", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13104"]))))
# tscsPlot(amelia.out, cs="13105", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13105"]))))
# tscsPlot(amelia.out, cs="13106", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13106"]))))
# tscsPlot(amelia.out, cs="13107", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13107"]))))
# tscsPlot(amelia.out, cs="13108", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13108"]))))
# tscsPlot(amelia.out, cs="13109", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13109"]))))
# tscsPlot(amelia.out, cs="13110", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13110"]))))
# tscsPlot(amelia.out, cs="13111", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13111"]))))
# tscsPlot(amelia.out, cs="13112", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13112"]))))
# tscsPlot(amelia.out, cs="13113", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13113"]))))
# tscsPlot(amelia.out, cs="13114", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13114"]))))
# tscsPlot(amelia.out, cs="13115", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13115"]))))
# tscsPlot(amelia.out, cs="13116", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13116"]))))
# tscsPlot(amelia.out, cs="13117", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13117"]))))
# tscsPlot(amelia.out, cs="13118", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13118"]))))
# tscsPlot(amelia.out, cs="13119", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13119"]))))
# tscsPlot(amelia.out, cs="13120", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13120"]))))
# tscsPlot(amelia.out, cs="13121", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13121"]))))
# tscsPlot(amelia.out, cs="13122", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13122"]))))
# tscsPlot(amelia.out, cs="13123", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13123"]))))
# tscsPlot(amelia.out, cs="13124", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13124"]))))
# tscsPlot(amelia.out, cs="13125", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13125"]))))
# tscsPlot(amelia.out, cs="13126", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13126"]))))
# tscsPlot(amelia.out, cs="13127", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13127"]))))
# tscsPlot(amelia.out, cs="13128", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13128"]))))
# tscsPlot(amelia.out, cs="13129", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13129"]))))
# tscsPlot(amelia.out, cs="13130", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13130"]))))
# tscsPlot(amelia.out, cs="13131", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13131"]))))
# tscsPlot(amelia.out, cs="13132", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13132"]))))
# tscsPlot(amelia.out, cs="13201", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13201"]))))
# tscsPlot(amelia.out, cs="13202", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13202"]))))
# tscsPlot(amelia.out, cs="13203", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13203"]))))
# tscsPlot(amelia.out, cs="13301", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13301"]))))
# tscsPlot(amelia.out, cs="13302", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13302"]))))
# tscsPlot(amelia.out, cs="13303", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13303"]))))
# tscsPlot(amelia.out, cs="13401", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13401"]))))
# tscsPlot(amelia.out, cs="13402", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13402"]))))
# tscsPlot(amelia.out, cs="13403", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13403"]))))
# tscsPlot(amelia.out, cs="13404", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13404"]))))
# tscsPlot(amelia.out, cs="13501", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13501"]))))
# tscsPlot(amelia.out, cs="13502", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13502"]))))
# tscsPlot(amelia.out, cs="13503", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13503"]))))
# tscsPlot(amelia.out, cs="13504", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13504"]))))
# tscsPlot(amelia.out, cs="13505", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13505"]))))
# tscsPlot(amelia.out, cs="13601", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13601"]))))
# tscsPlot(amelia.out, cs="13602", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13602"]))))
# tscsPlot(amelia.out, cs="13603", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13603"]))))
# tscsPlot(amelia.out, cs="13604", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13604"]))))
# tscsPlot(amelia.out, cs="13605", var = "Covid", main = unique(as.character(na.omit(air$Municipality[air$mun.cod=="13605"]))))
# dev.off();dev.off();

# options(scipen=10000) # Apaga la notacion cientifica
# compare.density(amelia.out, var= "Covid")


# average all m imputed datasets
covid.imputed <- data.frame(t(rbind(
  imp1 = amelia.out$imputations$imp1$Covid,
  imp2 = amelia.out$imputations$imp2$Covid,
  imp3 = amelia.out$imputations$imp3$Covid,
  imp4 = amelia.out$imputations$imp4$Covid,
  imp5 = amelia.out$imputations$imp5$Covid,
  mun.cod = amelia.out$imputations$imp1$mun.cod,
  Date = as.Date(amelia.out$imputations$imp1$Date, "%Y%m%d"))
))
covid.imputed$Date = as.Date(amelia.out$imputations$imp1$Date, "%Y%m%d")


covid.imputed$Covid.mi <- round(rowMeans(subset(covid.imputed, select = c(imp1,imp2,imp3,imp4,imp5)), na.rm = TRUE),0)
air.mi = covid.imputed %>% dplyr::select("Date", "mun.cod", "Covid.mi") 

# merge imputed air with air
paso.d = merge(air.mi,paso.d, by = c("Date","mun.cod"))


# Airport paper

# Merge with Covid and Airport Data
air = merge(dat,paso.d, by = c("Date","mun.cod"))
p_load(dplyr)
air = air %>% dplyr::select(-c("Comuna Aeródromo")) 
colnames(air)[colnames(air)=="place"] <- "Airport"
air$Paso = as.factor(air$Paso)
p_load(stringr)
air$Airport = str_to_title(air$Airport)
air = air[!is.na(air$operation), ]
air = air[!is.na(air$Latitude), ]
air = air[!is.na(air$Longitude), ]

save(air, file = "/Users/hectorbahamonde/research/Tobalaba/dat.Rdata")

# Bus paper
p_load(dplyr)
bus = paso.d %>% dplyr::select(-c("Covid")) 

save(bus, file = "/Users/hectorbahamonde/research/Bus/dat.Rdata")


############################## 
# Analyses
##############################
cat("\014")
rm(list=ls())
graphics.off()

# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

# import data
p_load(rio)
air = rio::import(file = 'https://github.com/hbahamonde/Tobalaba/raw/main/dat.Rdata')

# Airport locations
p_load(dplyr)
airport.d = unique(air %>% select(mun.cod, Latitude, Longitude, Comuna))

# RM map
p_load(chilemapas)

# filter only RM
rm.d = mapa_comunas %>% 
  filter(codigo_region == 13) %>% 
  left_join(
    codigos_territoriales %>% 
      select(matches("comuna"))
  )

colnames(rm.d)[colnames(rm.d)=="codigo_comuna"] <- "mun.cod"

# add IDC to rm.d
p_load(rio, tidyverse)
idc.d = rio::import(file = 'https://github.com/hbahamonde/Tobalaba/raw/main/IDC_data.csv',which = 1)
idc.d = idc.d %>% select(mun.cod, Economia)

rm.d = merge(rm.d, idc.d, by = "mun.cod")

p_load(dplyr, ggplot2)

# map
ggplot(rm.d) + 
  geom_sf(aes(fill = Economia, geometry = geometry)) +
  theme_minimal(base_size = 13) +
  geom_point(size = 8, aes(x = Longitude, y = Latitude, colour = Comuna),
             data = airport.d[airport.d$Comuna=="La Reina",]) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        plot.background = element_rect(fill = "transparent", color = NA), 
        panel.grid.major = element_blank(),
        legend.position = 'none') +
  #ggtitle("Santiago City: Airport Located at Wealthy Hub") +
  labs(title = "Santiago City: Aerodrome Located at Wealthy Hub", caption = "The lighter the wealthier")


# time series plot
p_load(ggplot2)
ggplot(air, aes(x=Date, fill = Paso)) + geom_bar(width = 0.8) +  facet_grid(scales = "free_y", rows = vars(Comuna))




# Logit model

## recode 1 = paso 1, 0, otherwise
air$Paso.d = ifelse(air$Paso==1 | air$Paso==2, 1, 0)

# toy plot
p_load(lattice)
lattice::histogram(as.factor(air$Paso.d))

# model
formula = as.formula(Paso.d~
                       #Bienestar+
                       #Economia +
                       #Educacion +
                       #party+
                       #IDC +
                       #mun.pop+
                       #Economia + 
                       #Covid.mi #+ 
                       Covid.mi * Economia
                       #Comuna
                     )

logit.model <- glm(formula, data=air, family=binomial(link="logit")) 
summary(logit.model)
p_load("effects") 
plot(predictorEffects(logit.model))

plot(air$Economia,air$Covid.mi)


# interaction fx
## https://cran.r-project.org/web/packages/sjPlot/vignettes/plot_interactions.html
## install.packages("sjPlot")
## install.packages("sjmisc")
library(sjPlot)
library(sjmisc)

plot_model(logit.model, type = "pred", terms = c("Economia", "Covid.mi"))
plot_model(logit.model, type = "pred", terms = c("Covid.mi [all]", "Economia"))



###
air2 = air
air2 = air2[air2$Comuna=="La Reina",]
air2$`2020-2021` = air2$Date
air2$Restrictions = air2$Paso

levels(air2$Restrictions)<-factor(c("Lockdown","Lockdown","Lockdown","Some restrictions","Some restrictions"))


# 1
p1 = ggplot(air2, aes(x=`2020-2021`, fill = Restrictions)) + 
  geom_bar(width = 0.8, stat = "count") +  
  facet_grid(scales = "free_y") + theme_bw() + 
  ggtitle("Airport Operations Located at Wealthy Municipality: Landings and Arrivals") +
  xlab("Date") + ylab("Landings and Arrivals\n(count)") +
  theme(legend.position="bottom")  # none

# 
test = air2 %>% 
  group_by(Date) %>%
  distinct(Covid.mi, Restrictions)



p2 = qplot(data = test,
      x = Date,
      y = Covid.mi,
      fill= Restrictions,
      geom = "col",
      xlab = "Date", ylab = "Covid19\n(count)", 
      main = "Count of Covid19: Wealthy Municipality where Airport is at") +
  theme_bw() + theme(legend.position="bottom")

library(ggpubr)
ggarrange(p1, p2, 
          labels = c("", ""),
          ncol = 1, nrow = 2) 










