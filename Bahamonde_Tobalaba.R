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
#airport$lat.2 = jitter(airport$lat.2, 7)
#airport$long.2 = jitter(airport$long.2, 7)



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


# Merge with Covid and Airport Data
aux.dat = merge(dat,paso.d, by.y = c("Date","mun.cod"))

# Separate into different datasets
p_load(dplyr)


# Mobility paper (Bus)
mobility = aux.dat %>% dplyr::select(-c("operation", "plate", "aircraft", "place", "Comuna Aeródromo", "Latitude", "Longitude" ))
mobility$Paso = as.factor(mobility$Paso)
mobility = data.frame(na.omit(mobility))
#save(mobility, file = "/Users/hectorbahamonde/research/Bus/data.Rdata")

# Airport paper
air = aux.dat %>% dplyr::select(-c("Covid", "Comuna"))
air = data.frame(na.omit(air))
air$Paso = as.factor(air$Paso)
colnames(air)[colnames(air)=="Comuna.Aeródromo"] <- "Municipality"
colnames(air)[colnames(air)=="place"] <- "Airport"
p_load(stringr)
air$Municipality = str_to_title(air$Municipality) 
air$Airport = str_to_title(air$Airport) 
save(air, file = "/Users/hectorbahamonde/research/Tobalaba/dat.Rdata")



############################## 
# Toy map
##############################
cat("\014")
rm(list=ls())
graphics.off()

# loads pacman
if (!require("pacman")) install.packages("pacman"); library(pacman) 

setwd("~/research/Tobalaba")
load("dat.Rdata")



## https://github.com/dkahle/ggmap
# if(!requireNamespace("devtools")) install.packages("devtools")
# devtools::install_github("dkahle/ggmap")
p_load("ggmap")


#airport.centro = dat[ which(dat$Latitude <= -29 & dat$Latitude>= -36 & dat$Longitude >= -73 & dat$Longitude <= -69),]


qmplot(Longitude, Latitude, 
       geom = "auto",
       #zoom = 4, 
       data = air, 
       maptype = "toner-lite", 
       #darken = .4,
       alpha = I(0.2),
       color = I("red"),
       legend = "topleft",
       facets = ~Paso
)




#### Transportation Analyses

# https://spatial.blog.ryerson.ca/2019/09/03/transportation-flow-mapping-using-r/
# https://rstudio-pubs-static.s3.amazonaws.com/259095_2f8cb24b43284692a8af916bd447931d.html
# https://servicios.dgac.gob.cl/portal_consulta_aeronaves/
# https://journal.r-project.org/archive/2013-1/kahle-wickham.pdf
