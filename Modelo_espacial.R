library(googlesheets)
library(raster)
library(rasterVis)
library(spdep)
library(dplyr)
library(maptools)

##################################
#Lectura de datos con googlesheets
##################################

gs_auth()
my_sheets <- gs_ls()
gap <- gs_title("Meteorologia")
met <- gap %>%
  gs_read(ws = "ET_F")
met <- met[,c(1:7,12,13)]

#Seleccion de una sola fecha
met1 <- subset(met,date=="28/02/2017")
met1 <- na.omit(met1)

##################################
#Creacion de estructura espacial
##################################

coordinates(met1) <- ~Longitud+Latitud
crs(met1) <- "+proj=longlat +ellps=WGS84 +no_defs"
plot(met1)
maptools::pointLabel(coordinates(met1),labels=met1$estacion)

#1. matriz de pesos
sw <- knearneigh(met1, longlat = TRUE)
#2. Crear objeto nb
sw <- knn2nb(sw,sym = TRUE)
is.symmetric.nb(sw)
#3. Crear lista de vecinos
sw <- nb2listw(sw,zero.policy = TRUE)
#4. Comprobar autocorrelacion espacial
#Ho: El coeficiennte es 0 - no hay autocorrelacion espacial
#Ha: El coeficiente es diferente de 0
moran.test(x = met1$ETo,listw = sw,randomisation=TRUE,adjust.n=TRUE)
moran.test(x = met1$ETa,listw = sw)
#4.1 Comprobar autocorrelacion espacial "no para metrico"
moran.mc(x = met1$ETo,listw = sw,nsim=100000,adjust.n=TRUE)

##################################
#Modelamiento tradicional y comprobacion de autocorrelacion de residuos
##################################
md.1 <- lm(ETo ~ ETa+date, data=met1)
summary(md.1)
#comprobacion de autocorrelacion de residuos
lm.morantest(md.1,sw)
