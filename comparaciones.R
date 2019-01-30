library(googlesheets)
library(raster)
library(rasterVis)
library(spdep)
library(dplyr)
library(maptools)
library(agricolae)
##################################
#Lectura de datos con googlesheets
##################################

gs_auth()
my_sheets <- gs_ls()
gap <- gs_title("Meteorologia")
meteoro <- gap %>%
  gs_read(ws = "ET_F")

evpt <- meteoro[,c(1:7,12:13)]
evpt<- na.omit(evpt)
por <- evpt$ETa*100/evpt$ETo
mean(por)
####################
#Prueba de Wilcoxon
####################
library(moments) 
library(lawstat)
library(exactRankTests)

#Supuesto de Asimetría
moments::skewness(evpt$ETo,na.rm=TRUE)
moments::skewness(evpt$ETa,na.rm=TRUE)
moments::skewness(evpt$ETo-evpt$ETa,na.rm=TRUE)


hist(evpt$ETo-evpt$ETa) #ver la simetria
# Ho: As = 0
# Ha: As != 0 
symmetry.test(na.omit(evpt$ETo),boot=FALSE,option="MGG")
symmetry.test(na.omit(evpt$ETa),boot=FALSE,option="MGG")
symmetry.test(na.omit(evpt$ETo-evpt$ETa),boot=FALSE,option="MGG")

#Prueba de Wilcoxon pareada
# Ho: ETo - Eta >= 0
# Ha: Eto - Eta < 0    ||  ETo < Eta

wilcox.exact(x=evpt$ETo,y=evpt$ETa,paired = TRUE,mu = -1,alternative = "less",exact = TRUE)


##RESUMEN ESTADISTICO##
sd(evpt$ETo,na.rm=TRUE)*100/mean(evpt$ETo,na.rm=TRUE)
sd(evpt$ETa,na.rm=TRUE)*100/mean(evpt$ETa,na.rm=TRUE)
sd(evpt$ETo-evpt$ETa,na.rm=TRUE)*100/abs(mean(evpt$ETo-evpt$ETa,na.rm=TRUE))
max(evpt$ETo,na.rm=TRUE)
max(evpt$ETa,na.rm=TRUE)
max(evpt$ETo-evpt$ETa,na.rm=TRUE)
min(evpt$ETo,na.rm=TRUE)
min(evpt$ETa,na.rm=TRUE)
min(evpt$ETo-evpt$ETa,na.rm=TRUE)
####################
#MODELOS DE REGRESION EVAPOTRANSPIRACION - ESTACIONES METEOROLOGICAS
####################
library(ggplot2)

#Lineas de regresion coincidentes (el comun)
md.1 <- lm(ETo ~ ETa, data=meteoro, na.action=na.omit)
summary(md.1)

ggplot(meteoro, aes(x=ETa, y=ETo, na.rm = TRUE)) + 
  geom_point(shape=1, na.rm=TRUE) +
  geom_smooth(method=lm, na.rm=TRUE)

#Agregando la altitud como un factor mas
md.2 <- lm(ETo ~ ETa + Altitud, data=meteoro, na.action = na.omit)
summary(md.2)

ggplot(meteoro, aes(x=ETa, y=ETo, na.rm = TRUE)) + 
  geom_point(shape=1, na.rm=TRUE) +
  geom_smooth(method=lm, na.rm=TRUE)


####################
#TRANSFORMACIONES A LA POTENCIA
####################
library(alr3)
boxCox(md.1, lambda = seq(0, 3, by = 0.1))

####################
#Coeficiente de determinacion por estacion
####################

md.listo <- nlme::lmList(ETo ~ ETa | estacion, evpt,na.action=na.omit)
md.listo.resu <- summary(md.listo)
md.listo.resu$r.squared

####################
#COMPROBACION DE LOS SUPUESTOS
####################

##Normalidad de los errores
plot(md.2,2) #grafico normal q-q
nortest::ad.test(resid(md.1))

##Homogeneidad de varianzas
plot(md.2,3)
car::ncvTest(md.1)

##Errores correlacionados (independencia)
# Ho: Los errores No están correlacionados
car::durbinWatsonTest(md.2)
plot(md.1,1)
tt <- broom::augment(md.1)
plot(x=tt$ETa,y=tt$.resid)

#Comprobar si hay autocorrelacion espacial
evpt.sp <- evpt
evpt.sp[,"Residuals"] <- resid(md.2)
coordinates(evpt.sp) <- ~Longitud+Latitud
crs(evpt.sp) <- "+proj=longlat +ellps=WGS84 +no_defs"
sw <- knearneigh(evpt.sp, longlat = TRUE)
sw <- knn2nb(sw,sym = TRUE)
sw <- nb2listw(sw,zero.policy = TRUE)
moran.mc(x = evpt.sp$Residuals,listw = sw,nsim=10000,adjust.n=TRUE) #Ho: No hay autocorrelacion



#Comprobar si hay autocorrelacion temporal
plot(acf(rstandard(md.1), lag = 40))


######REGRESION POR MESES DEL AÑO######
unique(evpt$date)
feb <- subset(evpt,date=="28/02/2017")
mar <- subset(evpt,date=="16/03/2017")
abr <- subset(evpt,date=="17/04/2017")
may <- subset(evpt,date=="19/05/2017")
jun <- subset(evpt,date=="20/06/2017"& date=="04/06/2017")
jul <- subset(evpt,date=="06/07/2017"& date=="22/07/2017")
ago <- subset(evpt,date=="07/08/2017"& date=="23/08/2017")


########################################EXTRA##########################################
#######################################################################################
#Modelo mas general, cada nivel tiene su propia pendiente e intercepto
md.2 <- lm(ETo ~ estacion + ETa:estacion, data=meteoro, na.action = na.omit)
summary(md.2)

ggplot(meteoro, aes(x=ETa, y=ETo, color= estacion, na.rm = TRUE)) + 
  geom_point(shape=1, na.rm=TRUE) +
  geom_smooth(method=lm, na.rm=TRUE,se=FALSE,size=0.5)

#Modelo de regresiones paralelas, cada nivel tiene su propio intercepto pero pendiente comun
md.3 <- lm(ETo ~ ETa + estacion, data=meteoro, na.action = na.omit)
summary(md.3)

library(broom)
ggplot(augment(md.3), aes(x=ETa, y=ETo, color= estacion)) + 
  geom_point(shape=1) +
  geom_line(aes(y = .fitted))

#Model de intercepto comun, cada nivel tiene su propia pendiente, pero intercepto comun
md.4 <- lm(ETo ~ ETa + estacion:ETa, data=meteoro, na.action = na.omit)
summary(md.4)

ggplot(augment(md.4), aes(x=ETa, y=ETo, color= estacion)) + 
  geom_point(shape=1) +
  geom_line(aes(y = .fitted))
#######################################################################################
#######################################################################################
