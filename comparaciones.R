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
meteoro <- as.data.frame.data.frame(meteoro)

evpt <- meteoro[,c(1:8,13:14)]
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
library(gridExtra)
library(ggpubr)

#Lineas de regresion coincidentes (el comun)
md.1 <- lm(ETo ~ ETa, data=meteoro, na.action=na.omit)
summary(md.1)
sqrt(mean(md.1$residuals^2))

g1 <- ggplot(meteoro, aes(x=ETa, y=ETo, na.rm = TRUE,color=MES)) + 
  geom_point(shape=20, na.rm=TRUE) +
  geom_smooth(method=lm, na.rm=TRUE, aes(group=1)) +
  theme_light() +
  theme(axis.text=element_text(size=10),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab(expression(bold(paste('ETa (mm.día'^"-1"*')'))))+
  ylab(expression(bold(paste('ETo (mm.día'^"-1"*')'))))

#Agregando la altitud como un factor mas
md.2 <- lm(ETo ~ ETa + Altitud, data=evpt, na.action = na.omit)
summary(md.2)
sqrt(mean(md.2$residuals^2))
evpt["fit"] <- fitted(md.2) 

g2 <- ggplot(evpt, aes(x=ETo, y=fit, na.rm = TRUE,color=MES)) + 
  geom_point(shape=20, na.rm=TRUE) +
  geom_smooth(method=lm, na.rm=TRUE, aes(group=1)) +
  theme_light()+
  theme(axis.text=element_text(size=10),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  xlab(expression(bold(paste('ETa (mm.día'^"-1"*')'))))+
  ylab(expression(bold(paste('Valores predichos (mm.día'^"-1"*')'))))


png("G:/QuinuaSmartApp/Articulo_ET/Imagenes_Resultados/comparaciones1.png", width = 15, height = 10, units = 'cm', res = 300)
ggarrange(g1,g2, ncol=2, common.legend = TRUE, legend="bottom")
dev.off()
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

##Funcion compiladora
compilado <- function(x){
  nor <- nortest::ad.test(resid(x))$p-value
  hom <- car::ncvTest(x)$p
  ind <-car::durbinWatsonTest(x)$p-value
  return(nor,hom,ind)
}

##Normalidad de los errores
plot(md.2,2) #grafico normal q-q
nortest::ad.test(resid(md.2))

##Homogeneidad de varianzas
plot(md.2,3)
car::ncvTest(md.2)

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
plot(acf(rstandard(md.2), lag = 40))


######REGRESION POR MESES DEL AÑO######
unique(evpt$date)
feb <- subset(evpt,date=="28/02/2017")
mar <- subset(evpt,date=="16/03/2017")
abr <- subset(evpt,date=="17/04/2017")
may <- subset(evpt,date=="19/05/2017")
jun <- subset(evpt,date=="20/06/2017" | date=="04/06/2017")
jul <- subset(evpt,date=="06/07/2017" | date=="22/07/2017")
ago <- subset(evpt,date=="07/08/2017" | date=="23/08/2017")
sep <- subset(evpt,date=="08/09/2017")
oct <- subset(evpt,date=="26/10/2017")
dic <- subset(evpt,date=="13/12/2017")

#Regresiones por meses
md.31 <- lm(ETo ~ ETa, data=dic, na.action=na.omit)
summary(md.31)
sqrt(mean(md.31$residuals^2))
nortest::ad.test(resid(md.31))
car::ncvTest(md.31)
car::durbinWatsonTest(md.31)

md.32 <- lm(ETo ~ ETa + Altitud, data=dic, na.action=na.omit)
summary(md.32)
sqrt(mean(md.32$residuals^2))
nortest::ad.test(resid(md.32))
car::ncvTest(md.32)
car::durbinWatsonTest(md.32)


####################
#COMPARACIONES MESES METRIC
####################
library(rasterVis)
startdir <- getwd()
setwd("E:/QuinuaSmartApp/Articulo_ET/Rasters/ET_METRIC")
files <- list.files(pattern="tif$")
files
stack1 <- list()
for(i in 1:length(files)) {
  stack1[[i]] <- raster(files[i])}
stack1
metric <- do.call(stack, stack1) ### JO!
setwd(startdir)
rm(startdir,files,stack1)
metric

est <- gap %>%
  gs_read(ws = "coordinates")
coordinates(est) = ~X+Y
proj4string(est) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
est <- spTransform(est, crs(metric))

nam <- c("4-jun","06-jul","07-ago","08-sep","13-dic","16-mar","17-abr","19-may","20-jun",
                   "22-jul","23-ago","26-oct","28-feb")

library(colorspace)
myTheme <- rasterTheme(region=sequential_hcl(20, power=2.2))

png("E:/QuinuaSmartApp/Articulo_ET/Imagenes_Resultados/comparaciones2.png", width = 15, height = 10, units = 'cm', res = 400)
levelplot(metric,margin=FALSE,par.settings = myTheme, scales=list(draw=FALSE),names.attr=nam)  +
  latticeExtra::layer(sp.points(est,pch=20,cex=0.5,col="black"))
#Nota: Cuando ggplot2 esta activo se cruza con layer() de latticeExtra
dev.off()




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

####################
#SERIES DE TIEMPO
####################
library("lubridate")
meteoro$date <- as.Date(meteoro$date,format='%d/%m/%Y')
meteoro[,"DOY"] <- yday(meteoro$date) 

ggplot(meteoro, aes(x=DOY, y=ETo,na.rm=TRUE)) +
  #scale_x_date(limits=c(as.Date("01/01/2017"),as.Date("31/12/2017"))) +
  geom_point(na.rm=TRUE) +
  geom_line()

#Objetos de clase serie de tiempo#
timEto <- ts(na.omit(meteoro$ETo),start=2,end=12)
timEto
plot.ts(timEto)
abline(reg=lm(timEto~time(timEto)))


timeEta <- ts(na.omit(meteoro$ETa),start=2,end=12)
timeEta
plot.ts(timeEta)
abline(reg=lm(timeEta~time(timeEta)))

#Boxplot por meses
unique(meteoro$MES)
meteoro$MES<-ordered(meteoro$MES,levels=c("Feb","Mar","Abr","May","Jun","Jul",
                             "Ago","Sep","Oct","Dic"))
boxplot(ETo~MES,data=meteoro)

ggplot(meteoro,aes(x=MES,y=ETo,na.rm=TRUE)) + 
  geom_boxplot()

#Crear un conjunto de datos dual
a1 <- meteoro[,c(6,13)]
a1[,"tt"] <- rep("ETo",length(a1))
names(a1) <- c("MES","Evp","Tipo")
a2 <- meteoro[,c(6,14)]
a2[,"tt"] <- rep("ETa",length(a2))
names(a2) <- c("MES","Evp","Tipo")
a3 <- rbind(a1,a2) #para unir uno bajo otro rbind(a1,a2) horizontal cbind(a1,a2)
rm(a1,a2)

png("G:/QuinuaSmartApp/Articulo_ET/Imagenes_Resultados/boxplotemporal.png", width = 15, height = 10, units = 'cm', res = 400)
ggplot(a3,aes(x=MES,y=Evp,fill=Tipo,na.rm=TRUE)) + 
  geom_boxplot() +
  theme_light() +
  theme(axis.text=element_text(size=10),
        axis.title.x = element_text(face="bold"),
        axis.title.y = element_text(face="bold"))+
  ylab(expression(bold(paste('Evapotranspiración (mm.día'^"-1"*')'))))+ xlab("")+
  guides(fill=guide_legend(title=NULL))
dev.off()