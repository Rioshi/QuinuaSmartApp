#Lectura de datos
meteoro <- read.delim("clipboard",header = TRUE)
evpt <- meteoro[,c(1:7,12:13)]
head(evpt)
evpt <- na.omit(evpt)
####################
#Prueba de Wilcoxon
####################
library(moments) 
library(lawstat)
library(exactRankTests)

#Supuesto 
skewness(evpt$ETo-evpt$ETa) #asimetria de la diferencia
hist(evpt$ETo-evpt$ETa) #ver la simetria
# Ho: As = 0
# Ha: As != 0 
symmetry.test(evpt$ETo-evpt$ETa,boot=FALSE,option="MGG")

#Prueba de Wilcoxon pareada
# Ho: ETo - Eta >= 0
# Ha: Eto - Eta < 0    ||  ETo < Eta

wilcox.exact(x=evpt$ETo,y=evpt$ETa,paired = TRUE,mu = 0,alternative = "less",exact = TRUE)


by(data = evpt,INDICES = evpt[,"estacion"],
   FUN = function(x) wilcox.exact(x=evpt$ETo,y=evpt$ETa,paired = TRUE,mu = 0,alternative = "less",exact = TRUE))

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

####################
#COMPARACION DE MODELOS
####################

#Modelo 1 vs Modelo 2
anova(md.1,md.2)
#Modelo 1 vs Modelo 3
anova(md.1,md.3)
#Modelo 1 vs Modelo 4
anova(md.1,md.4)
#Modelo 2 vs Modelo 3
anova(md.2,md.3)
#Modelo 2 vs Modelo 4
anova(md.2,md.4)
#Modelo 3 vs Modelo 4
anova(md.3,md.4)

####################
#MODELOS DE REGRESION EVAPOTRANSPIRACION - ZONAS DE VIDA
####################

#Lineas de regresion coincidentes (el comun)
md.1 <- lm(ETo ~ ETa, data=meteoro, na.action=na.omit)
summary(md.1)


#Modelo mas general, cada nivel tiene su propia pendiente e intercepto
md.2 <- lm(ETo ~ ZV + ETa:ZV, data=meteoro, na.action = na.omit)
summary(md.2)

#Modelo de regresiones paralelas, cada nivel tiene su propio intercepto pero pendiente comun
md.3 <- lm(ETo ~ ETa + ZV, data=meteoro, na.action = na.omit)
summary(md.3)

#Model de intercepto comun, cada nivel tiene su propia pendiente, pero intercepto comun
md.4 <- lm(ETo ~ ETa + ZV:ETa, data=meteoro, na.action = na.omit)
summary(md.4)



####################
#TRANSFORMACIONES A LA POTENCIA
####################
library(alr3)
boxCox(md.1, lambda = seq(0, 3, by = 0.1))

####################
#Modelo de regresion por estaciones
####################
cnx <- with(evpt,
            by(evpt, evpt[,"estacion"],
               function(x) lm(ETo ~ ETa, data = evpt)))
sapply(cnx,function(cnx) summary(cnx)$r.squared)

####################
#COMPROBACION DE LOS SUPUESTOS
####################

#Normalidad de los errores
plot(md.1,2) #grafico normal q-q
nortest::ad.test(resid(md.1))

#Homogeneidad de varianzas
plot(md.1,3)
car::ncvTest(md.1)

#Errores correlacionados (independencia)
car::durbinWatsonTest(md.1)
plot(md.1,1)
tt <- broom::augment(md.1)
plot(x=tt$ETa,y=tt$.resid)

