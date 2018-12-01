#Lectura de datos
evpt <- read.delim("clipboard",header = TRUE)
evpt$ETa[evpt$ETa==0] <- NA
evpt <- evpt[,c(1:6,8:12)]
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
#modelo de regresion total
####################
plot(x=evpt$ETa, y=evpt$ETo)

md.1 <- lm(ETo~ ETa, data = evpt)
summary(md.1)
plot(md.1)

md.2 <- lm(ETo~ ETa + estacion, data = evpt)
jojo <-summary(md.2)
plot(md.2)

md.3 <- lm(ETo~ ETa + Altitud, data = evpt)
summary(md.3)
plot(md.3)

####################
#Modelo de regresion por estaciones
####################
cnx <- with(evpt,
            by(evpt, evpt[,"estacion"],
               function(x) lm(ETo ~ ETa, data = evpt)))
sapply(cnx,function(cnx) summary(cnx)$r.squared)

