######################################
##Lectura de Librerias
#####################################
library(agricolae)
library(ggplot2)

######################################
##Lectura de datos
#####################################
hr <- read.delim("clipboard",header = TRUE)
hr$bloq <- as.factor(hr$bloq)
attach(hr)

######################################
##Generacion del modelo parcelas divididas y graficado
#####################################
model<-with(hr,ssp.plot(bloq,den,fert,var,w1000))          
gla<-model$gl.a;glb<-model$gl.b;glc<-model$gl.c
Ea<-model$Ea;Eb<-model$Eb;Ec<-model$Ec
out1<-with(hr,LSD.test(w1000,den,gla,Ea,console=TRUE))
out2<-with(hr,LSD.test(w1000,den,glb,Eb,console=TRUE))
out3<-with(hr,LSD.test(w1000,den,glc,Ec,console=TRUE))

plot(out1,xlab="Densidad",las=1,variation="IQR")

######################################
##Anova de parcelas divididas PRIMERA FORMA
#####################################
AOV<-aov(w1000 ~ bloq + den*fert*var + Error(rep/den/fert),data=hr)
summary(AOV)

ggplot(data = hr, aes(x = den, y = w1000, colour = fert, group = fert)) +
  stat_summary(fun.y = mean, geom = "point") + stat_summary(fun.y = mean,
                                                            geom = "line") + labs(y = "Peso de 100 g") + theme_bw()
######################################
##Anova de parcelas divididas SEGUNDA FORMA
#####################################
library(lmerTest)
with(hr, interaction.plot(x.factor = fert, trace.factor = den, 
                            response = w1000))

fit <- lmer(w1000 ~ den*fert*var + (1 | bloq), data = hr)
anova(fit)
