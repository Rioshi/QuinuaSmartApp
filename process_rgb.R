### Librerias
library(sp)
library(raster)
library(agricolae)
library(nnet)
library(caret)
##Lectura datos
pclas <- shapefile(file.choose())
img2 <- stack(file.choose())
img3 <- as.data.frame(img2,xy=TRUE)

pclas <- cbind(pclas, extract(img2, pclas))
pclas2<-as.data.frame(pclas,xy=TRUE)


#Clasificacion con Arboles de decision
library(rpart)
class.rpart <- rpart(Type~RGB_res.1+RGB_res.2+RGB_res.3+RGB_res.4, data = pclas2)
plot(class.rpart, compress=TRUE,uniform=TRUE)
text(class.rpart,use.n=T,all=T,cex=.7,pretty=0,xpd=TRUE, col="red")
#Alternativo
library(rattle)
drawTreeNodes(class.rpart,cex=.6,pch=11,size=4*.8, col=NULL,nodeinfo=TRUE,
              units="",cases="obs",digits=getOption("digits"),decimals=2,print.levels=TRUE,
              new=TRUE)
#Prediccion con arbol
img3[,"clase"] <- predict(object = class.rpart,newdata = img3,type ="class")
img3[,"clase2"] <- as.numeric(img3$clase)

write.table(img3,"/home/quinua2/Documentos/Quinua_Smart_App/Drones/clasificacion.txt", sep="\t", row.names=F)

#Validacion
confusionMatrix(data=predict(class.rpart, type="class"), reference=pclas2)
