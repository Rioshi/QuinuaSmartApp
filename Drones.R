###Libraries####
library(raster)
library(dplyr)
library(googlesheets)
library(reshape2)
library(sp)
library(ggplot2)
##Read files###
startdir <- getwd()
setwd("/media/carlos/CJMN_ext/QuinuaSmartApp/Drone/Multiespectral/21_Mayo/Resample_0.10")
files <- list.files(pattern="tif$")
stack1 <- list()
for(i in 1:length(files)) {
  stack1[[i]] <- raster(files[i])}
stack1
covariables <- do.call(stack, stack1) ### JO!
setwd(startdir)

limite <- shapefile("/media/carlos/CJMN_ext/QuinuaSmartApp/Drone/Multiespectral/21_Mayo/Shape/Limite.shp")
covariables <- crop(covariables,limite)
##Convert raster to data frame##
ras.df <- as.data.frame(covariables, xy=TRUE)
ras.df1 <- ras.df
ras.df1 <- na.omit(ras.df1) 
#Cluster numbers
ras.df <- ras.df[,-c(1:2)]
ras.df <- na.omit(ras.df) 
ras.df <- scale(ras.df)
wss <- (nrow(ras.df)-1)*sum(apply(ras.df,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(ras.df, 
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Numero de clusters",
     ylab="Suma de cuadrados dentro del grupo")

##Analisis Cluster
fit <- kmeans(ras.df, 6)
aggregate(ras.df,by=list(fit$cluster),FUN=mean)
ras.df1 <- data.frame(ras.df1, fit$cluster)
ras.df1$fit.cluster <- as.numeric(ras.df1$fit.cluster)
ras.df.sp <- ras.df1
coordinates(ras.df.sp) <- ~ x + y
gridded(ras.df.sp) <- TRUE
rasterDF <- stack(ras.df.sp)
plot(rasterDF$fit.cluster)
writeRaster(rasterDF$fit.cluster, 
            "/media/carlos/CJMN_ext/QuinuaSmartApp/Drone/Multiespectral/21_Mayo/Resample_0.10/cluster.tif",
            overwrite=TRUE)

#Procesado
ndvi <- raster("H:/QuinuaSmartApp/Drone/Multiespectral/21_Mayo/Resample_0.10/NDVI.tif")
ze <- shapefile("H:/QuinuaSmartApp/Drone/Multiespectral/21_Mayo/Shape/Limite.shp")

ndvi <- crop(ndvi,ze)
hist(as.data.frame(ndvi,na.rm=TRUE)$NDVI, col="lightblue",ylab="",xlab="",main="Tetracam",xlim=c(-0.2,1))
abline(v = cellStats(ndvi, stat='mean', na.rm=TRUE), col="red", lwd=3, lty=2)

#Abrir Libro de Campo
gs_auth()
my_sheets <- gs_ls()
gap <- gs_title("Remote Sensing")
RS <- gap %>%
  gs_read(ws = "Results")
RS<- as.data.frame(RS)

RS <- subset(RS,Fecha=="16/04/2018"|Fecha=="17/04/2018")
hist(na.omit(RS$NDVI), col="lightblue",ylab="",xlab="",main="Green Seeker",xlim=c(-0.2,1))
abline(v = mean(na.omit(RS$NDVI)), col="red", lwd=3, lty=2)

par(mfrow=c(1,2))

#########################
## DATOS LIDAR############
#########################
library(dplyr)
library(readr)
setwd("D:/QuinuaSmartApp/Drone/Lidar/220418_Jauja/pcap_8_11")
df <- list.files(full.names = TRUE) %>% 
  lapply(read_csv) %>% 
  bind_rows 
df<- as.data.frame(df)
df<- df[,c(4:6)]
df.sp <- df
#Crear el objeto espacial
coordinates(df.sp)<-~X+Y

#Crear un raster del objeto espacial
lidar.ras <- rasterFromXYZ(df)
