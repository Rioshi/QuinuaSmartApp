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
