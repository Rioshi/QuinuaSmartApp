library(raster)
library(MASS)
library(car)

ndvi <- raster(file.choose())
samples <- shapefile(file.choose())

par(mfrow=c(1,2))
hist(na.omit(ndvi$NDVI_002), col="lightblue",ylab="",xlab="",main="Tetracam NDVI",xlim=c(0,1))

p1 <- hist(na.omit(samples$NDVI_002))
p2 <- hist(na.omit(samples$NDVI_campo))

plot( p1, col=rgb(0,0,1,1/4), xlim=c(0.4,1),main="Comparación NDVI",xlab="")  # first histogram
plot( p2, col=rgb(1,0,0,1/4), xlim=c(0.4,1), add=T)  # second


md1 <- lm(NDVI_campo~NDVI_002,samples[-14,])
summary(md1)
plot(md1)

