##### EVAPOTRANSPIRACION USANDO METODO METRIC
##### paquete "water"

library(water)
library(RStoolbox)
setwd("C:/Users/ULima/Documents/Proyectos_R_Carlos/ET_remote_sensing")
#Delimitacion area de interes "aoi"

aoi <- createAoi(topleft = c(439432,-1293748), 
                 bottomright = c(504827,-1351046), EPSG = 32618)
#drawExtent() para demarcar un area en el mapa

###Ubicaciones estaciones
est <- read.csv("Meteorologia/estaciones.csv",header = TRUE)
coordinates(est) = ~X+Y
proj4string(est) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
est <- spTransform(est, crs(imageL8)) #imageL8 esta mas adelante
#plot(imageL8$B)
#plot(est,add=TRUE)
#plot(aoi,add=TRUE)
###############################################################

#Lectura de datos estacion meteorologica
csvfile <- read.csv("Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile

#Lectura de MTL landsat 8
MTLfile <- "Landsat/Example2/DN/LC08_L1TP_006068_20170604_20170616_01_T1_MTL.txt"

#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.860133, long= -75.397031, elev=3304, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              cf=c(1,1,1),
                              MTL = MTLfile,
                              tz="America/Lima")

plot(WeatherStation,hourly=TRUE) #sat=FALSE,date=...

#Carga de imagenes satelitales
imageL8 <- loadImage(path = "Landsat/Example2/DN", sat = "L8",aoi)
imageL8 <- crop(imageL8,aoi)
#plot(imageL8)
###Cargar MDE
#checkSRTMgrids(imageL8)
dem <- raster("MDE/MDE_mosaic.tif")
#dem <- projectRaster(dem,crs=crs(imageL8))
#plot(dem,add=TRUE)
dem <- crop(x=dem,y=aoi)
dem <- resample(dem, imageL8$B, method="bilinear")
origin(dem)
origin(imageL8)
#All coefficients
surface.model <-METRICtopo(dem)
solar.angles.r <- solarAngles(surface.model = surface.model, 
                              WeatherStation = WeatherStation, MTL = MTLfile)
plot(solar.angles.r)
Rs.inc <- incSWradiation(surface.model = surface.model, 
                         solar.angles = solar.angles.r, 
                         WeatherStation = WeatherStation)

#problem here THE PROBLEM IS THE DEM RESOLUTION igualala a la de la imagen 30 m
image.TOAr <- calcTOAr(image.DN = imageL8, sat="L8", MTL = MTLfile, 
                       incidence.rel = solar.angles.r$incidence.rel)

image.SR <- loadImageSR(path="Landsat/Example2/Reflectance",aoi=aoi)




plot(imageL8$B, col = "grey")
plot(dem, add = TRUE)

#Momentum Roughness Length
lai <- LAI(method = "metric2010",image=image.TOAr,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)
rm(lai)
rm(ndvi)
rm(alb)
#wpoint <- SpatialPoints(coords = est@coords[8,])
#WeatherStation$location[1,1:2]
#proj4string(wpoint) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
#wpoint <- spTransform(wpoint, crs(imageL8))
#extract(x=imageL8,y=wpoint)
#extract(x=mom,y=est[8,])

#Energy Balance
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=2, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "metric2010", 
                            Z.om.ws = extract(x=mom,y=est[8,]),
                            MTL=MTLfile)


plot(Energy.Balance)

#Evapotranspiration 
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)


writeRaster(ET.24$layer,filename = "Resultados/ET1.tif",format="GTiff",overwrite=TRUE)

#Calibrar el metodo
extract(x=ET.24,y=est@coords)

#library(Evapotranspiration)
huayta <- read.csv("Meteorologia/huaytapallana.csv",
                    header = TRUE,na.strings=c("NA"))

Mest2 <-  read.WSdata(WSdata = huayta, date.format = '%d/%m/%Y',
                      time.format='%H:%M:%S',lat=-11.92666667, long= -75.06166667, elev=4684, height= 2.2,
                      columns=c("date" = 1, "time" = 2, "radiation" = 9,
                                "wind" = 3, "RH" = 8, "temp" = 5, "rain" = 7), 
                      cf=c(1,1,1),
                      MTL = MTLfile,
                      tz="America/Lima")
plot(Mest2)
