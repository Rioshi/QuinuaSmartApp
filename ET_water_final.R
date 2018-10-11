library(water)
library(RStoolbox)
library(doParallel)
library(foreach)
setwd("/media/carlos/CJMN_ext/QuinuaSmartApp")

#Optimizacion del rendimiento
rasterOptions(maxmemory=1e+11)
UseCores <- detectCores() -1
#Register CoreCluster
cl       <- makeCluster(3)
registerDoParallel(cl)  #Para iniciar
stopCluster(cl) #para terminar
waterOptions(writeResults = FALSE)

#Lectura de datos estacion meteorologica
csvfile <- read.csv("ET_remote_sensing/Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile

#####################################
#####Lectura de MTL landsat 8 12-sept
#####################################

MTLfile <- "Articulo_ET/Landsat/DN/12-sep-2017/LC08_L1TP_006068_20170823_20170912_01_T1/LC08_L1TP_006068_20170823_20170912_01_T1_MTL.txt"

#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.860133, long= -75.397031, elev=3304, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              cf=c(1,1,1),
                              MTL = MTLfile,
                              tz="America/Lima")

plot(WeatherStation,hourly=TRUE) #sat=FALSE,date=...

#Cargar imagen
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/12-sep-2017/LC08_L1TP_006068_20170823_20170912_01_T1", sat = "L8",aoi)
plot(imageL8)

#Cargar todas las estaciones
est <- read.csv("ET_remote_sensing/Meteorologia/estaciones.csv",header = TRUE)
coordinates(est) = ~X+Y
proj4string(est) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
est <- spTransform(est, crs(imageL8))

#Crear area de trabajo
plot(imageL8$B)
plot(est,add=TRUE)
#drawExtent()
#aoi <- createAoi(topleft = c(438140,-1293171), 
#                 bottomright = c(505341,-1367176), EPSG = 32618) #AOI ANTERIOR
aoi <- createAoi(topleft = c(434738.4,-1234477), 
                 bottomright = c(555528.7,-1385039), EPSG = 32618)

#Cortar el area de trabajo
imageL8 <- crop(imageL8,aoi)

###Cargar MDE
dem <- raster("ET_remote_sensing/MDE/MDE_mosaic.tif")
dem <- crop(x=dem,y=aoi)
dem <- resample(dem, imageL8$B, method="bilinear")
origin(dem)
origin(imageL8)

######### Inicia METRIC
#All coefficients
surface.model <-METRICtopo(dem)
solar.angles.r <- solarAngles(surface.model = surface.model, 
                              WeatherStation = WeatherStation, MTL = MTLfile)
Rs.inc <- incSWradiation(surface.model = surface.model, 
                         solar.angles = solar.angles.r, 
                         WeatherStation = WeatherStation)
image.TOAr <- calcTOAr(image.DN = imageL8, sat="L8", MTL = MTLfile, 
                       incidence.rel = solar.angles.r$incidence.rel)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/12-sep-2017/LC080060682017082301T1-SC20180714004437",
  aoi=aoi)

#Momentum Roughness Length
lai <- LAI(method = "metric2010",image=image.TOAr,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)
rm(lai,ndvi,alb)

#Energy Balance
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=2, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "metric2010", 
                            Z.om.ws = extract(x=mom,y=est[8,]),
                            MTL=MTLfile)
rm(mom,Rs.inc,solar.angles.r,surface.model)

#Evapotranspiration 
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_12_sep.tif",format="GTiff",overwrite=TRUE)

extract(x=ET.24,y=est@coords)

#####################################
#####Lectura de MTL landsat 8 13-Ago
#####################################

csvfile <- read.csv("ET_remote_sensing/Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile

MTLfile <- "Articulo_ET/Landsat/DN/13-ago-2017/LC08_L1TP_006068_20170807_20170813_01_T1/LC08_L1TP_006068_20170807_20170813_01_T1_MTL.txt"

#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.860133, long= -75.397031, elev=3304, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              cf=c(1,1,1),
                              MTL = MTLfile,
                              tz="America/Lima")

#Cargar imagen
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/13-ago-2017/LC08_L1TP_006068_20170807_20170813_01_T1", sat = "L8")
imageL8 <- crop(imageL8,aoi)

###Cargar MDE
dem <- raster("ET_remote_sensing/MDE/MDE_mosaic.tif")
dem <- crop(x=dem,y=aoi)
dem <- resample(dem, imageL8$B, method="bilinear")

######### Inicia METRIC
#All coefficients
surface.model <-METRICtopo(dem)
solar.angles.r <- solarAngles(surface.model = surface.model, 
                              WeatherStation = WeatherStation, MTL = MTLfile)
Rs.inc <- incSWradiation(surface.model = surface.model, 
                         solar.angles = solar.angles.r, 
                         WeatherStation = WeatherStation)
image.TOAr <- calcTOAr(image.DN = imageL8, sat="L8", MTL = MTLfile, 
                       incidence.rel = solar.angles.r$incidence.rel)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/13-ago-2017/LC080060682017080701T1-SC20180714004416",
  aoi=aoi)

#Momentum Roughness Length
lai <- LAI(method = "metric2010",image=image.TOAr,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)
rm(lai,ndvi,alb)

#Energy Balance
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=2, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "metric2010", 
                            Z.om.ws = extract(x=mom,y=est[8,]),
                            MTL=MTLfile)
rm(mom,Rs.inc,solar.angles.r,surface.model)

#Evapotranspiration 
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_13_ago.tif",format="GTiff",overwrite=TRUE)

extract(x=ET.24,y=est@coords)
est$Estacion







#####################################
#####Lectura de MTL landsat 8 28-julio
#####################################

csvfile <- read.csv("ET_remote_sensing/Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile

MTLfile <- "Articulo_ET/Landsat/DN/28-jul-2017/LC08_L1TP_006068_20170722_20170728_01_T1/LC08_L1TP_006068_20170722_20170728_01_T1_MTL.txt"

#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.860133, long= -75.397031, elev=3304, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              cf=c(1,1,1),
                              MTL = MTLfile,
                              tz="America/Lima")

#Cargar imagen
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/28-jul-2017/LC08_L1TP_006068_20170722_20170728_01_T1", sat = "L8")
imageL8 <- crop(imageL8,aoi)

###Cargar MDE
dem <- raster("ET_remote_sensing/MDE/MDE_mosaic.tif")
dem <- crop(x=dem,y=aoi)
dem <- resample(dem, imageL8$B, method="bilinear")

######### Inicia METRIC
#All coefficients
surface.model <-METRICtopo(dem)
solar.angles.r <- solarAngles(surface.model = surface.model, 
                              WeatherStation = WeatherStation, MTL = MTLfile)
Rs.inc <- incSWradiation(surface.model = surface.model, 
                         solar.angles = solar.angles.r, 
                         WeatherStation = WeatherStation)
image.TOAr <- calcTOAr(image.DN = imageL8, sat="L8", MTL = MTLfile, 
                       incidence.rel = solar.angles.r$incidence.rel)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/28-jul-2017/LC080060682017072201T1-SC20180714004431",
  aoi=aoi)

#Momentum Roughness Length
lai <- LAI(method = "metric2010",image=image.TOAr,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)
rm(lai,ndvi,alb)

#Energy Balance
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=2, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "metric2010", 
                            Z.om.ws = extract(x=mom,y=est[8,]),
                            MTL=MTLfile)
rm(mom,Rs.inc,solar.angles.r,surface.model)

#Evapotranspiration 
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_28_jul.tif",format="GTiff",overwrite=TRUE)

extract(x=ET.24,y=est@coords)
est$Estacion




#####################################
#####Lectura de MTL landsat 8 16-junio
#####################################

csvfile <- read.csv("ET_remote_sensing/Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile

MTLfile <- "Articulo_ET/Landsat/DN/16-junio-2017/LC08_L1TP_006068_20170604_20170616_01_T1/LC08_L1TP_006068_20170604_20170616_01_T1_MTL.txt"

#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.860133, long= -75.397031, elev=3304, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              cf=c(1,1,1),
                              MTL = MTLfile,
                              tz="America/Lima")

#Cargar imagen
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/16-junio-2017/LC08_L1TP_006068_20170604_20170616_01_T1", sat = "L8")
imageL8 <- crop(imageL8,aoi)

###Cargar MDE
dem <- raster("ET_remote_sensing/MDE/MDE_mosaic.tif")
dem <- crop(x=dem,y=aoi)
dem <- resample(dem, imageL8$B, method="bilinear")

######### Inicia METRIC
#All coefficients
surface.model <-METRICtopo(dem)
solar.angles.r <- solarAngles(surface.model = surface.model, 
                              WeatherStation = WeatherStation, MTL = MTLfile)
Rs.inc <- incSWradiation(surface.model = surface.model, 
                         solar.angles = solar.angles.r, 
                         WeatherStation = WeatherStation)
image.TOAr <- calcTOAr(image.DN = imageL8, sat="L8", MTL = MTLfile, 
                       incidence.rel = solar.angles.r$incidence.rel)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/16-jun-2017/LC080060682017060401T1-SC20180714004355",
  aoi=aoi)

#Momentum Roughness Length
lai <- LAI(method = "metric2010",image=image.TOAr,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)
rm(lai,ndvi,alb)

#Energy Balance
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=2, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "metric2010", 
                            Z.om.ws = extract(x=mom,y=est[8,]),
                            MTL=MTLfile)
rm(mom,Rs.inc,solar.angles.r,surface.model)

#Evapotranspiration 
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_16_junio.tif",format="GTiff",overwrite=TRUE)

extract(x=ET.24,y=est@coords)
est$Estacion




