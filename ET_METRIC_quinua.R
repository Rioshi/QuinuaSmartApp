library(water)
library(RStoolbox)
library(doParallel)
library(foreach)
setwd("D:/QuinuaSmartApp")

#Lectura de datos estacion meteorologica
csvfile <- read.csv("ET_remote_sensing/Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile


#aoi <- createAoi(topleft = c(434738.4,-1234477), 
#                 bottomright = c(555528.7,-1385039), EPSG = 32618)
aoi <- createAoi(topleft = c(435929,-1269644), 
                 bottomright = c(558392.9,-1384533), EPSG = 32618)
                 

#####################################
##LECTURA DE LANDSAT
#####################################
#####Lectura de MTL landsat 8 || 04-jun
MTLfile <- "file:///D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/04-Jun-2017/LC08_L1TP_006068_20170604_20170616_01_T1/LC08_L1TP_006068_20170604_20170616_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 07-ago
MTLfile <- "file:///D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/07-Ago-2017/LC08_L1TP_006068_20170807_20170813_01_T1/LC08_L1TP_006068_20170807_20170813_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 22-jul
MTLfile <- "file:///D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/22-Jul-2017/LC08_L1TP_006068_20170722_20170728_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 23-ago
MTLfile <- "file:///D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/23-Ago-2017/LC08_L1TP_006068_20170823_20170912_01_T1/LC08_L1TP_006068_20170823_20170912_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 26-oct
MTLfile <- "file:///D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/26-Oct-2017/LC08_L1TP_006068_20171026_20171107_01_T1/LC08_L1TP_006068_20171026_20171107_01_T1_MTL.txt"



#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.860133, long= -75.397031, elev=3304, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              cf=c(1,1,1),
                              MTL = MTLfile,
                              tz="America/Lima")
#Cargar imagen
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/04-Jun-2017/LC08_L1TP_006068_20170604_20170616_01_T1", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/07-Ago-2017/LC08_L1TP_006068_20170807_20170813_01_T1", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/22-Jul-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/23-Ago-2017/LC08_L1TP_006068_20170823_20170912_01_T1", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/26-Oct-2017/LC08_L1TP_006068_20171026_20171107_01_T1", sat = "L8",aoi)


image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/04-jun-2017/LC080060682017060401T1-SC20181107130021",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/07-ago-2017/LC080060682017080701T1-SC20181107125749",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/22-jul-2017/LC080060682017072201T1-SC20181107130005",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/23-ago-2017/LC080060682017082301T1-SC20181107125734",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/16-jun-2017/LC080060682017060401T1-SC20180714004355",
  aoi=aoi)

############CORTAR NUBESS
cmsk = cloudMask(imageL8, threshold = .4, blue = 1, tir = 8)
wocl = mask(imageL8, cmsk$CMASK, maskvalue = NA)
plot(wocl$R)




values(imageL8)[values(imageL8) == 0] = NA

#Cargar todas las estaciones
est <- read.csv("ET_remote_sensing/Meteorologia/estaciones.csv",header = TRUE)
coordinates(est) = ~X+Y
proj4string(est) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
est <- spTransform(est, crs(imageL8))

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

#Momentum Roughness Length
lai <- LAI(method = "metric2010",image=image.TOAr,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)
rm(lai,ndvi,alb,image.TOAr)

#Energy Balance
sleep_for_a_minute <- function() { Sys.sleep(60) }

start_time <- Sys.time()
sleep_for_a_minute()
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=2, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "metric2010",anchors.method = "random", 
                            Z.om.ws = extract(x=mom,y=est[7,]),
                            MTL=MTLfile)
end_time <- Sys.time()
end_time - start_time



ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)
extract(x=ET.24,y=est@coords)


writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_12_sep.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_13_ago.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_28_jul.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/et_16_junio.tif",format="GTiff",overwrite=TRUE)

##Extraer datos de ETa
et.12.sep <- raster("Articulo_ET/Rasters/ET_METRIC/et_12_sep.tif")
et.13.ago <-raster("Articulo_ET/Rasters/ET_METRIC/et_13_ago.tif")
et.28.jul <- raster("Articulo_ET/Rasters/ET_METRIC/et_28_jul.tif")
et.16.jun <- raster("Articulo_ET/Rasters/ET_METRIC/et_16_junio.tif")

nombr <- est@data$Estacion
sep.12 <- extract(x=et.12.sep,y=est@coords)
ago.13 <- extract(x=et.13.ago,y=est@coords)
jul.28 <- extract(x=et.28.jul,y=est@coords)
jun.16 <- extract(x=et.16.jun,y=est@coords)

todo <- data.frame(nombr,sep.12,ago.13,jul.28,jun.16)
write.csv(todo,file="C:/Users/USER/Desktop/GianCarlo/todo.csv")
