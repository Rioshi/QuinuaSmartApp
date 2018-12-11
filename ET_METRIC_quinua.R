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
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/04-Jun-2017/LC08_L1TP_006068_20170604_20170616_01_T1/LC08_L1TP_006068_20170604_20170616_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 07-ago
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/07-Ago-2017/LC08_L1TP_006068_20170807_20170813_01_T1/LC08_L1TP_006068_20170807_20170813_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 22-jul
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/22-Jul-2017/LC08_L1TP_006068_20170722_20170728_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 23-ago
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/23-Ago-2017/LC08_L1TP_006068_20170823_20170912_01_T1/LC08_L1TP_006068_20170823_20170912_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 26-oct
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/26-Oct-2017/LC08_L1TP_006068_20171026_20171107_01_T1/LC08_L1TP_006068_20171026_20171107_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 13-Dic
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/13-Dic-2017/LC08_L1TP_006068_20171213_20171223_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 08-sep
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/08-Sep-2017/LC08_L1TP_006068_20170908_20170927_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 06-jul
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/06-jul-2017/LC08_L1TP_006068_20170706_20170716_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 20-jun
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/20-Jun-2017/LC08_L1TP_006068_20170620_20170630_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 19-may
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/19-May-2017/LC08_L1TP_006068_20170519_20170525_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 03-may
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/03-May-2017/LC08_L1TP_006068_20170503_20170515_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 17-abr
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/17-Abr-2017/LC08_L1TP_006068_20170417_20170501_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 16-mar
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/16-Mar-2017/LC08_L1TP_006068_20170316_20170328_01_T1_MTL.txt"
#####Lectura de MTL landsat 8 || 28-feb
MTLfile <- "D:/QuinuaSmartApp/Articulo_ET/Landsat/DN/28-Feb-2017/LC08_L1TP_006068_20170228_20170316_01_T1_MTL.txt"




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
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/13-Dic-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/08-Sep-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/06-jul-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/20-Jun-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/19-May-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/03-May-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/17-Abr-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/16-Mar-2017", sat = "L8",aoi)
imageL8 <- loadImage(path = "Articulo_ET/Landsat/DN/28-Feb-2017", sat = "L8",aoi)






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
  path="Articulo_ET/Landsat/Reflectancia/26-Oct-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/13-Dic_2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/08-Sep-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/06-jul-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/20-jun-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/19-may-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/03-may-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/17-Abr-2017",
  aoi=aoi)

image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/16-Mar-2017",
  aoi=aoi)
image.SR <- loadImageSR(
  path="Articulo_ET/Landsat/Reflectancia/28-Feb-2017",
  aoi=aoi)



############CORTAR NUBESS

 

cmsk <- cloudMask(imageL8, threshold = .4, blue = 1, tir = 8)
imageL8 <- mask(imageL8, cmsk$CMASK, maskvalue = 1)
image.SR <- mask(image.SR, cmsk$CMASK, maskvalue = 1)
values(imageL8)[values(imageL8) == 0] = NA
values(image.SR)[values(image.SR) == 0] = NA






#Cargar todas las estaciones
est <- read.csv("ET_remote_sensing/Meteorologia/estaciones.csv",header = TRUE)
coordinates(est) = ~X+Y
proj4string(est) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
est <- spTransform(est, crs(imageL8))

#Cortar el area de trabajo
#imageL8 <- crop(imageL8,aoi)

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
                            LAI.method = "metric2010",anchors.method = "flexible", 
                            Z.om.ws = extract(x=mom,y=est[7,]),
                            MTL=MTLfile)
 end_time <- Sys.time()
end_time - start_time




ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)
extract(x=ET.24,y=est@coords)


writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/04_jun.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/07_ago.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/22_jul.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/23_ago.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/26_oct.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/13_dic.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/08_sep.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/06_jul.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/20_jun.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/19_may.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/03_may.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/17_abr.tif",format="GTiff",overwrite=TRUE)
writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/16_mar.tif",format="GTiff",overwrite=TRUE)

writeRaster(ET.24$layer,filename = "Articulo_ET/Rasters/ET_METRIC/28_feb.tif",format="GTiff",overwrite=TRUE)



##Extraer datos de ETa
et.04.jun <- raster("Articulo_ET/Rasters/ET_METRIC/04_jun.tif")
et.07.ago <-raster("Articulo_ET/Rasters/ET_METRIC/07_ago.tif")
et.22.jul <- raster("Articulo_ET/Rasters/ET_METRIC/22_jul.tif")
et.23.ago <- raster("Articulo_ET/Rasters/ET_METRIC/23_ago.tif")
et.26.oct <- raster("Articulo_ET/Rasters/ET_METRIC/26_oct.tif")

et.13.dic <- raster("Articulo_ET/Rasters/ET_METRIC/13_dic.tif")
et.08.sep <- raster("Articulo_ET/Rasters/ET_METRIC/08_sep.tif")

et.06.jul <- raster("Articulo_ET/Rasters/ET_METRIC/06_jul.tif")
et.20.jun <- raster("Articulo_ET/Rasters/ET_METRIC/20_jun.tif")
et.19.may <- raster("Articulo_ET/Rasters/ET_METRIC/19_may.tif")
et.03.may <- raster("Articulo_ET/Rasters/ET_METRIC/03_may.tif")
et.17.abr <- raster("Articulo_ET/Rasters/ET_METRIC/17_abr.tif")
et.16.mar <- raster("Articulo_ET/Rasters/ET_METRIC/16_mar.tif")
et.28.feb <- raster("Articulo_ET/Rasters/ET_METRIC/28_feb.tif")



nombr <- est@data$Estacion
jun.04 <- extract(x=et.04.jun,y=est@coords)
ago.07 <- extract(x=et.07.ago,y=est@coords)
jul.22 <- extract(x=et.22.jul,y=est@coords)
ago.23 <- extract(x=et.23.ago,y=est@coords)
oct.26 <- extract(x=et.26.oct,y=est@coords)

dic.13 <- extract(x=et.13.dic,y=est@coords)
sep.08 <- extract(x=et.08.sep,y=est@coords)
jul.06 <- extract(x=et.06.jul,y=est@coords)
jun.20 <- extract(x=et.20.jun,y=est@coords)
jun.04 <- extract(x=et.04.jun,y=est@coords)
may.19 <- extract(x=et.19.may,y=est@coords)
may.03 <- extract(x=et.03.may,y=est@coords)
abr.17 <- extract(x=et.17.abr,y=est@coords)
mar.16 <- extract(x=et.16.mar,y=est@coords)
feb.28 <- extract(x=et.28.feb,y=est@coords)


todo <- data.frame(nombr,jun.04,ago.07,jul.22,ago.23,oct.26,dic.13,sep.08,jul.06,jun.20,
                   jun.04,may.19,abr.17,mar.16,feb.28)
write.csv(todo,file="C:/Users/Carlos/Documents/Provicional/met.csv")


#DISTANCIA ENTRE ESTACIONES

pointDistance(p1=est[8,], p2=est[2,], lonlat=TRUE)
