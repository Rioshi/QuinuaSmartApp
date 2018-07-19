##### EVAPOTRANSPIRACION USANDO METODO METRIC
##### paquete "water"

library(water)
library(RStoolbox)
setwd("C:/Users/ULima/Documents/Proyectos_R_Carlos/ET_remote_sensing")
#Delimitacion area de interes "aoi"

#aoi <- createAoi(topleft = c(456600,-1310000), 
#                 bottomright = c(459200,-1312400), EPSG = 32618)

aoi <- createAoi(topleft = c(408885,-1395315), 
                 bottomright = c(637515,-1162785), EPSG = 32618)

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

###Cargar MDE
#checkSRTMgrids(imageL8)
dem <- raster("MDE/s12_w076_1arc_v3.tif")
dem <- projectRaster(dem,crs=crs(imageL8))
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
lai <- LAI(method = "turner",image=image.SR,aoi = aoi)
ndvi <- (image.SR$NIR - image.SR$R)/(image.SR$NIR + image.SR$R)
alb <- albedo(image.SR, aoi, coeff="Olmedo",sat="L8")

mom <- momentumRoughnessLength(method = "short.crops",LAI = lai,NDVI=ndvi,albedo = alb,
                               mountainous = TRUE,surface.model = surface.model)

wpoint <- SpatialPoints(coords = WeatherStation$location[1,1:2])
proj4string(wpoint) <- CRS("+proj=longlat +datum=WGS84")
wpoint <- spTransform(wpoint, crs(mom))
extract(x=mom,y=wpoint)

#Energy Balance
Energy.Balance <- METRIC.EB(image.DN = imageL8,image.SR = image.SR,
                            plain = FALSE, DEM=dem,
                            aoi = aoi, n=5, WeatherStation = WeatherStation,
                            ETp.coef = 1,sat = "L8",alb.coeff = "Olmedo",LST.method = "SW",
                            LAI.method = "turner",Zom.method = "short.crops", Z.om.ws = 0.03,MTL=MTLfile)




extract(x=projectRaster(mom2,crs=crs("+proj=longlat +datum=WGS84")),y=matrix(data=WeatherStation$location[,1:2],nrow = 1,ncol = 2))

plot(Energy.Balance)

#Evapotranspiration 
ET_WS <- dailyET(WeatherStation = WeatherStation, MTL = MTLfile)
ET.24 <- ET24h(Rn = Energy.Balance$EB$NetRadiation, G=Energy.Balance$EB$SoilHeat, 
               H = Energy.Balance$EB$SensibleHeat, Ts=Energy.Balance$EB$surfaceTemperature, 
               WeatherStation = WeatherStation, ETr.daily=ET_WS)

###Ubicaciones estaciones
est <- read.csv("Meteorologia/estaciones.csv",header = TRUE)
proj4string(est) <- CRS("+proj=utm +zone=18 +south +ellps=WGS84 +datum=WGS84 +units=m +no_defs")
est <- spTransform(est, crs(imageL8))
coordinates(est) = ~X+Y
plot(est)
plot(imageL8$B)
plot(est,add=TRUE)
