##### EVAPOTRANSPIRACION USANDO METODO METRIC
##### paquete "water"

library(water)
library(RStoolbox)
setwd("C:/Users/ULima/Documents/Proyectos_R_Carlos/ET_remote_sensing")
#Delimitacion area de interes "aoi"

aoi <- createAoi(topleft = c(456600,-1310000), 
                 bottomright = c(459200,-1312400), EPSG = 32618)

#Lectura de datos estacion meteorologica
csvfile <- read.csv("Meteorologia/IRD_sierra.csv",
                    header = TRUE,na.strings=c("---"))
csvfile <- na.omit(csvfile)
csvfile <- csvfile[,c(1:3,6,8,18,20)] #short cvfile

#Lectura de MTL landsat 8
MTLfile <- "Landsat/Example/LC08_L1TP_006068_20171026_20171107_01_T1_MTL.txt"

#Lectura y graficado de datos meteorologicos
WeatherStation <- read.WSdata(WSdata = csvfile, date.format = '%d/%m/%Y',
                              time.format='%H:%M',lat=-11.859958, long= -75.395708, elev=3309, height= 2.2,
                              columns=c("date" = 1, "time" = 2, "radiation" = 7,
                                        "wind" = 5, "RH" = 4, "temp" = 3, "rain" = 6), 
                              MTL = MTLfile)

plot(WeatherStation,hourly=TRUE) #sat=FALSE,date=...

#Carga de imagenes satelitales
imageL8 <- loadImage(path = "Landsat/Example", sat = "L8",aoi)
imageL8 <- crop(imageL8,aoi)
###Cargar MDE
#checkSRTMgrids(imageL8)
dem <- raster("MDE/s12_w076_1arc_v3.tif")
dem <- projectRaster(dem,crs=crs(imageL8))
dem <- crop(x=dem,y=aoi)
dem <- resample(dem, imageL8$B, method="bilinear")
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

image.SR <- calcSR(image.TOAr=image.TOAr, sat = "L8", 
                   surface.model=surface.model, 
                   incidence.hor = solar.angles.r$incidence.hor, 
                   WeatherStation=WeatherStation)

albedo <- albedo(image.SR = image.SR,  coeff="Tasumi", sat="L8")


plot(imageL8$B, col = "grey")
plot(dem, add = TRUE)
