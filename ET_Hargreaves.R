# Modify from ___ https://github.com/micahwoods/ETo/blob/master/server.R
# And http://www.scielo.br/scielo.php?script=sci_arttext&pid=S0102-77862012000300002

meteoro <- read.csv(file.choose(),header = TRUE)
meteoro$date <- as.Date(meteoro$date,format='%d/%m/%Y')
library("lubridate")

avgTemp <- (meteoro$T_Max + meteoro$T_Min) / 2

#Constantes
k1 <- (24*60)/pi   
solar.constant <- 0.0820  #MJ/m^2min
day.of.year <- yday(meteoro$date) 
    
# inverse relative distance Earth-Sun, d-sub-r and solar declination
inverse.distance <- 1 + 0.033 * cos(((2 * pi) / 365) * day.of.year)
solar.declination <- 0.409 * sin((((2 * pi) / 365) * day.of.year) - 1.39)
latitudeRadians <- (pi/180) * meteoro$Latitud 
sunset.hour.angle <- acos(-tan(latitudeRadians) * tan(solar.declination))
    
# Ra, extraterrestrial irradiance
Ra <- ((k1 * solar.constant) * inverse.distance) *
      (sunset.hour.angle * sin(latitudeRadians) * sin(solar.declination) +
         cos(latitudeRadians) * cos(solar.declination) * sin(sunset.hour.angle))
    
# Ra expressed in equivalent evaporation
RaMm <- Ra * 0.408
    
ETo <- 0.0023 * (avgTemp + 17.8) *
      (meteoro$T_Max - meteoro$T_Min)^0.5 * RaMm


meteoro["ETo"] <- ETo
meteoro$Rad_ext <- Ra

write.csv(x = meteoro,file = "/home/carlos/Documentos/Quinua SmartApp/et_met.csv")


####Camargo 1971 method
f <- 0.01

ND <- 1

ETo_camargo <- f*avgTemp*RaMm*ND
ETo_camargo
