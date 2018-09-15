# Copy from ___ https://github.com/micahwoods/ETo/blob/master/server.R

meteoro <- read.csv(file.choose(),header = TRUE)
library("lubridate")

avgTemp <- (meteoro$T_Max + meteoro$T_Min) / 2

#Constantes
k1 <- (24*60)/pi   
solar.constant <- 0.0820  
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
