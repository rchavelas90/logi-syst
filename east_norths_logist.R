# Haversine formula is appropriate for calculating distances from lat/long

EarthRad=6371000 ##R# [in meters]
haverDist<-function(aLong,aLat,bLong,bLat){
 dLat=2*pi*(bLat-aLat)/360.0; dLon=2*pi*(bLong-aLong)/360.0
 a=(sin(dLat/2))^2+cos(2*pi*aLat/360)*cos(2*pi*bLat/360)*(sin(dLon/2)^2)
 return(EarthRad*2*atan2(sqrt(a),sqrt(1-a)))
}

## Load CSV file with coordinates of 
df <- read.csv("locations.csv",check.names=FALSE,header=T,skip=0)
df
colnames(df)=c("Description","Latitude","Longitude")

# Calculate northings and eastings from location 0 to east and north coordinates
df$East=haverDist(df[1,"Longitude"],df[1,"Latitude"],df$Longitude,df[1,"Latitude"])*
 sign(df$Longitude-df[1,"Longitude"])
df$North=haverDist(df[1,"Longitude"],df[1,"Latitude"],df[1,"Longitude"],df$Latitude)*
 sign(df$Latitude-df[1,"Latitude"])
df$distFromWh <- sqrt(df$East^2+df$North^2)
df
# write a new CSV file with the data on it
#write.csv(df,file="locations2.csv")


# Map of locations
df
df2 <- data.frame(loc=numeric(nrow(df)),factor=numeric(nrow(df)))
df2$loc=paste(df$Latitude[1:nrow(df)],df$Longitude[1:nrow(df)],sep=":")
df2$factor <- df$Description
df2

library(googleVis)
g2 <- gvisMap(df2,"loc",
              options=list(mapType='normal',
                           useMapTypeControl=T,
                           showTip=TRUE,
                           enableScrollWheel=T))
plot(g2)

## Source http://www.movable-type.co.uk/scripts/latlong.html
