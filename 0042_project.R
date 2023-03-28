####################################
############Chapter 0###############
####################################

####------0.1 Importing data------
volume01 <-  read.delim("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/station_day/d03_text_station_hour_2023_01.txt",
                        sep=",", header=FALSE)
#pick freeway 5
volume01_fwy5 <- volume01[volume01$V4 == '5',]
#pick 1 week
week<-c('01/23/2023','01/24/2023','01/25/2023','01/26/2023','01/27/2023','01/28/2023','01/29/2023')

volume01_fwy5_23 <- volume01_fwy5[grepl(week[1],volume01_fwy5$V1),]
volume01_fwy5_24 <- volume01_fwy5[grepl(week[2],volume01_fwy5$V1),]
volume01_fwy5_25 <- volume01_fwy5[grepl(week[3],volume01_fwy5$V1),]
volume01_fwy5_26 <- volume01_fwy5[grepl(week[4],volume01_fwy5$V1),]
volume01_fwy5_27 <- volume01_fwy5[grepl(week[5],volume01_fwy5$V1),]
volume01_fwy5_28 <- volume01_fwy5[grepl(week[6],volume01_fwy5$V1),]
volume01_fwy5_29 <- volume01_fwy5[grepl(week[7],volume01_fwy5$V1),]


# 1 hour as a time unit -> 7days -> 168 columns

station_meta<- read.delim("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/station_meta/d03_text_meta_2023_03_01.txt")
station_fwy5 <- station_meta[station_meta$'Fwy'=="5",]
station_fwy5 <- station_fwy5[order(station_fwy5$ID),]


library(dplyr)
volume_df<-station_fwy5 %>% select('ID','Dir','Type','Length','Latitude','Longitude')

for(i in 1:7){
  for(j in 1:24){
    col_name <- paste('t',i,'_',j-1,sep="")
    volume_df[col_name]<-1
  }
}

#insert the traffic volume into dataframe based on station id
#1/23
volume01_fwy5_23 <- volume01_fwy5_23 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+6] = volume01_fwy5_23[volume01_fwy5_23$V2==volume_df$ID[i],3][j]
  }
}

#1/24
volume01_fwy5_24 <- volume01_fwy5_24 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+30] = volume01_fwy5_24[volume01_fwy5_24$V2==volume_df$ID[i],3][j]
  }
}


#1/25
volume01_fwy5_25 <- volume01_fwy5_25 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+54] = volume01_fwy5_25[volume01_fwy5_25$V2==volume_df$ID[i],3][j]
  }
}


#1/26
volume01_fwy5_26 <- volume01_fwy5_26 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+78] = volume01_fwy5_26[volume01_fwy5_26$V2==volume_df$ID[i],3][j]
  }
}


#1/27
volume01_fwy5_27 <- volume01_fwy5_27 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+102] = volume01_fwy5_27[volume01_fwy5_27$V2==volume_df$ID[i],3][j]
  }
}


#1/28
volume01_fwy5_28 <- volume01_fwy5_28 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+126] = volume01_fwy5_28[volume01_fwy5_28$V2==volume_df$ID[i],3][j]
  }
}



#1/29
volume01_fwy5_29 <- volume01_fwy5_29 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume_df[i,j+150] = volume01_fwy5_29[volume01_fwy5_29$V2==volume_df$ID[i],3][j]
  }
}

#Do I need replace NA with 0?
volume_df[is.na(volume_df)] <- 0





####################################
############Chapter 1###############
####################################
library(scatterplot3d)
library(plot3D)
library(rgl)
volume_matrix<-data.matrix(volume_df[,7:ncol(volume_df)])

####------1.1 Examining non spatio-temporal data characteristics------
mu = mean(volume_matrix)
mu

sdev = sd(volume_matrix)
sdev


hist(volume_matrix)
abline(v=mu, col="red")

qqnorm(volume_matrix)
qqline(volume_matrix, col="red")


pairs(~Latitude+Longitude+Length+rowMeans(volume_matrix),data=volume_df,
      main="Simple Scatterplot Matrix")


#1. Using Scatterplot3d
scatterplot3d(x=volume_df$Latitude, y=volume_df$Longitude, z=rowMeans(volume_matrix))

#2. Using plot3D (x,y and z are first three variables so no need to # explicitly define them).
#scatter3D(temp$LAT, temp$ALT, rowMeans(temp_matrix))

# Need to see if this can be interactive in web-version
plot3d(volume_df$Latitude, volume_df$Longitude, rowMeans(volume_matrix))



####--------1.2 Examining temporal characteristics------
library(reshape)
library(lattice)

plot(colMeans(volume_matrix), xlab = "Days", ylab = "Traffic Volume", type="l", xaxt="n")
axis(1, at = seq(1, 168, 24), labels=seq(1, 7, 1))


newvolume <- melt(volume_df, id.vars=1:6, measure.vars=7:ncol(volume_df))

colnames(newvolume)[7:8] <- c("Hour", "volume") 


station.chosen=c("311832","311844","311847","311864","312132","312133",
                 "312134","313159","314652","314780")
#Create a variable containing just the selected stations
a <- newvolume[newvolume$ID %in% station.chosen,]

xyplot(volume ~ Hour | ID, xlab = "Hour", type = "l",
       layout = c(5, 2),
       data=a,
       main = "Traffic volume")


#Create the heatmap:
heatmap(volume_matrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column", 
        margins=c(5,3),xlab="Hour",ylab="Station ID", cexCol=1.1,y.scale.components.subticks(n=10))


volume_order<-volume_df[order(volume_df$Latitude, decreasing=TRUE),]
volume_ordermatrix<-data.matrix(volume_order[,7:ncol(volume_df)])
heatmap(volume_ordermatrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column",margins=c(3,3))

levelplot(t(volume_ordermatrix), aspect="fill")


####--------1.3 Examining spatial characteristics------
library(gstat)
install.packages('terra')
library(terra)
install.packages('raster')
library(raster)
library(ggplot2)
install.packages('OpenStreetMap')
library(OpenStreetMap)

data_last<-cbind(volume_df[1:6],volume_df$"t7_23")

# Change the column name:
colnames(data_last)[7]<-"volume"
# Make a proportional symbol of the latest data. Convert latitude and longitude to Mercator projection:
data_last[,5:6] <-projectMercator(data_last$Latitude, data_last$Longitude)
# Download a map tile:
map <- openmap(c(39.92778,-123.28275),c(38.07320,-120.91692),type= 'osm')


autoplot.OpenStreetMap(map)+ 
  geom_point(data=data_last, aes(x=Latitude,y=Longitude, color=volume, size=volume))+ 
  ggtitle("Hourly traffic volume in Freeway5, 29/01/2023 ")


hist(data_last$Length,15)

#not very normally distributed
hist(log(data_last$Length), 15)

autoplot.OpenStreetMap(map)+ 
  geom_point(data = data_last, aes(x=Latitude,y=Longitude, color=volume, size=log(Length)))+
  ggtitle("Hourly traffic volume in Freeway5, 29/01/2023")+
  scale_colour_gradient2(low="blue", high="red")

hist(rowMeans(volume_matrix), 20)

high_volume <- which(rowMeans(volume_matrix)>3000)
data_last <- cbind(data_last, "Low", stringsAsFactors = FALSE)
colnames(data_last)[ncol(data_last)] <- "VolumeGroup"
data_last[high_volume,"VolumeGroup"] <- "High"

autoplot.OpenStreetMap(map) + 
  geom_point(data = data_last, aes(x = Latitude, y = Longitude, color = VolumeGroup, size = log(Length)))+ 
  ggtitle("Stations with High/Low volume")

#multiple time period

newvolume[,5:6] <- projectMercator(newvolume$Latitude, newvolume$Longitude)

# Select hours:consider to choose hours that can show the difference between peak-time and off-peak time
hours <- c('t1_0','t1_1','t1_2','t1_3','t1_4','t1_5','t1_6','t1_7','t1_8','t1_9'
           ,'t1_10','t1_11','t1_12','t1_13','t1_14','t1_15')
autoplot.OpenStreetMap(map) + geom_point(data=newvolume[newvolume$Hour %in% hours,], aes(x=Latitude,y=Longitude, color=volume, size=log(Length))) + facet_wrap(facets=~Hour) +
  scale_colour_gradient2(low="blue", high="red")
#volume difference

# Calculate difference in volume from hour to hour (first year is zero as there is no preceding year)
volumediff <- cbind(0,volume_df[,8:ncol(volume_df)]-volume_df[,7:(ncol(volume_df)-1)])
newvolume<- cbind(newvolume, unlist(volumediff))
colnames(newvolume)[ncol(newvolume)] <- "volumediff"

autoplot.OpenStreetMap(map) + 
  geom_point(data=newvolume[newvolume$Hour %in% hours,], aes(x=Latitude,y=Longitude, color=volumediff, size=log(Length))) + 
  scale_colour_gradient2(low="blue", mid='white', high="red") + facet_wrap(facets=~Hour)










####################################
############Chapter 2###############
####################################
# Load the required libraries
library(maptools)
library(lattice)
library(spdep)
library(sp)
library(rgdal)
library(tmap)
library(ggplot2)
library(gridExtra)
library(gstat)
library(OpenStreetMap)
library(spacetime)

####----2.2 Mapping the data: ----
#Since it's point data for one freeway, it's hard to aggregate the data into certain output area
#what method can I use?


####----2.3 The concept of lagged variables----
MeanVolume <- colMeans(volume_matrix)

VoLagged <- data.frame(hour = 1:167, t=MeanVolume[2:(length(MeanVolume))], t_minus_1=MeanVolume[1:(length(MeanVolume)-1)])

p1 <- ggplot(VoLagged , aes(x=hour, y=t)) + geom_line()
p2 <- ggplot(VoLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  ggplot2::annotate("text", 8.5, 10, label=paste("r =", round(cor(VoLagged$t, VoLagged$t_minus_1), 3))) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)
cor(VoLagged$t, VoLagged$t_minus_1)

####----2.4 Calculating Temporal autocorrelation----
acf(MeanVolume)

acf(volume_matrix[2,], lag.max=72, main="ACF, station 311844")

pacf(volume_matrix[2,], lag.max=72, main="ACF, station 311844")

####----2.5 Measuring spatial autocorrelation----
#maybe,split the data into N and S
coords = list(projectMercator(volume_df[,5], volume_df[,6]))
plot(variogram(list(rowMeans(volume_matrix)), locations=coords))

plot(variogram(list(rowMeans(volume_matrix)), 
               locations=coords, alpha=c(0,45,90,135,180,270)))


volume_N <- volume_df[volume_df$Dir=="N",]
volume_S <- volume_df[volume_df$Dir=="S",]

volume_matrixN <- data.matrix(volume_N[,7:ncol(volume_N)])
volume_matrixS <- data.matrix(volume_S[,7:ncol(volume_S)])

coordsN = list(projectMercator(volume_N[,5], volume_N[,6]))
plot(variogram(list(rowMeans(volume_matrixN)), locations=coordsN))

coordsS = list(projectMercator(volume_S[,5], volume_S[,6]))
plot(variogram(list(rowMeans(volume_matrixS)), locations=coordsS))


####----2.6 Spatio-temporal Autocorrelation----
source("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/starima_package.R")


####----2.6.2 The space-time semivariogram----
library(timeDate)
# Project the points to get spatial lags in metres
pts1 <- SpatialPoints(volume_df[,5:6], 
                     proj4string=CRS("+init=epsg:4326 +proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs +towgs84=0,0,0"))

# Years are converted to date format
time1 <- seq(as.timeDate("2023-01-23 00:00"), length = 168, by = "hour")
stfdf1 <- STFDF(pts1, time1, data.frame(as.vector(t(volume_matrix))))
names(stfdf1@data) <- "Volume"


# Calculate ST-semivariogram with ? what should be the appropriate 
VolSTVar <- variogram(Volume~1, stfdf1, width=5, cutoff=100,tlags=0:12)
plot(VolSTVar)
plot(VolSTVar, wireframe=T)





####################################
############Chapter 3###############
####################################
#Statistical modelling of time series and Spatio-temporal series

####----3.2.1 Stationarity and trends----
library(forecast)
library(ggplot2)
library(fpp2)
library(gridExtra)

#all stations rows
volume_t <- t(volume_matrix)
volume_ts <- ts(data = volume_t,start=c(1,172),end=c(168,172))
autoplot(volume_ts)


#using column mean


###----3.2.2 Time series decomposition-----
volume_ts2 <- ts(data = volume_t,start=c(1,172),end=c(168,172),frequency=2)
decom <- stl(volume_ts2[,5], t.window=14, s.window="periodic")
autoplot(decom)



###----3.2.3 Differencing-----
#all rows
autoplot(volume_ts)
autoplot(diff(ts(volume_ts)))

#column means
p1<-autoplot(ts(MeanVolume,1,168,1))
p2<-autoplot(diff(ts(MeanVolume,1,168,1)))
grid.arrange(p1,p2)




p1 <- autoplot(acf(ts(MeanVolume,1,168,1),plot=FALSE))
p2 <- autoplot(pacf(ts(MeanVolume,1,168,1),plot=FALSE))
p3 <- autoplot(acf(diff(ts(MeanVolume,1,168,1)),plot=FALSE)) 
p4 <- autoplot(pacf(diff(ts(MeanVolume,1,168,1)),plot=FALSE))
grid.arrange(p1,p2, p3, p4)


###----3.2.4 Seasonal Differencing-----
p1 <- autoplot(ts(MeanVolume,1,168,1))
p2 <- autoplot(diff(ts(MeanVolume,1,168,1)))
p3 <- autoplot(diff(ts(MeanVolume,1,168,1), lag=24))

grid.arrange(p1,p2,p3)


###----3.3 The ARIMA modelling framework----


#practical example
library(rgdal)
uk_districts <- readOGR(dsn="/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/uk_districts.shp", layer="uk_districts", 
                        p4s = CRS("+proj=tmerc +lat_0=49 +lon_0=-2 +k=0.9996012717 +x_0=400000 +y_0=-100000 +ellps=airy +datum=OSGB36 +units=m +no_defs")@projargs)

uk_temp_matrix<-data.matrix(uk_districts@data[,-c(1:3)])
rownames(uk_temp_matrix) <- uk_districts@data[,"NAME"]




rownames(volume_matrix) <- paste("sta",1:nrow(volume_matrix),sep = "")
plot(volume_matrix["sta5",],ylab="Hourly average volume",xlab="Time (in hours)",type="l")

lag.plot(volume_matrix["sta5",], lags=3, do.lines=FALSE)



fit.ar <- arima(volume_matrix["sta5",],
                order=c(1,0,2),seasonal=list(order=c(2,1,1),period=24))
fit.ar


source("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/starima_package.R")
NRMSE_fit <- NRMSE(res=fit.ar$residuals, obs=volume_matrix["sta5",])


NRMSE_fit

tsdiag(fit.ar)


pre.ar<-predict(fit.ar, n.ahead=24)
matplot(1:24,cbind(volume_matrix["sta5",145:168],pre.ar$pred),type="l",main="", xlab="Hour", ylab="Average Volume")


fit.Ar <- Arima(volume_matrix["sta5",1:144],order=c(1,0,2),seasonal=list(order=c(2,1,1),period=24))
pre.Ar <- Arima(volume_matrix["sta5", ], model=fit.Ar)
matplot(cbind(pre.Ar$fitted, pre.Ar$x), type="l")



###----3.4.6 Summary of ARIMA modelling framework----

fit.auto.ar <- auto.arima(volume_matrix["sta5",1:144])
fit.auto.ar


#Fitting an auto.arima model in R using the Forecast package
pre.auto.ar<-forecast(fit.auto.ar)

matplot(cbind(pre.auto.ar$fitted, pre.auto.ar$x), type="l")



#starima



options("rgdal_show_exportToProj4_warnings"="none")

library(spNetwork)
library(sf)
library(dplyr)
library(spdep)


#starima on traffic volume


#1. weight matrix
#find nearest neighbourhood
#since the study area is one freeway, finding the 1 nearest neighbour allow each station connects
#to each other, preenting aong the freeway
coords1<-cbind(volume_df$Latitude,volume_df$Longitude)

knn1<-knearneigh(coords1,k=1)
str(knn1)

#convert to neighbour list
k1 <- knn2nb(knn1)

critical.threshold <- max(unlist(nbdists(k1,coords1)))
critical.threshold

#通过nearest1 neighbour list，找出critical threshold（by maximum distance）
#再用dnearneighbour找出整个area之间的distance based weight matrix
nb.dist.band <- dnearneigh(coords1, 0, critical.threshold)
summary(nb.dist.band)

#connectivity histogram
dist.band.card <- card(nb.dist.band)
dist.band.card

ggplot() +
  geom_histogram(aes(x=dist.band.card)) +
  xlab("Number of Neighbors")

#connectivity graph
plot(nb.dist.band, coords1, lwd=.2, col="blue", cex = .5)

w <- nb2mat(nb.dist.band,style='W')
w_list<-nb2listw(nb.dist.band)

source()
#2. Space-time autocorrelation and partial autocorrelation analysis
source("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/starima_package.R")


volume_ts.stacf<-stacf(volume_ts,w,72)
volume_ts.diff <- diff(volume_ts,lag=72,diff=1)
stacf(volume_ts.diff,w)
stpacf(volume_ts,w,72)
stpacf(volume_ts.diff,w)


#model identification

#after 3 lags ->q=3
#after 3 lags ->p=3
#d=1 since no significant peak
test<-rbind(volume_ts,volume_ts,volume_ts,volume_ts,volume_ts,volume_ts)
source("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/starima_package.R")
list(w1=w)
fit.star <- starima_fit(volume_ts[1:72,],list(w1=w),p=3,d=10,q=3)
#diagnostic checking
stacf(fit.star$RES,w)
hist(fit.star$RES[,6])


pre.star <- starima_pre(volume_ts[(72-10-6+1):168,], model=fit.star)

#这些时间里，第10个station的prediction情况
matplot(1:96,cbind(volume_ts[73:168,10],(pre.star$PRE[,10])),type='l')

#normalised mean squared value
pre.star$NRMSE





