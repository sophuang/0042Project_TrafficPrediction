####################################
########### Chapter 1 ##############
####################################
##############ESTDA#################

vol <- read.csv(file = '/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/traffic_volume_df_no.csv')
library(scatterplot3d)
library(plot3D)
library(rgl)
library(reshape)
library(lattice)
library(gstat)
install.packages('terra')
library(terra)
install.packages('raster')
library(raster)
library(ggplot2)
install.packages('OpenStreetMap')
library(OpenStreetMap)
####------1.1 Examining non spatio-temporal data characteristics------
vol_matrix<-data.matrix(vol[,7:ncol(vol)])

mu = mean(vol_matrix)
mu

sdev = sd(vol_matrix)
sdev

#Histogram of volume matrix
hist(vol_matrix)
abline(v=mu, col="red")


#1. Using Scatterplot3d
scatterplot3d(x=vol$Latitude, y=vol$Longitude, z=rowMeans(vol_matrix))


####--------1.2 Examining temporal characteristics------

plot(colMeans(vol_matrix), xlab = "Days", ylab = "Traffic Volume", type="l", xaxt="n")

axis(1, at = seq(1, 672, 24), labels=seq(1, 28, 1))


#for 10 chosen stations:
newvolume <- melt(vol, id.vars=1:6, measure.vars=7:ncol(vol))
colnames(newvolume)[7:8] <- c("Hour", "volume") 

station.chosen=c("311832","311844","311847","311864","312132","312133",
                 "312134","313159","314652","314780")

#Create a variable containing just the selected stations
a <- newvolume[newvolume$ID %in% station.chosen,]
name=c('detector1','detector2','detector3','detector4','detector5',
       'detector6','detector7','detector8','detector9','detector10')
xyplot(volume ~ Hour | name, xlab = "Hour", type = "l",
       layout = c(5, 2),
       data=a,
       main = "Traffic volume")


#Create the heatmap:
heatmap(vol_matrix,Rowv=NA,Colv=NA, col=heat.colors(256),scale="column", 
        margins=c(5,3),xlab="Hour",ylab="Station ID", cexCol=1.1,y.scale.components.subticks(n=10))


####--------1.3 Examining spatial characteristics------
data_last<-cbind(vol[1:6],vol$"t28_23")

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

hist(rowMeans(vol_matrix), 20)

high_volume <- which(rowMeans(vol_matrix)>3000)
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
hour <- c('t1_0','t1_4','t1_8','t1_12','t1_16','t1_20','t1_24')
          
autoplot.OpenStreetMap(map) + geom_point(data=newvolume[newvolume$Hour %in% hour,], aes(x=Latitude,y=Longitude, color=volume, size=log(Length))) + facet_wrap(facets=~Hour) +
  scale_colour_gradient2(low="blue", high="red")

####################################
############Chapter 2###############
####################################
# Autocorrelation Analysis
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
####----2.1 The concept of lagged variables----
MeanVolume <- colMeans(vol_matrix)

VoLagged <- data.frame(hour = 1:671, t=MeanVolume[2:(length(MeanVolume))], t_minus_1=MeanVolume[1:(length(MeanVolume)-1)])

p1 <- ggplot(VoLagged , aes(x=hour, y=t)) + geom_line()
p2 <- ggplot(VoLagged, aes(x=t, y=t_minus_1)) + 
  geom_point() + 
  labs(y="t-1") +
  geom_smooth(method="lm")+ # Add a regression line to the plot
  ggplot2::annotate("text", 8.5, 10, label=paste("r =", round(cor(VoLagged$t, VoLagged$t_minus_1), 3))) # Calculate PMCC

grid.arrange(p1,p2, nrow=1)
cor(VoLagged$t, VoLagged$t_minus_1)

####----2.2 Calculating Temporal autocorrelation----
acf(MeanVolume)

acf(vol_matrix[10,], lag.max=72, main="ACF, detector No.10")

pacf(vol_matrix[10,], lag.max=72, main="PACF, detector No.10")

####----2.3 Measuring spatial autocorrelation----

coords = list(projectMercator(vol[,"Latitude"], vol[,"Longitude"]))
plot(variogram(list(rowMeans(vol_matrix)), locations=coords))

plot(variogram(list(rowMeans(vol_matrix)), 
               locations=coords, alpha=c(0,45,90,135,180,270)))


####################################
############Chapter 3###############
####################################
#Statistical modelling of time series and Spatio-temporal series
library(spNetwork)
library(sf)
library(dplyr)
library(spdep)
options("rgdal_show_exportToProj4_warnings"="none")
coords1<-cbind(vol$Longitude,vol$Latitude)
knn1<-knearneigh(coords1,k=5)
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

#2. Space-time autocorrelation and partial autocorrelation analysis
source("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/starima_package.R")
vol_t <- t(vol_matrix)
vol_ts <- ts(data = vol_t,start=c(1,172),end=c(672,172))

vol_ts.stacf<-stacf(vol_ts,w,72)
vol_ts.diff <- diff(vol_ts,lag=72,diff=5)

library(tseries)
# Initialize a matrix to store the ADF test results
adf_results <- matrix(NA, ncol = ncol(vol_ts.diff), nrow = 2)
colnames(adf_results) <- colnames(vol_ts.diff)
rownames(adf_results) <- c("ADF statistic", "p-value")

# Perform the ADF test for each column (location) in test.diff
for (i in 1:ncol(vol_ts.diff)) {
  col_data <- vol_ts.diff[, i]
  adf_result <- adf.test(col_data)
  adf_results[1, i] <- adf_result$statistic
  adf_results[2, i] <- adf_result$p.value
}

# Print the ADF test results
print(adf_results)


stacf(vol_ts.diff,w)
stpacf(vol_ts,w,72)
stpacf(vol_ts.diff,w)


#model identification

#after 3 lags ->q=3
#after 3 lags ->p=6
#d=1 since no significant peak
source("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/Data/starima_package.R")
list(w1=w_test)
fit.star <- starima_fit(test[1:504,],list(w1=w_test),p=6,d=5,q=3)
#diagnostic checking
stacf(fit.star$RES,w_test,24)
hist(fit.star$RES[,10])
boxplot(fit.star$RES[,10])



#这些时间里，第10个station的prediction情况
matplot(1:168,cbind(test[505:672,10],(pre.star$PRE[,10])),type='l',
        ylab = "Values", xlab = "Time Points", 
        main = "Comparison of Actual and Predicted Values",
        col = c("black", "red"))
legend("topleft", legend = c("Actual Values", "Predicted Values"), col = c("black", "red"), lty = 1)

hist(fit.star$NRMSE)

