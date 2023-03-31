####################################
############Chapter 0###############
####################################

####------0.1 Importing data------
#import Janurary Data
getwd()
volume01 <-  read.delim("./Data/Raw Data/station_day/d03_text_station_hour_2023_01.txt",
                        sep=",", header=FALSE)
#pick freeway 5
volume01_fwy5 <- volume01[volume01$V4 == '5',]

#pick 4 weeks
weeks<-c('01/02/2023','01/03/2023','01/04/2023','01/05/2023','01/06/2023','01/07/2023','01/08/2023',
        '01/09/2023','01/10/2023','01/11/2023','01/12/2023','01/13/2023','01/14/2023','01/15/2023',
        '01/16/2023','01/17/2023','01/18/2023','01/19/2023','01/20/2023','01/21/2023','01/22/2023',
        '01/23/2023','01/24/2023','01/25/2023','01/26/2023','01/27/2023','01/28/2023','01/29/2023')

volume01_fwy5_2 <- volume01_fwy5[grepl(weeks[1],volume01_fwy5$V1),]
volume01_fwy5_3 <- volume01_fwy5[grepl(weeks[2],volume01_fwy5$V1),]
volume01_fwy5_4 <- volume01_fwy5[grepl(weeks[3],volume01_fwy5$V1),]
volume01_fwy5_5 <- volume01_fwy5[grepl(weeks[4],volume01_fwy5$V1),]
volume01_fwy5_6 <- volume01_fwy5[grepl(weeks[5],volume01_fwy5$V1),]
volume01_fwy5_7 <- volume01_fwy5[grepl(weeks[6],volume01_fwy5$V1),]
volume01_fwy5_8 <- volume01_fwy5[grepl(weeks[7],volume01_fwy5$V1),]

volume01_fwy5_9 <- volume01_fwy5[grepl(weeks[8],volume01_fwy5$V1),]
volume01_fwy5_10 <- volume01_fwy5[grepl(weeks[9],volume01_fwy5$V1),]
volume01_fwy5_11 <- volume01_fwy5[grepl(weeks[10],volume01_fwy5$V1),]
volume01_fwy5_12 <- volume01_fwy5[grepl(weeks[11],volume01_fwy5$V1),]
volume01_fwy5_13 <- volume01_fwy5[grepl(weeks[12],volume01_fwy5$V1),]
volume01_fwy5_14 <- volume01_fwy5[grepl(weeks[13],volume01_fwy5$V1),]
volume01_fwy5_15 <- volume01_fwy5[grepl(weeks[14],volume01_fwy5$V1),]

volume01_fwy5_16 <- volume01_fwy5[grepl(weeks[15],volume01_fwy5$V1),]
volume01_fwy5_17 <- volume01_fwy5[grepl(weeks[16],volume01_fwy5$V1),]
volume01_fwy5_18 <- volume01_fwy5[grepl(weeks[17],volume01_fwy5$V1),]
volume01_fwy5_19 <- volume01_fwy5[grepl(weeks[18],volume01_fwy5$V1),]
volume01_fwy5_20 <- volume01_fwy5[grepl(weeks[19],volume01_fwy5$V1),]
volume01_fwy5_21 <- volume01_fwy5[grepl(weeks[20],volume01_fwy5$V1),]
volume01_fwy5_33 <- volume01_fwy5[grepl(weeks[21],volume01_fwy5$V1),]


volume01_fwy5_23 <- volume01_fwy5[grepl(weeks[22],volume01_fwy5$V1),]
volume01_fwy5_24 <- volume01_fwy5[grepl(weeks[23],volume01_fwy5$V1),]
volume01_fwy5_25 <- volume01_fwy5[grepl(weeks[24],volume01_fwy5$V1),]
volume01_fwy5_26 <- volume01_fwy5[grepl(weeks[25],volume01_fwy5$V1),]
volume01_fwy5_27 <- volume01_fwy5[grepl(weeks[26],volume01_fwy5$V1),]
volume01_fwy5_28 <- volume01_fwy5[grepl(weeks[27],volume01_fwy5$V1),]
volume01_fwy5_29 <- volume01_fwy5[grepl(weeks[28],volume01_fwy5$V1),]

#Import the staion meta data including the coodrinate
station_meta<- read.delim("/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/station_meta/d03_text_meta_2023_03_01.txt")
station_fwy5 <- station_meta[station_meta$'Fwy'=="5",]
station_fwy5 <- station_fwy5[order(station_fwy5$ID),]


library(dplyr)

volume01_df<-station_fwy5 %>% select('ID','Dir','Type','Length','Latitude','Longitude')

for(i in 1:28){
  for(j in 1:24){
    col_name <- paste('t',i,'_',j-1,sep="")
    volume01_df[col_name]<-1
  }
}

#replace the null in length into 0
volume01_df[is.na(volume01_df)] <- 0

#insert the traffic volume into dataframe based on station id
#1/2
count=6
volume01_fwy5_2 <- volume01_fwy5_2 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_2[volume01_fwy5_2$V2==volume01_df$ID[i],3][j]
  }
}

#1/3
count = count + 24
volume01_fwy5_3 <- volume01_fwy5_3 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_3[volume01_fwy5_3$V2==volume01_df$ID[i],3][j]
  }
}

#1/4
count = count +24
volume01_fwy5_4 <- volume01_fwy5_4 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_4[volume01_fwy5_4$V2==volume01_df$ID[i],4][j]
  }
}

#1/5
count = count +24
volume01_fwy5_5 <- volume01_fwy5_5 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_5[volume01_fwy5_5$V2==volume01_df$ID[i],5][j]
  }
}


#1/6
count = count +24
volume01_fwy5_6 <- volume01_fwy5_6 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_6[volume01_fwy5_6$V2==volume01_df$ID[i],5][j]
  }
}

#1/7
count = count +24
volume01_fwy5_7 <- volume01_fwy5_7 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_7[volume01_fwy5_7$V2==volume01_df$ID[i],5][j]
  }
}

#1/8
count = count +24
volume01_fwy5_8 <- volume01_fwy5_8 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_8[volume01_fwy5_8$V2==volume01_df$ID[i],5][j]
  }
}

#1/9
count = count +24
volume01_fwy5_9 <- volume01_fwy5_9 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_9[volume01_fwy5_9$V2==volume01_df$ID[i],5][j]
  }
}

#1/10
count = count +24
volume01_fwy5_10 <- volume01_fwy5_10 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_10[volume01_fwy5_10$V2==volume01_df$ID[i],5][j]
  }
}

#1/11
count = count +24
volume01_fwy5_11 <- volume01_fwy5_11 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_11[volume01_fwy5_11$V2==volume01_df$ID[i],5][j]
  }
}

#1/12
count = count +24
volume01_fwy5_12 <- volume01_fwy5_12 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_12[volume01_fwy5_12$V2==volume01_df$ID[i],5][j]
  }
}

#1/13
count = count +24
volume01_fwy5_13 <- volume01_fwy5_13 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_13[volume01_fwy5_13$V2==volume01_df$ID[i],5][j]
  }
}

#1/14
count = count +24
volume01_fwy5_14 <- volume01_fwy5_14 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_14[volume01_fwy5_14$V2==volume01_df$ID[i],5][j]
  }
}

#1/15
count = count +24
volume01_fwy5_15 <- volume01_fwy5_15 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_15[volume01_fwy5_15$V2==volume01_df$ID[i],5][j]
  }
}

#1/16
count = count +24
volume01_fwy5_16 <- volume01_fwy5_16 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_16[volume01_fwy5_16$V2==volume01_df$ID[i],5][j]
  }
}

#1/17
count = count +24
volume01_fwy5_17 <- volume01_fwy5_17 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_17[volume01_fwy5_17$V2==volume01_df$ID[i],5][j]
  }
}

#1/18
count = count +24
volume01_fwy5_18 <- volume01_fwy5_18 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_18[volume01_fwy5_18$V2==volume01_df$ID[i],5][j]
  }
}

#1/19
count = count +24
volume01_fwy5_19 <- volume01_fwy5_19 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_19[volume01_fwy5_19$V2==volume01_df$ID[i],5][j]
  }
}

#1/20
count = count +24
volume01_fwy5_20 <- volume01_fwy5_20 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_20[volume01_fwy5_20$V2==volume01_df$ID[i],5][j]
  }
}

#1/21
count = count +24
volume01_fwy5_21 <- volume01_fwy5_21 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_21[volume01_fwy5_21$V2==volume01_df$ID[i],5][j]
  }
}

#1/22
count = count +24
volume01_fwy5_22 <- volume01_fwy5_22 %>% select(c(1,2,10))
for(i in 1:172){
  for(j in 1:25){
    volume01_df[i,j+count] = volume01_fwy5_22[volume01_fwy5_22$V2==volume01_df$ID[i],5][j]
  }
}

#1/23
count = count +24
volume01_fwy5_23 <- volume01_fwy5_23 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_23[volume01_fwy5_23$V2==volume01_df$ID[i],3][j]
  }
}

#1/24
count = count +24
volume01_fwy5_24 <- volume01_fwy5_24 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_24[volume01_fwy5_24$V2==volume01_df$ID[i],3][j]
  }
}


#1/25
count = count +24
volume01_fwy5_25 <- volume01_fwy5_25 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_25[volume01_fwy5_25$V2==volume01_df$ID[i],3][j]
  }
}


#1/26
count = count +24
volume01_fwy5_26 <- volume01_fwy5_26 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_26[volume01_fwy5_26$V2==volume01_df$ID[i],3][j]
  }
}


#1/27
count = count +24
volume01_fwy5_27 <- volume01_fwy5_27 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_27[volume01_fwy5_27$V2==volume01_df$ID[i],3][j]
  }
}


#1/28
count = count +24
volume01_fwy5_28 <- volume01_fwy5_28 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_28[volume01_fwy5_28$V2==volume01_df$ID[i],3][j]
  }
}



#1/29
count = count +24
volume01_fwy5_29 <- volume01_fwy5_29 %>% select(c(1,2,10))


for(i in 1:172){
  for(j in 1:24){
    volume01_df[i,j+count] = volume01_fwy5_29[volume01_fwy5_29$V2==volume01_df$ID[i],3][j]
  }
}

#output cleaned data with null
write.csv(volume01_df, "/Users/apple/Desktop/0042 Spatial and temporal analysis and data mining/traffic_volume_df.csv", row.names=FALSE)

#replace null with -1
volume01_df_no <- volume01_df
volume01_df_no[is.na(volume01_df_no)] <- -1

#output cleaned data without null
write.csv(volume01_df_no, "/Data/Clean Data/traffic_volume_df_no.csv", row.names=FALSE)







