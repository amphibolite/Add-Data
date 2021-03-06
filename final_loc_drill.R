library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

loc_HY_UR <- full_join(loc_HY,loc_UR, by=c("drillhole","date","x","y","z"))
loc_HY_UR <- unite(loc_HY_UR,block,block.x,block.y,sep="",na.rm=TRUE)
loc_HY_UR <- unite(loc_HY_UR,easting,easting.x,easting.y,sep="",na.rm=TRUE)
loc_HY_UR <- unite(loc_HY_UR,northing,northing.x,northing.y,sep="",na.rm=TRUE)
loc_HY_UR <- unite(loc_HY_UR,elev,elev.x,elev.y,sep="",na.rm=TRUE)

loc_T1_T3 <- full_join(loc_T1,loc_T3, by=c("drillhole","date","x","y","z"))
loc_T1_T3 <- unite(loc_T1_T3,block,block.x,block.y,sep="",na.rm=TRUE)
loc_T1_T3 <- unite(loc_T1_T3,easting,easting.x,easting.y,sep="",na.rm=TRUE)
loc_T1_T3 <- unite(loc_T1_T3,northing,northing.x,northing.y,sep="",na.rm=TRUE)
loc_T1_T3 <- unite(loc_T1_T3,elev,elev.x,elev.y,sep="",na.rm=TRUE)
loc_T1_T3 <- mutate(loc_T1_T3, date=as.Date(date))

loc_HUT13 <- full_join(loc_T1_T3,loc_HY_UR, by=c("drillhole","date","x","y","z"))
loc_HUT13 <- unite(loc_HUT13,block,block.x,block.y,sep="",na.rm=TRUE)
loc_HUT13 <- unite(loc_HUT13,easting,easting.x,easting.y,sep="",na.rm=TRUE)
loc_HUT13 <- unite(loc_HUT13,northing,northing.x,northing.y,sep="",na.rm=TRUE)
loc_HUT13 <- unite(loc_HUT13,elev,elev.x,elev.y,sep="",na.rm=TRUE)

write.csv(loc_HUT13,"Add Date/Updated_loc_drill.csv", na="")