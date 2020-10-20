library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

loc_drill <- read_csv("Add Date/location_drillhole_HYNY_T1NY_T3XV.csv",
                      col_names=c("drillhole","date","block","easting","northing",
                                  "elev","x","y","z"))
loc_drill <- mutate(loc_drill, date=as.character(date))

T3_geo    <- read_csv("Add Date/T3_geo.csv", skip=TRUE, col_names=c("drillhole",
                              "date","block","easting","northing","elev","ni",
                              "fe","co"))
T3_geo    <- separate(T3_geo, date, into=c("date", "month", "year"), sep="-" )
T3_geo    <- mutate(T3_geo, date=str_pad(date, 2, side="left", pad="0"))
T3_geo    <- mutate(T3_geo, month=str_pad(match(month, month.abb),2,side="left",pad="0"))
T3_geo    <- mutate(T3_geo, year=as.integer(year))
T3_geo    <- mutate(T3_geo, year=ifelse(year<50, year+2000, year+1900))
T3_geo    <- mutate(T3_geo, code=ifelse(year=="2005"|year=="2006", "T3NY",""))
T3_geo    <- mutate(T3_geo, code=ifelse(str_detect(T3_geo$drillhole,"VDH"),"T3VD",code))
T3_geo    <- mutate(T3_geo, code=ifelse(grepl("DL",T3_geo$drillhole,
                                          ignore.case=TRUE),"T3DL",code))
T3_geo    <- mutate(T3_geo, code=ifelse(grepl("T3-15",T3_geo$drillhole,
                                              ignore.case=TRUE),"T3XV",code))
T3_geo    <- mutate(T3_geo, drillhole=str_replace(T3_geo$drillhole,"T3-15-",""))
T3_geo    <- mutate(T3_geo, drillhole=str_replace(T3_geo$drillhole,"VDH",""))
T3_geo    <- mutate(T3_geo, drillhole=str_replace(T3_geo$drillhole,"YBM",""))
T3_geo    <- mutate(T3_geo, code=ifelse(code!="", code,"T3YB"))
T3_geo    <- unite(T3_geo, date, year, month, date, sep="-")
T3_geo    <- mutate(T3_geo, numb=sub("[^0-9].*","",drillhole))
T3_geo    <- mutate(T3_geo, numb=str_pad(numb,4,side="left","0"))
T3_geo    <- unite(T3_geo, drillhole, code, numb, sep="")

T3_matching1 <- subset(T3_geo, str_detect(drillhole,pattern="DL"))
T3_matching2 <- subset(TAGA3_SAMP, str_detect(drillhole,pattern="DL"))
T3_matching  <- left_join(T3_matching1,T3_matching2, by=c("ni","fe","co"))
T3_matching  <- mutate(T3_matching,drillhole=drillhole.y)
T3_matching  <- select(T3_matching, drillhole, date, easting, northing,elev)

T3_geo    <- subset(T3_geo, str_detect(drillhole,pattern="DL")==FALSE)
T3_geo    <- full_join(T3_geo,T3_matching, by="drillhole")
T3_geo    <- unite(T3_geo,easting, easting.x,easting.y, na.rm=TRUE, sep="")
T3_geo    <- unite(T3_geo,northing, northing.x, northing.y,na.rm=TRUE, sep="")
T3_geo    <- unite(T3_geo,elev, elev.x,elev.y,na.rm=TRUE, sep="")
T3_geo    <- unite(T3_geo,date, date.x,date.y,na.rm=TRUE, sep="")
T3_geo    <- select(T3_geo, drillhole, date,block, easting, northing, elev)

loc_T3    <- left_join(loc_drill,T3_geo, by="drillhole")
loc_T3    <- mutate(loc_T3, date=date.x)
loc_T3    <- mutate(loc_T3, date=ifelse(is.na(date),date.y,date))
loc_T3    <- unite(loc_T3, block, block.x,block.y,sep="", na.rm=TRUE)
loc_T3    <- unite(loc_T3, easting, easting.x,easting.y,sep="", na.rm=TRUE)
loc_T3    <- unite(loc_T3, northing, northing.x,northing.y,sep="", na.rm=TRUE)
loc_T3    <- unite(loc_T3, elev, elev.x,elev.y,sep="", na.rm=TRUE)
loc_T3    <- select(loc_T3, drillhole, date, block, easting, northing,elev,x,y,z)


