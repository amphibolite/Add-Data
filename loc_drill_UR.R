library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)

loc_drill <- read_csv("Add Date/location_drillhole_HYNY_T1NY_T3XV.csv",
                      col_names=c("drillhole","date","block","easting","northing",
                                  "elev","x","y","z"))

URBZ_geo    <- read_csv("Add Date/URBZ_geo.csv")
URBZ_geo    <- separate(URBZ_geo, Date, into=c("date", "month", "year"), sep="-" )
URBZ_geo    <- mutate(URBZ_geo, date=str_pad(date, 2, side="left", pad="0"))
URBZ_geo    <- mutate(URBZ_geo, month=str_pad(match(month, month.abb),2,side="left",pad="0"))
URBZ_geo    <- mutate(URBZ_geo, year=str_pad(year,3,side="left",pad="0"))
URBZ_geo    <- mutate(URBZ_geo, year=str_pad(year,4,side="left",pad="2"))
URBZ_geo    <- separate(URBZ_geo, Hole, into=c("numb", "code"), sep="_" )
URBZ_geo    <- mutate(URBZ_geo, code="UZYB")
URBZ_geo    <- mutate(URBZ_geo, numb=str_pad(numb,4,side="left",pad="0"))
URBZ_geo    <- unite(URBZ_geo, drillhole,code, numb,sep="")
URBZ_geo    <- select(URBZ_geo, "drillhole","Block","E/W","N/S","ELEV")
URBZ_geo    <- rename(URBZ_geo, block="Block",easting="E/W",northing="N/S",elev=ELEV)

loc_UR      <- left_join(loc_drill,URBZ_geo, by="drillhole")
loc_UR      <- unite(loc_UR, block, block.x,block.y,sep="", na.rm=TRUE)
loc_UR      <- unite(loc_UR, easting, easting.x,easting.y,sep="", na.rm=TRUE)
loc_UR      <- unite(loc_UR, northing, northing.x,northing.y,sep="", na.rm=TRUE)
loc_UR      <- unite(loc_UR, elev, elev.x,elev.y,sep="", na.rm=TRUE)