#===============================================
#
# mapping
#
#
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
# library(rgdal)
#=====================================
#   best St. Croix limits

# latmax<-17.78
# latmin<-17.68
# 
# lonmax<-(-64.57)
# lonmin<-(-64.9)


#=====================================
#   best St. Thomas/St. John limits

# latmax<-18.39
# latmin<-18.31
# 
# lonmax<-(-64.66)
# lonmin<-(-65.03)

#=====================================
#   best St. Thomas limits
# 
# latmax<-18.4
# latmin<-18.3
# 
# lonmax<-(-64.926)
# lonmin<-(-64.937)

#=====================================
#   best St. John limits
#   needs some tweaking
# 
# latmax<-18.38
# latmin<-18.31
# 
# lonmax<--64.741
# lonmin<--64.7428

est<- terra::vect("./data_raw/tl_2014_78_estate.shp") #rgdal::readOGR("./data_raw/tl_2014_78_estate.shp")

islands<-c("St. Croix","St. Thomas","St. John")

lim<-c(
  c(17.729,17.731,-64.90,-64.57),
  c(18.3,18.4,-65.047,-64.823),
  c(18.298,18.373,-64.81,-64.64)
)

limits<-matrix(lim,4,3)
colnames(limits)<- islands
rownames(limits)<- c("minlat","maxlat","minlon","maxlon")

#color the map based on population
est_pop<-read.csv("../Census/data/ethnicity.csv")
est_pop<-est_pop[est_pop$Category=="Total" & est_pop$Estate.Code>0,]

fp<-as.integer(as.character(est$ESTATEFP))
df_fp<-data.frame(fp,stringsAsFactors = F)

df_fp<- dplyr::left_join(df_fp,est_pop,by=c("fp"="Estate.Code"))
pop_max<-max(df_fp$Population,na.rm = T)

df_fp$pop_val<-df_fp$Population/pop_max

#fp%in%est_pop$Estate.Code

show.axes<-FALSE


island<-islands[c(1)]

lim_tmp<-as.data.frame(limits[,island])

maplims<-numeric(4)
names(maplims)<-rownames(limits)
maplims["minlat"]<-min(lim_tmp["minlat",])
maplims["minlon"]<-min(lim_tmp["minlon",])
maplims["maxlat"]<-max(lim_tmp["maxlat",])
maplims["maxlon"]<-max(lim_tmp["maxlon",])


colors<-alpha("darkgreen",0.1+df_fp$pop_val*0.9)

par(mar=c(3.0, 3.0, 1.5, 1.5))
plot(est,axes=show.axes,xlim=c(maplims["minlon"],maplims["maxlon"]),ylim=c(maplims["minlat"],maplims["maxlat"]), col=colors, border=TRUE)  #plot the species range
