#===============================================
#
# build workspace for vi estates mapping
#
#
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency

#=====================================
#   best St. Croix limits
#
# latmin<-17.729
# latmax<-17.731
# 
# lonmin<--64.9
# lonmax<--64.57


#=====================================
#   best St. Thomas/St. John limits

# latmax<-18.39
# latmin<-18.31
# 
# lonmax<--64.66
# lonmin<--65.03

#=====================================
#   best St. Thomas limits
# 
# latmin<-18.3
# latmax<-18.4
# 
# lonmin<--65.047
# lonmax<--64.823

#=====================================
#   best St. John limits
#   needs some tweaking
#
# latmax<-18.298
# latmin<-18.373
# 
# lonmax<--64.743
# lonmin<--64.735


estates <- readShapePoly("./data_raw/tl_2014_78_estate.shp")

l<-c(c(17.729,17.731,-64.90,-64.57),c(18.3,18.4,-65.047,-64.823),c(18.298,18.373,-64.743,-64.735))
limits<-matrix(l,4,3)
colnames(limits)<- c("St. Croix","St. Thomas","St. John")
rownames(limits)<- c("minlat","maxlat","minlon","maxlon")

rm(l)

