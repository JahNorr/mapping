#===============================================
#
# mapping
#
#
library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency

#=====================================
#   best St. Croix limits

# latmax<-17.78
# latmin<-17.68
# 
# lonmax<--64.57
# lonmin<--64.9


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
# latmax<-18.4
# latmin<-18.3
# 
# lonmax<--64.926
# lonmin<--64.937

#=====================================
#   best St. John limits
#   needs some tweaking
# 
# latmax<-18.38
# latmin<-18.31
# 
# lonmax<--64.741
# lonmin<--64.7428

estates <- readShapePoly("C:/Users/john/Documents/Geodata/VI/tl_2014_78_estate/tl_2014_78_estate.shp")

l<-c(c(17.729,17.731,-64.90,-64.57),c(18.3,18.4,-65.047,-64.823),c(18.298,18.373,-64.743,-64.735))
limits<-matrix(l,4,3)
colnames(limits)<- c("St. Croix","St. Thomas","St. John")
rownames(limits)<- c("minlat","maxlat","minlon","maxlon")

island<-"St. Croix"
show.axes<-FALSE


maplims<-limits[,c(island)]

length(estates)

names(estates)

colors<-rep(alpha("darkgreen", 0.4),times=length(estates))

colors[33:55]<-alpha("pink", 0.4)

par(mar=c(3.0, 3.0, 1.5, 1.5))
plot(estates,axes=show.axes,xlim=c(maplims["minlon"],maplims["maxlon"]),ylim=c(maplims["minlat"],maplims["maxlat"]), col=colors, border=TRUE)  #plot the species range
#points(samps$lon, samps$lat, pch=19, col="red", cex=0.5)  #plot my sample sites