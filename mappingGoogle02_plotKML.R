library("plotKML")
data("eberg")
eberg <- eberg[runif(nrow(eberg)) < 0.2, ]
coordinates(eberg) <- ~ X + Y
proj4string(eberg) <- CRS("+init=epsg:31467")
#Next, we can reproject this object to the WGS84 coordinate system:
eberg.ll <- reproject(eberg)
#so that it can be plotted in Google Earth by using:
plotKML(eberg.ll["CLYMHT_A"])
