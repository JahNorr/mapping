#===============================================
#
# mapping with Google
#
#


library(maptools)
library(RColorBrewer)
library(ggmap)

url<-"http://www2.census.gov/geo/tiger/TIGER2014/ESTATE/tl_2014_78_estate.zip"
destfile<-"./data_raw/zip/tl_2014_78_estate.zip"
method<-"auto"

download.file(url, destfile, method, quiet = FALSE, mode = "w",
              cacheOK = TRUE)

exdir<-"./data_raw"

unzip(destfile,exdir=exdir)


area <- readShapePoly("C:/Users/john/Documents/Geodata/VI/tl_2014_78_estate/tl_2014_78_estate.shp")

# # or file.choose:
# area <- readShapePoly(file.choose())


colors <- brewer.pal(9, "BuGn")

mapImage <- get_map(location = c(lon = -64.77, lat = 17.75),
                    color = "color",
                    source = "google",
                     maptype = "terrain",
                    zoom = 15)

area.points <- fortify(area)

# Finally, we can map our shape files!
  
  ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")

