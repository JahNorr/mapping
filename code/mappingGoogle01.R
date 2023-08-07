#===============================================
#
# mapping with Google
#
#


library(maptools)
<<<<<<< HEAD:code/mappingGoogle01.R
area <- readShapePoly("./data_raw/tl_2014_78_estate.shp")
=======
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
>>>>>>> 0c2c7b2da4673813252177dccfcd257fc70d628c:mappingGoogle01.R

# # or file.choose:
# area <- readShapePoly(file.choose())


colors <- brewer.pal(9, "BuGn")

mapImage <- get_map(location = c(lon = -64.77, lat = 17.75),
                    color = "color",
                    source = "google",
                     maptype = "terrain",
                    zoom = 13)

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

