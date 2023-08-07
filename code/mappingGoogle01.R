#===============================================
#
# mapping with Google
#
#


library(maptools)
area <- readShapePoly("./data_raw/tl_2014_78_estate.shp")

# # or file.choose:
# area <- readShapePoly(file.choose())


library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")

library(ggmap)
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

