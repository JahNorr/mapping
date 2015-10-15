library(dismo)
e = extent( -64.9 , -64.58 , 17.64 , 17.8 )
# you can also get an Extent object by
#clicking on the map twice after using:
# drawExtent()
rmt = gmap(e)#default=Mercator
plot(rmt, interpolate=TRUE)