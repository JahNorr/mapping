#===============================================
#
# mapping
#
#
library(maps)

m <- map("state",fill=TRUE,plot=FALSE)

area.map(m)
area.map(m, ".*dakota")
area.map(m, c("North Dakota","South Dakota"))


#map.scale(-160,45,relwidth = 0.15, metric = TRUE, ratio = TRUE)
map('world', 'Virgin Islands',resolution = 0 )    # county map of New Jersey

