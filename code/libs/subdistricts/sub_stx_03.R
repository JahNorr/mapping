
require(dplyr)

require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_estates_subdist.R")

maplims <- numeric()
maplims["minlat"]<-17.665
maplims["minlon"]<- -64.7
maplims["maxlat"]<-17.79
maplims["maxlon"]<- -64.55

print(ggplot_estates("STX",subdist_03(), maplim = maplims))

