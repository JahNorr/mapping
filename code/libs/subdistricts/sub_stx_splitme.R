
require(dplyr)
require(ggplot2)

source("./code/libs/lib_estates.R")
source("./code/libs/lib_subdistricts.R")
source("./code/libs/lib_islands.R")
source("./code/libs/lib_subdistrict_estates.R")

build_sub_stx_99 <- function() {
  
  subdist <- subdist_from_func() 
  isl <- isl_from_func() #
  
  fips <- islands(isl) %>% 
    pull(county_fips) 
  
  est <- subdistrict_estates() 
  
  srchlims <- numeric()
  srchlims["minlat"]<-17.68
  srchlims["maxlat"]<-17.74
  
  srchlims["minlon"]<- -64.84
  srchlims["maxlon"]<- -64.71
  
  estates <-  c("Peters Rest") #,  "Cane Garden", "Barren Spot West",
 
  offset <- 0.01
  maplims <- numeric()
  maplims["minlat"] <- srchlims["minlat"] - offset
  maplims["maxlat"] <- srchlims["maxlat"] + offset
  
  maplims["minlon"] <- srchlims["minlon"] - offset
  maplims["maxlon"] <- srchlims["maxlon"] + offset
  
  
  file <- paste0("./data/subdistricts/sub_", isl, "_", subdist, ".rds")
  
  saveRDS(estates, file = file)
  updte_subdistrict_estates(isl = isl, estates = estates, as.integer(subdist))
  
  print(ggplot_estates(isl, estates, maplim = maplims, subdistricts = T))
  
}


